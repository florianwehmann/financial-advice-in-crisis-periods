
# =============================================================================
# total_return_split.R -- the total-return analysis of 07_estim_v3.R, split into
# a PASSIVE (buy-and-hold) and an ACTIVE part.
#
# 07 estimates the event study on pf_idx_w: the client's chain-linked portfolio
# return, rebased to rel_month -1. This script reproduces that outcome and adds
#
#   pb_idx   what the SAME client would have earned by simply holding the
#            rel_month -1 portfolio, each asset valued with its own realized
#            monthly return (pooled over all holders in pos_m1.parquet)
#   act_gap  act_idx - pb_idx, i.e. everything the client DID: selling into the
#            drawdown, buying the rebound, rotating, or liquidating entirely
#
# Sample, FE, controls, clustering, window and reference month follow
# 07_estim_v3.R. Reads existing data read-only; helpers come from pb_funs.R
# (functions only) and tr_funs.R. Writes only into output/total_return/.
#
# Run from R/estimation_new in a fresh session.
# Parameters: PF_PRE_MIN (as in 07), REBUILD_AR (rebuild the asset-return panel).
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(arrow)
  library(ggplot2)
  library(duckdb)
  library(lubridate)
})
source("pb_funs.R")   # pb_est / pb_get_ct / pb_rhs / pb_avg / pb_prep_est / pb_add_ids
source("tr_funs.R")

# ---- Config (mirrors 07_estim_v3.R) -----------------------------------------
OUT <- "../../output/total_return"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

stk_path <- "../../data/stk2.parquet"
pos_path <- "../../data/pos_m1.parquet"     # position level, from 01_merge_pos.R
ep_path  <- "../../data/episodes_short.parquet"
ar_file  <- file.path(OUT, "tr_asset_returns.parquet")

TRTS     <- c(BROAD = "treat_inv_a_p", NARROW = "treat_perfinv_a_p")
TRT_LABS <- c(BROAD  = "BROAD: adv-initiated personal investment contacts",
              NARROW = "NARROW: adv-initiated performance review + investment advice")
TRT_CTRL <- "treat_inv_c_p"
REF      <- -1L
win      <- c(-6L, 9L)
p_win    <- 0.99
drop_nonpos_wealth <- TRUE
if (!exists("PF_PRE_MIN")) PF_PRE_MIN <- 2000    # 07: est_dt[tot_pf_pre >= 5000]
if (!exists("REBUILD_AR")) REBUILD_AR <- FALSE
AVG_BANDS <- list(c(0L, 1L), c(2L, 4L), c(5L, 10L))

setFixest_nthreads(parallel::detectCores())
setFixest_notes(FALSE)
CHK <- list()

# ---- Load the stacked panel (read-only) --------------------------------------
stk_cols <- c("Bp_ID", "ep_id", "MDate", "advisor_id", "rel_month",
              "tot_wealth", "tot_wealth_pre", "tot_wealth_pre_mean", "tot_pf", "tot_pf_pre",
              "dp_tot_pf", "dfx_tot_pf", "dq_tot_pf", unname(TRTS), TRT_CTRL,
              "anlagepaket_pre", "adv_segment_pre", "d_deposit_pre_sum", "eq_share_of_w_pre_mean")
d <- setDT(read_parquet(stk_path, col_select = all_of(stk_cols), mmap = FALSE))
d <- pb_prep_est(d[!is.na(MDate)], win, REF, drop_nonpos_wealth)
d <- d[tot_pf_pre >= PF_PRE_MIN]                 # same restriction as 07

ep <- setDT(read_parquet(ep_path))[usable == TRUE]
setorder(ep, dd_start)

# ---- Passive counterfactual --------------------------------------------------
tr_asset_returns(pos_path, ar_file, from = "2010-01-31", to = "2024-12-31", force = REBUILD_AR)
pb <- tr_build_passive(ep, d[, .(Bp_ID, ep_id)], pos_path, ar_file)

d <- merge(d, pb, by = c("Bp_ID", "ep_id", "MDate"), all.x = TRUE)
d <- tr_outcomes(d, ref = REF, p_win = p_win)

# both indices must be anchored at the freeze month
d[, has_ref := any(rel_month == REF & !is.na(pb_val) & !is.na(tot_pf)), by = .(Bp_ID, ep_id)]
ce <- \(x) uniqueN(x, by = c("Bp_ID", "ep_id"))
CHK$sample <- data.table(
  step = c("07 sample (window, wealth filters)", sprintf("tot_pf_pre >= %s", PF_PRE_MIN),
           "passive value at m = -1", "estimation sample"),
  client_eps = c(ce(d), ce(d), ce(d[has_ref == TRUE]),
                 ce(d[has_ref == TRUE & !is.na(act_idx) & !is.na(pb_idx)])))
print(CHK$sample)

est_dt <- pb_add_ids(d[has_ref == TRUE & !is.na(act_idx) & !is.na(pb_idx)])

# =============================================================================
# ESTIMATION -- same spec as 07, three outcomes
# =============================================================================
mods <- paths <- avgs <- list()
for (nm in names(TRTS)) {
  trt <- TRTS[[nm]]
  m <- pb_est(TR_OUTCOMES, est_dt, pb_rhs(trt, TRT_CTRL, REF))
  mods[[nm]] <- m
  paths[[nm]] <- rbindlist(lapply(TR_OUTCOMES, \(v) pb_get_ct(m[[v]], v, trt, REF)))[
    , treatment := nm]
  avgs[[nm]] <- rbindlist(lapply(TR_OUTCOMES, \(v) rbindlist(lapply(AVG_BANDS, \(b)
    cbind(data.table(treatment = nm, outcome = v, band = sprintf("[%d,%d]", b[1], b[2])),
          pb_avg(m[[v]], trt, b[1], b[2]))))))
  etable(m, keep = paste0("%", trt), tex = TRUE, replace = TRUE,
         file = file.path(OUT, sprintf("tr_es_%s.tex", nm)))
}
paths <- rbindlist(paths); avgs <- rbindlist(avgs)
avgs[, `:=`(t = avg / se, ci_lo = avg - 1.96 * se, ci_hi = avg + 1.96 * se)]
fwrite(paths, file.path(OUT, "tr_event_study_paths.csv"))
fwrite(avgs,  file.path(OUT, "tr_pooled_averages.csv"))

# the split is additive: actual = passive + active, coefficient by coefficient
add_chk <- dcast(paths, treatment + rel_month ~ outcome, value.var = "est")
add_chk[, gap := act_idx - (pb_idx + act_gap)]
CHK$additive <- add_chk[, .(max_abs_dev = max(abs(gap))), by = treatment]
stopifnot(CHK$additive[, max(max_abs_dev)] < 1e-10)

# =============================================================================
# PLOTS
# =============================================================================
tr_theme <- theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

# 1. actual vs passive vs active, per treatment
pd <- copy(paths)[, series := factor(TR_LABS[outcome], levels = TR_LABS)]
p_all <- ggplot(pd, aes(rel_month, est, colour = series, fill = series)) +
  geom_ribbon(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.7) + geom_point(size = 1.2) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_vline(xintercept = REF + 0.5, linetype = "dashed", linewidth = 0.4) +
  facet_wrap(~treatment) +
  scale_colour_manual(values = c("#C0392B", "#41729F", "grey25"), aesthetics = c("colour", "fill")) +
  scale_x_continuous(breaks = seq(win[1], win[2], 2)) +
  labs(x = "Months relative to pre-crisis month", y = "Cumulative return, rebased to m = -1",
       colour = NULL, fill = NULL,
       title = "Portfolio total return of advice, split into passive and active",
       subtitle = sprintf("Passive = holding the m=-1 portfolio at realized asset returns | N = %s | 95%% CI clustered by advisor",
                          format(nobs(mods[[1]]$act_idx), big.mark = "'"))) +
  tr_theme
ggsave(file.path(OUT, "tr_actual_passive_active.pdf"), p_all, width = 10, height = 5)

# 2. the active part alone, both treatments
p_gap <- ggplot(paths[outcome == "act_gap"], aes(rel_month, est, colour = treatment, fill = treatment)) +
  geom_ribbon(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.7) + geom_point(size = 1.3) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_vline(xintercept = REF + 0.5, linetype = "dashed", linewidth = 0.4) +
  scale_colour_manual(values = c(BROAD = "#41729F", NARROW = "#C0392B"),
                      aesthetics = c("colour", "fill")) +
  scale_x_continuous(breaks = seq(win[1], win[2], 2)) +
  labs(x = "Months relative to pre-crisis month", y = "Active return contribution",
       colour = NULL, fill = NULL,
       title = "The active part: actual return minus buy-and-hold",
       subtitle = "Positive = advised clients beat holding their own m=-1 portfolio. 95% CI clustered by advisor") +
  tr_theme
ggsave(file.path(OUT, "tr_active_gap.pdf"), p_gap, width = 8, height = 5)

# 3. stacked decomposition: passive + active = actual
stk_dt <- paths[outcome %in% c("pb_idx", "act_gap")]
stk_dt[, part := factor(TR_LABS[outcome], levels = TR_LABS[c("pb_idx", "act_gap")])]
tot_dt <- paths[outcome == "act_idx"]
p_stack <- ggplot() +
  geom_col(data = stk_dt, aes(rel_month, est, fill = part), width = 0.75, alpha = 0.85) +
  geom_errorbar(data = tot_dt, aes(rel_month, ymin = est - 1.96 * se, ymax = est + 1.96 * se),
                width = 0.25, linewidth = 0.4) +
  geom_line(data = tot_dt, aes(rel_month, est), linewidth = 0.3) +
  geom_point(data = tot_dt, aes(rel_month, est), size = 1.6) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_vline(xintercept = REF + 0.5, linetype = "dashed", linewidth = 0.4) +
  facet_wrap(~treatment) +
  scale_fill_manual(values = c("#41729F", "#F28E2B")) +
  scale_x_continuous(breaks = seq(win[1], win[2], 2)) +
  labs(x = "Months relative to pre-crisis month", y = "Cumulative return, rebased to m = -1",
       fill = NULL, title = "Total return of advice = passive + active",
       subtitle = "Bars sum to the total return (dots, with 95% CI)") +
  tr_theme
ggsave(file.path(OUT, "tr_stacked_decomposition.pdf"), p_stack, width = 10, height = 5)

# =============================================================================
# CHECKS
# =============================================================================
# (a) clients who never traded: actual and passive must coincide
est_dt[, traded := any(abs(dq_tot_pf) > 1, na.rm = TRUE), by = .(Bp_ID, ep_id)]
zt <- est_dt[traded == FALSE & rel_month > REF]
CHK$zero_trade <- data.table(
  client_eps = uniqueN(zt, by = c("Bp_ID", "ep_id")), obs = nrow(zt),
  median_gap = median(zt$act_gap, na.rm = TRUE),
  p05 = quantile(zt$act_gap, .05, na.rm = TRUE), p95 = quantile(zt$act_gap, .95, na.rm = TRUE),
  sh_abs_gt_1pct = mean(abs(zt$act_gap) > 0.01, na.rm = TRUE))
CHK$zero_trade_ep <- zt[, .(client_eps = uniqueN(paste(Bp_ID, ep_id)),
                            median_gap = median(act_gap, na.rm = TRUE),
                            sh_abs_gt_1pct = mean(abs(act_gap) > 0.01, na.rm = TRUE)), keyby = ep_id]

# (b) indices are exactly 0 at the freeze month
CHK$ref_zero <- est_dt[rel_month == REF, .(obs = .N,
                                           max_abs_act = max(abs(act_idx), na.rm = TRUE),
                                           max_abs_pb  = max(abs(pb_idx), na.rm = TRUE))]

# (c) asset-return coverage of the frozen portfolio
CHK$coverage <- est_dt[, .(client_eps = uniqueN(paste(Bp_ID, ep_id)),
                           sh_value_no_return = sum(v0_no_ret) / sum(v0_sum),
                           med_n_pos = median(n_pos),
                           pb_cover_at_ref = median((pb_val / tot_pf)[rel_month == REF],
                                                    na.rm = TRUE)), keyby = ep_id]

# (d) liquidation: where the active part should come from
est_dt[, liquidated := any(tot_pf < 100 & rel_month >= 0), by = .(Bp_ID, ep_id)]
CHK$liquidation <- est_dt[est_dt[, .I[rel_month == max(rel_month)], by = ep_id]$V1, .(
  last_month = max(rel_month), client_eps = .N, sh_liquidated = mean(liquidated),
  med_gap_liq = median(act_gap[liquidated], na.rm = TRUE),
  med_gap_rest = median(act_gap[!liquidated], na.rm = TRUE)), keyby = ep_id]

# (e) does the actual index reproduce 07's pf_idx_w? (same construction, same sample)
CHK$vs_07 <- est_dt[, .(obs = .N, med_act_idx = median(act_idx, na.rm = TRUE),
                        med_pb_idx = median(pb_idx, na.rm = TRUE),
                        med_gap = median(act_gap, na.rm = TRUE)), keyby = rel_month]

md <- function(x) {
  x <- as.data.table(x)
  for (j in names(x)) if (is.numeric(x[[j]])) set(x, j = j, value = signif(x[[j]], 4))
  c(paste("|", paste(names(x), collapse = " | "), "|"),
    paste("|", paste(rep("---", ncol(x)), collapse = " | "), "|"),
    apply(x, 1, \(r) paste("|", paste(format(r, trim = TRUE), collapse = " | "), "|")))
}
writeLines(c(
  "# Total return split into passive and active - checks", "",
  sprintf("Generated %s | window [%d, %d] | ref %d | tot_pf_pre >= %s | winsor %.2f",
          format(Sys.time(), "%Y-%m-%d %H:%M"), win[1], win[2], REF, PF_PRE_MIN, p_win), "",
  "## Sample", "", md(CHK$sample), "",
  "## (a) Clients who never traded: act_gap must be ~0", "",
  "No trade means the actual portfolio IS the frozen one, so a gap can only come from the",
  "asset-return panel not reproducing the client's own price+FX attribution.", "",
  md(CHK$zero_trade), "", md(CHK$zero_trade_ep), "",
  "## (b) Both indices at m = -1 (must be 0)", "", md(CHK$ref_zero), "",
  "## (c) Asset-return coverage of the frozen portfolio", "",
  "`sh_value_no_return` = share of frozen value with no asset return in a month (held flat);",
  "`pb_cover_at_ref` = passive value over actual portfolio at m = -1 (1 = full coverage).", "",
  md(CHK$coverage), "",
  "## (d) Liquidation, the main source of an active gap", "", md(CHK$liquidation), "",
  "## (e) Median index paths", "", md(CHK$vs_07), ""), file.path(OUT, "checks.md"))

cat("\nTotal-return split written to", normalizePath(OUT), "\n")
print(avgs[, .(treatment, outcome, band, avg = round(avg, 4), se = round(se, 4), t = round(t, 2))])
