# =============================================================================
# stock_buyhold.R -- stock-level buy-and-hold counterfactual per client, built
# from REAL security prices (data/T60_Aktienkurse.parquet).
#
# passive_benchmark.R priced the frozen portfolio with prices implied by the
# holdings themselves (value / quantity). This script does the same exercise for
# the EQUITY SLEEVE with the bank's own security price history, which is
# independent of the position file -- so it is both a cleaner counterfactual and
# a check on the implied prices.
#
#   act_eq      actual equity price+FX change per client x episode x month
#   sb_passive  the same clients' equity holdings FROZEN at rel_month -1 and
#               valued at T60 month-end prices
#   sb_gap      act_eq - sb_passive = equity trading and timing
#
# T60 covers equities only (no funds, bonds or structured products), so this is
# the equity sleeve, which is 36-53% of portfolio value depending on the episode.
#
# Sample, FE, controls, clustering, window and reference month follow
# 07_estim_v3.R (helpers reused read-only from pb_funs.R).
# New files only: everything lands in output/stock_bh/.
#
# Run from R/estimation_new in a fresh session.
# Parameters: MAX_STALE_D (quote age cap), COVER_MIN (min. T60 coverage of the
# frozen equity at the freeze month), REBUILD_PX.
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(arrow)
  library(ggplot2)
  library(duckdb)
  library(lubridate)
})
source("pb_funs.R")   # pb_est / pb_get_ct / pb_rhs / pb_avg / pb_winsor / pb_prep_est
source("sb_funs.R")

# ---- Config ------------------------------------------------------------------
OUT <- "../../output/stock_bh"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

stk_path <- "../../data/stk2.parquet"
pos_path <- "../../data/pos_m1.parquet"
t60_path <- "../../data/T60_Aktienkurse.parquet"
ep_path  <- "../../data/episodes_short.parquet"
px_file  <- file.path(OUT, "sb_t60_month_end.parquet")

TRTS     <- c(BROAD = "treat_inv_a_p", NARROW = "treat_perfinv_a_p")
TRT_LABS <- c(BROAD  = "BROAD: adv-initiated personal investment contacts",
              NARROW = "NARROW: adv-initiated performance review + investment advice")
TRT_CTRL <- "treat_inv_c_p"
REF      <- -1L
win      <- c(-8L, 12L)
p_win    <- 0.99
drop_nonpos_wealth <- TRUE
AVG_BANDS <- list(c(0L, 1L), c(2L, 4L), c(5L, 10L))

if (!exists("MAX_STALE_D")) MAX_STALE_D <- 45L    # ignore quotes older than this
if (!exists("COVER_MIN"))   COVER_MIN   <- 0.90   # T60 must price >= 90% of frozen equity
if (!exists("REBUILD_PX"))  REBUILD_PX  <- FALSE
COVER_STRICT <- 0.99                              # robustness cut
# variant tag, so runs with different quote-age caps never overwrite each other
TAG <- if (MAX_STALE_D == 45L) "" else sprintf("_stale%d", MAX_STALE_D)
f_out <- function(...) file.path(OUT, paste0(sub("\\.([a-z]+)$", "", paste0(...)), TAG,
                                             sub("^.*(\\.[a-z]+)$", "\\1", paste0(...))))

setFixest_nthreads(parallel::detectCores())
setFixest_notes(FALSE)
CHK <- list()

# ---- Load the stacked panel (read-only) --------------------------------------
stk_cols <- c("Bp_ID", "ep_id", "MDate", "advisor_id", "rel_month",
              "tot_wealth", "tot_wealth_pre", "tot_wealth_pre_mean", "tot_pf", "tot_pf_pre",
              "equity", "equity_pre", "dp_eq_c", "dq_tot_pf", "dp_tot_pf_c", "dfx_tot_pf_c",
              unname(TRTS), TRT_CTRL, "anlagepaket_pre", "adv_segment_pre",
              "d_deposit", "eq_share_of_w_pre_mean")
stk <- setDT(read_parquet(stk_path, col_select = all_of(stk_cols), mmap = FALSE))
stk <- pb_prep_est(stk[!is.na(MDate)], win, REF, drop_nonpos_wealth)

ep <- setDT(read_parquet(ep_path))[usable == TRUE]
setorder(ep, dd_start)

# ---- Build the counterfactual ------------------------------------------------
sb_price_panel(t60_path, px_file, from = "2010-01-31", to = "2024-12-31", force = REBUILD_PX)
sb <- sb_build_equity(ep, stk[, .(Bp_ID, ep_id)], pos_path, px_file, max_stale_d = MAX_STALE_D)

d <- merge(stk, sb, by = c("Bp_ID", "ep_id", "MDate"), all.x = TRUE)
d <- sb_outcomes(d, ref = REF)

# ---- Sample ------------------------------------------------------------------
# equity at the freeze month, a counterfactual anchored there, and enough T60
# coverage of that equity for the counterfactual to mean anything
d[, has_ref := any(rel_month == REF & !is.na(sb_val)), by = .(Bp_ID, ep_id)]
ce <- \(x) uniqueN(x, by = c("Bp_ID", "ep_id"))
CHK$sample <- data.table(
  step = c("07 sample", "equity at m = -1", "counterfactual at m = -1",
           sprintf("T60 covers >= %.0f%% of frozen equity", 100 * COVER_MIN),
           sprintf("T60 covers >= %.0f%% (strict)", 100 * COVER_STRICT)),
  client_eps = c(ce(d), ce(d[equity_pre > 0]), ce(d[equity_pre > 0 & has_ref]),
                 ce(d[equity_pre > 0 & has_ref & cov0 >= COVER_MIN]),
                 ce(d[equity_pre > 0 & has_ref & cov0 >= COVER_STRICT])))
print(CHK$sample)

est_dt <- pb_add_ids(d[equity_pre > 0 & has_ref & cov0 >= COVER_MIN])
raw_dt <- copy(est_dt)
w_vars <- c(SB_OUTCOMES, "act_eq_of_pf", "sb_passive_of_pf", "sb_gap_of_pf")
est_dt[, (w_vars) := lapply(.SD, pb_winsor, p = p_win), .SDcols = w_vars]

# =============================================================================
# ESTIMATION
# =============================================================================
mods <- paths <- avgs <- list()
for (nm in names(TRTS)) {
  trt <- TRTS[[nm]]
  rhs <- pb_rhs(trt, TRT_CTRL, REF)
  m <- pb_est(w_vars, est_dt, rhs)
  mods[[nm]] <- m
  paths[[nm]] <- rbindlist(lapply(w_vars, \(v) pb_get_ct(m[[v]], v, trt, REF)))[, treatment := nm]
  avgs[[nm]] <- rbindlist(lapply(w_vars, \(v) rbindlist(lapply(AVG_BANDS, \(b)
    cbind(data.table(treatment = nm, outcome = v, band = sprintf("[%d,%d]", b[1], b[2])),
          pb_avg(m[[v]], trt, b[1], b[2]))))))
  etable(m[SB_OUTCOMES], keep = paste0("%", trt), tex = TRUE, replace = TRUE,
         file = f_out(sprintf("sb_es_%s.tex", nm)))
}
paths <- rbindlist(paths); avgs <- rbindlist(avgs)
avgs[, `:=`(t = avg / se, ci_lo = avg - 1.96 * se, ci_hi = avg + 1.96 * se)]
fwrite(paths, f_out("sb_event_study_paths.csv"))
fwrite(avgs,  f_out("sb_pooled_averages.csv"))

# robustness: only client-episodes where T60 prices ~all of the frozen equity
strict_dt <- pb_add_ids(copy(raw_dt)[cov0 >= COVER_STRICT])
strict_dt[, (w_vars) := lapply(.SD, pb_winsor, p = p_win), .SDcols = w_vars]
avg_strict <- rbindlist(lapply(names(TRTS), \(nm) {
  m <- pb_est(SB_OUTCOMES, strict_dt, pb_rhs(TRTS[[nm]], TRT_CTRL, REF))
  rbindlist(lapply(SB_OUTCOMES, \(v) rbindlist(lapply(AVG_BANDS, \(b)
    cbind(data.table(treatment = nm, outcome = v, band = sprintf("[%d,%d]", b[1], b[2])),
          pb_avg(m[[v]], TRTS[[nm]], b[1], b[2]))))))
}))
avg_strict[, t := avg / se]
fwrite(avg_strict, f_out("sb_pooled_averages_strict.csv"))

# =============================================================================
# PLOTS
# =============================================================================
sb_theme <- theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

for (nm in names(TRTS)) {
  pd <- paths[treatment == nm & outcome %in% c("act_eq", "sb_passive")]
  pd[, series := SB_LABS[outcome]]
  p <- ggplot(pd, aes(rel_month, est, colour = series, fill = series)) +
    geom_ribbon(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), alpha = 0.15, colour = NA) +
    geom_line(linewidth = 0.7) + geom_point(size = 1.3) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_vline(xintercept = REF + 0.5, linetype = "dashed", linewidth = 0.4) +
    scale_colour_manual(values = c("#C0392B", "#41729F"), aesthetics = c("colour", "fill")) +
    scale_x_continuous(breaks = seq(win[1], win[2], 2)) +
    labs(x = "Months relative to pre-crisis month", y = "Share of pre-period equity",
         colour = NULL, fill = NULL,
         title = sprintf("Equity sleeve: actual vs stock-level buy-and-hold (%s)", nm),
         subtitle = sprintf("%s | frozen holdings valued at T60 prices | N = %s; 95%% CI clustered by advisor",
                            TRT_LABS[[nm]], format(nobs(mods[[nm]]$sb_gap), big.mark = "'"))) +
    sb_theme
  ggsave(f_out(sprintf("sb_actual_vs_buyhold_%s.pdf", nm)), p, width = 8, height = 5)
}

p_gap <- ggplot(paths[outcome == "sb_gap"], aes(rel_month, est, colour = treatment, fill = treatment)) +
  geom_ribbon(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.7) + geom_point(size = 1.3) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_vline(xintercept = REF + 0.5, linetype = "dashed", linewidth = 0.4) +
  scale_colour_manual(values = c(BROAD = "#41729F", NARROW = "#C0392B"),
                      aesthetics = c("colour", "fill")) +
  scale_x_continuous(breaks = seq(win[1], win[2], 2)) +
  labs(x = "Months relative to pre-crisis month", y = "Share of pre-period equity",
       colour = NULL, fill = NULL,
       title = "Equity active gap: actual minus stock-level buy-and-hold",
       subtitle = "Positive = advised clients did better than holding their m=-1 stocks. 95% CI clustered by advisor") +
  sb_theme
ggsave(f_out("sb_gap_coefplot.pdf"), p_gap, width = 8, height = 5)

# =============================================================================
# CHECKS
# =============================================================================
# (a) T60 coverage and quote staleness, by episode
CHK$cover <- raw_dt[rel_month == REF, .(
  client_eps = .N,
  med_cov0 = median(cov0, na.rm = TRUE),
  sh_cov_ge_99 = mean(cov0 >= 0.99, na.rm = TRUE),
  med_n_eq = median(n_eq), med_n_eq_px = median(n_eq_px)), keyby = ep_id]
CHK$stale <- raw_dt[, .(med_px_age_d = median(px_age_max, na.rm = TRUE),
                        p95_px_age_d = quantile(px_age_max, .95, na.rm = TRUE),
                        sh_month_uncovered = mean(cov_m < COVER_MIN, na.rm = TRUE)), keyby = ep_id]

# (b) zero-trade client-episodes: the gap must be ~0
raw_dt[, traded := any(abs(dq_tot_pf) > 1, na.rm = TRUE), by = .(Bp_ID, ep_id)]
zt <- raw_dt[traded == FALSE & rel_month > REF]
CHK$zero_trade <- data.table(
  client_eps = uniqueN(zt, by = c("Bp_ID", "ep_id")), obs = nrow(zt),
  mean_gap = mean(zt$sb_gap, na.rm = TRUE), median_gap = median(zt$sb_gap, na.rm = TRUE),
  p05 = quantile(zt$sb_gap, .05, na.rm = TRUE), p95 = quantile(zt$sb_gap, .95, na.rm = TRUE),
  share_abs_gt_1pct = mean(abs(zt$sb_gap) > 0.01, na.rm = TRUE))
# by episode and by quote age: is the error staleness, or a price-basis mismatch?
CHK$zero_trade_ep <- zt[, .(client_eps = uniqueN(paste(Bp_ID, ep_id)),
                            median_gap = median(sb_gap, na.rm = TRUE),
                            p05 = quantile(sb_gap, .05, na.rm = TRUE),
                            p95 = quantile(sb_gap, .95, na.rm = TRUE),
                            sh_abs_gt_1pct = mean(abs(sb_gap) > 0.01, na.rm = TRUE),
                            med_px_age = median(px_age_max, na.rm = TRUE)), keyby = ep_id]
CHK$zero_trade_age <- zt[!is.na(px_age_max), .(
  obs = .N, median_gap = median(sb_gap, na.rm = TRUE),
  sh_abs_gt_1pct = mean(abs(sb_gap) > 0.01, na.rm = TRUE)),
  keyby = .(px_age = cut(px_age_max, c(-1, 0, 3, 7, 15, 31, Inf),
                         labels = c("0d", "1-3d", "4-7d", "8-15d", "16-31d", ">31d")))]

# (c) outcomes at the freeze month must be exactly 0
CHK$ref_zero <- raw_dt[rel_month == REF, .(obs = .N,
                                           max_abs_sb = max(abs(sb_passive), na.rm = TRUE),
                                           max_abs_act = max(abs(act_eq), na.rm = TRUE))]

# (d) T60 counterfactual vs the implied-price counterfactual of passive_benchmark.R
pb_file <- "../../output/passive_bm/pb_event_study_paths.csv"
CHK$vs_pb <- if (file.exists(pb_file)) {
  pb <- fread(pb_file)[outcome == "pb_gap", .(treatment, rel_month, pb_gap = est)]
  cmp <- merge(pb, paths[outcome == "sb_gap_of_pf", .(treatment, rel_month, sb_gap_of_pf = est)],
               by = c("treatment", "rel_month"))
  cmp[, .(months = .N, cor = cor(pb_gap, sb_gap_of_pf),
          mean_pb = mean(pb_gap), mean_sb = mean(sb_gap_of_pf)), by = treatment]
} else data.table(note = "passive_benchmark.R output not found; run it first")

# (e) how big is the equity sleeve here
CHK$sleeve <- raw_dt[rel_month == REF, .(med_equity_share_of_pf = median(equity_pre / tot_pf_pre,
                                                                        na.rm = TRUE),
                                         med_equity_pre_k = median(equity_pre) / 1e3), keyby = ep_id]

# (g) VALIDATED EPISODES ONLY
# The zero-trade test is a pass/fail on the price basis: if a client traded
# nothing, the counterfactual IS the actual portfolio, so any gap is T60 not
# reconciling with the position file. Episodes that fail are re-estimated out.
# In practice this keeps the 'Bestand' price regime (2021-11 onwards, daily
# quotes) and drops the 'Buchung' era, whose booking-derived prices drift
# against the value/quantity basis of the positions.
ok_eps <- CHK$zero_trade_ep[abs(median_gap) < 1e-4 & sh_abs_gt_1pct < 0.05, ep_id]
CHK$valid <- CHK$zero_trade_ep[, .(ep_id, median_gap, sh_abs_gt_1pct,
                                   passes = ep_id %chin% ok_eps)]
if (length(ok_eps)) {
  v_dt <- pb_add_ids(copy(raw_dt)[ep_id %chin% ok_eps])
  v_dt[, (w_vars) := lapply(.SD, pb_winsor, p = p_win), .SDcols = w_vars]
  v_paths <- v_avgs <- list()
  for (nm in names(TRTS)) {
    m <- pb_est(SB_OUTCOMES, v_dt, pb_rhs(TRTS[[nm]], TRT_CTRL, REF))
    v_paths[[nm]] <- rbindlist(lapply(SB_OUTCOMES, \(v) pb_get_ct(m[[v]], v, TRTS[[nm]], REF)))[
      , treatment := nm]
    v_avgs[[nm]] <- rbindlist(lapply(SB_OUTCOMES, \(v) rbindlist(lapply(AVG_BANDS, \(b)
      cbind(data.table(treatment = nm, outcome = v, band = sprintf("[%d,%d]", b[1], b[2])),
            pb_avg(m[[v]], TRTS[[nm]], b[1], b[2]))))))
  }
  v_paths <- rbindlist(v_paths)
  v_avgs  <- rbindlist(v_avgs)[, t := avg / se]
  fwrite(v_paths, f_out("sb_event_study_paths_validated.csv"))
  fwrite(v_avgs,  f_out("sb_pooled_averages_validated.csv"))
  CHK$valid_avgs <- v_avgs

  vp <- v_paths[outcome %in% c("act_eq", "sb_passive", "sb_gap")]
  vp[, series := SB_LABS[outcome]]
  p_v <- ggplot(vp, aes(rel_month, est, colour = series, fill = series)) +
    geom_ribbon(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), alpha = 0.15, colour = NA) +
    geom_line(linewidth = 0.7) + geom_point(size = 1.2) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_vline(xintercept = REF + 0.5, linetype = "dashed", linewidth = 0.4) +
    facet_wrap(~treatment) +
    scale_colour_manual(values = c("#C0392B", "#41729F", "grey25"), aesthetics = c("colour", "fill")) +
    scale_x_continuous(breaks = seq(win[1], win[2], 2)) +
    labs(x = "Months relative to pre-crisis month", y = "Share of pre-period equity",
         colour = NULL, fill = NULL,
         title = "Equity sleeve, episodes where T60 reconciles exactly",
         subtitle = sprintf("Episodes: %s | frozen holdings at T60 daily prices | 95%% CI clustered by advisor",
                            paste(ok_eps, collapse = ", "))) +
    sb_theme
  ggsave(f_out("sb_validated_episodes.pdf"), p_v, width = 10, height = 5)
}

md <- function(x) {
  x <- as.data.table(x)
  for (j in names(x)) if (is.numeric(x[[j]])) set(x, j = j, value = signif(x[[j]], 4))
  c(paste("|", paste(names(x), collapse = " | "), "|"),
    paste("|", paste(rep("---", ncol(x)), collapse = " | "), "|"),
    apply(x, 1, \(r) paste("|", paste(format(r, trim = TRUE), collapse = " | "), "|")))
}
writeLines(c(
  "# Stock-level buy-and-hold (T60 prices) - checks", "",
  sprintf("Generated %s | quotes older than %d days unused | coverage cut %.2f | window [%d, %d]",
          format(Sys.time(), "%Y-%m-%d %H:%M"), MAX_STALE_D, COVER_MIN, win[1], win[2]), "",
  "T60_Aktienkurse covers EQUITIES ONLY (no funds, bonds, structured products),",
  "so everything here is the equity sleeve of the portfolio.", "",
  "## Sample", "", md(CHK$sample), "",
  "## (a) T60 coverage of the frozen equity at the freeze month", "", md(CHK$cover), "",
  "## (b) Quote staleness and months below the coverage cut", "", md(CHK$stale), "",
  "## (c) Zero-trade client-episodes: sb_gap should be ~0", "", md(CHK$zero_trade), "",
  "By episode (svb_cs_202303 is the only one fully inside the dense 'Bestand' price regime):", "",
  md(CHK$zero_trade_ep), "",
  "By age of the quote used that month:", "", md(CHK$zero_trade_age), "",
  "## (d) Outcomes at m = -1 (must be exactly 0)", "", md(CHK$ref_zero), "",
  "## (e) T60 counterfactual vs the implied-price one (passive_benchmark.R)", "",
  "Both scaled by pre-period portfolio. The T60 version covers only equity, the implied-price",
  "version the whole portfolio, so the levels differ; the correlation is the interesting part.", "",
  md(CHK$vs_pb), "",
  "## (f) Size of the equity sleeve", "", md(CHK$sleeve), "",
  "## (g) Which episodes does T60 reconcile with? (zero-trade test)", "",
  "An episode passes if, for clients who traded nothing, the gap is ~0: |median| < 1e-4 and",
  "fewer than 5% of months beyond +/-1%. Failing episodes are all in the 'Buchung' price regime.", "",
  md(CHK$valid), "",
  if (!is.null(CHK$valid_avgs)) c("Pooled averages on the passing episodes only:", "",
                                  md(CHK$valid_avgs), "") else
    "No episode passes; the T60 counterfactual is not usable on this sample."),
  f_out("checks.md"))

cat("\nStock-level buy-and-hold outputs in", normalizePath(OUT), "\n")
print(avgs[outcome %in% SB_OUTCOMES, .(treatment, outcome, band, avg = round(avg, 4),
                                       se = round(se, 4), t = round(t, 2))])
