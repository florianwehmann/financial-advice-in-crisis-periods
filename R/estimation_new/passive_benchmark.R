# =============================================================================
# passive_benchmark.R -- does advice do anything ACTIVE, or is the price effect
# just risk exposure?
#
# The existing "PF price change" outcome mixes two things: what the client HELD
# when the crisis hit (composition / beta) and what the client DID during it
# (trading and timing). This script separates them with a buy-and-hold benchmark:
#
#   pb_price  passive: quantities frozen at rel_month -1, valued at realized
#             month-m prices in CHF (price and FX move, nothing is traded)
#   act_price actual price+FX change, anchored at the same month
#   pb_gap    act_price - pb_price = the active component
#
# plus time-weighted returns (act_twr, pb_twr, twr_gap), which are immune to
# flows altogether.
#
# Everything downstream (sample, FE, controls, clustering, window, reference
# month) follows 07_estim_v3.R. Reads existing data and copies functions through
# pb_funs.R; writes only into output/passive_bm/.
#
# Run from R/estimation_new in a fresh session. Parameters that can be set
# beforehand: N_DRAWS (placebo draws, default 20), REBUILD_PX (force a rebuild
# of the price panel), RUN_PLACEBO.
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(arrow)
  library(ggplot2)
  library(duckdb)
  library(lubridate)
})
source("pb_funs.R")

# ---- Config ------------------------------------------------------------------
OUT <- "../../output/passive_bm"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

stk_path <- "../../data/stk2.parquet"          # stacked panel (07's input)
pos_path <- "../../data/pos_m1.parquet"        # position level, 36m rows
posm_path <- "../../data/pos_pf.parquet"       # client x month panel (placebo rebuild)
ep_path  <- "../../data/episodes_short.parquet"
px_file  <- file.path(OUT, "pb_price_panel.parquet")

TRTS      <- c(BROAD = "treat_inv_a_p", NARROW = "treat_perfinv_a_p")
TRT_LABS  <- c(BROAD  = "BROAD: adv-initiated personal investment contacts",
               NARROW = "NARROW: adv-initiated performance review + investment advice")
TRT_CTRL  <- "treat_inv_c_p"
REF       <- -1L
win       <- c(-8L, 12L)
p_win     <- 0.99
drop_nonpos_wealth <- TRUE
AVG_BANDS <- list(c(0L, 1L), c(2L, 4L), c(5L, 10L))

# placebo (same design as 07_estim_v3_placebo.R)
if (!exists("RUN_PLACEBO")) RUN_PLACEBO <- TRUE
if (!exists("N_DRAWS"))     N_DRAWS     <- 20L
if (!exists("REBUILD_PX"))  REBUILD_PX  <- FALSE
PL_SEED     <- 20260915L
PLACEBO_TRT <- "NARROW"
PL_OUTCOMES <- c("act_price", "pb_price", "pb_gap")
SMP_PANEL <- as.Date(c("2011-03-31", "2024-12-31"))
PRE_M <- 6L; POST_M <- 8L; BUF <- 2L; STRICT <- FALSE; MIN_GAP <- 6L

setFixest_nthreads(parallel::detectCores())
setFixest_notes(FALSE)
CHK <- list()   # collected for checks.md

# ---- Load the crisis panel (read-only) ---------------------------------------
stk_cols <- c("Bp_ID", "ep_id", "MDate", "advisor_id", "rel_month",
              "tot_wealth", "tot_wealth_pre", "tot_wealth_pre_mean",
              "tot_pf", "tot_pf_pre", "dp_tot_pf", "dfx_tot_pf", "dq_tot_pf",
              "dp_tot_pf_c", "dfx_tot_pf_c", unname(TRTS), TRT_CTRL,
              "anlagepaket_pre", "adv_segment_pre", "d_deposit", "eq_share_of_w_pre_mean")
crisis <- setDT(read_parquet(stk_path, col_select = all_of(stk_cols), mmap = FALSE))
crisis <- crisis[!is.na(MDate)]
crisis <- pb_prep_est(crisis, win, REF, drop_nonpos_wealth)

ep <- setDT(read_parquet(ep_path))[usable == TRUE]
setorder(ep, dd_start)
crisis_len <- ep$n_months + 1L

# ---- Passive benchmark for the crisis episodes -------------------------------
pb_price_panel(pos_path, px_file, from = "2010-01-31", to = "2024-12-31", force = REBUILD_PX)

pb_c <- pb_build_passive(ep, crisis[, .(Bp_ID, ep_id)], pos_path, px_file)
crisis <- merge(crisis, pb_c, by = c("Bp_ID", "ep_id", "MDate"), all.x = TRUE)
crisis <- pb_outcomes(crisis, ref = REF)

# main sample: the 07 sample, plus a pre-period portfolio to scale by and a
# benchmark that exists at the freeze month
crisis[, has_ref := any(rel_month == REF & !is.na(pb_pf)), by = .(Bp_ID, ep_id)]
CHK$sample <- data.table(
  step = c("07 sample", "tot_pf_pre > 0", "passive value at m = -1"),
  client_eps = c(uniqueN(crisis, by = c("Bp_ID", "ep_id")),
                 uniqueN(crisis[tot_pf_pre > 0], by = c("Bp_ID", "ep_id")),
                 uniqueN(crisis[tot_pf_pre > 0 & has_ref], by = c("Bp_ID", "ep_id"))))
est_dt <- pb_add_ids(crisis[tot_pf_pre > 0 & has_ref])

# winsorize the outcomes exactly as 07 winsorizes its flow variables
raw_dt <- copy(est_dt)                       # unwinsorized, for the reconciliation
w_vars <- c(PB_OUTCOMES, "act_price_w", "pb_price_w", "pb_gap_w", "act_price_po")
est_dt[, (w_vars) := lapply(.SD, pb_winsor, p = p_win), .SDcols = w_vars]

# =============================================================================
# ESTIMATION
# =============================================================================
mods <- paths <- avgs <- list()
for (nm in names(TRTS)) {
  trt <- TRTS[[nm]]
  rhs <- pb_rhs(trt, TRT_CTRL, REF)
  m <- pb_est(c(PB_OUTCOMES, "act_price_w", "pb_price_w", "pb_gap_w"), est_dt, rhs)
  mods[[nm]] <- m
  paths[[nm]] <- rbindlist(lapply(PB_OUTCOMES, \(v) pb_get_ct(m[[v]], v, trt, REF)))[
    , `:=`(treatment = nm)]
  avgs[[nm]] <- rbindlist(lapply(names(m), \(v) rbindlist(lapply(AVG_BANDS, \(b)
    cbind(data.table(treatment = nm, outcome = v,
                     band = sprintf("[%d,%d]", b[1], b[2])),
          pb_avg(m[[v]], trt, b[1], b[2]))))))
  etable(m[PB_OUTCOMES], keep = paste0("%", trt), tex = TRUE, replace = TRUE,
         file = file.path(OUT, sprintf("pb_es_%s.tex", nm)))
}
paths <- rbindlist(paths); avgs <- rbindlist(avgs)
avgs[, `:=`(t = avg / se, ci_lo = avg - 1.96 * se, ci_hi = avg + 1.96 * se)]
fwrite(paths, file.path(OUT, "pb_event_study_paths.csv"))
fwrite(avgs,  file.path(OUT, "pb_pooled_averages.csv"))

# pooled-average table as LaTeX (one panel per treatment)
tex <- c("% requires \\usepackage{booktabs}", "\\begin{tabular}{lrrrrrr}", "\\toprule")
for (nm in names(TRTS)) {
  tex <- c(tex, sprintf("\\multicolumn{7}{l}{\\textit{%s}} \\\\",
                        gsub("([_&%])", "\\\\\\1", TRT_LABS[[nm]])), "\\addlinespace",
           paste("Outcome", paste(vapply(AVG_BANDS, \(b) sprintf("\\multicolumn{2}{c}{m %d--%d}", b[1], b[2]), ""),
                                  collapse = " & "), sep = " & ") |> paste("\\\\"),
           "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}",
           " & Est. & SE & Est. & SE & Est. & SE \\\\", "\\midrule")
  for (v in PB_OUTCOMES) {
    a <- avgs[treatment == nm & outcome == v]
    setkey(a, band)
    cells <- unlist(lapply(AVG_BANDS, \(b) {
      r <- a[band == sprintf("[%d,%d]", b[1], b[2])]
      c(sprintf("%.4f", r$avg), sprintf("(%.4f)", r$se))
    }))
    tex <- c(tex, paste(c(PB_LABS[[v]], cells), collapse = " & ") |> paste("\\\\"))
  }
  tex <- c(tex, "\\addlinespace")
}
writeLines(c(tex, "\\bottomrule", "\\end{tabular}"), file.path(OUT, "pb_pooled_averages.tex"))

# =============================================================================
# PLOTS
# =============================================================================
pb_theme <- theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
sub_n <- function(nm) sprintf("%s | N = %s client-episode-months; 95%% CI clustered by advisor",
                              TRT_LABS[[nm]], format(nobs(mods[[nm]]$pb_gap), big.mark = "'"))

# 1. actual vs passive
for (nm in names(TRTS)) {
  d <- paths[treatment == nm & outcome %in% c("act_price", "pb_price")]
  d[, series := PB_LABS[outcome]]
  p <- ggplot(d, aes(rel_month, est, colour = series, fill = series)) +
    geom_ribbon(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), alpha = 0.15, colour = NA) +
    geom_line(linewidth = 0.7) + geom_point(size = 1.3) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_vline(xintercept = REF + 0.5, linetype = "dashed", linewidth = 0.4) +
    scale_colour_manual(values = c("#C0392B", "#41729F"), aesthetics = c("colour", "fill")) +
    scale_x_continuous(breaks = seq(win[1], win[2], 2)) +
    labs(x = "Months relative to pre-crisis month",
         y = "Share of pre-period portfolio", colour = NULL, fill = NULL,
         title = sprintf("Actual vs passive price effect of advice (%s)", nm),
         subtitle = sub_n(nm)) + pb_theme
  ggsave(file.path(OUT, sprintf("pb_actual_vs_passive_%s.pdf", nm)), p, width = 8, height = 5)
}

# 2. the active gap
d <- paths[outcome == "pb_gap"]
p_gap <- ggplot(d, aes(rel_month, est, colour = treatment, fill = treatment)) +
  geom_ribbon(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.7) + geom_point(size = 1.3) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_vline(xintercept = REF + 0.5, linetype = "dashed", linewidth = 0.4) +
  scale_colour_manual(values = c(BROAD = "#41729F", NARROW = "#C0392B"),
                      aesthetics = c("colour", "fill")) +
  scale_x_continuous(breaks = seq(win[1], win[2], 2)) +
  labs(x = "Months relative to pre-crisis month",
       y = "Active gap, share of pre-period portfolio", colour = NULL, fill = NULL,
       title = "Active component of advice: actual minus buy-and-hold",
       subtitle = "BROAD = adv-init. investment contacts; NARROW = + performance review. 95% CI clustered by advisor") +
  pb_theme
ggsave(file.path(OUT, "pb_gap_coefplot.pdf"), p_gap, width = 8, height = 5)

# TWR version of both
d <- paths[outcome %in% c("act_twr", "pb_twr", "twr_gap")]
d[, series := PB_LABS[outcome]]
p_twr <- ggplot(d, aes(rel_month, est, colour = series, fill = series)) +
  geom_ribbon(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_vline(xintercept = REF + 0.5, linetype = "dashed", linewidth = 0.4) +
  facet_wrap(~treatment) +
  scale_colour_manual(values = c("#C0392B", "#41729F", "grey25"), aesthetics = c("colour", "fill")) +
  scale_x_continuous(breaks = seq(win[1], win[2], 2)) +
  labs(x = "Months relative to pre-crisis month", y = "Time-weighted return",
       colour = NULL, fill = NULL,
       title = "Time-weighted returns: actual, passive and the gap",
       subtitle = "Flow-free version of the same decomposition") + pb_theme
ggsave(file.path(OUT, "pb_twr_actual_vs_passive.pdf"), p_twr, width = 10, height = 5)

# =============================================================================
# SANITY CHECKS
# =============================================================================
# (a) zero-trade client-episodes: the gap must be ~0
raw_dt[, traded := any(abs(dq_tot_pf) > 1, na.rm = TRUE) & any(rel_month > REF),
       by = .(Bp_ID, ep_id)]
zt <- raw_dt[traded == FALSE & rel_month > REF]
CHK$zero_trade <- data.table(
  client_eps = uniqueN(zt, by = c("Bp_ID", "ep_id")), obs = nrow(zt),
  mean_gap = mean(zt$pb_gap, na.rm = TRUE), median_gap = median(zt$pb_gap, na.rm = TRUE),
  p05 = quantile(zt$pb_gap, .05, na.rm = TRUE), p95 = quantile(zt$pb_gap, .95, na.rm = TRUE),
  share_abs_gt_1pct = mean(abs(zt$pb_gap) > 0.01, na.rm = TRUE),
  share_abs_gt_5pct = mean(abs(zt$pb_gap) > 0.05, na.rm = TRUE))

# (b) pb_price at the freeze month
CHK$ref_zero <- raw_dt[rel_month == REF, .(obs = .N, max_abs_pb = max(abs(pb_price), na.rm = TRUE),
                                           max_abs_act = max(abs(act_price), na.rm = TRUE))]

# (c) how much of the benchmark rests on carried-forward prices
CHK$stale <- raw_dt[, .(client_eps = uniqueN(paste(Bp_ID, ep_id)),
                        sh_pos_stale = sum(n_pos_stale) / sum(n_pos),
                        sh_val_stale = sum(abs(pb_val_stale), na.rm = TRUE) / sum(abs(pb_pf), na.rm = TRUE),
                        max_px_age_m = max(max_px_age, na.rm = TRUE),
                        sh_pos_nopx = sum(n_pos_nopx) / sum(n_pos),
                        pb_cover = median(pb_pf[rel_month == REF] / tot_pf_pre[rel_month == REF],
                                          na.rm = TRUE)), keyby = ep_id]

# (d) clients per rel_month per episode
CHK$panel <- dcast(raw_dt[, .N, by = .(ep_id, rel_month)], rel_month ~ ep_id, value.var = "N")

# (e) reconciliation against the existing PF-price coefficients
raw_dt[, dp_pct_to_pf := dp_tot_pf_c / tot_pf_pre]   # the outcome as 07 builds it
rec <- rbindlist(lapply(names(TRTS), \(nm) {
  rhs <- pb_rhs(TRTS[[nm]], TRT_CTRL, REF)
  mm  <- pb_est(c("dp_pct_to_pf", "act_price_po"), raw_dt, rhs)
  a <- pb_get_ct(mm$dp_pct_to_pf, "07 dp_pct_to_pf", TRTS[[nm]], REF)
  b <- pb_get_ct(mm$act_price_po, "pb act_price_po", TRTS[[nm]], REF)
  merge(a[, .(rel_month, est_07 = est)], b[, .(rel_month, est_pb = est)], by = "rel_month")[
    , .(treatment = nm, max_abs_diff = max(abs(est_07 - est_pb)),
        cor = cor(est_07, est_pb))]
}))
CHK$reconcile <- rec

# write checks.md
md <- function(d) c(paste("|", paste(names(d), collapse = " | "), "|"),
                    paste("|", paste(rep("---", ncol(d)), collapse = " | "), "|"),
                    apply(d, 1, \(r) paste("|", paste(format(r, trim = TRUE), collapse = " | "), "|")))
writeLines(c(
  "# Passive benchmark - sanity checks", "",
  sprintf("Generated %s | window [%d, %d] | reference month %d | winsorized at %.2f",
          format(Sys.time(), "%Y-%m-%d %H:%M"), win[1], win[2], REF, p_win), "",
  "## Sample", "", md(CHK$sample), "",
  "## (a) Zero-trade client-episodes: pb_gap should be ~0", "",
  "No trade in the window means the actual portfolio IS the frozen one, so any gap is a",
  "pricing artefact (stale prices, corporate actions, positions without a quantity).", "",
  md(CHK$zero_trade), "",
  "## (b) Outcomes at the freeze month m = -1 (must be exactly 0)", "", md(CHK$ref_zero), "",
  "## (c) Carried-forward prices and benchmark coverage, by episode", "",
  "`sh_pos_stale` = position-months valued at a carried price; `sh_val_stale` = their value share;",
  "`pb_cover` = median passive value at m = -1 over tot_pf_pre (1 = the benchmark covers the whole portfolio).", "",
  md(CHK$stale), "",
  "## (d) Observations per rel_month per episode (panel attrition)", "", md(CHK$panel), "",
  "## (e) Reconciliation with the existing PF-price coefficients", "",
  "Estimated on the same rows, unwinsorized: 07's `dp_tot_pf_c / tot_pf_pre` against the",
  "anchored price-only version built here. Anchoring shifts a client-episode constant, which",
  "the `ci` fixed effect absorbs, so the coefficients must agree to numerical precision.", "",
  md(CHK$reconcile), ""), file.path(OUT, "checks.md"))

# =============================================================================
# PLACEBO: the same outcomes on calm-month pseudo-episodes
# =============================================================================
if (RUN_PLACEBO && N_DRAWS > 0) {
  source("../estimation/stress_events.R")        # EVENTS (data only)
  posm <- setDT(read_parquet(posm_path, mmap = FALSE))
  # 06_prep.R creates these AFTER writing pos_pf.parquet, so rebuild them here
  posm[, `:=`(pf_share_of_w  = tot_pf / tot_wealth,
              eq_share_of_pf = equity / tot_pf,
              eq_share_of_w  = equity / tot_wealth)]
  setorder(posm, Bp_ID, MDate)
  posm[, d_deposit := c(0, diff(deposit_cash_check)), by = Bp_ID]
  setkey(posm, Bp_ID, MDate)

  contacts <- c("inv_a_p", "perfinv_a_p", "inv_c_p")
  pl_cols <- c("Bp_ID", "ep_id", "MDate", "advisor_id", "rel_month",
               "tot_wealth", "tot_wealth_pre", "tot_wealth_pre_mean",
               "tot_pf", "tot_pf_pre", "dp_tot_pf", "dfx_tot_pf", "dq_tot_pf",
               "dp_tot_pf_c", "dfx_tot_pf_c", paste0("treat_", contacts),
               "anlagepaket_pre", "adv_segment_pre", "d_deposit", "eq_share_of_w_pre_mean")

  stress_m <- pb_stress_months(EVENTS, BUF)
  elig <- setNames(lapply(sort(unique(crisis_len)),
                          \(L) pb_eligible_starts(L, stress_m, SMP_PANEL, PRE_M, POST_M, STRICT)),
                   sort(unique(crisis_len)))

  trt <- TRTS[[PLACEBO_TRT]]
  rhs <- pb_rhs(trt, TRT_CTRL, REF)
  draws <- vector("list", N_DRAWS)
  for (r in seq_len(N_DRAWS)) {
    t0 <- Sys.time()
    ep_r <- pb_draw_eps(PL_SEED + r, elig, crisis_len, MIN_GAP, PRE_M, POST_M)
    d_r  <- pb_prep_est(pb_build_stk(ep_r, posm, contacts, pl_cols), win, REF, drop_nonpos_wealth)
    pb_r <- pb_build_passive(ep_r, d_r[, .(Bp_ID, ep_id)], pos_path, px_file, quiet = TRUE)
    d_r  <- pb_outcomes(merge(d_r, pb_r, by = c("Bp_ID", "ep_id", "MDate"), all.x = TRUE), ref = REF)
    d_r[, has_ref := any(rel_month == REF & !is.na(pb_pf)), by = .(Bp_ID, ep_id)]
    d_r  <- pb_add_ids(d_r[tot_pf_pre > 0 & has_ref])
    d_r[, (PL_OUTCOMES) := lapply(.SD, pb_winsor, p = p_win), .SDcols = PL_OUTCOMES]
    m_r  <- pb_est(PL_OUTCOMES, d_r, rhs)
    draws[[r]] <- rbindlist(lapply(PL_OUTCOMES, \(v) pb_get_ct(m_r[[v]], v, trt, REF)))[
      , `:=`(draw = r, eps = paste(format(ep_r$dd_start, "%Y-%m"), collapse = " "))]
    fwrite(rbindlist(draws), file.path(OUT, sprintf("pb_placebo_draws_%s.csv", PLACEBO_TRT)))
    cat(sprintf("placebo draw %d/%d (%.0fs): %s\n", r, N_DRAWS,
                as.numeric(difftime(Sys.time(), t0, units = "secs")), draws[[r]]$eps[1]))
    rm(d_r, pb_r, m_r); gc()
  }
  draws <- rbindlist(draws)

  cr <- paths[treatment == PLACEBO_TRT & outcome %in% PL_OUTCOMES]
  lab <- \(x) factor(PB_LABS[x], levels = PB_LABS[PL_OUTCOMES])
  band <- draws[, .(lo = quantile(est, .05), hi = quantile(est, .95)), by = .(outcome, rel_month)]
  p_pl <- ggplot() +
    geom_ribbon(data = band[, outcome := lab(outcome)],
                aes(rel_month, ymin = lo, ymax = hi), fill = "#41729F", alpha = 0.18) +
    geom_line(data = copy(draws)[, outcome := lab(outcome)],
              aes(rel_month, est, group = draw), colour = "#41729F", alpha = 0.35, linewidth = 0.3) +
    geom_ribbon(data = copy(cr)[, outcome := lab(outcome)],
                aes(rel_month, ymin = est - 1.96 * se, ymax = est + 1.96 * se),
                fill = "#C0392B", alpha = 0.12) +
    geom_line(data = copy(cr)[, outcome := lab(outcome)], aes(rel_month, est),
              colour = "#C0392B", linewidth = 0.9) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_vline(xintercept = REF + 0.5, linetype = "dashed", linewidth = 0.4) +
    facet_wrap(~outcome, scales = "free_y") +
    scale_x_continuous(breaks = seq(win[1], win[2], 2)) +
    labs(x = "Months relative to (pseudo-)drawdown start", y = "Estimate",
         title = "Crisis (red, 95% CI) vs calm-month placebo draws (blue, 5-95% band)",
         subtitle = sprintf("%s | %d draws of %d calm-month episodes",
                            TRT_LABS[[PLACEBO_TRT]], uniqueN(draws$draw), length(crisis_len))) +
    pb_theme
  ggsave(file.path(OUT, sprintf("pb_gap_placebo_draws_%s.pdf", PLACEBO_TRT)), p_pl,
         width = 10, height = 5)

  # where the crisis post-average sits in the placebo distribution
  pl_avg <- draws[between(rel_month, 0L, 10L), .(avg = mean(est)), by = .(outcome, draw)]
  cr_avg <- cr[between(rel_month, 0L, 10L), .(crisis_avg = mean(est)), by = outcome]
  ri <- merge(cr_avg, pl_avg[, .(placebo_mean = mean(avg), placebo_sd = sd(avg),
                                 p05 = quantile(avg, .05), p95 = quantile(avg, .95),
                                 n_draws = .N), by = outcome], by = "outcome")
  ri[, p_two_sided := vapply(seq_len(.N), \(i) {
    a <- pl_avg[outcome == ri$outcome[i], avg]
    mean(abs(a - mean(a)) >= abs(crisis_avg[i] - mean(a)))
  }, 0)]
  print(ri)
  fwrite(ri, file.path(OUT, sprintf("pb_placebo_ri_%s.csv", PLACEBO_TRT)))
  cat(c("", "## (f) Placebo: crisis vs calm-month draws, mean effect over m 0-10", "",
        md(ri), ""), file = file.path(OUT, "checks.md"), sep = "\n", append = TRUE)
}

cat("\nPassive-benchmark outputs in", normalizePath(OUT), "\n")
print(avgs[outcome %in% PB_OUTCOMES])
