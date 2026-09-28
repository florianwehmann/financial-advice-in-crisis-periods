# =============================================================================
# total_return_split_placebo.R -- the calm-month placebo of 07_estim_v3_placebo.R,
# run on the PASSIVE/ACTIVE split of the portfolio total return.
#
# total_return_split.R shows that advised clients earn more in crises and that
# part of it is passive (what they held) and part active (what they did). The
# obvious question is whether either part is crisis-specific. Here the same
# outcomes are estimated on pseudo-episodes drawn from CALM months, away from
# every hand-dated stress window in stress_events.R.
#
#   act_idx  actual chain-linked portfolio return, rebased to rel_month -1
#   pb_idx   buy-and-hold of the rel_month -1 portfolio at realized asset returns
#   act_gap  act_idx - pb_idx, the active part
#
#   1. main placebo draw -> the three event studies
#   2. crisis vs placebo in one pooled regression (treatment x crisis dummy)
#   3. N_DRAWS draws -> where the crisis estimate falls in the placebo spread
#
# Episode drawing, panel rebuild and estimation helpers are reused read-only
# from pb_funs.R; the passive machinery from tr_funs.R. Existing files are not
# modified; everything lands in output/total_return_placebo/.
#
# Run from R/estimation_new in a fresh session.
# Parameters: N_DRAWS (default 20), PLACEBO_TRT, PF_PRE_MIN, RUN_DRAWS.
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(arrow)
  library(ggplot2)
  library(duckdb)
  library(lubridate)
})
source("pb_funs.R")   # pb_build_stk / pb_draw_eps / pb_est / pb_get_ct / pb_rhs / pb_avg
source("tr_funs.R")   # tr_asset_returns / tr_build_passive / tr_outcomes

# ---- Config ------------------------------------------------------------------
OUT <- "../../output/total_return_placebo"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

stk_path  <- "../../data/stk2.parquet"
pos_path  <- "../../data/pos_m1.parquet"     # position level (01_merge_pos.R)
posm_path <- "../../data/pos_pf.parquet"     # client x month panel (06_prep.R)
ep_path   <- "../../data/episodes_short.parquet"
ar_file   <- file.path("../../output/total_return", "tr_asset_returns.parquet")

TRTS     <- c(BROAD = "treat_inv_a_p", NARROW = "treat_perfinv_a_p")
TRT_LABS <- c(BROAD  = "BROAD: adv-initiated personal investment contacts",
              NARROW = "NARROW: adv-initiated performance review + investment advice")
TRT_CTRL <- "treat_inv_c_p"
REF      <- -1L
win      <- c(-6L, 9L)
p_win    <- 0.99
drop_nonpos_wealth <- TRUE
PF_PRE_MIN  <- 5000   # as in 07 / total_return_split.R
N_DRAWS     <- 100L
if (!exists("RUN_DRAWS"))   RUN_DRAWS   <- TRUE
PLACEBO_TRT <- "BROAD"

# placebo design, identical to 07_estim_v3_placebo.R
SMP_PANEL <- as.Date(c("2011-03-31", "2024-12-31"))
PRE_M <- 6L; POST_M <- 8L; BUF <- 2L; STRICT <- FALSE; MIN_GAP <- 6L
PL_SEED  <- 20260915L                      # same seed -> same calm months as before
POST_AVG <- c(0L, 10L)

setFixest_nthreads(parallel::detectCores())
setFixest_notes(FALSE)
CHK <- list()

# fixest with a custom FE block (pb_est is fixed to ci + te)
est_fe <- function(lhs, data, rhs, fe = "ci + te") {
  lhs_str <- if (length(lhs) == 1) lhs else sprintf("c(%s)", paste(lhs, collapse = ", "))
  m <- feols(as.formula(paste(lhs_str, "~", rhs, "|", fe)), data = data,
             vcov = ~advisor_id, lean = TRUE, fixef.tol = 1e-5, mem.clean = TRUE)
  if (length(lhs) == 1) return(setNames(list(m), lhs))
  setNames(as.list(m), lhs)
}

# ---- Inputs ------------------------------------------------------------------
source("../estimation/stress_events.R")          # EVENTS (data only)
ep_crisis <- setDT(read_parquet(ep_path))[usable == TRUE]
setorder(ep_crisis, dd_start)
crisis_len <- ep_crisis$n_months + 1L

stk_cols <- c("Bp_ID", "ep_id", "MDate", "advisor_id", "rel_month",
              "tot_wealth", "tot_wealth_pre", "tot_wealth_pre_mean", "tot_pf", "tot_pf_pre",
              "dp_tot_pf", "dfx_tot_pf", "dq_tot_pf", unname(TRTS), TRT_CTRL,
              "anlagepaket_pre", "adv_segment_pre", "d_deposit_pre_sum", "eq_share_of_w_pre_mean")

# the client x month panel the placebo episodes are rebuilt from; 06_prep.R
# creates these columns only AFTER writing pos_pf.parquet, so recreate them
contacts <- c("inv_a_p", "perfinv_a_p", "inv_c_p")
posm <- setDT(read_parquet(posm_path, mmap = FALSE))
posm[, `:=`(pf_share_of_w  = tot_pf / tot_wealth,
            eq_share_of_pf = equity / tot_pf,
            eq_share_of_w  = equity / tot_wealth)]
setorder(posm, Bp_ID, MDate)
posm[, d_deposit := c(0, diff(deposit_cash_check)), by = Bp_ID]
setkey(posm, Bp_ID, MDate)

tr_asset_returns(pos_path, ar_file, from = "2010-01-31", to = "2024-12-31")

# ---- One pipeline, used for crisis and for every placebo draw ----------------
build_outcomes <- function(ep, panel, quiet = FALSE) {
  d <- pb_prep_est(panel[!is.na(MDate)], win, REF, drop_nonpos_wealth)
  d <- d[tot_pf_pre >= PF_PRE_MIN]
  pb <- tr_build_passive(ep, d[, .(Bp_ID, ep_id)], pos_path, ar_file, quiet = quiet)
  d <- tr_outcomes(merge(d, pb, by = c("Bp_ID", "ep_id", "MDate"), all.x = TRUE),
                   ref = REF, p_win = p_win)
  d[, has_ref := any(rel_month == REF & !is.na(pb_val) & !is.na(tot_pf)), by = .(Bp_ID, ep_id)]
  pb_add_ids(d[has_ref == TRUE & !is.na(act_idx) & !is.na(pb_idx)])
}

paths_of <- function(m, trt, tag) rbindlist(lapply(TR_OUTCOMES, \(v)
  pb_get_ct(m[[v]], v, trt, REF)))[, type := tag]

# ---- Crisis baseline ---------------------------------------------------------
crisis <- build_outcomes(ep_crisis,
                         setDT(read_parquet(stk_path, col_select = all_of(stk_cols), mmap = FALSE)))
cat("crisis sample:", uniqueN(crisis, by = c("Bp_ID", "ep_id")), "client-episodes\n")

# =============================================================================
# 1. MAIN PLACEBO DRAW
# =============================================================================
stress_m <- pb_stress_months(EVENTS, BUF)
elig <- setNames(lapply(sort(unique(crisis_len)),
                        \(L) pb_eligible_starts(L, stress_m, SMP_PANEL, PRE_M, POST_M, STRICT)),
                 sort(unique(crisis_len)))
# The rebuild delivers the MONTHLY d_deposit; d_deposit_pre_sum is an episode
# concept (the sum over pre_start..pre_month), so posm cannot carry it and it is
# formed once the panel is stacked. At this point the panel still spans
# pre_start..post_end, so rel_month <= REF is exactly the pre window.
pl_cols <- unique(c(setdiff(stk_cols, "d_deposit_pre_sum"), "d_deposit"))

draw_panel <- function(ep_r) {
  p <- pb_build_stk(ep_r, posm, contacts, pl_cols)
  p[, d_deposit_pre_sum := sum(d_deposit[rel_month <= REF], na.rm = TRUE),
    by = .(Bp_ID, ep_id)]
  p[]
}

ep_pl <- pb_draw_eps(PL_SEED, elig, crisis_len, MIN_GAP, PRE_M, POST_M)
cat("\nMain placebo episodes (seed ", PL_SEED, "):\n", sep = ""); print(ep_pl)
fwrite(ep_pl, file.path(OUT, "tr_placebo_episodes_main.csv"))

placebo <- build_outcomes(ep_pl, draw_panel(ep_pl))
cat("placebo sample:", uniqueN(placebo, by = c("Bp_ID", "ep_id")), "client-episodes\n")

main_paths <- main_avgs <- list()
for (nm in names(TRTS)) {
  trt <- TRTS[[nm]]
  mc <- pb_est(TR_OUTCOMES, crisis,  pb_rhs(trt, TRT_CTRL, REF))
  mp <- pb_est(TR_OUTCOMES, placebo, pb_rhs(trt, TRT_CTRL, REF))
  main_paths[[nm]] <- rbind(paths_of(mc, trt, "Crisis"),
                            paths_of(mp, trt, "Placebo (calm)"))[, treatment := nm]
  main_avgs[[nm]] <- rbindlist(lapply(TR_OUTCOMES, \(v) rbind(
    cbind(data.table(treatment = nm, type = "Crisis", outcome = v),
          pb_avg(mc[[v]], trt, POST_AVG[1], POST_AVG[2])),
    cbind(data.table(treatment = nm, type = "Placebo (calm)", outcome = v),
          pb_avg(mp[[v]], trt, POST_AVG[1], POST_AVG[2])))))
  etable(mp, keep = paste0("%", trt), tex = TRUE, replace = TRUE,
         file = file.path(OUT, sprintf("tr_es_placebo_%s.tex", nm)))
}
main_paths <- rbindlist(main_paths); main_avgs <- rbindlist(main_avgs)[, t := avg / se]
fwrite(main_paths, file.path(OUT, "tr_paths_crisis_vs_placebo.csv"))
fwrite(main_avgs,  file.path(OUT, "tr_avgs_crisis_vs_placebo.csv"))

tr_theme <- theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
pd <- copy(main_paths)[, series := factor(TR_LABS[outcome], levels = TR_LABS)]
# p_main <- ggplot(pd, aes(rel_month, est, colour = type, fill = type)) +
#   geom_ribbon(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), alpha = 0.15, colour = NA) +
#   geom_line(linewidth = 0.7) +
#   geom_hline(yintercept = 0, linewidth = 0.3) +
#   geom_vline(xintercept = REF + 0.5, linetype = "dashed", linewidth = 0.4) +
#   facet_grid(treatment ~ series) +
#   scale_colour_manual(values = c("Crisis" = "#C0392B", "Placebo (calm)" = "#41729F"),
#                       aesthetics = c("colour", "fill")) +
#   scale_x_continuous(breaks = seq(win[1], win[2], 4)) +
#   labs(x = "Months relative to (pseudo-)drawdown start", y = "Cumulative return, rebased to m = -1",
#        colour = NULL, fill = NULL,
#        title = "Return effect of advice: crisis vs calm months, passive and active",
#        subtitle = "Passive = holding the m=-1 portfolio; active = actual minus passive. 95% CI clustered by advisor") +
#   tr_theme
# ggsave(file.path(OUT, "tr_crisis_vs_placebo.pdf"), p_main, width = 11, height = 6)
# 
# # =============================================================================
# # 2. POOLED: crisis minus placebo, in one regression
# # =============================================================================
# # Memory-safe spec (as in 07_estim_v3_placebo.R): categorical controls absorbed
# # as FE interacted with crisis, only the treatment terms interacted.
# pool_cols <- unique(c("Bp_ID", "ep_id", "MDate", "advisor_id", "rel_month", TR_OUTCOMES,
#                       unname(TRTS), TRT_CTRL, "log_w_pre_mean", "d_deposit_pre_sum",
#                       "eq_share_of_w_pre_mean", "anlagepaket_pre", "adv_segment_pre"))
# pool <- rbind(crisis[, ..pool_cols][, crisis := 1L],
#               placebo[, ..pool_cols][, crisis := 0L])
# pool[, ci := .GRP, by = .(Bp_ID, ep_id)]
# pool[, te := .GRP, by = .(MDate, ep_id)]
# 
# ev <- function(v) sprintf("i(rel_month, %s, ref = %d)", v, REF)
# wald_tab <- list(); pool_paths <- list()
# for (nm in names(TRTS)) {
#   trt <- TRTS[[nm]]
#   for (v in c(trt, TRT_CTRL)) set(pool, j = paste0(v, "_xc"), value = pool[[v]] * pool$crisis)
#   rhs_pool <- paste(ev(c(trt, TRT_CTRL, paste0(c(trt, TRT_CTRL), "_xc"),
#                          "log_w_pre_mean", "d_deposit_pre_sum", "eq_share_of_w_pre_mean")),
#                     collapse = " + ")
#   fe_pool <- "ci + te + anlagepaket_pre^rel_month^crisis + adv_segment_pre^rel_month^crisis"
#   mp <- est_fe(TR_OUTCOMES, pool, rhs_pool, fe_pool)
# 
#   pool_paths[[nm]] <- rbindlist(lapply(TR_OUTCOMES, \(v) {
#     b <- coef(mp[[v]]); V <- vcov(mp[[v]])
#     nm_p <- grep(paste0("^rel_month::-?[0-9]+:", trt, "$"), names(b), value = TRUE)
#     nm_x <- paste0(nm_p, "_xc"); keep <- nm_x %in% names(b)
#     nm_p <- nm_p[keep]; nm_x <- nm_x[keep]
#     data.table(treatment = nm, outcome = v,
#                rel_month = as.integer(sub("^rel_month::(-?[0-9]+):.*$", "\\1", nm_p)),
#                diff = b[nm_x], se = sqrt(diag(V)[nm_x]))
#   }))
#   wald_tab[[nm]] <- rbindlist(lapply(TR_OUTCOMES, \(v) {
#     w_post <- wald(mp[[v]], keep = paste0("^rel_month::[0-9]+:", trt, "_xc$"), print = FALSE)
#     w_pre  <- wald(mp[[v]], keep = paste0("^rel_month::-[0-9]+:", trt, "_xc$"), print = FALSE)
#     data.table(treatment = nm, outcome = v, F_post = w_post$stat, p_post = w_post$p,
#                F_pre = w_pre$stat, p_pre = w_pre$p)
#   }))
#   rm(mp); gc()
# }
# pool_paths <- rbindlist(pool_paths); wald_tab <- rbindlist(wald_tab)
# cat("\nJoint tests: crisis minus placebo = 0\n"); print(wald_tab)
# fwrite(pool_paths, file.path(OUT, "tr_pooled_difference_paths.csv"))
# fwrite(wald_tab,   file.path(OUT, "tr_pooled_wald.csv"))
# 
# p_diff <- ggplot(copy(pool_paths)[, series := factor(TR_LABS[outcome], levels = TR_LABS)],
#                  aes(rel_month, diff, colour = treatment, fill = treatment)) +
#   geom_ribbon(aes(ymin = diff - 1.96 * se, ymax = diff + 1.96 * se), alpha = 0.15, colour = NA) +
#   geom_line(linewidth = 0.7) +
#   geom_hline(yintercept = 0, linewidth = 0.3) +
#   geom_vline(xintercept = REF + 0.5, linetype = "dashed", linewidth = 0.4) +
#   facet_wrap(~series) +
#   scale_colour_manual(values = c(BROAD = "#41729F", NARROW = "#C0392B"),
#                       aesthetics = c("colour", "fill")) +
#   scale_x_continuous(breaks = seq(win[1], win[2], 4)) +
#   labs(x = "Months relative to (pseudo-)drawdown start", y = "Crisis minus placebo",
#        colour = NULL, fill = NULL,
#        title = "Is the return effect crisis-specific?",
#        subtitle = "Pooled regression, treatment interacted with a crisis dummy; SE clustered by advisor") +
#   tr_theme
# ggsave(file.path(OUT, "tr_crisis_minus_placebo.pdf"), p_diff, width = 10, height = 5)
# rm(pool); gc()

# =============================================================================
# 3. RANDOMIZATION DRAWS
# =============================================================================
if (RUN_DRAWS && N_DRAWS > 0) {
  trt <- TRTS[[PLACEBO_TRT]]
  rhs <- pb_rhs(trt, TRT_CTRL, REF)
  draws_file <- file.path(OUT, sprintf("tr_placebo_draws_%s.csv", PLACEBO_TRT))
  draws <- vector("list", N_DRAWS)
  for (r in seq_len(N_DRAWS)) {
    t0 <- Sys.time()
    ep_r <- pb_draw_eps(PL_SEED + r, elig, crisis_len, MIN_GAP, PRE_M, POST_M)
    d_r  <- build_outcomes(ep_r, draw_panel(ep_r), quiet = TRUE)
    m_r  <- pb_est(TR_OUTCOMES, d_r, rhs)
    draws[[r]] <- rbindlist(lapply(TR_OUTCOMES, \(v) pb_get_ct(m_r[[v]], v, trt, REF)))[
      , `:=`(draw = r, eps = paste(format(ep_r$dd_start, "%Y-%m"), collapse = " "))]
    fwrite(rbindlist(draws), draws_file)
    cat(sprintf("draw %d/%d (%.0fs): %s\n", r, N_DRAWS,
                as.numeric(difftime(Sys.time(), t0, units = "secs")), draws[[r]]$eps[1]))
    rm(d_r, m_r); gc()
  }
  draws <- rbindlist(draws)

  cr <- main_paths[treatment == PLACEBO_TRT & type == "Crisis"]
  lab <- \(x) factor(TR_LABS[x], levels = TR_LABS)
  band <- draws[, .(lo = quantile(est, .05), hi = quantile(est, .95)), by = .(outcome, rel_month)]
  p_ri <- ggplot() +
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
    tr_theme
  ggsave(file.path(OUT, sprintf("tr_placebo_draws_%s.pdf", PLACEBO_TRT)), p_ri,
         width = 11, height = 5)

  pl_avg <- draws[between(rel_month, POST_AVG[1], POST_AVG[2]), .(avg = mean(est)),
                  by = .(outcome, draw)]
  cr_avg <- cr[between(rel_month, POST_AVG[1], POST_AVG[2]), .(crisis_avg = mean(est)), by = outcome]
  ri <- merge(cr_avg, pl_avg[, .(placebo_mean = mean(avg), placebo_sd = sd(avg),
                                 p05 = quantile(avg, .05), p95 = quantile(avg, .95),
                                 n_draws = .N), by = outcome], by = "outcome")
  ri[, p_two_sided := vapply(seq_len(.N), \(i) {
    a <- pl_avg[outcome == ri$outcome[i], avg]
    mean(abs(a - mean(a)) >= abs(crisis_avg[i] - mean(a)))
  }, 0)]
  CHK$ri <- ri
  cat(sprintf("\nRandomization inference, mean effect over m %d..%d:\n", POST_AVG[1], POST_AVG[2]))
  print(ri)
  fwrite(ri, file.path(OUT, sprintf("tr_placebo_ri_%s.csv", PLACEBO_TRT)))
}

# ---- Checks ------------------------------------------------------------------
zt <- placebo[, traded := any(abs(dq_tot_pf) > 1, na.rm = TRUE), by = .(Bp_ID, ep_id)][
  traded == FALSE & rel_month > REF]
CHK$zero_trade <- data.table(panel = "placebo",
                             client_eps = uniqueN(zt, by = c("Bp_ID", "ep_id")),
                             median_gap = median(zt$act_gap, na.rm = TRUE),
                             sh_abs_gt_1pct = mean(abs(zt$act_gap) > 0.01, na.rm = TRUE))
trt_pl <- TRTS[[PLACEBO_TRT]]
CHK$treated <- rbind(
  cbind(type = "crisis",  crisis[, .(clients = uniqueN(Bp_ID),
                                     n_trt = uniqueN(Bp_ID[get(trt_pl) == 1])), by = ep_id]),
  cbind(type = "placebo", placebo[, .(clients = uniqueN(Bp_ID),
                                      n_trt = uniqueN(Bp_ID[get(trt_pl) == 1])), by = ep_id]))
CHK$liquidation <- rbind(
  cbind(type = "crisis",  crisis[, .(sh_liq = mean(tot_pf < 100 & rel_month >= 0)), by = ep_id]),
  cbind(type = "placebo", placebo[, .(sh_liq = mean(tot_pf < 100 & rel_month >= 0)), by = ep_id]))

md <- function(x) {
  x <- as.data.table(x)
  for (j in names(x)) if (is.numeric(x[[j]])) set(x, j = j, value = signif(x[[j]], 4))
  c(paste("|", paste(names(x), collapse = " | "), "|"),
    paste("|", paste(rep("---", ncol(x)), collapse = " | "), "|"),
    apply(x, 1, \(r) paste("|", paste(format(r, trim = TRUE), collapse = " | "), "|")))
}
writeLines(c(
  "# Total-return split, calm-month placebo - checks", "",
  sprintf("Generated %s | seed %d | window [%d, %d] | tot_pf_pre >= %s",
          format(Sys.time(), "%Y-%m-%d %H:%M"), PL_SEED, win[1], win[2], PF_PRE_MIN), "",
  "## Placebo episodes (main draw)", "", md(ep_pl[, .(ep_id, dd_start, dd_end, pre_month, post_end)]), "",
  "## Crisis vs placebo: mean effect over the post window", "", md(main_avgs), "",
  "## Joint tests, crisis minus placebo", "", md(wald_tab), "",
  if (!is.null(CHK$ri)) c("## Randomization inference over draws", "", md(CHK$ri), "") else NULL,
  "## Zero-trade client-episodes in the placebo panel (act_gap should be ~0)", "",
  md(CHK$zero_trade), "",
  "## Treated client-episodes per episode", "", md(CHK$treated), "",
  "## Share of client-episode-months with a liquidated portfolio", "",
  md(CHK$liquidation), ""), file.path(OUT, "checks.md"))

cat("\nPlacebo outputs in", normalizePath(OUT), "\n")
