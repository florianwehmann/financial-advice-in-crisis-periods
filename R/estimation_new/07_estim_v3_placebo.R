# =============================================================================
# Placebo for 07_estim_v3.R: same event study, but on randomly drawn CALM months
# instead of crisis episodes. Is the advice effect specific to crises?
#
#   0. Build placebo episodes: dd_start drawn from months that are away from every
#      hand-dated stress window in stress_events.R (incl. the ones 05_episodes
#      does NOT turn into episodes), dd lengths matched to the crisis episodes,
#      windows truncated at the neighbour exactly as in 05_episodes.R.
#   1. Rebuild the stacked panel (06_prep.R logic) for those episodes, verified
#      against stk1.parquet on the real crisis episodes.
#   2. Main placebo draw: the 07 outputs (stand-alone plots, wealth and PF
#      decompositions), file names suffixed "_placebo".
#   3. Crisis vs placebo in ONE pooled regression, all terms interacted with a
#      crisis dummy -> the *_xc coefficients are the crisis-minus-placebo
#      difference, with advisor-clustered SEs (clients appear in both).
#   4. Randomization distribution: N_DRAWS placebo draws, crisis path vs the
#      spread of placebo paths, and where the crisis post-average falls in it.
#
# Run in a fresh R session with the working directory set to R/estimation_new.
# To test quickly: N_DRAWS <- 1L; OUT_TO_OVERLEAF <- FALSE; source(this file)
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(arrow)
  library(ggplot2)
  library(lubridate)
})

# shared y-axis with the crisis figures of 07_estim_v3.R
source("decomp_fig.R")
data.table::setDTthreads(0)
# ---- Config ------------------------------------------------------------------
data_path  <- "../../data/stk2.parquet"        # crisis panel used by 07
pos_path   <- "../../data/pos_pf.parquet"      # filtered monthly panel from 06_prep
ep_path    <- "../../data/episodes_short.parquet"
res_dir    <- "../../results/placebo"

if (!exists("OUT_TO_OVERLEAF")) OUT_TO_OVERLEAF <- TRUE
overleaf_dir <- "C:/Users/FWehmann/Dropbox/Apps/Overleaf/Uncertainty and Financial Advise (1)"
fig_dir <- if (OUT_TO_OVERLEAF) file.path(overleaf_dir, "figures") else res_dir
tab_dir <- if (OUT_TO_OVERLEAF) file.path(overleaf_dir, "tables")  else res_dir
for (d in unique(c(res_dir, fig_dir, tab_dir))) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# Same specification as 07_estim_v3.R -- keep in sync
TRT      <- "treat_inv_a_p"      # "treat_inv_a_p" | "treat_perfinv_a_p"
TRT_LAB  <- "Adv-init. personal inv. advice"
TRT_CTRL <- "treat_inv_c_p"
REF      <- -1L
win      <- c(-6L, 9L)
p_win    <- 0.99
drop_nonpos_wealth <- TRUE

# Placebo design
SMP_PANEL <- as.Date(c("2011-03-31", "2024-12-31"))  # as in 05_episodes.R
PRE_M     <- 6L      # as in 05_episodes.R
POST_M    <- 8L      # as in 05_episodes.R
BUF       <- 2L      # months of buffer around every stress window
STRICT    <- FALSE   # TRUE: the WHOLE placebo window (pre..post) must be calm;
                     # FALSE: only the pseudo-drawdown and the reference month
MIN_GAP   <- 6L      # min. months between two placebo dd_starts in one draw
PL_SEED   <- 20260915L
N_DRAWS <- 100L   # randomization draws (0 = skip section 4)
POST_AVG  <- c(0L, 8L)                   # rel_months averaged for the RI summary
DRAW_OUTCOMES <- c("w_rel", "dp_pct", "dq_pct", "cash_liq_pct","pf_idx_w")
VERIFY    <- F    # rebuild the crisis panel and compare with stk1.parquet

setFixest_nthreads(parallel::detectCores())
setFixest_notes(FALSE)

# ---- Helpers: dates ----------------------------------------------------------
eom   <- function(x) ceiling_date(as.Date(x), "month") - 1
madd  <- function(a, k) eom(as.Date(a) %m+% months(k))
mdiff <- function(a, b) as.integer((year(a) - year(b)) * 12L + (month(a) - month(b)))

# ---- Load --------------------------------------------------------------------
source("../estimation/stress_events.R")                      # EVENTS
ep_crisis <- setDT(read_parquet(ep_path))[usable == TRUE]
setorder(ep_crisis, dd_start)
crisis_len <- ep_crisis$n_months + 1L                        # dd length in months

contacts <- c("inv_a_p", "perfinv_a_p", "inv_c_p")
pos_cols <- c("Bp_ID", "MDate", "advisor_id", "anlagepaket", "adv_segment",
              "equity", "bond", "tot_pf", "cash_liq", "cash_locked", "hypo", "tot_wealth",
              "credit_check", "deposit_cash_check", "dp_tot_pf", "dq_tot_pf", "dfx_tot_pf",
              "dp_equity", "dq_equity", "dp_bond", "dq_bond", contacts)
pos <- setDT(read_parquet(pos_path, col_select = all_of(pos_cols), mmap = FALSE))
# shares and d_deposit are created in 06_prep AFTER pos_pf.parquet is written -> recreate
pos[, `:=`(pf_share_of_w  = tot_pf / tot_wealth,
           eq_share_of_pf = equity / tot_pf,
           eq_share_of_w  = equity / tot_wealth)]
setorder(pos, Bp_ID, MDate)
pos[, d_deposit := c(0, diff(deposit_cash_check)), by = Bp_ID]
setkey(pos, Bp_ID, MDate)


# Columns 07 reads from stk1 (+ tot_pf for the PF decomposition)
load_cols <- c("Bp_ID", "ep_id", "MDate", "advisor_id", "rel_month",
               "tot_wealth", "tot_wealth_pre", "tot_wealth_pre_mean", "tot_pf", "tot_pf_pre",
               "dp_tot_pf_c", "dq_tot_pf_c", "dfx_tot_pf_c","dp_tot_pf","dfx_tot_pf",
               "cash_liq", "cash_liq_pre", "cash_locked", "cash_locked_pre",
               "hypo", "hypo_pre", "credit_check", "credit_check_pre",
               "treat_perfinv_a_p", "treat_inv_a_p", "treat_inv_c_p",
               "anlagepaket_pre", "adv_segment_pre","d_deposit","d_deposit_pre","d_deposit_pre_sum",
               "pf_share_of_w", "eq_share_of_pf", "eq_share_of_w",
               "dq_eq_c", "dp_eq_c", "dq_bd_c", "dp_bd_c", "equity_pre", "bond_pre",
               "eq_share_of_w_pre_mean","eq_share_of_pf_pre_mean")

# ---- 1. Panel builder (the slice of 06_prep.R that 07 reads) -----------------
build_stk <- function(ep) {
  ep <- as.data.table(ep)[, .(ep_id, pre_month, pre_start, post_end, dd_start, dd_end)]
  cle <- CJ(Bp_ID = unique(pos$Bp_ID), ep_id = ep$ep_id)
  cle <- ep[cle, on = "ep_id"]

  # values in the pre-month
  pre_src <- c("anlagepaket", "adv_segment", "tot_pf", "cash_liq", "cash_locked", "hypo",
               "tot_wealth", "credit_check", "equity", "bond", "d_deposit")
  pre <- pos[, c("Bp_ID", "MDate", pre_src), with = FALSE]
  setnames(pre, pre_src, paste0(pre_src, "_pre"))
  cle <- merge(cle, pre, by.x = c("Bp_ID", "pre_month"), by.y = c("Bp_ID", "MDate"), all.x = TRUE)

  # pre-period means and sums, built the way 06_prep.R builds them
  pre_mean_src <- c("tot_wealth", "eq_share_of_w", "eq_share_of_pf")
  cle[, (paste0(pre_mean_src, "_pre_mean")) :=
        pos[cle, on = .(Bp_ID, MDate >= pre_start, MDate <= pre_month),
            lapply(.SD, mean, na.rm = TRUE), .SDcols = pre_mean_src,
            by = .EACHI][, ..pre_mean_src]]
  cle[, d_deposit_pre_sum :=
        pos[cle, on = .(Bp_ID, MDate >= pre_start, MDate <= pre_month),
            sum(d_deposit, na.rm = TRUE), by = .EACHI]$V1]

  # treatment = any contact during the (pseudo-)drawdown
  n_tr <- pos[cle, on = .(Bp_ID, MDate >= dd_start, MDate <= dd_end),
              lapply(.SD, sum, na.rm = TRUE), .SDcols = contacts, by = .EACHI][, ..contacts]
  cle[, (paste0("treat_", contacts)) := lapply(n_tr, \(x) as.numeric(x > 0))]

  # stacked client x episode x month
  stk <- pos[cle[, .(Bp_ID, ep_id, pre_start, post_end)],
             on = .(Bp_ID, MDate >= pre_start, MDate <= post_end),
             .(Bp_ID, ep_id, MDate = x.MDate, advisor_id,
               tot_wealth, tot_pf, cash_liq, cash_locked, hypo, credit_check, d_deposit,
               dp_tot_pf, dq_tot_pf, dfx_tot_pf, dp_equity, dq_equity, dp_bond, dq_bond,
               pf_share_of_w, eq_share_of_pf, eq_share_of_w),
             nomatch = NULL, allow.cartesian = TRUE]
  stk <- ep[, .(ep_id, dd_start)][stk, on = "ep_id"]
  stk[, rel_month := mdiff(MDate, dd_start)]
  setorder(stk, Bp_ID, ep_id, MDate)
  stk[, `:=`(dp_tot_pf_c  = cumsum(dp_tot_pf),
             dfx_tot_pf_c = cumsum(dfx_tot_pf),
             dq_tot_pf_c  = cumsum(dq_tot_pf),
             dp_eq_c = cumsum(dp_equity), dq_eq_c = cumsum(dq_equity),
             dp_bd_c = cumsum(dp_bond),   dq_bd_c = cumsum(dq_bond)), by = .(Bp_ID, ep_id)]

  # only the client x episode constants load_cols actually asks for: 06_prep.R
  # carries many more, but none of them are built above, so listing them here
  # would just fail the join
  keep_cle <- c("Bp_ID", "ep_id", paste0("treat_", contacts),
                "tot_pf_pre", "tot_wealth_pre", "tot_wealth_pre_mean",
                "cash_liq_pre", "cash_locked_pre", "hypo_pre", "credit_check_pre",
                "equity_pre", "bond_pre", "anlagepaket_pre", "adv_segment_pre",
                "d_deposit_pre", "d_deposit_pre_sum",
                "eq_share_of_w_pre_mean", "eq_share_of_pf_pre_mean")
  stk <- cle[, ..keep_cle][stk, on = .(Bp_ID, ep_id)]
  stk[, ..load_cols]
}

# ---- Sample + outcomes (identical to 07) -------------------------------------
prep_est <- function(d) {
  d <- d[between(rel_month, win[1], win[2])]
  d <- d[tot_wealth_pre > 0 & tot_wealth_pre_mean > 0]
  if (drop_nonpos_wealth) d <- d[tot_wealth > 0]
  d[, rel_month := as.integer(rel_month)]
  d[, `:=`(
    log_w_pre_mean   = log(tot_wealth_pre_mean),
    w_rel            = tot_wealth / tot_wealth_pre,
    dw_pct           = tot_wealth / tot_wealth_pre - 1,
    dp_pct           = dp_tot_pf_c  / tot_wealth_pre,
    dq_pct           = dq_tot_pf_c  / tot_wealth_pre,
    dfx_pct          = dfx_tot_pf_c / tot_wealth_pre,
    cash_liq_pct     = (cash_liq    - cash_liq_pre)    / tot_wealth_pre,
    cash_locked_pct  = (cash_locked - cash_locked_pre) / tot_wealth_pre,
    hypo_pct         = (hypo - hypo_pre) / tot_wealth_pre,
    other_credit_pct = ((credit_check - hypo) - (credit_check_pre - hypo_pre)) / tot_wealth_pre,
    dp_eq_pct_of_pf  = dp_eq_c / tot_pf_pre,
    dq_eq_pct_of_pf  = dq_eq_c / tot_pf_pre,
    dp_bd_pct_of_pf  = dp_bd_c / tot_pf_pre,
    dq_bd_pct_of_pf  = dq_bd_c / tot_pf_pre
  )]
  # tot_pf_pre <= 0 (about 38% of rows) -> NaN / +-Inf in everything scaled by the
  # pre-period portfolio. Left in, they also break winsor() below. Same fix as
  # 07_estim_v3.R.
  pf_scaled <- grep("_pct_of_pf$", names(d), value = TRUE)
  d[tot_pf_pre <= 0, (pf_scaled) := NA_real_]
  d[, ci := .GRP, by = .(Bp_ID, ep_id)]
  d[, te := .GRP, by = .(MDate, ep_id)]
  d[]
}

# Inf-safe: quantile() keeps Inf, so a single Inf in the upper tail makes the 99%
# cut-off Inf and pmin() a no-op -- no winsorizing on that side -- while -Inf gets
# clipped to a finite value and stays in the regression as if it were data.
winsor <- function(x, p = 0.99) {
  q <- quantile(x[is.finite(x)], c(1 - p, p), na.rm = TRUE)
  x[is.infinite(x)] <- NA_real_
  pmin(pmax(x, q[1]), q[2])
}
flow_vars <- c("w_rel", "dp_pct", "dq_pct", "dfx_pct", "cash_liq_pct",
               "cash_locked_pct", "hypo_pct", "other_credit_pct",
               "dp_eq_pct_of_pf", "dq_eq_pct_of_pf", "dp_bd_pct_of_pf", "dq_bd_pct_of_pf")
make_wdt <- function(d) {
  w <- copy(d)
  w[, (flow_vars) := lapply(.SD, winsor, p = p_win), .SDcols = flow_vars]
  w[]
}

# ---- Specification (identical to 07) -----------------------------------------
ev_terms <- function(v) sprintf("i(rel_month, %s, ref = %d)", v, REF)
# rhs_base <- paste(ev_terms(c(TRT, TRT_CTRL, "log_w_pre_mean",
#                              "anlagepaket_pre", "adv_segment_pre")), collapse = " + ")
NUM_CTRL <- c("log_w_pre_mean", "d_deposit_pre_sum", "eq_share_of_w_pre_mean")
CAT_CTRL <- c("anlagepaket_pre", "adv_segment_pre")
rhs_base <- paste(ev_terms(c(TRT, TRT_CTRL, NUM_CTRL, CAT_CTRL)), collapse = " + ")

est <- function(lhs, data, rhs = rhs_base, fe = "ci + te") {
  lhs_str <- if (length(lhs) == 1) lhs else sprintf("c(%s)", paste(lhs, collapse = ", "))
  fml <- as.formula(paste(lhs_str, "~", rhs, "|", fe))
  m <- feols(fml, data = data, vcov = ~advisor_id,
             lean = TRUE, fixef.tol = 1e-5, mem.clean = TRUE)
  if (length(lhs) == 1) return(setNames(list(m), lhs))
  setNames(as.list(m), lhs)
}

get_ct <- function(mod, label, term = TRT, ref = REF) {
  ct <- as.data.table(coeftable(mod), keep.rownames = "coef")
  ct <- ct[grepl(paste0(":", term, "$"), coef)]
  ct[, rel_month := as.numeric(sub("^rel_month::(-?[0-9.]+):.*$", "\\1", coef))]
  ct <- ct[, .(rel_month, est = Estimate, se = `Std. Error`)]
  ct <- rbind(ct, data.table(rel_month = ref, est = 0, se = 0))
  ct[, outcome := label][order(rel_month)]
}

# ---- Plot helpers (as in 07) -------------------------------------------------
pal  <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E")
pchs <- c(19, 17, 15, 18, 8)
plot_es <- function(mods, main = "Event-study estimates") {
  k <- length(mods)
  iplot(mods, main = main,
        xlab = "Months relative to pre-period month", ylab = "Estimate (95% CI)",
        col = pal[1:k], pt.pch = pchs[1:k], pt.join = TRUE, pt.join.par = list(lwd = 2),
        sep = 0.15, ci.lwd = 1.5, ci.width = 0.1)
  legend("topleft", legend = names(mods), col = pal[1:k], pch = pchs[1:k],
         lty = 1, lwd = 1.5, bty = "n", cex = 0.9)
}
save_es <- function(mods, main, file, width = 8, height = 5) {
  plot_es(mods, main)
  pdf(file.path(fig_dir, file), width = width, height = height)
  on.exit(dev.off())
  plot_es(mods, main)
}

# Additive decomposition: common sample, trimmed on the total, explicit residual
run_decomp <- function(d, total, comp_lab, total_lab, fills, ylab, title, file, group) {
  comps <- setdiff(names(comp_lab), "resid")
  dd <- na.omit(d, cols = c(total, comps))
  q <- quantile(dd[[total]], c(1 - p_win, p_win))
  dd <- dd[between(get(total), q[1], q[2])]
  set(dd, j = "resid", value = dd[[total]] - Reduce(`+`, dd[, ..comps]))
  m <- est(c(total, names(comp_lab)), dd)

  comp <- rbindlist(lapply(names(comp_lab), \(v) get_ct(m[[v]], comp_lab[[v]])))
  comp[, outcome := factor(outcome, levels = comp_lab)]
  tot <- get_ct(m[[total]], total_lab)
  chk <- merge(comp[, .(sum_comp = sum(est)), by = rel_month], tot, by = "rel_month")
  stopifnot(chk[, max(abs(sum_comp - est))] < 1e-4)

  # drawn by decomp_fig.R: this figure and the crisis one share a y-axis, and
  # decomp_render() redraws the crisis figure too if 07_estim_v3.R has run
  decomp_save(group, "placebo", comp, tot, list(
    fig_dir  = fig_dir, file = file, title = title,
    subtitle = sprintf("PLACEBO (calm months). Treatment: %s, N = %s; 95%% CI clustered by advisor",
                       TRT_LAB, format(nobs(m[[total]]), big.mark = "'")),
    ylab     = ylab, fills = fills,
    xbreaks  = seq(win[1], win[2], 2), ref = REF))
  p <- decomp_render(group, current = "placebo")
  print(p)
  invisible(list(models = m, plot = p))
}

# =============================================================================
# 0. PLACEBO EPISODES
# =============================================================================
month_seq <- function(s, e) madd(s, 0:mdiff(e, s))

# every month touched by a hand-dated stress window (all 20, not only the 7
# crisis episodes), widened by BUF months on both sides
stress_m <- unique(do.call(c, lapply(seq_len(nrow(EVENTS)), \(i)
  month_seq(madd(EVENTS$start[i], -BUF), madd(EVENTS$end[i], BUF)))))

eligible_starts <- function(L) {
  k_max <- mdiff(SMP_PANEL[2], SMP_PANEL[1]) - POST_M - L + 1L
  cand  <- madd(SMP_PANEL[1], PRE_M:k_max)
  offs  <- if (STRICT) -PRE_M:(L - 1L + POST_M) else -1L:(L - 1L)
  ok <- vapply(seq_along(cand), \(i) !any(madd(cand[i], offs) %in% stress_m), TRUE)
  cand[ok]
}
elig <- setNames(lapply(sort(unique(crisis_len)), eligible_starts), sort(unique(crisis_len)))
cat("\nEligible placebo dd_start months by dd length (BUF =", BUF, ", STRICT =", STRICT, "):\n")
for (L in names(elig)) cat(sprintf("  L = %s: %d months: %s\n", L, length(elig[[L]]),
                                  paste(format(elig[[L]], "%Y-%m"), collapse = " ")))

# same window rules as build_episodes() in 05_episodes.R
make_eps <- function(dd_start, len) {
  e <- data.table(dd_start = as.Date(dd_start), n_len = as.integer(len))
  setorder(e, dd_start)
  e[, `:=`(dd_end = madd(dd_start, n_len - 1L))]
  e[, `:=`(ep_id     = sprintf("placebo_%s", format(dd_start, "%Y%m")),
           pre_month = madd(dd_start, -1L),
           pre_start = madd(dd_start, -PRE_M),
           post_end  = madd(dd_end, POST_M),
           n_months  = n_len - 1L)]
  e[, next_start := shift(dd_start, type = "lead")]
  e[!is.na(next_start) & post_end >= next_start, post_end := madd(next_start, -1L)]
  e[, prev_end := shift(dd_end)]
  e[!is.na(prev_end) & pre_start <= prev_end, pre_start := madd(prev_end, 1L)]
  e[, c("next_start", "prev_end", "n_len") := NULL]
  e[]
}

# K = number of crisis episodes, dd lengths permuted from the crisis lengths,
# dd_starts at least MIN_GAP months apart. Retries until K episodes fit.
draw_eps <- function(seed, tries = 200L) {
  set.seed(seed)
  best <- NULL
  for (t in seq_len(tries)) {
    lens <- sample(crisis_len)
    acc_s <- as.Date(character()); acc_l <- integer()
    for (L in lens) {
      cand <- elig[[as.character(L)]]
      if (length(acc_s))
        cand <- cand[vapply(seq_along(cand), \(i) all(abs(mdiff(cand[i], acc_s)) >= MIN_GAP), TRUE)]
      if (!length(cand)) next
      acc_s <- c(acc_s, cand[sample.int(length(cand), 1L)]); acc_l <- c(acc_l, L)
    }
    if (is.null(best) || length(acc_s) > nrow(best)) best <- data.table(s = acc_s, l = acc_l)
    if (length(acc_s) == length(crisis_len)) break
  }
  if (nrow(best) < length(crisis_len))
    warning(sprintf("seed %d: only %d of %d placebo episodes fit", seed, nrow(best), length(crisis_len)))
  make_eps(best$s, best$l)
}

treat_counts <- function(d) {
  ce <- unique(d[, c("Bp_ID", "ep_id", "treat_inv_a_p", "treat_perfinv_a_p", "treat_inv_c_p"), with = FALSE])
  ce[, .(clients = .N, n_inv_a = sum(treat_inv_a_p), n_perfinv_a = sum(treat_perfinv_a_p),
         n_inv_c = sum(treat_inv_c_p), sh_trt = mean(get(TRT))), by = ep_id]
}

calc_pf_idx <- function(dt){
  setorder(dt, ep_id, Bp_ID, MDate)
  dt[, pf_prev := shift(tot_pf), by = .(ep_id, Bp_ID)]
  
  # monthly return; 0 after liquidation (cash) and in first window month
  dt[, valid_ret := !is.na(pf_prev) & pf_prev > 0]
  dt[, pf_ret := fifelse(valid_ret, (dp_tot_pf + dfx_tot_pf) / pf_prev, 0)]
  
  # winsorize per episode-month, quantiles on real returns only
  dt[, pf_ret_w := {
    q <- quantile(pf_ret[valid_ret], c(.01, .99), na.rm = TRUE)
    fifelse(valid_ret, pmin(pmax(pf_ret, q[1]), q[2]), 0)
  }, by = .(ep_id, MDate)]
  
  # cumulative index, rebased to m = -1
  dt[, `:=`(
    pf_idx   = cumprod(1 + pf_ret),
    pf_idx_w = cumprod(1 + pf_ret_w)
  ), by = .(ep_id, Bp_ID)]
  dt[, `:=`(
    pf_idx   = pf_idx   / pf_idx[rel_month == -1]   - 1,
    pf_idx_w = pf_idx_w / pf_idx_w[rel_month == -1] - 1
  ), by = .(ep_id, Bp_ID)]
  return(dt)
}

# =============================================================================
# 1. VERIFY THE BUILDER ON THE CRISIS EPISODES
# =============================================================================
crisis_raw <- setDT(read_parquet(data_path, col_select = all_of(load_cols), mmap = FALSE))
crisis_raw <- crisis_raw[!is.na(MDate)]

# pos filter
crisis_raw <- crisis_raw[tot_pf_pre>5000]

if (VERIFY) {
  rebuilt <- build_stk(ep_crisis)
  summ <- function(d) d[, .(N = .N,
                            trt_inv_a = sum(treat_inv_a_p), trt_perfinv_a = sum(treat_perfinv_a_p),
                            trt_inv_c = sum(treat_inv_c_p),
                            w_pre = sum(tot_wealth_pre, na.rm = TRUE),
                            w_pre_mean = sum(tot_wealth_pre_mean, na.rm = TRUE),
                            dq_c = sum(dq_tot_pf_c, na.rm = TRUE),
                            cash = sum(cash_liq, na.rm = TRUE)), keyby = ep_id]
  s_old <- summ(crisis_raw); s_new <- summ(rebuilt)
  cmp <- all.equal(s_old, s_new, tolerance = 1e-8, check.attributes = FALSE)
  if (!isTRUE(cmp)) {
    print(s_old); print(s_new)
    stop("build_stk() does not reproduce ", basename(data_path), ": ", paste(cmp, collapse = "; "),
         "\n  (pos_pf.parquet may be stale -- its write in 06_prep.R is commented out;",
         "\n   re-run 06_prep.R with that line active, or set VERIFY <- FALSE)", call. = FALSE)
  }
  cat("\nbuild_stk() reproduces", basename(data_path), "on the crisis episodes.\n")
  rm(rebuilt); gc()
}

crisis_dt <- prep_est(crisis_raw); rm(crisis_raw); gc()

crisis_dt <- calc_pf_idx(crisis_dt)


# =============================================================================
# 2. MAIN PLACEBO DRAW -- the 07 outputs
# =============================================================================
# ep_pl <- draw_eps(PL_SEED)
# cat("\nMain placebo episodes (seed ", PL_SEED, "):\n", sep = "")
# print(ep_pl)
# fwrite(ep_pl, file.path(res_dir, "placebo_episodes_main.csv"))
# 
# pl_dt <- prep_est(build_stk(ep_pl))
# 
# pl_dt <- pl_dt[tot_pf_pre>2000]
# 
# cmp_counts <- rbind(cbind(type = "crisis",  treat_counts(crisis_dt)),
#                     cbind(type = "placebo", treat_counts(pl_dt)))
# cat("\nTreated per episode (crisis vs placebo):\n"); print(cmp_counts)
# fwrite(cmp_counts, file.path(res_dir, sprintf("treat_counts_%s.csv", TRT)))
# 
# pl_dt <- calc_pf_idx(pl_dt)
# 
# pl_w <- make_wdt(pl_dt)
# 
# groups <- list(
#   levels = list(title  = "PLACEBO: wealth and its components (share of pre-period wealth)",
#                 vars   = c("w_rel", "dp_pct", "dq_pct", "cash_liq_pct"),
#                 labels = c("Wealth / pre-period wealth", "PF price change",
#                            "PF quantity change", "Liquid cash change")),
#   shares = list(title  = "PLACEBO: portfolio shares",
#                 vars   = c("pf_share_of_w", "eq_share_of_pf", "eq_share_of_w"),
#                 labels = c("Portfolio / wealth", "Equity / portfolio", "Equity / wealth")),
#   equity = list(title  = "PLACEBO: equity changes (share of pre-period portfolio)",
#                 vars   = c("dq_eq_pct_of_pf", "dp_eq_pct_of_pf"),
#                 labels = c("Equity quantity change", "Equity price change")),
#   # pf1 = list(
#   #   title  = "Portfolio Price changes (share of pre-period portfolio)",
#   #   vars   = c("dp_pct_to_pf"),
#   #   labels = c("Portfolio price change")
#   # ),
#   pf2 = list(
#     title  = "Portfolio Price changes (share of pre-period wealth)",
#     vars   = c("dp_pct"),
#     labels = c("Portfolio price change")
#   ),
#   pf3 = list(
#     title  = "Portfolio Return",
#     vars   = c("pf_idx_w"),
#     labels = c("Portfolio Return (wins)")
# ))
# y_main <- unique(unlist(lapply(groups, `[[`, "vars")))
# m_pl <- est(y_main, pl_w)
# etable(m_pl, keep = TRT)
# etable(m_pl, keep = TRT, tex = TRUE, replace = TRUE,
#        file = file.path(tab_dir, sprintf("es_main_%s_placebo.tex", TRT)))
# for (g in names(groups)) {
#   grp <- groups[[g]]
#   save_es(setNames(m_pl[grp$vars], grp$labels), main = grp$title,
#           file = sprintf("es_%s_%s_placebo.pdf", g, TRT))
# }
# 
# dec_w <- run_decomp(
#   pl_dt, "dw_pct",
#   c(dp_pct = "PF price", dq_pct = "PF quantity", dfx_pct = "PF FX",
#     cash_liq_pct = "Liquid cash", cash_locked_pct = "Locked cash", resid = "Residual"),
#   "Total wealth", c("#4E79A7", "#59A14F", "#76B7B2", "#E15759", "#F28E2B", "grey70"),
#   "Change in wealth (share of pre-period wealth)",
#   "PLACEBO: effect on total wealth, decomposed",
#   sprintf("decomp_wealth_%s_placebo.pdf", TRT), group = paste0("wealth_", TRT))
# 
# pf_sub <- pl_dt[tot_pf_pre > 0]
# pf_sub[, `:=`(dpf_pct    = tot_pf / tot_pf_pre - 1,
#               dp_pf_pct  = dp_tot_pf_c  / tot_pf_pre,
#               dq_pf_pct  = dq_tot_pf_c  / tot_pf_pre,
#               dfx_pf_pct = dfx_tot_pf_c / tot_pf_pre)]
# dec_pf <- run_decomp(
#   pf_sub, "dpf_pct",
#   c(dp_pf_pct = "PF price", dq_pf_pct = "PF quantity", dfx_pf_pct = "PF FX", resid = "Residual"),
#   "Total portfolio", c("#4E79A7", "#59A14F", "#76B7B2", "grey70"),
#   "Change in portfolio (share of pre-period portfolio)",
#   "PLACEBO: effect on total portfolio value, decomposed",
#   sprintf("decomp_pf_%s_placebo.pdf", TRT), group = paste0("pf_", TRT))
# rm(pf_sub); gc()

# =============================================================================
# 3. CRISIS vs PLACEBO: pooled, fully interacted with a crisis dummy
# =============================================================================
# Memory: every control here enters as rel_month x control dummies on the
# STACKED crisis+placebo panel, so the added controls blow up the design matrix
# (16 GB+ with everything interacted). Two changes keep it feasible and leave
# the coefficients of interest untouched:
#   - the categorical controls are absorbed as fixed effects instead of i()
#     terms, interacted with crisis (same fit, a fraction of the memory);
#   - only the TREATMENT terms are interacted with crisis; the numeric controls
#     are held common across crisis and placebo.
# Winsorize within type, so each half matches its stand-alone regression.
# 
DRAW_OUTCOMES <- c("w_rel","dp_pct","dq_pct","cash_liq_pct","pf_idx_w")
# 
# pool_cols <- unique(c("Bp_ID", "ep_id", "MDate", "advisor_id", "rel_month",
#                       DRAW_OUTCOMES, TRT, TRT_CTRL, NUM_CTRL, CAT_CTRL))
# pool_half <- function(d, crisis) {
#   x <- d[, ..pool_cols]
#   x[, (DRAW_OUTCOMES) := lapply(.SD, winsor, p = p_win), .SDcols = DRAW_OUTCOMES]
#   x[, crisis := crisis][]
# }
# pool <- rbind(pool_half(crisis_dt, 1L), pool_half(pl_dt, 0L))
# pool[, ci := .GRP, by = .(Bp_ID, ep_id)]
# pool[, te := .GRP, by = .(MDate, ep_id)]      # time x episode FE absorb the crisis main effect
# 
# num_x <- c(TRT, TRT_CTRL)
# for (v in num_x) set(pool, j = paste0(v, "_xc"), value = pool[[v]] * pool$crisis)
# rhs_pool <- paste(ev_terms(c(num_x, paste0(num_x, "_xc"), NUM_CTRL)), collapse = " + ")
# fe_pool  <- paste("ci + te",
#                   paste(sprintf("%s^rel_month^crisis", CAT_CTRL), collapse = " + "), sep = " + ")
# 
# m_pool <- est(DRAW_OUTCOMES, pool, rhs = rhs_pool, fe = fe_pool)
# 
# # placebo path = TRT, difference = TRT_xc, crisis path = TRT + TRT_xc (SE from vcov)
# pool_paths <- function(mod, label) {
#   b <- coef(mod); V <- vcov(mod)
#   nm_p <- grep(paste0("^rel_month::-?[0-9]+:", TRT, "$"), names(b), value = TRUE)
#   nm_x <- paste0(nm_p, "_xc")
#   keep <- nm_x %in% names(b); nm_p <- nm_p[keep]; nm_x <- nm_x[keep]
#   rm_  <- as.integer(sub("^rel_month::(-?[0-9]+):.*$", "\\1", nm_p))
#   se_c <- sqrt(diag(V)[nm_p] + diag(V)[nm_x] + 2 * V[cbind(nm_p, nm_x)])
#   out <- rbind(
#     data.table(rel_month = rm_, series = "Placebo (calm)",       est = b[nm_p],          se = sqrt(diag(V)[nm_p])),
#     data.table(rel_month = rm_, series = "Crisis",               est = b[nm_p] + b[nm_x], se = se_c),
#     data.table(rel_month = rm_, series = "Crisis minus placebo", est = b[nm_x],          se = sqrt(diag(V)[nm_x])))
#   out <- rbind(out, data.table(rel_month = REF, series = unique(out$series), est = 0, se = 0))
#   out[, outcome := label]
# }
out_lab <- c(w_rel = "Wealth / pre-period wealth", dp_pct = "PF price change",
             dq_pct = "PF quantity change", cash_liq_pct = "Liquid cash change",pf_idx_w = "PF Return Index")
# paths <- rbindlist(lapply(DRAW_OUTCOMES, \(v) pool_paths(m_pool[[v]], out_lab[[v]])))
# paths[, outcome := factor(outcome, levels = out_lab[DRAW_OUTCOMES])]
# fwrite(paths, file.path(res_dir, sprintf("pooled_paths_%s.csv", TRT)))
# 
# p_pool <- ggplot(paths, aes(rel_month, est, colour = series, fill = series)) +
#   geom_ribbon(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), alpha = 0.15, colour = NA) +
#   geom_line(linewidth = 0.6) + geom_point(size = 1.2) +
#   geom_hline(yintercept = 0, linewidth = 0.3) +
#   geom_vline(xintercept = REF + 0.5, linetype = "dashed", linewidth = 0.4) +
#   facet_wrap(~outcome, scales = "free_y") +
#   scale_colour_manual(values = c("Crisis" = "#C0392B", "Placebo (calm)" = "#41729F",
#                                  "Crisis minus placebo" = "grey25"), aesthetics = c("colour", "fill")) +
#   scale_x_continuous(breaks = seq(win[1], win[2], 2)) +
#   labs(x = "Months relative to (pseudo-)drawdown start", y = "Estimate (95% CI)",
#        colour = NULL, fill = NULL, title = "Advice effect in crises vs calm months",
#        subtitle = sprintf("Treatment: %s. Pooled, all terms interacted with a crisis dummy; SE clustered by advisor",
#                           TRT_LAB)) +
#   theme_minimal(base_size = 11) +
#   theme(legend.position = "bottom", panel.grid.minor = element_blank())
# print(p_pool)
# ggsave(file.path(fig_dir, sprintf("es_crisis_vs_placebo_%s.pdf", TRT)), p_pool, width = 9, height = 6)
# 
# # joint Wald tests on the difference, pre and post
# wald_tab <- rbindlist(lapply(DRAW_OUTCOMES, \(v) {
#   w_post <- wald(m_pool[[v]], keep = paste0("^rel_month::[0-9]+:", TRT, "_xc$"), print = FALSE)
#   w_pre  <- wald(m_pool[[v]], keep = paste0("^rel_month::-[0-9]+:", TRT, "_xc$"), print = FALSE)
#   data.table(outcome = out_lab[[v]],
#              F_post = w_post$stat, p_post = w_post$p, F_pre = w_pre$stat, p_pre = w_pre$p)
# }))
# cat("\nJoint tests: crisis minus placebo = 0\n"); print(wald_tab)
# fwrite(wald_tab, file.path(res_dir, sprintf("wald_crisis_vs_placebo_%s.csv", TRT)))
# # "%" = match the raw coefficient names; picks up both TRT and TRT_xc
# etable(m_pool, keep = paste0("%", TRT), tex = TRUE, replace = TRUE,
#        file = file.path(tab_dir, sprintf("es_crisis_vs_placebo_%s.tex", TRT)))
# 
# # free the pooled panel first: pool + a second feols do not fit in 16 GB
# rm(pool, m_pool, pl_w, pl_dt, m_pl, dec_w, dec_pf); gc()

# crisis stand-alone paths for section 4 (same as 07's stand-alone regressions)
crisis_w <- make_wdt(crisis_dt)
rm(crisis_dt); gc()
m_crisis <- est(DRAW_OUTCOMES, crisis_w)
crisis_ct <- rbindlist(lapply(DRAW_OUTCOMES, \(v) get_ct(m_crisis[[v]], v)))
rm(crisis_w, m_crisis); gc()

# =============================================================================
# 4. RANDOMIZATION DISTRIBUTION OVER PLACEBO DRAWS
# =============================================================================
if (N_DRAWS > 0) {
  draws_file <- file.path(res_dir, sprintf("placebo_draws_%s.csv", TRT))
  draws <- vector("list", N_DRAWS)
  for (r in seq_len(N_DRAWS)) {
    t0 <- Sys.time()
    ep_r <- draw_eps(PL_SEED + r)
    d_r  <- make_wdt(prep_est(build_stk(ep_r)))
    d_r <- calc_pf_idx(d_r)
    d_r <- d_r[tot_pf_pre > 5000]
    m_r  <- est(DRAW_OUTCOMES, d_r)
    draws[[r]] <- rbindlist(lapply(DRAW_OUTCOMES, \(v) get_ct(m_r[[v]], v)))[
      , `:=`(draw = r, n_eps = nrow(ep_r), n_treated = uniqueN(d_r[get(TRT) == 1], by = c("Bp_ID", "ep_id")),
             eps = paste(format(ep_r$dd_start, "%Y-%m"), collapse = " "))]
    fwrite(rbindlist(draws), draws_file)   # keep partial progress
    cat(sprintf("draw %d/%d done (%.0fs): %s\n", r, N_DRAWS,
                as.numeric(difftime(Sys.time(), t0, units = "secs")), draws[[r]]$eps[1]))
    rm(d_r, m_r); gc()
  }
  draws <- rbindlist(draws)

  # post-period average of the TRT path, crisis vs placebo distribution
  avg_post <- function(d) d[between(rel_month, POST_AVG[1], POST_AVG[2]), .(avg = mean(est)), by = outcome]
  pl_avg <- draws[between(rel_month, POST_AVG[1], POST_AVG[2]), .(avg = mean(est)), by = .(outcome, draw)]
  cr_avg <- avg_post(crisis_ct)
  ri <- merge(cr_avg[, .(outcome, crisis_avg = avg)],
              pl_avg[, .(placebo_mean = mean(avg), placebo_sd = sd(avg),
                         placebo_p05 = quantile(avg, .05), placebo_p95 = quantile(avg, .95),
                         n_draws = .N), by = outcome], by = "outcome")
  ri[, `:=`(rank_share = vapply(seq_len(.N), \(i) mean(pl_avg[outcome == ri$outcome[i], avg] <= crisis_avg[i]), 0),
            p_two_sided = vapply(seq_len(.N), \(i) {
              a <- pl_avg[outcome == ri$outcome[i], avg]
              mean(abs(a - mean(a)) >= abs(crisis_avg[i] - mean(a)))
            }, 0))]
  cat(sprintf("\nRandomization inference: mean TRT effect over rel_month %d..%d\n", POST_AVG[1], POST_AVG[2]))
  print(ri)
  fwrite(ri, file.path(res_dir, sprintf("ri_summary_%s.csv", TRT)))

  lab_fac <- \(x) factor(out_lab[x], levels = out_lab[DRAW_OUTCOMES])
  band <- draws[, .(lo = quantile(est, .05), hi = quantile(est, .95)), by = .(outcome, rel_month)]
  p_ri <- ggplot() +
    geom_ribbon(data = band[, outcome := lab_fac(outcome)],
                aes(rel_month, ymin = lo, ymax = hi), fill = "#41729F", alpha = 0.18) +
    geom_line(data = copy(draws)[, outcome := lab_fac(outcome)],
              aes(rel_month, est, group = draw), colour = "#41729F", alpha = 0.35, linewidth = 0.3) +
    geom_ribbon(data = copy(crisis_ct)[, outcome := lab_fac(outcome)],
                aes(rel_month, ymin = est - 1.96 * se, ymax = est + 1.96 * se), fill = "#C0392B", alpha = 0.12) +
    geom_line(data = copy(crisis_ct)[, outcome := lab_fac(outcome)],
              aes(rel_month, est), colour = "#C0392B", linewidth = 0.9) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_vline(xintercept = REF + 0.5, linetype = "dashed", linewidth = 0.4) +
    facet_wrap(~outcome, scales = "free_y") +
    scale_x_continuous(breaks = seq(win[1], win[2], 2)) +
    labs(x = "Months relative to (pseudo-)drawdown start", y = "Estimate",
         title = "Crisis estimate (red, 95% CI) vs placebo draws (blue, 5-95% band)",
         subtitle = sprintf("Treatment: %s. %d draws of %d calm-month episodes each",
                            TRT_LAB, uniqueN(draws$draw), length(crisis_len))) +
    theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank())
  print(p_ri)
  ggsave(file.path(fig_dir, sprintf("es_placebo_draws_%s.pdf", TRT)), p_ri, width = 9, height = 6)
}

cat("\nPlacebo outputs: figures/tables in", fig_dir, "| CSVs in", normalizePath(res_dir), "\n")
 