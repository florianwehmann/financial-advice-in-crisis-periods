# =============================================================================
# Estimation: event-study specs (stacked episodes, client-episode + time-episode FE)
# Run in a fresh R session (Ctrl+Shift+F10) instead of rm(list = ls()).
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(arrow)
  library(ggplot2)
})

rm(list=ls());gc()
setwd("C:/GitHub/financial-advice-in-crisis-periods/R/estimation_new")
# shared y-axis for the crisis and placebo decomposition figures
source("decomp_fig.R")

# ---- Config ------------------------------------------------------------------
data_path    <- "../../data/stk2.parquet"
overleaf_dir <- "C:/Users/FWehmann/Dropbox/Apps/Overleaf/Uncertainty and Financial Advise (1)"
fig_dir      <- file.path(overleaf_dir, "figures")
tab_dir      <- file.path(overleaf_dir, "tables")
for (d in c(fig_dir, tab_dir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# eps <- c("ep1_201305", "ep2_201501", "ep3_201508", "ep6_201801",
#          "ep7_201811", "ep9_202002", "ep10_202201", "ep12_202310")

# Treatment of interest + treatments held fixed as controls
TRT      <- "treat_inv_a_p"      # "treat_inv_a_p" | "treat_perfinv_a_p"
TRT_LAB  <- "Adv-init. personal inv. advice"   # adjust together with TRT
TRT_CTRL <- "treat_inv_c_p"
REF      <- -1L                  # reference period

win   <- c(-6L, 9L)              # event window
p_win <- 0.99                    # winsor / trim quantile

# Dropping tot_wealth <= 0 conditions on the outcome -> selection.
# TRUE only replicates the old results; FALSE is cleaner.
drop_nonpos_wealth <- TRUE

setFixest_nthreads(parallel::detectCores())
setFixest_notes(FALSE)

# ---- Load (only needed columns) ---------------------------------------------
load_cols <- c("Bp_ID", "ep_id", "MDate", "advisor_id", "rel_month",
               "tot_wealth", "tot_wealth_pre", "tot_wealth_pre_mean", "tot_pf", "tot_pf_pre",
               "dp_tot_pf_c", "dq_tot_pf_c", "dfx_tot_pf_c","dp_tot_pf","dfx_tot_pf","dq_tot_pf",
               "cash_liq", "cash_liq_pre", "cash_locked", "cash_locked_pre",
               "hypo", "hypo_pre", "credit_check", "credit_check_pre",
               "treat_perfinv_a_p", "treat_inv_a_p", "treat_inv_c_p",
               "anlagepaket_pre", "adv_segment_pre","d_deposit","d_deposit_pre","d_deposit_pre_sum",
               "pf_share_of_w", "eq_share_of_pf", "eq_share_of_w",
               "dq_eq_c", "dp_eq_c", "dq_bd_c", "dp_bd_c", "equity_pre", "bond_pre",
               "eq_share_of_w_pre_mean","eq_share_of_pf_pre_mean")
stopifnot(all(c(TRT, TRT_CTRL) %in% load_cols))

# mmap = FALSE: no file lock on Windows
est_dt <- setDT(read_parquet(data_path, col_select = all_of(load_cols), mmap = FALSE))

# eps <- c("china_crash_201508","covid_202002","snb_floor_out_201501","svb_cs_202303",
#          "taper_tantrum_201305","volmageddon_201801","xmas_plunge_201811")
# eps <- c("china_crash_201508","covid_202002","svb_cs_202303",
#          "taper_tantrum_201305","volmageddon_201801","xmas_plunge_201811")
# 
# est_dt <- est_dt[ep_id %chin% eps]

# ---- Sample ------------------------------------------------------------------
# Restrict window BEFORE winsorizing so quantiles refer to the estimation sample
est_dt <- est_dt[between(rel_month, win[1], win[2])]
# est_dt <- est_dt[ep_id %chin% eps]
est_dt <- est_dt[tot_wealth_pre > 0 & tot_wealth_pre_mean > 0]  # avoids Inf in w_rel / log
if (drop_nonpos_wealth) est_dt <- est_dt[tot_wealth > 0]


# est_dt <- est_dt[tot_pf_pre > 5000]
# 
# est_dt <- est_dt[tot_pf > 0] # ==> CHECK IF THIS MAKES SENSE


# Integer IDs (faster and leaner than paste + factor)
est_dt[, ci := .GRP, by = .(Bp_ID, ep_id)]
est_dt[, te := .GRP, by = .(MDate, ep_id)]
est_dt[, advisor_id := .GRP, by = advisor_id]
est_dt[, rel_month := as.integer(rel_month)]

# ---- Outcomes ----------------------------------------------------------------
# *_pct       : scaled by pre-period wealth
# *_pct_of_pf : scaled by pre-period portfolio
# No conditioning on cash != 0 / hypo != 0 (would drop clients who went to zero).
est_dt[, `:=`(
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
  dq_bd_pct_of_pf  = dq_bd_c / tot_pf_pre,
  dp_pct_to_pf     = dp_tot_pf_c / tot_pf_pre
)]


# =================================================
## create return variables

# sample restriction at client-episode level (tot_pf_pre = value at m = -1)
# est_dt[,uniqueN(Bp_ID),by=.(ep_id)]
# est_dt <- est_dt[tot_pf_pre > 0]
# est_dt[,uniqueN(Bp_ID),by=.(ep_id)]
est_dt <- est_dt[tot_pf_pre >= 5000]
# est_dt[,uniqueN(Bp_ID),by=.(ep_id)]

setorder(est_dt, ep_id, Bp_ID, MDate)
est_dt[, pf_prev := shift(tot_pf), by = .(ep_id, Bp_ID)]

# monthly return; 0 after liquidation (cash) and in first window month
est_dt[, valid_ret := !is.na(pf_prev) & pf_prev > 0]
est_dt[, pf_ret := fifelse(valid_ret, (dp_tot_pf + dfx_tot_pf) / pf_prev, 0)]

# winsorize per episode-month, quantiles on real returns only
est_dt[, pf_ret_w := {
  q <- quantile(pf_ret[valid_ret], c(.01, .99), na.rm = TRUE)
  fifelse(valid_ret, pmin(pmax(pf_ret, q[1]), q[2]), 0)
}, by = .(ep_id, MDate)]

# cumulative index, rebased to m = -1
est_dt[, `:=`(
  pf_idx   = cumprod(1 + pf_ret),
  pf_idx_w = cumprod(1 + pf_ret_w)
), by = .(ep_id, Bp_ID)]
est_dt[, `:=`(
  pf_idx   = pf_idx   / pf_idx[rel_month == -1]   - 1,
  pf_idx_w = pf_idx_w / pf_idx_w[rel_month == -1] - 1
), by = .(ep_id, Bp_ID)]

# selling outcomes
liq_thr <- 100  # CHF, treat residual dust as empty
est_dt[, empty_pf := as.integer(tot_pf < liq_thr)]
est_dt[, ever_liq := as.integer(cummax(empty_pf * (rel_month >= 0))),
       by = .(ep_id, Bp_ID)]
est_dt[, big_sale := as.integer(valid_ret & dq_tot_pf / pf_prev < -0.2)]


# =================================================

# tot_pf_pre <= 0 (38% of rows) -> NaN / +-Inf in everything scaled by the pre-period
# portfolio. Those rows carry no information about a portfolio return, and left in they
# break the winsorizing below (see winsor()), so they are set to NA here.
# NB the suffix is _pct_to_pf for dp_pct_to_pf and _pct_of_pf for the rest.
pf_scaled <- c("dp_pct_to_pf", grep("_pct_of_pf$", names(est_dt), value = TRUE))
est_dt[tot_pf_pre <= 0, (pf_scaled) := NA_real_]

nrow(est_dt)
est_dt[,uniqueN(Bp_ID),by=.(ep_id)]
# est_dt <- est_dt[tot_pf_pre > 0]

nrow(est_dt)
est_dt[,uniqueN(Bp_ID),by=.(ep_id)]


# Shares are bounded -> do not winsorize, just check
for (v in c("pf_share_of_w", "eq_share_of_pf", "eq_share_of_w")) {
  n_bad <- est_dt[!between(get(v), 0, 1), .N]
  if (n_bad > 0) warning(sprintf("%s: %d obs outside [0,1] - fix upstream", v, n_bad))
}

# ---- Winsorize (for stand-alone regressions only) ----------------------------
# Inf-safe: quantile() keeps Inf, so a single Inf in the upper tail makes the 99%
# cut-off Inf and pmin() a no-op -- i.e. no winsorizing at all on that side, while
# -Inf gets clipped to a finite value and stays in the regression as if it were data.
winsor <- function(x, p = 0.99) {
  q <- quantile(x[is.finite(x)], c(1 - p, p), na.rm = TRUE)
  x[is.infinite(x)] <- NA_real_
  pmin(pmax(x, q[1]), q[2])
}
flow_vars <- c("w_rel", "dw_pct", "dp_pct", "dq_pct", "dfx_pct", "cash_liq_pct",
               "cash_locked_pct", "hypo_pct", "other_credit_pct",
               "dp_eq_pct_of_pf", "dq_eq_pct_of_pf", "dp_bd_pct_of_pf", "dq_bd_pct_of_pf",
               "dp_pct_to_pf","dp_tot_pf_c","dp_tot_pf","dfx_tot_pf","tot_pf")
wdt <- copy(est_dt)
wdt[, (flow_vars) := lapply(.SD, winsor, p = p_win), .SDcols = flow_vars]





# ---- Specification -----------------------------------------------------------
# If anlagepaket_pre / adv_segment_pre are categorical, set ref2 in i(),
# otherwise fixest drops a collinear level arbitrarily.
ev_terms <- function(v) sprintf("i(rel_month, %s, ref = %d)", v, REF)
rhs <- paste(ev_terms(c(TRT, TRT_CTRL, "log_w_pre_mean",
                        "anlagepaket_pre", "adv_segment_pre",
                        "d_deposit_pre_sum","eq_share_of_w_pre_mean")), collapse = " + ")
# rhs <- paste(ev_terms(c(TRT, "log_w_pre_mean",
#                         "anlagepaket_pre", "adv_segment_pre",
#                         "d_deposit_pre_sum","eq_share_of_w_pre_mean")), collapse = " + ")


# Returns a plain named list of fixest models (names = outcomes)
est <- function(lhs, data) {
  lhs_str <- if (length(lhs) == 1) lhs else sprintf("c(%s)", paste(lhs, collapse = ", "))
  fml <- as.formula(paste(lhs_str, "~", rhs, "| ci + te"))
  m <- feols(fml, data = data, vcov = ~advisor_id,
             lean = TRUE, fixef.tol = 1e-5, mem.clean = TRUE)
  if (length(lhs) == 1) return(setNames(list(m), lhs))
  setNames(as.list(m), lhs)
}

# ---- Plot helpers (base iplot) -----------------------------------------------
pal  <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E")
pchs <- c(19, 17, 15, 18, 8)

plot_es <- function(mods, main = "Event-study estimates") {
  k <- length(mods)
  iplot(mods, main = main,
        xlab = "Months relative to pre-crisis month",
        ylab = "Estimate (95% CI)",
        col = pal[1:k], pt.pch = pchs[1:k],
        pt.join = TRUE, pt.join.par = list(lwd = 2),
        sep = 0.15, ci.lwd = 1.5, ci.width = 0.1)
  legend("topleft", legend = names(mods),
         col = pal[1:k], pch = pchs[1:k],
         lty = 1, lwd = 1.5, bty = "n", cex = 0.9)
}

# Draws on screen and writes the same plot to fig_dir as PDF
save_es <- function(mods, main, file, width = 8, height = 5) {
  plot_es(mods, main)
  path <- file.path(fig_dir, file)
  pdf(path, width = width, height = height)
  on.exit(dev.off())
  plot_es(mods, main)
  invisible(path)
}

# # ---- Stand-alone outcomes (winsorized) ---------------------------------------
# # Plot groups = single source of truth for outcomes, labels and figure files
# groups <- list(
#   levels = list(
#     title  = "Wealth and its components (share of pre-period wealth)",
#     vars   = c("dw_pct", "dp_pct", "dq_pct", "cash_liq_pct"),
#     labels = c("Wealth / pre-period wealth", "PF price change",
#                "PF quantity change", "Liquid cash change")
#   ),
#   shares = list(
#     title  = "Portfolio shares",
#     vars   = c("pf_share_of_w", "eq_share_of_pf", "eq_share_of_w"),
#     labels = c("Portfolio / wealth", "Equity / portfolio", "Equity / wealth")
#   ),
#   equity = list(
#     title  = "Equity changes (share of pre-period portfolio)",
#     vars   = c("dq_eq_pct_of_pf", "dp_eq_pct_of_pf"),
#     labels = c("Equity quantity change", "Equity price change")
#   ),
#   pf1 = list(
#     title  = "Portfolio Price changes (share of pre-period portfolio)",
#     vars   = c("dp_pct_to_pf"),
#     labels = c("Portfolio price change")
#   ),
#   pf2 = list(
#     title  = "Portfolio Price changes (share of pre-period wealth)",
#     vars   = c("dp_pct"),
#     labels = c("Portfolio price change")
#   ),
#   pf3 = list(
#       title  = "Portfolio Return",
#       vars   = c("pf_idx_w"),
#       labels = c("Portfolio Return (wins)")
#   )
# )
# # add as needed: "dfx_pct", "cash_locked_pct", "hypo_pct", "other_credit_pct"
# 
# y_main <- unique(unlist(lapply(groups, `[[`, "vars")))
# m <- est(y_main, wdt)
# 
# # iplot(m)
# 
# etable(m, keep = TRT)
# etable(m, keep = TRT, tex = TRUE, replace = TRUE,
#        file = file.path(tab_dir, sprintf("es_main_%s.tex", TRT)))
# 
# for (g in names(groups)) {
#   grp  <- groups[[g]]
#   mods <- setNames(m[grp$vars], grp$labels)
#   save_es(mods, main = grp$title, file = sprintf("es_%s_%s.pdf", g, TRT))
# }
# 
# iplot(m[groups[["levels"]]$vars[1]])

# ---- Additive decomposition --------------------------------------------------
# Identity: dw = dp + dq + dfx + dcash_liq + dcash_locked + residual.
# Common sample, trimmed rows (not per-variable winsorizing), explicit residual
# -> coefficients add up exactly (OLS is linear).
comp_lab <- c(dp_pct          = "PF price",
              dq_pct          = "PF quantity",
              dfx_pct         = "PF FX",
              cash_liq_pct    = "Liquid cash",
              cash_locked_pct = "Locked cash",
              resid_pct       = "Residual")

# dec_dt <- na.omit(est_dt, cols = c("dw_pct", setdiff(names(comp_lab), "resid_pct")))
# q <- quantile(dec_dt$dw_pct, c(1 - p_win, p_win))
# dec_dt <- dec_dt[between(dw_pct, q[1], q[2])]


wdt[, resid_pct := dw_pct - dp_pct - dq_pct - dfx_pct - cash_liq_pct - cash_locked_pct]
# If mortgage is netted in tot_wealth: add hypo_pct as a component instead of leaving it in the residual.

m_dec <- est(c("dw_pct", names(comp_lab)), wdt)

iplot(m_dec$dw_pct)

get_ct <- function(mod, label, term = TRT, ref = REF) {
  ct <- as.data.table(coeftable(mod), keep.rownames = "coef")
  ct <- ct[grepl(paste0(":", term, "$"), coef)]
  ct[, rel_month := as.numeric(sub("^rel_month::(-?[0-9.]+):.*$", "\\1", coef))]
  ct <- ct[, .(rel_month, est = Estimate, se = `Std. Error`)]
  ct <- rbind(ct, data.table(rel_month = ref, est = 0, se = 0))  # reference period
  ct[, outcome := label][order(rel_month)]
}

comp <- rbindlist(lapply(names(comp_lab), \(v) get_ct(m_dec[[v]], comp_lab[[v]])))
comp[, outcome := factor(outcome, levels = comp_lab)]
tot  <- get_ct(m_dec$dw_pct, "Total wealth")

# Sanity check: components must add up to the total
chk <- merge(comp[, .(sum_comp = sum(est)), by = rel_month], tot, by = "rel_month")
stopifnot(chk[, max(abs(sum_comp - est))] < 1e-4)

# Drawn by decomp_fig.R so that this figure and its placebo counterpart share
# one y-axis; decomp_render() redraws every stored variant of the group.
decomp_save(paste0("wealth_", TRT), "crisis", comp, tot, list(
  fig_dir  = fig_dir,
  file     = sprintf("decomp_wealth_%s.pdf", TRT),
  title    = "Effect on total wealth, decomposed",
  subtitle = sprintf("Treatment: %s, N = %s (client x crisis x month); 95%% CI clustered by advisor",
                     TRT_LAB, format(nobs(m_dec$dw_pct), big.mark = "'")),
  ylab     = "Change in wealth (share of pre-period wealth)",
  fills    = c("#4E79A7", "#59A14F", "#76B7B2", "#E15759", "#F28E2B", "grey70"),
  xbreaks  = seq(win[1], win[2], 2), ref = REF))
p_dec <- decomp_render(paste0("wealth_", TRT), current = "crisis")
p_dec

etable(m_dec, keep = TRT, tex = TRUE, replace = TRUE,
       file = file.path(tab_dir, sprintf("es_decomp_%s.tex", TRT)))

# # ---- Additive decomposition: total portfolio (no cash) -----------------------
# # Identity: dPF = dp + dq + dfx + residual, all scaled by pre-period portfolio.
# # tot_pf (02a: is_pf) = all non-cash, non-credit positions, i.e. equity, bond,
# # reales, fund_mixed, deriv, alt (+ any instrument group not mapped to a class).
# # Same logic as above: common sample, trimmed rows, explicit residual.
# comp_lab_pf <- c(dp_pf_pct  = "PF price",
#                  dq_pf_pct  = "PF quantity",
#                  dfx_pf_pct = "PF FX",
#                  resid_pf_pct = "Residual")
# 
# dec_pf <- est_dt[tot_pf_pre > 0]   # avoids Inf when scaling by tot_pf_pre
# dec_pf[, `:=`(
#   dpf_pct    = tot_pf / tot_pf_pre - 1,
#   dp_pf_pct  = dp_tot_pf_c  / tot_pf,
#   dq_pf_pct  = dq_tot_pf_c  / tot_pf,
#   dfx_pf_pct = dfx_tot_pf_c / tot_pf
# )]
# dec_pf <- na.omit(dec_pf, cols = c("dpf_pct", "dp_pf_pct", "dq_pf_pct", "dfx_pf_pct"))
# q_pf <- quantile(dec_pf$dpf_pct, c(1 - p_win, p_win))
# dec_pf <- dec_pf[between(dpf_pct, q_pf[1], q_pf[2])]
# dec_pf[, resid_pf_pct := dpf_pct - dp_pf_pct - dq_pf_pct - dfx_pf_pct]
# 
# m_dec_pf <- est(c("dpf_pct", names(comp_lab_pf)), dec_pf)
# 
# iplot(m_dec_pf["dp_pf_pct"])
# 
# comp_pf <- rbindlist(lapply(names(comp_lab_pf), \(v) get_ct(m_dec_pf[[v]], comp_lab_pf[[v]])))
# comp_pf[, outcome := factor(outcome, levels = comp_lab_pf)]
# tot_pf_ct <- get_ct(m_dec_pf$dpf_pct, "Total portfolio")
# 
# chk_pf <- merge(comp_pf[, .(sum_comp = sum(est)), by = rel_month], tot_pf_ct, by = "rel_month")
# stopifnot(chk_pf[, max(abs(sum_comp - est))] < 1e-4)
# 
# decomp_save(paste0("pf_", TRT), "crisis", comp_pf, tot_pf_ct, list(
#   fig_dir  = fig_dir,
#   file     = sprintf("decomp_pf_%s.pdf", TRT),
#   title    = "Effect on total portfolio value, decomposed",
#   subtitle = sprintf("Treatment: %s, N = %s (client x crisis x month); 95%% CI clustered by advisor",
#                      TRT_LAB, format(nobs(m_dec_pf$dpf_pct), big.mark = "'")),
#   ylab     = "Change in portfolio (share of pre-period portfolio)",
#   fills    = c("#4E79A7", "#59A14F", "#76B7B2", "grey70"),
#   xbreaks  = seq(win[1], win[2], 2), ref = REF))
# p_dec_pf <- decomp_render(paste0("pf_", TRT), current = "crisis")
# p_dec_pf
# 
# etable(m_dec_pf, keep = TRT, tex = TRUE, replace = TRUE,
#        file = file.path(tab_dir, sprintf("es_decomp_pf_%s.tex", TRT)))
