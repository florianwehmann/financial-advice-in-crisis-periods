# =============================================================================
# Estimation: event-study specs (stacked episodes, client-episode + time-episode FE)
# Run in a fresh R session (instead of rm(list = ls())).
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(arrow)
  library(ggplot2)
  library(ggfixest)
})

rm(list=ls());gc()

# ---- Config ------------------------------------------------------------------
data_path <- "../../data/stk1.parquet"
overleaf_dir <- "C:/Users/FWehmann/Dropbox/Apps/Overleaf/Uncertainty and Financial Advise (1)"
fig_dir <- file.path(overleaf_dir, "figures")
tab_dir <- file.path(overleaf_dir, "tables")
for (d in c(fig_dir, tab_dir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)
# eps <- c("ep1_201305", "ep2_201501", "ep3_201508", "ep6_201801",
#          "ep7_201811", "ep9_202002", "ep10_202201", "ep12_202310")
# eps <- c("ep1_201305", "ep2_201501", "ep3_201508", "ep6_201801",
#          "ep7_201811", "ep9_202002", "ep11_202303")
# Treatment of interest + treatments held fixed as controls
TRT      <- "treat_inv_a_p"  # "treat_inv_a_p"   "treat_perfinv_a_p"
TRT_CTRL <- "treat_inv_c_p"
REF      <- -1L        # reference period

win   <- c(-6L, 8L)   # event window
p_win <- 0.99          # winsor / trim quantile

# Dropping tot_wealth <= 0 conditions on the outcome -> selection.
# TRUE only replicates the old results; FALSE is cleaner.
drop_nonpos_wealth <- TRUE

setFixest_nthreads(parallel::detectCores())
setFixest_notes(FALSE)

# ---- Load (only needed columns) ---------------------------------------------
cols <- c("Bp_ID", "ep_id", "MDate", "advisor_id", "rel_month",
          "tot_wealth", "tot_wealth_pre", "tot_wealth_pre_mean", "tot_pf_pre",
          "dp_tot_pf_c", "dq_tot_pf_c", "dfx_tot_pf_c",
          "cash_liq", "cash_liq_pre", "cash_locked", "cash_locked_pre",
          "hypo", "hypo_pre", "credit_check", "credit_check_pre",
          "treat_perfinv_a_p", "treat_inv_a_p", "treat_inv_c_p",
          "anlagepaket_pre", "adv_segment_pre",
          "pf_share_of_w", "eq_share_of_pf", "eq_share_of_w",
          "dq_eq_c","dp_eq_c","dq_bd_c","dp_bd_c","equity_pre","bond_pre")

est_dt <- setDT(read_parquet(data_path, col_select = cols))
# stk <- setDT(read_parquet(data_path))

# ---- Sample ------------------------------------------------------------------
# Restrict window BEFORE winsorizing so quantiles refer to the estimation sample
est_dt <- est_dt[between(rel_month, win[1], win[2])]
# est_dt <- est_dt[ep_id %chin% eps & between(rel_month, win[1], win[2])]
est_dt <- est_dt[tot_wealth_pre > 0 & tot_wealth_pre_mean > 0]  # avoids Inf in w_rel / log
if (drop_nonpos_wealth) est_dt <- est_dt[tot_wealth > 0]

# IDs as integers directly (faster and leaner than paste + factor)
est_dt[, ci := .GRP, by = .(Bp_ID, ep_id)]
est_dt[, te := .GRP, by = .(MDate, ep_id)]
est_dt[, advisor_id := .GRP, by = advisor_id]
est_dt[, rel_month := as.integer(rel_month)]

# ---- Outcomes (all scaled by pre-period wealth) ------------------------------
# No conditioning on cash != 0 / hypo != 0: that drops exactly the clients
# who went to zero. Missing values simply propagate as NA.
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
  dq_eq_pcf_of_pf  = dq_eq_c / tot_pf_pre,
  dp_bd_pct_of_pf  = dp_bd_c / tot_pf_pre,
  dq_bd_pct_of_pf  = dq_bd_c / tot_pf_pre
)]

# Shares are bounded -> do not winsorize, just check
for (v in c("pf_share_of_w", "eq_share_of_pf", "eq_share_of_w")) {
  n_bad <- est_dt[!between(get(v), 0, 1), .N]
  if (n_bad > 0) warning(sprintf("%s: %d obs outside [0,1] – fix upstream", v, n_bad))
}

# ---- Winsorize (for stand-alone regressions only) ----------------------------
winsor <- function(x, p = 0.99) {
  q <- quantile(x, c(1 - p, p), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}
flow_vars <- c("w_rel", "dp_pct", "dq_pct", "dfx_pct", "cash_liq_pct",
               "cash_locked_pct", "hypo_pct", "other_credit_pct",
               "dp_eq_pct_of_pf","dq_eq_pcf_of_pf","dp_bd_pct_of_pf","dq_bd_pct_of_pf")
wdt <- copy(est_dt)
wdt[, (flow_vars) := lapply(.SD, winsor, p = p_win), .SDcols = flow_vars]

# ---- Specification -----------------------------------------------------------
# Note: if anlagepaket_pre / adv_segment_pre are categorical, set ref2 in i(),
# otherwise one level is collinear with te and fixest drops one arbitrarily.
stopifnot(all(c(TRT, TRT_CTRL) %in% cols))

ev_terms <- function(v) sprintf("i(rel_month, %s, ref = %d)", v, REF)
rhs <- paste(ev_terms(c(TRT, TRT_CTRL, "log_w_pre_mean",
                        "anlagepaket_pre", "adv_segment_pre")), collapse = " + ")

est <- function(lhs, data) {
  lhs_str <- if (length(lhs) == 1) lhs else sprintf("c(%s)", paste(lhs, collapse = ", "))
  fml <- as.formula(paste(lhs_str, "~", rhs, "| ci + te"))
  m <- feols(fml, data = data, vcov = ~advisor_id,
             lean = TRUE, fixef.tol = 1e-5, mem.clean = TRUE)
  if (length(lhs) == 1) return(setNames(list(m), lhs))
  setNames(as.list(m), lhs)  # multi-LHS: order = lhs order
}

# ---- Stand-alone outcomes (winsorized) ---------------------------------------
y_main <- c("w_rel", "dp_pct", "dq_pct", "cash_liq_pct",
            "pf_share_of_w", "eq_share_of_pf", "eq_share_of_w",
            "dq_eq_pcf_of_pf","dp_eq_pct_of_pf")
# add as needed: "dfx_pct", "cash_locked_pct", "hypo_pct", "other_credit_pct"
m <- est(y_main, wdt)

iplot(m[c("pf_share_of_w", "eq_share_of_pf", "eq_share_of_w")])
iplot(m[c("w_rel", "dp_pct", "dq_pct", "cash_liq_pct")])
iplot(m[c("dq_eq_pcf_of_pf", "dp_eq_pct_of_pf")])
etable(m, keep = TRT)
etable(m, keep = TRT, tex = TRUE, replace = TRUE,
       file = file.path(tab_dir, sprintf("es_main_%s.tex", TRT)))


pick <- function(m, vars, labels = vars) {
  mods <- lapply(vars, function(v) {
    if (inherits(m, "fixest_multi")) {
      r <- m[lhs = paste0("^", v, "$")]
      if (inherits(r, "fixest_multi")) r[[1]] else r
    } else {
      m[[v]]                      # if est() returns a plain named list
    }
  })
  setNames(mods, labels)
}

shares <- pick(m,
               c("pf_share_of_w", "eq_share_of_pf", "eq_share_of_w"),
               c("Portfolio / wealth", "Equity / portfolio", "Equity / wealth"))
cols3 <- c("#1B9E77", "#D95F02", "#7570B3")

shares_pf <- pick(m,
               c("dq_eq_pcf_of_pf", "dp_eq_pct_of_pf"),
               c("Equity Quantity Change (in pct of PF)", "Equity Price Change (in pct of PF)"))
cols2 <- c("#1B9E77", "#D95F02")


plot_es <- function(mods, cols, main = "Event-study estimates") {
  pchs <- c(19, 17, 15, 18, 8)[seq_along(mods)]
  iplot(mods, main = main,
        xlab = "Months relative to Pre-Crisis Month", ylab = "Estimate (95% CI)",
        col = cols, pt.pch = pchs, pt.join = TRUE, sep = 0.15,
        ci.lwd = 1.5, ci.width = 0.1,lwd=2)
  legend("topleft", legend = names(mods), col = cols, pch = pchs,
         lty = 1, lwd = 1.5, bty = "n", cex = 0.9)
}

plot_es(shares, cols3, main="Shares")
plot_es(shares_pf, cols2, main="Equity")



# ---- Additive decomposition --------------------------------------------------
# Identity: dw = dp + dq + dfx + dcash_liq + dcash_locked + residual.
# Requirements: common sample, trimmed rows (not per-variable winsorizing),
# explicit residual. Then the coefficients add up exactly (OLS is linear).
comp_lab <- c(dp_pct          = "PF price",
              dq_pct          = "PF quantity",
              dfx_pct         = "PF FX",
              cash_liq_pct    = "Liquid cash",
              cash_locked_pct = "Locked cash",
              resid_pct       = "Residual")

dec_dt <- na.omit(est_dt, cols = c("dw_pct", setdiff(names(comp_lab), "resid_pct")))
q <- quantile(dec_dt$dw_pct, c(1 - p_win, p_win))
dec_dt <- dec_dt[between(dw_pct, q[1], q[2])]
dec_dt[, resid_pct := dw_pct - dp_pct - dq_pct - dfx_pct - cash_liq_pct - cash_locked_pct]
# If mortgage is netted in tot_wealth: add hypo_pct as a component instead of leaving it in the residual.

m_dec <- est(c("dw_pct", names(comp_lab)), dec_dt)

get_ct <- function(m, label, term = TRT, ref = REF) {
  ct <- as.data.table(coeftable(m), keep.rownames = "coef")
  ct <- ct[grepl(paste0(":", term, "$"), coef)]
  ct[, rel_month := as.numeric(sub("^rel_month::(-?[0-9.]+):.*$", "\\1", coef))]
  ct <- ct[, .(rel_month, est = Estimate, se = `Std. Error`)]
  ct <- rbind(ct, data.table(rel_month = ref, est = 0, se = 0))  # reference period
  ct[, outcome := label][order(rel_month)]
}

comp <- rbindlist(lapply(names(comp_lab), \(v) get_ct(m_dec[[v]], comp_lab[[v]])))
comp[, outcome := factor(outcome, levels = comp_lab)]
main <- get_ct(m_dec$dw_pct, "Total wealth")

# Sanity check: components must add up to the total
chk <- merge(comp[, .(sum_comp = sum(est)), by = rel_month], main, by = "rel_month")
stopifnot(chk[, max(abs(sum_comp - est))] < 1e-4)

p <- ggplot() +
  geom_col(data = comp, aes(rel_month, est, fill = outcome), width = 0.75, alpha = 0.85) +
  geom_errorbar(data = main, aes(rel_month, ymin = est - 1.96 * se, ymax = est + 1.96 * se),
                width = 0.25, linewidth = 0.4) +
  geom_line(data = main, aes(rel_month, est), linewidth = 0.3) +
  geom_point(data = main, aes(rel_month, est), size = 1.8) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_vline(xintercept = REF + 0.5, linetype = "dashed", linewidth = 0.4) +
  scale_fill_manual(values = c("#4E79A7", "#59A14F", "#76B7B2",
                               "#E15759", "#F28E2B", "grey70")) +
  scale_x_continuous(breaks = seq(win[1], win[2], 2)) +
  labs(x = "Months relative to event",
       y = "Change in wealth (share of pre-period wealth)",
       fill = NULL,
       title = sprintf("Effect of %s on total wealth, decomposed", TRT),
       subtitle = sprintf("Treatment: Adv-init. personal Inv-Advise, N = %s (client x crisis x month); 95%% CI clustered by advisor",
                          format(nobs(m_dec$dw_pct), big.mark = "'"))) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
p

ggsave(file.path(fig_dir, sprintf("decomp_wealth_%s.pdf", TRT)), p, width = 8, height = 5)

etable(m_dec, keep = TRT, tex = TRUE, replace = TRUE,
       file = file.path(tab_dir, sprintf("es_decomp_%s.tex", TRT)))
