# estimation / specs
library(fixest)
library(arrow)

rm(list=ls());gc()

stk <- read_parquet("../../data/stk.parquet")


stk[, ci := paste(Bp_ID, ep_id)]
stk[, te := paste(MDate, ep_id)]  

setindex(stk, ep_id)
# episodes to keep
eps <- c("ep1_201305", "ep2_201501", "ep3_201508", "ep6_201801", "ep7_201811", "ep9_202002", "ep10_202201", "ep12_202310")
# eps <- "ep12_202310"
# eps <- c("ep9_202002")
# columns to keep
keep <- c("Bp_ID","ep_id","ci","te","advisor_id","cash_liq","cash_locked",
          "tot_wealth","tot_wealth_pre","tot_wealth_pre_mean","dp_tot_pf_c",
          "dq_tot_pf_c","tot_pf_pre","dfx_tot_pf_c","hypo","hypo_pre",
          "rel_month","treat_perfinv_a_p","treat_inv_a_p","treat_inv_c_p",
          "cash_liq_pre","cash_locked_pre","anlagepaket_pre","adv_segment_pre",
          "pf_share_of_w","eq_share_of_pf","eq_share_of_w","credit_check","credit_check_pre")
est_dt <- stk[ep_id %chin% eps, ..keep]

# est_dt <- na.omit(est_dt)

est_dt <- est_dt[tot_wealth>0]


est_dt[,log_w_pre_mean := log(tot_wealth_pre_mean)]

est_dt[,w_rel := tot_wealth / tot_wealth_pre ]
est_dt[tot_pf_pre > 0, dp_pct := dp_tot_pf_c / tot_wealth_pre]
est_dt[tot_pf_pre >0, dq_pct := dq_tot_pf_c / tot_wealth_pre]
est_dt[tot_pf_pre >0, dfx_pct := dfx_tot_pf_c / tot_wealth_pre]
est_dt[!is.na(cash_liq) & cash_liq!=0, cash_liq_pct := (cash_liq-cash_liq_pre) / tot_wealth_pre]
est_dt[!is.na(cash_locked) & cash_locked!=0, cash_locked_pct := (cash_locked-cash_locked_pre) / tot_wealth_pre]
est_dt[hypo!=0, hypo_pct := (hypo-hypo_pre) / tot_wealth_pre]
est_dt[hypo!=0, other_credit_pct := ((credit_check-hypo)-(credit_check_pre-hypo_pre)) / tot_wealth_pre]


# trim scaled outcomes (06_prep no longer winsorizes CHF levels within client)


# trim <- function(x, p = 0.01) { q <- quantile(x, c(p, 1 - p), na.rm = TRUE); pmin(pmax(x, q[1]), q[2]) }
# est_dt[, `:=`(w_rel = trim(w_rel), dp_pct = trim(dp_pct))]

winsor <- function(x, p = 0.99) {
  q <- quantile(x, c(1 - p, p), na.rm = TRUE, type = 7)
  pmin(pmax(x, q[1]), q[2])
}
est_dt[, `:=`(w_rel = winsor(w_rel),
              dp_pct = winsor(dp_pct),
              dq_pct = winsor(dq_pct),
              dfx_pct = winsor(dfx_pct),
              cash_liq_pct = winsor(cash_liq_pct),
              cash_locked_pct = winsor(cash_locked_pct),
              hypo_pct = winsor(hypo_pct),
              pf_share_of_w = winsor(pf_share_of_w),
              eq_share_of_pf = winsor(eq_share_of_pf),
              eq_share_of_w = winsor(eq_share_of_w),
              other_credit_pct = winsor(other_credit_pct))]




est_dt[is.infinite(est_dt$w_rel)]

est_dt <- est_dt[rel_month %in% c(-8:12)]


est_dt[, `:=`(ci = as.integer(factor(ci)),
              te = as.integer(factor(te)),
              ep_id = as.integer(factor(ep_id)),
              advisor_id = as.integer(factor(advisor_id)),
              rel_month = as.integer(rel_month))]




spec_w <- w_rel ~ i(rel_month, treat_perfinv_a_p,   ref = -1) +
  i(rel_month, treat_inv_c_p,   ref = -1) +
  i(rel_month, log_w_pre_mean,  ref = -1) +
  i(rel_month, anlagepaket_pre, ref = -1) +
  i(rel_month, adv_segment_pre, ref = -1) | ci + te  #  | Bp_ID + rel_month 

spec_p <- dp_pct ~ i(rel_month, treat_perfinv_a_p,   ref = -1) +
  i(rel_month, treat_inv_c_p,   ref = -1) +
  i(rel_month, log_w_pre_mean,  ref = -1) +
  i(rel_month, anlagepaket_pre, ref = -1) +
  i(rel_month, adv_segment_pre, ref = -1) | ci + te

spec_q <- dq_pct ~ i(rel_month, treat_perfinv_a_p,   ref = -1) +
  i(rel_month, treat_inv_c_p,   ref = -1) +
  i(rel_month, log_w_pre_mean,  ref = -1) +
  i(rel_month, anlagepaket_pre, ref = -1) +
  i(rel_month, adv_segment_pre, ref = -1) | ci + te

# spec_fx <- dfx_pct ~ i(rel_month, treat_perfinv_a_p,   ref = -1) +
#   i(rel_month, treat_inv_c_p,   ref = -1) +
#   i(rel_month, log_w_pre_mean,  ref = -1) +
#   i(rel_month, anlagepaket_pre, ref = -1) +
#   i(rel_month, adv_segment_pre, ref = -1) | ci + te

spec_cash <- cash_liq_pct ~ i(rel_month, treat_perfinv_a_p,   ref = -1) +
  i(rel_month, treat_inv_c_p,   ref = -1) +
  i(rel_month, log_w_pre_mean,  ref = -1) +
  i(rel_month, anlagepaket_pre, ref = -1) +
  i(rel_month, adv_segment_pre, ref = -1) | ci + te

# spec_cash_locked <- cash_locked_pct ~ i(rel_month, treat_perfinv_a_p,   ref = -1) +
#   i(rel_month, treat_inv_c_p,   ref = -1) +
#   i(rel_month, log_w_pre_mean,  ref = -1) +
#   i(rel_month, anlagepaket_pre, ref = -1) +
#   i(rel_month, adv_segment_pre, ref = -1) | ci + te

# spec_hypo <- hypo_pct ~ i(rel_month, treat_perfinv_a_p,   ref = -1) +
#   i(rel_month, treat_inv_c_p,   ref = -1) +
#   i(rel_month, log_w_pre_mean,  ref = -1) +
#   i(rel_month, anlagepaket_pre, ref = -1) +
#   i(rel_month, adv_segment_pre, ref = -1) | ci + te

spec_pf_share <- pf_share_of_w ~ i(rel_month, treat_perfinv_a_p,   ref = -1) +
  i(rel_month, treat_inv_c_p,   ref = -1) +
  i(rel_month, log_w_pre_mean,  ref = -1) +
  i(rel_month, anlagepaket_pre, ref = -1) +
  i(rel_month, adv_segment_pre, ref = -1) | ci + te

spec_eq_share_pf <- eq_share_of_pf ~ i(rel_month, treat_perfinv_a_p,   ref = -1) +
  i(rel_month, treat_inv_c_p,   ref = -1) +
  i(rel_month, log_w_pre_mean,  ref = -1) +
  i(rel_month, anlagepaket_pre, ref = -1) +
  i(rel_month, adv_segment_pre, ref = -1) | ci + te

spec_eq_share_w <- eq_share_of_w ~ i(rel_month, treat_perfinv_a_p,   ref = -1) +
  i(rel_month, treat_inv_c_p,   ref = -1) +
  i(rel_month, log_w_pre_mean,  ref = -1) +
  i(rel_month, anlagepaket_pre, ref = -1) +
  i(rel_month, adv_segment_pre, ref = -1) | ci + te


# spec_other_credit <- other_credit_pct ~ i(rel_month, treat_perfinv_a_p,   ref = -1) +
#   i(rel_month, treat_inv_c_p,   ref = -1) +
#   i(rel_month, log_w_pre_mean,  ref = -1) +
#   i(rel_month, anlagepaket_pre, ref = -1) +
#   i(rel_month, adv_segment_pre, ref = -1) | ci + te




setFixest_nthreads(parallel::detectCores())   # default is only 50% of cores

m_w <- feols(spec_w, est_dt, vcov = ~advisor_id, notes = FALSE,
           lean = TRUE, fixef.tol = 1e-5, mem.clean = TRUE)
m_p <- feols(spec_p, est_dt, vcov = ~advisor_id, notes = FALSE,
             lean = TRUE, fixef.tol = 1e-5, mem.clean = TRUE)
m_q <- feols(spec_q, est_dt, vcov = ~advisor_id, notes = FALSE,
             lean = TRUE, fixef.tol = 1e-5, mem.clean = TRUE)
# m_fx <- feols(spec_fx, est_dt, vcov = ~advisor_id, notes = FALSE,
#              lean = TRUE, fixef.tol = 1e-5, mem.clean = TRUE)
m_cash <- feols(spec_cash, est_dt, vcov = ~advisor_id, notes = FALSE,
             lean = TRUE, fixef.tol = 1e-5, mem.clean = TRUE)
# m_cash_locked <- feols(spec_cash_locked, est_dt, vcov = ~advisor_id, notes = FALSE,
#                 lean = TRUE, fixef.tol = 1e-5, mem.clean = TRUE)

# m_hypo <- feols(spec_hypo, est_dt, vcov = ~advisor_id, notes = FALSE,
#                 lean = TRUE, fixef.tol = 1e-5, mem.clean = TRUE)
# 
m_pf_share <- feols(spec_pf_share, est_dt, vcov = ~advisor_id, notes = FALSE,
                lean = TRUE, fixef.tol = 1e-5, mem.clean = TRUE)
m_eq_share_pf <- feols(spec_eq_share_pf, est_dt, vcov = ~advisor_id, notes = FALSE,
                    lean = TRUE, fixef.tol = 1e-5, mem.clean = TRUE)
m_eq_share_w <- feols(spec_eq_share_w, est_dt, vcov = ~advisor_id, notes = FALSE,
                    lean = TRUE, fixef.tol = 1e-5, mem.clean = TRUE)


# m_other_credit <- feols(spec_other_credit, est_dt, vcov = ~advisor_id, notes = FALSE,
#                       lean = TRUE, fixef.tol = 1e-5, mem.clean = TRUE)


iplot(m_other_credit)

iplot(m_pf_share,m_eq_share_pf,m_eq_share_w)
iplot(m_pf_share)
iplot(m_eq_share_pf)
iplot(m_eq_share_w)



# spec <- w_rel ~ i(rel_month, treat_perfinv_a_p,   ref = -1) +
#   i(rel_month, treat_inv_c_p,   ref = -1) +
#   i(rel_month, log_w_pre_mean,  ref = -1) +
#   i(rel_month, anlagepaket_pre, ref = -1) +
#   i(rel_month, adv_segment_pre, ref = -1) | ci + te
# m <- feols(spec, est_dt, vcov = ~advisor_id, notes = FALSE,
#              lean = TRUE, fixef.tol = 1e-5, mem.clean = TRUE)

# iplot(m)
# 
# iplot(m_w)
# iplot(m_cash_locked)
# iplot(m_w,m_p,m_q,m_cash)

models <- list("w_rel" = m_w, "dp_pct" = m_p, "dq_pct" = m_q,
               "cash_liq_pct" = m_cash)



iplot(models)



get_ct <- function(m, label, term = "treat_perfinv_a_p") {
  ct <- as.data.table(coeftable(m), keep.rownames = "coef")
  ct <- ct[grepl(paste0(":", term, "$"), coef)]
  ct[, rel_month := as.numeric(sub("^rel_month::(-?[0-9.]+):.*$", "\\1", coef))]
  setnames(ct, c("Estimate", "Std. Error"), c("est", "se"))
  ct <- ct[, .(rel_month, est, se)]
  # fixest drops the reference period — put it back explicitly as an exact zero
  ct <- rbind(ct, data.table(rel_month = -1, est = 0, se = 0))
  ct[, outcome := label][order(rel_month)]
}

comp <- rbindlist(list(
  get_ct(m_p,    "Price (dp_pct)"),
  get_ct(m_q,    "Quantity (dq_pct)"),
  get_ct(m_cash, "Cash (cash_liq_pct)")
))
comp[, outcome := factor(outcome, levels = c("PF Price Changes",
                                             "PF Quantity Changes",
                                             "Cash Changes"))]

main <- get_ct(m_w, "w_rel")

p <- ggplot() +
  geom_col(data = comp, aes(rel_month, est, fill = outcome),
           width = 0.75, alpha = 0.8) +
  geom_errorbar(data = main, aes(rel_month, ymin = est - 1.96 * se,
                                 ymax = est + 1.96 * se),
                width = 0.25, linewidth = 0.4) +
  geom_point(data = main, aes(rel_month, est), size = 1.8) +
  geom_line(data = main, aes(rel_month, est), linewidth = 0.3) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_vline(xintercept = -1, linetype = "dashed", linewidth = 0.4) +
  scale_fill_manual(values = c("#4E79A7", "#59A14F", "#E15759")) +
  scale_x_continuous(breaks = seq(-10, 15, 5)) +
  labs(x = "Months relative to event", y = "Estimate and 95% CI",
       fill = NULL, title = "Effect on w_rel, decomposed") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())
p
