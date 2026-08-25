

S_main  <- stk[smp_main  == 1L]      # non-discretionary
S_cycle <- stk[smp_cycle == 1L]


TRT <- "treat_adv"

spec1 <-  as.formula(sprintf("netflow_r ~ i(rel_month, %s, ref = -1) + i(rel_month, treat_cli, ref = -1) +
            (log_w_pre + n_assets_pre + contacts_pre) : factor(rel_month) | ci + te",TRT))
spec2 <-  as.formula(sprintf("sold ~ i(rel_month, %s, ref = -1) + i(rel_month, treat_cli, ref = -1) +
             (log_w_pre + n_assets_pre + contacts_pre) : factor(rel_month) | ci + te",TRT))
spec3 <-  as.formula(sprintf("d_eq_pre ~ i(rel_month, %s, ref = -1) + i(rel_month, treat_cli, ref = -1) +
             (log_w_pre + n_assets_pre + contacts_pre) : factor(rel_month) | ci + te",TRT))
spec4 <-  as.formula(sprintf("d_risky_pre ~ i(rel_month, %s, ref = -1) + i(rel_month, treat_cli, ref = -1) +
             (log_w_pre + n_assets_pre + contacts_pre) : factor(rel_month) | ci + te",TRT))
spec5 <-  as.formula(sprintf("w_rel ~ i(rel_month, %s, ref = -1) + i(rel_month, treat_cli, ref = -1) +
           (log_w_pre + n_assets_pre + contacts_pre) : factor(rel_month) | ci + te",TRT))
spec6 <-  as.formula(sprintf("cf_gap_mkt ~ i(rel_month, %s, ref = -1) + i(rel_month, treat_cli, ref = -1) +
             (log_w_pre + n_assets_pre + contacts_pre) : factor(rel_month) | ci + te",TRT))
spec7 <-  as.formula(sprintf("cf_gap_hold ~ i(rel_month, %s, ref = -1) + i(rel_month, treat_cli, ref = -1) +
             (log_w_pre + n_assets_pre + contacts_pre) : factor(rel_month) | ci + te",TRT))
spec8 <-  as.formula(sprintf("pf_ret ~ i(rel_month, %s, ref = -1) + i(rel_month, treat_cli, ref = -1) +
             (log_w_pre + n_assets_pre + contacts_pre) : factor(rel_month) | ci + te",TRT))

 
m_treat_adv <- list()
 

dta <- S_main[ep_id == "ep5_202112"]


m_treat_adv[["netflow_r"]] <- feols(spec1,data=dta,vcov = ~advisor_id)
m_treat_adv[["sold"]] <- feols(spec2,data=dta,vcov = ~advisor_id)
m_treat_adv[["d_eq_pre"]] <- feols(spec3,data=dta,vcov = ~advisor_id)
m_treat_adv[["d_risky_pre"]] <- feols(spec4,data=dta,vcov = ~advisor_id)
m_treat_adv[["w_rel"]] <- feols(spec5,data=dta,vcov = ~advisor_id)
m_treat_adv[["cf_gap_mkt"]] <- feols(spec6,data=dta,vcov = ~advisor_id)
m_treat_adv[["cf_gap_hold"]] <- feols(spec7,data=dta,vcov = ~advisor_id)
m_treat_adv[["pf_ret"]] <- feols(spec8,data=dta,vcov = ~advisor_id)


iplot(m_treat_adv)
iplot(m_treat_adv$netflow_r)
iplot(m_treat_adv$sold)
iplot(m_treat_adv$d_eq_pre)
iplot(m_treat_adv$d_risky_pre)
iplot(m_treat_adv$w_rel)
iplot(m_treat_adv$cf_gap_mkt)
iplot(m_treat_adv$cf_gap_hold)
iplot(m_treat_adv$pf_ret)


# m_treat_adv_S_main <- copy(m_treat_adv)
