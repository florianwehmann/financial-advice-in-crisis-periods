# preparation
library(data.table)
library(arrow)
library(ggplot2)

rm(list=ls());gc()

pos <- read_parquet("../../data/pos_aggm_c.parquet")


# ep <- read_parquet("../../results/cache/episodes.parquet")

ep <- read_parquet("../../data/episodes_short.parquet")

## ============================================================================
## FUNCTIONS
mdiff <- function(a, b) as.integer((year(a) - year(b)) * 12L + (month(a) - month(b)))

winsor <- function(x, p = 0.99) {
  q <- quantile(x, c(1 - p, p), na.rm = TRUE, type = 7)
  pmin(pmax(x, q[1]), q[2])
}

# winsorize but exclude zeros
winsor_nz <- function(x, p = 0.95) {
  ok <- !is.na(x) & x != 0
  if (!any(ok)) return(x)
  q <- quantile(x[ok], c(1 - p, p), na.rm = TRUE, type = 7)
  x[ok] <- pmin(pmax(x[ok], q[1]), q[2])
  x
}


## ================================================================================
# winsorize

names(pos)

win_cols <- c("equity","bond","fund_mixed","deriv","alt",
                     "chf_net_equity","chf_net_bond","chf_gross_equity","chf_gross_bond",
                     "chf_bought","chf_sold","chf_net",
                     "dq_equity","dp_equity","dp_bond","dq_bond","dp_reales","dq_reales",
                     "dp_fund_mixed","dq_fund_mixed","dp_alt","dq_alt",
                     "dq_tot_pf","dfx_tot_pf","dpfx_tot_pf",
                     "dp_equity","dp_bond","dp_tot_pf",
                     
                     "buy_tot_pf","sell_tot_pf",
                     "tot_pf","cash_liq","cash_locked","hypo","tot_wealth")

# NOT within client (by = Bp_ID): that clips each client's own extreme months, i.e. exactly the
# crisis trough and the recovery peak. Scaled outcomes are trimmed in 07_estim.R instead.
# pos[, (win_cols) := lapply(.SD, winsor_nz), .SDcols = win_cols,by=.(Bp_ID)]
stopifnot(!anyDuplicated(pos, by = c("Bp_ID","MDate")))



## ============================================================================
## FILTER

# (a) exclude discretionary mandates (internal and external)
# pos <- pos[vv_depot==0 & (depotvol_disc_mandate==0) & anlagepaket != "COMFORT"] # internal mandates
pos <- pos[vv_depot == 0 & depotvol_disc_mandate == 0 & (is.na(anlagepaket) | anlagepaket != "COMFORT")]
pos <- pos[evv==0]      # external mandates (EAM)

pos[,`:=`(
  vv_acc = NULL,
  vv_depot = NULL,
  depotvol_disc_mandate = NULL,
  evv = NULL
)]


# (b) exclude based on advisor segment
# keep: Beratungszentrum, natürliche Personen PB, PK-Team, FK-Team, GK-Team

## institutional segment, EVV segment, CWO segment
pos <- pos[!(adv_segment %in% c("Institutionelle Anleger","EVV-Team","CWO","nicht zugeteilt"))]


# (c) exclude Anlagepaket "CONSULT international" and "CONSULT expert" (both outliers and only a hand full of clients)
pos <- pos[!(anlagepaket %in% c("CONSULT international","CONSULT expert"))]


# (d) select clients with positive PF value
m_bp <- pos[,.(mean_pf = mean(tot_pf,na.rm=T),
               mean_w = mean(tot_wealth,na.rm=T)),by=.(Bp_ID)]
bp_pf <- m_bp[mean_pf>0]$Bp_ID

# (e) select clients with more than 3 years in the data
n_bp <- pos[,.N,by=.(Bp_ID)][N>36]$Bp_ID

pos_pf <- pos[Bp_ID %in% intersect(bp_pf,n_bp)]

# (f) select clients with positive total wealth
pos_pf <- pos_pf[tot_wealth>0]

pos_pf[is.na(anlagepaket)] #1102251

uniqueN(pos_pf$Bp_ID) #12201

# 
# ggplot()+
#   geom_line(data=pos[,.N,by=MDate],aes(x=MDate,y=N,color="Total Clients in Filtered Sample"),size=1)+
#   geom_line(data=pos[Bp_ID %in% bp_pf,.N,by=MDate],aes(x=MDate,y=N,color="Clients with positive Portfolio Value"),size=1)+
#   ylim(0,NA)+
#   theme(legend.position="bottom")+
#   guides(color=guide_legend(title=NULL))+
#   labs(y="N Clients",x=NULL)
# 
# 
# 
# 
# 
# pos_pf_nclients <- pos_pf[MDate >= "2011-03-01",.(
#   N = .N,
#   n_inv_p = sum(inv_p,na.rm=T),
#   n_perfinv_p = sum(perfinv_p,na.rm=T),
#   n_inv_a_p = sum(inv_a_p,na.rm=T),
#   n_perfinv_a_p = sum(perfinv_a_p,na.rm=T)
# ),by=MDate]
# 
# 
# 
# ggplot(pos_pf_nclients,aes(x=MDate))+
#   # geom_line(aes(y=N,color="N"))+
#   geom_line(aes(y=n_inv_p,color="N inv (p)"))+
#   geom_line(aes(y=n_inv_a_p,color="N inv (adv, p)"))+
#   geom_line(aes(y=n_perfinv_a_p,color="N perf & inv (adv, p)"))+
#   ylim(0,NA)




# =============================================================================
# Fill NAs
advise_cols <- c("inv","perf","perfinv","inv_p","perf_p","perfinv_p","inv_a_p",
                 "perf_a_p","perfinv_a_p","inv_c_p","advise","advise_a_p","advise_c_p")

for (col in advise_cols) {
  pos_pf[is.na(get(col)),(col):=0]
}


# write_parquet(pos_pf,"../../data/pos_pf.parquet")

# pos_pf <- read_parquet("../../data/pos_pf.parquet")
# 
# pos_pf[,tot_wealth_check := cash_liq + cash_locked + tot_pf]
# 
# ggplot(pos_pf[,lapply(.SD,sum,na.rm=T),by=MDate,.SDcols=c("tot_wealth","tot_wealth_check")][order(MDate)],aes(x=MDate))+
#   geom_line(aes(y=tot_wealth,color="original"))+
#   geom_line(aes(y=tot_wealth_check,color="check"),linetype=2)
# 
# ggplot(pos_pf[,lapply(.SD,sum,na.rm=T),by=MDate,.SDcols=c("cash_liq","cash_locked","deposit_cash_check")][order(MDate)],aes(x=MDate))+
#   geom_line(aes(y=cash_liq+cash_locked,color="original"))+
#   geom_line(aes(y=deposit_cash_check,color="check"),linetype=2)
# 
# ggplot(pos_pf[,lapply(.SD,sum,na.rm=T),by=MDate,.SDcols=c("tot_pf","depotvol_advisor_mandate")][order(MDate)],aes(x=MDate))+
#   geom_line(aes(y=tot_pf,color="original"))+
#   geom_line(aes(y=depotvol_advisor_mandate,color="check"),linetype=2)
# 
# ggplot(pos_pf[,lapply(.SD,sum,na.rm=T),by=MDate,.SDcols=c("hypo","hypo_check")][order(MDate)],aes(x=MDate))+
#   geom_line(aes(y=hypo,color="original"))+
#   geom_line(aes(y=hypo_check,color="check"),linetype=2)
# 
# ggplot(pos_pf[,lapply(.SD,sum,na.rm=T),by=MDate,.SDcols=c("credit_check","hypo_check")][order(MDate)],aes(x=MDate))+
#   geom_line(aes(y=hypo_check,color="hypo"))+
#   geom_line(aes(y=credit_check,color="credit"),linetype=2)

# =============================================================================
# create variables

pos_pf[,pf_share_of_w := tot_pf/tot_wealth]
pos_pf[,eq_share_of_pf := equity/tot_pf]
pos_pf[,eq_share_of_w := equity/tot_wealth]

setorder(pos_pf,Bp_ID,MDate)
pos_pf[,d_deposit := c(0,diff(deposit_cash_check)),by=.(Bp_ID)]

# =============================================================================
# create pre-month and pre-period values

# define the variables that will be controls or references from the PRE-DRAWDOWN MONTH
cols_pre <- c("anlagepaket","anlegerprofil",
            "equity","bond","reales","fund_mixed","deriv","alt",
            "tot_pf","cash_liq","cash_locked","hypo","tot_wealth",
            "chf_net_equity","chf_net_bond","chf_gross_equity","chf_gross_bond",
            "chf_bought","chf_sold","chf_net","d_deposit",
            "deposit_cash_check","credit_check","hypo_check","depotvol_advisor_mandate",
            "adv_team","adv_nclients","adv_segment","adv_rank",
            "pf_share_of_w","eq_share_of_pf","eq_share_of_w")

pre <- pos_pf[,c("Bp_ID","MDate",cols_pre),with=F]
setnames(pre,cols_pre,paste0(cols_pre,"_pre"))


# and define the variables where a period average or sum is better
cols_pre_mean <- c("inv_p","inv_a_p","inv_c_p","perfinv_a_p","tot_wealth","tot_pf","cash_liq","cash_locked",
                   "deposit_cash_check","credit_check","hypo_check","eq_share_of_w","eq_share_of_pf")
cols_pre_sum <- c("dp_tot_pf","dq_tot_pf","dqsum_tot_pf","buy_tot_pf","sell_tot_pf",
                  "chf_net_equity","chf_net_bond","chf_gross_equity","chf_gross_bond",
                  "chf_bought","chf_sold","chf_net","d_deposit")


# client x episode
cle <- CJ(Bp_ID = unique(pos_pf$Bp_ID), ep_id = ep$ep_id, unique = TRUE)
cle <- merge(cle,ep[,.(ep_id,pre_month,pre_start,post_end,dd_start,dd_end)],by="ep_id")

cle <- merge(cle,pre,by.x=c("Bp_ID","pre_month"),by.y=c("Bp_ID","MDate"),all.x=T)


# pre period MEAN
cle[, (paste0(cols_pre_mean,"_pre_mean")) :=
      pos_pf[cle, on = .(Bp_ID, MDate >= pre_start, MDate <= pre_month),
             lapply(.SD,mean,na.rm=T),.SDcols=cols_pre_mean,
             by=.EACHI][,..cols_pre_mean]]
# pre period SUM
cle[, (paste0(cols_pre_sum,"_pre_sum")) :=
      pos_pf[cle, on = .(Bp_ID, MDate >= pre_start, MDate <= pre_month),
             lapply(.SD,sum,na.rm=T),.SDcols=cols_pre_sum,
             by=.EACHI][,..cols_pre_sum]]


# =============================================================================
# create ciris variables and define treatment


# treatment
treat_cols <- c("inv_p","perfinv_p","inv_a_p","perfinv_a_p","inv_c_p")

cle[, (paste0("n_",treat_cols)) := 
      pos_pf[cle, on = .(Bp_ID, MDate >= dd_start, MDate <= dd_end),
             lapply(.SD,sum,na.rm=T),.SDcols=treat_cols,
             by=.EACHI][,..treat_cols]]

cle[,`:=`(
  treat_inv_p = as.numeric(n_inv_p>0),
  treat_perfinv_p = as.numeric(n_perfinv_p>0),
  treat_inv_a_p = as.numeric(n_inv_a_p>0),
  treat_perfinv_a_p = as.numeric(n_perfinv_a_p>0),
  treat_inv_c_p = as.numeric(n_inv_c_p>0)
)]


# outcomes
outcome_cols_sum <- c("dp_equity","dq_equity","dp_bond","dq_bond","dp_tot_pf","dfx_tot_pf","dpfx_tot_pf",             
                       "dq_tot_pf","dqsum_tot_pf","buy_tot_pf","sell_tot_pf",
                      "chf_net_equity","chf_net_bond","chf_gross_equity","chf_gross_bond",
                      "chf_bought","chf_sold","chf_net")
outcome_cols_mean <- c("tot_wealth","tot_pf","equity","bond","reales")


cle[, (paste0(outcome_cols_sum,"_dd")) := 
      pos_pf[cle, on = .(Bp_ID, MDate >= dd_start, MDate <= dd_end),
             lapply(.SD,sum,na.rm=T),.SDcols=outcome_cols_sum,
             by=.EACHI][,..outcome_cols_sum]]

cle[, (paste0(outcome_cols_sum,"_dd_post")) := 
      pos_pf[cle, on = .(Bp_ID, MDate >= dd_start, MDate <= post_end),
             lapply(.SD,sum,na.rm=T),.SDcols=outcome_cols_sum,
             by=.EACHI][,..outcome_cols_sum]]

cle[, (paste0(outcome_cols_mean,"_dd")) := 
      pos_pf[cle, on = .(Bp_ID, MDate >= dd_start, MDate <= dd_end),
             lapply(.SD,sum,na.rm=T),.SDcols=outcome_cols_mean,
             by=.EACHI][,..outcome_cols_mean]]

cle[, (paste0(outcome_cols_mean,"_dd_post")) := 
      pos_pf[cle, on = .(Bp_ID, MDate >= dd_start, MDate <= post_end),
             lapply(.SD,sum,na.rm=T),.SDcols=outcome_cols_mean,
             by=.EACHI][,..outcome_cols_mean]]




# =============================================================================
# client x episode-month
win <- cle[, .(Bp_ID, ep_id, pre_start, post_end)]

stk <- pos_pf[win, on = .(Bp_ID, MDate >= pre_start, MDate <= post_end),
           .(Bp_ID, ep_id, MDate = x.MDate,
             advisor_id,anlagepaket,anlegerprofil,
             equity,bond,reales,fund_mixed,deriv,alt,
             tot_pf,cash_liq,cash_locked,hypo,tot_wealth,d_deposit,
             dp_equity,dq_equity,dp_bond,dq_bond,
             deposit_cash_check,credit_check,hypo_check,
             depotvol_advisor_mandate,adv_team,adv_nclients,adv_segment,adv_rank,
             inv_p,inv_a_p,inv_c_p,perfinv_a_p,
             dp_tot_pf,dfx_tot_pf,dq_tot_pf,dqsum_tot_pf,buy_tot_pf,sell_tot_pf,
             pf_share_of_w,eq_share_of_pf,eq_share_of_w),
           allow.cartesian = TRUE]

stk <- ep[, .(ep_id, dd_start, dd_end, pre_month, post_end)][stk, on = "ep_id"]

stk[, rel_month := mdiff(MDate, dd_start)]
stk[, phase := fcase(MDate <  dd_start, "pre",
                     MDate <= dd_end,   "drawdown",
                     default =          "recovery")]

setorder(stk, Bp_ID, ep_id, MDate)

stk[,`:=`(dp_tot_pf_c = cumsum(dp_tot_pf),
          dfx_tot_pf_c =cumsum(dfx_tot_pf),
          dq_tot_pf_c   = cumsum(dq_tot_pf),
          dp_eq_c = cumsum(dp_equity),
          dq_eq_c = cumsum(dq_equity),
          dp_bd_c = cumsum(dp_bond),
          dq_bd_c = cumsum(dq_bond)),by=.(Bp_ID,ep_id)]

## carry the client x episode constants that every spec needs
stk <- cle[, .(Bp_ID, ep_id, 
               treat_inv_p,treat_perfinv_p,treat_inv_a_p,treat_perfinv_a_p,treat_inv_c_p,
               tot_pf_pre,tot_wealth_pre,tot_pf_pre_mean,tot_wealth_pre_mean,
               cash_liq_pre,cash_liq_pre_mean,cash_locked_pre,cash_locked_pre_mean,
               equity_pre,bond_pre,d_deposit_pre,d_deposit_pre_sum,
               deposit_cash_check_pre,credit_check_pre,hypo_check_pre,hypo_pre,
               deposit_cash_check_pre_mean,credit_check_pre_mean,hypo_check_pre_mean,
               anlagepaket_pre,inv_p_pre_mean,inv_a_p_pre_mean,inv_c_p_pre_mean,
               adv_team_pre,adv_nclients_pre,adv_segment_pre,adv_rank_pre,
               eq_share_of_w_pre,eq_share_of_pf_pre,eq_share_of_w_pre_mean,eq_share_of_pf_pre_mean)][stk, on = .(Bp_ID, ep_id)]



write_parquet(cle,"../../data/cle.parquet")
write_parquet(stk,"../../data/stk2.parquet")
write_parquet(pos_pf,"../../data/pos_pf.parquet")
