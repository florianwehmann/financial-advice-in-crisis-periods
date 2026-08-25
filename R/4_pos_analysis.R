## POS ANALYSIS
library(arrow)
library(data.table)
library(fixest)
library(ggplot2)
library(zoo)

rm(list=ls());gc()

# load pos_m file
pos_m <- read_parquet("../data/pos_m.parquet")
# pos_m <- pos_m[MDate >= "2011-01-01"]

pos_a <- read_parquet("../data/pos_a.parquet")
pos_a <- pos_a[MDate >= "2011-01-01"]

# ====================================================================
## some variable analysis

# Proft variables (bank profit per client):
am <- pos_m[,sum(Profit_Wertschriften,na.rm=T),by=c("MDate")]
am <- pos_m[,sum(Profit_Fonds,na.rm=T),by=c("MDate")]
ggplot(am,aes(x=MDate,y=V1))+geom_line()
am <- pos_m[,sum(Profit_Depot,na.rm=T),by=.(yearqtr = as.yearqtr(MDate))]
am <- pos_m[,sum(Profit_Vermoegensverwaltung,na.rm=T),by=.(yearqtr = as.yearqtr(MDate))]
ggplot(am,aes(x=yearqtr,y=V1))+geom_line()


# ---------------------------------------------------

pos_m[MDate=="2024-12-31" & main_bank == "Ja",.N,by=Wohnland]

# select only main bank clients
pos_ms <- pos_m[main_bank == "Ja"]


names(pos_ms)
setorder(pos_ms,Bp_ID,MDate)


pos_ms_a <- pos_ms[,.(v=sum(vol),dprice=sum(dprice),dtotal=sum(dtotal)),by=.(MDate)]
setorder(pos_ms_a,MDate)
pos_ms_a[,ret := dprice / (v-dtotal)]
pos_ms_a[,idx := cumprod(1+ret)-1,by=]


ggplot(pos_ms_a[MDate %between% c("2021-01-01","2021-12-31")],aes(x=MDate,y=idx))+geom_line()

# ====================================================================


dd_c1_min <- ceiling_date(as.Date("2020-02-28"),"month")-1
dd_c1_max <- ceiling_date(as.Date("2020-04-01"),"month")-1

# -------------------------

# dd_c1_min <- ceiling_date(as.Date("2018-09-01"),"month")-1
# dd_c1_max <- ceiling_date(as.Date("2018-12-01"),"month")-1

# -------------------------

pre_window <- rollforward(as.Date(dd_c1_min) %m+% months(-12))
post_window <- rollforward(as.Date(dd_c1_max) %m+% months(12))





# ====================================================================
## treated clients: at least one performance-advised trade during crisis 1

# Bp_IDs with >= 1 advised-performance trade in the crisis window
bp_adv_c1 <- pos_ms[MDate >= dd_c1_min & MDate <= dd_c1_max &
                           n_trades_adv_perf > 0 & n_trades_adv_init_a>0, unique(Bp_ID)]
bp_adv_c1 <- pos_ms[MDate >= dd_c1_min & MDate <= dd_c1_max &
                           n_trades_adv_inv > 0, unique(Bp_ID)]
bp_adv_c1 <- pos_ms[MDate >= dd_c1_min & MDate <= dd_c1_max &
                           n_trades_adv_init_a > 0, unique(Bp_ID)]

length(bp_adv_c1)

# flag them in the panel
pos_ms[, adv_perf_c1 := as.factor(ifelse(Bp_ID %in% bp_adv_c1,"advised","not advised"))]

# months covered by the window (sanity check: MDate is month-end)
pos_ms[MDate >= dd_c1_min & MDate <= dd_c1_max, .N, by = MDate]


pos_ms_c1 <- pos_ms[MDate>=pre_window&MDate<post_window,.(
  vol = mean(vol,na.rm=T),
  dprice = mean(dprice,na.rm=T),
  dtotal = mean(dtotal,na.rm=T),
  buysell =mean(buysell,na.rm=T),
  sell = mean(sell,na.rm=T),
  buy = mean(buy,na.rm=T)
),by=c("MDate","adv_perf_c1")]

pos_ms_c1[,return := dprice / (vol-dtotal)]

setorder(pos_ms_c1,adv_perf_c1,MDate)

pos_ms_c1[,adv_perf_c1 := as.factor(adv_perf_c1)]
pos_ms_c1[,idx := cumprod(1+return),by=adv_perf_c1]
pos_ms_c1[,idx := -sell,by=adv_perf_c1]
based <- rollforward(as.Date(dd_c1_min) %m+% months(-6))
pos_ms_c1[,idx := idx / idx[MDate == based],by=adv_perf_c1]


pos_ms[MDate >= dd_c1_min & MDate <= dd_c1_max, mean(sell,na.rm=T),by=adv_perf_c1]


ggplot(pos_ms_c1,aes(x=MDate))+
  geom_line(aes(y=idx,color=adv_perf_c1))



# ==========================================================
asset_adv_c1 <- pos_a[MDate >= dd_c1_min & MDate <= dd_c1_max &
                        n_trades_adv_init_a > 0, unique(Asset_ID)]

length(asset_adv_c1)


pos_a[, adv_perf_c1 := as.factor(ifelse(Asset_ID %in% asset_adv_c1,"advised","not advised"))]


pos_a_c1 <- pos_a[MDate>=pre_window&MDate<post_window,.(
  vol = mean(vol,na.rm=T),
  dprice = mean(dprice,na.rm=T),
  dtotal = mean(dtotal,na.rm=T),
  buysell =sum(buysell,na.rm=T),
  sell = mean(sell,na.rm=T),
  buy = mean(buy,na.rm=T)
),by=c("MDate","adv_perf_c1")]

pos_a_c1[,return := dprice / (vol-dtotal)]

setorder(pos_a_c1,adv_perf_c1,MDate)


based <- rollforward(as.Date(dd_c1_min) %m+% months(-3))

pos_a_c1[,adv_perf_c1 := as.factor(adv_perf_c1)]
pos_a_c1[,idx := cumprod(1+return),by=adv_perf_c1]
# pos_a_c1[,idx := buysell,by=adv_perf_c1]
pos_a_c1[,idx := idx / idx[MDate == based],by=adv_perf_c1]


ggplot(pos_a_c1,aes(x=MDate))+
  geom_line(aes(y=idx,color=adv_perf_c1))



## ==========================================================================


pos_m[,`:=`(
  adv_inv = ifelse(n_trades_adv_inv > 0, T, F),
  adv_perf = ifelse(n_trades_adv_perf > 0, T, F),
  adv_inv_perf = ifelse(n_trades_adv_inv_perf > 0, T, F),
  adv_inv_init_a = ifelse(n_trades_adv_init_a > 0 & n_trades_adv_inv > 0, T, F),
  adv_perf_init_a = ifelse(n_trades_adv_init_a > 0 & n_trades_adv_perf > 0, T, F),
  adv_inv_perf_init_a = ifelse(n_trades_adv_init_a > 0 & n_trades_adv_inv_perf > 0, T, F)
  )]


## from smi_sp500.R
sel_rect[,xmin := ceiling_date(xmin,"months")-1]
sel_rect[,xmax := ceiling_date(xmax,"months")-1]

## DRAWDOWN Dummy
pos_m[, dd_dummy := MDate %inrange% sel_rect[, .(xmin, xmax)]]


## compute returns
setorder(pos_m,Bp_ID,MDate)

pos_m[,pf_ret := dprice / (vol-dtotal),by=Bp_ID]
pos_m[,pf_idx := cumprod(1+pf_ret),by=Bp_ID]
pos_m[,pf_idx := pf_idx / pf_idx[1],by=Bp_ID]


# -----------------------------------


spec <- pf_ret ~ dd_dummy*adv_perf + dd_dummy + adv_perf + EVV + log(wealth) + Nationalitaet + Wohnland + Zivilstand | Bp_ID + MDate

reg <- feols(spec,pos_m)

etable(reg)




# ----------------------------------

library(data.table)
setDT(pos_m)

episodes <- data.table(
  ep       = c("euro2011","q42018","covid2020","y2022"),
  dd_start = as.yearmon(c("May 2011","Oct 2018","Feb 2020","Jan 2022")),
  dd_end   = as.yearmon(c("Sep 2011","Dec 2018","Mar 2020","Oct 2022"))
)
episodes[, `:=`(pre_start  = dd_start - 12/12,
                post_end   = dd_end   + 12/12)]

stk <- episodes[, {
  w <- pos_m[MDate >= pre_start & MDate <= post_end]
  w[, `:=`(ep        = ep,
           rel_month = round((MDate - dd_start) * 12),
           treat     = as.integer(Bp_ID %in% contacted_in_episode(ep)))]
}, by = ep]

stk[, ci := paste(Bp_ID, ep)]          # client x episode
stk[, te := paste(MDate, ep)]          # calendar month x episode



pos_agg <- pos_m[,.(
  dprice = mean(dprice,na.rm=T),
  dtotal = mean(dtotal,na.rm=T),
  vol = mean(vol,na.rm=T)
),by=MDate]
setorder(pos_agg,MDate)
pos_agg[,ret := dprice/(vol-dtotal)]
pos_agg[,idx := cumprod(1+ret)]

ggplot(pos_agg,aes(x=MDate,y=idx))+geom_line()


