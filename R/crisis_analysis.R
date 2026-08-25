### 

library(data.table)
library(arrow)
library(lubridate)
library(ggplot2)
library(fixest)

rm(list=ls());gc()


out_fig_path <- "C:/Users/FWehmann/Dropbox/Apps/Overleaf/Uncertainty and Financial Advise (1)/figures"




source("smi_sp500.R")

# smi & sp500 loaded from previous script (smi_sp500.R) including dd_periods
dd_periods <- copy(sel_rect)
smi_sp <- copy(dtm)
rm(dtm,sel_rect)
rm(idx_l,idx,lines_l,lines_sel,smi,sp500,dd_rect,dd_smi,dd_sp500,dd_top5,cols)

id_col <- "Bp_ID"

smp_start <- as.Date("2011-01-01")


instr_sel <- c("Aktien","Fonds","Obligationen","Strukturierte Prod./Zertifikate","Optionen")
instr_sel <- c("Aktien","Fonds")


# ==============================================================================
# data
# ==============================================================================

pos_file <- "../data/pos_b_merged.parquet"

# col selection
pos_cols <- unique(c(id_col, "Bp_ID", "Cont_ID", "Asset_ID", "MDate",
                     "advised","K_Aufnahme", "Instrumentengruppe", "Vermoegen_CHF",
                     "Geschaeftsvolumen_CHF", "DA_Titelkursabweichung_CHF","DA_Gesamtabweichung_CHF","DA_Mengenabweichung_CHF",
                     "last_contact","Anlagepaket","Kontoprodukt","Depotprodukt"))

pos_ds <- arrow::open_dataset(pos_file)

miss <- setdiff(pos_cols, names(pos_ds))
if (length(miss)) stop("not in ", pos_file, ": ", paste(miss, collapse = ", "))

# the date cut can only be pushed down if MDate is stored as a date/timestamp;
# if it sits in the file as a string, it is filtered after the read instead
mdate_type <- pos_ds$schema$GetFieldByName("MDate")$type$ToString()
push_date <- grepl("date|timestamp", mdate_type, ignore.case = TRUE) &&
  requireNamespace("dplyr", quietly = TRUE)

pos <- if (push_date) {
  # arrow's dplyr backend turns this into a projected + filtered scan; nothing
  # but the surviving rows of the selected columns is ever built in memory
  dplyr::collect(dplyr::filter(dplyr::select(pos_ds, dplyr::all_of(pos_cols)),
                               MDate >= smp_start))
} else {
  # no pushdown here, but the wide columns are still never read
  arrow::read_parquet(pos_file, col_select = tidyselect::all_of(pos_cols))
}
setDT(pos)
rm(pos_ds); gc()

pos[, MDate := as.Date(MDate)]
pos <- pos[MDate >= smp_start]   # no-op when the cut was already pushed down

# a handful of distinct values over millions of rows - as a factor this costs an
# integer per row instead of a string pointer. `%in% instr_sel` is unaffected
if (is.character(pos$Instrumentengruppe)) {
  pos[, Instrumentengruppe := factor(Instrumentengruppe)]
}

 # add numeric advised by
pos[,advised_init_client := ifelse(is.na(K_Aufnahme),0,ifelse(K_Aufnahme=="Durch Kunde",1,0))]
pos[,advised_init_advisor := ifelse(is.na(K_Aufnahme),0,ifelse(K_Aufnahme=="Durch Kundenberater",1,0))]

gc()

pos[,MDate := lubridate::ceiling_date(MDate,"months")-1]

pos_wealth <- pos[,.(wealth = sum(Vermoegen_CHF,na.rm=T)),by=c("MDate","Bp_ID")]

## filter Instrumentengruppe
pos <- pos[Instrumentengruppe %in% instr_sel]

pos <- merge(pos,pos_wealth,by=c("MDate","Bp_ID"))

## ============================================================================
## Aggregate on Person x month
pos_p <- pos[,.(
  n_trades_adv = sum(advised,na.rm=T),
  n_trades_adv_init_c = sum(advised_init_client,na.rm=T),
  n_trades_adv_init_a = sum(advised_init_advisor,na.rm=T),
  n_trades = .N,
  # n_assets = uniqueN(Asset_ID),
  vol = sum(Geschaeftsvolumen_CHF,na.rm=T),
  dprice = sum(DA_Titelkursabweichung_CHF,na.rm=T),
  dtotal = sum(DA_Gesamtabweichung_CHF,na.rm=T),
  buysell = sum(DA_Mengenabweichung_CHF,na.rm=T),
  Anlagepaket = last(Anlagepaket),
  Kontoprodukt = last(Kontoprodukt),
  Depotprodukt = last(Depotprodukt),
  last_contact = last(last_contact),
  wealth = last(wealth)
),by=c("MDate","Bp_ID")]
# ),by=c("MDate")]
n_ass <- unique(pos, by = c("MDate","Bp_ID","Asset_ID"))[
  , .(n_assets = .N), by = .(MDate, Bp_ID)]
pos_p[n_ass, n_assets := i.n_assets, on = .(MDate, Bp_ID)]
gc()
setorder(pos_p,MDate,Bp_ID)

## ============================================================================
## compute return
pos_p[,ret := dprice / (vol - dtotal)]  # change in price - volumn previous period (volumn today - total change)

arrow::write_parquet(pos_p,"../data/pos_p.parquet")

# ggplot(pos_p,aes(x=MDate,y=cumprod(1+ret)))+geom_line()

## ============================================================================
## Aggregate on Asset x month
# pos_a <- pos[,.(
#   n_trades_adv = sum(advised,na.rm=T),
#   n_trades_adv_init_c = sum(advised_init_client,na.rm=T),
#   n_trades_adv_init_a = sum(advised_init_advisor,na.rm=T),
#   vol = sum(Geschaeftsvolumen_CHF,na.rm=T),
#   dprice = sum(DA_Titelkursabweichung_CHF,na.rm=T),
#   dtotal = sum(DA_Gesamtabweichung_CHF,na.rm=T)
# ),by=c("MDate","Asset_ID")]
# gc()
# setorder(pos_a,MDate,Asset_ID)

# pos_a[,ret := dprice / (vol - dtotal)]


## ============================================================================

## ============================================================================
## DRAWDOWN PERIODS
## ============================================================================

# Covid (as test case) -> call "c1" for crisis1

dd_c1_min <- "2020-02-01"
dd_c1_max <- "2020-04-01"

pre_window <- "2018-01-01"
post_window <- "2022-01-01"

c1_rect <- data.table(xmin = as.Date(dd_c1_min), xmax = as.Date(dd_c1_max))

## ============================================================================
## PERSONS with ADVISED TRADE during COVID
# check if which persons traded on advise during crisis
c1_p <- pos_p[MDate %between% c(dd_c1_min,dd_c1_max)]
c1_p <- c1_p[,.(
  n_trades_adv = sum(n_trades_adv),
  n_trades_adv_init_c = sum(n_trades_adv_init_c),
  n_trades_adv_init_a = sum(n_trades_adv_init_a)
),by=Bp_ID]

c1_p_id_adv <- c1_p[n_trades_adv >0]$Bp_ID
c1_p_id_no <- c1_p[n_trades_adv == 0]$Bp_ID


pos_c1_adv <- pos_p[MDate %between% c(pre_window,post_window)& Bp_ID %in% c1_p_id_adv,.(
  n_trades_adv = sum(n_trades_adv),
  n_trades_adv_init_c = sum(n_trades_adv_init_c),
  n_trades_adv_init_a = sum(n_trades_adv_init_a),
  vol = sum(vol),
  dprice = sum(dprice),
  dtotal = sum(dtotal)
),by="MDate"]
pos_c1_adv[,ret := dprice / (vol - dtotal)]
pos_c1_no <- pos_p[MDate %between% c(pre_window,post_window)& Bp_ID %in% c1_p_id_no,.(
  n_trades_adv = sum(n_trades_adv),
  n_trades_adv_init_c = sum(n_trades_adv_init_c),
  n_trades_adv_init_a = sum(n_trades_adv_init_a),
  vol = sum(vol),
  dprice = sum(dprice),
  dtotal = sum(dtotal)
),by="MDate"]
pos_c1_no[,ret := dprice / (vol - dtotal)]

base_date <- "2019-12-31"
pos_c1_adv[,idx := cumprod(1+ret)]
pos_c1_adv[,idx := idx / idx[MDate==base_date]]
pos_c1_no[,idx := cumprod(1+ret)]
pos_c1_no[,idx := idx/ idx[MDate==base_date]]
ggplot()+
  geom_rect(data=c1_rect,aes(xmin=xmin,xmax=xmax,ymin=-Inf,ymax=Inf),
            fill="grey60",alpha=0.35)+
  geom_line(data=pos_c1_adv,aes(x=MDate,y=idx,color="Advised Clients (during DD)"),size=1)+
  geom_line(data=pos_c1_no, aes(x=MDate,y=idx,color="Not-Advised Clients"),size=1)+
  labs(title="Portfolio-Performance of advised vs. non-advised clients (during Covid-drawdown)",
       x=NULL,y=NULL)+
  guides(color=guide_legend(title=NULL))+
  theme_light()+
  theme(legend.position="bottom")


## ============================================================================
## PERSONS with ADVISOR CONTACT during COVID
## person ids that had a least one invest contact during covid
source("load_contacts_during_crisis.R")
bp_contacts_inv_covid

pos_c1_contact <- pos_p[MDate %between% c(pre_window,post_window)& Bp_ID %in% bp_contacts_inv_covid,.(
  n_trades_adv = sum(n_trades_adv),
  n_trades_adv_init_c = sum(n_trades_adv_init_c),
  n_trades_adv_init_a = sum(n_trades_adv_init_a),
  vol = sum(vol),
  dprice = sum(dprice),
  dtotal = sum(dtotal)
),by="MDate"]
pos_c1_contact[,ret := dprice / (vol - dtotal)]
pos_c1_nocontact <- pos_p[MDate %between% c(pre_window,post_window)& !(Bp_ID %in% bp_contacts_inv_covid),.(
  n_trades_adv = sum(n_trades_adv),
  n_trades_adv_init_c = sum(n_trades_adv_init_c),
  n_trades_adv_init_a = sum(n_trades_adv_init_a),
  vol = sum(vol),
  dprice = sum(dprice),
  dtotal = sum(dtotal)
),by="MDate"]
pos_c1_nocontact[,ret := dprice / (vol - dtotal)]

pos_c1_contact[,idx_c := cumprod(1+ret)]
pos_c1_contact[,idx_c := idx_c / idx_c[MDate==base_date]]
pos_c1_nocontact[,idx_nc := cumprod(1+ret)]
pos_c1_nocontact[,idx_nc := idx_nc/ idx_nc[MDate==base_date]]
ggplot()+
  geom_rect(data=c1_rect,aes(xmin=xmin,xmax=xmax,ymin=-Inf,ymax=Inf),
            fill="grey60",alpha=0.35)+
  geom_line(data=pos_c1_contact,aes(x=MDate,y=idx_c,color="Advised Clients (during DD)"),size=1)+
  geom_line(data=pos_c1_nocontact, aes(x=MDate,y=idx_nc,color="Not-Advised Clients"),size=1)+
  labs(title="Portfolio-Performance of advised vs. non-advised clients (during Covid-drawdown)",
       x=NULL,y=NULL)+
  guides(color=guide_legend(title=NULL))+
  theme_light()+
  theme(legend.position="bottom")


pos_c1_contact_merge <- merge(pos_c1_contact[,c("MDate","idx_c")],pos_c1_nocontact[,c("MDate","idx_nc")],by="MDate")
ggplot()+
  geom_rect(data=c1_rect,aes(xmin=xmin,xmax=xmax,ymin=-Inf,ymax=Inf),
            fill="grey60",alpha=0.35)+
  geom_hline(yintercept=0)+
  geom_line(data=pos_c1_contact_merge,aes(x=MDate,y=idx_c-idx_nc,color="Diff Advised vs. Non-Advised"),size=1)+
  labs(title="Effect of Advise Contacts during Crisis",
       x=NULL,y=NULL)+
  guides(color=guide_legend(title=NULL))+
  theme_light()+
  theme(legend.position="bottom")

## ============================================================================
## ASSETS that werer TRADED on ADVISE during COVID
# check if which persons traded on advise during crisis
# c1_a <- pos_a[MDate %between% c(dd_c1_min,dd_c1_max)]
# c1_a <- c1_a[,.(
#   n_trades_adv = sum(n_trades_adv),
#   n_trades_adv_init_c = sum(n_trades_adv_init_c),
#   n_trades_adv_init_a = sum(n_trades_adv_init_a)
# ),by=Asset_ID]
# 
# c1_a_id_adv <- c1_a[n_trades_adv >0]$Asset_ID
# c1_a_id_no <- c1_a[n_trades_adv == 0]$Asset_ID
# 
# 
# 
# pos_a_c1_adv <- pos_a[MDate %between% c(pre_window,post_window)& Asset_ID %in% c1_a_id_adv,.(
#   n_trades_adv = sum(n_trades_adv),
#   n_trades_adv_init_c = sum(n_trades_adv_init_c),
#   n_trades_adv_init_a = sum(n_trades_adv_init_a),
#   vol = sum(vol),
#   dprice = sum(dprice),
#   dtotal = sum(dtotal)
# ),by="MDate"]
# pos_a_c1_adv[,ret := dprice / (vol - dtotal)]
# pos_a_c1_no <- pos_a[MDate %between% c(pre_window,post_window)& Asset_ID %in% c1_a_id_no,.(
#   n_trades_adv = sum(n_trades_adv),
#   n_trades_adv_init_c = sum(n_trades_adv_init_c),
#   n_trades_adv_init_a = sum(n_trades_adv_init_a),
#   vol = sum(vol),
#   dprice = sum(dprice),
#   dtotal = sum(dtotal)
# ),by="MDate"]
# pos_a_c1_no[,ret := dprice / (vol - dtotal)]
# 
# pos_a_c1_adv[,idx := cumprod(1+ret)]
# pos_a_c1_adv[,idx := idx / idx[MDate==base_date]]
# pos_a_c1_no[,idx := cumprod(1+ret)]
# pos_a_c1_no[,idx := idx/ idx[MDate==base_date]]
# 
# ggplot()+
#   geom_rect(data=c1_rect,aes(xmin=xmin,xmax=xmax,ymin=-Inf,ymax=Inf),
#             fill="grey60",alpha=0.35)+
#   geom_line(data=pos_a_c1_adv,aes(x=MDate,y=idx,color="advised"))+
#   geom_line(data=pos_a_c1_no, aes(x=MDate,y=idx,color="no advise"))+
#   theme_light()



#### ==========================================================================

covid_dd_start <- "2020-02-19"
covid_dd_end <- "2020-03-23"

dd_c1_min <- "2020-02-01"
dd_c1_max <- "2020-04-01"

pre_window <- "2018-01-01"
post_window <- "2022-01-01"

pos_p[,inv_contact_crisis := 0]
pos_p[Bp_ID %in% bp_contacts_inv_covid, inv_contact_crisis := 1]

pos_p[,inv_contact_init_a_crisis := 0]
pos_p[Bp_ID %in% bp_contacts_init_a_inv_covid, inv_contact_init_a_crisis := 1]

buysell_covid <- pos_p[MDate >= dd_c1_min & MDate < dd_c1_max, .(
  buysell = sum(buysell,na.rm=T),
  inv_contact_crisis = sum(inv_contact_crisis,na.rm=T),
  Anlagepaket = last(Anlagepaket),
  Kontoprodukt = last(Kontoprodukt),
  Depotprodukt = last(Depotprodukt),
  wealth = last(wealth)
  ),by="Bp_ID"]




## ===========================================================================
## Depotprodukt

depotprodukt <- buysell_covid[, .N, by = .(Depotprodukt, contact = inv_contact_crisis > 0)]
depotprodukt[, share := N / sum(N), by = contact]

# fill missing combinations — products that occur in only one of the two groups
depotprodukt <- depotprodukt[CJ(Depotprodukt = unique(Depotprodukt), contact = c(FALSE, TRUE), unique = TRUE),
       on = .(Depotprodukt, contact)]
depotprodukt[is.na(N), `:=`(N = 0L, share = 0)]

depotprodukt[, contact := factor(contact, c(FALSE, TRUE),
                      c("No contact", "Contact during crisis"))]
setorder(depotprodukt, -share)
depotprodukt[, Depotprodukt := factor(Depotprodukt, levels = unique(Depotprodukt))]

ggplot(depotprodukt, aes(x = share, y = Depotprodukt, fill = contact)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_x_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("No contact" = "grey65",
                               "Contact during crisis" = "steelblue4")) +
  labs(title = "Custody account product mix, by advisory contact during the Covid crisis",
       subtitle = "Shares within each group; bars sum to 100% per colour",
       x = NULL, y = NULL, fill = NULL) +
  theme_light() +
  theme(legend.position = "bottom", panel.grid.major.y = element_blank())



## ===========================================================================
## Anlagepaket

anlagepaket <- buysell_covid[, .N, by = .(Anlagepaket, contact = inv_contact_crisis > 0)]
anlagepaket[, share := N / sum(N), by = contact]

# fill missing combinations — CONSULT expert has no contact==FALSE row
anlagepaket <- anlagepaket[CJ(Anlagepaket = unique(Anlagepaket), contact = c(FALSE, TRUE), unique = TRUE),
       on = .(Anlagepaket, contact)]
anlagepaket[is.na(N), `:=`(N = 0L, share = 0)]

anlagepaket[, contact := factor(contact, c(FALSE, TRUE),
                      c("No contact", "Contact during crisis"))]
setorder(anlagepaket, -share)
anlagepaket[, Anlagepaket := factor(Anlagepaket, levels = unique(Anlagepaket))]

ggplot(anlagepaket, aes(x = share, y = Anlagepaket, fill = contact)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_x_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("No contact" = "grey65",
                               "Contact during crisis" = "steelblue4")) +
  labs(title = "Investment package mix, by advisory contact during the Covid crisis",
       subtitle = "Shares within each group; bars sum to 100% per colour",
       x = NULL, y = NULL, fill = NULL) +
  theme_light() +
  theme(legend.position = "bottom", panel.grid.major.y = element_blank())


# ===========================================================================
# Regressions

library(fixest)

spec <- buysell ~ inv_contact_crisis + wealth
reg1 <- feols(buysell_covid,spec)
etable(reg1)

## => shows that advise reduces selling probability


## compute post return
## and then run: postreturn ~ advice_during_crisis + wealth

## run VVM vs. Beratungsmandat etc.

## include number of traded assets per person (as control)

pos_p


post_covid <- pos_p[MDate >= dd_c1_max & MDate < post_window, .(
  vol_post = sum(vol),
  dprice_post = sum(dprice),
  dtotal_post = sum(dtotal),
  n_trades_post = sum(n_trades),
  n_assets_post = sum(n_assets),
  buysell_post = sum(buysell,na.rm=T),
  wealth_post = last(wealth)
),by="Bp_ID"]

pre_covid <- pos_p[MDate < dd_c1_min & MDate >= pre_window, .(
  vol_pre = sum(vol),
  dprice_pre = sum(dprice),
  dtotal_pre = sum(dtotal),
  n_trades_pre = sum(n_trades),
  n_assets_pre = sum(n_assets),
  buysell_pre = sum(buysell,na.rm=T),
  wealth_pre = first(wealth)
),by="Bp_ID"]

covid <- pos_p[MDate >= dd_c1_min & MDate < dd_c1_max, .(
  vol_crisis = sum(vol),
  dprice_crisis = sum(dprice),
  dtotal_crisis = sum(dtotal),
  n_trades_crisis = sum(n_trades),
  n_trades_adv_crisis = sum(n_trades_adv),
  n_trades_init_c_adv_crisis = sum(n_trades_adv_init_c),
  n_trades_init_a_adv_crisis = sum(n_trades_adv_init_a),
  n_assets_crisis = sum(n_assets),
  buysell_crisis = sum(buysell,na.rm=T),
  inv_contact_crisis = sum(inv_contact_crisis,na.rm=T),
  inv_contact_init_a_crisis = sum(inv_contact_init_a_crisis,na.rm=T),
  Anlagepaket = last(Anlagepaket),
  Kontoprodukt = last(Kontoprodukt),
  Depotprodukt = last(Depotprodukt),
  wealth_crisis = first(wealth)
),by="Bp_ID"]

covid_m <- merge(covid,post_covid,by="Bp_ID")
covid_m <- merge(covid_m,pre_covid,by="Bp_ID")
covid_m_post <- merge(covid,post_covid,by="Bp_ID")
covid_m_pre <- merge(covid,pre_covid,by="Bp_ID")

covid_m[,post_return := dprice_post/(vol_post - dtotal_post)]
covid_m[,crisis_return := dprice_crisis/(vol_crisis - dtotal_crisis)]

covid_m[,adv_contact_crisis := ifelse(inv_contact_crisis>0,T,F)]
covid_m[,adv_trade_crisis := ifelse(n_trades_adv_crisis>0,T,F)]

covid_m[,pre_return := dprice_pre/(vol_pre - dtotal_pre)]
covid_m[,crisis_return := dprice_crisis/(vol_crisis - dtotal_crisis)]



# -----------------------------------
# Regression Specifications
spec1_post <- post_return ~ inv_contact_crisis 
spec1_pre <- pre_return ~ inv_contact_crisis 
spec2_post <- post_return ~ inv_contact_crisis + log(wealth_crisis)
spec2_pre <- pre_return ~ inv_contact_crisis + log(wealth_crisis)
spec2.1_post <- post_return ~ inv_contact_crisis + log(wealth_pre)
spec2.1_pre <- pre_return ~ inv_contact_crisis + log(wealth_pre)

spec1_post <- post_return ~ inv_contact_init_a_crisis 
spec1_pre <- pre_return ~ inv_contact_init_a_crisis 
spec2_post <- post_return ~ inv_contact_init_a_crisis + log(wealth_crisis)
spec2_pre <- pre_return ~ inv_contact_init_a_crisis + log(wealth_crisis)
spec2.1_post <- post_return ~ inv_contact_init_a_crisis + log(wealth_pre)
spec2.1_pre <- pre_return ~ inv_contact_init_a_crisis + log(wealth_pre)

regs <- list()
regs[["1_post"]] <- feols(spec1_post,covid_m)
regs[["1_pre"]] <- feols(spec1_pre,covid_m)
regs[["2_post"]] <- feols(spec2_post,covid_m)
regs[["2_pre"]] <- feols(spec2_pre,covid_m)
regs[["2.1_post"]] <- feols(spec2.1_post,covid_m)
regs[["2.1_pre"]] <- feols(spec2.1_pre,covid_m)

etable(regs)


## post


spec1 <- post_return ~ inv_contact_crisis #+ log(wealth_crisis)
spec1.1 <- post_return ~ inv_contact_crisis + log(wealth_crisis) + Depotprodukt + Anlagepaket
spec1b <- post_return ~ adv_contact_crisis + log(wealth_crisis)
spec1b.1 <- post_return ~ inv_contact_crisis + log(wealth_crisis) + Depotprodukt + Anlagepaket
spec1c <- post_return ~ inv_contact_crisis + log(wealth_pre)
# spec1b <- post_return ~ adv_contact_crisis + log(wealth_crisis)
# spec2 <- post_return ~ n_trades_adv_crisis + log(wealth_crisis) + Depotprodukt + Anlagepaket
# spec2b <- post_return ~ adv_trade_crisis + log(wealth_crisis) + Depotprodukt + Anlagepaket
spec3 <- post_return ~ log(buysell_crisis) + log(wealth_crisis) + Depotprodukt + Anlagepaket

regs <- list()


# reg2 <- feols(spec2,covid_m)
# reg2b <- feols(spec2b,covid_m)
# reg3 <- feols(spec3,covid_m)




# ---------------------------------------------------------------------
## pre (check parallel trends)


spec1_pre <- pre_return ~ inv_contact_crisis #+ log(wealth_crisis)
spec1.1_pre <- pre_return ~ inv_contact_crisis + log(wealth_crisis) + Depotprodukt + Anlagepaket
spec1b_pre <- pre_return ~ adv_contact_crisis + log(wealth_crisis)
spec1b.1_pre <- pre_return ~ adv_contact_crisis + log(wealth_crisis) + Depotprodukt + Anlagepaket
spec1c_pre <- pre_return ~ inv_contact_crisis + log(wealth_pre)
spec1.1c_pre <- pre_return ~ inv_contact_crisis + log(wealth_pre) + Depotprodukt + Anlagepaket
spec1d_pre <- pre_return ~ adv_contact_crisis + log(wealth_pre)
spec1.1d_pre <- pre_return ~ adv_contact_crisis + log(wealth_pre) + Depotprodukt + Anlagepaket
# spec1b <- post_return ~ adv_contact_crisis + log(wealth_crisis)
# spec2 <- post_return ~ n_trades_adv_crisis + log(wealth_crisis) + Depotprodukt + Anlagepaket
# spec2b <- post_return ~ adv_trade_crisis + log(wealth_crisis) + Depotprodukt + Anlagepaket
spec3_pre <- post_return ~ log(buysell_crisis) + log(wealth_crisis) + Depotprodukt + Anlagepaket

# reg1_pre <- feols(spec1_pre,covid_m)
# reg1b_pre <- feols(spec1b_pre,covid_m)
# reg2_pre <- feols(spec2_pre,covid_m)
# reg2b_pre <- feols(spec2b_pre,covid_m)
# reg3_pre <- feols(spec3_pre,covid_m)

# etable(list(reg1_pre,reg1b_pre,reg2_pre,reg2b_pre,reg3_pre))


# etable(list(reg1_pre,reg1,reg1b_pre,reg1b))

regs <- list()
# regs[["post 1"]] <- feols(spec1,covid_m)
# regs[["pre 1"]] <- feols(spec1_pre,covid_m)
# regs[["post 1.1"]] <- feols(spec1.1,covid_m)
# regs[["pre 1.1"]] <- feols(spec1.1_pre,covid_m)
# regs[["post 1b"]] <- feols(spec1b,covid_m)
# regs[["pre 1b"]] <- feols(spec1b_pre,covid_m)
# regs[["post 1b.1"]] <- feols(spec1b.1,covid_m)
# regs[["pre 1b.1"]] <- feols(spec1b.1_pre,covid_m)
# regs[["post 1c"]] <- feols(spec1c,covid_m)
regs[["pre 1c"]] <- feols(spec1c_pre,covid_m)
regs[["pre 1.1c"]] <- feols(spec1.1c_pre,covid_m)
regs[["pre 1d"]] <- feols(spec1d_pre,covid_m)
regs[["pre 1.1d"]] <- feols(spec1.1d_pre,covid_m)

etable(regs)

reg1 <- feols(spec1,covid_m)
reg1_pre <- feols(spec1_pre,covid_m)

etable(reg1,reg1_pre)
