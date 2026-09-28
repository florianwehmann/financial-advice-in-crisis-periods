# load packages
library(data.table)
library(haven)
library(arrow)
library(lubridate)
library(dplyr)
library(duckdb)
library(ggplot2)


rm(list=ls()); gc()

emp_dir_raw <- "C:/Users/FWehmann/Dropbox/Wehmann Household Finance Project/RawData_Stata/"

source("../fx_converter.R")


winsor <- function(x, p = 0.99) {
  q <- quantile(x, p, na.rm = TRUE, type = 7)
  pmin(pmax(x, q[1]), q[2])
}


# Börse (and drop some cols)
trades <- read_parquet(paste0(emp_dir_raw,"T40_Bkg_Boerse.parquet"))
trades <- select(trades,-c("Ausfuehrungszeit_DT","Doc_ID","Konto_Pos_ID","Titel_Pos_ID","Order_Typisierung","Boersenplatz"))
trades[,Bruttowert_CHF := to_chf(Bruttowert,Handelswaehrung,DDate,freq="daily")]
trades[,Nettowert_CHF := to_chf(Nettowert,Handelswaehrung,DDate,freq="daily")]
trades[,MDate := as.IDate(lubridate::ceiling_date(MDate,"months")-1)]

# Asset Stammdaten
assets0 <- read_parquet(paste0(emp_dir_raw,"T31_Asset_Stammdaten.parquet"))
assets0[, MinDate := as.IDate(lubridate::ceiling_date(as.Date(paste0(MinDate, "01"), "%Y%m%d"), "month") - 1)]

assets <- read_parquet(paste0(emp_dir_raw,"T32_Asset_Stammdaten_Panel.parquet"))


keep <- c("Period_ID","Asset_ID","ISIN_Key_str","Instrumentengruppe","Fondsart","StruktProd_Klasse","Asset_Waehrung")
assets <- assets[,..keep]
assets[, MDate := as.IDate(lubridate::ceiling_date(as.Date(paste0(Period_ID, "01"), "%Y%m%d"), "month") - 1)]
assets[,Period_ID := NULL]




# ============================================================




# merge trades with asset stammdaten
# =================================

tradesm <- merge(trades,assets,by=c("Asset_ID","MDate"),all.x=T)

tradesm[is.na(Instrumentengruppe)]  # 36450


fill_cols <- c("Instrumentengruppe", "Asset_Waehrung")
recent <- assets0[MostRecent == 1, c("Asset_ID", "MinDate", ..fill_cols)]

tradesm[recent, on = .(Asset_ID),
        (fill_cols) := lapply(fill_cols, function(cl)
          fifelse(is.na(get(cl)) & !is.na(i.MinDate) & MDate >= i.MinDate,
                  get(paste0("i.", cl)),
                  get(cl)))]


## => check merge


# ==========================================
# exclude 

# (a) Securities events (dividends etc.)
tradesm <- tradesm[Medium != "Sec Event"]

# (b) Verwaltungsmandat
tradesm <- tradesm[Medium != "Verwaltungsmandat"]

tradesm[, `:=` (sell    = as.integer(Menge < 0),
                buy    = as.integer(Menge > 0),
        abs_qty     = abs(Menge),
        abs_chf     = abs(Bruttowert_CHF),
        sell_qty    = ifelse(Menge<0,Menge,0),
        sell_chf    = ifelse(Menge<0,Bruttowert_CHF,0),
        buy_qty     = ifelse(Menge>0,Menge,0),
        buy_chf     = ifelse(Menge>0,Bruttowert_CHF,0))]

tradesm[,px_chf := abs_chf/abs_qty]

tradesm <- tradesm[is.finite(px_chf) & px_chf > 0]


## ---------------------------------------------------------------------------
## DROP THE FUND-LAUNCH CAMPAIGN in April 2017
## ---------------------------------------------------------------------------
launch <- tradesm[MDate == "2017-04-30" & Asset_ID == 14034647]
if (nrow(launch)) {tradesm <- tradesm[!(MDate == "2017-04-30" & Asset_ID == 14034647)]}

## ---------------------------------------------------------------------------
## commission, converted to CHF
## ---------------------------------------------------------------------------
tradesm[, fx := fifelse(is.finite(Bruttowert) & Bruttowert != 0,
                   Bruttowert_CHF / Bruttowert, NA_real_)]
tradesm[, kosten_chf := abs(Kosten * fx)]
tradesm[!is.finite(kosten_chf), kosten_chf := NA_real_]




setorder(tradesm,Asset_ID,MDate,DDate)

write_parquet(tradesm,"../../data/trades.parquet")


## =============================================================
eq_funds   <- c("Fund - Shares (09)", "Fund - Exchange Traded (03)", "Fund - Index (04)")
bd_funds   <- "Fund - Bond (12)"
re_funds   <- "Fund - Real Estate (01)"
deriv_grp  <- c("Strukturierte Prod./Zertifikate", "Optionen", "Warrants", "Futures")
# deriv_grp  <- c("Strukturierte Prod./Zertifikate", "Warrants", "Futures")
alt_grp    <- c("Metall", "Kryptowaehrung", "Kryptowährung",
                "Ansprueche", "Ansprüche", "Anrechte", "Waehrung", "Währung")

tradesm[, asset_class := fcase(
  Instrumentengruppe == "Aktien",                                      "equity",
  Instrumentengruppe == "Fonds" & Fondsart %in% eq_funds,              "equity",
  Instrumentengruppe == "Obligationen",                                "bond",
  Instrumentengruppe == "Fonds" & Fondsart %in% bd_funds,              "bond",
  Instrumentengruppe == "Fonds" & Fondsart %in% re_funds,              "reales",
  Instrumentengruppe == "Fonds" & !is.na(Fondsart),                    "fund_mixed",
  Instrumentengruppe %in% deriv_grp,                                   "deriv",
  Instrumentengruppe %in% alt_grp,                                     "alt",
  default = NA_character_)]


# ===============================================================
# aggregate tradesm to daily


setnames(tradesm,c("Nettowert_CHF","Bruttowert_CHF"),c("net_chf","gross_chf"))

keep <- c("DDate","MDate","Bp_ID","Menge","gross_chf","net_chf","asset_class")
sum_cols <- c("Menge","gross_chf","net_chf","px_chf")

tradesm_d_long <- tradesm[, lapply(.SD, mean, na.rm = TRUE),
                          by = .(DDate, MDate, Bp_ID, asset_class),
                          .SDcols = sum_cols]

tradesm_d <- dcast(tradesm_d_long, DDate + MDate + Bp_ID ~ asset_class,
                   value.var = c("net_chf","px_chf"))

N_tot <- tradesm[, .(N = .N,
                     n_sell = -sum(sell_qty>0),
                     n_buy = sum(buy_qty>0)), by = .(DDate, MDate, Bp_ID)]

tradesm_d <- N_tot[tradesm_d, on = .(DDate, MDate, Bp_ID)]

setorder(tradesm_d_long,MDate,DDate,Bp_ID)
tradesm_d_long[,brutto_c := cumsum(gross_chf),by=.(asset_class)]
tradesm_d_long[,px_c := cumsum(px_chf),by=.(asset_class)]


ggplot(tradesm_d_long[DDate<=lubridate::today()],
       aes(x=DDate,y=px_c,color=asset_class))+
  geom_line()


setorder(tradesm_d,MDate,DDate,Bp_ID)


# =========================================================================
# aggregate tradesm to monthly

tradesm_d[,monthd := lubridate::ceiling_date(DDate,"month")-1]
tradesm[,monthd := lubridate::ceiling_date(DDate,"month")-1]


tradesm_m <- tradesm[,.(
  n_trades = .N,
  n_sells  = sum(sell),
  n_buys   = sum(sell == 0L),
  chf_sold = sum(gross_chf[sell == 1L]),   # sells carry gross_chf > 0, buys < 0
  chf_bought = -sum(gross_chf[sell == 0L]),
  n_traded_assets = uniqueN(Asset_ID),
  costs_chf = sum(kosten_chf,na.rm=T),
  costs_chf_sell = sum(kosten_chf[sell==1L],na.rm=T),
  chf_net_equity = -sum(net_chf[asset_class=="equity"]),
  chf_net_bond = -sum(net_chf[asset_class=="bond"]),
  chf_net_deriv = -sum(net_chf[asset_class=="deriv"]),
  chf_net_fund_mixed = -sum(net_chf[asset_class=="fund_mixed"]),
  chf_gross_equity = -sum(gross_chf[asset_class=="equity"]),
  chf_gross_bond = -sum(gross_chf[asset_class=="bond"])
),by=.(Bp_ID,monthd)]   # not MDate: ~21% of trades settle in a different month than booked -> duplicate pos rows
tradesm_m[, chf_net := chf_bought - chf_sold]


pos <- read_parquet("../../data/pos_aggm.parquet")


posm <- merge(pos,tradesm_m,by.x=c("Bp_ID","MDate"),by.y=c("Bp_ID","monthd"),all.x=T)
stopifnot(!anyDuplicated(posm, by = c("Bp_ID","MDate")))

# write_parquet(posm,"../../data/pos_agg_tr.parquet")






write_parquet(posm,"../../data/pos_agg_tr.parquet")
## ================================================================================
# 
# 
# ggplot(posm[,.(eq = sum(chf_net_equity,na.rm=T),
#                bd = sum(chf_net_bond,na.rm=T),
#                bought = sum(chf_bought,na.rm=T),
#                sold = sum(chf_sold,na.rm=T)),by=.(MDate)][order(MDate)]
#        [,`:=`(
#          eq_c = cumsum(eq),
#          bd_c = cumsum(bd),
#          bought_c = cumsum(bought),
#          sold_c =cumsum(sold)
#        )],aes(x=MDate))+
#   geom_line(aes(y=eq_c,color="equity"))+
#   geom_line(aes(y=bd_c,color="bond"))+
#   geom_line(aes(y=bought_c,color="bought"))+
#   geom_line(aes(y=sold_c,color="sold"))
# 
# 
# 
# ggplot(posm[,lapply(.SD,sum,na.rm=T),by=.(MDate),.SDcols=c("equity","bond","fund_mixed","deriv","alt")],aes(x=MDate))+
#   geom_line(aes(y=equity,color="equity"))+
#   geom_line(aes(y=bond+fund_mixed,color="bond"))+
#   # geom_line(aes(y=fund_mixed,color="fund_mixed"))+
#   geom_line(aes(y=deriv,color="deriv"))+
#   geom_line(aes(y=alt,color="alt"))
# 
# 
# ggplot(posm[,lapply(.SD,sum,na.rm=T),by=.(MDate),.SDcols=c("dp_equity","dp_bond")][order(MDate)],aes(x=MDate))+
#   geom_line(aes(y=cumsum(dp_equity),color="dp_equity"))+
#   geom_line(aes(y=cumsum(dp_bond),color="dp_bond"))
# 
# 
# ggplot(posm[,lapply(.SD,sum,na.rm=T),by=.(MDate),.SDcols=c("dq_equity","dq_bond")][order(MDate)],aes(x=MDate))+
#   geom_line(aes(y=cumsum(dq_equity),color="dq_equity"))+
#   geom_line(aes(y=cumsum(dq_bond),color="dq_bond"))
# 
# 
# ggplot(posm[!is.na(chf_net_equity),lapply(.SD,sum,na.rm=T),by=.(MDate),.SDcols=c("chf_net_equity","chf_net_bond","chf_net_deriv","dq_equity","dq_bond")][order(MDate)],aes(x=MDate))+
#   geom_line(aes(y=cumsum(chf_net_equity),color="chf_net_equity"))+
#   geom_line(aes(y=cumsum(chf_net_bond),color="chf_net_bond"))+
#   # geom_line(aes(y=cumsum(chf_gross_equity),color="chf_gross_equity"))+
#   # geom_line(aes(y=cumsum(chf_gross_bond),color="chf_gross_bond"))+
#   geom_line(aes(y=cumsum(dq_equity),color="dq_equity"))+
#   geom_line(aes(y=cumsum(dq_bond),color="dq_bond"))
# 
# ggplot(posm[!is.na(chf_net_equity),lapply(.SD,sum,na.rm=T),by=.(MDate),.SDcols=c("chf_net_equity","chf_net_bond","chf_net_deriv","chf_gross_equity","chf_gross_bond","dq_equity","dq_bond")][order(MDate)],aes(x=MDate))+
#   geom_line(aes(y=cumsum(chf_net_equity),color="chf_net_equity"))+
#   geom_line(aes(y=cumsum(chf_net_bond),color="chf_net_bond"))+
#   # geom_line(aes(y=cumsum(chf_net_deriv),color="chf_net_deriv"))+
#   geom_line(aes(y=cumsum(dq_equity),color="dq_equity"))+
#   geom_line(aes(y=cumsum(dq_bond),color="dq_bond"))
# 
# 
# ggplot(posm[year(MDate)==2018,lapply(.SD,sum,na.rm=T),by=.(MDate),.SDcols=c("dq_tot_pf","dfx_tot_pf","dq_equity")][,dqfx_tot_pf:=dq_tot_pf-dfx_tot_pf][order(MDate)],aes(x=MDate))+
#   geom_line(aes(y=cumsum(dq_tot_pf),color="qty"))+
#   geom_line(aes(y=cumsum(dqfx_tot_pf),color="qty-fx"))+
#   geom_line(aes(y=cumsum(dq_equity),color="qty (Equity)"))
# 
# 
# 
# ggplot(posm[,lapply(.SD,sum,na.rm=T),by=.(MDate),.SDcols=c("dq_tot_pf","dfx_tot_pf","dq_equity","chf_net")][,dqfx_tot_pf:=dq_tot_pf-dfx_tot_pf][order(MDate)],aes(x=MDate))+
#   geom_line(aes(y=cumsum(dq_tot_pf),color="qty"))+
#   geom_line(aes(y=cumsum(dqfx_tot_pf),color="qty-fx"))+
#   geom_line(aes(y=cumsum(dq_equity),color="qty (Equity)"))+
#   geom_line(aes(y=cumsum(chf_net),color="chf_net"))







