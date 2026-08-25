
library(data.table)
library(arrow)
library(lubridate)
library(ggplot2)

rm(list = ls()); gc()

out_fig_path <- "C:/Users/FWehmann/Dropbox/Apps/Overleaf/Uncertainty and Financial Advise (1)/figures"
emp_dir_raw <- "C:/Users/FWehmann/Dropbox/Wehmann Household Finance Project/RawData_Stata/"

source("fx_converter.R")

save_fig <- function(p, name, width = 8, height = 5) {
  ggsave(file.path(out_fig_path, paste0(name, ".pdf")), p, width = width, height = height)
  invisible(p)
}

smp_start <- as.Date("2011-01-01")
instr_sel <- c("Aktien", "Fonds")

win_pre  <- 12L    # months before the trade
win_post <- 12L    # months after the trade

# ==================================================================
# Aktienkurse
asset_p <- arrow::read_parquet(paste0(emp_dir_raw,"T60_Aktienkurse.parquet"))
asset_p[, Datum := as.IDate(levels(Datum), format = "%Y-%m-%d")[Datum]]
asset_p[, ym := Datum - mday(Datum) + 1L]
# asset_p[, Datum := as.Date(as.character(Datum))]
# asset_p[, ym := as.Date(cut(Datum, "month"))] 
setorderv(asset_p,c("Datum","Asset_ID"))


# asset_s <- arrow::read_parquet(paste0(emp_dir_raw,"T31_Asset_Stammdaten.parquet"))
asset_sp <- arrow::read_parquet(paste0(emp_dir_raw,"T32_Asset_Stammdaten_Panel.parquet"))

u <- unique(asset_sp$Period_ID)
lut <- as.IDate(paste0(u, "01"), format = "%Y%m%d")
asset_sp[, ym := lut[match(Period_ID, u)]]

keep <- c("ym","Asset_ID","ISIN_Key_str","Asset_Waehrung")
asset_sp <- asset_sp[,..keep]

# merge asset prices with stammdaten
asset_p_s <- merge(asset_p,asset_sp,by=c("Asset_ID","ym"),all.x=T)

asset_p_s[,Kurs_fxconv := to_chf(Kurs_CHF,Asset_Waehrung,Datum,freq="monthly")]

pos[Asset_ID==19861158] %>% View()

asset_p_m <- unique(asset_p_s, by = c("Asset_ID", "ym"))

# asset_p_m[,ret_1m := c(diff(Kurs_CHF,1)/shift(Kurs_CHF,-1)),by=Asset_ID]

grid <- CJ(Asset_ID = unique(asset_p_m$Asset_ID),
           ym       = seq(min(asset_p_m$ym), max(asset_p_m$ym), by = "month"))

setkey(asset_p_m, Asset_ID, ym)
panel <- asset_p_m[grid]                                     # NA where no observation
panel[, Kurs_CHF := nafill(Kurs_CHF, "locf"), by = Asset_ID]
panel[, Kurs_fxconv := nafill(Kurs_fxconv, "locf"), by = Asset_ID]

setorderv(panel,c("Asset_ID","ym"))
panel[, ret_1m := Kurs_CHF / shift(Kurs_CHF) - 1, by = Asset_ID]
panel[, ret_12m := Kurs_CHF / shift(Kurs_CHF,12) - 1, by = Asset_ID]
panel[, ret_1m_fxconv := Kurs_fxconv / shift(Kurs_fxconv) - 1, by = Asset_ID]
panel[, ret_12m_fxconv := Kurs_fxconv / shift(Kurs_fxconv,12) - 1, by = Asset_ID]

# interpolate return where missing
panel[, Kurs_obs := fifelse(is.na(Datum), NA_real_, Kurs_CHF)]  # NA where no real trade
panel[, is_obs   := !is.na(Datum)] 

panel[, Kurs_ip := {
  i <- which(is_obs)
  if (length(i) >= 2)
    exp(approx(x = as.numeric(Datum[i]), y = log(Kurs_obs[i]),
               xout = as.numeric(ym))$y)
  else Kurs_obs
}, by = Asset_ID]

panel[, ret_1m_ip := Kurs_ip / shift(Kurs_ip) - 1, by = Asset_ID]
panel[, ret_12m_ip := Kurs_ip / shift(Kurs_ip,12) - 1, by = Asset_ID]

ggplot(panel[Asset_ID==274171 & !is.na(ret_12m)  & !is.na(ret_1m)],aes(x=ym))+
     geom_line(aes(y=cumprod(1+ret_12m/12)),color="tomato")+
     geom_line(aes(y=cumprod(1+ret_12m_ip/12)),linetype=2)+
     geom_line(aes(y=cumprod(1+ret_1m)),color="skyblue")+
     geom_line(aes(y=cumprod(1+ret_1m_ip)),linetype=2,color="goldenrod1")


## add forward returns
panel[, ret_f12m    := shift(Kurs_CHF, 12, type = "lead") / Kurs_CHF - 1, by = Asset_ID]
panel[, ret_f12m_ip := shift(Kurs_ip,  12, type = "lead") / Kurs_ip   - 1, by = Asset_ID]

drop <- c("Datum","Source","Kurs_CHF","Kurs_obs","Kurs_fxconv","is_obs","Kurs_ip")
panel[, (drop) := NULL]


## ===================================================================
## positions
pos_file <- "../data/pos_b_merged.parquet"

pos_ds <- arrow::open_dataset(pos_file)

# `Menge` is the position quantity and is only needed for the direction proxy;
# K_Aufnahme is who initiated the contact behind an advised trade
pos_cols <- c("Bp_ID", "Cont_ID", "Asset_ID", "MDate", "advised", "K_Aufnahme",
              "Instrumentengruppe", "Menge",
              "Geschaeftsvolumen_CHF", "DA_Titelkursabweichung_CHF","DA_Devisenkursabweichung_CHF", "DA_Mengenabweichung_CHF","DA_Delta_CHF","DA_Gesamtabweichung_CHF")

miss <- setdiff(pos_cols, names(pos_ds))
if (length(miss)) stop("not in ", pos_file, ": ", paste(miss, collapse = ", "))

# a buy/sell field would come from the boerse side of the merge. these are the
# plausible names - whatever is there is loaded too and reported below
dir_cand <- grep("kauf|verkauf|richtung|seite|auftragsart|geschaeftsart|transaktion|bewegung",
                 names(pos_ds), value = TRUE, ignore.case = TRUE)
message("candidate direction columns in the parquet: ",
        if (length(dir_cand)) paste(dir_cand, collapse = ", ") else "none found")

pos_cols <- unique(c(pos_cols, dir_cand))

mdate_type <- pos_ds$schema$GetFieldByName("MDate")$type$ToString()
push_date  <- grepl("date|timestamp", mdate_type, ignore.case = TRUE) &&
  requireNamespace("dplyr", quietly = TRUE)

pos <- if (push_date) {
  dplyr::collect(dplyr::filter(dplyr::select(pos_ds, dplyr::all_of(pos_cols)),
                               MDate >= smp_start))
} else {
  arrow::read_parquet(pos_file, col_select = tidyselect::all_of(pos_cols))
}
setDT(pos)
rm(pos_ds); gc()

pos[, MDate := as.Date(MDate)]
pos <- pos[MDate >= smp_start]

if (is.character(pos$Instrumentengruppe)) pos[, Instrumentengruppe := factor(Instrumentengruppe)]
pos <- pos[Instrumentengruppe %in% instr_sel]
gc()

pos[, mnum := year(MDate) * 12L + month(MDate)]


pos_a <- pos[,lapply(.SD,sum,na.rm=T),by=MDate,.SDcols=c("Geschaeftsvolumen_CHF","DA_Titelkursabweichung_CHF","DA_Gesamtabweichung_CHF")]

pos_a[,ret := DA_Titelkursabweichung_CHF / (Geschaeftsvolumen_CHF - DA_Gesamtabweichung_CHF)]
ggplot(pos_a,aes(x=MDate,y=cumprod(1+ret)))+geom_line()


# SMI, same construction as the other scripts: month-end close stamped to the 1st
smi <- fread("../data/hsmi.csv", skip = 4, select = c(1, 2, 5))
setnames(smi, c("Date", "SMI", "SMI_TR"))
smi[, Date := as.Date(Date, format = "%d.%m.%Y")]
smi <- smi[!is.na(Date)][order(Date)]
smi[, MDate := floor_date(Date, "month")]

smi_m <- smi[, .(SMI = last(SMI), SMI_TR = last(SMI_TR)), by = MDate]
smi_m[, mnum := year(MDate) * 12L + month(MDate)]
smi_m[, smi_ret := SMI / shift(SMI) - 1]


# ===============================================
# Asset returns
pos[, ok_ret := !is.na(Geschaeftsvolumen_CHF) & Geschaeftsvolumen_CHF > 0 &
      !is.na(DA_Titelkursabweichung_CHF)]


# pos_a <- merge(pos,panel,by.x=c("MDate","Asset_ID"),by.y=c("ym","Asset_ID"),all.x=T)
# gc()


# asset_m <- pos[ok_ret == TRUE,
#                .(v_all = sum(Geschaeftsvolumen_CHF),
#                  d_all = sum(DA_Titelkursabweichung_CHF),
#                  n_all = .N,
#                  v_ut  = sum(Geschaeftsvolumen_CHF[is.na(advised)]),
#                  d_ut  = sum(DA_Titelkursabweichung_CHF[is.na(advised)]),
#                  n_ut  = sum(is.na(advised))),
#                by = .(Asset_ID, MDate)]
asset_m <- pos[ok_ret == TRUE,
               .(v = sum(Geschaeftsvolumen_CHF),
                 dtitel = sum(DA_Titelkursabweichung_CHF),
                 dmenge = sum(DA_Mengenabweichung_CHF),
                 ddevisen = sum(DA_Devisenkursabweichung_CHF),
                 ddelta = sum(DA_Delta_CHF),
                 dgesamt = sum(DA_Gesamtabweichung_CHF),
                 n = .N),
               by = .(Asset_ID, MDate)]
gc()

# asset_m[,ret_all := d_all/v_all]
# asset_m[,ret_ut := d_ut/v_ut]

asset_m[,ret := dtitel/v]
asset_m[,ret_madj := dtitel/(v-dmenge)]
asset_m[,ret_gesamt := dgesamt/v]
asset_m[,ret_gesamt_madj := dgesamt/(v-dmenge)]
asset_m[,ret_adj := dtitel/(v-dmenge-ddevisen-ddelta)]
asset_m[,ret_adj2 := dtitel/(v-dgesamt)]


am <- merge(asset_m,panel,by.x=c("MDate","Asset_ID"),by.y=c("ym","Asset_ID"),all.x=T)

View(am[year(MDate)==2024])
View(am[Asset_ID == 19861158])

am[dtitel==0,ret_adj:=0]
am[dtitel==0,ret_adj2:=0]
am[is.na(ret_adj),ret_adj:=0]
am[is.na(ret_adj),ret_adj2:=0]
am[is.na(ret_1m),ret_1m:=0]
am[is.na(ret_1m_fxconv),ret_1m_fxconv:=0]

ggplot()+
  geom_line(data=am[Asset_ID == 274965 & MDate>="2014-01-01"],aes(x=MDate,y=cumprod(1+ret_adj2)/(1+ret_adj2[1]),color="pos"),size=1)+
  geom_line(data=am[Asset_ID == 274965 & MDate>="2014-01-01"],aes(x=MDate,y=cumprod(1+ret_1m)/(1+ret_1m[1]),color="kurs"),size=1)
  # geom_line(data=am[Asset_ID == 276086 & MDate>="2014-01-01"],aes(x=MDate,y=cumprod(1+ret_1m_fxconv)/(1+ret_1m_fxconv[1]),color="kurs fx"))+
  # geom_line(data=unh_l[date>="2014-01-01" & date < "2025-01-01"],aes(x=date,y=adj_chf/adj_chf[1],color="yahoo in chf"),size=1)#+
  # geom_line(data=unh_l[date>="2014-01-01" & date < "2025-01-01"],aes(x=date,y=adjusted/adjusted[1],color="yahoo in usd"))




pos[,ret := DA_Titelkursabweichung_CHF/Geschaeftsvolumen_CHF]
pos[,ret_madj := DA_Titelkursabweichung_CHF/(Geschaeftsvolumen_CHF-DA_Mengenabweichung_CHF)]
pos[,ret_gesamt := DA_Gesamtabweichung_CHF/Geschaeftsvolumen_CHF]
pos[,ret_gesamt_madj := DA_Gesamtabweichung_CHF/(Geschaeftsvolumen_CHF-DA_Mengenabweichung_CHF)]
pos[,ret_adj := DA_Titelkursabweichung_CHF/(Geschaeftsvolumen_CHF-DA_Mengenabweichung_CHF-DA_Devisenkursabweichung_CHF-DA_Delta_CHF)]
pos[,ret_adj2 := DA_Titelkursabweichung_CHF/(Geschaeftsvolumen_CHF-DA_Gesamtabweichung_CHF)]

pos[Asset_ID == 19861158 & DA_Titelkursabweichung_CHF !=0] %>% select(c("Bp_ID","Asset_ID","MDate","advised",
                                                                        "ret","ret_madj","ret_gesamt","ret_gesamt_madj","ret_adj","ret_adj2")) %>% View()



dev_agg <- pos[,sum(DA_Devisenkursabweichung_CHF,na.rm=T),by=MDate]
ggplot(dev_agg[MDate>="2015-07-01"],aes(x=MDate,y=cumsum(V1)))+geom_line()
  
  