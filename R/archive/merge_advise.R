## file merge for advise analysis
library(data.table)
library(haven)
library(arrow)


rm(list=ls()); gc()

source("fx_converter.R")

emp_dir_raw <- "C:/Users/FWehmann/Dropbox/Wehmann Household Finance Project/RawData_Stata/"
# emp_dir_prep <- "C:/Users/FWehmann/Dropbox/Wehmann Household Finance Project/DataPrepared/"
# emp_dir_prep_adv <- paste0(emp_dir_prep,"/Advice/")



# Bp Snapshot
# bp_snap <- read_dta(paste0(emp_dir_raw,"T01_Bp_Snapshot.dta")) %>% setDT()
# for (nm in names(bp_snap)[23:length(names(bp_snap))]) {
#   bp_snap[,(nm) := as_factor(get(nm))]
# }

bp_snap <- read_parquet(paste0(emp_dir_raw,"T01_Bp_Snapshot.parquet"))

# Börse file raw
# borse <- read_dta(paste0(emp_dir_raw,"T40_Bkg_Boerse.dta")) %>% setDT()
# for (nm in names(borse)[15:length(names(borse))]) {
#   borse[,(nm) := as_factor(get(nm))]
# }
# borse[,MDate := as.Date(paste0(substr(Period_ID,1,4),"-",substr(Period_ID,5,6),"-01"))]
# 
# borse[, ts    := as.POSIXct(Ausfuehrungszeit_DT, format = "%b %d %Y %I:%M%p", tz = "UTC")]
# borse[, DDate := as.IDate(ts)]
# 
# write_parquet(borse,paste0(emp_dir_raw,"T40_Bkg_Boerse.parquet"))

borse <- read_parquet(paste0(emp_dir_raw,"T40_Bkg_Boerse.parquet"))

# Asset Stammdaten
# assets <- read_dta(paste0(emp_dir_raw,"T31_Asset_Stammdaten.dta")) %>% setDT()
# for (nm in names(assets)[c(6:10,13:14)]) {
#   assets[,(nm) := as_factor(get(nm))]
# }
# assets[,MDate := as.Date(paste0(substr(Period_ID,1,4),"-",substr(Period_ID,5,6),"-01"))]

assets <- read_parquet(paste0(emp_dir_raw,"T31_Asset_Stammdaten.parquet"))

# Contacts file
contacts <- read_dta(paste0(emp_dir_raw,"T20_Kundenkontakte.dta")) %>% setDT()
for (nm in names(contacts)[21:ncol(contacts)]) {
  contacts[,(nm) := as_factor(get(nm))]
}
contacts[,Kontakt_DT := as.IDate(Kontakt_DT)]
contacts[,MDate := as.IDate(paste0(substr(Period_ID,1,4),"-",substr(Period_ID,5,6),"-01"))]

# Börse advise file
borse_adv <- read_dta(paste0(emp_dir_prep_adv,"T40_Bkg_Boerse_prepared_advice.dta")) %>% setDT()
for (nm in names(borse_adv)[c(5,8,9,17,18)]) {
  borse_adv[,(nm) := as_factor(get(nm))]
}
borse_adv[, MDate := as.IDate(ISOdate(1960 + MDate %/% 12, MDate %% 12 + 1, 1))]


# Cont file
cont <- read_dta(paste0(emp_dir_raw,"T05_Cont_Snapshot.dta")) %>% setDT()
for (nm in names(cont)[3:length(names(cont))]) {
  cont[,(nm) := as_factor(get(nm))]
}

# ===============================================================================================================
##### merge borse with contacts and identify advised trades

# prepare borse
borse_adv <- select(borse,-c("Ausfuehrungszeit_DT","Doc_ID","Konto_Pos_ID","Titel_Pos_ID","Order_Typisierung","Boersenplatz"))
borse_adv[,Bruttowert_CHF := to_chf(Bruttowert,Handelswaehrung,DDate,freq="daily")]
borse_adv[,Nettowert_CHF := to_chf(Nettowert,Handelswaehrung,DDate,freq="daily")]
borse_adv[,Nettowert_CHF2 := to_chf(Netto_in_Konto_Waehrung,Kontowaehrung,DDate,freq="daily")]


# filter only invest advice
contacts_inv <- contacts[K_Anlegen==1] %>% select(c("Bp_ID","Kontakt_DT","K_Anlegen","K_Performancebesprechung","K_Aufnahme","K_Art"))

borse_adv[, DDate      := as.IDate(DDate)]
contacts_inv[, Kontakt_DT := as.IDate(Kontakt_DT)]

setkey(contacts_inv, Bp_ID, Kontakt_DT)

contacts_dd <- unique(contacts_inv, by = c("Bp_ID", "Kontakt_DT"))
setkey(contacts_dd, Bp_ID, Kontakt_DT)

borse_adv[, c("last_contact", "K_Aufnahme") :=
                 contacts_dd[borse_adv,
                             on = .(Bp_ID, Kontakt_DT = DDate),
                             roll = 5,
                             .(x.Kontakt_DT, x.K_Aufnahme)]]
borse_adv[, advised := !is.na(last_contact)]
borse_adv[, days_since_contact := as.integer(DDate - last_contact)]

borse_adv[K_Aufnahme == "n/a", K_Aufnahme := NA]
borse_adv[K_Aufnahme == "Durch Bevollmächtigten", K_Aufnahme := "Durch Kunde"]
borse_adv[K_Aufnahme == "Zentral", K_Aufnahme := "Durch Kundenberater"]

# ===============================================================================================================




####
# merge börse with assets
assets[, join_date := as.IDate(paste0(substr(MinDate,1,4),"-",substr(MinDate,5,6),"-01"))]
setkey(assets, Asset_ID, join_date)

asset_cols <- setdiff(names(assets), c("Asset_ID", "join_date"))
borse_adv[, (asset_cols) :=
                assets[borse_adv,
                       on = .(Asset_ID, join_date = MDate),
                       roll = TRUE,
                       mget(paste0("x.", asset_cols))]]

# force the (possibly pre-existing) asset columns to the end, regardless of name overlap
setcolorder(borse_adv, c(setdiff(names(borse_adv), asset_cols), asset_cols))


## merge börse with snapshot
bp_snap_cols <- c("Bp_ID","Person_ID","Geburtsjahr","Hauptbankkunde","Geschlecht")

borse_adv <- merge(borse_adv, bp_snap[,bp_snap_cols, with = FALSE], by = "Bp_ID", all.x = TRUE)


## merge dt with cont
cont_cols <- c("Cont_ID","Bp_ID","Anlagepaket","Anlegerprofil")
borse_adv <- merge(borse_adv,cont[,cont_cols,with=F],by=c("Bp_ID","Cont_ID"),all.x=T)


write_parquet(borse_adv,"../data/boerse_merged.parquet")

