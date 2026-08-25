## merge pos 2 (advise, bp_zeitreihe, mitarbeiter etc.)

# load packages
library(data.table)
library(haven)
library(arrow)
library(lubridate)
library(dplyr)
library(duckdb)


rm(list=ls()); gc()

source("fx_converter.R")

emp_dir_raw <- "C:/Users/FWehmann/Dropbox/Wehmann Household Finance Project/RawData_Stata/"


### ==========================================================================
### LOAD FILES


# load pos
con <- dbConnect(duckdb())

path <- "../data/pos_m1.parquet"

dbGetQuery(con, sprintf("DESCRIBE SELECT * FROM read_parquet('%s')", path))
# col selection
pos_cols <- c("MDate","Bp_ID","Person_ID","Cont_ID","Period_ID","Asset_ID","Asset","Kontoprodukt","Depotprodukt","Bilanzposition",
              "Instrumentengruppe","Fondsart","Asset_Waehrung","Referenzwaehrung","Positionswaehrung",
              "Menge","Vermoegen_CHF","Geschaeftsvolumen_CHF",
              "DA_Titelkursabweichung_CHF","DA_Devisenkursabweichung_CHF","DA_Mengenabweichung_CHF","DA_Delta_CHF","DA_Gesamtabweichung_CHF",
              "Geburtsjahr","Hauptbankkunde","Geschlecht","Anlagepaket")

cols_sql <- paste(sprintf('"%s"', pos_cols), collapse = ", ")

exclude <- c("Bar","Nicht zugeteilt (Dummy)","Money Market Deposit","Swaps","Limite","Waehrung")

excl_sql <- paste(dbQuoteString(con, exclude), collapse = ", ")

# pos <- setDT(dbGetQuery(con, sprintf(
#   "SELECT %s FROM read_parquet('%s')
#    WHERE Instrumentengruppe NOT IN (%s)",
#   cols_sql, path, excl_sql
# )))
pos <- setDT(dbGetQuery(con, sprintf(
  "SELECT %s FROM read_parquet('%s')",
  cols_sql, path, excl_sql
)))


gc()

# BP Snapshot
bp_snap <- read_parquet(paste0(emp_dir_raw,"T01_Bp_Snapshot.parquet"))

# Börse (and drop some cols)
borse <- read_parquet(paste0(emp_dir_raw,"T40_Bkg_Boerse.parquet"))
borse <- select(borse,-c("Ausfuehrungszeit_DT","Doc_ID","Konto_Pos_ID","Titel_Pos_ID","Order_Typisierung","Boersenplatz"))
borse[,Bruttowert_CHF := to_chf(Bruttowert,Handelswaehrung,DDate,freq="daily")]
borse[,Nettowert_CHF := to_chf(Nettowert,Handelswaehrung,DDate,freq="daily")]
borse[,MDate := lubridate::ceiling_date(MDate,"months")-1]

# Asset Stammdaten
assets <- read_parquet(paste0(emp_dir_raw,"T31_Asset_Stammdaten.parquet"))

# Contacts
contacts <- read_parquet(paste0(emp_dir_raw,"T20_Kundenkontakte.parquet"))

# Cont
cont <- read_parquet(paste0(emp_dir_raw,"T05_Cont_Snapshot.parquet"))

# Mitarbeiter
mitarbeiter <- read_parquet(paste0(emp_dir_raw,"T10_Mitarbeiter_Merkmale.parquet"))

# Bp Zeitreihe
bp_cols <- c("MDate","Bp_ID","EBanking_YN","Hauptbetreuer_ID","Nebenbetreuer_ID","EVV",
             "Profit_Fonds","Profit_Depot","Profit_Wertschriften","Profit_Vermoegensverwaltung",
             "Profit_SonstigerErfolg","Hypothekarzinsen",
             "Nationalitaet","Wohnland","PLZ_des_Wohnortes","Zivilstand")

bp_zeitreihe <- setDT(arrow::read_parquet(
  paste0(emp_dir_raw, "T02_Bp_Zeitreihe.parquet"),
  col_select = all_of(bp_cols)
))

gc()

### ==========================================================================
# merge BÖRSE with CONTACTS and IDENTIFY ADVISED TRADES

# contacts_inv <- contacts[K_Anlegen==1] %>% select(c("Bp_ID","Kontakt_DT","K_Anlegen","K_Performancebesprechung","K_Aufnahme","K_Art","K_Physisch"))
# contacts_perf <- contacts[K_Performancebesprechung==1] %>% select(c("Bp_ID","Kontakt_DT","K_Anlegen","K_Performancebesprechung","K_Aufnahme","K_Art","K_Physisch"))
# contacts_inv_perf <- contacts[K_Anlegen==1 & K_Performancebesprechung==1] %>% select(c("Bp_ID","Kontakt_DT","K_Anlegen","K_Performancebesprechung","K_Aufnahme","K_Art","K_Physisch"))
# 
# borse[, DDate      := as.IDate(DDate)]
# contacts_inv[, Kontakt_DT := as.IDate(Kontakt_DT)]
# 
# setkey(contacts_inv, Bp_ID, Kontakt_DT)
# 
# contacts_dd <- unique(contacts_inv, by = c("Bp_ID", "Kontakt_DT"))
# setkey(contacts_dd, Bp_ID, Kontakt_DT)
# 
# borse[, c("last_contact", "K_Aufnahme","K_Anlegen","K_Performancebesprechung","K_Art","K_Physisch") :=
#             contacts_dd[borse,
#                         on = .(Bp_ID, Kontakt_DT = DDate),
#                         roll = 5,
#                         .(x.Kontakt_DT, x.K_Aufnahme, x.K_Anlegen, x.K_Performancebesprechung, x.K_Art, K_Physisch)]]
# borse[, advised := !is.na(last_contact)]
# borse[, days_since_contact := as.integer(DDate - last_contact)]

##

contacts[, Kontakt_DT := as.IDate(Kontakt_DT)]
borse[,    DDate      := as.IDate(DDate)]

WINDOW <- 5L
CCOLS  <- c("K_Aufnahme","K_Anlegen","K_Performancebesprechung","K_Art","K_Physisch")

cq <- contacts[K_Anlegen == 1L | K_Performancebesprechung == 1L,
               c("Bp_ID", "Kontakt_DT", CCOLS), with = FALSE]

# same-day tie-break: a contact flagged as both wins, then Anlegen, then Perf
cq[, prio := fifelse(K_Anlegen == 1L & K_Performancebesprechung == 1L, 1L,
                     fifelse(K_Anlegen == 1L, 2L, 3L))]
setorder(cq, Bp_ID, Kontakt_DT, prio)

cd <- unique(cq, by = c("Bp_ID", "Kontakt_DT"))   # unique() keeps the first row per group
cd[, prio := NULL]
setkey(cd, Bp_ID, Kontakt_DT)

idx <- cd[borse, on = .(Bp_ID, Kontakt_DT = DDate), roll = WINDOW, which = TRUE]

borse[, last_contact       := cd$Kontakt_DT[idx]]
borse[, (CCOLS)            := lapply(CCOLS, function(cn) cd[[cn]][idx])]
borse[, advised            := !is.na(last_contact)]
borse[, days_since_contact := as.integer(DDate - last_contact)]

has <- function(keep) {
  k <- unique(contacts[keep, .(Bp_ID, Kontakt_DT)]); setkey(k, Bp_ID, Kontakt_DT)
  !is.na(k[borse, on = .(Bp_ID, Kontakt_DT = DDate), roll = WINDOW, which = TRUE])
}
borse[, advised_inv  := has(contacts$K_Anlegen == 1L)]
borse[, advised_perf := has(contacts$K_Performancebesprechung == 1L)]
borse[, advised_inv_perf := has(contacts$K_Anlegen == 1L & contacts$K_Performancebesprechung == 1L)]


## ===========================================================================



assets[, join_date := as.IDate(ceiling_date(as.Date(paste0(substr(MinDate,1,4),"-",substr(MinDate,5,6),"-01")),"months")-1)]
setkey(assets, Asset_ID, join_date)

asset_cols <- setdiff(names(assets), c("Asset_ID", "join_date"))
borse[, (asset_cols) :=
            assets[borse,
                   on = .(Asset_ID, join_date = MDate),
                   roll = TRUE,
                   mget(paste0("x.", asset_cols))]]

# force the (possibly pre-existing) asset columns to the end, regardless of name overlap
setcolorder(borse, c(setdiff(names(borse), asset_cols), asset_cols))
rm(assets)

## merge börse with snapshot
bp_snap_cols <- c("Bp_ID","Person_ID","Geburtsjahr","Hauptbankkunde","Geschlecht")

borse <- merge(borse, bp_snap[,bp_snap_cols, with = FALSE], by = "Bp_ID", all.x = TRUE)
rm(bp_snap)

## merge dt with cont
cont_cols <- c("Cont_ID","Bp_ID","Anlagepaket","Anlegerprofil")
borse <- merge(borse,cont[,cont_cols,with=F],by=c("Bp_ID","Cont_ID"),all.x=T)
rm(cont)

setcolorder(borse,c("DDate","MDate"))
setorder(borse,MDate,DDate,Bp_ID,Asset_ID)


write_parquet(borse,"../data/boerse.parquet")

### ==========================================================================
## merge BÖRSE with POS

# collapse to Bp_ID x Asset_ID x MDate: if traded multiple times in the same
# month, keep an advised trade over a non-advised one, then just the first
setorder(borse, Bp_ID, Asset_ID, MDate, -advised_inv, -advised_perf, DDate)
borse <- unique(borse, by = c("Bp_ID", "Asset_ID", "MDate"))

# merge into pos: only the borse columns not already in pos
gc()
borse_cols <- setdiff(names(borse), names(pos))
pos <- merge(pos, borse[, c("MDate", "Bp_ID", "Asset_ID", borse_cols), with = FALSE],
             by = c("MDate", "Bp_ID", "Asset_ID"), all.x = TRUE)
gc()

write_parquet(pos,"../data/pos_mb.parquet")
