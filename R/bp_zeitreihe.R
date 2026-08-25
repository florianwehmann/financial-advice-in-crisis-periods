library(data.table)
library(lubridate)
library(dplyr)


rm(list=ls());gc()

emp_dir_raw <- "C:/Users/FWehmann/Dropbox/Wehmann Household Finance Project/RawData_Stata/"


bp_zeitreihe <- arrow::read_parquet(paste0(emp_dir_raw,"T02_Bp_Zeitreihe.parquet"))

pos_p <- arrow::read_parquet("../data/pos_p.parquet")

# 
# bp_zeitreihe[, MDate := as.IDate(ceiling_date(
#   as.Date(paste0(Period_ID, "01"), format = "%Y%m%d"), unit = "month") - 1)]
# 
# setcolorder(bp_zeitreihe,"MDate")
# 
# setorder(bp_zeitreihe,MDate,Bp_ID)
# 
# arrow::write_parquet(bp_zeitreihe,paste0(emp_dir_raw,"T02_Bp_Zeitreihe.parquet"))

bp_zeitreihe <- arrow::read_parquet(paste0(emp_dir_raw,"T02_Bp_Zeitreihe.parquet"))

bp_cols <- c("MDate","Bp_ID","EBanking_YN","Hauptbetreuer_ID","Nebenbetreuer_ID","EVV",
             "Profit_Fonds","Profit_Depot","Profit_Wertschriften","Profit_Vermoegensverwaltung",
             "Profit_SonstigerErfolg","Hypothekarzinsen",
             "Nationalitaet","Wohnland","PLZ_des_Wohnortes","Zivilstand")

bp_zeitreihe <- bp_zeitreihe[,..bp_cols]

pos_pm <- merge(pos_p,bp_zeitreihe,by=c("MDate","Bp_ID"),all.x=T)
rm(pos_p);gc()
# arrow::write_parquet(pos_pm,"../data/pos_pm.parquet")

## ============================================================================
# contacts

emp_dir_raw <- "C:/Users/FWehmann/Dropbox/Wehmann Household Finance Project/RawData_Stata/"

contacts <- read_parquet(paste0(emp_dir_raw,"T20_Kundenkontakte.parquet"))

# contacts[,Date := lubridate::ceiling_date(as.Date(paste0(substr(Period_ID,1,4),"-",substr(Period_ID,5,6),"-01")))-1]
contacts[, MDate := as.IDate(ceiling_date(
  as.Date(paste0(Period_ID, "01"), format = "%Y%m%d"), unit = "month") - 1)]
contacts[,ContactDate := as.IDate(Kontakt_DT)]


contacts_m <- merge(contacts,bp_zeitreihe,by=c("MDate","Bp_ID"),all.x=T)


bp_zeitreihe[MDate=="2024-12-31",.N,by=c("MDate","Hauptbetreuer_ID")] %>% summary()


advisors <- contacts_m[,.(
  n_bp = uniqueN(Bp_ID),
  n_contacts = .N,
  n_phys = sum(K_Physisch,na.rm=T),
  n_comprehensive = sum(K_Umfassendes_Beratungsgespraech,na.rm=T),
  n_invest = sum(K_Anlegen),
  n_basis = sum(K_Basisdienstleistung),
  n_satisfaction = sum(K_Kundenzufriedenheit),
  n_sys = sum(K_Syst_Geschaeftsmoeglichkeit),
  n_perf = sum(K_Performancebesprechung),
  n_compliance = sum(K_Compliance),
  n_mail = sum(K_Art == "E-Mail, Brief, Fax"),
  n_phone = sum(K_Art == "Telefonkontakt"),
  n_meeting = sum(K_Art %in% c("Kundentermin in der Bank","Kundentermin beim Kunden / extern","Videoberatung")),
  n_init_by_c = sum(K_Aufnahme %in% c("Durch Kunde","Durch Bevollmähtigten")),
  n_init_by_a = sum(K_Aufnahme %in% c("Durch Kundenberater","Zentral"))
),by=c("MDate","Hauptbetreuer_ID")]


mitarbeiter <- setDT(haven::read_stata(paste0(emp_dir_raw,"T10_Mitarbeiter_Merkmale.dta")))

for (nm in names(mitarbeiter)[c(3,11:18)]) {
  mitarbeiter[,(nm) := haven::as_factor(get(nm))]
}

mitarbeiter[, MDate := as.IDate(ceiling_date(
  as.Date(paste0(MA_Period_ID, "01"), format = "%Y%m%d"), unit = "month") - 1)]

write_parquet(mitarbeiter,paste0(emp_dir_raw,"T10_Mitarbeiter_Merkmale.parquet"))

ma_cols <- c("MDate","MA_ID","MA_Status","MA_ist_Team","MA_Anzahl_Kunden","MA_Team_ID","MA_Altersklasse","MA_Rang","MA_Berufsbild","MA_Kundensegment")


advisors_m <- merge(advisors,mitarbeiter[,..ma_cols],by.x=c("MDate","Hauptbetreuer_ID"),by.y=c("MDate","MA_ID"),all.x=T)


advisors_m[,.N,by=MA_Kundensegment]


contacts_m_ma <- merge(contacts_m,mitarbeiter[,..ma_cols],by.x=c("MDate","Hauptbetreuer_ID"),by.y=c("MDate","MA_ID"),all.x=T)

contacts_m_ma[,uniqueN(Bp_ID),by=MA_Kundensegment]




### POSITION File (incl. contacts and advisor informations)
pos_pm_ma <- merge(pos_pm,mitarbeiter[,..ma_cols],by.x=c("MDate","Hauptbetreuer_ID"),by.y=c("MDate","MA_ID"),all.x=T)
