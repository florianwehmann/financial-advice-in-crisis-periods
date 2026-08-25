## merge pos with zeitreihe und mitarbeiter

# load packages

library(dplyr)
library(data.table)
library(arrow)
library(lubridate)


rm(list=ls()); gc()

emp_dir_raw <- "C:/Users/FWehmann/Dropbox/Wehmann Household Finance Project/RawData_Stata/"


instr_sel <- c("Aktien","Fonds","Obligationen","Strukturierte Prod./Zertifikate","Optionen")
# instr_sel <- c("Aktien","Fonds")


## POS
pos <- read_parquet("../data/pos_mb.parquet")

# wealth over ALL instrument groups — must run before the filter
pos_wealth <- pos[, .(wealth = sum(Vermoegen_CHF, na.rm = TRUE)),
                  by = .(MDate, Bp_ID)]

# filter

lvl <- c("Aktien","Anrechte","Ansprüche","Fonds","Futures","Kredit","Kryptowährung",
         "Metall","Obligationen","Optionen","Strukturierte Prod./Zertifikate",
         "Währung","Warrants")

pos[, ig_id := chmatch(Instrumentengruppe, lvl)][, Instrumentengruppe := NULL]
pos <- pos[ig_id %in% chmatch(instr_sel, lvl)]
pos[, Instrumentengruppe := lvl[ig_id]][, ig_id := NULL]

MEETING <- c("Kundentermin beim Kunden / extern","Kundentermin in der Bank","Videoberatung")

pos[, `:=`(
  K_phone              = as.integer(K_Art %in% "Telefonkontakt"),
  K_meeting            = as.integer(K_Art %in% MEETING),
  K_mail               = as.integer(K_Art %in% "E-Mail, Brief, Fax"),
  t_medium_phone       = as.integer(Medium %in% "Telefon"),
  t_medium_visit       = as.integer(Medium %in% "Besuch"),
  t_medium_ebanking    = as.integer(Medium %in% "E-Banking"),
  t_medium_vvm         = as.integer(Medium %in% "Verwaltungsmandat"),
  advised_init_client  = as.integer(K_Aufnahme %in% "Durch Kunde"),
  advised_init_advisor = as.integer(K_Aufnahme %in% "Durch Kundenberater")
)]

pos[pos_wealth, wealth := i.wealth, on = .(MDate, Bp_ID)]
rm(pos_wealth); gc()

write_parquet(pos,"../data/pos_full.parquet")

# Mitarbeiter
mitarbeiter <- read_parquet(paste0(emp_dir_raw,"T10_Mitarbeiter_Merkmale.parquet"))
ma_cols <- c("MDate","MA_ID","MA_Status","MA_ist_Team","MA_Anzahl_Kunden","MA_Team_ID","MA_Altersklasse","MA_Rang","MA_Berufsbild","MA_Kundensegment")
mitarbeiter <- mitarbeiter[,..ma_cols]

# Bp Zeitreihe
bp_cols <- c("MDate","Bp_ID","EBanking_YN","Hauptbetreuer_ID","Nebenbetreuer_ID","EVV",
             "Profit_Fonds","Profit_Depot","Profit_Wertschriften","Profit_Vermoegensverwaltung",
             "Profit_SonstigerErfolg","Hypothekarzinsen",
             "Nationalitaet","Wohnland","PLZ_des_Wohnortes","Zivilstand")

bp_zeitreihe <- setDT(arrow::read_parquet(
  paste0(emp_dir_raw, "T02_Bp_Zeitreihe.parquet"),
  col_select = all_of(bp_cols)
))

# write_parquet(pos,"../data/pos.parquet")
# 
# pos <- read_parquet("../data/pos.parquet")

## Aggregate POS to monthly
## ============================================================================
## Aggregate on Person x month
pos_m <- pos[,.(
  n_trades_adv_inv = sum(advised_inv,na.rm=T),
  n_trades_adv_perf = sum(advised_perf,na.rm=T),
  n_trades_adv_inv_perf = sum(advised_inv_perf,na.rm=T),
  n_trades_adv_init_c = sum(advised_init_client,na.rm=T),
  n_trades_adv_init_a = sum(advised_init_advisor,na.rm=T),
  n_trades = .N,
  # n_assets = uniqueN(Asset_ID),
  vol = sum(Geschaeftsvolumen_CHF,na.rm=T),
  dprice = sum(DA_Titelkursabweichung_CHF,na.rm=T),
  dtotal = sum(DA_Gesamtabweichung_CHF,na.rm=T),
  buysell = sum(DA_Mengenabweichung_CHF,na.rm=T),
  sell = sum(DA_Mengenabweichung_CHF[DA_Mengenabweichung_CHF<0],na.rm=T),
  buy = sum(DA_Mengenabweichung_CHF[DA_Mengenabweichung_CHF>0],na.rm=T),
  dfx = sum(DA_Devisenkursabweichung_CHF,na.rm=T),
  Anlagepaket = last(Anlagepaket),
  Kontoprodukt = last(Kontoprodukt),
  Depotprodukt = last(Depotprodukt),
  last_contact = last(last_contact),
  wealth = mean(wealth),
  n_physical = sum(K_Physisch,na.rm=T),
  n_phone = sum(K_phone,na.rm=T),
  n_meeting = sum(K_meeting,na.rm=T),
  n_mail = sum(K_mail,na.rm=T),
  n_contacts_performance = sum(K_Performancebesprechung,na.rm=T),
  n_contacts_investment = sum(K_Anlegen,na.rm=T),
  s_bruttowert = sum(Bruttowert_CHF,na.rm=T),
  s_nettowert = sum(Nettowert_CHF,na.rm=T),
  main_bank = last(Hauptbankkunde),
  n_medium_phone = sum(t_medium_phone,na.rm=T),
  n_medium_visit = sum(t_medium_visit,na.rm=T),
  n_medium_ebanking = sum(t_medium_ebanking,na.rm=T),
  n_medium_vvm = sum(t_medium_vvm,na.rm=T)
),by=c("MDate","Bp_ID")]
n_ass <- unique(pos, by = c("MDate","Bp_ID","Asset_ID"))[
  , .(n_assets = .N), by = .(MDate, Bp_ID)]
pos_m[n_ass, n_assets := i.n_assets, on = .(MDate, Bp_ID)]
gc()
setorder(pos_m,MDate,Bp_ID)



# rm(pos)

## ============================================================================
# add Bp_Zeitreihe and Mitarbeiter Merkmale

bp_ma_zeitreihe <- merge(bp_zeitreihe,mitarbeiter,by.x=c("MDate","Hauptbetreuer_ID"),by.y=c("MDate","MA_ID"),all.x=T)

pos_m <- merge(pos_m,bp_ma_zeitreihe,by=c("MDate","Bp_ID"),all.x=T)


write_parquet(pos_m,"../data/pos_m.parquet")


## ============================================================================
## Aggregate on Asset x month
pos_a <- pos[,.(
  n_trades_adv_inv = sum(advised_inv,na.rm=T),
  n_trades_adv_perf = sum(advised_perf,na.rm=T),
  n_trades_adv_inv_perf = sum(advised_inv_perf,na.rm=T),
  n_trades_adv_init_c = sum(advised_init_client,na.rm=T),
  n_trades_adv_init_a = sum(advised_init_advisor,na.rm=T),
  n_trades = .N,
  # n_assets = uniqueN(Asset_ID),
  vol = sum(Geschaeftsvolumen_CHF,na.rm=T),
  dprice = sum(DA_Titelkursabweichung_CHF,na.rm=T),
  dtotal = sum(DA_Gesamtabweichung_CHF,na.rm=T),
  buysell = sum(DA_Mengenabweichung_CHF,na.rm=T),
  sell = sum(DA_Mengenabweichung_CHF[DA_Mengenabweichung_CHF<0],na.rm=T),
  buy = sum(DA_Mengenabweichung_CHF[DA_Mengenabweichung_CHF>0],na.rm=T),
  dfx = sum(DA_Devisenkursabweichung_CHF,na.rm=T),
  # Anlagepaket = last(Anlagepaket),
  # Kontoprodukt = last(Kontoprodukt),
  # Depotprodukt = last(Depotprodukt),
  last_contact = last(last_contact),
  # wealth = mean(wealth),
  n_physical = sum(K_Physisch,na.rm=T),
  n_phone = sum(K_phone,na.rm=T),
  n_meeting = sum(K_meeting,na.rm=T),
  n_mail = sum(K_mail,na.rm=T),
  n_contacts_performance = sum(K_Performancebesprechung,na.rm=T),
  n_contacts_investment = sum(K_Anlegen,na.rm=T),
  s_bruttowert = sum(Bruttowert_CHF,na.rm=T),
  s_nettowert = sum(Nettowert_CHF,na.rm=T),
  # main_bank = sum(Hauptbankkunde),
  n_medium_phone = sum(t_medium_phone,na.rm=T),
  n_medium_visit = sum(t_medium_visit,na.rm=T),
  n_medium_ebanking = sum(t_medium_ebanking,na.rm=T),
  n_medium_vvm = sum(t_medium_vvm,na.rm=T)
),by=c("MDate","Asset_ID")]
n_bp <- unique(pos, by = c("MDate","Bp_ID","Asset_ID"))[
  , .(n_bps = .N), by = .(MDate, Asset_ID)]
pos_a[n_bp, n_bps := i.n_bps, on = .(MDate, Asset_ID)]
gc()
setorder(pos_a,MDate,Asset_ID)

write_parquet(pos_a,"../data/pos_a.parquet")

## ============================================================================
## advisor dataset

contacts <- read_parquet(paste0(emp_dir_raw,"T20_Kundenkontakte.parquet"))

contacts[, MDate := as.IDate(ceiling_date(
  as.Date(paste0(Period_ID, "01"), format = "%Y%m%d"), unit = "month") - 1)]
contacts[,ContactDate := as.IDate(Kontakt_DT)]

contacts[,init_by_client := ifelse(K_Aufnahme %in% c("Durch Kunde","Durch Bevollmächtigten"),1,0)]
contacts[,init_by_advisor := ifelse(K_Aufnahme %in% c("Durch Kundenberater","Zentral"),1,0)]
contacts[,`:=`(
  K_phone              = as.integer(K_Art %in% "Telefonkontakt"),
  K_meeting            = as.integer(K_Art %in% MEETING),
  K_mail               = as.integer(K_Art %in% "E-Mail, Brief, Fax")
)]

write_parquet(contacts,"../data/contacts.parquet")


contacts_m <- merge(contacts,bp_zeitreihe,by=c("MDate","Bp_ID"),all.x=T)

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


advisors_m <- merge(advisors,mitarbeiter,by.x=c("MDate","Hauptbetreuer_ID"),by.y=c("MDate","MA_ID"),all.x=T)


# advisors_m[,.N,by=MA_Kundensegment]


write_parquet(advisors_m,"../data/advisors_m.parquet")
