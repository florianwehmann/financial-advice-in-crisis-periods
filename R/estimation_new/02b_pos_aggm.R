# load packages
library(data.table)
library(haven)
library(arrow)
library(lubridate)
library(dplyr)
library(duckdb)


rm(list=ls()); gc()

emp_dir_raw <- "C:/Users/FWehmann/Dropbox/Wehmann Household Finance Project/RawData_Stata/"


# Mitarbeiter
adv_cols <- c("MA_ID","MDate","MA_Status","MA_ist_Team",
              "MA_Eintrittsjahr","MA_Austrittsjahr","MA_Arbeitspensum",
              "MA_Anzahl_Kunden","MA_Team_ID","MA_Geschlecht","MA_Altersklasse",
              "MA_Rang","MA_Kundensegment")

advisor <- read_parquet(paste0(emp_dir_raw,"T10_Mitarbeiter_Merkmale.parquet"),
                        col_select = adv_cols)
setnames(advisor,adv_cols,
         c("advisor_id","MDate","advisor_status","adv_team","adv_eintritt","adv_austritt",
           "adv_pensum","adv_nclients","adv_team_id","adv_sex","adv_age_group","adv_rank","adv_segment"))


# Bp Zeitreihe
bp_cols <- c("MDate","Bp_ID","EBanking_YN","EVV",
             "Profit_Fonds","Profit_Depot","Profit_Wertschriften","Profit_Vermoegensverwaltung",
             "Profit_SonstigerErfolg","Hypothekarzinsen",
             "Nationalitaet","Wohnland","PLZ_des_Wohnortes","Zivilstand")

bp_zeitreihe <- setDT(arrow::read_parquet(
  paste0(emp_dir_raw, "T02_Bp_Zeitreihe.parquet"),
  col_select = all_of(bp_cols)
))
setnames(bp_zeitreihe,bp_cols,
         c("MDate","Bp_ID","ebanking","evv",
           "profit_funds","profit_depot","profit_securities","profit_vvw","profit_other",
           "hypo_interest","nationality","country","zipcode","civil_status"))

bp_zeitreihe[,MDate := as.IDate(MDate)]

# load pos
pos <- read_parquet(paste0("../../data/pos_aggm_sql.parquet"))

pos[, MDate := as.IDate(lubridate::ceiling_date(as.Date(paste0(Period_ID, "01"), "%Y%m%d"), "month") - 1)]

# merge pos with bp zeitreihe
pos <- merge(pos,bp_zeitreihe,by=c("MDate","Bp_ID"),all.x=T)

rm(bp_zeitreihe);gc()


# merge pos with advisor
pos <- merge(pos,advisor,by=c("advisor_id","MDate"),all.x=T)


write_parquet(pos,"../../data/pos_aggm.parquet")
