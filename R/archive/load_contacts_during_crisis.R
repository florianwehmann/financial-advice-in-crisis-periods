
## contacts during drawdown period

emp_dir_raw <- "C:/Users/FWehmann/Dropbox/Wehmann Household Finance Project/RawData_Stata/"


contacts <- read_parquet(paste0(emp_dir_raw,"T20_Kundenkontakte.parquet"))

contacts[,Date := lubridate::ceiling_date(as.Date(paste0(substr(Period_ID,1,4),"-",substr(Period_ID,5,6),"-01")))-1]
contacts[,ContactDate := as.Date(Kontakt_DT)]

# covid drawdown
covid_dd_start <- "2020-02-19"
covid_dd_end <- "2020-03-23"


bp_contacts_inv_covid <- unique(contacts[ContactDate %between% c(covid_dd_start,covid_dd_end) & K_Anlegen==1]$Bp_ID)
bp_contacts_init_a_inv_covid <- unique(contacts[ContactDate %between% c(covid_dd_start,covid_dd_end) & K_Anlegen==1 & K_Aufnahme %in% c("Durch Kundenberater","Zentral")]$Bp_ID)

rm(contacts,covid_dd_start,covid_dd_end)