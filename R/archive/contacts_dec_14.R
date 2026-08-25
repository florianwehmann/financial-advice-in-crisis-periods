# contacts december 2014


emp_dir_prep <- "C:/Users/FWehmann/Dropbox/Wehmann Household Finance Project/DataPrepared/"
emp_dir_prep_adv <- paste0(emp_dir_prep,"/Advice/")

boerse_file <- "T40_Bkg_Boerse_prepared_advice.rds"


borse <- readRDS(paste0(emp_dir_prep_adv,boerse_file))
asset_s <- read_stata(paste0(emp_dir_prep,"T31_Asset_Stammdaten.dta")) %>% setDT()

dt <- merge(borse,asset_s[MostRecent==1],by="Asset_ID",all.x=T)

dt[, MDate := as.Date(ISOdate(1960 + MDate %/% 12, MDate %% 12 + 1, 1))]


dt[MDate == as.Date("2014-12-01")] %>% View()
