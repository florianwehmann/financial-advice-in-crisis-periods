## read colnames of .dta files
rm(list=ls()); gc()

source("fx_converter.R")

emp_dir_raw <- "C:/Users/FWehmann/Dropbox/Wehmann Household Finance Project/RawData_Stata/"
emp_dir_prep <- "C:/Users/FWehmann/Dropbox/Wehmann Household Finance Project/DataPrepared/"
emp_dir_prep_adv <- paste0(emp_dir_prep,"/Advice/")

files <- list.files(emp_dir_raw, pattern = "\\.dta$", full.names = F)
files

file <- "T30_Pos_fact.dta"

file <- files[1]

f <- paste0(emp_dir_raw,file)

d <- read_dta(f) %>% setDT()
d <- d[Bp_ID == 303511]
d <- d[Person_ID == 10548244]

for (nm in names(d)[23:length(names(d))]) {
  d[,(nm) := as_factor(get(nm))]
}

d_finfox <- d[Medium == "FINFOX"]

d[Bp_ID %in% unique(d_finfox$Bp_ID),.N,by=Anlagepaket]

d_vmandat <- d[Medium == "Verwaltungsmandat"]
unique(d_vmandat$Bp_ID)
d[Bp_ID %in% unique(d_vmandat$Bp_ID),.N,by=Anlagepaket]

d[,.N,by=Anlagepaket]

bpid_direct <- d[Anlagepaket=="DIRECT"]$Bp_ID

borse_direct <- d[Bp_ID %in% bpid_direct]
borse_direct[,.N,by=Medium]

# für börse
d[, ts    := as.POSIXct(Ausfuehrungszeit_DT, format = "%b %d %Y %I:%M%p", tz = "UTC")]
d[, date := as.IDate(ts)]
d[,Netto_in_CHF := to_chf(Netto_in_Konto_Waehrung,Kontowaehrung,date,"daily")]


fwrite(d,paste0("../data/showcase_303511/",sub(".dta","_303511.csv",file)))

# arrow::write_parquet(d, paste0(emp_dir_raw,"T01_Bp_Snapshot.parquet"))
# arrow::write_parquet(d, paste0(emp_dir_raw,"T05_Cont_Snapshot.parquet"))
arrow::write_parquet(d, paste0(emp_dir_raw,"T30_Pos_fact.parquet"))

for (nm in names(d)[c(6:8,11:14,17:26)]) {
  d[,(nm) := as_factor(get(nm))]
}

d <- read_dta(f,n_max=0) %>% names()
d

d <- read_dta(paste0(emp_dir_raw,file),n_max=5) %>% setDT()
d

d[Pos_ID == 6515022] %>% View()
d[Bp_ID == 334164] %>% View()

dc <- read_dta(paste0(emp_dir_raw,file), col_select = 1)

lab_depot <- attr(read_dta(f, n_max = 0, col_select = "Depotprodukt")[[1]], "labels")
lab_konto <- attr(read_dta(f, n_max = 0, col_select = "Kontoprodukt")[[1]], "labels")
lab_instr <- attr(read_dta(f, n_max = 0, col_select = "Instrumentengruppe")[[1]], "labels")
lab_fonds <- attr(read_dta(f, n_max = 0, col_select = "Fondsart")[[1]], "labels")
lab_assetcur <- attr(read_dta(f, n_max = 0, col_select = "Asset_Waehrung")[[1]], "labels")
lab_bilanzgl2 <- attr(read_dta(f, n_max = 0, col_select = "BilanzpositionGL2")[[1]], "labels")
lab_bilanzgl1 <- attr(read_dta(f, n_max = 0, col_select = "BilanzpositionGL1")[[1]], "labels")
lab_bilanz <- attr(read_dta(f, n_max = 0, col_select = "Bilanzposition")[[1]], "labels")
lab_kontogl2 <- attr(read_dta(f, n_max = 0, col_select = "KontoproduktGL2")[[1]], "labels")
lab_kontogl1 <- attr(read_dta(f, n_max = 0, col_select = "KontoproduktGL1")[[1]], "labels")
lab_refcur <- attr(read_dta(f, n_max = 0, col_select = "Referenzwaehrung")[[1]], "labels")
lab_poscur <- attr(read_dta(f, n_max = 0, col_select = "Positionswaehrung")[[1]], "labels")
lab_limit <- attr(read_dta(f, n_max = 0, col_select = "Limitenart")[[1]], "labels")
lab_ctryem <- attr(read_dta(f, n_max = 0, col_select = "Land_Emittent")[[1]], "labels")
lab_strprod <- attr(read_dta(f, n_max = 0, col_select = "StruktProd_Klasse")[[1]], "labels")
lab_asset <- attr(read_dta(f, n_max = 0, col_select = "Asset")[[1]], "labels")

d_col <- read_dta(paste0(emp_dir_raw,file), col_select = 1)
d_col_period <- read_dta(paste0(emp_dir_raw,file), col_select = 3)


# pos file

library(duckdb)
con <- dbConnect(duckdb())

fpos <- paste0(emp_dir_raw,"T30_Pos_fact.parquet")

d <- dbGetQuery(con,
           "SELECT * FROM read_parquet(?) WHERE Kontoprodukt = ?",
           params = list(fpos, 7)) %>% setDT()

d <- dbGetQuery(con,
                "SELECT * FROM read_parquet(?) WHERE Instrumentengruppe = ?",
                params = list(fpos, 1)) %>% setDT()

d <- dbGetQuery(con,
                "SELECT * FROM read_parquet(?) WHERE Bp_ID = ?",
                params = list(fpos, 13732497)) %>% setDT()

d[, Depotprodukt := factor(Depotprodukt, levels = as.vector(lab_depot), labels = names(lab_depot))]
d[, Kontoprodukt := factor(Kontoprodukt, levels = as.vector(lab_konto), labels = names(lab_konto))]
d[, Instrumentengruppe := factor(Instrumentengruppe, levels = as.vector(lab_instr), labels = names(lab_instr))]
d[, Fondsart := factor(Fondsart, levels = as.vector(lab_fonds), labels = names(lab_fonds))]
d[, Asset_Waehrung := factor(Asset_Waehrung, levels = as.vector(lab_assetcur), labels = names(lab_assetcur))]
d[, BilanzpositionGL2 := factor(BilanzpositionGL2, levels = as.vector(lab_bilanzgl2), labels = names(lab_bilanzgl2))]
d[, BilanzpositionGL1 := factor(BilanzpositionGL1, levels = as.vector(lab_bilanzgl1), labels = names(lab_bilanzgl1))]
d[, Bilanzposition := factor(Bilanzposition, levels = as.vector(lab_bilanz), labels = names(lab_bilanz))]
d[, KontoproduktGL2 := factor(KontoproduktGL2, levels = as.vector(lab_kontogl2), labels = names(lab_kontogl2))]
d[, KontoproduktGL1 := factor(KontoproduktGL1, levels = as.vector(lab_kontogl1), labels = names(lab_kontogl1))]
d[, Referenzwaehrung := factor(Referenzwaehrung, levels = as.vector(lab_refcur), labels = names(lab_refcur))]
d[, Positionswaehrung := factor(Positionswaehrung, levels = as.vector(lab_poscur), labels = names(lab_poscur))]
d[, Limitenart := factor(Limitenart, levels = as.vector(lab_limit), labels = names(lab_limit))]
d[, Land_Emittent := factor(Land_Emittent, levels = as.vector(lab_ctryem), labels = names(lab_ctryem))]
d[, StruktProd_Klasse := factor(StruktProd_Klasse, levels = as.vector(lab_strprod), labels = names(lab_strprod))]
d[, Asset := factor(Asset, levels = as.vector(lab_asset), labels = names(lab_asset))]

fwrite(d,"../data/pos_303511.csv")
fwrite(d,paste0("../data/showcase_303511/",sub(".dta","_303511.csv",file)))

uniqueN(d$Bp_ID)

d_c <- d[,.(vol_sum=sum(Geschaeftsvolumen_CHF,na.rm=T),
            vol_mean=mean(Geschaeftsvolumen_CHF,na.rm=T),
            vol_median=median(Geschaeftsvolumen_CHF,na.rm=T)),by="Period_ID"]
d_c[,Date := as.Date(paste0(substr(Period_ID,1,4),"-",substr(Period_ID,5,6),"-01"))]


ggplot(d_c,aes(x=Date))+
  geom_line(aes(y=vol_mean,color="mean"))+
  geom_line(aes(y=vol_median,color="median"))


ggplot(d_c,aes(x=Date,y=vol_sum))+geom_line()


# covid credits
covid_credits <- dbGetQuery(con,
           "SELECT * FROM read_parquet(?) WHERE Kontoprodukt = ?",
           params = list(fpos, 5)) %>% setDT()
covid_credits_c <- covid_credits[,.(vol=sum(Geschaeftsvolumen_CHF,na.rm=T)),by="Period_ID"]
covid_credits_c[,Date := as.Date(paste0(substr(Period_ID,1,4),"-",substr(Period_ID,5,6),"-01"))]

ggplot(covid_credits_c,aes(x=Date,y=vol))+geom_line()



## prepared advise file

borse_adv <- read_dta(paste0(emp_dir_prep_adv,"T40_Bkg_Boerse_prepared_advice.dta")) %>% setDT()
for (nm in names(borse_adv)[c(5,8,9,17,18)]) {
  borse_adv[,(nm) := as_factor(get(nm))]
}
borse_adv <- borse_adv[Person_ID == 10339001]
borse_adv[, MDate := as.Date(ISOdate(1960 + MDate %/% 12, MDate %% 12 + 1, 1))]
fwrite(borse_adv,paste0("../data/showcase_303511/",sub(".dta","_303511.csv","T40_Bkg_Boerse_prepared_advice.dta")))


# ------------------------------------------------------------------------------
## kundenevents

file <- "T21_Kundenevents.dta"

events <- read_dta(paste0(emp_dir_raw,file)) %>% setDT()

events[,Event_DT := as_factor(Event_DT)]
events[,Event_Kategorie := as_factor(Event_Kategorie)]
events[,Einladung_Status := as_factor(Einladung_Status)]

events_c <- events[Einladung_Status=="teilgenommen",.N,by=Event_DT]
events_c[,Event_DT := as.Date(Event_DT)]
setorder(events_c,Event_DT)

ggplot(events_c,aes(x=Event_DT,y=N))+geom_col()
