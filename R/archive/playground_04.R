
source("fx_converter.R")

emp_dir_prep <- "C:/Users/FWehmann/Dropbox/Wehmann Household Finance Project/DataPrepared/"
emp_dir_prep_adv <- paste0(emp_dir_prep,"/Advice/")
emp_dir <- "C:/Users/FWehmann/Dropbox/Financial Advice in Crisis Periods/Empirical Analysis/"

borse <- read_stata(paste0(emp_dir_prep,"T40_Bkg_Boerse_prepared.dta"))

borse2 <- readRDS(paste0(emp_dir,"Source/Original Data/T40_Bkg_Boerse.rds"))

borse_adv <- readRDS(paste0(emp_dir_prep_adv,"T40_Bkg_Boerse_prepared_advice.rds"))


asset_s <- read_stata(paste0(emp_dir_prep,"T31_Asset_Stammdaten.dta")) %>% setDT()

setDT(borse)


dt <- merge(borse,asset_s[MostRecent==1],by="Asset_ID",all.x=T)
dt[,Asset := as_factor(Asset)]
dt[,Order_Type := as_factor(Order_Type)]
dt[,Order_Typisierung := as_factor(Order_Typisierung)]
dt[,Boersenplatz := as_factor(Boersenplatz)]
dt[,Medium := as_factor(Medium)]
dt[,Kontowaehrung := as_factor(Kontowaehrung)]
dt[,Handelswaehrung := as_factor(Handelswaehrung)]
dt[,Order_Type_simple := as_factor(Order_Type_simple)]
dt[,Instrumentengruppe := as_factor(Instrumentengruppe)]
dt[,Fondsart := as_factor(Fondsart)]
dt[,Land_Emittent := as_factor(Land_Emittent)]
dt[,Asset_Waehrung := as_factor(Asset_Waehrung)]


dt[,Netto_in_CHF := to_chf(Netto_in_Konto_Waehrung,Kontowaehrung,DDate_Order,"daily")]


dtd <- dt[,.(N=.N,
            wert = sum(Netto_in_CHF,na.rm=T)),by=DDate_Order]

# ggplot(dtd[DDate_Order>"2021-06-30"&DDate_Order<="2023-06-30"],aes(x=DDate_Order,y=N))+geom_col()
ggplot(dtd,aes(x=DDate_Order,y=wert))+geom_line()

dtdx <- dt[!(Asset_ID %in% c(28077882,31794919)),.(N=.N,
             wert = sum(Netto_in_CHF,na.rm=T)),by=DDate_Order]
dtdx1 <- dt[Asset_ID == 28077882,.(N=.N,
                                                   wert = sum(Netto_in_CHF,na.rm=T)),by=DDate_Order]
dtdx2 <- dt[Asset_ID == 31794919,.(N=.N,
                                                   wert = sum(Netto_in_CHF,na.rm=T)),by=DDate_Order]
ggplot()+
  geom_line(data=dtdx,aes(x=DDate_Order,y=wert))+
  geom_line(data=dtdx1,aes(x=DDate_Order,y=wert),color="tomato2")+
  geom_line(data=dtdx2,aes(x=DDate_Order,y=wert),color="skyblue")



dtd[wert %in% tail(sort(dtd$wert,decreasing=T))]
dtd[wert %in% head(sort(dtd$wert,decreasing=T))]

dt[DDate_Order == "2023-03-14"] %>% View()


dt[DDate_Order == "2022-01-24"][,.N, by=Asset_ID][order(-N)]  ## 28077882 -> IE00BF29SZ08
dt[DDate_Order == "2023-03-14"][,.N, by=Asset_ID][order(-N)]  ## 31794919 -> LU2592289560
dt[DDate_Order == "2023-03-20"][,.N, by=Asset_ID][order(-N)]  ## 28077882 -> IE00BF29SZ08

asset_s[Asset_ID == 31794919]
asset_s[Asset_ID == 28077882]

borse_adv[, MDate := as.Date(ISOdate(1960 + MDate %/% 12, MDate %% 12 + 1, 1))]
borse_adv[Asset_ID == 28077882] %>% View()
borse_adv[Asset_ID == 28077882 & Trade_Advised ==1] %>% View()
borse_adv[Asset_ID == 28077882 & Trade_Advised ==1 & MDate == "2023-03-01"] %>% View()
borse_adv[Asset_ID == 28077882 & Trade_Advised ==1 & MDate == "2022-01-01"] %>% View()



dtd_sub <- dt[Person_ID %in% borse_adv[Asset_ID == 28077882 & Trade_Advised ==0 & MDate == "2022-01-01"]$Person_ID,.(N=.N,
                                wert = sum(Bruttowert,na.rm=T)),by=DDate_Order]

ggplot(dtd_sub,aes(x=DDate_Order,y=N))+geom_col()
ggplot(dtd_sub,aes(x=DDate_Order,y=wert))+geom_line()




dt[Asset_ID == 28077882] %>% View()
