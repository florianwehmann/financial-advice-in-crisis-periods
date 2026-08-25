## first analysis
library(duckdb)
library(data.table)
library(ggplot2)

# tidy up
rm(list=ls()); gc()


# dtp <- read_parquet("../data/pos_merged.parquet")
# dtb <- read_parquet("../data/boerse_merged.parquet")

# set up SQL connection
con <- dbConnect(duckdb::duckdb())

dbGetQuery(con, "DESCRIBE SELECT * FROM read_parquet('../data/pos_merged.parquet')")

# select pos columns
cols <- c("Bp_ID", "Person_ID", "Cont_ID", "Period_ID",
          "Asset_ID","Asset","Kontoprodukt","Depotprodukt","Instrumentengruppe","Fondsart","StruktProd_Klasse",
          "Asset_Waehrung","Bilanzposition","Referenzwaehrung","Positionswaehrung","Menge",
          "AUM_CHF","Vermoegen_CHF","Geschaeftsvolumen_CHF",
          "DA_Titelkursabweichung_CHF","DA_Devisenkursabweichung_CHF","DA_Mengenabweichung_CHF","DA_Delta_CHF","DA_Gesamtabweichung_CHF",
          "Anlagepaket","Anlegerprofil",
          "Geburtsjahr","Geschlecht","Hauptbankkunde")

col_sql <- paste0('"', cols, '"', collapse = ", ")

# load dt
dtp <- dbGetQuery(con, sprintf(
  "SELECT %s FROM read_parquet('../data/pos_merged.parquet')", col_sql
))
setDT(dtp)

# break SQL connection
dbDisconnect(con, shutdown = TRUE)


## consolidate pos

dtp[,.N,by=Instrumentengruppe]
dtp[,.N,by=Depotprodukt]
dtp[,.N,by=Anlagepaket]
# dtp[Instrumentengruppe=="Bar",.N,by=Kontoprodukt]

dtpc1 <- dtp[!(Instrumentengruppe %in% c("Bar","Nicht zugeteilt (Dummy)")),.(pf_value = mean(Vermoegen_CHF,na.rm=T)),by=c("Period_ID","Anlagepaket")]
dtpc1[,MDate := as.Date(paste0(substr(Period_ID,1,4),"-",substr(Period_ID,5,6),"-01"))]

dtpc2 <- dtp[Instrumentengruppe %in% c("Aktien","Fonds"),.(pf_value = mean(Vermoegen_CHF,na.rm=T)),by=c("Period_ID","Anlagepaket")]
dtpc2[,MDate := as.Date(paste0(substr(Period_ID,1,4),"-",substr(Period_ID,5,6),"-01"))]

unique(dtpc1$Anlagepaket)
unique(dtpc1$Period_ID)

dtpc1[, pf_value_idx := pf_value/pf_value[MDate=="2020-04-01"]*100,by=Anlagepaket]
dtpc2[, pf_value_idx := pf_value/pf_value[MDate=="2020-04-01"]*100,by=Anlagepaket]

# ggplot()+
#   geom_line(data=dtpc1[Anlagepaket=="DIRECT"],aes(x=MDate,y=pf_value,color="DIRECT"))+
#   geom_line(data=dtpc1[Anlagepaket=="COMFORT"],aes(x=MDate,y=pf_value,color="COMFORT"))

ggplot(dtpc2[!is.na(Anlagepaket) & Anlagepaket %in% c("DIRECT","COMFORT","CONSULT basic","CONSULT plus","CONSULT top") &
               MDate >= "2019-01-01"],
       aes(x=MDate,y=pf_value_idx,color=Anlagepaket,linetype=Anlagepaket))+geom_line(size=1)+theme_light()+
  theme(panel.grid.major = element_line(color = "grey20"), panel.grid.minor = element_line(color = "grey20"))+
  labs(x=NULL,y="Portfolio Value")


dtpc3 <- dtp[!(Instrumentengruppe %in% c("Bar","Nicht zugeteilt (Dummy)")) & 
               !(Depotprodukt %in% c("Personaldepot","Nicht zugeteilt (Dummy)","Hilfs-Container","Sparen 3-Konto"))
               ,.(pf_value = mean(Vermoegen_CHF,na.rm=T)),by=c("Period_ID","Depotprodukt")]
dtpc3[,MDate := as.Date(paste0(substr(Period_ID,1,4),"-",substr(Period_ID,5,6),"-01"))]
dtpc3[, pf_value_idx := pf_value/pf_value[MDate=="2020-04-01"]*100,by=Depotprodukt]

ggplot(dtpc3[MDate >= "2019-01-01"],
       aes(x=MDate,y=pf_value_idx,color=Depotprodukt))+geom_line(size=1)+theme_light()+
  theme(panel.grid.major = element_line(color = "grey20"), panel.grid.minor = element_line(color = "grey20"))+
  labs(x=NULL,y="Portfolio Value")




