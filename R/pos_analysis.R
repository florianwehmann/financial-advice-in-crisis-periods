library(duckdb)
library(data.table)

rm(list=ls()); gc()

con <- dbConnect(duckdb())

path <- "../data/pos_merged.parquet"

dbGetQuery(con, sprintf("DESCRIBE SELECT * FROM read_parquet('%s')", path))


pos_cols <- c("Bp_ID","Person_ID","Cont_ID","Period_ID","Asset_ID","Asset","Kontoprodukt","Depotprodukt","Bilanzposition",
              "Instrumentengruppe","Fondsart","StruktProd_Klasse","Asset_Waehrung","Referenzwaehrung","Positionswaehrung",
              "Menge","Vermoegen_CHF","Geschaeftsvolumen_CHF",
              "DA_Titelkursabweichung_CHF","DA_Devisenkursabweichung_CHF","DA_Mengenabweichung_CHF","DA_Delta_CHF","DA_Gesamtabweichung_CHF",
              "Geburtsjahr","Hauptbankkunde","Geschlecht","Anlagepaket","Anlegerprofil")

cols_sql <- paste(sprintf('"%s"', pos_cols), collapse = ", ")

exclude <- c("Bar","Nicht zugeteilt (Dummy)","Money Market Deposit","Swaps","Limite","Waehrung")

excl_sql <- paste(dbQuoteString(con, exclude), collapse = ", ")

pos <- dbGetQuery(con, sprintf(
  "SELECT %s FROM read_parquet('%s')
   WHERE Instrumentengruppe NOT IN (%s)",
  cols_sql, path, excl_sql
)) %>% setDT()

gc()

dbDisconnect(con, shutdown = TRUE)

pos_a <- pos[Instrumentengruppe %in% c("Aktien","Fonds"),lapply(.SD,sum,na.rm=T),by=Period_ID,.SDcols=c("Geschaeftsvolumen_CHF","DA_Titelkursabweichung_CHF","DA_Gesamtabweichung_CHF")]
pos_a[,date := lubridate::ceiling_date(as.Date(paste0(substr(Period_ID,1,4),"-",substr(Period_ID,5,6),"-01")),"months")-1]
setorder(pos_a,date)
pos_a[,ret := DA_Titelkursabweichung_CHF / (Geschaeftsvolumen_CHF - DA_Gesamtabweichung_CHF)]
ggplot()+
  geom_line(data=pos_a[date>="2020-01-01"],aes(x=date,y=cumprod(1+ret)/(1+ret[1])))+
  geom_line(data=smi[Date>="2020-01-01"],aes(x=Date,y=cumprod(1+smi_ret)/(1+smi_ret[1])))



# ==============================================================================
# TODO:
#
# merge with borse_advise
# * on "Bp_ID", "Asset_ID", and "MDate"
#
# (only buys and sells should be advised then)

dtb <- read_parquet("../data/boerse_merged.parquet") %>% setDT()
dtb <- dtb[DDate >= "2011-01-01" & DDate <= "2024-12-31"]

# collapse to Bp_ID x Asset_ID x MDate: if traded multiple times in the same
# month, keep an advised trade over a non-advised one, then just the first
setorder(dtb, Bp_ID, Asset_ID, MDate, -advised, DDate)
dtb <- unique(dtb, by = c("Bp_ID", "Asset_ID", "MDate"))

# merge into pos: only the dtb columns not already in pos
pos[, MDate := as.Date(paste0(substr(Period_ID, 1, 4), "-", substr(Period_ID, 5, 6), "-01"))]
gc()
dtb_cols <- setdiff(names(dtb), names(pos))
pos <- merge(pos, dtb[, c("MDate", "Bp_ID", "Asset_ID", dtb_cols), with = FALSE],
             by = c("MDate", "Bp_ID", "Asset_ID"), all.x = TRUE)
gc()
write_parquet(pos,"../data/pos_b_merged.parquet")



# ==============================================================================








