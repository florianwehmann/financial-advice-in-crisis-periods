## merge pos
library(duckdb)
library(glue)
library(data.table)
library(arrow)

rm(list=ls()); gc()

emp_dir_raw <- "C:/Users/FWehmann/Dropbox/Wehmann Household Finance Project/RawData_Stata/"


# merge pos with bp-stammdaten und cons
# * pos and bp-stammdaten: by "Bp_ID"
# * pos and cons: by c("Bp_ID","Cont_ID")


f_pos  <- file.path(emp_dir_raw, "T30_Pos_fact.parquet")
f_bp   <- file.path(emp_dir_raw, "T01_Bp_Snapshot.parquet")
f_cont <- file.path(emp_dir_raw, "T05_Cont_Snapshot.parquet")
f_out  <- file.path("../data/pos_merged.parquet")

stopifnot(all(file.exists(f_pos, f_bp, f_cont)))

con <- dbConnect(duckdb::duckdb())
dbExecute(con, "SET memory_limit = '16GB'")
dbExecute(con, "SET threads = 8")
dbExecute(con, glue("SET temp_directory = '{file.path(emp_dir_raw, 'duckdb_tmp')}'"))

sql <- glue_sql("
  COPY (
    SELECT *
    FROM read_parquet({f_pos})           AS p
    LEFT JOIN read_parquet({f_bp})       AS b USING (\"Bp_ID\")
    LEFT JOIN read_parquet({f_cont})     AS c USING (\"Bp_ID\", \"Cont_ID\")
  ) TO {f_out} (FORMAT PARQUET, COMPRESSION ZSTD)
", .con = con)

dbExecute(con, sql)
dbDisconnect(con, shutdown = TRUE)

gc()


## ============================================================================
## select sub sample
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

# pos <- setDT(dbGetQuery(con, sprintf(
#   "SELECT %s FROM read_parquet('%s')
#    WHERE Instrumentengruppe NOT IN (%s)",
#   cols_sql, path, excl_sql
# )))

pos <- setDT(dbGetQuery(con, sprintf(
  "SELECT %s FROM read_parquet('%s')
   WHERE Instrumentengruppe NOT IN (%s)",
  cols_sql, path, excl_sql
)))

gc()

# exclude some more columns
pos[,StruktProd_Klasse := NULL]
pos[,Anlegerprofil := NULL]

# Create MDate
pos[, MDate := as.IDate(lubridate::ceiling_date(as.Date(paste0(substr(Period_ID, 1, 4), "-", substr(Period_ID, 5, 6), "-01")),"months")-1)]
setcolorder(pos,"MDate")
setorder(pos,MDate,Bp_ID,Asset_ID)

write_parquet(pos,paste0("../data/pos_m1.parquet"))
