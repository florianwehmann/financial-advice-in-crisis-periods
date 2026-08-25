## =============================================================================
## 02e_cash.R -- deposit balances
##
## Source: data/pos_merged.parquet (72.75 m rows) -- the UNFILTERED position
## join written by R/1_merge_pos.R, before the `exclude` list is applied. Every
## downstream file (pos_m1 -> pos_mb -> pos_m, and pos_full) had 'Bar',
## 'Money Market Deposit', 'Waehrung', 'Limite' and the dummy group stripped out,
## which is why cash was invisible until now.
##
## What this adds, and it is the single most important addition to the project:
##   32.8 m 'Bar' rows over 142'479 clients (Privatkonto, Sparkonto, Sparen 3,
##   Kontokorrent, Depotkonto, ...) plus 0.18 m 'Money Market Deposit' rows
##   (Festgeld, Callgeld, Termingeld, Treuhand). Bilanzposition confirms these
##   are customer deposits: "Verpflichtungen Kunden Anlageform / Spareinlagen /
##   3. Saeule / Sicht".
##
##   => "sold and sat in cash" and "left the bank" become SEPARABLE, and
##      risky_share becomes a real weight instead of a near-constant 1.0.
##
## Pillar 3a (Sparen 3 / "Verpflichtungen Kunden 3. Saeule") is kept apart:
## it is locked retirement money and is not dry powder for market timing.
## Rental-deposit accounts are treated the same way.
##
## Output: results/cache/cash_m.parquet (Bp_ID x MDate)
## =============================================================================

source("00_setup.R")
suppressPackageStartupMessages({library(duckdb); library(DBI)})
log_init("02e_cash")

POS_MERGED <- file.path(DATA, "pos_merged.parquet")
if (!file.exists(POS_MERGED))
  stop("not found: ", POS_MERGED,
       "\n  It is written by R/1_merge_pos.R (the COPY step, before the",
       "\n  `exclude` filter). That step must be run with the filter commented",
       "\n  out, otherwise the cash rows are gone.", call. = FALSE)

sink(file.path(RESULTS, "02e_cash.txt"), split = TRUE)
on.exit(sink(), add = TRUE)

## only the clients that can ever enter the estimation sample
bp <- unique(setDT(read_parquet(POS_M_FILE, col_select = "Bp_ID"))$Bp_ID)
cat("clients requested:", length(bp), "\n")

con <- dbConnect(duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
duck_setup(con)
dbExecute(con, sprintf("SET temp_directory = '%s'", file.path(CACHE, "duckdb_tmp")))
duckdb_register(con, "bp", data.table(Bp_ID = bp))

## locked money: pillar 3a and rental deposits cannot be moved into the market
LOCKED <- "(p.Bilanzposition = 'Verpflichtungen Kunden 3. Säule'
            OR p.Kontoprodukt IN ('Sparen 3-Konto','Mieterkautionssparkonto',
                                  'Grabunterhaltskonto'))"

cash <- setDT(dbGetQuery(con, sprintf("
SELECT p.Bp_ID,
       CAST(p.Period_ID AS VARCHAR)                        AS period,
       sum(CASE WHEN %s THEN 0 ELSE p.Vermoegen_CHF END)   AS cash_free,
       sum(CASE WHEN %s THEN p.Vermoegen_CHF ELSE 0 END)   AS cash_locked,
       sum(CASE WHEN p.Instrumentengruppe = 'Money Market Deposit'
                THEN p.Vermoegen_CHF ELSE 0 END)           AS cash_time,
       count(*)                                            AS n_cash_acc
FROM read_parquet('%s') p
JOIN bp ON p.Bp_ID = bp.Bp_ID
WHERE p.Instrumentengruppe IN ('Bar','Money Market Deposit')
GROUP BY 1,2", LOCKED, LOCKED, POS_MERGED)))

cash[, MDate := eom(as.Date(paste0(substr(period, 1, 4), "-", substr(period, 5, 6), "-01")))]
cash[, period := NULL]
cash[, cash_total := cash_free + cash_locked]
setcolorder(cash, c("Bp_ID", "MDate"))
setorder(cash, Bp_ID, MDate)
log_step("cash panel (client x month)", cash)

cat("\n== deposit balances, client-months with a cash row ==\n")
print(round(quantile(cash$cash_free,  c(.1,.25,.5,.75,.9,.99), na.rm = TRUE)))
cat("mean cash_free:", round(mean(cash$cash_free)), "  mean locked:",
    round(mean(cash$cash_locked)), "\n")

cat("\n== coverage by year (clients in the pos_m universe) ==\n")
print(cash[, .(client_months = .N, clients = uniqueN(Bp_ID),
                bn_free = round(sum(cash_free) / 1e9, 1)),
           by = .(y = year(MDate))][order(y)])

## ---------------------------------------------------------------------------
## the decisive check: when securities go to zero, is the money in cash?
## ---------------------------------------------------------------------------
pf <- load_dt("portfolio_m")[, .(Bp_ID, MDate, sec_value = tot_value)]
j  <- merge(pf, cash, by = c("Bp_ID","MDate"), all = TRUE)
j[is.na(sec_value), sec_value := 0]
for (v in c("cash_free","cash_locked","cash_time","cash_total")) j[is.na(get(v)), (v) := 0]

cat("\n== securities vs. cash, all client-months in scope ==\n")
print(j[, .N, by = .(has_securities = sec_value > 0, has_cash = cash_total > 0)])
cat("\nshare of client-months WITHOUT securities that still have a cash account: ",
    round(j[sec_value == 0, mean(cash_total > 0)], 4), "\n", sep = "")
cat("=> that is the group the pipeline previously had to call 'exited'.\n")

cat("\n== the real risky share, now that the denominator includes cash ==\n")
j[, wealth_tot := sec_value + cash_free]
j[, risky_share_c := fifelse(wealth_tot >= P$min_bom_value, sec_value / wealth_tot, NA_real_)]
print(round(quantile(j$risky_share_c, c(.01,.1,.25,.5,.75,.9,.99), na.rm = TRUE), 3))
cat("(pos_full alone gave a median of 1.000 because it had no cash to divide by)\n")
cat("\nmean risky share by year:\n")
print(j[, .(n = .N, risky = round(mean(risky_share_c, na.rm = TRUE), 3),
            med_cash = round(median(cash_free))), by = .(y = year(MDate))][order(y)])

save_dt(cash, "cash_m")
log_step("cash_m written")
