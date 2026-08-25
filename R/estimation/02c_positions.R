## =============================================================================
## 02c_positions.R -- portfolio composition and an asset price panel
##
## Source: data/pos_full.parquet (36.0 m rows, client x month x POSITION, all
## instrument groups). Aggregated with duckdb so the file is never held in R.
##
## Two things this unlocks that pos_m could not support:
##   1. a real EQUITY / RISKY SHARE, so de-risking can be measured on portfolio
##      weights instead of only on net flows;
##   2. an implied CHF unit price per Asset_ID x month
##      (sum(Vermoegen_CHF) / sum(Menge) over holders), which is what 04b needs
##      to build the design's actual cf_gap: freeze the holdings at the last
##      pre-drawdown month end and drift them with observed security prices.
##
## WHAT IT STILL DOES NOT CONTAIN -- checked, not assumed:
##   There is NO cash or deposit balance in pos_full. The instrument groups are
##   Aktien, Fonds, Obligationen, Strukturierte, Optionen, Warrants, Futures,
##   Anrechte, Ansprueche, Metall, Kryptowaehrung, Waehrung (25 rows), Kredit.
##   Kontoprodukt contains only mortgage/loan products plus Kassenobligation.
##   So "sold and sat in cash" is still not observable; the closest visible
##   substitute is a rotation into bonds / Kassenobligationen, which IS now
##   measurable. Kredit rows carry Vermoegen_CHF = 0 throughout, so there is no
##   liability side either.
##
## Output: results/cache/portfolio_m.parquet  (Bp_ID x MDate composition)
##         results/cache/prices_m.parquet     (Asset_ID x MDate CHF unit price)
## =============================================================================

source("00_setup.R")
suppressPackageStartupMessages({library(duckdb); library(DBI)})
log_init("02c_positions")

POS_FULL <- file.path(DATA, "pos_full.parquet")
if (!file.exists(POS_FULL)) stop("not found: ", POS_FULL, call. = FALSE)

con <- dbConnect(duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
duck_setup(con)

sink(file.path(RESULTS, "02c_positions.txt"), split = TRUE)
on.exit(sink(), add = TRUE)

## ---------------------------------------------------------------------------
## asset class mapping
##   equity : Aktien + equity funds (Shares / Exchange Traded / Index)
##   bond   : Obligationen (incl. Kassenobligationen) + bond funds
##   mixed  : allocation / strategy / fund-of-funds / other funds
##   reales : real estate funds
##   deriv  : structured products, options, warrants, futures
##   alt    : metals, crypto, claims, rights
##   Kredit is dropped: Vermoegen_CHF is 0 for every one of its 4.4 m rows.
## ---------------------------------------------------------------------------
CLS <- "
  CASE
    WHEN Instrumentengruppe = 'Aktien' THEN 'equity'
    WHEN Instrumentengruppe = 'Fonds' AND Fondsart IN
         ('Fund - Shares (09)','Fund - Exchange Traded (03)','Fund - Index (04)')
         THEN 'equity'
    WHEN Instrumentengruppe = 'Fonds' AND Fondsart = 'Fund - Bond (12)' THEN 'bond'
    WHEN Instrumentengruppe = 'Fonds' AND Fondsart = 'Fund - Real Estate (01)' THEN 'reales'
    WHEN Instrumentengruppe = 'Fonds' THEN 'mixed'
    WHEN Instrumentengruppe = 'Obligationen' THEN 'bond'
    WHEN Instrumentengruppe IN ('Strukturierte Prod./Zertifikate','Optionen',
                                'Warrants','Futures') THEN 'deriv'
    WHEN Instrumentengruppe IN ('Metall','Kryptowaehrung','Kryptowährung',
                                'Ansprueche','Ansprüche','Anrechte','Waehrung',
                                'Währung') THEN 'alt'
    ELSE 'other'
  END"

cat("== value by asset class and year (CHF bn, summed over monthly snapshots) ==\n")
print(setDT(dbGetQuery(con, sprintf(
  "SELECT year(MDate) y, %s cls, round(sum(Vermoegen_CHF)/1e9,2) bn
   FROM read_parquet('%s') WHERE Instrumentengruppe <> 'Kredit'
   GROUP BY 1,2 ORDER BY 1,2", CLS, POS_FULL))))

## ---------------------------------------------------------------------------
## 0. PIN EACH ASSET TO ONE CLASS FOR ALL TIME
##
## The CASE above is evaluated per ROW, per MONTH, so a relabelling in the
## bank's asset master reads as a portfolio change for every client holding
## that security. That is not hypothetical: on 2021-04-30 the bank re-coded
## Fondsart on 118 of 3'121 fund Asset_IDs, 44 of them into
## 'Fund - Allocation (07)', which this mapping sends to 'mixed'. Aggregate
## equity VALUE did not move (5.085 -> 5.089 bn) but eq_share is an
## equal-weighted mean of CLIENT-level shares, so it fell 4.7pp in one month:
## 1'839 clients lost more than 10 share points, 85.5% of them without trading
## a single position, and 1'031 went from a positive equity share to exactly
## zero. It put a visible step in the ep5 equity-share event study between
## rel_month -9 and -8 and is most of why that outcome failed parallel trends.
##
## Each Asset_ID therefore gets ONE class: the one it carries in the most
## MONTHS (not the most rows -- a fund held by thousands of clients for one
## month must not outvote five years of history), ties broken by the most
## recent month. See results/NOTES.txt section 6c.
## ---------------------------------------------------------------------------
dbExecute(con, sprintf("
CREATE TEMP TABLE asset_cls AS
WITH r AS (
  SELECT Asset_ID, %s AS cls, MDate
  FROM read_parquet('%s') WHERE Instrumentengruppe <> 'Kredit'
), v AS (
  SELECT Asset_ID, cls,
         count(DISTINCT MDate) AS n_months,
         max(MDate)            AS last_seen
  FROM r GROUP BY 1,2
)
SELECT Asset_ID, cls, n_months FROM (
  SELECT *, row_number() OVER (PARTITION BY Asset_ID
                               ORDER BY n_months DESC, last_seen DESC, cls) AS rk
  FROM v
) WHERE rk = 1", CLS, POS_FULL))

cat("\n== time-invariant asset classification ==\n")
cat("assets pinned:", dbGetQuery(con, "SELECT count(*) n FROM asset_cls")$n, "\n")
amb <- setDT(dbGetQuery(con, sprintf("
WITH r AS (SELECT DISTINCT Asset_ID, %s AS cls FROM read_parquet('%s')
           WHERE Instrumentengruppe <> 'Kredit')
SELECT n_cls, count(*) AS assets FROM (
  SELECT Asset_ID, count(*) AS n_cls FROM r GROUP BY 1
) GROUP BY 1 ORDER BY 1", CLS, POS_FULL)))
cat("assets by number of DISTINCT classes seen over the sample:\n"); print(amb)
cat("(everything above n_cls = 1 is a relabelling that the per-month mapping\n")
cat(" would have read as a portfolio change)\n")

cat("\n-- rows whose class changes once pinned, by year --\n")
print(setDT(dbGetQuery(con, sprintf("
SELECT year(MDate) y,
       round(avg(CASE WHEN p.cls IS DISTINCT FROM (%s) THEN 1 ELSE 0 END), 4) AS share_moved,
       round(sum(CASE WHEN p.cls IS DISTINCT FROM (%s) THEN Vermoegen_CHF ELSE 0 END)/1e9, 3) AS bn_moved
FROM read_parquet('%s') f JOIN asset_cls p USING (Asset_ID)
WHERE Instrumentengruppe <> 'Kredit' GROUP BY 1 ORDER BY 1",
  CLS, CLS, POS_FULL))))

## ---------------------------------------------------------------------------
## 1. client x month composition
## ---------------------------------------------------------------------------
sql_pf <- sprintf("
WITH p AS (
  SELECT f.Bp_ID, f.MDate, f.Asset_ID, f.Vermoegen_CHF AS v,
         coalesce(a.cls, 'other') AS cls
  FROM read_parquet('%s') f
  LEFT JOIN asset_cls a ON f.Asset_ID = a.Asset_ID
  WHERE f.Instrumentengruppe <> 'Kredit'
)
SELECT Bp_ID, MDate,
       sum(v)                                         AS tot_value,
       sum(CASE WHEN cls='equity' THEN v ELSE 0 END)  AS v_equity,
       sum(CASE WHEN cls='bond'   THEN v ELSE 0 END)  AS v_bond,
       sum(CASE WHEN cls='mixed'  THEN v ELSE 0 END)  AS v_mixed,
       sum(CASE WHEN cls='reales' THEN v ELSE 0 END)  AS v_reales,
       sum(CASE WHEN cls='deriv'  THEN v ELSE 0 END)  AS v_deriv,
       sum(CASE WHEN cls='alt'    THEN v ELSE 0 END)  AS v_alt,
       count(*)                                       AS n_pos_all,
       count(DISTINCT Asset_ID)                       AS n_assets_all
FROM p GROUP BY 1,2", POS_FULL)

pf <- setDT(dbGetQuery(con, sql_pf))
pf[, MDate := eom(MDate)]
log_step("portfolio composition (client x month)", pf)

## shares are only defined where the portfolio is big enough to divide by
pf[, `:=`(
  eq_share    = fifelse(tot_value >= P$min_bom_value, v_equity / tot_value, NA_real_),
  bond_share  = fifelse(tot_value >= P$min_bom_value, v_bond   / tot_value, NA_real_),
  risky_share = fifelse(tot_value >= P$min_bom_value,
                        (v_equity + v_mixed + v_reales + v_deriv + v_alt) / tot_value,
                        NA_real_)
)]
## a handful of short-option portfolios can push a share outside [0,1]
for (v in c("eq_share","bond_share","risky_share"))
  pf[, (v) := pmin(pmax(get(v), -0.5), 1.5)]

cat("\n== equity share distribution (client-months with tot_value >= floor) ==\n")
print(round(quantile(pf$eq_share, c(.01,.1,.25,.5,.75,.9,.99), na.rm = TRUE), 3))
cat("\n== risky share ==\n")
print(round(quantile(pf$risky_share, c(.01,.1,.25,.5,.75,.9,.99), na.rm = TRUE), 3))
cat("\n== mean shares by year ==\n")
print(pf[, .(n = .N, eq = round(mean(eq_share, na.rm = TRUE), 3),
             bond = round(mean(bond_share, na.rm = TRUE), 3),
             risky = round(mean(risky_share, na.rm = TRUE), 3)),
         by = .(y = year(MDate))][order(y)])

## ---------------------------------------------------------------------------
## 1b. GUARD: no month-on-month step in the equal-weighted mean equity share
##     that is not a price move. This is the check that would have caught the
##     April 2021 relabelling; it now runs on every build.
## ---------------------------------------------------------------------------
mm <- pf[!is.na(eq_share), .(clients = .N, eq = mean(eq_share),
                             zero_eq = mean(v_equity == 0)), by = MDate][order(MDate)]
mm[, `:=`(d_eq = eq - shift(eq), d_zero = zero_eq - shift(zero_eq))]
cat("\n== largest month-on-month moves in the mean equity share ==\n")
print(mm[order(-abs(d_eq))][1:8, .(MDate, clients, eq = round(eq, 4),
                                   d_eq = round(d_eq, 4),
                                   zero_eq = round(zero_eq, 4),
                                   d_zero = round(d_zero, 4))])
big <- mm[abs(d_eq) > 0.02 | abs(d_zero) > 0.02]
if (nrow(big)) {
  cat("\n!! WARNING: ", nrow(big), " month(s) move the mean equity share by >2 share\n", sep = "")
  cat("   points, or the share of zero-equity clients by >2pp, in a single step.\n")
  cat("   A price move cannot do that to an equal-weighted mean. Check whether the\n")
  cat("   asset master was relabelled in that month (NOTES section 6c).\n")
  print(big[, .(MDate, d_eq = round(d_eq, 4), d_zero = round(d_zero, 4))])
} else {
  cat("\nno month moves the mean equity share by more than 2 share points: OK\n")
}

save_dt(pf, "portfolio_m")
save_dt(setDT(dbGetQuery(con, "SELECT * FROM asset_cls")), "asset_cls")

## ---------------------------------------------------------------------------
## 2. asset price panel: implied CHF unit price
##    value-weighted across holders, so one client's odd row cannot move it
## ---------------------------------------------------------------------------
sql_px <- sprintf("
SELECT Asset_ID, MDate,
       sum(Vermoegen_CHF) / sum(Menge) AS px,
       count(*)                        AS n_holders,
       sum(Vermoegen_CHF)              AS v_tot
FROM read_parquet('%s')
WHERE Menge > 0 AND Vermoegen_CHF > 0
  AND Instrumentengruppe IN ('Aktien','Fonds','Obligationen',
                             'Strukturierte Prod./Zertifikate','Metall','Warrants')
GROUP BY 1,2", POS_FULL)

px <- setDT(dbGetQuery(con, sql_px))
px[, MDate := eom(MDate)]
setorder(px, Asset_ID, MDate)
log_step("asset price panel (asset x month)", px, id = "Asset_ID")

cat("\n== price panel coverage ==\n")
cat("assets:", uniqueN(px$Asset_ID), "  months:", uniqueN(px$MDate), "\n")
print(px[, .(assets = uniqueN(Asset_ID), median_holders = median(n_holders)),
         by = .(y = year(MDate))][order(y)])

## sanity: implied monthly asset returns should look like asset returns
px[, ret := px / shift(px) - 1, by = Asset_ID]
px[, gap := mdiff(MDate, shift(MDate)), by = Asset_ID]
px[gap != 1L, ret := NA_real_]          # not a consecutive month -> not a return
cat("\n== implied monthly asset returns (consecutive months only) ==\n")
print(round(quantile(px$ret, c(.01,.05,.25,.5,.75,.95,.99), na.rm = TRUE), 4))
cat("share |ret| > 0.5 :", round(px[is.finite(ret), mean(abs(ret) > 0.5)], 5), "\n")
cat("(a fat tail here is usually a share split or a distribution, not a price move;\n")
cat(" 04b therefore values the FROZEN portfolio with prices directly, never by\n")
cat(" compounding these returns)\n")
px[, `:=`(ret = NULL, gap = NULL)]

save_dt(px, "prices_m")
log_step("prices written")
