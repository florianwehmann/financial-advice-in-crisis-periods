## =============================================================================
## 04b_cfgap.R -- the design's actual counterfactual, at last
##
## Freeze each client's holdings at the last pre-drawdown month end and drift
## them with observed security prices through post_end with NO trading, then
## compare to the realised value path. This is what section 4.2 of the design
## asked for; it was impossible from pos_m and is possible from pos_full.
##
## Method
##   holdings  : Bp_ID x Asset_ID x Menge at pre_month (pos_full)
##   prices    : Asset_ID x MDate implied CHF unit price (02c)
##   cf_value  : sum over frozen assets of Menge_pre * price(t)
##   realised  : the client's actual total portfolio value at t (02c)
##   cf_gap_hold = realised / cf_value - 1, rebased so it is 0 at pre_month
##
## A price is matched ASOF: the most recent quote at or before t. `px_age` is
## how many months stale that quote is, and `cover` is the share of the frozen
## portfolio's pre-episode value that could be priced at all in month t. Both
## are carried into the output so a specification can require, say, cover > 0.9.
##
## Output: results/cache/cfhold.parquet (Bp_ID x ep_id x MDate)
## =============================================================================

source("00_setup.R")
suppressPackageStartupMessages({library(duckdb); library(DBI)})
log_init("04b_cfgap")

POS_FULL <- file.path(DATA, "pos_full.parquet")
cle <- load_dt("cle")
ep  <- load_dt("episodes")[usable == TRUE]
px  <- load_dt("prices_m")

sink(file.path(RESULTS, "04b_cfgap.txt"), split = TRUE)
on.exit(sink(), add = TRUE)

## Month grid per episode, running from pre_start (rel_month -12), NOT from
## pre_month. Starting at the freeze date would leave the gap undefined before
## the crash, so the primary performance outcome would have no pre-period and no
## parallel-trends evidence. The frozen basket is still the pre_month holding;
## before pre_month it is simply valued backwards at the same quantities.
grid <- ep[, .(MDate = eom(seq(floor_date(pre_start, "month"),
                               floor_date(post_end,  "month"), by = "month"))),
           by = ep_id]
cat("month grid:\n"); print(grid[, .(months = .N, from = min(MDate), to = max(MDate)), by = ep_id])

keys <- cle[, .(Bp_ID, ep_id, pre_month)]

con <- dbConnect(duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
duck_setup(con)
duckdb_register(con, "keys", keys)
duckdb_register(con, "grid", grid)
duckdb_register(con, "px",   px[, .(Asset_ID, MDate, px)])

## ---------------------------------------------------------------------------
## 1. frozen holdings at the pre-episode month end
## ---------------------------------------------------------------------------
dbExecute(con, sprintf("
CREATE TEMP TABLE hold AS
SELECT k.Bp_ID, k.ep_id, f.Asset_ID, f.Menge AS qty, f.Vermoegen_CHF AS v_pre
FROM read_parquet('%s') f
JOIN keys k
  ON f.Bp_ID = k.Bp_ID AND last_day(f.MDate) = k.pre_month
WHERE f.Menge > 0 AND f.Vermoegen_CHF > 0
  AND f.Instrumentengruppe IN ('Aktien','Fonds','Obligationen',
      'Strukturierte Prod./Zertifikate','Metall','Warrants')", POS_FULL))

nh <- dbGetQuery(con, "SELECT count(*) n, count(DISTINCT Bp_ID||'|'||ep_id) ci FROM hold")
cat(sprintf("\nfrozen positions: %s over %s client-episodes\n",
            format(nh$n, big.mark = "'"), format(nh$ci, big.mark = "'")))

## ---------------------------------------------------------------------------
## 2. value the frozen basket in every month of the window (ASOF price match)
## ---------------------------------------------------------------------------
cfh <- setDT(dbGetQuery(con, "
WITH hg AS (
  SELECT h.Bp_ID, h.ep_id, h.Asset_ID, h.qty, h.v_pre, g.MDate
  FROM hold h JOIN grid g ON h.ep_id = g.ep_id
),
priced AS (
  SELECT hg.*, p.px, p.MDate AS px_date
  FROM hg ASOF LEFT JOIN px p
    ON hg.Asset_ID = p.Asset_ID AND hg.MDate >= p.MDate
)
SELECT Bp_ID, ep_id, MDate,
       sum(CASE WHEN px IS NOT NULL THEN qty * px ELSE 0 END) AS cf_value,
       sum(CASE WHEN px IS NOT NULL THEN v_pre  ELSE 0 END)   AS v_pre_priced,
       sum(v_pre)                                             AS v_pre_tot,
       max(date_diff('month', px_date, MDate))                AS px_age_max,
       count(*)                                               AS n_frozen
FROM priced GROUP BY 1,2,3"))

cfh[, MDate := eom(MDate)]
cfh[, cover := v_pre_priced / v_pre_tot]
setorder(cfh, Bp_ID, ep_id, MDate)
log_step("frozen-portfolio value path", cfh)

## ---------------------------------------------------------------------------
## 3. rebase on the pre-episode month and attach the realised path
## ---------------------------------------------------------------------------
## both indices are rebased on pre_month (rel_month -1), so the gap is 0 there by
## construction and the months before it carry genuine pre-trend information
cfh <- ep[, .(ep_id, pre_month)][cfh, on = "ep_id"]
cfh[, ci := paste(Bp_ID, ep_id)]
cfh[, cf_base := cf_value[MDate == pre_month][1], by = ci]
cfh <- cfh[!is.na(cf_base) & cf_base > 0]
cfh[, cf_idx := cf_value / cf_base]        # frozen basket, pre_month = 1

pf <- load_dt("portfolio_m")[, .(Bp_ID, MDate, tot_value)]
cfh <- pf[cfh, on = .(Bp_ID, MDate)]
cfh[is.na(tot_value), tot_value := 0]      # no position row = no securities left
cfh[, w_base := tot_value[MDate == pre_month][1], by = ci]
cfh <- cfh[!is.na(w_base) & w_base > 0]
cfh[, w_idx := tot_value / w_base]         # realised SECURITIES, pre_month = 1

cfh[, cf_gap_hold := w_idx / cf_idx - 1]

## ---------------------------------------------------------------------------
## 3b. the same gap on TOTAL WEALTH, i.e. counting the cash a seller is holding
##     Comparing realised securities with a frozen securities basket charges a
##     client who sold at the trough the full loss even though the money is
##     still on deposit at the bank. With cash available (02e) the economically
##     correct comparison is
##        (securities + cash)  vs  (frozen securities + the cash held at freeze)
##     Both sides carry the same starting cash, so the gap still isolates the
##     effect of the trading decision.
## ---------------------------------------------------------------------------
csh <- load_dt("cash_m")[, .(Bp_ID, MDate, cash_free)]
cfh <- csh[cfh, on = .(Bp_ID, MDate)]
cfh[is.na(cash_free), cash_free := 0]
cfh[, cash_pre := cash_free[MDate == pre_month][1], by = ci]
cfh[is.na(cash_pre), cash_pre := 0]

cfh[, wtot_base := w_base + cash_pre]
## cf_idx is the frozen basket as an index on pre_month, so cf_idx * w_base is
## its CHF value; both sides are then divided by the same starting total wealth
cfh[, `:=`(w_tot_idx  = (tot_value + cash_free)      / wtot_base,
           cf_tot_idx = (cf_idx * w_base + cash_pre) / wtot_base)]
## guard: in the earliest pre-window months a frozen asset may not exist yet, so
## cf_value can be 0. Dividing by it produces Inf, which would silently poison
## every mean downstream.
cfh[, cf_gap_hold_w := fifelse(cf_tot_idx > 0, w_tot_idx / cf_tot_idx - 1, NA_real_)]
cfh[, cf_gap_hold   := fifelse(cf_idx     > 0, w_idx     / cf_idx     - 1, NA_real_)]
cat("\nrows with a zero-valued frozen basket (gap set to NA): ",
    cfh[!(cf_idx > 0), .N], " of ", nrow(cfh), "\n", sep = "")

cat("\n== how much the cash correction matters ==\n")
cat("client-months where the client holds no securities but does hold cash: ",
    cfh[tot_value == 0 & cash_free > 0, .N], " of ", nrow(cfh), "\n", sep = "")
cat("mean gap at those rows, securities only : ",
    round(cfh[tot_value == 0 & cash_free > 0, mean(cf_gap_hold, na.rm = TRUE)], 4), "\n")
cat("mean gap at those rows, total wealth    : ",
    round(cfh[tot_value == 0 & cash_free > 0, mean(cf_gap_hold_w, na.rm = TRUE)], 4), "\n")
cat("(the first number charges a seller the entire portfolio; the second does not)\n")

cat("\n== price coverage of the frozen portfolio (share of pre-episode value) ==\n")
print(cfh[, .(mean_cover = round(mean(cover), 4),
              p10_cover  = round(quantile(cover, .1), 4),
              share_full = round(mean(cover > 0.99), 4),
              share_90   = round(mean(cover > 0.90), 4)), by = ep_id][order(ep_id)])
cat("\n== price staleness: months since the last quote (max within a portfolio) ==\n")
print(cfh[, .(median = median(px_age_max, na.rm = TRUE),
              p90 = quantile(px_age_max, .9, na.rm = TRUE)), by = ep_id])

cat("\n== cf_gap_hold vs. the two pos_m approximations, at post_end ==\n")
end <- ep[, .(ep_id, post_end)][cfh, on = .(ep_id, post_end = MDate), nomatch = 0]
print(end[, .(N = .N,
              cf_gap_hold = round(mean(cf_gap_hold, na.rm = TRUE), 4),
              median      = round(median(cf_gap_hold, na.rm = TRUE), 4)), by = ep_id])

save_dt(cfh[, .(Bp_ID, ep_id, MDate, cf_idx, w_idx, cf_gap_hold,
                w_tot_idx, cf_tot_idx, cf_gap_hold_w, cash_free, cash_pre,
                cover, px_age_max, n_frozen)], "cfhold")
log_step("cfhold written")

