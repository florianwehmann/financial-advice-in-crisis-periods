# =============================================================================
# sb_funs.R -- stock-level buy-and-hold counterfactual from REAL security prices
# (T60_Aktienkurse.parquet), as opposed to the implied prices of pb_funs.R.
#
# Sourced by stock_buyhold.R; defines functions only, writes nothing.
# Estimation helpers are reused read-only from pb_funs.R (functions only, no
# side effects).
#
# What T60 is, and what that implies:
#   Asset_ID, Datum, Kurs_CHF, Source -- 5.4m quotes on 12,408 securities.
#   * EQUITIES ONLY. Funds, bonds and structured products have no T60 price at
#     all, so the counterfactual here is the EQUITY SLEEVE of the portfolio,
#     not the whole portfolio.
#   * Two regimes: Source = 'Buchung' (2009-09..2021-10, prices implied by
#     bookings, so irregular dates) and 'Bestand' (2021-11..2024-12, daily).
#     Month-end prices are therefore carried back from the last quote on or
#     before month end; sb_price_panel() records the age of every quote so the
#     staleness can be capped and reported.
#   * Kurs_CHF is already in CHF, so its change is a price AND FX move. The
#     actual side is built to match (DA_Titelkurs + DA_Devisenkurs).
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(duckdb)
  library(lubridate)
})

SB_EQUITY_GRP <- "Aktien"   # T60 only prices this Instrumentengruppe

sb_con <- function(mem = "8GB") {
  dbConnect(duckdb(), config = list(threads = as.character(parallel::detectCores()),
                                    memory_limit = mem))
}

# =============================================================================
# 1. MONTH-END PRICE PANEL FROM T60
# =============================================================================
# One row per security x month end: the last quote on or before that month end
# (ASOF join), with the age of the quote in days and its source.
sb_price_panel <- function(t60_path, out_file, from, to, force = FALSE, quiet = FALSE) {
  if (!force && file.exists(out_file)) {
    if (!quiet) cat("T60 month-end panel: reusing", out_file, "\n")
    return(invisible(out_file))
  }
  con <- sb_con(); on.exit(dbDisconnect(con, shutdown = TRUE))
  t0 <- Sys.time()
  dbExecute(con, sprintf("
    CREATE OR REPLACE TEMP TABLE q AS
    SELECT Asset_ID, CAST(Datum AS DATE) AS dt, Kurs_CHF AS px, Source
    FROM read_parquet('%s')
    WHERE Kurs_CHF IS NOT NULL AND Kurs_CHF > 0", t60_path))
  dbExecute(con, sprintf("
    CREATE OR REPLACE TEMP TABLE grid AS
    SELECT a.Asset_ID, last_day(m.d) AS MDate
    FROM (SELECT DISTINCT Asset_ID FROM q) a
    CROSS JOIN (SELECT UNNEST(generate_series(DATE '%s', DATE '%s', INTERVAL 1 MONTH)) AS d) m",
    format(as.Date(from)), format(as.Date(to))))
  dbExecute(con, sprintf("
    COPY (
      SELECT g.Asset_ID, g.MDate, q.px, q.dt AS px_date, q.Source AS px_source,
             date_diff('day', q.dt, g.MDate) AS px_age_d
      FROM grid g
      ASOF LEFT JOIN q ON g.Asset_ID = q.Asset_ID AND g.MDate >= q.dt
    ) TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD)", out_file))
  if (!quiet) cat(sprintf("T60 month-end panel: built %s in %.0fs\n", out_file,
                          as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  invisible(out_file)
}

# =============================================================================
# 2. EQUITY SLEEVE: ACTUAL PATH AND FROZEN-QUANTITY COUNTERFACTUAL
# =============================================================================
# ep  : ep_id, pre_month, pre_start, post_end
# smp : Bp_ID, ep_id (estimation sample)
# max_stale_d: quotes older than this are not used (the position is then valued
#              at its last usable price and counted as uncovered)
#
# Returns one row per client x episode x month:
#   eq_act        actual equity value that month (all equity the client holds)
#   d_eq_act      actual equity price+FX change that month (DA_Titel + DA_Devisen)
#   sb_val        frozen-quantity equity value at T60 month-end prices
#   sb_val0       frozen equity value at the freeze month (T60 prices)
#   eq_pre_pos    actual frozen equity value at the freeze month (book value)
#   cov0          share of frozen equity value that T60 can price at the freeze month
#   n_eq, n_eq_px positions frozen / of those priced by T60 in that month
#   px_age_max    oldest quote used that month, in days
sb_build_equity <- function(ep, smp, pos_path, px_file, max_stale_d = 45L, quiet = FALSE) {
  ep  <- as.data.frame(ep[, .(ep_id, pre_month = as.Date(pre_month),
                              pre_start = as.Date(pre_start), post_end = as.Date(post_end))])
  smp <- as.data.frame(unique(smp[, .(Bp_ID, ep_id)]))
  con <- sb_con(); on.exit(dbDisconnect(con, shutdown = TRUE))
  duckdb_register(con, "ep", ep)
  duckdb_register(con, "smp", smp)
  ex <- function(s, ...) dbExecute(con, sprintf(s, ...))

  # months each episode spans
  ex("CREATE OR REPLACE TEMP TABLE epm AS
      SELECT ep_id, last_day(g.m) AS MDate
      FROM ep, LATERAL generate_series(pre_start, post_end, INTERVAL 1 MONTH) g(m)")

  # (a) frozen equity holdings at the pre-month
  ex("CREATE OR REPLACE TEMP TABLE hold AS
      SELECT e.ep_id, p.Bp_ID, p.Asset_ID,
             SUM(p.Menge) AS q, SUM(p.Geschaeftsvolumen_CHF) AS v0
      FROM read_parquet('%s') p
      JOIN ep  e ON p.MDate = e.pre_month
      JOIN smp s ON s.Bp_ID = p.Bp_ID AND s.ep_id = e.ep_id
      WHERE p.Instrumentengruppe = '%s' AND p.Menge <> 0
        AND p.Geschaeftsvolumen_CHF IS NOT NULL
      GROUP BY 1, 2, 3
      HAVING SUM(p.Menge) <> 0", pos_path, SB_EQUITY_GRP)

  # (b) usable T60 quotes, carried forward within max_stale_d
  ex("CREATE OR REPLACE TEMP TABLE px AS
      SELECT Asset_ID, MDate, px, px_age_d, px_source
      FROM read_parquet('%s')
      WHERE px IS NOT NULL AND px_age_d <= %d", px_file, as.integer(max_stale_d))

  # (c) counterfactual value: frozen quantities at T60 month-end prices.
  #     A position whose quote is missing or too old keeps its freeze-month book
  #     value (0% return) and is counted as uncovered.
  ex("CREATE OR REPLACE TEMP TABLE cf AS
      SELECT h.ep_id, h.Bp_ID, m.MDate,
             SUM(CASE WHEN p.px IS NOT NULL THEN h.q * p.px ELSE h.v0 END) AS sb_val,
             SUM(h.v0)                                                     AS eq_pre_pos,
             COUNT(*)                                                      AS n_eq,
             SUM(CASE WHEN p.px IS NOT NULL THEN 1 ELSE 0 END)             AS n_eq_px,
             SUM(CASE WHEN p.px IS NOT NULL THEN h.v0 ELSE 0 END)          AS v0_priced,
             MAX(p.px_age_d)                                               AS px_age_max
      FROM hold h
      JOIN epm m ON m.ep_id = h.ep_id
      LEFT JOIN px p ON p.Asset_ID = h.Asset_ID AND p.MDate = m.MDate
      GROUP BY 1, 2, 3")

  # (d) actual equity sleeve, month by month (every equity position held)
  ex("CREATE OR REPLACE TEMP TABLE act AS
      SELECT e.ep_id, p.Bp_ID, p.MDate,
             SUM(p.Geschaeftsvolumen_CHF) AS eq_act,
             SUM(p.DA_Titelkursabweichung_CHF + p.DA_Devisenkursabweichung_CHF) AS d_eq_act
      FROM read_parquet('%s') p
      JOIN ep  e ON p.MDate BETWEEN e.pre_start AND e.post_end
      JOIN smp s ON s.Bp_ID = p.Bp_ID AND s.ep_id = e.ep_id
      WHERE p.Instrumentengruppe = '%s'
      GROUP BY 1, 2, 3", pos_path, SB_EQUITY_GRP)

  out <- dbGetQuery(con, "
      SELECT c.ep_id, c.Bp_ID, c.MDate, c.sb_val, c.eq_pre_pos, c.n_eq, c.n_eq_px,
             c.v0_priced / NULLIF(c.eq_pre_pos, 0) AS cov_m, c.px_age_max,
             COALESCE(a.eq_act, 0)   AS eq_act,
             COALESCE(a.d_eq_act, 0) AS d_eq_act
      FROM cf c
      LEFT JOIN act a ON a.ep_id = c.ep_id AND a.Bp_ID = c.Bp_ID AND a.MDate = c.MDate")
  setDT(out)
  out[, MDate := as.Date(MDate)]
  if (!quiet) cat(sprintf("equity sleeve: %s client-episode-months, %s client-episodes\n",
                          format(nrow(out), big.mark = "'"),
                          format(uniqueN(out, by = c("Bp_ID", "ep_id")), big.mark = "'")))
  out[]
}

# =============================================================================
# 3. OUTCOMES
# =============================================================================
# Anchored at rel_month == ref, so actual and counterfactual both start at zero.
#   sb_passive : frozen equity at T60 prices  (composition / market move)
#   act_eq     : actual equity price+FX change (what really happened)
#   sb_gap     : act_eq - sb_passive          (equity trading and timing)
# Scaled by the freeze-month equity value (main) and by tot_pf_pre (comparable
# to the pb_* outcomes of passive_benchmark.R).
sb_outcomes <- function(d, ref = -1L) {
  g <- c("Bp_ID", "ep_id")
  setorderv(d, c(g, "rel_month"))
  d[, d_eq_act_c := cumsum(fifelse(is.na(d_eq_act), 0, d_eq_act)), by = g]
  d[, `:=`(act_anchor = d_eq_act_c[rel_month == ref][1],
           sb_anchor  = sb_val[rel_month == ref][1],
           eq_ref     = eq_pre_pos[rel_month == ref][1],
           cov0       = cov_m[rel_month == ref][1]), by = g]
  d[, `:=`(act_chf = d_eq_act_c - act_anchor,
           sb_chf  = sb_val - sb_anchor)]
  d[, `:=`(act_eq        = act_chf / eq_ref,        # share of pre-period equity
           sb_passive    = sb_chf  / eq_ref,
           act_eq_of_pf  = act_chf / tot_pf_pre,    # share of pre-period portfolio
           sb_passive_of_pf = sb_chf / tot_pf_pre)]
  d[, `:=`(sb_gap       = act_eq - sb_passive,
           sb_gap_of_pf = act_eq_of_pf - sb_passive_of_pf)]
  # simple returns, for reporting
  d[, `:=`(sb_ret  = sb_val / sb_anchor - 1,
           act_ret = eq_act / eq_ref - 1)]
  d[, c("act_anchor", "sb_anchor", "act_chf", "sb_chf") := NULL]
  d[]
}

SB_OUTCOMES <- c("act_eq", "sb_passive", "sb_gap")
SB_LABS <- c(act_eq        = "Actual equity price+FX",
             sb_passive    = "Stock-level buy & hold (T60)",
             sb_gap        = "Equity active gap (actual - buy & hold)",
             act_eq_of_pf  = "Actual equity, share of portfolio",
             sb_passive_of_pf = "Buy & hold equity, share of portfolio",
             sb_gap_of_pf  = "Equity active gap, share of portfolio")
