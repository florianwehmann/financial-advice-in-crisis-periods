# =============================================================================
# tr_funs.R -- machinery for splitting the portfolio TOTAL RETURN of
# 07_estim_v3.R (pf_ret -> pf_idx_w) into a passive and an active part.
#
# Sourced by total_return_split.R; defines functions only, writes nothing.
#
# The idea
# --------
# 07 builds the client's realized time-weighted return
#     pf_ret_t = (dp_tot_pf_t + dfx_tot_pf_t) / tot_pf_{t-1},  pf_idx = cumprod(1+pf_ret)
# which is the value-weighted average of the individual assets' returns using the
# client's ACTUAL weights each month. Those weights move for two reasons: prices
# move (mechanical, passive) and the client trades (active).
#
# The passive counterfactual freezes the portfolio at rel_month -1 and lets only
# prices work:
#     pb_val_t = SUM_i V_i,-1 * PROD_s (1 + r_a(i),s)
# where r_a,s is asset a's own monthly CHF return. Then
#     passive = pb_val_t / pb_val_-1 - 1
#     active  = actual - passive
# Active picks up everything the client DID: selling into the drawdown, buying
# the rebound, rotating between assets, and (for a client who liquidates) the
# return they no longer earn, because 07 sets pf_ret = 0 once the portfolio is
# empty while the frozen portfolio keeps running.
#
# Asset returns come from the position file itself (01_merge_pos.R -> pos_m1),
# pooled over ALL clients holding the asset:
#     r_a,t = SUM_c (dp + dfx)_c,a,t / SUM_c V_c,a,t-1
# i.e. exactly the same price+FX attribution that the actual return uses, so the
# two series are constructed from one source and the split is additive by
# construction. Only client-asset pairs observed in two consecutive months enter,
# so new and sold positions never distort the asset's own return.
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(duckdb)
  library(lubridate)
})

# portfolio scope, as in 02a_pos_aggm.R (is_pf): everything that is not cash,
# credit, a credit limit or the dummy group. pos_m1 already drops the cash groups.
TR_EXCL_GRP <- "('Kredit')"

tr_con <- function(mem = "8GB") {
  dbConnect(duckdb(), config = list(threads = as.character(parallel::detectCores()),
                                    memory_limit = mem))
}

tr_eom   <- function(x) ceiling_date(as.Date(x), "month") - 1
tr_madd  <- function(a, k) tr_eom(as.Date(a) %m+% months(k))
tr_mdiff <- function(a, b) as.integer((year(a) - year(b)) * 12L + (month(a) - month(b)))

# =============================================================================
# 1. ASSET x MONTH RETURN PANEL
# =============================================================================
# One row per Asset_ID x month: the value-weighted CHF return over every client
# holding it in both that month and the one before.
tr_asset_returns <- function(pos_path, out_file, from, to, force = FALSE, quiet = FALSE) {
  if (!force && file.exists(out_file)) {
    if (!quiet) cat("asset returns: reusing", out_file, "\n")
    return(invisible(out_file))
  }
  con <- tr_con(); on.exit(dbDisconnect(con, shutdown = TRUE))
  t0 <- Sys.time()
  sql <- sprintf("
    COPY (
      WITH p AS (          -- one row per client x asset x month (sum over custody accounts)
        SELECT Bp_ID, Asset_ID, MDate,
               SUM(Geschaeftsvolumen_CHF) AS v,
               SUM(DA_Titelkursabweichung_CHF + DA_Devisenkursabweichung_CHF) AS dpfx
        FROM read_parquet('%s')
        WHERE Instrumentengruppe NOT IN %s
          AND MDate BETWEEN DATE '%s' AND DATE '%s'
        GROUP BY 1, 2, 3
      ),
      q AS (
        SELECT *, LAG(v) OVER w AS v_lag, LAG(MDate) OVER w AS m_lag
        FROM p WINDOW w AS (PARTITION BY Bp_ID, Asset_ID ORDER BY MDate)
      )
      SELECT Asset_ID, MDate,
             SUM(dpfx) / SUM(v_lag) AS r,
             SUM(v_lag)             AS v_lag_tot,
             COUNT(*)               AS n_holders
      FROM q
      WHERE v_lag IS NOT NULL AND v_lag > 0
        AND date_diff('month', m_lag, MDate) = 1
      GROUP BY 1, 2
      HAVING SUM(v_lag) > 0
    ) TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD)",
    pos_path, TR_EXCL_GRP, format(as.Date(from)), format(as.Date(to)), out_file)
  dbExecute(con, sql)
  if (!quiet) cat(sprintf("asset returns: built %s in %.0fs\n", out_file,
                          as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  invisible(out_file)
}

# =============================================================================
# 2. PASSIVE (BUY-AND-HOLD) PORTFOLIO VALUE
# =============================================================================
# ep  : ep_id, pre_month, pre_start, post_end
# smp : Bp_ID, ep_id  (the estimation sample)
# r_cap: monthly asset returns are clamped to [-1 + eps, r_cap] before compounding,
#        so one broken observation cannot wipe out or explode a whole path.
#
# Returns one row per client x episode x month:
#   pb_val      passive portfolio value (frozen m=-1 quantities, asset returns)
#   v0_sum      frozen portfolio value at the freeze month
#   n_pos       frozen positions
#   v0_no_ret   value of frozen positions with no asset return that month
tr_build_passive <- function(ep, smp, pos_path, ar_file, r_cap = 3, quiet = FALSE) {
  ep  <- as.data.frame(ep[, .(ep_id, pre_month = as.Date(pre_month),
                              pre_start = as.Date(pre_start), post_end = as.Date(post_end))])
  smp <- as.data.frame(unique(smp[, .(Bp_ID, ep_id)]))
  con <- tr_con(); on.exit(dbDisconnect(con, shutdown = TRUE))
  duckdb_register(con, "ep", ep)
  duckdb_register(con, "smp", smp)
  ex <- function(s, ...) dbExecute(con, sprintf(s, ...))

  # months each episode spans
  ex("CREATE OR REPLACE TEMP TABLE epm AS
      SELECT ep_id, last_day(g.m) AS MDate
      FROM ep, LATERAL generate_series(pre_start, post_end, INTERVAL 1 MONTH) g(m)")

  # holdings frozen at the pre-month
  ex("CREATE OR REPLACE TEMP TABLE hold AS
      SELECT e.ep_id, p.Bp_ID, p.Asset_ID, SUM(p.Geschaeftsvolumen_CHF) AS v0
      FROM read_parquet('%s') p
      JOIN ep  e ON p.MDate = e.pre_month
      JOIN smp s ON s.Bp_ID = p.Bp_ID AND s.ep_id = e.ep_id
      WHERE p.Instrumentengruppe NOT IN %s AND p.Geschaeftsvolumen_CHF IS NOT NULL
      GROUP BY 1, 2, 3
      HAVING SUM(p.Geschaeftsvolumen_CHF) > 0", pos_path, TR_EXCL_GRP)

  # cumulative return factor of every held asset over its episode's months,
  # rebased to the freeze month (months before it divide back out)
  ex("CREATE OR REPLACE TEMP TABLE af AS
      WITH a AS (SELECT DISTINCT ep_id, Asset_ID FROM hold),
           g AS (SELECT a.ep_id, a.Asset_ID, m.MDate
                 FROM a JOIN epm m ON m.ep_id = a.ep_id),
           j AS (SELECT g.*, r.r,
                        GREATEST(1 + COALESCE(r.r, 0), 0.01) AS f
                 FROM g LEFT JOIN read_parquet('%s') r
                   ON r.Asset_ID = g.Asset_ID AND r.MDate = g.MDate),
           c AS (SELECT *, EXP(SUM(LN(LEAST(f, %f))) OVER
                               (PARTITION BY ep_id, Asset_ID ORDER BY MDate)) AS cf
                 FROM j),
           b AS (SELECT c.ep_id, c.Asset_ID, c.cf AS cf0
                 FROM c JOIN ep e ON e.ep_id = c.ep_id AND c.MDate = e.pre_month)
      SELECT c.ep_id, c.Asset_ID, c.MDate, c.r, c.cf / b.cf0 AS cf_reb
      FROM c JOIN b ON b.ep_id = c.ep_id AND b.Asset_ID = c.Asset_ID",
     ar_file, 1 + r_cap)

  out <- dbGetQuery(con, "
      SELECT h.ep_id, h.Bp_ID, m.MDate,
             SUM(h.v0 * COALESCE(f.cf_reb, 1))                  AS pb_val,
             SUM(h.v0)                                          AS v0_sum,
             COUNT(*)                                           AS n_pos,
             SUM(CASE WHEN f.r IS NULL THEN h.v0 ELSE 0 END)    AS v0_no_ret
      FROM hold h
      JOIN epm m ON m.ep_id = h.ep_id
      LEFT JOIN af f ON f.ep_id = h.ep_id AND f.Asset_ID = h.Asset_ID AND f.MDate = m.MDate
      GROUP BY 1, 2, 3")
  setDT(out)
  out[, MDate := as.Date(MDate)]
  if (!quiet) cat(sprintf("passive: %s client-episode-months, %s client-episodes\n",
                          format(nrow(out), big.mark = "'"),
                          format(uniqueN(out, by = c("Bp_ID", "ep_id")), big.mark = "'")))
  out[]
}

# =============================================================================
# 3. OUTCOMES -- actual, passive, active
# =============================================================================
# The actual side is built exactly as in 07_estim_v3.R (lines 108-135): monthly
# return on the lagged portfolio value, 0 when the portfolio is empty or in the
# first window month, winsorized per episode x month, then compounded and rebased
# to rel_month -1. The passive side gets the same treatment, so the two indices
# are comparable and active = actual - passive is a clean residual.
#
# d needs: Bp_ID, ep_id, MDate, rel_month, tot_pf, dp_tot_pf, dfx_tot_pf, pb_val
tr_outcomes <- function(d, ref = -1L, p_win = 0.99) {
  g <- c("ep_id", "Bp_ID")
  setorderv(d, c("ep_id", "Bp_ID", "MDate"))

  # --- actual (07's construction) --------------------------------------------
  d[, pf_prev := shift(tot_pf), by = g]
  d[, valid_ret := !is.na(pf_prev) & pf_prev > 0]
  d[, pf_ret := fifelse(valid_ret, (dp_tot_pf + dfx_tot_pf) / pf_prev, 0)]

  # --- passive ----------------------------------------------------------------
  d[, pb_prev := shift(pb_val), by = g]
  d[, valid_pb := !is.na(pb_prev) & pb_prev > 0]
  d[, pb_ret := fifelse(valid_pb, pb_val / pb_prev - 1, 0)]

  # --- winsorize both per episode x month, on real returns only ---------------
  wz <- function(x, ok) {
    q <- quantile(x[ok], c(1 - p_win, p_win), na.rm = TRUE)
    fifelse(ok, pmin(pmax(x, q[1]), q[2]), 0)
  }
  d[, pf_ret_w := wz(pf_ret, valid_ret), by = .(ep_id, MDate)]
  d[, pb_ret_w := wz(pb_ret, valid_pb),  by = .(ep_id, MDate)]

  # --- compound and rebase to the freeze month --------------------------------
  d[, `:=`(act_idx = cumprod(1 + pf_ret_w),
           pb_idx  = cumprod(1 + pb_ret_w)), by = g]
  d[, `:=`(act_idx = act_idx / act_idx[rel_month == ref][1] - 1,
           pb_idx  = pb_idx  / pb_idx[rel_month == ref][1]  - 1), by = g]
  d[, act_gap := act_idx - pb_idx]        # what trading added (or cost)

  # unwinsorized versions, for the checks
  d[, `:=`(act_idx_raw = cumprod(1 + pf_ret),
           pb_idx_raw  = cumprod(1 + pb_ret)), by = g]
  d[, `:=`(act_idx_raw = act_idx_raw / act_idx_raw[rel_month == ref][1] - 1,
           pb_idx_raw  = pb_idx_raw  / pb_idx_raw[rel_month == ref][1]  - 1), by = g]
  d[]
}

TR_OUTCOMES <- c("act_idx", "pb_idx", "act_gap")
TR_LABS <- c(act_idx = "Actual portfolio return",
             pb_idx  = "Passive (buy & hold m=-1 portfolio)",
             act_gap = "Active (actual - passive)")
