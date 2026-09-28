# =============================================================================
# pb_funs.R -- machinery for the passive (buy-and-hold) benchmark.
#
# Sourced by passive_benchmark.R; defines functions only, writes nothing.
#
# What the benchmark is: freeze every client's positions at rel_month -1 and
# value THOSE quantities at each month's realized prices (CHF, i.e. price and FX
# move, quantities never do). The difference to the actual portfolio is then the
# active component -- trading and timing.
#
# Prices come from the position data itself: Geschaeftsvolumen_CHF / Menge is the
# CHF unit price of a security, and pooling over ALL clients (not just the
# estimation sample) gives a security x month price panel that keeps valuing a
# position after the client has sold it. Dividends and coupons are not in the
# data (they arrive as cash), so this is a price(+FX) return -- the same object
# as the DA_Titelkurs + DA_Devisenkurs component of the existing decomposition.
#
# Functions copied (not sourced) from existing scripts, because sourcing those
# runs a full estimation:
#   pb_build_stk   <- build_stk()  in 07_estim_v3_placebo.R
#   pb_prep_est    <- prep_est()   in 07_estim_v3_placebo.R
#   pb_make_eps / pb_eligible_starts / pb_draw_eps
#                  <- make_eps() / eligible_starts() / draw_eps() in 07_estim_v3_placebo.R
#   pb_est / pb_get_ct / pb_ev_terms
#                  <- est() / get_ct() / ev_terms() in 07_estim_v3.R
#                     (control name eq_share_of_w_pre_mean corrected -- see README note)
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(duckdb)
  library(fixest)
  library(arrow)
  library(lubridate)
})

# ---- dates (from 05_episodes.R) ---------------------------------------------
pb_eom   <- function(x) ceiling_date(as.Date(x), "month") - 1
pb_madd  <- function(a, k) pb_eom(as.Date(a) %m+% months(k))
pb_mdiff <- function(a, b) as.integer((year(a) - year(b)) * 12L + (month(a) - month(b)))

# =============================================================================
# 1. SECURITY x MONTH PRICE PANEL
# =============================================================================
# One row per Asset_ID x month: the median CHF unit price over every client
# holding it, plus the spread across holders (a sanity flag) and the number of
# holders. Built once and cached as parquet; every episode set reuses it.
pb_price_panel <- function(pos_path, out_file, from, to, force = FALSE, quiet = FALSE) {
  if (!force && file.exists(out_file)) {
    if (!quiet) cat("price panel: reusing", out_file, "\n")
    return(invisible(out_file))
  }
  con <- dbConnect(duckdb(), config = list(threads = as.character(parallel::detectCores()),
                                           memory_limit = "8GB"))
  on.exit(dbDisconnect(con, shutdown = TRUE))
  sql <- sprintf("
    COPY (
      SELECT Asset_ID, MDate,
             MEDIAN(Geschaeftsvolumen_CHF / Menge)                       AS px,
             COUNT(*)                                                    AS n_hold,
             (MAX(Geschaeftsvolumen_CHF / Menge) - MIN(Geschaeftsvolumen_CHF / Menge))
               / NULLIF(ABS(MEDIAN(Geschaeftsvolumen_CHF / Menge)), 0)   AS px_spread
      FROM read_parquet('%s')
      WHERE Menge <> 0 AND Geschaeftsvolumen_CHF IS NOT NULL
        AND Instrumentengruppe NOT IN ('Kredit')      -- portfolio scope, as in 02a (is_pf)
        AND MDate BETWEEN DATE '%s' AND DATE '%s'
      GROUP BY 1, 2
    ) TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD)",
    pos_path, format(as.Date(from)), format(as.Date(to)), out_file)
  t0 <- Sys.time()
  dbExecute(con, sql)
  if (!quiet) cat(sprintf("price panel: built %s in %.0fs\n", out_file,
                          as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  invisible(out_file)
}

# =============================================================================
# 2. PASSIVE PORTFOLIO VALUE
# =============================================================================
# ep : ep_id, pre_month, pre_start, post_end   (one row per episode)
# smp: Bp_ID, ep_id                            (the estimation sample)
# Returns one row per client x episode x month with
#   pb_pf         passive portfolio value (frozen quantities, month-m prices)
#   v0_sum        value of the frozen positions at the freeze month
#   n_pos         frozen positions
#   n_pos_stale   positions valued at a carried-forward price in that month
#   pb_val_stale  their value (share of pb_pf = how much of the benchmark is carried)
pb_build_passive <- function(ep, smp, pos_path, px_file, quiet = FALSE) {
  ep  <- as.data.frame(ep[, .(ep_id, pre_month = as.Date(pre_month),
                              pre_start = as.Date(pre_start), post_end = as.Date(post_end))])
  smp <- as.data.frame(unique(smp[, .(Bp_ID, ep_id)]))

  con <- dbConnect(duckdb(), config = list(threads = as.character(parallel::detectCores()),
                                           memory_limit = "8GB"))
  on.exit(dbDisconnect(con, shutdown = TRUE))
  duckdb_register(con, "ep", ep)
  duckdb_register(con, "smp", smp)
  ex <- function(s, ...) dbExecute(con, sprintf(s, ...))

  # (a) holdings frozen at the pre-month, summed over custody accounts
  ex("CREATE OR REPLACE TEMP TABLE hold AS
      SELECT e.ep_id, p.Bp_ID, p.Asset_ID,
             SUM(p.Menge) AS q, SUM(p.Geschaeftsvolumen_CHF) AS v0
      FROM read_parquet('%s') p
      JOIN ep  e ON p.MDate = e.pre_month
      JOIN smp s ON s.Bp_ID = p.Bp_ID AND s.ep_id = e.ep_id
      WHERE p.Instrumentengruppe NOT IN ('Kredit')
        AND p.Menge <> 0 AND p.Geschaeftsvolumen_CHF IS NOT NULL
      GROUP BY 1, 2, 3
      HAVING SUM(p.Menge) <> 0", pos_path)

  # (b) the months each episode spans
  ex("CREATE OR REPLACE TEMP TABLE epm AS
      SELECT ep_id, last_day(g.m) AS MDate
      FROM ep, LATERAL generate_series(pre_start, post_end, INTERVAL 1 MONTH) g(m)")

  # (c) prices for the frozen securities over the whole panel, carried forward
  #     (and backward for pre-window months before a security's first quote)
  ex("CREATE OR REPLACE TEMP TABLE pxff AS
      WITH a AS (SELECT DISTINCT Asset_ID FROM hold),
           m AS (SELECT DISTINCT MDate FROM read_parquet('%s')),
           grid AS (SELECT a.Asset_ID, m.MDate FROM a CROSS JOIN m)
      SELECT g.Asset_ID, g.MDate, p.px AS px_obs,
             LAST_VALUE(p.px IGNORE NULLS) OVER w_bwd  AS px_ff,
             FIRST_VALUE(p.px IGNORE NULLS) OVER w_fwd AS px_bf,
             LAST_VALUE(CASE WHEN p.px IS NOT NULL THEN g.MDate END IGNORE NULLS)
               OVER w_bwd AS px_date
      FROM grid g
      LEFT JOIN read_parquet('%s') p ON p.Asset_ID = g.Asset_ID AND p.MDate = g.MDate
      WINDOW w_bwd AS (PARTITION BY g.Asset_ID ORDER BY g.MDate
                       ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW),
             w_fwd AS (PARTITION BY g.Asset_ID ORDER BY g.MDate
                       ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING)",
     px_file, px_file)

  # (d) value the frozen quantities month by month
  out <- dbGetQuery(con, "
      SELECT h.ep_id, h.Bp_ID, m.MDate,
             SUM(h.q * COALESCE(f.px_ff, f.px_bf))                        AS pb_pf,
             SUM(h.v0)                                                    AS v0_sum,
             COUNT(*)                                                     AS n_pos,
             SUM(CASE WHEN f.px_obs IS NULL THEN 1 ELSE 0 END)            AS n_pos_stale,
             SUM(CASE WHEN f.px_obs IS NULL
                      THEN h.q * COALESCE(f.px_ff, f.px_bf) ELSE 0 END)   AS pb_val_stale,
             SUM(CASE WHEN f.px_ff IS NULL AND f.px_bf IS NULL THEN 1 ELSE 0 END) AS n_pos_nopx,
             MAX(CASE WHEN f.px_obs IS NULL
                      THEN date_diff('month', f.px_date, m.MDate) END)    AS max_px_age
      FROM hold h
      JOIN epm m ON m.ep_id = h.ep_id
      LEFT JOIN pxff f ON f.Asset_ID = h.Asset_ID AND f.MDate = m.MDate
      GROUP BY 1, 2, 3")
  setDT(out)
  out[, MDate := as.Date(MDate)]
  if (!quiet) cat(sprintf("passive: %s client-episode-months, %s client-episodes\n",
                          format(nrow(out), big.mark = "'"),
                          format(uniqueN(out, by = c("Bp_ID", "ep_id")), big.mark = "'")))
  out[]
}

# =============================================================================
# 3. OUTCOMES
# =============================================================================
# Everything is anchored at rel_month == ref, so actual and passive both start at
# zero there and pb_gap is the active part only. Anchoring subtracts a client x
# episode constant, which the ci fixed effect absorbs anyway -- it just makes the
# two series comparable in levels.
#
# d needs: Bp_ID, ep_id, rel_month, tot_pf, tot_pf_pre, tot_wealth_pre,
#          dp_tot_pf, dfx_tot_pf, dp_tot_pf_c, dfx_tot_pf_c, pb_pf
pb_outcomes <- function(d, ref = -1L, min_pf = 1000) {
  g <- c("Bp_ID", "ep_id")
  setorderv(d, c(g, "rel_month"))

  # --- cumulative CHF changes, anchored at the freeze month -------------------
  d[, act_cum := dp_tot_pf_c + dfx_tot_pf_c]          # actual price + FX
  d[, `:=`(act_anchor = act_cum[rel_month == ref][1],
           act_po_anchor = dp_tot_pf_c[rel_month == ref][1],
           pb_anchor  = pb_pf[rel_month == ref][1]), by = g]
  d[, `:=`(act_chf    = act_cum - act_anchor,
           act_po_chf = dp_tot_pf_c - act_po_anchor,  # price only, for reconciliation
           pb_chf     = pb_pf - pb_anchor)]

  # --- scaled outcomes --------------------------------------------------------
  d[, `:=`(act_price   = act_chf / tot_pf_pre,        # main scaling: pre-period portfolio
           pb_price    = pb_chf  / tot_pf_pre,
           act_price_w = act_chf / tot_wealth_pre,    # wealth scaling, as in dp_pct
           pb_price_w  = pb_chf  / tot_wealth_pre,
           act_price_po = act_po_chf / tot_pf_pre)]
  d[, `:=`(pb_gap   = act_price   - pb_price,
           pb_gap_w = act_price_w - pb_price_w)]

  # --- time-weighted returns (flow-free) -------------------------------------
  # chain-linked monthly portfolio return; the passive index is already a TWR
  d[, tot_pf_lag := shift(tot_pf), by = g]
  d[, r_act := (dp_tot_pf + dfx_tot_pf) / tot_pf_lag]
  d[!is.finite(r_act) | is.na(tot_pf_lag) | tot_pf_lag < min_pf, r_act := NA_real_]
  d[, f_act := 1 + r_act]
  d[rel_month == min(rel_month), f_act := 1, by = g]  # first month has no lag
  d[, lf := log(pmax(f_act, 1e-8))]                   # -100% returns are clamped
  d[, clf := cumsum(lf), by = g]
  d[, clf_ref := clf[rel_month == ref][1], by = g]
  d[, act_twr := exp(clf - clf_ref) - 1]
  d[, pb_twr  := pb_pf / pb_anchor - 1]
  d[, twr_gap := act_twr - pb_twr]

  d[, c("act_cum", "act_anchor", "act_po_anchor", "act_po_chf", "lf", "clf", "clf_ref") := NULL]
  d[]
}

PB_OUTCOMES <- c("act_price", "pb_price", "pb_gap", "act_twr", "pb_twr", "twr_gap")
PB_LABS <- c(act_price = "Actual price+FX", pb_price = "Passive (buy & hold)",
             pb_gap    = "Active gap (actual - passive)",
             act_twr   = "Actual TWR", pb_twr = "Passive TWR",
             twr_gap   = "TWR gap (actual - passive)")

pb_winsor <- function(x, p = 0.99) {
  q <- quantile(x, c(1 - p, p), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}

# =============================================================================
# 4. ESTIMATION (spec copied from 07_estim_v3.R)
# =============================================================================
pb_ev_terms <- function(v, ref) sprintf("i(rel_month, %s, ref = %d)", v, ref)

pb_rhs <- function(trt, trt_ctrl, ref, controls = PB_CONTROLS) {
  paste(pb_ev_terms(c(trt, trt_ctrl, controls), ref), collapse = " + ")
}
# NOTE: 07_estim_v3.R:120 names this control eq_share_of_ew_pre_mean, which is not
# a column in stk2 (the loaded name is eq_share_of_w_pre_mean). Corrected here only.
PB_CONTROLS <- c("log_w_pre_mean", "anlagepaket_pre", "adv_segment_pre",
                 "d_deposit_pre_sum", "eq_share_of_w_pre_mean")

pb_est <- function(lhs, data, rhs, cluster = ~advisor_id) {
  lhs_str <- if (length(lhs) == 1) lhs else sprintf("c(%s)", paste(lhs, collapse = ", "))
  fml <- as.formula(paste(lhs_str, "~", rhs, "| ci + te"))
  m <- feols(fml, data = data, vcov = cluster, lean = TRUE,
             fixef.tol = 1e-5, mem.clean = TRUE)
  if (length(lhs) == 1) return(setNames(list(m), lhs))
  setNames(as.list(m), lhs)
}

# event-study path of one treatment term, with the reference month added back
pb_get_ct <- function(mod, label, term, ref = -1L) {
  ct <- as.data.table(coeftable(mod), keep.rownames = "coef")
  ct <- ct[grepl(paste0(":", term, "$"), coef)]
  ct[, rel_month := as.numeric(sub("^rel_month::(-?[0-9.]+):.*$", "\\1", coef))]
  ct <- ct[, .(rel_month, est = Estimate, se = `Std. Error`)]
  ct <- rbind(ct, data.table(rel_month = ref, est = 0, se = 0))
  ct[, outcome := label][order(rel_month)]
}

# average of the event-study coefficients over a rel_month range, with its SE
# (linear combination -> w' V w, so the correlation across months is kept)
pb_avg <- function(mod, term, lo, hi) {
  b  <- coef(mod); V <- vcov(mod)
  nm <- grep(paste0("^rel_month::(-?[0-9]+):", term, "$"), names(b), value = TRUE)
  k  <- as.integer(sub("^rel_month::(-?[0-9]+):.*$", "\\1", nm))
  nm <- nm[k >= lo & k <= hi]
  if (!length(nm)) return(data.table(avg = NA_real_, se = NA_real_, n_months = 0L))
  w <- rep(1 / length(nm), length(nm))
  data.table(avg = sum(w * b[nm]), se = sqrt(drop(t(w) %*% V[nm, nm] %*% w)),
             n_months = length(nm))
}

# =============================================================================
# 5. PANEL REBUILD AND PLACEBO DRAWS (copied from 07_estim_v3_placebo.R)
# =============================================================================
# build_stk() there; unchanged except for the pb_ prefix and the explicit
# arguments (pos, contacts, load_cols) instead of globals.
pb_build_stk <- function(ep, pos, contacts, load_cols) {
  ep <- as.data.table(ep)[, .(ep_id, pre_month, pre_start, post_end, dd_start, dd_end)]
  cle <- CJ(Bp_ID = unique(pos$Bp_ID), ep_id = ep$ep_id)
  cle <- ep[cle, on = "ep_id"]

  pre_src <- c("anlagepaket", "adv_segment", "tot_pf", "cash_liq", "cash_locked", "hypo",
               "tot_wealth", "credit_check", "equity", "bond", "eq_share_of_w")
  pre <- pos[, c("Bp_ID", "MDate", pre_src), with = FALSE]
  setnames(pre, pre_src, paste0(pre_src, "_pre"))
  cle <- merge(cle, pre, by.x = c("Bp_ID", "pre_month"), by.y = c("Bp_ID", "MDate"), all.x = TRUE)

  cle[, tot_wealth_pre_mean :=
        pos[cle, on = .(Bp_ID, MDate >= pre_start, MDate <= pre_month),
            mean(tot_wealth, na.rm = TRUE), by = .EACHI]$V1]
  cle[, eq_share_of_w_pre_mean :=
        pos[cle, on = .(Bp_ID, MDate >= pre_start, MDate <= pre_month),
            mean(eq_share_of_w, na.rm = TRUE), by = .EACHI]$V1]

  n_tr <- pos[cle, on = .(Bp_ID, MDate >= dd_start, MDate <= dd_end),
              lapply(.SD, sum, na.rm = TRUE), .SDcols = contacts, by = .EACHI][, ..contacts]
  cle[, (paste0("treat_", contacts)) := lapply(n_tr, \(x) as.numeric(x > 0))]

  stk <- pos[cle[, .(Bp_ID, ep_id, pre_start, post_end)],
             on = .(Bp_ID, MDate >= pre_start, MDate <= post_end),
             .(Bp_ID, ep_id, MDate = x.MDate, advisor_id,
               tot_wealth, tot_pf, cash_liq, cash_locked, hypo, credit_check, d_deposit,
               dp_tot_pf, dq_tot_pf, dfx_tot_pf, dp_equity, dq_equity, dp_bond, dq_bond,
               pf_share_of_w, eq_share_of_pf, eq_share_of_w),
             nomatch = NULL, allow.cartesian = TRUE]
  stk <- ep[, .(ep_id, dd_start, pre_month, pre_start, post_end)][stk, on = "ep_id"]
  stk[, rel_month := pb_mdiff(MDate, dd_start)]
  setorder(stk, Bp_ID, ep_id, MDate)
  stk[, `:=`(dp_tot_pf_c  = cumsum(dp_tot_pf),
             dfx_tot_pf_c = cumsum(dfx_tot_pf),
             dq_tot_pf_c  = cumsum(dq_tot_pf)), by = .(Bp_ID, ep_id)]

  keep_cle <- c("Bp_ID", "ep_id", paste0("treat_", contacts),
                "tot_pf_pre", "tot_wealth_pre", "tot_wealth_pre_mean", "eq_share_of_w_pre_mean",
                "cash_liq_pre", "cash_locked_pre", "hypo_pre", "credit_check_pre",
                "equity_pre", "bond_pre", "anlagepaket_pre", "adv_segment_pre")
  stk <- cle[, ..keep_cle][stk, on = .(Bp_ID, ep_id)]
  stk[, ..load_cols]
}

# prep_est() from 07_estim_v3_placebo.R, reduced to what the benchmark needs
pb_prep_est <- function(d, win, ref, drop_nonpos_wealth = TRUE) {
  d <- d[between(rel_month, win[1], win[2])]
  d <- d[tot_wealth_pre > 0 & tot_wealth_pre_mean > 0]
  if (drop_nonpos_wealth) d <- d[tot_wealth > 0]
  d[, rel_month := as.integer(rel_month)]
  d[, log_w_pre_mean := log(tot_wealth_pre_mean)]
  d[]
}

pb_add_ids <- function(d) {
  d[, ci := .GRP, by = .(Bp_ID, ep_id)]
  d[, te := .GRP, by = .(MDate, ep_id)]
  d[]
}

# --- placebo episode drawing (make_eps / eligible_starts / draw_eps) ----------
pb_make_eps <- function(dd_start, len, pre_m, post_m) {
  e <- data.table(dd_start = as.Date(dd_start), n_len = as.integer(len))
  setorder(e, dd_start)
  e[, dd_end := pb_madd(dd_start, n_len - 1L)]
  e[, `:=`(ep_id     = sprintf("placebo_%s", format(dd_start, "%Y%m")),
           pre_month = pb_madd(dd_start, -1L),
           pre_start = pb_madd(dd_start, -pre_m),
           post_end  = pb_madd(dd_end, post_m),
           n_months  = n_len - 1L)]
  e[, next_start := shift(dd_start, type = "lead")]
  e[!is.na(next_start) & post_end >= next_start, post_end := pb_madd(next_start, -1L)]
  e[, prev_end := shift(dd_end)]
  e[!is.na(prev_end) & pre_start <= prev_end, pre_start := pb_madd(prev_end, 1L)]
  e[, c("next_start", "prev_end", "n_len") := NULL]
  e[]
}

pb_stress_months <- function(events, buf) {
  unique(do.call(c, lapply(seq_len(nrow(events)), \(i) {
    s <- pb_madd(events$start[i], -buf); e <- pb_madd(events$end[i], buf)
    pb_madd(s, 0:pb_mdiff(e, s))
  })))
}

pb_eligible_starts <- function(L, stress_m, smp_panel, pre_m, post_m, strict = FALSE) {
  k_max <- pb_mdiff(smp_panel[2], smp_panel[1]) - post_m - L + 1L
  cand  <- pb_madd(smp_panel[1], pre_m:k_max)
  offs  <- if (strict) -pre_m:(L - 1L + post_m) else -1L:(L - 1L)
  cand[vapply(seq_along(cand), \(i) !any(pb_madd(cand[i], offs) %in% stress_m), TRUE)]
}

pb_draw_eps <- function(seed, elig, lens, min_gap, pre_m, post_m, tries = 200L) {
  set.seed(seed)
  best <- NULL
  for (t in seq_len(tries)) {
    ord <- sample(lens)
    acc_s <- as.Date(character()); acc_l <- integer()
    for (L in ord) {
      cand <- elig[[as.character(L)]]
      if (length(acc_s))
        cand <- cand[vapply(seq_along(cand), \(i) all(abs(pb_mdiff(cand[i], acc_s)) >= min_gap), TRUE)]
      if (!length(cand)) next
      acc_s <- c(acc_s, cand[sample.int(length(cand), 1L)]); acc_l <- c(acc_l, L)
    }
    if (is.null(best) || length(acc_s) > nrow(best)) best <- data.table(s = acc_s, l = acc_l)
    if (length(acc_s) == length(lens)) break
  }
  pb_make_eps(best$s, best$l, pre_m, post_m)
}
