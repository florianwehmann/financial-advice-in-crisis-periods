## =============================================================================
## placebo_funs.R -- placebo-window machinery, shared by 16 and 17
##
## Sourced, never run on its own. Defines functions and one input cache, so the
## client x episode file rebuilt for the cross section (16) and the stacked
## monthly panel rebuilt for the event study (17) can never drift apart.
##
## Why anything is rebuilt at all: a placebo window has no cle.parquet and no
## stk.parquet, and re-running 04 + 04b + 05 for every draw is minutes per draw.
## The functions below reproduce exactly the slice of 04_treatment.R and
## 05_outcomes.R that those two specifications read, and both scripts VERIFY the
## rebuild against the real caches before drawing anything.
##
## Two shortcuts make a few hundred draws affordable. Both are exact, not
## approximations, and both rest on the panel being an explicit gap-free monthly
## grid per client (02_clean step 5):
##   - a window SUM is one cumulative-sum lookup minus another, instead of a
##     non-equi scan over 5.9 m rows for each window in each draw;
##   - the market counterfactual cf_mkt is client-invariant, so it is a ratio of
##     two compounded index levels rather than a cumprod over a stacked panel.
## =============================================================================

## input cache. Held in its own environment rather than in globals so that
## sourcing this file twice, or running 16 and 17 in one session, cannot leave a
## half-loaded panel behind.
PLD <- new.env(parent = emptyenv())

PL_PCOLS <- c("Bp_ID", "MDate", "observed", "wealth", "log_w", "n_assets",
              "ret_past12", "ret_next12", "discretionary", "main_bank_yn",
              "Hauptbetreuer_ID", "Anlagepaket", "MA_Kundensegment",
              "eq_share", "risky_share_c", "cash_free", "wealth_tot",
              "buysell", "sell", "c_advice")

## ---------------------------------------------------------------------------
## pl_load_inputs() -- panel, running totals, daily contact log, index.
## Idempotent: the second call is free.
## ---------------------------------------------------------------------------
##   light        : build only the contact running total. The cross-section
##                  outcomes need cumulative buysell / sell; the event study
##                  does not, and those two columns are 150 MB.
##   keep_clients : a function(pan) returning the Bp_IDs to retain. Use it to
##                  drop clients who can never enter the estimation sample under
##                  ANY placebo window -- the eligibility filters are all frozen
##                  at a pre-episode month, so a client failing them in every
##                  month of the panel fails them in every draw. Halves the
##                  panel, which is what lets several workers hold one each.
##                  Both scripts VERIFY the rebuild after this, so a filter that
##                  is too aggressive is caught rather than silently believed.
pl_load_inputs <- function(quiet = FALSE, light = FALSE, keep_clients = NULL) {
  if (!is.null(PLD$pan)) return(invisible(PLD))

  pan <- setDT(arrow::read_parquet(file.path(CACHE, "panel.parquet"),
                                   col_select = all_of(PL_PCOLS)))
  if (is.function(keep_clients)) {
    n0 <- uniqueN(pan$Bp_ID)
    keep <- keep_clients(pan)
    pan <- pan[Bp_ID %in% keep]
    if (!quiet)
      cat("
client pre-filter: ", length(keep), " of ", n0,
          " clients retained (", round(100 * length(keep) / n0), "%)
", sep = "")
  }
  setorder(pan, Bp_ID, MDate)

  ## client-level running totals, in panel order
  PLD$CUM <- if (light)
    pan[, .(MDate, k_cadv = cumsum(as.numeric(c_advice == 1L))), by = Bp_ID]
  else
    pan[, .(MDate,
            k_buysell = cumsum(buysell),
            k_sell    = cumsum(sell),
            k_cadv    = cumsum(as.numeric(c_advice == 1L))), by = Bp_ID]
  setkey(PLD$CUM, Bp_ID, MDate)
  PLD$light <- light

  pan[, c("buysell", "sell", "c_advice") := NULL]
  setkey(pan, MDate, Bp_ID)
  PLD$pan <- pan
  PLD$min <- pan[, min(MDate)]
  PLD$max <- pan[, max(MDate)]

  ## daily contact log, restricted to the flags the treatment variants use
  cd <- load_dt("contacts_d")[
    , .(Bp_ID, ContactDate, perf, perf_a_p, advice_a, advice_a_p, advice_c,
        perf_inv_a_p)]
  PLD$cd <- cd[perf == 1L | perf_a_p == 1L | advice_a == 1L |
                 advice_a_p == 1L | advice_c == 1L | perf_inv_a_p == 1L]

  ## compounded index level by month. Zero-filling a missing monthly return is
  ## what 05 does (mkt_ret <- 0 where spi_ret is NA).
  idx <- load_dt("index_m")[, .(MDate, spi_ret)]
  setorder(idx, MDate)
  idx[, kmkt := cumprod(1 + fifelse(is.na(spi_ret), 0, spi_ret))]
  setkey(idx, MDate)
  PLD$idx <- idx

  gc(verbose = FALSE)
  if (!quiet)
    cat("\npanel: ", format(PLD$min), " .. ", format(PLD$max), "  (",
        uniqueN(pan$MDate), " months, ", format(nrow(pan), big.mark = "'"),
        " rows)\n", sep = "")
  invisible(PLD)
}

pl_mkt_at <- function(m) PLD$idx[.(m), kmkt]

## ---------------------------------------------------------------------------
## build_cle(epx)
##   epx: an episode table with the 03_episodes columns ep_id, peak_date,
##        trough_date, dd_start, dd_end, pre_month, pre_start, post_end.
##   returns: one row per client x episode with the treatment variants, the
##        frozen pre-episode covariates, the sample flags and the cross-section
##        outcomes, winsorised exactly as 05 winsorises them.
## ---------------------------------------------------------------------------
##   light: skip the cross-section outcomes (flows, the counterfactual gaps and
##          the winsorisation). The event study reads none of them; contacts_pre
##          is a control and is always built.
build_cle <- function(epx, light = isTRUE(PLD$light)) {
  pan <- PLD$pan; CUM <- PLD$CUM; cd <- PLD$cd

  ## -- 04 step 1: frozen pre-episode covariates, clients present at pre_month
  base <- pan[.(unique(epx$pre_month)), nomatch = NULL][
    observed == 1L & wealth >= P$min_wealth_pre,
    .(pre_month = MDate, Bp_ID, wealth_pre = wealth, log_w_pre = log_w,
      n_assets_pre = n_assets, ret_past12_pre = ret_past12,
      discr_pre = discretionary, main_bank_pre = main_bank_yn,
      advisor_id = Hauptbetreuer_ID, anlagepaket_pre = Anlagepaket,
      segment_pre = MA_Kundensegment,
      eq_share_pre = eq_share, risky_share_pre = risky_share_c,
      cash_pre = cash_free, wealth_tot_pre = wealth_tot)]
  if (!nrow(base)) return(NULL)

  cle <- epx[, .(ep_id, pre_month, pre_start, dd_start, dd_end, post_end,
                 peak_date, trough_date)][base, on = "pre_month",
                                          allow.cartesian = TRUE]
  if (!nrow(cle)) return(NULL)

  ## -- 04 step 2: treatment on the exact DAILY window [peak_date, trough_date]
  trt <- cd[cle[, .(Bp_ID, ep_id, peak_date, trough_date)],
            on = .(Bp_ID, ContactDate >= peak_date, ContactDate <= trough_date),
            .(Bp_ID, ep_id, perf, perf_a_p, advice_a, advice_a_p, advice_c,
              perf_inv_a_p),
            allow.cartesian = TRUE, nomatch = NULL][
    , .(treat_perf         = as.integer(any(perf         == 1L)),
        treat_perf_a_p     = as.integer(any(perf_a_p     == 1L)),
        treat_adv          = as.integer(any(advice_a     == 1L)),
        treat_advice_a_p   = as.integer(any(advice_a_p   == 1L)),
        treat_cli          = as.integer(any(advice_c     == 1L)),
        treat_perf_inv_a_p = as.integer(any(perf_inv_a_p == 1L))),
      by = .(Bp_ID, ep_id)]
  cle <- trt[cle, on = .(Bp_ID, ep_id)]
  ## no contact row inside the window is a true 0, not "unknown" -- see 04
  for (v in c("treat_perf", "treat_perf_a_p", "treat_adv", "treat_advice_a_p",
              "treat_cli", "treat_perf_inv_a_p"))
    cle[is.na(get(v)), (v) := 0L]

  ## -- window sums: sum over [a, b] = k[b] - k[a - 1 month]. A missing k[a-1]
  ##    means the client's grid had not started yet, which is a genuine zero.
  KC <- setdiff(names(CUM), c("Bp_ID", "MDate"))   # light mode has k_cadv only
  cum_at <- function(m) {
    z <- CUM[cle[, .(Bp_ID, MDate = m)], ..KC]
    for (v in names(z)) z[is.na(get(v)), (v) := 0]
    z
  }
  lo_dd  <- cum_at(cle$pre_month)                  # pre_month == dd_start - 1m
  lo_pre <- cum_at(madd(cle$pre_start, -1L))
  cle[, contacts_pre := as.integer(round(lo_dd$k_cadv - lo_pre$k_cadv))]
  if (!light) {
    hi_dd <- cum_at(cle$dd_end)
    cle[, `:=`(net_chf  =   hi_dd$k_buysell - lo_dd$k_buysell,
               sell_chf = -(hi_dd$k_sell    - lo_dd$k_sell))]
  }

  ## -- the sample flags, which both specifications need
  cle[, smp_main := as.integer(discr_pre == 0L)]
  cle[, `:=`(smp_mainbank  = as.integer(main_bank_pre == 1L),
             smp_mb        = as.integer(main_bank_pre == 1L),
             smp_mb_exdisc = as.integer(smp_main == 1L & main_bank_pre == 1L))]
  if (light) return(cle[])

  ## -- point values at the end of the drawdown and at the end of the recovery
  at_month <- function(m, cols) pan[cle[, .(MDate = m, Bp_ID)], ..cols]
  ddv <- at_month(cle$dd_end, c("wealth", "eq_share", "risky_share_c",
                                "cash_free", "ret_next12"))
  env <- at_month(cle$post_end, "wealth")
  cle[, `:=`(w_dd               = ddv$wealth,
             eq_share_dd_end    = ddv$eq_share,
             risky_dd_end       = ddv$risky_share_c,
             cash_dd_end        = ddv$cash_free,
             ret_next12_from_dd = ddv$ret_next12,
             w_end              = env$wealth)]

  ## -- 05 family 3: behaviour over the drawdown
  cle[, `:=`(netflow_dd  = net_chf  / wealth_pre,
             sellflow_dd = sell_chf / wealth_pre,
             d_eq_dd     = eq_share_dd_end - eq_share_pre,
             d_risky_dd  = risky_dd_end    - risky_share_pre,
             d_cash_dd   = (cash_dd_end - cash_pre) / wealth_pre)]

  ## -- 05 family 2: wealth against the market counterfactual, rebased on the
  ##    pre-episode month exactly as 05 rebases cf_mkt on rel_month == -1
  cle[, `:=`(cf_mkt_dd  = pl_mkt_at(dd_end)   / pl_mkt_at(pre_month),
             cf_mkt_end = pl_mkt_at(post_end) / pl_mkt_at(pre_month))]
  cle[, `:=`(cf_gap_mkt_dd  = (w_dd  / wealth_pre) / cf_mkt_dd  - 1,
             cf_gap_mkt_end = (w_end / wealth_pre) / cf_mkt_end - 1)]
  cle[, cf_gap_mkt_rec := (1 + cf_gap_mkt_end) / (1 + cf_gap_mkt_dd) - 1]

  ## -- 05: the two flow ratios are winsorised POOLED, everything else BY EPISODE
  cle[, `:=`(netflow_dd_w  = winsor(netflow_dd,  c(0.01, 0.99)),
             sellflow_dd_w = winsor(sellflow_dd, c(0.00, 0.99)))]
  wcols <- c("cf_gap_mkt_dd", "cf_gap_mkt_end", "cf_gap_mkt_rec",
             "d_eq_dd", "d_risky_dd", "d_cash_dd", "ret_next12_from_dd")
  cle[, (paste0(wcols, "_w")) := lapply(.SD, winsor), by = ep_id, .SDcols = wcols]
  cle[]
}

## ---------------------------------------------------------------------------
## build_stk(cle, rels)
##   the stacked client x episode x month panel 05 writes to stk.parquet, for
##   the rel_months in `rels` only. Feed it an ALREADY FILTERED cle (the
##   estimation sample): 05 stacks every client x episode over the whole
##   [pre_start, post_end] window, which is 3.9 m rows and 20x more than any one
##   event study reads.
##
##   Months before a client's first observed month have no panel row, exactly as
##   in stk (the non-equi join in 05 cannot invent one). Here they come back
##   from the join as NA and feols drops them, which is the same estimation
##   sample -- see the verification in 17.
##
##   Outcomes built: w_rel, w_tot_rel, cf_gap_mkt, d_eq_pre, d_risky_pre. None
##   of the five is winsorised, because 05 winsorises only the cle-level `_w`
##   columns. w_tot_rel divides by wealth_tot_pre, which is only the client's
##   real total when the deposits are here -- i.e. on smp_mb.
## ---------------------------------------------------------------------------
build_stk <- function(cle, rels) {
  pan <- PLD$pan
  KEEP <- c("Bp_ID", "ep_id", "advisor_id", "dd_start", "pre_month",
            "wealth_pre", "wealth_tot_pre", "eq_share_pre", "risky_share_pre",
            "log_w_pre", "n_assets_pre", "contacts_pre", "anlagepaket_pre",
            "segment_pre", "ret_past12_pre",
            "treat_perf", "treat_perf_a_p", "treat_adv", "treat_advice_a_p",
            "treat_cli", "treat_perf_inv_a_p")
  b <- cle[, ..KEEP]

  ## one block per rel_month: madd() is vectorised over dates but not over k,
  ## and a month-end plus k months is the only safe way to walk the grid
  stk <- rbindlist(lapply(rels, function(k) {
    z <- copy(b)
    z[, `:=`(rel_month = k, MDate = madd(dd_start, k))]
    z
  }))

  m <- pan[stk[, .(MDate, Bp_ID)],
           .(wealth, wealth_tot, eq_share, risky_share_c)]
  stk[, `:=`(wealth = m$wealth, wealth_tot = m$wealth_tot,
             eq_share = m$eq_share, risky_share_c = m$risky_share_c)]

  ## the market counterfactual, rebased on the pre-episode month
  stk[, cf_mkt := pl_mkt_at(MDate) / pl_mkt_at(pre_month)]

  stk[, `:=`(w_rel        = wealth     / wealth_pre,
             w_tot_rel    = wealth_tot / wealth_tot_pre,
             d_eq_pre     = eq_share      - eq_share_pre,
             d_risky_pre  = risky_share_c - risky_share_pre)]
  stk[, cf_gap_mkt := w_rel / cf_mkt - 1]
  stk[]
}

## ---------------------------------------------------------------------------
## window geometry -- what a placebo standing in for each real episode must
## reproduce. Only the calendar position is randomised; the daily length and the
## pre/post month counts are held fixed, because the treatment rate scales with
## window length (1.4% in the 27-day ep12 against 12.7% in the 275-day ep10) and
## a common length would compare the real estimate against a differently
## powered regression.
## ---------------------------------------------------------------------------
pl_geometry <- function(ep) {
  g <- ep[, .(src_ep = ep_id, ep_label, dd_start,
              len_days = as.integer(trough_date - peak_date),
              n_months = mdiff(dd_end, dd_start),
              n_pre    = mdiff(dd_start, pre_start),
              n_post   = mdiff(post_end, dd_end))]
  setorder(g, dd_start)
  g[]
}

## ---------------------------------------------------------------------------
## pl_candidates(g, mode, stress, dmin, dmax, match_n_months)
##   feasible placebo start dates for one window geometry.
##   mode "any"  : anywhere the pre and post window still fit in the sample.
##   mode "calm" : and the window may not touch ANY hand-dated stress window,
##                 including the events that form no estimation episode -- a
##                 placebo landing on one of those is not a calm window either.
## ---------------------------------------------------------------------------
pl_candidates <- function(g, mode, stress, dmin, dmax, match_n_months = TRUE) {
  d  <- seq(dmin, dmax, by = "day")
  tr <- d + g$len_days
  ds <- eom(d)
  de <- eom(tr)
  ok <- madd(ds, -g$n_pre) >= dmin & madd(de, g$n_post) <= dmax
  if (match_n_months) ok <- ok & mdiff(de, ds) == g$n_months
  if (mode == "calm")
    ok <- ok & !Reduce(`|`, lapply(seq_len(nrow(stress)), function(i)
      d <= stress$e[i] & tr >= stress$s[i]))
  d[ok]
}

## ---------------------------------------------------------------------------
## pl_draw(cands, geo, max_attempts)
##   one full placebo episode SET: one window per real episode, with the month
##   windows [dd_start, dd_end] disjoint within the draw. Their pre and post
##   windows may overlap each other, exactly as the real episodes' do.
##
##   A draw is a whole SET, not a single window, because both specifications
##   carry episode and client fixed effects: the real estimate is identified off
##   variation within client across episodes, so a placebo that did not
##   reproduce the stacked structure would not be the same regression.
## ---------------------------------------------------------------------------
pl_draw <- function(cands, geo, max_attempts = 200L) {
  for (att in seq_len(max_attempts)) {
    taken <- data.table(s = as.Date(character()), e = as.Date(character()))
    out   <- vector("list", nrow(geo))
    ok    <- TRUE
    for (j in seq_len(nrow(geo))) {
      pool <- cands[[j]]
      if (nrow(taken)) {
        ps <- eom(pool); pe <- eom(pool + geo$len_days[j])
        pool <- pool[!Reduce(`|`, lapply(seq_len(nrow(taken)), function(i)
          ps <= taken$e[i] & pe >= taken$s[i]))]
      }
      if (!length(pool)) { ok <- FALSE; break }
      pk <- pool[sample.int(length(pool), 1L)]
      ds <- eom(pk); de <- eom(pk + geo$len_days[j])
      taken <- rbind(taken, data.table(s = ds, e = de))
      out[[j]] <- data.table(
        ep_id       = sprintf("pl%02d", j),
        src_ep      = geo$src_ep[j],
        ep_label    = geo$ep_label[j],
        peak_date   = pk,
        trough_date = pk + geo$len_days[j],
        dd_start    = ds,
        dd_end      = de,
        pre_month   = madd(ds, -1L),
        pre_start   = madd(ds, -geo$n_pre[j]),
        post_end    = madd(de,  geo$n_post[j]))
    }
    if (ok) return(rbindlist(out))
  }
  NULL
}

## ---------------------------------------------------------------------------
## pl_es_path(d, tag, fml, trt)
##   the rel_month x treatment coefficient path -- the only part of the event
##   study the placebo band is about. The reference month is appended as an
##   explicit zero so the plotted path has no gap at -1.
##
##   `segment_pre` is a 9-level factor of which the sample uses 3, so 6 x 11
##   all-zero interaction columns would otherwise be built and then dropped as
##   collinear on every single draw. droplevels() removes them and lean() drops
##   the residual/design objects nothing here reads: together they take the fit
##   from 8.6 s to 6.0 s, and both leave the coefficients bit-identical (which
##   is what the verification step in 17 checks).
## ---------------------------------------------------------------------------
pl_es_path <- function(d, tag, fml, trt) {
  if (is.factor(d$segment_pre)) d[, segment_pre := droplevels(segment_pre)]
  m <- tryCatch(feols(fml, d, vcov = ~ advisor_id, notes = FALSE, lean = TRUE),
                error = function(e) NULL)
  if (is.null(m)) return(NULL)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, 2:5, c("est", "se", "t", "p"))
  ct <- ct[grepl(paste0(":", trt, "$"), term)]
  if (!nrow(ct)) return(NULL)
  ct[, rel_month := as.integer(sub("^rel_month::(-?\\d+):.*$", "\\1", term))]
  out <- rbind(ct[, .(tag, rel_month, est, se, p)],
               data.table(tag = tag, rel_month = -1L, est = 0, se = 0,
                          p = NA_real_))
  setorder(out, rel_month)
  out[, n := nobs(m)]
  out[]
}

## ---------------------------------------------------------------------------
## pl_one_draw(md, r, cands, K) -- one placebo draw, end to end.
##
##   K is a list of PLAIN values (no environments): seed, modes, max_attempts,
##   geo, pl_eps, sample, segs, trt, min_treated, keep_rel, fml. It lives here
##   rather than in the calling script precisely so that a parallel worker gets
##   this function by sourcing this file instead of by serialising a closure --
##   a closure defined in the script would carry the script's environment, and
##   that environment holds the panel.
##
##   Seeded from (mode, draw) rather than once per mode, so draws are
##   independent of the order they run in: a resumed or parallel run redraws
##   exactly the windows a single serial run would have drawn.
## ---------------------------------------------------------------------------
pl_one_draw <- function(md, r, cands, K) {
  set.seed(K$seed + 1000L * match(md, K$modes) + r)
  epx <- pl_draw(cands, K$geo, K$max_attempts)
  if (is.null(epx)) return(NULL)
  epx <- epx[ep_id %in% K$pl_eps]
  cle <- build_cle(epx)
  if (is.null(cle)) return(NULL)
  d <- cle[cle[[K$sample]] == 1L][segment_pre %in% K$segs]
  if (!nrow(d) || sum(d[[K$trt]]) < K$min_treated) return(NULL)
  pth <- pl_es_path(build_stk(d, K$keep_rel), md, K$fml, K$trt)
  if (is.null(pth)) return(NULL)
  pth[, `:=`(mode = md, draw = r, n_cells = nrow(d),
             n_treated = sum(d[[K$trt]]), trt_rate = mean(d[[K$trt]]))]
  list(path = pth,
       win  = epx[, .(mode = md, draw = r, ep_id, src_ep, peak_date,
                      trough_date, dd_start, dd_end, pre_start, post_end)])
}
