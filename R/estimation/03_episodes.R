## =============================================================================
## 03_episodes.R -- stress episodes, defined on the MARKET INDEX
##
## Never on a client's own portfolio: client-triggered windows produce mechanical
## mean reversion. Both index files are local (no network call).
##
## Two sources, selected by P$episode_src in 00_setup.R:
##   "events"   -- the hand-dated SMI/VIX stress windows in stress_events.R.
##                 19 events read off the SMI and the VIX, merged where their
##                 month-snapped windows touch. This is the default.
##   "drawdown" -- the old mechanical rule: every peak-to-trough decline of at
##                 least P$dd_threshold on the daily index. 3 usable episodes.
## Either way the output columns are identical, so nothing downstream changes.
##
## Output: results/cache/episodes.parquet, results/cache/index_m.parquet
##         results/03_episodes.txt, results/figures/03_episodes.pdf
##
## Sections 5 and 6 are diagnostic and do not feed anything downstream:
##   5   spacing and rel_month reach of the episodes 07_estim_new.R estimates on
##   5b  a mechanical drawdown scan, to find stress stress_events.R does not name
##   6   results/figures/03_episodes_timeline{,_labelled}.{pdf,png}
##   6b  results/figures/03_episodes_windows.{pdf,png}
## Section 6 makes the only network call in the pipeline (VIX and S&P 500 from
## Yahoo, cached in results/cache/mkt_ext_d.parquet) and skips itself offline.
## =============================================================================

source("../estimation/00_setup.R")
source("../estimation/stress_events.R")
log_init("03_episodes")

sink(file.path(RESULTS, "03_episodes.txt"), split = TRUE)
on.exit(sink(), add = TRUE)

## ---------------------------------------------------------------------------
## 1. index levels -- SPI total return (main) and SMI price (robustness)
## ---------------------------------------------------------------------------

read_six <- function(file, value_col, name) {
  x <- fread(file, skip = 5L, select = c(1L, value_col), header = FALSE,
             col.names = c("Date", name))
  x[, Date := as.Date(Date, format = "%d.%m.%Y")]
  x <- x[!is.na(Date) & !is.na(get(name))]
  setorder(x, Date)
  x[]
}

spi <- read_six(SPI_FILE, 2L, "spi_tr")   # SXGE - Swiss Performance Index SPI TR
smi <- fread(SMI_FILE, skip = 4L, select = c(1L, 2L), col.names = c("Date", "smi"))
smi[, Date := as.Date(Date, format = "%d.%m.%Y")]
smi <- smi[!is.na(Date) & !is.na(smi)]
setorder(smi, Date)

cat("SPI TR:", format(range(spi$Date)), " n =", nrow(spi), "\n")
cat("SMI   :", format(range(smi$Date)), " n =", nrow(smi), "\n\n")

## month-end closes (used for the counterfactual drift, not for dating episodes)
to_monthly <- function(x, col) {
  x[, .(lvl = last(get(col))), by = .(MDate = eom(Date))][, .(MDate, lvl)]
}
idx <- merge(to_monthly(spi, "spi_tr")[, .(MDate, spi_tr = lvl)],
             to_monthly(smi, "smi")[, .(MDate, smi = lvl)],
             by = "MDate", all = TRUE)
setorder(idx, MDate)
idx <- idx[MDate >= madd(P$smp_start, -13L) & MDate <= P$smp_end]
idx[, `:=`(spi_ret = spi_tr / shift(spi_tr) - 1,
           smi_ret = smi / shift(smi) - 1)]
save_dt(idx, "index_m")

lvl_col <- if (P$index_main == "spi_tr") "spi_tr" else "smi"
dly <- if (lvl_col == "spi_tr") spi[, .(Date, lvl = spi_tr)] else smi[, .(Date, lvl = smi)]
dly <- dly[Date >= madd(P$smp_start, -25L) & Date <= P$smp_end]

## ---------------------------------------------------------------------------
## 2a. the mechanical rule, dated on DAILY closes
##     Monthly closes miss most of a fast crash -- the Covid drawdown is -26% on
##     daily SPI TR but only -13% month-end to month-end, so a monthly rule would
##     drop the single sharpest episode in the sample. Episodes are therefore
##     found on daily data and then snapped to month ends.
## ---------------------------------------------------------------------------

dd_episodes <- function(dates, lvl, thresh) {
  dd  <- lvl / cummax(lvl) - 1
  grp <- cumsum(dd == 0)                        # a new group starts at each peak
  ep  <- data.table(i = seq_along(lvl), dd = dd, grp = grp)[
    , .(peak_i = min(i), trough_i = i[which.min(dd)], depth = min(dd)), by = grp][
      depth <= -thresh]
  if (!nrow(ep)) return(data.table())
  out <- ep[, .(peak_date   = dates[peak_i],
                trough_date = dates[trough_i],
                depth       = depth,
                n_days      = trough_i - peak_i)]
  setorder(out, peak_date)
  out[]
}

## ---------------------------------------------------------------------------
## 2b. the hand-dated stress windows
##     The windows come from stress_events.R and are taken as given: they ARE
##     the daily treatment window, so peak_date/trough_date are the event's own
##     start and end, not an index extremum. `depth` is measured inside the
##     window purely for the report -- nothing downstream reads it.
##
##     Two events are one episode when their month-snapped windows overlap or
##     sit at most P$event_merge_gap months apart. Without this the 2011 pair
##     (the September SNB floor sits INSIDE the August euro-crisis window) and
##     the 2022 pair (inflation ends and Ukraine begins in the same month) would
##     each produce two episodes covering the same months, and the post-window
##     truncation below would then hand the first of the pair a negative
##     recovery window.
## ---------------------------------------------------------------------------

event_episodes <- function(events, dates, lvl, gap = 0L) {
  e <- copy(events)
  setorder(e, start)
  e[, `:=`(m_start = eom(start), m_end = eom(end))]

  ## chain-merge: a new group starts only when this event's month window opens
  ## more than `gap` months after the running end of the group so far
  grp <- integer(nrow(e))
  run_end <- e$m_end[1]
  g <- 1L
  for (i in seq_len(nrow(e))) {
    if (i > 1L && mdiff(e$m_start[i], run_end) > gap) {
      g <- g + 1L
      run_end <- e$m_end[i]
    }
    if (e$m_end[i] > run_end) run_end <- e$m_end[i]
    grp[i] <- g
  }
  e[, grp := grp]

  out <- e[, .(peak_date   = min(start),
               trough_date = max(end),
               ep_label    = paste(tag, collapse = "+"),
               n_events    = .N), by = grp]
  out[, grp := NULL]

  ## depth of the index inside each window, for the report only
  w <- lapply(seq_len(nrow(out)), function(i) {
    s <- dates >= out$peak_date[i] & dates <= out$trough_date[i]
    if (!any(s)) return(list(NA_real_, NA_integer_, NA_real_, NA_real_))
    d  <- dates[s]
    v  <- lvl[s]
    dd <- v / cummax(v) - 1
    j  <- which.min(dd)
    k  <- which.max(v[seq_len(j)])
    list(dd[j], as.integer(d[j] - d[k]), as.numeric(d[k]), as.numeric(d[j]))
  })
  out[, `:=`(depth      = vapply(w, function(z) as.numeric(z[[1]]), 0),
             n_days     = vapply(w, function(z) as.integer(z[[2]]), 0L),
             idx_peak   = as.Date(vapply(w, function(z) as.numeric(z[[3]]), 0),
                                  origin = "1970-01-01"),
             idx_trough = as.Date(vapply(w, function(z) as.numeric(z[[4]]), 0),
                                  origin = "1970-01-01"))]
  setorder(out, peak_date)
  out[]
}

## ---------------------------------------------------------------------------
## 3. build the episode table
## ---------------------------------------------------------------------------

if (!P$episode_src %in% c("events", "drawdown"))
  stop("P$episode_src must be 'events' or 'drawdown', not '", P$episode_src, "'",
       call. = FALSE)

if (P$episode_src == "events") {
  ep <- event_episodes(EVENTS, dly$Date, dly$lvl, P$event_merge_gap)
  if (!nrow(ep)) stop("stress_events.R produced no events", call. = FALSE)
  cat("episode source: EVENTS -- ", nrow(EVENTS), " hand-dated windows merged into ",
      nrow(ep), " episodes (P$event_merge_gap = ", P$event_merge_gap, " months)\n\n",
      sep = "")
} else {
  ep <- dd_episodes(dly$Date, dly$lvl, P$dd_threshold)
  if (!nrow(ep))
    stop("no drawdown of at least ", 100 * P$dd_threshold, "% found on ", lvl_col,
         " between ", min(dly$Date), " and ", max(dly$Date),
         " -- lower P$dd_threshold in 00_setup.R", call. = FALSE)
  ep[, `:=`(ep_label = "drawdown", n_events = 1L,
            idx_peak = peak_date, idx_trough = trough_date)]
  cat("episode source: DRAWDOWN -- peak-to-trough >= ", 100 * P$dd_threshold,
      "% on daily ", lvl_col, ": ", nrow(ep), " episodes\n\n", sep = "")
}

## dd_start = month in which the decline begins (contains the daily peak)
## pre_month = last month end fully before the decline; it is the reference month
##             for the event study (rel_month == -1) and for all frozen covariates
ep[, `:=`(dd_start = eom(peak_date),
          dd_end   = eom(trough_date))]
ep[, ep_id := sprintf("ep%d_%s", .I, format(dd_start, "%Y%m"))]
ep[, `:=`(pre_month = madd(dd_start, -1L),
          pre_start = madd(dd_start, -P$pre_months),
          post_end  = madd(dd_end,    P$post_months),
          n_months  = mdiff(dd_end, dd_start))]

## truncate a post window at the start of the next episode so recovery windows
## never overlap the next crisis
ep[, next_start := shift(dd_start, type = "lead")]
ep[!is.na(next_start) & post_end >= next_start, post_end := madd(next_start, -1L)]
ep[, next_start := NULL]

## an episode is usable only if its full pre and post window is inside the sample
ep[, usable := pre_start >= P$smp_start & post_end <= P$smp_end]
ep[, `:=`(n_pre  = mdiff(dd_start, pre_start),
          n_post = mdiff(post_end, dd_end))]

cat("-------------------------------------------------------------\n")
cat("STRESS EPISODES (", P$episode_src, ") on daily ", lvl_col, "\n", sep = "")
cat("-------------------------------------------------------------\n")
print(ep[, .(ep_id,ep_label, peak_date, trough_date, depth = round(depth, 3),
             n_days, dd_start, dd_end, n_months, pre_start, post_end, n_post, usable)])
cat("\nINSPECT THIS TABLE BEFORE CONTINUING.\n")
cat("dd_start = month end of the month containing the window start (rel_month 0);\n")
cat("rel_month -1 (pre_month) is the last month end before the decline and is the\n")
cat("reference period and the freeze date for every covariate.\n")
cat("post_end is truncated at the month before the next episode's dd_start, so\n")
cat("n_post is the recovery window the episode actually gets (0 = none).\n")
cat("'usable' requires the full pre window to lie inside the sample, which starts\n")
cat("in ", format(P$smp_start), " because the advice fields are empty before 2011.\n\n",
    sep = "")

## for the record: what the other source would have given
if (P$episode_src == "events") {
  cat("for comparison, the mechanical ", 100 * P$dd_threshold, "% rule on daily ",
      lvl_col, ":\n", sep = "")
  print(dd_episodes(dly$Date, dly$lvl, P$dd_threshold)[
    , .(peak_date, trough_date, depth = round(depth, 3), n_days)])
} else {
  alt <- if (lvl_col == "spi_tr") smi[, .(Date, lvl = smi)] else spi[, .(Date, lvl = spi_tr)]
  alt <- alt[Date >= madd(P$smp_start, -25L) & Date <= P$smp_end]
  cat("for comparison, episodes on the alternative index:\n")
  print(dd_episodes(alt$Date, alt$lvl, P$dd_threshold)[
    , .(peak_date, trough_date, depth = round(depth, 3), n_days)])
}

## a pre window may still reach back into the previous episode's recovery window;
## that is allowed (each episode is its own stack) but must be visible
ep[, prev_post_end := shift(post_end)]
ep[, pre_overlaps_prev_post := !is.na(prev_post_end) & pre_start <= prev_post_end]
if (any(ep$usable & ep$pre_overlaps_prev_post))
  cat("\nNOTE: pre window overlaps the previous episode's recovery window for: ",
      paste(ep[usable & pre_overlaps_prev_post, ep_id], collapse = ", "),
      "\n  -> the pre-trend for those episodes is measured during another episode's\n",
      "     recovery. Check the episode-by-episode estimates in 07_robustness.\n", sep = "")
ep[, prev_post_end := NULL]

## a pre window that reaches back into the previous DRAWDOWN is worse: the
## reference period is then itself a stress month
ep[, prev_dd_end := shift(dd_end)]
ep[, pre_overlaps_prev_dd := !is.na(prev_dd_end) & pre_start <= prev_dd_end]
if (any(ep$usable & ep$pre_overlaps_prev_dd))
  cat("\nWARNING: pre window reaches back into the previous episode's DRAWDOWN for: ",
      paste(ep[usable & pre_overlaps_prev_dd, ep_id], collapse = ", "),
      "\n  -> some of those pre-period months are themselves stress months.\n", sep = "")
ep[, prev_dd_end := NULL]

## ---------------------------------------------------------------------------
## 3b. ESTIMATION BLOCKS -- merge episodes that sit too close together
##
## The individual episodes above are the RECORD: they keep their hand-dated
## labels and are written to episodes_raw.parquet untouched. They are not,
## however, usable as separate stacks. The events cluster, so a 12-month pre
## window almost always reaches into the previous episode: 16 of 17 pre windows
## overlap the previous recovery and 10 reach into the previous DRAWDOWN, which
## makes the reference period (rel_month -1) itself a stress month.
##
## Consecutive episodes are chain-merged into one block whenever the next one
## begins P$ep_merge_gap months or fewer after the previous one ends. The block
## is then one drawdown running from the first peak to the last trough.
##
## Merging alone is not enough: a chain can still leave a neighbour inside the
## pre window. The pre window is therefore truncated at the previous block's
## dd_end, exactly as post_end is already truncated at the next block's
## dd_start. Windows are then clean by construction, and n_pre shows what each
## block actually gets -- shorter, but honest, and rel_month -1 always exists.
## ---------------------------------------------------------------------------
save_dt(ep, "episodes_raw")
## keep the FULL list for the contamination check below: an event that forms no
## estimation episode is still a stress window, and if one lands in an episode's
## pre or post period that has to be visible
ep_all <- copy(ep)

## ---------------------------------------------------------------------------
## ESTIMATION EPISODES FROM THE HAND-SET GROUPS (P$ep_groups in 00_setup)
##
## Each group is one episode in the regressions, running from the earliest start
## to the latest end of its member events. This replaces the automatic depth
## filter and gap chaining: those were defensible but blunt, and grouping by
## hand keeps the economics explicit -- the 2015-16 China/Fed sequence is one
## episode because it is one continuous risk-off phase, and the four 2022 events
## are one because they are the same repricing.
##
## Events not named in any group form no episode. They are listed below and
## still count for the "is the reference period calm" test further down.
## ---------------------------------------------------------------------------
use_groups <- !is.null(P$ep_groups) && length(P$ep_groups)

if (use_groups) {
  gmap <- rbindlist(lapply(names(P$ep_groups),
                           function(g) data.table(grp = g, tag = P$ep_groups[[g]])))
  miss <- setdiff(gmap$tag, EVENTS$tag)
  if (length(miss))
    stop("P$ep_groups names tags that stress_events.R does not define: ",
         paste(miss, collapse = ", "), call. = FALSE)

  ev <- EVENTS[gmap, on = "tag"]
  setorder(ev, start)
  blk <- ev[, .(ep_label    = paste(tag, collapse = " + "),
                members     = paste(tag, collapse = ","),
                n_sub       = .N,
                n_events    = .N,
                peak_date   = min(start),
                trough_date = max(end)), by = grp]
  blk[, `:=`(dd_start = eom(peak_date), dd_end = eom(trough_date))]
  setorder(blk, dd_start)
  blk[, grp := NULL]

  cat("\n-------------------------------------------------------------\n")
  cat("ESTIMATION EPISODES, hand-set groups (P$ep_groups)\n")
  cat("-------------------------------------------------------------\n")
  cat(nrow(EVENTS), " stress events -> ", nrow(blk), " estimation episodes.\n", sep = "")
  unassigned <- setdiff(EVENTS$tag, gmap$tag)
  cat("events in no group (no episode of their own): ",
      if (length(unassigned)) paste(unassigned, collapse = ", ") else "none", "\n", sep = "")
} else {
  ## fallback: the automatic depth filter + gap chaining
  if (P$ep_min_depth > 0) ep <- ep[abs(depth) >= P$ep_min_depth]
  setorder(ep, dd_start)
  ep[, gap_prev := c(NA_integer_, mdiff(dd_start[-1], dd_end[-.N]))]
  ep[, blk := cumsum(c(1L, as.integer(gap_prev[-1] > P$ep_merge_gap)))]
  blk <- ep[, .(ep_label    = paste(ep_label, collapse = " + "),
                members     = paste(ep_id, collapse = ","),
                n_sub       = .N,
                n_events    = sum(n_events),
                peak_date   = min(peak_date),
                trough_date = max(trough_date),
                dd_start    = min(dd_start),
                dd_end      = max(dd_end)), by = blk]
  setorder(blk, dd_start)
  blk[, blk := NULL]
}

## depth over the whole block window, recomputed rather than taken from the
## deepest member: the block IS one drawdown now
wz <- lapply(seq_len(nrow(blk)), function(i) {
  s <- dly$Date >= blk$peak_date[i] & dly$Date <= blk$trough_date[i]
  if (!any(s)) return(c(NA_real_, NA_real_, NA_real_, NA_real_))
  d <- dly$Date[s]; v <- dly$lvl[s]
  dd <- v / cummax(v) - 1; j <- which.min(dd); k <- which.max(v[seq_len(j)])
  c(dd[j], as.numeric(d[j] - d[k]), as.numeric(d[k]), as.numeric(d[j]))
})
blk[, `:=`(depth      = vapply(wz, function(z) z[[1]], 0),
           n_days     = as.integer(vapply(wz, function(z) z[[2]], 0)),
           idx_peak   = as.Date(vapply(wz, function(z) z[[3]], 0), origin = "1970-01-01"),
           idx_trough = as.Date(vapply(wz, function(z) z[[4]], 0), origin = "1970-01-01"))]

blk[, ep_id := sprintf("ep%d_%s", .I, format(dd_start, "%Y%m"))]
blk[, `:=`(pre_month = madd(dd_start, -1L),
           pre_start = madd(dd_start, -P$pre_months),
           post_end  = madd(dd_end,    P$post_months),
           n_months  = mdiff(dd_end, dd_start))]

## truncate BOTH sides at the neighbouring block
blk[, next_start := shift(dd_start, type = "lead")]
blk[!is.na(next_start) & post_end >= next_start, post_end := madd(next_start, -1L)]
blk[, prev_end := shift(dd_end)]
blk[!is.na(prev_end) & pre_start <= prev_end, pre_start := madd(prev_end, 1L)]
blk[, c("next_start", "prev_end") := NULL]

blk[, `:=`(n_pre = mdiff(dd_start, pre_start), n_post = mdiff(post_end, dd_end))]
## rel_month -1 is the reference period, so a block needs at least one pre month
blk[, usable := pre_start >= P$smp_start & post_end <= P$smp_end & n_pre >= 1L]

cat("\n-------------------------------------------------------------\n")
cat("ESTIMATION BLOCKS (P$ep_merge_gap = ", P$ep_merge_gap, " months)\n", sep = "")
cat("-------------------------------------------------------------\n")
cat(nrow(ep), " episodes -> ", nrow(blk), " blocks. Windows are truncated at the\n",
    "neighbouring block on BOTH sides, so n_pre / n_post are what each block\n",
    "actually gets. The merged episodes keep their labels in ep_label.\n\n", sep = "")
print(blk[, .(ep_id, n_sub, ep_label, depth = round(depth, 3), dd_start, dd_end,
              pre_start, n_pre, post_end, n_post, usable)])

## the merge must have removed the contamination -- verify, do not assume.
## The meaningful test is whether the reference period is CALM, i.e. whether any
## hand-dated stress window still falls inside a block's pre window. Note that
## "pre window overlaps the previous block's recovery" is now true BY
## CONSTRUCTION and is not a defect: the months between two blocks are at once
## the previous block's recovery and the next block's run-up, and the two
## truncations meet in that gap. Only a stress window inside the pre period is
## a problem, because that is what makes rel_month -1 a crisis month.
blk[, prev_dd := shift(dd_end)]
blk[, ov_dd := !is.na(prev_dd) & pre_start <= prev_dd]
## checked against ep_all, i.e. INCLUDING the shallow episodes the depth filter
## removed -- they no longer form blocks, but they are still stress windows and
## a reference period that contains one is still contaminated
blk[, n_stress_in_pre := vapply(seq_len(.N), function(i)
  sum(ep_all$dd_end >= pre_start[i] & ep_all$dd_start < dd_start[i]), 0L)]
blk[, n_stress_in_post := vapply(seq_len(.N), function(i)
  sum(ep_all$dd_start > dd_end[i] & ep_all$dd_start <= post_end[i]), 0L)]
cat("
is the reference period calm?
")
cat("  blocks whose pre window contains a hand-dated stress window: ",
    sum(blk$usable & blk$n_stress_in_pre > 0), " of ", sum(blk$usable), "
", sep = "")
cat("  blocks whose pre window reaches into the previous DRAWDOWN:  ",
    sum(blk$usable & blk$ov_dd), " of ", sum(blk$usable), "
", sep = "")
cat("  blocks whose POST window contains a stress window (incl. the shallow
")
cat("    ones the depth filter removed):                            ",
    sum(blk$usable & blk$n_stress_in_post > 0), " of ", sum(blk$usable), "
", sep = "")
if (any(blk$usable & (blk$ov_dd | blk$n_stress_in_pre > 0)))
  cat("  !! still contaminated: ",
      paste(blk[usable & (ov_dd | n_stress_in_pre > 0), ep_id], collapse = ", "),
      "
", sep = "")
cat("(the pre window always sits inside the previous block's recovery window --
")
cat(" the gap between two blocks is both, so that overlap is mechanical.)
")
blk[, c("prev_dd", "ov_dd") := NULL]

epu <- blk[usable == TRUE]
if (!nrow(epu))
  stop("no block has a usable pre and post window inside ", P$smp_start, "..",
       P$smp_end, call. = FALSE)
cat("\nusable blocks:", nrow(epu), " -> ", paste(epu$ep_id, collapse = ", "), "\n")
cat("pre windows:  median ", median(epu$n_pre), " months (min ", min(epu$n_pre), ")\n",
    sep = "")
cat("recovery:     n_post = 0 for ", sum(epu$n_post == 0), " of ", nrow(epu),
    " blocks; median ", median(epu$n_post), " months\n", sep = "")

## everything downstream reads "episodes", so the BLOCKS are what is saved there.
## The individual stress episodes keep their labels in episodes_raw.
ep <- blk
save_dt(blk, "episodes")

## ---------------------------------------------------------------------------
## 4. figure
## ---------------------------------------------------------------------------
pl <- idx[!is.na(get(lvl_col))]
pl[, lvl100 := 100 * get(lvl_col) / get(lvl_col)[1]]
rect <- ep[, .(xmin = peak_date, xmax = trough_date, usable)]

p <- ggplot() +
  geom_rect(data = rect, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf,
                             fill = usable), alpha = 0.25) +
  geom_line(data = pl, aes(MDate, lvl100), linewidth = 0.7, colour = "grey20") +
  scale_fill_manual(values = c(`TRUE` = "#C0392B", `FALSE` = "grey50"),
                    labels = c(`TRUE` = "usable episode", `FALSE` = "window outside sample")) +
  labs(x = NULL, y = sprintf("%s (%s = 100)", toupper(lvl_col), format(pl$MDate[1], "%Y-%m")),
       fill = NULL,
       title = if (P$episode_src == "events") "Hand-dated SMI/VIX stress episodes"
               else sprintf("Drawdown episodes, peak-to-trough >= %.0f%%", 100 * P$dd_threshold)) +
  theme_light() + theme(legend.position = "bottom")

## ggsave() calls pdf() internally, so it dies the same way when the figure is
## open in a viewer -- and it would take the whole script with it AFTER the
## episode table has already been computed. Same treatment as 06 and 08: warn
## and carry on, because the blocks are the output that matters here.
figf <- file.path(FIG_DIR, "03_episodes.pdf")
if (pdf_ok(figf, width = 9, height = 4.5)) {
  print(p); dev.off()
} else {
  cat("
!! 03_episodes.pdf not written -- the file is locked by another
")
  cat("   process (a PDF viewer). The episode table above is unaffected.
")
}

## ---------------------------------------------------------------------------
## 5. SELECTION DIAGNOSTIC -- is this episode set usable as an event study?
##
## The event study in R/estimation_new/07_estim_new.R runs on a SUBSET of the
## blocks above, over a fixed relative window. Both are restated here rather
## than imported, because 07 lives in another folder and is run by hand; if the
## two drift apart the check below says so. Episode ids are POSITIONAL, so a
## change to P$ep_groups or P$smp_start silently renumbers them.
##
## An episode contributes at rel_month k only if its stack reaches k, and
## 06_prep.R clips every stack to [pre_start, post_end]. So the reach is
##       rel_month in [ -n_pre , n_months + n_post ]
## and a coefficient at k is identified off the episodes that reach k -- which
## is what the coverage ladder below counts. Where that count falls, the
## coefficient changes COMPOSITION, not just precision.
## ---------------------------------------------------------------------------

## the episodes 07_estim_new.R estimates on, and the window it asks for
EP_ESTIM <- c("ep1_201305", "ep2_201501", "ep3_201508", "ep6_201801",
              "ep7_201811", "ep9_202002", "ep10_202201", "ep12_202310")
ES_WIN   <- c(-8L, 12L)

miss_est <- setdiff(EP_ESTIM, blk$ep_id)
if (length(miss_est))
  cat("\n!! 07_estim_new.R names episodes this run does not build: ",
      paste(miss_est, collapse = ", "),
      "\n   ep_ids are positional -- re-read the block table above.\n", sep = "")

blk[, in_estim := ep_id %chin% EP_ESTIM]
blk[, `:=`(reach_lo = -n_pre, reach_hi = n_months + n_post)]
blk[, gap_prev := c(NA_integer_, mdiff(dd_start[-1], dd_end[-.N]))]
blk[, gap_next := shift(gap_prev, type = "lead")]

cat("\n-------------------------------------------------------------\n")
cat("SPACING AND REACH (event-study window ", ES_WIN[1], " .. ", ES_WIN[2], ")\n", sep = "")
cat("-------------------------------------------------------------\n")
cat("gap_prev / gap_next = calm months between this block's drawdown and its\n")
cat("neighbour. reach_lo / reach_hi = the rel_months this block's stack spans.\n")
cat("covers_win is TRUE only if reach_lo <= ", ES_WIN[1], " and reach_hi >= ",
    ES_WIN[2], ".\n\n", sep = "")
print(blk[, .(ep_id, ep_label, depth = round(depth, 3), gap_prev, gap_next,
              reach_lo, reach_hi,
              covers_win = reach_lo <= ES_WIN[1] & reach_hi >= ES_WIN[2],
              in_estim)])

est_blk <- blk[in_estim == TRUE]
if (nrow(est_blk)) {
  cat("\ncoverage ladder -- how many of the ", nrow(est_blk),
      " estimated episodes reach each rel_month:\n", sep = "")
  for (k in seq.int(ES_WIN[1], ES_WIN[2])) {
    have <- est_blk[reach_lo <= k & reach_hi >= k]
    cat(sprintf("  %+4d  %d/%d  %-8s%s\n", k, nrow(have), nrow(est_blk),
                strrep("#", nrow(have)),
                if (nrow(have) < nrow(est_blk))
                  paste0("  missing: ",
                         paste(setdiff(est_blk$ep_id, have$ep_id), collapse = ", "))
                else ""))
  }
  cat("\nBALANCED CORE (all ", nrow(est_blk), " episodes present): ",
      max(est_blk$reach_lo), " .. ", min(est_blk$reach_hi), "\n", sep = "")
  cat("Outside that range the coefficients rest on a different, shrinking set of\n")
  cat("episodes. Either read only the balanced core, or report a balanced-sample\n")
  cat("version next to the full one.\n")
}

## ---------------------------------------------------------------------------
## 5b. IS ANYTHING MISSING? -- a mechanical scan for stress the hand list skips
##
## stress_events.R is hand-dated, so it can only be validated against something
## that is not. The scan below finds every stretch in which the daily index sits
## at least dd_scan below its trailing 12-month peak, merges stretches that are
## close together, and reports the ones no hand-dated window covers.
##
## An uncovered stretch is not automatically an omission. One that lands in an
## estimated episode's PRE window is a problem, because rel_month -1 is then
## measured in a market that is itself falling.
## ---------------------------------------------------------------------------
dd_scan   <- 0.05    # depth below the trailing peak that counts as stress
scan_look <- 250L    # trading days in the trailing max (~12 months)
scan_join <- 40L     # merge two stretches less than this many trading days apart

## the trailing max is computed on the FULL daily series (which starts 25 months
## before smp_start) so the first year of the sample is not lost to the window
sc <- data.table(Date = dly$Date, lvl = dly$lvl)
sc[, tmax := frollapply(lvl, scan_look, max, align = "right", fill = NA)]
sc <- sc[!is.na(tmax) & Date >= P$smp_start & Date <= P$smp_end]
sc[, dd := lvl / tmax - 1]

hit   <- which(sc$dd <= -dd_scan)
uncov <- data.table()
if (length(hit)) {
  cut_at <- c(0L, which(diff(hit) > scan_join), length(hit))
  runs <- rbindlist(lapply(seq_len(length(cut_at) - 1L), function(k) {
    s <- sc[hit[(cut_at[k] + 1L):cut_at[k + 1L]]]
    data.table(from = min(s$Date), trough = s$Date[which.min(s$dd)],
               to = max(s$Date), depth = min(s$dd))
  }))
  runs[, covered := vapply(seq_len(.N), function(i)
    any(EVENTS$start <= to[i] & EVENTS$end >= from[i]), TRUE)]
  runs[, events := vapply(seq_len(.N), function(i)
    paste(EVENTS$tag[EVENTS$start <= to[i] & EVENTS$end >= from[i]],
          collapse = ","), "")]
  ## which block's pre / post window does an uncovered stretch fall into?
  hits_win <- function(d1, d2, a, b, id) paste(id[!(d2 < a | d1 > b)], collapse = ",")
  runs[, in_pre := vapply(seq_len(.N), function(i)
    hits_win(from[i], to[i], blk$pre_start, blk$pre_month, blk$ep_id), "")]
  runs[, in_post := vapply(seq_len(.N), function(i)
    hits_win(from[i], to[i], madd(blk$dd_end, 1L), blk$post_end, blk$ep_id), "")]
  uncov <- runs[covered == FALSE]

  cat("\n-------------------------------------------------------------\n")
  cat("MECHANICAL STRESS SCAN on daily ", lvl_col, "\n", sep = "")
  cat("(", 100 * dd_scan, "% below the trailing ", scan_look,
      "-day peak; stretches merged if <", scan_join,
      " trading days apart)\n", sep = "")
  cat("-------------------------------------------------------------\n")
  print(runs[, .(from, trough, to, depth = round(depth, 3), events)])
  cat("\nstretches NO hand-dated window covers",
      if (nrow(uncov)) ":" else ": none", "\n", sep = "")
  if (nrow(uncov))
    print(uncov[, .(from, trough, to, depth = round(depth, 3), in_pre, in_post)])
  bad <- uncov[in_pre != ""]
  if (nrow(bad))
    cat("\n!! UNNAMED STRESS INSIDE A PRE WINDOW -- the reference period of ",
        paste(unique(unlist(strsplit(bad$in_pre, ","))), collapse = ", "), "\n",
        "   sits in a decline stress_events.R does not name. Either add the\n",
        "   window there (the truncation then shortens the pre window) or say\n",
        "   so explicitly in the paper.\n", sep = "")
}

## ---------------------------------------------------------------------------
## 6. TIMELINE FIGURE -- level, drawdown and volatility with shaded episodes
##
## Three panels on one x-axis, because the three things a reader checks about an
## episode list are exactly these: where the market was (level), how far it
## actually fell (drawdown -- this is what `depth` measures), and whether the
## fall was a volatility event at all (VIX).
##
## The shading separates three things usually drawn identically:
##   used   -- an estimation episode 07_estim_new.R actually runs on
##   built  -- an estimation episode that exists but is not estimated
##   unused -- a hand-dated stress window that forms no episode at all
##
## Two versions are written, identical except for the label band on top.
##
## VIX and the S&P 500 are optional: they need one network call, are cached in
## results/cache/mkt_ext_d.parquet, and the figure drops the VIX panel if
## neither the cache nor the network is available. Nothing else here touches
## the network, so the script still runs offline.
## ---------------------------------------------------------------------------

ext_file <- file.path(CACHE, "mkt_ext_d.parquet")
ext <- if (file.exists(ext_file))
  tryCatch(setDT(arrow::read_parquet(ext_file)), error = function(e) NULL) else NULL

if (is.null(ext) && requireNamespace("quantmod", quietly = TRUE)) {
  cat("\nfetching VIX and S&P 500 once (cached in ", basename(ext_file), ")\n", sep = "")
  ext <- tryCatch({
    grab <- function(sym, nm) {
      x <- quantmod::getSymbols(sym, src = "yahoo", auto.assign = FALSE,
                                from = P$smp_start - 400L, to = P$smp_end + 1L)
      data.table(Date = as.Date(zoo::index(x)),
                 value = as.numeric(quantmod::Cl(x)), series = nm)
    }
    rbindlist(list(grab("^VIX", "VIX"), grab("^GSPC", "SP500")))
  }, error = function(e) {
    cat("  [no network: ", conditionMessage(e), " -- VIX panel dropped]\n", sep = ""); NULL
  })
  if (!is.null(ext)) {
    ext <- ext[!is.na(value)]
    tryCatch(arrow::write_parquet(ext, ext_file), error = function(e) NULL)
  }
}
has_ext <- !is.null(ext) && nrow(ext) > 0

## ---- series, long ----------------------------------------------------------
SWISS <- if (lvl_col == "spi_tr") "SPI TR" else "SMI"
PAN <- c(lvl = sprintf("Index level (%s = 100)", format(P$smp_start, "%b %Y")),
         dd  = "Drawdown from the trailing 12-month peak (%)",
         vol = "VIX (implied volatility, % p.a.)")

rebase <- function(d) d[, .(Date, value = 100 * value / value[1])]
insmp  <- function(d) d[Date >= P$smp_start & Date <= P$smp_end]

lvl_dt <- rbindlist(list(
  cbind(rebase(insmp(spi[, .(Date, value = spi_tr)])), series = "SPI TR"),
  cbind(rebase(insmp(smi[, .(Date, value = smi)])),    series = "SMI")))
if (has_ext)
  lvl_dt <- rbind(lvl_dt, cbind(
    rebase(insmp(ext[series == "SP500", .(Date, value)])), series = "S&P 500 (USD)"))
lvl_dt[, panel := PAN[["lvl"]]]

ser <- rbind(lvl_dt,
             sc[, .(Date, value = 100 * dd, series = SWISS, panel = PAN[["dd"]])])
if (has_ext)
  ser <- rbind(ser, insmp(ext[series == "VIX"])[
    , .(Date, value, series = "VIX", panel = PAN[["vol"]])])
ser[, panel := factor(panel, levels = PAN[c("lvl", "dd", if (has_ext) "vol")])]

## ---- shading ---------------------------------------------------------------
CLS <- c(used   = "estimated episode (07_estim_new.R)",
         built  = "episode built, not estimated",
         unused = "stress window, no episode")

grouped <- if (use_groups) unlist(P$ep_groups, use.names = FALSE) else character(0)
rect <- rbind(
  blk[, .(xmin = peak_date, xmax = trough_date,
          cls = fifelse(in_estim, CLS[["used"]], CLS[["built"]]))],
  EVENTS[!tag %chin% grouped, .(xmin = start, xmax = end, cls = CLS[["unused"]])])
rect[, cls := factor(cls, levels = CLS)]


## ---- labels ----------------------------------------------------------------
## Keyed on ep_label, so a regrouping in P$ep_groups shows up as the raw tag
## string rather than as a silently wrong name.
BLK_LAB <- c(
  "taper_tantrum"                                    = "2013 May\nTaper tantrum",
  "snb_floor_out"                                    = "2015 Jan\nSNB floor abandoned",
  "china_crash + fed_hike + china_oil"               = "2015-16\nChina, oil, 1st Fed hike",
  "brexit"                                           = "2016 Jun\nBrexit vote",
  "trump"                                            = "2016 Nov\nTrump election",
  "volmageddon"                                      = "2018 Feb\nVolmageddon",
  "xmas_plunge"                                      = "2018 Dec\nChristmas plunge",
  "trade_war"                                        = "2019 May\nUS-China trade war",
  "covid"                                            = "2020 Mar\nCOVID crash",
  "inflation + ukraine + supply_chain + hawkish_fed" = "2022\nInflation, Ukraine,\nhawkish Fed",
  "svb_cs"                                           = "2023 Mar\nSVB & Credit Suisse",
  "geopol_rates"                                     = "2023 Oct\nGeopolitics, rate peak")

lab <- rbind(
  blk[, .(xmin = peak_date, xmax = trough_date,
          cls  = fifelse(in_estim, CLS[["used"]], CLS[["built"]]),
          text = fifelse(ep_label %chin% names(BLK_LAB),
                         unname(BLK_LAB[ep_label]), ep_label))],
  EVENTS[!tag %chin% grouped,
         .(xmin = start, xmax = end, cls = CLS[["unused"]],
           text = gsub(",\n", " ", sub(":\n", "\n", label)))])
lab[, cls := factor(cls, levels = CLS)]
lab[, mid := xmin + (xmax - xmin) / 2]
setorder(lab, mid)

## Greedy row packing: each label goes in the lowest row whose previous label
## has already ended. The width is estimated from the longest line, which is
## crude but needs no device query and does not collide at this figure size.
pack_rows <- function(mid, text, span_days, width_in, chars_per_in = 13) {
  nch  <- vapply(strsplit(text, "\n", fixed = TRUE), function(s) max(nchar(s)), 0L)
  half <- (nch / chars_per_in) * (span_days / width_in) / 2
  row  <- integer(length(mid)); occupied <- numeric(0)
  for (i in seq_along(mid)) {
    r <- 1L
    while (r <= length(occupied) && occupied[r] > as.numeric(mid[i]) - half[i]) r <- r + 1L
    row[i] <- r
    occupied[r] <- as.numeric(mid[i]) + half[i]
  }
  row
}

FIG_W <- 13; FIG_H <- 8.5
lab[, row := pack_rows(mid, text, as.numeric(diff(range(ser$Date))), FIG_W)]

## the band sits above the top panel, which therefore needs headroom
top_lvl <- levels(ser$panel)[1]
y_hi    <- max(ser[panel == top_lvl, value])
y_lo    <- min(ser[panel == top_lvl, value])
STEP    <- 0.15 * (y_hi - y_lo)
lab[, `:=`(panel = factor(top_lvl, levels = levels(ser$panel)),
           y = y_hi + row * STEP)]
head_room <- data.table(Date = ser$Date[1], value = max(lab$y) + 0.9 * STEP,
                        panel = factor(top_lvl, levels = levels(ser$panel)))

## ---- the plot --------------------------------------------------------------
FILL <- setNames(c("#C0392B", "#41729F", "#B0B7BC"), CLS)   # rect fill
TXT  <- setNames(c("#8E2B20", "#2F5375", "#79838A"), CLS)   # label ink, darker
LINE <- c("SPI TR" = "grey15", "SMI" = "#2E86AB",
          "S&P 500 (USD)" = "#D08C34", "VIX" = "#7D3C98")
zero <- data.table(y = 0, panel = factor(PAN[["dd"]], levels = levels(ser$panel)))

ep_timeline <- function(with_labels) {
  p <- ggplot() +
    geom_rect(data = rect,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = cls),
              alpha = 0.30) +
    geom_hline(data = zero, aes(yintercept = y), linewidth = 0.3, colour = "grey60") +
    geom_line(data = ser, aes(Date, value, colour = series), linewidth = 0.45) +
    scale_fill_manual(values = FILL, drop = FALSE, name = NULL) +
    scale_colour_manual(values = LINE, breaks = unique(ser$series), name = NULL) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                 expand = expansion(mult = 0.012)) +
    facet_grid(panel ~ ., scales = "free_y", switch = "y",
               labeller = label_wrap_gen(30)) +
    labs(x = NULL, y = NULL,
         title = "Stress episodes in the Swiss market, 2011-2024",
         subtitle = sprintf(paste0(
           "%d hand-dated stress windows -> %d estimation episodes, %d of them estimated in 07_estim_new.R.\n",
           "Shading marks the drawdown itself (rel_month 0 to dd_end); the %d-month pre and %d-month post ",
           "windows extend beyond it and are truncated\nat the neighbouring episode -- see ",
           "03_episodes_windows for what each episode actually contributes."),
           nrow(EVENTS), nrow(blk), sum(blk$in_estim), P$pre_months, P$post_months)) +
    theme_light(base_size = 11) +
    theme(legend.position = "bottom", legend.box = "vertical",
          legend.margin = margin(t = 0, b = 0),
          legend.key.height = unit(9, "pt"),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "grey92"),
          strip.text.y.left = element_text(colour = "grey20", angle = 90),
          strip.placement = "outside",
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey35", size = 8.2))
  if (with_labels)
    p <- p +
      geom_blank(data = head_room, aes(Date, value)) +
      geom_segment(data = lab, aes(x = mid, xend = mid, y = y_hi, yend = y),
                   linewidth = 0.25, colour = "grey60") +
      geom_text(data = lab, aes(mid, y, label = text), vjust = 0,
                size = 2.5, lineheight = 0.95,
                colour = TXT[as.character(lab$cls)]) +
      coord_cartesian(clip = "off")
  p
}

## ---------------------------------------------------------------------------
## 6b. WINDOW STRIP -- one row per episode, calendar time on the x-axis
##
## The timeline above shades the drawdowns; this one shows what each episode
## actually CONTRIBUTES: pre window, drawdown, recovery window. Reading down a
## column tells you whether two episodes share calendar months, and the row
## length tells you which rel_months that episode can identify. It is the direct
## picture of the "are the episodes far enough apart" question, which the
## shaded timeline cannot answer because neighbouring windows there overlap into
## one band.
## ---------------------------------------------------------------------------
PHASE <- c("pre window", "drawdown", "recovery window")
seg <- rbindlist(list(
  blk[, .(ep_id, in_estim, phase = PHASE[1], x = pre_start,        xend = dd_start)],
  blk[, .(ep_id, in_estim, phase = PHASE[2], x = dd_start,         xend = dd_end)],
  blk[, .(ep_id, in_estim, phase = PHASE[3], x = dd_end,           xend = post_end)]))
seg[, phase := factor(phase, levels = PHASE)]
seg[, in_estim := factor(in_estim, levels = c(TRUE, FALSE))]
seg <- seg[xend > x]
ord <- blk[order(-dd_start), ep_id]
seg[, ep_id := factor(ep_id, levels = ord)]

ann <- blk[, .(ep_id = factor(ep_id, levels = ord),
               in_estim = factor(in_estim, levels = c(TRUE, FALSE)), post_end,
               txt = sprintf("%+d .. %+d", reach_lo, reach_hi))]

pw <- ggplot(seg, aes(y = ep_id)) +
  geom_segment(aes(x = x, xend = xend, yend = ep_id, colour = phase,
                   linewidth = phase), lineend = "butt") +
  geom_text(data = ann, aes(x = post_end, label = txt), hjust = -0.12,
            size = 2.6, colour = "grey30") +
  scale_colour_manual(values = c("#9EC4E0", "#C0392B", "#7FB07F"), name = NULL) +
  scale_linewidth_manual(values = c(3.5, 6, 3.5), guide = "none") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.06))) +
  facet_grid(in_estim ~ ., scales = "free_y", space = "free_y", switch = "y",
             labeller = as_labeller(c(`TRUE` = "estimated in 07_estim_new.R",
                                      `FALSE` = "built, not estimated"))) +
  labs(x = NULL, y = NULL,
       title = "What each episode contributes: pre window, drawdown, recovery",
       subtitle = paste0(
         "Both sides are truncated at the neighbouring episode, so a short bar is a ",
         "crowded episode, not a short crisis.\nThe label on the right is the rel_month ",
         "range the episode can identify; the event study asks for ",
         ES_WIN[1], " .. ", ES_WIN[2], ".")) +
  theme_light(base_size = 11) +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92"),
        strip.text.y.left = element_text(colour = "grey20", angle = 90),
        strip.placement = "outside",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey35", size = 8.2))

## ---- write everything ------------------------------------------------------
cat("\n")
figs <- list(`03_episodes_timeline`          = list(ep_timeline(FALSE), FIG_W, FIG_H),
             `03_episodes_timeline_labelled` = list(ep_timeline(TRUE),  FIG_W, FIG_H),
             `03_episodes_windows`           = list(pw, 11, 5.5))
for (nm in names(figs)) {
  pp <- figs[[nm]][[1]]
  for (dev in c("pdf", "png")) {
    f <- file.path(FIG_DIR, paste0(nm, ".", dev))
    ok <- tryCatch({ ggsave(f, pp, width = figs[[nm]][[2]],
                            height = figs[[nm]][[3]], dpi = 200); TRUE },
                   error = function(e) {
                     cat("  [", basename(f), " not written -- ",
                         conditionMessage(e), "]\n", sep = ""); FALSE })
    if (ok) cat("  wrote ", f, "\n", sep = "")
  }
}

log_step(sprintf("episodes (%s): %d found, %d usable", P$episode_src, nrow(ep), nrow(epu)))
sink()
