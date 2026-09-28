# =============================================================================
# 05_episodes.R -- build and inspect the stress episodes the event study uses
#
# Two episode sets:
#   FULL  -- built by R/estimation/03_episodes.R out of P$ep_groups, read from
#            results/cache/episodes.parquet. 12 episodes, 8 of them estimated.
#   SHORT -- built HERE from a hand-picked list of stress_events.R tags, one
#            episode per event, no merging, with short windows. Written to
#            data/episodes_short.parquet.
#
# The point of SHORT is that dd_start..dd_end IS the treatment period, so a
# merged 9-month episode dilutes the treatment over months in which nothing is
# happening. One event, one short window.
#
# Both sets go through the same three checks:
#   1. spacing and rel_month reach  -> printed
#   2. mechanical stress scan       -> printed
#   3. figures                      -> 05_episodes{,_short}_timeline{,_labelled}
#                                      05_episodes{,_short}_windows
#                                      written to results/figures AND Overleaf
#
# Nothing here is read by the rest of the pipeline unless you point
# 06_prep.R line 9 at data/episodes_short.parquet. The hand-dated windows live
# in R/estimation/stress_events.R and are only ever sourced, never edited here.
#
# Run with the working directory set to R/estimation_new (as 06/07 are).
# One optional network call (VIX, S&P 500), cached; the script runs offline.
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(ggplot2)
  library(lubridate)
})

# ---- Config ------------------------------------------------------------------
CACHE <- "../../results/cache"
DATA  <- "../../data"

# every figure is written to both, so the paper and the repo never disagree.
# A directory that is not there (no Dropbox mount) is skipped with a message
# rather than killing the run.
overleaf_dir <- "C:/Users/FWehmann/Dropbox/Apps/Overleaf/Uncertainty and Financial Advise (1)"
FIG_DIRS <- c("../../results/figures", file.path(overleaf_dir, "figures"))
for (d in FIG_DIRS) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# the short episode table lands next to the other model inputs, so 06_prep.R
# reads it exactly the way it reads pos_aggm_c.parquet
SHORT_FILE <- file.path(DATA, "episodes_short.parquet")

SPI_FILE <- file.path(DATA, "hspitr_2.csv")  # SXGE, SPI total return, SIX export
SMI_FILE <- file.path(DATA, "hsmi.csv")      # SMI price
INDEX    <- "spi_tr"                         # index the episodes are dated on

# the panel sample -- must match P$smp_start / P$smp_end in 00_setup.R, because
# that is what decides whether an episode's windows fit inside the data
SMP_PANEL <- as.Date(c("2011-03-31", "2024-12-31"))

PRE_M  <- 6L     # months of pre window requested (truncated at the neighbour)
POST_M <- 8L     # months of recovery window requested (ditto)

# --- FULL set: which episodes 07_estim_new.R runs on, and over what window ----
# Restated rather than imported because 07 is run by hand; the check below says
# so if the two have drifted. ep_ids in the FULL set are POSITIONAL -- they
# renumber whenever P$ep_groups or P$smp_start in 00_setup.R changes.
EP_ESTIM <- c("ep1_201305", "ep2_201501", "ep3_201508", "ep6_201801",
              "ep7_201811", "ep9_202002", "ep10_202201", "ep12_202310")
ES_WIN   <- c(-8L, 12L)

# --- SHORT set ----------------------------------------------------------------
# One stress_events.R tag = one episode, in chronological order. Deliberately
# excluded: the 2015-12/2016-01 Fed-hike and China-oil legs (they are what turn
# the August 2015 spike into a six-month episode), brexit, trump, trade_war, the
# 2022 block and geopol_rates.
#
# ep_ids here are tag-based, NOT positional, so adding or removing an episode
# cannot silently renumber the others the way the FULL set does.
EP_SHORT <- c("taper_tantrum",   # 2013 May, taper tantrum
              "snb_floor_out",   # 2015 Jan, SNB abandons the EUR/CHF floor
              "china_crash",     # 2015 Aug-Oct, China devaluation / flash crash
              "volmageddon",     # 2018 Feb
              "xmas_plunge",     # 2018 Dec, Christmas Eve plunge
              "covid",           # 2020 Mar
              "svb_cs")          # 2023 Mar, SVB and Credit Suisse

# Optional: shorten a treatment window without touching stress_events.R.
# tag -> new end date; the start always comes from stress_events.R. Note the
# window is snapped to month ends, so an end of "2015-10-01" already buys the
# whole of October -- use "2015-09-30" to stop at September.
EP_SHORT_END <- c()   # e.g. c(china_crash = "2015-09-30")

# Window to test the SHORT set against. Matches PRE_M / POST_M: asking for more
# rel_months than the episodes are built to carry only buys unbalanced edges.
SHORT_WIN <- c(-6L, 8L)

# mechanical stress scan
dd_scan   <- 0.05    # depth below the trailing peak that counts as stress
scan_look <- 250L    # trading days in the trailing max (~12 months)
scan_join <- 40L     # merge two stretches less than this many trading days apart

eom   <- function(x) ceiling_date(as.Date(x), "month") - 1
madd  <- function(a, k) eom(as.Date(a) %m+% months(k))
mdiff <- function(a, b) as.integer((year(a) - year(b)) * 12L + (month(a) - month(b)))

# ---- Load --------------------------------------------------------------------
# the hand-dated windows, single source of truth for the episodes AND the figure
# labels. Sourced, never copied, so the two can never drift.
source("../estimation/stress_events.R")      # defines EVENTS

read_six <- function(file, skip, col, name) {
  x <- fread(file, skip = skip, select = c(1L, col), header = FALSE,
             col.names = c("Date", name))
  x[, Date := as.Date(Date, format = "%d.%m.%Y")]
  x <- x[!is.na(Date) & !is.na(get(name))]
  setorder(x, Date)
  x[]
}
spi <- read_six(SPI_FILE, 5L, 2L, "spi_tr")
smi <- read_six(SMI_FILE, 5L, 2L, "smi")

# the daily series the episodes are dated on
dly <- if (INDEX == "spi_tr") spi[, .(Date, lvl = spi_tr)] else smi[, .(Date, lvl = smi)]

ep_full <- setDT(read_parquet(file.path(CACHE, "episodes.parquet")))
setorder(ep_full, dd_start)

cat("FULL set:", nrow(ep_full), "episodes from episodes.parquet\n")
cat("SPI TR  :", format(range(spi$Date)), " n =", nrow(spi), "\n")
cat("SMI     :", format(range(smi$Date)), " n =", nrow(smi), "\n")

# =============================================================================
# 1. BUILD THE SHORT SET
#
# Same rules as 03_episodes.R, so the two tables are comparable:
#   dd_start / dd_end  month ends containing the window start / end
#   pre_month          rel_month -1, the reference month and covariate freeze
#   pre_start/post_end requested PRE_M / POST_M months, then truncated at the
#                      neighbouring episode on BOTH sides so no episode's
#                      reference months are another episode's drawdown
#   depth              deepest peak-to-trough inside the window, report only
# =============================================================================
build_episodes <- function(tags, ends = c(), pre_m = PRE_M, post_m = POST_M) {
  miss <- setdiff(tags, EVENTS$tag)
  if (length(miss))
    stop("tags not defined in stress_events.R: ", paste(miss, collapse = ", "),
         call. = FALSE)

  e <- EVENTS[match(tags, tag), .(ep_label = tag, peak_date = start, trough_date = end)]
  if (length(ends)) {
    ov <- intersect(names(ends), e$ep_label)
    e[match(ov, ep_label), trough_date := as.Date(ends[ov])]
  }
  if (e[, any(trough_date < peak_date)])
    stop("an EP_SHORT_END override ends before the event starts", call. = FALSE)
  setorder(e, peak_date)

  e[, `:=`(dd_start = eom(peak_date), dd_end = eom(trough_date))]
  e[, ep_id := sprintf("%s_%s", ep_label, format(dd_start, "%Y%m"))]
  e[, `:=`(members = ep_label, n_sub = 1L, n_events = 1L,
           pre_month = madd(dd_start, -1L),
           pre_start = madd(dd_start, -pre_m),
           post_end  = madd(dd_end,    post_m),
           n_months  = mdiff(dd_end, dd_start))]

  # truncate BOTH sides at the neighbouring episode
  e[, next_start := shift(dd_start, type = "lead")]
  e[!is.na(next_start) & post_end >= next_start, post_end := madd(next_start, -1L)]
  e[, prev_end := shift(dd_end)]
  e[!is.na(prev_end) & pre_start <= prev_end, pre_start := madd(prev_end, 1L)]
  e[, c("next_start", "prev_end") := NULL]

  e[, `:=`(n_pre = mdiff(dd_start, pre_start), n_post = mdiff(post_end, dd_end))]
  # rel_month -1 is the reference period, so an episode needs >= 1 pre month
  e[, usable := pre_start >= SMP_PANEL[1] & post_end <= SMP_PANEL[2] & n_pre >= 1L]

  # depth of the index inside the treatment window, for the report only
  w <- lapply(seq_len(nrow(e)), function(i) {
    s <- dly$Date >= e$peak_date[i] & dly$Date <= e$trough_date[i]
    if (!any(s)) return(c(NA_real_, NA_real_, NA_real_, NA_real_))
    d <- dly$Date[s]; v <- dly$lvl[s]
    dd <- v / cummax(v) - 1; j <- which.min(dd); k <- which.max(v[seq_len(j)])
    c(dd[j], as.numeric(d[j] - d[k]), as.numeric(d[k]), as.numeric(d[j]))
  })
  e[, `:=`(depth      = vapply(w, function(z) z[[1]], 0),
           n_days     = as.integer(vapply(w, function(z) z[[2]], 0)),
           idx_peak   = as.Date(vapply(w, function(z) z[[3]], 0), origin = "1970-01-01"),
           idx_trough = as.Date(vapply(w, function(z) z[[4]], 0), origin = "1970-01-01"))]

  # same column order as episodes.parquet, so this is a drop-in for 06_prep.R
  setcolorder(e, c("ep_label", "members", "n_sub", "n_events", "peak_date",
                   "trough_date", "dd_start", "dd_end", "depth", "n_days",
                   "idx_peak", "idx_trough", "ep_id", "pre_month", "pre_start",
                   "post_end", "n_months", "n_pre", "n_post", "usable"))
  e[]
}

ep_short <- build_episodes(EP_SHORT, EP_SHORT_END)

cat("\n-------------------------------------------------------------\n")
cat("SHORT SET -- ", nrow(ep_short), " episodes, one per stress event\n", sep = "")
cat("-------------------------------------------------------------\n")
print(ep_short[, .(ep_id, depth = round(depth, 3), dd_start, dd_end, n_months,
                   pre_start, n_pre, post_end, n_post, usable)])
if (!all(ep_short$usable))
  cat("\n!! not usable (window leaves the panel sample): ",
      paste(ep_short[usable == FALSE, ep_id], collapse = ", "), "\n", sep = "")

ok <- tryCatch({ write_parquet(ep_short, SHORT_FILE); TRUE },
               error = function(e) { cat("  [", basename(SHORT_FILE),
                                         " not written -- ", conditionMessage(e),
                                         "]\n", sep = ""); FALSE })
if (ok) cat("\nwrote ", normalizePath(SHORT_FILE, winslash = "/"),
            "\n  -> to estimate on it, change line 9 of 06_prep.R to\n",
            '       ep <- read_parquet("../../data/episodes_short.parquet")\n',
            "     set `eps` in 07_estim_new.R to\n       c(",
            paste0('"', ep_short[usable == TRUE, ep_id], '"', collapse = ", "), ")\n",
            "     and `win` to c(", SHORT_WIN[1], "L, ", SHORT_WIN[2], "L)\n",
            sep = "")

# =============================================================================
# 2. SPACING AND REACH -- is a set usable as an event study?
#
# An episode contributes at rel_month k only if its stack reaches k, and
# 06_prep.R clips every stack to [pre_start, post_end]. So the reach is
#       rel_month in [ -n_pre , n_months + n_post ]
# and a coefficient at k is identified off the episodes that reach k -- which is
# what the coverage ladder counts. Where that count falls, the coefficient
# changes COMPOSITION, not just precision.
#
# n_pre and n_post are already truncated at the neighbour, so a short reach
# means a crowded episode, not a short crisis.
# =============================================================================
add_reach <- function(ep, estim_ids) {
  ep <- copy(ep)
  setorder(ep, dd_start)
  ep[, in_estim := ep_id %chin% estim_ids]
  ep[, `:=`(reach_lo = -n_pre, reach_hi = n_months + n_post)]
  ep[, gap_prev := c(NA_integer_, mdiff(dd_start[-1], dd_end[-.N]))]
  ep[, gap_next := shift(gap_prev, type = "lead")]
  ep[]
}

report_spacing <- function(ep, win, set_name) {
  cat("\n-------------------------------------------------------------\n")
  cat(set_name, " -- SPACING AND REACH (window ", win[1], " .. ", win[2], ")\n", sep = "")
  cat("-------------------------------------------------------------\n")
  cat("gap_prev / gap_next = calm months between this episode's drawdown and its\n")
  cat("neighbour. reach_lo / reach_hi = the rel_months this episode's stack spans.\n\n")
  print(ep[, .(ep_id, depth = round(depth, 3), n_months, gap_prev, gap_next,
               reach_lo, reach_hi,
               covers_win = reach_lo <= win[1] & reach_hi >= win[2],
               in_estim)])

  est <- ep[in_estim == TRUE]
  if (!nrow(est)) return(invisible(NULL))
  cat("\ncoverage ladder -- how many of the ", nrow(est),
      " estimated episodes reach each rel_month:\n", sep = "")
  for (k in seq.int(win[1], win[2])) {
    have <- est[reach_lo <= k & reach_hi >= k]
    cat(sprintf("  %+4d  %d/%d  %-9s%s\n", k, nrow(have), nrow(est),
                strrep("#", nrow(have)),
                if (nrow(have) < nrow(est))
                  paste0("  missing: ",
                         paste(setdiff(est$ep_id, have$ep_id), collapse = ", "))
                else ""))
  }
  cat("\nBALANCED CORE (all ", nrow(est), " episodes present): ",
      max(est$reach_lo), " .. ", min(est$reach_hi), "\n", sep = "")
  bind_lo <- est[reach_lo == max(reach_lo), ep_id]
  bind_hi <- est[reach_hi == min(reach_hi), ep_id]
  cat("  binding on the left:  ", paste(bind_lo, collapse = ", "),
      "\n  binding on the right: ", paste(bind_hi, collapse = ", "), "\n", sep = "")
  cat("Outside that range the coefficients rest on a different, shrinking set of\n")
  cat("episodes. Either read only the balanced core, or drop the binding episode.\n")
  invisible(NULL)
}

ep_full  <- add_reach(ep_full,  EP_ESTIM)
ep_short <- add_reach(ep_short, ep_short$ep_id)   # every short episode is used

report_spacing(ep_full,  ES_WIN,    "FULL SET")
report_spacing(ep_short, SHORT_WIN, "SHORT SET")

# =============================================================================
# 3. IS ANYTHING MISSING? -- a mechanical scan for stress the hand list skips
#
# stress_events.R is hand-dated, so it can only be validated against something
# that is not. The scan finds every stretch in which the daily index sits at
# least dd_scan below its trailing 12-month peak, merges stretches that are
# close together, and reports the ones no hand-dated window covers.
#
# An uncovered stretch is not automatically an omission. One that lands in an
# episode's PRE window is a problem, because rel_month -1 is then measured in a
# market that is itself falling.
# =============================================================================
# The span the figures and the scan cover. Not the panel sample: the hand-dated
# windows that form no episode (2011, 2024) fall outside every pre/post window,
# and they are exactly the ones a reader needs to see.
SMP <- c(floor_date(min(c(ep_full$pre_start, ep_short$pre_start, EVENTS$start)), "year"),
         max(c(ep_full$post_end, ep_short$post_end, EVENTS$end)))

# the trailing max is computed on the FULL daily series, then filtered, so the
# first year is not lost to the rolling window
sc <- data.table(Date = dly$Date, lvl = dly$lvl)
sc[, tmax := frollapply(lvl, scan_look, max, align = "right", fill = NA)]
sc <- sc[!is.na(tmax) & Date %between% SMP]
sc[, dd := lvl / tmax - 1]

hit  <- which(sc$dd <= -dd_scan)
runs <- data.table()
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

  cat("\n-------------------------------------------------------------\n")
  cat("MECHANICAL STRESS SCAN on daily ", INDEX, "\n", sep = "")
  cat("(", 100 * dd_scan, "% below the trailing ", scan_look,
      "-day peak; stretches merged if <", scan_join,
      " trading days apart)\n", sep = "")
  cat("-------------------------------------------------------------\n")
  print(runs[, .(from, trough, to, depth = round(depth, 3), events)])
}

report_scan <- function(ep, set_name) {
  if (!nrow(runs)) return(invisible(NULL))
  hits_win <- function(d1, d2, a, b, id) paste(id[!(d2 < a | d1 > b)], collapse = ",")
  r <- copy(runs)
  r[, in_pre := vapply(seq_len(.N), function(i)
    hits_win(from[i], to[i], ep$pre_start, ep$pre_month, ep$ep_id), "")]
  r[, in_post := vapply(seq_len(.N), function(i)
    hits_win(from[i], to[i], madd(ep$dd_end, 1L), ep$post_end, ep$ep_id), "")]
  # stress that no hand-dated window names, OR that a window names but this set
  # does not turn into an episode -- both leave a falling market in a pre window
  r[, named_here := vapply(seq_len(.N), function(i)
    any(unlist(strsplit(ep$ep_label, " + ", fixed = TRUE)) %chin%
          strsplit(events[i], ",", fixed = TRUE)[[1]]), TRUE)]
  bad <- r[named_here == FALSE & in_pre != ""]

  cat("\n", set_name, " -- stress this set does not treat as an episode but that\n",
      "falls inside one of its PRE windows", if (nrow(bad)) ":" else ": none", "\n", sep = "")
  if (nrow(bad)) {
    print(bad[, .(from, trough, to, depth = round(depth, 3), events, in_pre)])
    cat("  -> rel_month -1 for ",
        paste(unique(unlist(strsplit(bad$in_pre, ","))), collapse = ", "),
        " is measured in a falling market.\n", sep = "")
  }
  invisible(NULL)
}
report_scan(ep_full,  "FULL SET")
report_scan(ep_short, "SHORT SET")

# =============================================================================
# 4. FIGURES
#
# Timeline: three panels on one x-axis, because the three things a reader checks
# about an episode list are exactly these -- where the market was (level), how
# far it actually fell (drawdown, which is what `depth` measures), and whether
# the fall was a volatility event at all (VIX).
#
# Shading separates three things usually drawn identically:
#   used   -- an episode the event study runs on
#   built  -- an episode in the table that is not estimated
#   unused -- a hand-dated stress window that forms no episode at all
#
# Window strip: one row per episode, showing what it actually CONTRIBUTES.
# Reading down a column says whether two episodes share calendar months; the row
# length says which rel_months the episode can identify. The timeline cannot
# answer that, because neighbouring windows there overlap into one band.
#
# VIX and the S&P 500 are optional: one network call, cached in
# results/cache/mkt_ext_d.parquet, and the VIX panel drops if neither cache nor
# network is there.
# =============================================================================
ext_file <- file.path(CACHE, "mkt_ext_d.parquet")
ext <- if (file.exists(ext_file))
  tryCatch(setDT(read_parquet(ext_file)), error = function(e) NULL) else NULL

if (is.null(ext) && requireNamespace("quantmod", quietly = TRUE)) {
  cat("\nfetching VIX and S&P 500 once (cached in ", basename(ext_file), ")\n", sep = "")
  ext <- tryCatch({
    grab <- function(sym, nm) {
      x <- quantmod::getSymbols(sym, src = "yahoo", auto.assign = FALSE,
                                from = SMP[1] - 400L, to = SMP[2] + 1L)
      data.table(Date = as.Date(zoo::index(x)),
                 value = as.numeric(quantmod::Cl(x)), series = nm)
    }
    rbindlist(list(grab("^VIX", "VIX"), grab("^GSPC", "SP500")))
  }, error = function(e) {
    cat("  [no network: ", conditionMessage(e), " -- VIX panel dropped]\n", sep = ""); NULL
  })
  if (!is.null(ext)) {
    ext <- ext[!is.na(value)]
    tryCatch(write_parquet(ext, ext_file), error = function(e) NULL)
  }
}
has_ext <- !is.null(ext) && nrow(ext) > 0

# ---- series, long (shared by both sets, so the figures are comparable) -------
SWISS <- if (INDEX == "spi_tr") "SPI TR" else "SMI"
PAN <- c(lvl = sprintf("Index level (%s = 100)", format(SMP[1], "%b %Y")),
         dd  = "Drawdown from the trailing 12-month peak (%)",
         vol = "VIX (implied volatility, % p.a.)")

rebase <- function(d) d[, .(Date, value = 100 * value / value[1])]
insmp  <- function(d) d[Date %between% SMP]

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

CLS <- c(used   = "episode used in the event study",
         built  = "episode in the table, not estimated",
         unused = "stress window, no episode")

# Label per episode, keyed on ep_label. Single-event episodes fall back to the
# stress_events.R label automatically; only merged groups need an entry here, so
# a regrouping shows up as the raw tag string rather than a silently wrong name.
EP_LAB <- c(
  setNames(gsub(",\n", " ", sub(":\n", "\n", EVENTS$label)), EVENTS$tag),
  "china_crash + fed_hike + china_oil"               = "2015-16\nChina, oil, 1st Fed hike",
  "inflation + ukraine + supply_chain + hawkish_fed" = "2022\nInflation, Ukraine,\nhawkish Fed")

# Greedy row packing: each label goes in the lowest row whose previous label has
# already ended. The width is estimated from the longest line, which is crude
# but needs no device query and does not collide at this figure size.
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

FILL <- setNames(c("#C0392B", "#41729F", "#B0B7BC"), CLS)   # rect fill
TXT  <- setNames(c("#8E2B20", "#2F5375", "#79838A"), CLS)   # label ink, darker
LINE <- c("SPI TR" = "grey15", "SMI" = "#2E86AB",
          "S&P 500 (USD)" = "#D08C34", "VIX" = "#7D3C98")
zero <- data.table(y = 0, panel = factor(PAN[["dd"]], levels = levels(ser$panel)))
FIG_W <- 13; FIG_H <- 8.5

make_timeline <- function(ep, set_name, with_labels) {
  # events that form no episode in THIS set
  grouped <- unlist(strsplit(ep$ep_label, " + ", fixed = TRUE))
  rect <- rbind(
    ep[, .(xmin = peak_date, xmax = trough_date,
           cls = fifelse(in_estim, CLS[["used"]], CLS[["built"]]))],
    EVENTS[!tag %chin% grouped, .(xmin = start, xmax = end, cls = CLS[["unused"]])])
  rect[, cls := factor(cls, levels = CLS)]

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
         title = paste0("Stress episodes in the Swiss market -- ", set_name),
         subtitle = sprintf(paste0(
           "%d hand-dated stress windows -> %d episodes, %d of them estimated. ",
           "Median treatment window %s months.\nShading marks the drawdown itself ",
           "(rel_month 0 to dd_end); the pre and post windows extend beyond it and ",
           "are truncated\nat the neighbouring episode -- see the companion window ",
           "strip for what each episode actually contributes."),
           nrow(EVENTS), nrow(ep), sum(ep$in_estim),
           format(median(ep$n_months + 1L)))) +
    theme_light(base_size = 11) +
    theme(legend.position = "bottom", legend.box = "vertical",
          legend.margin = margin(t = 0, b = 0),
          legend.key.height = unit(9, "pt"),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "grey92"),
          strip.text.y.left = element_text(colour = "grey20", angle = 90),
          strip.placement = "outside",
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey35", size = 8.2),
          plot.caption = element_text(colour = "grey45", size = 7.5, hjust = 0))

  if (!with_labels) return(p)

  # Every extra label row squeezes the series into a smaller slice of the panel,
  # so the excluded windows are named only when there are few of them. The short
  # set excludes 13, which would need seven rows and collide; there the grey
  # bands stay shaded but unlabelled and the legend carries the meaning.
  n_unused   <- EVENTS[!tag %chin% grouped, .N]
  name_unused <- n_unused <= 5L

  lab <- ep[, .(xmin = peak_date, xmax = trough_date,
                cls  = fifelse(in_estim, CLS[["used"]], CLS[["built"]]),
                text = fifelse(ep_label %chin% names(EP_LAB),
                               unname(EP_LAB[ep_label]), ep_label))]
  if (name_unused)
    lab <- rbind(lab, EVENTS[!tag %chin% grouped,
                             .(xmin = start, xmax = end, cls = CLS[["unused"]],
                               text = gsub(",\n", " ", sub(":\n", "\n", label)))])
  else
    p <- p + labs(caption = sprintf(
      "%d further hand-dated stress windows are shaded in grey but left unlabelled.", n_unused))
  lab[, cls := factor(cls, levels = CLS)]
  lab[, mid := xmin + (xmax - xmin) / 2]
  setorder(lab, mid)
  lab[, row := pack_rows(mid, text, as.numeric(diff(range(ser$Date))), FIG_W)]

  # the band sits above the top panel, which therefore needs headroom
  top_lvl <- levels(ser$panel)[1]
  y_hi <- max(ser[panel == top_lvl, value]); y_lo <- min(ser[panel == top_lvl, value])
  STEP <- 0.15 * (y_hi - y_lo)
  lab[, `:=`(panel = factor(top_lvl, levels = levels(ser$panel)),
             y = y_hi + row * STEP)]
  head_room <- data.table(Date = ser$Date[1], value = max(lab$y) + 0.9 * STEP,
                          panel = factor(top_lvl, levels = levels(ser$panel)))

  p +
    geom_blank(data = head_room, aes(Date, value)) +
    geom_segment(data = lab, aes(x = mid, xend = mid, y = y_hi, yend = y),
                 linewidth = 0.25, colour = "grey60") +
    geom_text(data = lab, aes(mid, y, label = text), vjust = 0,
              size = 2.5, lineheight = 0.95,
              colour = TXT[as.character(lab$cls)]) +
    coord_cartesian(clip = "off")
}

PHASE <- c("pre window", "drawdown (treatment)", "recovery window")
make_windows <- function(ep, set_name, win) {
  seg <- rbindlist(list(
    ep[, .(ep_id, in_estim, phase = PHASE[1], x = pre_start, xend = dd_start)],
    ep[, .(ep_id, in_estim, phase = PHASE[2], x = dd_start,  xend = dd_end)],
    ep[, .(ep_id, in_estim, phase = PHASE[3], x = dd_end,    xend = post_end)]))
  seg[, phase := factor(phase, levels = PHASE)]
  seg[, in_estim := factor(in_estim, levels = c(TRUE, FALSE))]
  seg <- seg[xend > x]
  ord <- ep[order(-dd_start), ep_id]
  seg[, ep_id := factor(ep_id, levels = ord)]

  ann <- ep[, .(ep_id = factor(ep_id, levels = ord),
                in_estim = factor(in_estim, levels = c(TRUE, FALSE)), post_end,
                txt = sprintf("%+d .. %+d", reach_lo, reach_hi))]

  p <- ggplot(seg, aes(y = ep_id)) +
    geom_segment(aes(x = x, xend = xend, yend = ep_id, colour = phase,
                     linewidth = phase), lineend = "butt") +
    geom_text(data = ann, aes(x = post_end, label = txt), hjust = -0.12,
              size = 2.6, colour = "grey30") +
    scale_colour_manual(values = c("#9EC4E0", "#C0392B", "#7FB07F"), name = NULL) +
    scale_linewidth_manual(values = c(3.5, 6, 3.5), guide = "none") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                 expand = expansion(mult = c(0.01, 0.08))) +
    labs(x = NULL, y = NULL,
         title = paste0("What each episode contributes -- ", set_name),
         subtitle = paste0(
           "Both sides are truncated at the neighbouring episode, so a short bar is a ",
           "crowded episode, not a short crisis.\nThe label on the right is the rel_month ",
           "range the episode can identify; the event study asks for ",
           win[1], " .. ", win[2], ".")) +
    theme_light(base_size = 11) +
    theme(legend.position = "bottom",
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "grey92"),
          strip.text.y.left = element_text(colour = "grey20", angle = 90),
          strip.placement = "outside",
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey35", size = 8.2))
  # only split the panel when the set actually has both kinds
  if (uniqueN(ep$in_estim) > 1)
    p <- p + facet_grid(in_estim ~ ., scales = "free_y", space = "free_y", switch = "y",
                        labeller = as_labeller(c(`TRUE` = "used in the event study",
                                                 `FALSE` = "not estimated")))
  p
}

# ---- write -------------------------------------------------------------------
cat("\n")
figs <- c(
  setNames(list(list(make_timeline(ep_full, "full set", FALSE), FIG_W, FIG_H),
                list(make_timeline(ep_full, "full set", TRUE),  FIG_W, FIG_H),
                list(make_windows(ep_full, "full set", ES_WIN), 11, 5.5)),
           c("05_episodes_timeline", "05_episodes_timeline_labelled",
             "05_episodes_windows")),
  setNames(list(list(make_timeline(ep_short, "short set", FALSE), FIG_W, FIG_H),
                list(make_timeline(ep_short, "short set", TRUE),  FIG_W, FIG_H),
                list(make_windows(ep_short, "short set", SHORT_WIN), 11, 4.5)),
           c("05_episodes_short_timeline", "05_episodes_short_timeline_labelled",
             "05_episodes_short_windows")))

for (d in FIG_DIRS) {
  if (!dir.exists(d)) {
    cat("  [", d, " is not there -- skipped]\n", sep = ""); next
  }
  for (nm in names(figs)) {
    for (dev in c("pdf", "png")) {
      f <- file.path(d, paste0(nm, ".", dev))
      ok <- tryCatch({ ggsave(f, figs[[nm]][[1]], width = figs[[nm]][[2]],
                              height = figs[[nm]][[3]], dpi = 200); TRUE },
                     error = function(e) {
                       cat("  [", basename(f), " not written -- ",
                           conditionMessage(e), "]\n", sep = ""); FALSE })
      if (ok) cat("  wrote ", normalizePath(f, winslash = "/"), "\n", sep = "")
    }
  }
}
