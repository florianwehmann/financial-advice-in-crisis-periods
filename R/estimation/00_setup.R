## =============================================================================
## 00_setup.R -- paths, parameters, helpers, logging
##
## Sourced by every other script in R/estimation/. Nothing here touches data.
## =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(lubridate)
  library(fixest)
  library(ggplot2)
})

setDTthreads(0)

## ---------------------------------------------------------------------------
## paths -- work from R/ (repo convention) or from the repo root
## ---------------------------------------------------------------------------

find_root <- function() {
  cand <- c(".", "..", "../..")
  for (p in cand) if (dir.exists(file.path(p, "data")) && dir.exists(file.path(p, "R")))
    return(normalizePath(p, winslash = "/"))
  stop("cannot locate repo root (need a folder containing data/ and R/)")
}

overleaf <- "C:/Users/FWehmann/Dropbox/Apps/Overleaf/Uncertainty and Financial Advise (1)/estimation"
overleaf_tab <- "C:/Users/FWehmann/Dropbox/Apps/Overleaf/Uncertainty and Financial Advise (1)/estimation/tables"
overleaf_fig <- "C:/Users/FWehmann/Dropbox/Apps/Overleaf/Uncertainty and Financial Advise (1)/estimation/figures"


ROOT     <- find_root()
DATA     <- file.path(ROOT, "data")
RESULTS  <- file.path(ROOT, "results")
TAB_DIR  <- file.path(RESULTS, "tables")
FIG_DIR  <- file.path(RESULTS, "figures")
# TAB_DIR  <- file.path(overleaf, "tables")
# FIG_DIR  <- file.path(overleaf, "figures")
CACHE    <- file.path(RESULTS, "cache")
for (p in c(RESULTS, TAB_DIR, FIG_DIR, CACHE)) dir.create(p, showWarnings = FALSE, recursive = TRUE)

## Where 09_export.R publishes the finished tables and figures. This is the
## Overleaf project folder, synced by Dropbox, so a re-run lands straight in the
## paper. Override with the CRISIS_EXPORT_DIR environment variable; 09 skips
## with a warning (it never errors) if the folder is not there, so the pipeline
## still runs on a machine without the Dropbox mount.
EXPORT_DIR <- Sys.getenv(
  "CRISIS_EXPORT_DIR",
  "C:/Users/FWehmann/Dropbox/Apps/Overleaf/Uncertainty and Financial Advise (1)/estimation")

POS_M_FILE <- file.path(DATA, "pos_m.parquet")
SPI_FILE   <- file.path(DATA, "hspitr_2.csv")   # SPI total return, daily, SIX export
SMI_FILE   <- file.path(DATA, "hsmi.csv")       # SMI price + SMIC total return

## ---------------------------------------------------------------------------
## parameters -- every TODO in R/estimation.R is resolved here, in one place
## ---------------------------------------------------------------------------

P <- list(
  ## sample
  smp_start       = as.Date("2011-03-31"),  # advice flags are empty before 2011 (see 01_audit)
  smp_end         = as.Date("2024-12-31"),

  ## cleaning
  min_bom_value   = 1000,    # CHF; below this the return denominator is noise
  win_p           = c(0.01, 0.99),
  min_wealth_pre  = 10000,   # CHF; pre-episode portfolio value floor

  ## episodes
  ## "events"   -- the hand-dated SMI/VIX stress windows in stress_events.R
  ##               (19 events, merged where they overlap at month level)
  ## "drawdown" -- the mechanical peak-to-trough rule on the daily index,
  ##               dd_threshold or deeper (3 usable episodes)
  episode_src     = "events",
  index_main      = "spi_tr",  # "spi_tr" or "smi"
  dd_threshold    = 0.15,      # peak-to-trough decline, daily closes
  ## two hand-dated events are one episode if their month-snapped windows
  ## overlap or sit this many months apart or less
  event_merge_gap = 0L,
  ## ESTIMATION BLOCKS. The events above are close together -- 16 of the 17
  ## episodes have a pre-window that overlaps the previous episode's recovery,
  ## and 10 reach back into the previous DRAWDOWN, so the reference period is
  ## itself a stress period. Consecutive episodes are therefore chain-merged
  ## into one estimation block when the next one starts this many months or
  ## fewer after the previous one ends. The originals are kept, with their
  ## labels, in results/cache/episodes_raw.parquet.
  ##   6L keeps Covid as its own block. 12L would chain volmageddon ->
  ##   xmas_plunge -> trade_war -> Covid into a single 27-month block, which
  ##   destroys the sharpest episode in the sample. 0L disables merging.
  ## ESTIMATION EPISODES, set by hand. Each element is one episode in the
  ## regressions; its value lists the stress_events.R tags that make it up.
  ## This replaces the automatic depth filter and gap chaining, which produced
  ## defensible but blunt groupings (a 24-month 2015-16 block, and Covid chained
  ## to volmageddon at gap 12). Grouping by hand keeps the economics explicit:
  ## the 2015-16 China/Fed sequence is one episode because it is one continuous
  ## risk-off phase, and the four 2022 events are one because inflation, the
  ## invasion, supply chains and the hawkish Fed are the same repricing.
  ## Events NOT listed here form no estimation episode. us_downgrade and
  ## snb_floor_in (2011) and yen_carry (2024) fall outside the usable sample
  ## anyway -- their pre or post window leaves the panel -- but they stay in
  ## episodes_raw and are still used to test whether a pre or post window
  ## contains stress.
  ep_groups = list(
    taper_tantrum = "taper_tantrum",
    snb_floor_out = "snb_floor_out",
    china_fed_oil = c("china_crash", "fed_hike", "china_oil"),
    brexit        = "brexit",
    trump         = "trump",
    volmageddon   = "volmageddon",
    xmas_plunge   = "xmas_plunge",
    trade_war     = "trade_war",
    covid         = "covid",
    y2022         = c("inflation", "ukraine", "supply_chain", "hawkish_fed"),
    svb_cs        = "svb_cs",
    geopol_rates  = "geopol_rates"
  ),
  ## fallback only, used when ep_groups is NULL
  ep_merge_gap    = 5L,
  ## MINIMUM DEPTH for an episode to enter an estimation block. The hand-dated
  ## list includes very mild events, and their only effect was to BRIDGE chains:
  ## the 2015-16 block ran 24 months only because fed_hike (-6.7%), brexit
  ## (-5.5%) and trump (-2.3%) linked four real drawdowns into one; svb_cs
  ## (-3.7%) stretched the 2022 block from 10 to 16 months, and trade_war
  ## (-3.3%) stretched 2018-11 from 3 to 8. Filtering on depth shortens the
  ## blocks by removing the bridges rather than by cutting a genuine drawdown in
  ## half. At 0.09 the six dropped events are all shallower than 7%, the longest
  ## block falls from 24 to 10 months, and the 2015 period separates into the
  ## SNB floor removal and the China/oil pair.
  ## The dropped episodes stay in episodes_raw and are still used to check
  ## whether a pre or post window contains stress.
  ep_min_depth    = 0.09,
  pre_months      = 12L,
  post_months     = 12L,

  ## EVENT-STUDY WINDOW. Blocks have very different lengths, so the stacked
  ## panel is badly unbalanced across rel_month: with the 2026-08 blocks all 6
  ## contribute only at rel_month -7..+9, two contribute out to +27 and ONE out
  ## to +35. Every coefficient outside the balanced core is identified off a
  ## shrinking subset of episodes, which is what produced the step changes in
  ## 06a_es_netflow.pdf. es_balance keeps only the rel_months where every usable
  ## block is present; es_window is an additional hard cap so the plot never
  ## runs longer than this even if the blocks happen to line up.
  ## Episodes kept OUT of the stacked event studies (matched on ep_label).
  ## They stay in every cross section -- see es_window() below for why.
  es_exclude      = c("brexit", "trump", "trade_war", "svb_cs"),
  es_balance      = TRUE,
  es_window       = c(-12L, 20L),

  ## FLOW WINSORISATION. Returns are winsorised WITHIN calendar month, because a
  ## -26% market month is legitimate and month-specific (see NOTES judgement
  ## calls). Flow RATIOS are different: a client moving 79% of their portfolio
  ## in one month is an outlier whatever the month, and winsorising by month is
  ## self-defeating exactly when it matters -- 2017-04 has its own p99 at 0.794
  ## against a median month's 0.197, so the by-month cut preserves the whole
  ## anomaly. Flows therefore get the by-month cut AND a pooled cap on top; the
  ## tighter of the two binds, so ordinary months are untouched.
  win_p_flow      = c(0.005, 0.995),

  ## behavioural thresholds
  derisk_thresh   = 0.20,    # net sales during dd > 20% of pre-episode wealth
  derisk_eq_drop  = 0.10,    # equity share falls by more than 10 percentage points
  liq_thresh      = 0.95,    # wealth falls below 5% of pre-episode wealth
  reentry_tol     = 0.10,    # "back in": cum. net flow within -10% of pre-episode
                             # wealth, or equity share within 10pp of its pre level
  forgone_h       = c(6L, 12L),  # horizons for the per-sale forgone return

  ## trade CHANNEL, from boerse$Medium -- who physically entered the order.
  ## This is the only field in the data that separates a client who logged in and
  ## sold from a sale the advisor or the mandate desk executed, so it is what
  ## turns "P(sell)" into "P(the client panicked and sold)".
  chan_self    = c("E-Banking", "e-Banking mobile"),
  chan_advisor = c("Telefon", "Besuch", "Brief", "Fax"),
  chan_mandate = c("Verwaltungsmandat"),
  ## bookings that are NOT a decision to trade: corporate actions posted through
  ## the trade table, and securities transferred in rather than bought
  chan_passive = c("Sec Event"),
  otype_passive = "Titeleingang|Vorauszahlung",

  ## ---------------------------------------------------------------------
  ## KNOWN DATA EVENTS -- bank actions recorded in the same fields as client
  ## behaviour. Both are removed AT SOURCE (02b and 02d) rather than winsorised
  ## downstream, because neither is an outlier in the statistical sense: they
  ## are correctly recorded events that simply are not what the field is meant
  ## to measure.
  ##
  ## 1. Dec 2014 -- a mass K_Anlegen MAILING. 31108 contacts against ~6000 in
  ##    the neighbouring months, 26080 of them by mail (94.1% of that month's
  ##    advice contacts, against 20.5% over the sample), 28334 flagged
  ##    advisor-initiated. It sits on rel_month -1 of the 2015-01 block -- the
  ##    reference period -- and pushes the raw advisor-initiated contact rate
  ##    there to 0.899 against ~0.05 in every other block-month. It also
  ##    inflates contacts_pre / adv_a_pre, which are CONTROLS in 06 and 07.
  ##    Only MAIL contacts in these months are dropped: the 1526 meetings and
  ##    3502 phone calls that month are real and are kept.
  ev_bulk_mail_months = as.Date("2014-12-31"),
  ##
  ## 2. Apr 2017 -- the LAUNCH of the bank's own fund, "Ant SGKB (CH) Fund -
  ##    Strategie Einkommen -A-" (Asset_ID 14034647). Trades in that asset run
  ##    0, 0, 1, 1259, 105, 76 over 2017-01..06: it barely exists, then 1210
  ##    clients buy CHF 71.5 m of it in one month, 23% of all buying that month.
  ##    That is a marketing campaign, not a portfolio decision, and it is what
  ##    made 2017-04 the fattest-tailed flow month in the sample (p99 0.794
  ##    against a median month's 0.197).
  ev_fund_launch_month = as.Date("2017-04-30"),
  ev_fund_launch_asset = 14034647,

  ## placebo
  n_placebo       = 25L,
  seed            = 20240818L
)

## ---------------------------------------------------------------------------
## logging -- every filter step writes one line to results/pipeline_log.txt
## ---------------------------------------------------------------------------

LOG_FILE <- file.path(RESULTS, "pipeline_log.txt")

log_init <- function(script) {
  cat(sprintf("\n===== %s | %s =====\n", script, format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      file = LOG_FILE, append = TRUE)
}

log_step <- function(msg, dt = NULL, id = "Bp_ID") {
  txt <- if (is.null(dt)) msg else
    sprintf("%-58s rows=%10s  clients=%8s", msg,
            format(nrow(dt), big.mark = "'"),
            if (id %in% names(dt)) format(uniqueN(dt[[id]]), big.mark = "'") else "-")
  cat(txt, "\n", sep = "", file = LOG_FILE, append = TRUE)
  message(txt)
  invisible(dt)
}

## a hard stop with a readable list of what is missing
require_cols <- function(dt, cols, where) {
  miss <- setdiff(cols, names(dt))
  if (length(miss))
    stop("\n[", where, "] required column(s) not found in the data:\n  - ",
         paste(miss, collapse = "\n  - "),
         "\nAvailable: ", paste(sort(names(dt)), collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

## ---------------------------------------------------------------------------
## small helpers
## ---------------------------------------------------------------------------

eom <- function(x) ceiling_date(as.Date(x), "month") - 1

## months between two month-end dates (integer, calendar based)
mdiff <- function(a, b) as.integer((year(a) - year(b)) * 12L + (month(a) - month(b)))

## a + k months, snapped to month end
madd <- function(a, k) eom(as.Date(a) %m+% months(k))

## min() over an empty selection: NA instead of Inf, and always numeric, so a
## grouped data.table j-expression keeps a stable column type
min_na <- function(x) if (!length(x)) NA_real_ else as.numeric(min(x, na.rm = TRUE))

winsor <- function(x, p = P$win_p) {
  q <- quantile(x, p, na.rm = TRUE, type = 7)
  pmin(pmax(x, q[1]), q[2])
}

## cumulative return over the NEXT n months (inclusive of t+1..t+n), NA if any month
## is missing. x must be sorted within group and cover a gap-free monthly grid.
roll_ret_fwd <- function(x, n) {
  lx <- log1p(x)
  s  <- frollsum(lx, n, align = "left", na.rm = FALSE)
  expm1(shift(s, 1L, type = "lead"))
}

roll_ret_bwd <- function(x, n) {
  lx <- log1p(x)
  expm1(frollsum(lx, n, align = "right", na.rm = FALSE))
}

## Drop data.table's cached secondary indices before writing. data.table builds
## one automatically on any `col == value` subset and keeps it as an ATTRIBUTE --
## one integer per row -- and arrow serialises R attributes into the parquet
## FOOTER. On the 5.9 m-row panel that is a 185 MB footer, which exceeds arrow's
## own thrift size limit: write_parquet succeeds and the file is then unreadable
## ("Couldn't deserialize thrift: Exceeded size limit"). stk was already carrying
## a 23 MB footer for the same reason. setindex(dt, NULL) costs nothing -- the
## index is a lookup cache, not data.
## duckdb session settings, used by 02c / 02e / 04b.
## The old fixed "memory_limit='6GB'" SEGFAULTS when the machine has less than
## that free -- duckdb takes the limit as a promise and dies rather than
## spilling. Take the smaller of 6 GB and half of actual free RAM, and always
## give it a temp directory so it can spill to disk instead.
duck_setup <- function(con, threads = 4L) {
  free_mb <- tryCatch({
    x <- system2("powershell", c("-NoProfile", "-Command",
      "(Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory"), stdout = TRUE)
    as.numeric(x[nzchar(x)][1]) / 1024
  }, error = function(e) NA_real_)
  lim_gb <- if (is.finite(free_mb)) max(1, min(6, floor(free_mb / 1024 / 2))) else 2
  tmp <- file.path(CACHE, "duckdb_tmp")
  dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
  DBI::dbExecute(con, sprintf("PRAGMA threads=%d", threads))
  DBI::dbExecute(con, sprintf("PRAGMA memory_limit='%dGB'", lim_gb))
  DBI::dbExecute(con, sprintf("PRAGMA temp_directory='%s'", tmp))
  message(sprintf("duckdb: %d threads, %d GB limit, spilling to %s", threads, lim_gb, tmp))
  invisible(lim_gb)
}

## Open a pdf device unless the file is locked. On Windows a PDF open in a
## viewer holds a lock and pdf() aborts, which would otherwise throw away a
## 9-minute regression run over a stale figure. Returns TRUE if the device
## opened; the caller draws and calls dev.off() only then.
pdf_ok <- function(path, ...) {
  ok <- tryCatch({ grDevices::pdf(path, ...); TRUE },
                 error = function(e) FALSE, warning = function(e) FALSE)
  if (!ok) message("  [figure skipped -- ", basename(path),
                   " is locked by another process; close the PDF viewer]")
  ok
}

## Retry on a transient lock. arrow MEMORY-MAPS parquet files, so any R session
## that has read a cache file (an open RStudio session with `cle` in its
## environment, for instance) holds it against rewriting and write_parquet fails
## with Windows error 1224, "cannot be performed on a file with a user-mapped
## section open". The lock is usually momentary, but a single collision was
## enough to kill a 20-minute pipeline run at step 04. Retry a few times, then
## fail with a message that says what to actually do about it.
## Write to a TEMP file, then swap it into place.
##
## arrow MEMORY-MAPS parquet files, and it cannot overwrite a path it still has
## mapped: write_parquet fails with Windows error 1224, "cannot be performed on
## a file with a user-mapped section open". The mapping survives read_parquet,
## so the very common pattern
##     x <- load_dt("cle");  ...;  save_dt(x, "cle")
## can fail on its own, with no other process involved -- and it did, killing
## the pipeline at step 04 repeatedly. Confirmed in a single clean R session:
## read the file, then write the same path, and it fails, while writing a NEW
## path succeeds and the OS reports the file perfectly writable.
##
## Writing a fresh temp file therefore always works; only the final rename
## touches the mapped path, and a gc() first drops any mapping R still holds.
## The retry loop remains for the genuine case of ANOTHER process (an open
## RStudio session) holding the file.
## ---------------------------------------------------------------------------
## Balanced event window for the STACKED EVENT STUDIES (06, 08, 11).
##
## Two steps, and they do different jobs:
##
##  1. Drop the episodes named in P$es_exclude. Episodes that sit only a few
##     months from a neighbour have their pre and post windows truncated by it,
##     and because the balance rule levels every episode down to the narrowest,
##     one 2-month pre-window collapses the whole stack. With all 12 episodes
##     the window is rel -2..+3, which leaves a single pre-period coefficient
##     and effectively no parallel-trends test. Excluding brexit, trump and
##     trade_war gives -4..+5 on 9 episodes.
##     Matched on ep_label, NOT ep_id: ep_id is positional and renumbers
##     whenever the episode list changes (see NOTES section 8).
##     These episodes are dropped from the EVENT STUDIES ONLY. They remain in
##     every cross section, counterfactual gap and the supply/demand logit,
##     which use each episode's own drawdown window and do not need balance.
##
##  2. Keep the rel_months covered by EVERY remaining episode, capped by
##     P$es_window. An unbalanced stack steps whenever an episode drops out,
##     which is what produced the kinks in the original netflow event study.
##
## Returns the rel_months to keep; the caller subsets on them.
## ---------------------------------------------------------------------------
es_window <- function(stk, ep, verbose = TRUE) {
  exl <- if (is.null(P$es_exclude)) character(0) else P$es_exclude
  bad <- setdiff(exl, ep$ep_label)
  if (length(bad))
    warning("P$es_exclude names episodes that do not exist: ",
            paste(bad, collapse = ", "), call. = FALSE)
  keep_ep <- ep[!ep_label %in% exl, ep_id]

  cov <- stk[smp_main == 1L & ep_id %in% keep_ep,
             .(n_ep = uniqueN(ep_id)), by = rel_month]
  full <- max(cov$n_ep)
  keep <- cov[n_ep == full, rel_month]
  if (!isTRUE(P$es_balance)) keep <- cov$rel_month
  keep <- keep[keep >= P$es_window[1] & keep <= P$es_window[2]]

  if (verbose) {
    cat("\nevent-study sample\n")
    cat("  episodes excluded (event study only): ",
        if (length(exl)) paste(exl, collapse = ", ") else "none", "\n", sep = "")
    cat("  episodes used: ", length(keep_ep), " of ", nrow(ep), "\n", sep = "")
    cat(sprintf("  balanced window: rel_month %d .. %d  (%d months, %d pre-period)\n",
                min(keep), max(keep), length(keep), sum(keep < -1)))
    dropped <- setdiff(cov$rel_month, keep)
    cat("  rel_months dropped for thin coverage: ",
        if (length(dropped)) paste(sort(dropped), collapse = ", ") else "none",
        "\n", sep = "")
  }
  keep
}

save_dt <- function(dt, name, tries = 6L, wait = 5) {
  if (data.table::is.data.table(dt)) data.table::setindex(dt, NULL)
  f   <- file.path(CACHE, paste0(name, ".parquet"))
  tmp <- file.path(CACHE, sprintf(".%s.tmp%d.parquet", name, Sys.getpid()))
  arrow::write_parquet(dt, tmp)
  for (i in seq_len(tries)) {
    gc(verbose = FALSE)                    # drop any mapping this session holds
    if (file.exists(f)) suppressWarnings(file.remove(f))
    if (!file.exists(f) && suppressWarnings(file.rename(tmp, f)))
      return(invisible(dt))
    if (i == tries) {
      stop("could not replace ", f, " after ", tries, " attempts.
",
           "  The temp file is at ", tmp, "
",
           "  Another process has the target memory-mapped -- usually an open
",
           "  RStudio session that has read it. Restart that R session
",
           "  (Ctrl+Shift+F10) and re-run.", call. = FALSE)
    }
    message(sprintf("  [%s locked, retry %d/%d in %.0fs]", basename(f), i, tries, wait))
    Sys.sleep(wait)
  }
  invisible(dt)
}
load_dt <- function(name) setDT(arrow::read_parquet(file.path(CACHE, paste0(name, ".parquet"))))
