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

  ## drawdown episodes
  index_main      = "spi_tr",  # "spi_tr" or "smi"
  dd_threshold    = 0.15,      # peak-to-trough decline, monthly closes
  pre_months      = 12L,
  post_months     = 12L,

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

save_dt <- function(dt, name) {
  if (data.table::is.data.table(dt)) data.table::setindex(dt, NULL)
  arrow::write_parquet(dt, file.path(CACHE, paste0(name, ".parquet")))
  invisible(dt)
}
load_dt <- function(name) setDT(arrow::read_parquet(file.path(CACHE, paste0(name, ".parquet"))))
