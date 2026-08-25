## ---------------------------------------------------------------------------
## FX conversion to CHF via ECB euro reference rates
##
## Source file: data/eurofxref-hist.csv (ECB, daily, quoted as 1 EUR = x CUR).
## CHF is one of the quoted currencies, so any pair is converted as a cross-rate
## through EUR:
##
##   1 CUR = (1 / CUR_per_EUR) EUR   and   1 EUR = CHF_per_EUR CHF
##   =>  1 CUR = CHF_per_EUR / CUR_per_EUR  CHF
##
## Main entry points (both vectorised, safe to use inside data.table `[`):
##   to_chf(x, currency, date, freq)          -> converted values
##   fx_rate_chf(currency, date, freq)        -> the CHF rates themselves
##
## Examples:
##   dt[, wert_chf := to_chf(Bruttowert, Handelswaehrung, DDate_Order)]
##   dtm[, wert_chf := to_chf(wert, "USD", MDate, freq = "monthly")]
## ---------------------------------------------------------------------------

library(data.table)

fx_path <- function() {
  # .Rproj lives in R/, so data/ is one level up. Override with
  # options(fx.file = "...") if the script is sourced from elsewhere.
  getOption("fx.file", "../data/eurofxref-hist.csv")
}

.fx_cache <- new.env(parent = emptyenv())


## --- rate tables ------------------------------------------------------------

# Daily CHF cross-rates in long format, keyed by (currency, Date).
# Columns: currency (chr), Date (Date), rate_chf (num) = CHF per 1 unit of currency.
# Result is cached; pass refresh = TRUE to re-read the CSV.
fx_rates_chf <- function(path = fx_path(), refresh = FALSE) {
  if (!refresh && !is.null(.fx_cache$daily)) return(.fx_cache$daily)

  raw <- fread(path, na.strings = c("N/A", "NA", "-", ""))

  # the ECB header ends with a trailing comma -> drop the empty tail column
  empty <- names(raw)[names(raw) == "" | vapply(raw, function(v) all(is.na(v)), logical(1))]
  if (length(empty)) raw[, (empty) := NULL]

  raw[, Date := as.Date(Date)]
  cur_cols <- setdiff(names(raw), "Date")
  raw[, (cur_cols) := lapply(.SD, as.numeric), .SDcols = cur_cols]

  long <- melt(raw, id.vars = "Date", variable.name = "currency",
               value.name = "per_eur", variable.factor = FALSE)
  long <- long[!is.na(per_eur) & per_eur > 0]

  chf <- long[currency == "CHF", .(Date, chf_per_eur = per_eur)]
  if (!nrow(chf)) stop("No CHF rates found in ", path)

  out <- merge(long, chf, by = "Date")
  out[, rate_chf := chf_per_eur / per_eur]
  out <- out[, .(currency, Date, rate_chf)]

  # EUR is the base currency and therefore not a column in the file
  out <- rbind(out, chf[, .(currency = "EUR", Date, rate_chf = chf_per_eur)])

  setkey(out, currency, Date)
  .fx_cache$daily <- out
  .fx_cache$monthly <- NULL
  out
}

# Monthly average of the daily CHF cross-rates, keyed by (currency, month),
# where `month` is the first day of the month.
fx_rates_chf_monthly <- function(path = fx_path(), refresh = FALSE) {
  if (!refresh && !is.null(.fx_cache$monthly)) return(.fx_cache$monthly)

  daily <- fx_rates_chf(path, refresh = refresh)
  out <- daily[, .(rate_chf = mean(rate_chf), n_days = .N),
               by = .(currency, month = .month_start(Date))]
  setkey(out, currency, month)
  .fx_cache$monthly <- out
  out
}

.month_start <- function(x) as.Date(cut(as.Date(x), "month"))


## --- lookup -----------------------------------------------------------------

# CHF rate per 1 unit of `currency` on `date`.
#   currency : ISO code(s), character or factor; length 1 or length(date).
#              "CHF" returns 1. Case-insensitive.
#   date     : Date (or coercible) vector.
#   freq     : "daily"   - exact date match, rolled forward over weekends and
#                          holidays (see `roll`).
#              "monthly" - `date` is mapped to its month and the monthly average
#                          rate is used; any day of the month gives the same
#                          result, so BOM/EOM date conventions both work.
#   roll     : daily only. TRUE (default) carries the last available rate
#              forward, 0 requires an exact match, a number caps the carry-
#              forward at that many days.
# Returns a numeric vector of length(date), NA where no rate is available.
fx_rate_chf <- function(currency, date, freq = c("daily", "monthly"),
                        roll = TRUE, path = fx_path()) {
  freq <- match.arg(freq)

  date <- as.Date(date)
  currency <- toupper(as.character(currency))
  n <- max(length(date), length(currency))
  if (length(date) == 1L)     date     <- rep(date, n)
  if (length(currency) == 1L) currency <- rep(currency, n)
  if (length(date) != n || length(currency) != n)
    stop("`currency` and `date` must have the same length (or be length 1)")

  rates <- if (freq == "daily") fx_rates_chf(path) else fx_rates_chf_monthly(path)
  .warn_unknown(setdiff(unique(currency[!is.na(currency)]), unique(rates$currency)))

  if (freq == "daily") {
    q <- data.table(currency = currency, Date = date)
    out <- rates[q, on = .(currency, Date), roll = roll, x.rate_chf]
  } else {
    q <- data.table(currency = currency, month = .month_start(date))
    out <- rates[q, on = .(currency, month), x.rate_chf]
  }

  # CHF -> CHF is 1 by definition, independent of the file
  out[!is.na(currency) & currency == "CHF"] <- 1
  out[is.na(currency) | is.na(date)] <- NA_real_
  out
}

.warn_unknown <- function(missing_cur) {
  missing_cur <- setdiff(missing_cur, "CHF")
  if (length(missing_cur))
    warning("No ECB rates for: ", paste(missing_cur, collapse = ", "),
            " - those rows return NA", call. = FALSE)
  invisible(NULL)
}


## --- conversion -------------------------------------------------------------

# Convert `x` (amounts denominated in `currency`) to CHF.
# Arguments as in fx_rate_chf(); `x` is recycled against currency/date the usual
# data.table way (all vectors must have the same length, or length 1).
to_chf <- function(x, currency, date, freq = c("daily", "monthly"),
                   roll = TRUE, path = fx_path()) {
  rate <- fx_rate_chf(currency, date, freq = freq, roll = roll, path = path)
  as.numeric(x) * rate
}


## --- quick check ------------------------------------------------------------
if (FALSE) {
  d <- data.table(
    Date  = as.Date(c("2020-03-16", "2020-03-21", "2022-01-31", "2026-07-31")),
    cur   = c("USD", "USD", "GBP", "CHF"),   # 2020-03-21 is a Saturday
    value = c(100, 100, 100, 100)
  )
  d[, chf_daily := to_chf(value, cur, Date)]
  d[, chf_month := to_chf(value, cur, Date, freq = "monthly")]
  print(d)

  # sanity: USD -> CHF should sit near 0.8-1.0 over the sample
  fx_rates_chf()[currency == "USD", summary(rate_chf)]
}
