## smi_sp500 -- daily SMI (local file) and S&P 500 (Yahoo) series

library(data.table)
library(jsonlite)

# rm(list = ls()); gc()


## ---------------------------------------------------------------------------
## SMI (data/hsmi.csv)
## ---------------------------------------------------------------------------

# hsmi.csv: ";"-separated, 5 header rows, dates descending, dd.mm.yyyy
# col 1 = DATE, col 2 = SMI PR (price index), col 5 = SMIC (SMI total return)

smi <- fread("../data/hsmi.csv", skip = 4, select = c(1, 2, 5))
setnames(smi, c("Date", "smi", "smi_tr"))
smi[, Date := as.Date(Date, format = "%d.%m.%Y")]
smi <- smi[!is.na(Date) & !is.na(smi)]
setorder(smi, Date)

smi[, `:=`(smi_ret    = smi / shift(smi) - 1,
           smi_tr_ret = smi_tr / shift(smi_tr) - 1)]
setkey(smi, Date)


ggplot(smi[Date>="2020-01-01" & Date<"2020-03-01"],aes(x=Date,y=smi))+geom_line()


max_smi <- max(smi[Date>="2020-01-01" & Date<"2020-07-01"]$smi,na.rm=T)
min_smi <- min(smi[Date>="2020-01-01" & Date<"2020-07-01"]$smi,na.rm=T)
smi[smi==max_smi]
smi[smi==min_smi]


## ---------------------------------------------------------------------------
## S&P 500 (^GSPC) from Yahoo Finance
## ---------------------------------------------------------------------------

# same range as the Yahoo history page:
# https://finance.yahoo.com/quote/%5EGSPC/history/?period1=-1325583000&period2=1786544608
# the v8 chart endpoint returns JSON and needs no cookie/crumb (unlike the CSV download)

get_yahoo_daily <- function(symbol  = "^GSPC",
                            period1 = -1325583000,
                            period2 = as.integer(Sys.time())) {
  url <- sprintf(paste0("https://query1.finance.yahoo.com/v8/finance/chart/%s",
                        "?period1=%.0f&period2=%.0f&interval=1d&events=div%%2Csplit"),
                 URLencode(symbol, reserved = TRUE), period1, period2)

  js <- fromJSON(url, simplifyVector = TRUE)
  if (!is.null(js$chart$error)) stop("Yahoo error: ", js$chart$error$description)

  res <- js$chart$result
  q   <- res$indicators$quote[[1]]

  dt <- data.table(
    Date     = as.Date(as.POSIXct(res$timestamp[[1]], origin = "1970-01-01", tz = "UTC")),
    open     = as.numeric(q$open[[1]]),
    high     = as.numeric(q$high[[1]]),
    low      = as.numeric(q$low[[1]]),
    close    = as.numeric(q$close[[1]]),
    volume   = as.numeric(q$volume[[1]]),
    adjusted = as.numeric(res$indicators$adjclose[[1]]$adjclose[[1]])
  )

  dt <- dt[!is.na(close)]
  setorder(dt, Date)
  unique(dt, by = "Date")
}

sp500 <- get_yahoo_daily("^GSPC", period1 = -1325583000, period2 = 1786544608)
sp500[, `:=`(sp500     = close,
             sp500_ret = close / shift(close) - 1)]
setkey(sp500, Date)

ggplot(sp500[Date>="2018-01-01"],aes(x=Date,y=sp500))+geom_line()

## ---------------------------------------------------------------------------
## combined daily panel (trading days of either market)
## ---------------------------------------------------------------------------

idx <- merge(smi[, .(Date, smi, smi_tr, smi_ret, smi_tr_ret)],
             sp500[, .(Date, sp500, sp500_ret)],
             by = "Date", all = TRUE)
setkey(idx, Date)

# long format, handy for ggplot facets
idx_l <- melt(idx[, .(Date, smi, sp500)],
              id.vars = "Date", variable.name = "index", value.name = "close",
              na.rm = TRUE)

summary(idx[Date >= "2009-01-01", .(smi_ret, sp500_ret)])


ggplot(idx[Date>="2018-01-01" & !is.na(smi)],aes(x=Date))+
  geom_line(aes(y=smi/smi[1],color="smi"))+
  geom_line(aes(y=sp500/sp500[1],color="sp500"))

## ---------------------------------------------------------------------------
## monthly panel of both return series
## ---------------------------------------------------------------------------

last_obs <- function(x) { x <- x[!is.na(x)]; if (length(x)) x[length(x)] else NA_real_ }

# month-end level = last available quote of the month (per index)
dtm <- idx[Date >= "2010-12-01",
           .(Date  = last(Date),
             smi   = last_obs(smi),
             sp500 = last_obs(sp500)),
           by = .(ym = format(Date, "%Y-%m"))]

dtm <- idx[Date >= "2010-12-01",
           .(Date  = first(Date),
             smi   = mean(smi,na.rm=T),
             sp500 = mean(sp500,na.rm=T)),
           by = .(ym = format(Date, "%Y-%m"))]
dtm[,Date:=lubridate::floor_date(Date,"months")]
setorder(dtm, ym)
dtm[, month := as.Date(paste0(ym, "-01"))]
dtm <- dtm[!is.na(smi) & !is.na(sp500)]

dtm[, `:=`(smi_ret   = smi / shift(smi) - 1,
           sp500_ret = sp500 / shift(sp500) - 1)]

dtm <- dtm[month >= "2011-01-01"]        # Dec 2010 only served as return base
setcolorder(dtm, c("ym", "month", "Date", "smi", "smi_ret", "sp500", "sp500_ret"))
setkey(dtm, month)


ggplot(dtm,aes(x=Date))+
  geom_line(aes(y=smi,color="smi"))+
  geom_line(aes(y=sp500,color="sp500"))


## ---------------------------------------------------------------------------
## five largest drawdowns per index since 2011
## ---------------------------------------------------------------------------

# an episode runs from a new all-time (in-sample) peak to the low before the next
# peak; depth = trough / peak - 1. Ranked by depth, top k kept.

dd_episodes <- function(x, k = 5L) {
  dd  <- x / cummax(x) - 1
  grp <- cumsum(dd == 0)                       # each group starts at a peak
  ep  <- data.table(i = seq_along(x), dd = dd, grp = grp)[
    , .(peak_i   = min(i),
        trough_i = i[which.min(dd)],
        depth    = min(dd)), by = grp][depth < 0]
  setorder(ep, depth)
  head(ep, k)[, grp := NULL][]
}

# drawdown months = peak+1 ... trough (the peak month itself is still the high)
dd_flag <- function(x, k = 5L) {
  ep <- dd_episodes(x, k)
  f  <- rep(FALSE, length(x))
  for (r in seq_len(nrow(ep))) f[(ep$peak_i[r] + 1L):ep$trough_i[r]] <- TRUE
  f
}

dd_smi   <- dd_episodes(dtm$smi,   5L)
dd_sp500 <- dd_episodes(dtm$sp500, 5L)

# readable episode tables. The peak month is the last month BEFORE the decline,
# so the drawdown itself runs from month peak_i+1 through the trough month.
# peak_end / trough_end are the month-end trading days used for plotting.
dd_smi[,   index := "smi"]
dd_sp500[, index := "sp500"]
dd_top5 <- rbind(dd_smi, dd_sp500)[
  , .(index,
      from       = dtm$month[peak_i + 1L],   # first month of the drawdown
      to         = dtm$month[trough_i],      # trough month
      peak_end   = dtm$Date[peak_i],         # month-end date of the peak
      trough_end = dtm$Date[trough_i],
      n_months   = trough_i - peak_i,
      depth)]
# print(dd_top5)

# indicators: months inside one of the five largest drawdowns
dtm[, dd_smi   := dd_flag(smi,   5L)]
dtm[, dd_sp500 := dd_flag(sp500, 5L)]
dtm[, dd_any   := dd_smi | dd_sp500]     # union of both

dtm[, .N, by = .(dd_smi, dd_sp500, dd_any)]


## ---------------------------------------------------------------------------
## plot: both indices (2011-01 = 100) with the top-5 drawdowns shaded
## ---------------------------------------------------------------------------

library(ggplot2)

dtm[, `:=`(smi_idx   = 100 * smi / smi[1],
           sp500_idx = 100 * sp500 / sp500[1])]

# x axis = month-end trading day (Date), NOT the first of the month: the level
# is the month's closing value
lines_l <- melt(dtm[, .(Date, smi = smi_idx, sp500 = sp500_idx)],
                id.vars = "Date", variable.name = "index", value.name = "level")

# one rectangle per episode, from the peak's month end to the trough's month end
dd_rect <- dd_top5[, .(index, xmin = peak_end, xmax = trough_end, depth)]

cols <- c(smi = "#C0392B", sp500 = "#2C6FBB")

ggplot() +
  geom_rect(data = dd_rect,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = index),
            alpha = 0.18) +
  geom_line(data = lines_l, aes(Date, level, colour = index), linewidth = 0.7) +
  scale_colour_manual(values = cols, labels = c(smi = "SMI", sp500 = "S&P 500")) +
  scale_fill_manual(values = cols, labels = c(smi = "SMI drawdown",
                                              sp500 = "S&P 500 drawdown")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_light() +
  labs(x = NULL, y = "Index (Jan 2011 = 100)", colour = NULL, fill = NULL,
       title = "SMI and S&P 500 with their five largest drawdowns since 2011")




## ===========================================================================
## hand-adjust and pick the relevant drawdown periods

dtm <- dtm[year(Date)<2025]

dtm[,dd_sel := dd_any]
dtm[month == "2011-03-01",dd_sel := FALSE]
dtm[month == "2011-04-01",dd_sel := FALSE]
dtm[month == "2015-06-01",dd_sel := FALSE]
dtm[month == "2015-07-01",dd_sel := FALSE]


## ---------------------------------------------------------------------------
## plot: both indices with the selected drawdown periods shaded in grey
## ---------------------------------------------------------------------------

dtm[, `:=`(smi_idx   = 100 * smi / smi[1],
           sp500_idx = 100 * sp500 / sp500[1])]

lines_sel <- melt(dtm[, .(Date, smi = smi_idx, sp500 = sp500_idx)],
                  id.vars = "Date", variable.name = "index", value.name = "level")

# collapse consecutive dd_sel months into blocks. A month flagged TRUE covers the
# move from the previous month's close to its own close, so the block starts at
# the month end BEFORE its first flagged month.
sel_rect <- dtm[, .(Date, dd_sel, prev_end = shift(Date, fill = Date[1] - 30))][
  , blk := rleid(dd_sel)][dd_sel == TRUE,
  .(xmin = prev_end[1], xmax = Date[.N]), by = blk]

ggplot() +
  geom_rect(data = sel_rect,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey60", alpha = 0.35) +
  geom_line(data = lines_sel, aes(Date, level, colour = index), linewidth = 0.7) +
  scale_colour_manual(values = cols, labels = c(smi = "SMI", sp500 = "S&P 500")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_light() +
  labs(x = NULL, y = "Index (Jan 2011 = 100)", colour = NULL,
       title = "SMI and S&P 500, selected drawdown periods shaded")












