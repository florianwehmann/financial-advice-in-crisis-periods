## =============================================================================
## 03_episodes.R -- drawdown episodes, defined on the MARKET INDEX
##
## Never on a client's own portfolio: client-triggered windows produce mechanical
## mean reversion. Both index files are local (no network call).
##
## Output: results/cache/episodes.parquet, results/cache/index_m.parquet
##         results/03_episodes.txt, results/figures/03_episodes.pdf
## =============================================================================

source("00_setup.R")
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

## ---------------------------------------------------------------------------
## 2. peak-to-trough episodes, dated on DAILY closes
##    Monthly closes miss most of a fast crash -- the Covid drawdown is -26% on
##    daily SPI TR but only -13% month-end to month-end, so a monthly rule would
##    drop the single sharpest episode in the sample. Episodes are therefore
##    found on daily data and then snapped to month ends.
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

lvl_col <- if (P$index_main == "spi_tr") "spi_tr" else "smi"
dly <- if (lvl_col == "spi_tr") spi[, .(Date, lvl = spi_tr)] else smi[, .(Date, lvl = smi)]
dly <- dly[Date >= madd(P$smp_start, -25L) & Date <= P$smp_end]

ep <- dd_episodes(dly$Date, dly$lvl, P$dd_threshold)

if (!nrow(ep))
  stop("no drawdown of at least ", 100 * P$dd_threshold, "% found on ", lvl_col,
       " between ", min(dly$Date), " and ", max(dly$Date),
       " -- lower P$dd_threshold in 00_setup.R", call. = FALSE)

## dd_start = month in which the decline begins (contains the daily peak)
## pre_month = last month end fully before the decline; it is the reference month
##             for the event study (rel_month == -1) and for all frozen covariates
ep[, `:=`(dd_start = eom(peak_date),
          dd_end   = eom(trough_date))]
ep[, ep_id := sprintf("ep%d_%s", .I, format(dd_start, "%Y%m"))]
ep[, `:=`(pre_month = madd(dd_start, -1L),
          pre_start = madd(dd_start, -P$pre_months),
          post_end  = madd(dd_end,    P$post_months),
          n_months  = mdiff(eom(trough_date), eom(peak_date)))]

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
cat("DRAWDOWN EPISODES on daily ", lvl_col, "  (threshold ", 100 * P$dd_threshold, "%)\n", sep = "")
cat("-------------------------------------------------------------\n")
print(ep[, .(ep_id, peak_date, trough_date, depth = round(depth, 3), n_days,
             dd_start, dd_end, n_months, pre_start, post_end, usable)])
cat("\nINSPECT THIS TABLE BEFORE CONTINUING.\n")
cat("dd_start = month end of the month containing the daily peak (rel_month 0);\n")
cat("rel_month -1 (pre_month) is the last month end before the decline and is the\n")
cat("reference period and the freeze date for every covariate.\n")
cat("post_end is truncated at the month before the next episode's dd_start.\n")
cat("'usable' requires the full pre window to lie inside the sample, which starts\n")
cat("in ", format(P$smp_start), " because the advice fields are empty before 2011.\n\n", sep = "")

## same on the alternative index, for the record
alt <- if (lvl_col == "spi_tr") smi[, .(Date, lvl = smi)] else spi[, .(Date, lvl = spi_tr)]
alt <- alt[Date >= madd(P$smp_start, -25L) & Date <= P$smp_end]
cat("for comparison, episodes on the alternative index:\n")
print(dd_episodes(alt$Date, alt$lvl, P$dd_threshold)[
  , .(peak_date, trough_date, depth = round(depth, 3), n_days)])

## a pre window may still reach back into the previous episode's recovery window;
## that is allowed (each episode is its own stack) but must be visible
ep[, prev_post_end := shift(post_end)]
ep[, pre_overlaps_prev_post := !is.na(prev_post_end) & pre_start <= prev_post_end]
if (any(ep$usable & ep$pre_overlaps_prev_post))
  cat("WARNING: pre window overlaps the previous episode's recovery window for: ",
      paste(ep[usable & pre_overlaps_prev_post, ep_id], collapse = ", "),
      "\n  -> the pre-trend for that episode is measured during another episode's\n",
      "     recovery. Check the episode-by-episode estimates in 07_robustness.\n\n", sep = "")
ep[, prev_post_end := NULL]

epu <- ep[usable == TRUE]
if (!nrow(epu))
  stop("no episode has a complete 12m pre and post window inside ",
       P$smp_start, "..", P$smp_end, call. = FALSE)
cat("\nusable episodes:", nrow(epu), " -> ", paste(epu$ep_id, collapse = ", "), "\n")

save_dt(ep, "episodes")

## ---------------------------------------------------------------------------
## 3. figure
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
       title = sprintf("Drawdown episodes, peak-to-trough >= %.0f%%", 100 * P$dd_threshold)) +
  theme_light() + theme(legend.position = "bottom")

ggsave(file.path(FIG_DIR, "03_episodes.pdf"), p, width = 9, height = 4.5)
log_step(sprintf("episodes: %d found, %d usable", nrow(ep), nrow(epu)))
sink()
