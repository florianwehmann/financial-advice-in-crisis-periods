# trade-level 12-month returns: pre and post, advised vs. not advised
#
# one row per trade - Bp_ID x Asset_ID x month, the level `advised` was merged on
# in pos_analysis.R - carrying the return of the traded asset over the 12 months
# before and the 12 months after the trade month.
#
# the returns deliberately do NOT come from the trading client's own position
# rows: a client who sells is gone from the panel and their post-window would be
# missing exactly for the trades that are most interesting. they come from an
# asset-level monthly return series built across ALL holders in the bank, so a
# month is covered as long as anybody still held the asset. trades whose window
# is not fully covered even then are counted and reported, never silently dropped
# (see the coverage section) - `post_ok` / `pre_ok` mark them.
#
# ------------------------------------------------------------------------------
# two things to keep in mind when reading the comparison at the bottom:
#
# 1. DIRECTION. post-trade performance reads the opposite way for a sale than for
#    a purchase. the boerse columns merged into pos may or may not contain a
#    buy/sell field (pos_analysis.R keeps only boerse columns whose names do not
#    already exist in pos, so e.g. `Menge` was dropped). the script looks for one,
#    and otherwise falls back to the sign of the holder's month-over-month change
#    in `Menge` as a proxy. set `dir_col` by hand if you know the right column.
#
# 2. TIMING. advised trades cluster in time, and 12-month returns are dominated by
#    the market. so every raw return is reported next to an SMI-adjusted one, and
#    the headline comparison is also run within trade month (month fixed effects,
#    implemented as within-month demeaning).
#
# 3. TRADE UNIVERSE. the trade flags reached pos through a left join onto the
#    position panel (pos_analysis.R), so a trade is only in this data if the
#    client still had a position row for that asset in that month. a sale that
#    closed a position out of the month-end snapshot may therefore be missing
#    entirely - sells are likely under-represented relative to the boerse file.
#    ../data/boerse_merged.parquet is the place to check that if it matters.
# ==============================================================================

library(data.table)
library(arrow)
library(lubridate)
library(ggplot2)

rm(list = ls()); gc()

out_fig_path <- "C:/Users/FWehmann/Dropbox/Apps/Overleaf/Uncertainty and Financial Advise (1)/figures"

save_fig <- function(p, name, width = 8, height = 5) {
  ggsave(file.path(out_fig_path, paste0(name, ".pdf")), p, width = width, height = height)
  invisible(p)
}

smp_start <- as.Date("2011-01-01")
instr_sel <- c("Aktien", "Fonds")

win_pre  <- 12L    # months before the trade
win_post <- 12L    # months after the trade

# the trade happens *during* its month, so that month belongs to neither window.
# set FALSE to end the pre-window with the trade month instead
skip_event_month <- TRUE

# the asset return of a month in which a holder traded is distorted: the price
# change is earned on a position that was only partly held. so the asset series is
# built from non-trading holders where there are at least this many of them, and
# falls back to all holders otherwise
min_holders_ut <- 3L

# the trading client's own coverage is computed too, to show how much the
# cross-holder lookup actually buys. it is the one expensive step here - set FALSE
# to skip it
do_own_coverage <- TRUE

wins_p <- 0.01     # winsorising level for the mean comparisons


# ==============================================================================
# data
# ==============================================================================

pos_file <- "../data/pos_b_merged.parquet"

pos_ds <- arrow::open_dataset(pos_file)

# `Menge` is the position quantity and is only needed for the direction proxy;
# K_Aufnahme is who initiated the contact behind an advised trade
pos_cols <- c("Bp_ID", "Cont_ID", "Asset_ID", "MDate", "advised", "K_Aufnahme",
              "Instrumentengruppe", "Menge",
              "Geschaeftsvolumen_CHF", "DA_Titelkursabweichung_CHF")

miss <- setdiff(pos_cols, names(pos_ds))
if (length(miss)) stop("not in ", pos_file, ": ", paste(miss, collapse = ", "))

# a buy/sell field would come from the boerse side of the merge. these are the
# plausible names - whatever is there is loaded too and reported below
dir_cand <- grep("kauf|verkauf|richtung|seite|auftragsart|geschaeftsart|transaktion|bewegung",
                 names(pos_ds), value = TRUE, ignore.case = TRUE)
message("candidate direction columns in the parquet: ",
        if (length(dir_cand)) paste(dir_cand, collapse = ", ") else "none found")

pos_cols <- unique(c(pos_cols, dir_cand))

mdate_type <- pos_ds$schema$GetFieldByName("MDate")$type$ToString()
push_date  <- grepl("date|timestamp", mdate_type, ignore.case = TRUE) &&
  requireNamespace("dplyr", quietly = TRUE)

pos <- if (push_date) {
  dplyr::collect(dplyr::filter(dplyr::select(pos_ds, dplyr::all_of(pos_cols)),
                               MDate >= smp_start))
} else {
  arrow::read_parquet(pos_file, col_select = tidyselect::all_of(pos_cols))
}
setDT(pos)
rm(pos_ds); gc()

pos[, MDate := as.Date(MDate)]
pos <- pos[MDate >= smp_start]

if (is.character(pos$Instrumentengruppe)) pos[, Instrumentengruppe := factor(Instrumentengruppe)]
pos <- pos[Instrumentengruppe %in% instr_sel]
gc()

# a gap-safe month counter. every window below is defined on this rather than on
# row offsets, so a month missing from an asset's history can never be silently
# treated as the neighbouring one
pos[, mnum := year(MDate) * 12L + month(MDate)]

# SMI, same construction as the other scripts: month-end close stamped to the 1st
smi <- fread("../data/hsmi.csv", skip = 4, select = c(1, 2, 5))
setnames(smi, c("Date", "SMI", "SMI_TR"))
smi[, Date := as.Date(Date, format = "%d.%m.%Y")]
smi <- smi[!is.na(Date)][order(Date)]
smi[, MDate := floor_date(Date, "month")]

smi_m <- smi[, .(SMI = last(SMI), SMI_TR = last(SMI_TR)), by = MDate]
smi_m[, mnum := year(MDate) * 12L + month(MDate)]
smi_m[, smi_ret := SMI / shift(SMI) - 1]


# ==============================================================================
# asset-level monthly return series, pooled over all holders
# ==============================================================================

# the repo's return convention: CHF price deviation over CHF volume. it is a price
# return - DA_Devisenkursabweichung (FX) is deliberately not in the numerator - and
# it is only as good as the assumption that Geschaeftsvolumen_CHF is the base the
# deviation was earned on. sanity-check the pooled series against the SMI below.
pos[, ok_ret := !is.na(Geschaeftsvolumen_CHF) & Geschaeftsvolumen_CHF > 0 &
      !is.na(DA_Titelkursabweichung_CHF)]

# one pass, both variants: all holders, and holders who did not trade that month
asset_m <- pos[ok_ret == TRUE,
               .(v_all = sum(Geschaeftsvolumen_CHF),
                 d_all = sum(DA_Titelkursabweichung_CHF),
                 n_all = .N,
                 v_ut  = sum(Geschaeftsvolumen_CHF[is.na(advised)]),
                 d_ut  = sum(DA_Titelkursabweichung_CHF[is.na(advised)]),
                 n_ut  = sum(is.na(advised))),
               by = .(Asset_ID, mnum)]
gc()

# value-weighted across holders: aggregate price change over aggregate volume.
# one holder with an odd position cannot move it the way a plain mean would
asset_m[, ret := fifelse(n_ut >= min_holders_ut & v_ut > 0, d_ut / v_ut,
                         fifelse(v_all > 0, d_all / v_all, NA_real_))]
asset_m[, ret_src := fifelse(n_ut >= min_holders_ut & v_ut > 0, "non-traders", "all holders")]

# a single implausible month would sit in the cumulative chain for the rest of the
# asset's life, so those months are dropped rather than clipped. dropping is the
# honest move: it opens a gap, and the window check below then refuses the windows
# that span it instead of quietly compounding a bad number
n_raw <- nrow(asset_m)
asset_m <- asset_m[!is.na(ret) & is.finite(ret) & ret > -0.95 & ret < 5]
cat("asset-months dropped as implausible returns:", n_raw - nrow(asset_m),
    sprintf("(%.3f%%)\n", 100 * (n_raw - nrow(asset_m)) / n_raw))

asset_m[, MDate := as.Date(sprintf("%d-%02d-01", (mnum - 1L) %/% 12L, (mnum - 1L) %% 12L + 1L))]

cat("\nasset-month return panel\n")
print(asset_m[, .(asset_months = .N, assets = uniqueN(Asset_ID),
                  from = min(MDate), to = max(MDate),
                  share_from_non_traders = mean(ret_src == "non-traders"))])

# how far the pooled asset returns are from being a market index - not a test,
# just a smell check that the ratio behaves like a return
chk <- merge(asset_m[, .(ret_ew = mean(ret, na.rm = TRUE)), by = mnum],
             smi_m[, .(mnum, smi_ret)], by = "mnum")
cat("correlation of the equal-weighted asset return with the SMI:",
    round(chk[, cor(ret_ew, smi_ret, use = "complete.obs")], 3), "\n")

# cumulative log return per asset, plus a running count of observed months. `k`
# is what makes the windows gap-proof: a window is only valid if the number of
# observed months inside it equals its calendar length
setorder(asset_m, Asset_ID, mnum)
asset_m[, `:=`(cum = cumsum(log1p(ret)), k = seq_len(.N)), by = Asset_ID]

setorder(smi_m, mnum)
smi_m[!is.na(smi_ret), `:=`(cum = cumsum(log1p(smi_ret)), k = seq_len(.N))]


# ==============================================================================
# the window helper
# ==============================================================================

# return over months [t+from, t+to], read off a cumulative-log-return reference.
# `cum` includes its own month, so the window starts from the month before it.
# NA unless both endpoints exist AND every month between them is observed.
win_ret <- function(trd, ref, from, to, out, by = "Asset_ID") {
  n_need <- as.integer(to - from + 1L)
  tmp <- c("w_lo", "w_hi", "w_cum_lo", "w_cum_hi", "w_k_lo", "w_k_hi")

  trd[, `:=`(w_lo = mnum + as.integer(from) - 1L, w_hi = mnum + as.integer(to))]
  trd[, `:=`(w_cum_lo = NA_real_, w_cum_hi = NA_real_,
             w_k_lo = NA_integer_, w_k_hi = NA_integer_)]   # so a no-match stays NA

  trd[ref, on = c(by, "w_lo==mnum"), `:=`(w_cum_lo = i.cum, w_k_lo = i.k)]
  trd[ref, on = c(by, "w_hi==mnum"), `:=`(w_cum_hi = i.cum, w_k_hi = i.k)]

  trd[, (out) := fifelse(!is.na(w_cum_lo) & !is.na(w_cum_hi) & (w_k_hi - w_k_lo) == n_need,
                         expm1(w_cum_hi - w_cum_lo), NA_real_)]
  trd[, (tmp) := NULL]
  invisible(trd)
}

pre_to    <- if (skip_event_month) -1L else 0L
pre_from  <- pre_to - win_pre + 1L
post_from <- 1L
post_to   <- win_post


# ==============================================================================
# trades
# ==============================================================================

# `advised` is NA on position rows with no matching trade, so a trade is simply a
# non-NA row. it repeats across the client's contracts holding the same asset -
# the merge was on Bp_ID x Asset_ID x MDate - so collapse to that level, keeping
# an advised trade over a non-advised one if the contracts ever disagree
trd <- pos[!is.na(advised)]
setorder(trd, Bp_ID, Asset_ID, mnum, -advised)
trd <- unique(trd, by = c("Bp_ID", "Asset_ID", "mnum"))

cat("\ntrades:", nrow(trd), "| advised:", trd[, sum(advised)],
    sprintf("(%.1f%%)", 100 * trd[, mean(advised)]), "\n")

# --- direction ----------------------------------------------------------------
# set this by hand to the real buy/sell column if the data has one
dir_col <- NULL

if (!is.null(dir_col)) {
  trd[, trade_dir := get(dir_col)]
  dir_note <- paste0("direction from column `", dir_col, "`")
} else {
  # proxy: the holder's own change in quantity across the trade month, summed over
  # their contracts. splits and in-kind distributions also move Menge, so this is a
  # proxy, not the truth
  qty <- pos[!is.na(Menge), .(Menge = sum(Menge)), by = .(Bp_ID, Asset_ID, mnum)]
  setorder(qty, Bp_ID, Asset_ID, mnum)
  qty[, `:=`(mnum_prev = shift(mnum), Menge_prev = shift(Menge)), by = .(Bp_ID, Asset_ID)]
  qty[, first_obs := is.na(mnum_prev)]
  # only a directly preceding month gives a usable difference; over a gap the
  # change is not attributable to this trade
  qty[, dmenge := fifelse(!is.na(mnum_prev) & mnum - mnum_prev == 1L,
                          Menge - Menge_prev, NA_real_)]

  trd[qty, on = .(Bp_ID, Asset_ID, mnum), `:=`(dmenge = i.dmenge, first_obs = i.first_obs)]
  trd[, trade_dir := fifelse(!is.na(dmenge) & dmenge > 0, "buy",
                      fifelse(!is.na(dmenge) & dmenge < 0, "sell",
                       # the asset appearing in the client's book for the first
                       # time is a purchase whatever the quantity difference says
                       fifelse(first_obs %in% TRUE, "buy", "unclear")))]
  rm(qty); gc()
  dir_note <- "direction proxied by the month-over-month change in Menge"
}

cat(dir_note, "\n")
print(trd[, .N, by = trade_dir][order(-N)])

# --- the windows --------------------------------------------------------------
win_ret(trd, asset_m, pre_from,  pre_to,    "pre12")
win_ret(trd, asset_m, post_from, post_to,   "post12")

win_ret(trd, smi_m, pre_from,  pre_to,  "pre12_smi",  by = character(0))
win_ret(trd, smi_m, post_from, post_to, "post12_smi", by = character(0))

# excess over the market, in logs, so the two horizons stay additive
trd[, `:=`(pre12_x  = expm1(log1p(pre12)  - log1p(pre12_smi)),
           post12_x = expm1(log1p(post12) - log1p(post12_smi)))]

trd[, `:=`(pre_ok = !is.na(pre12), post_ok = !is.na(post12))]
gc()


# ==============================================================================
# coverage: what the cross-holder lookup buys, and what is still missing
# ==============================================================================

if (do_own_coverage) {
  # the same gap-proof month count, but on the trading client's own holdings of
  # that asset. cum is a constant, so win_ret returns 0 where the client held the
  # asset for every month of the window and NA where they did not - which is
  # exactly the coverage flag wanted
  own <- unique(pos[, .(Bp_ID, Asset_ID, mnum)])
  setorder(own, Bp_ID, Asset_ID, mnum)
  own[, `:=`(k = seq_len(.N), cum = 0), by = .(Bp_ID, Asset_ID)]

  win_ret(trd, own, post_from, post_to, "own_post", by = c("Bp_ID", "Asset_ID"))
  win_ret(trd, own, pre_from,  pre_to,  "own_pre",  by = c("Bp_ID", "Asset_ID"))

  trd[, `:=`(post_ok_own = !is.na(own_post), pre_ok_own = !is.na(own_pre))]
  trd[, c("own_post", "own_pre") := NULL]
  rm(own); gc()
}

cat("\n================ coverage ================\n")
cov_tab <- trd[, .(trades = .N,
                   post_covered = sum(post_ok),
                   post_share   = mean(post_ok),
                   pre_covered  = sum(pre_ok),
                   pre_share    = mean(pre_ok)),
               by = advised]
print(cov_tab)

if (do_own_coverage) {
  cat("\npost-window coverage: the client's own holdings vs. the pooled asset series\n")
  print(trd[, .N, by = .(post_ok_own, post_ok)][order(-post_ok_own, -post_ok)])
  cat("trades rescued by looking the asset up from another holder:",
      trd[post_ok & !post_ok_own, .N],
      sprintf("(%.1f%% of all trades)\n", 100 * trd[, mean(post_ok & !post_ok_own)]))
}

# the ones that stay missing, and why. a trade in the last 12 months of the sample
# simply has no post-window yet - that is mechanical, not a data gap
last_mnum <- asset_m[, max(mnum)]
trd[, miss_reason := fifelse(
  post_ok, "covered",
  fifelse(mnum + post_to > last_mnum, "window runs past the end of the sample",
          fifelse(!Asset_ID %in% asset_m$Asset_ID, "asset never in the return panel",
                  "asset series has a gap inside the window")))]

cat("\nwhy the post-window is missing\n")
print(trd[, .(trades = .N, share = .N / nrow(trd)), by = miss_reason][order(-trades)])

# the assets behind the remaining true gaps, in case they are worth chasing
gap_assets <- trd[miss_reason == "asset series has a gap inside the window",
                  .(trades = .N), by = Asset_ID][order(-trades)]
cat("\nassets with an in-window gap:", nrow(gap_assets),
    "| trades affected:", gap_assets[, sum(trades)], "\n")
print(head(gap_assets, 20))


# ==============================================================================
# advised vs. not
# ==============================================================================

# the estimation sample: both windows fully covered, so pre and post are measured
# on the same trades and the two comparisons are not run on different populations
est <- trd[post_ok & pre_ok]
cat("\nestimation sample:", nrow(est), "trades |",
    est[, sum(advised)], "advised\n")

wins <- function(x, p = wins_p) {
  q <- quantile(x, c(p, 1 - p), na.rm = TRUE)
  pmin(pmax(x, q[[1]]), q[[2]])
}

rcols <- c("pre12", "post12", "pre12_x", "post12_x")
est[, paste0(rcols, "_w") := lapply(.SD, wins), .SDcols = rcols]

# within-month demeaning = trade-month fixed effects. advised trades cluster in
# time, so the raw gap partly measures when clients were advised, not how well
est[, paste0(rcols, "_dm") := lapply(.SD, function(x) x - mean(x, na.rm = TRUE)),
    .SDcols = paste0(rcols, "_w"), by = MDate]

summ <- function(dt, cols, by) {
  dt[, c(.(n = .N),
         unlist(lapply(.SD, function(x)
           list(mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE),
                sd = sd(x, na.rm = TRUE))), recursive = FALSE)),
     .SDcols = cols, by = by]
}

cat("\n================ raw and market-adjusted returns, by advice ================\n")
print(summ(est, c("pre12_w", "post12_w", "pre12_x_w", "post12_x_w"), "advised"))

cat("\n---- difference in means (advised - not advised) ----\n")
for (v in c("pre12_w", "post12_w", "pre12_x_w", "post12_x_w", "post12_x_dm")) {
  # groups come out ordered FALSE, TRUE, so the difference is advised - not advised
  tt <- t.test(est[[v]] ~ est$advised)
  cat(sprintf("%-14s diff = %+8.4f   t = %6.2f   p = %.4f\n",
              v, unname(diff(tt$estimate)), tt$statistic, tt$p.value))
}

if (nrow(est) <= 5e5) {
  cat("\nWilcoxon on the market-adjusted post return: p =",
      format.pval(wilcox.test(post12_x_w ~ advised, data = est)$p.value), "\n")
}

# the same split by who initiated the contact behind the trade
cat("\n---- by contact initiator (advised trades only) ----\n")
print(summ(est[advised == TRUE], c("post12_w", "post12_x_w"), "K_Aufnahme"))

# and by direction, since a sale's post-return reads the other way round
cat("\n---- by direction (", dir_note, ") ----\n", sep = "")
print(summ(est, c("post12_w", "post12_x_w"), c("trade_dir", "advised")))

# optional: proper two-way fixed effects if fixest is around. month FE absorbs the
# timing, asset FE absorbs which assets get advised
if (requireNamespace("fixest", quietly = TRUE)) {
  cat("\n---- post12 ~ advised, month and asset FE, SE clustered by client ----\n")
  m1 <- fixest::feols(post12_w ~ advised | MDate, data = est, cluster = ~Bp_ID)
  m2 <- fixest::feols(post12_w ~ advised | MDate + Asset_ID, data = est, cluster = ~Bp_ID)
  print(fixest::etable(m1, m2))
} else {
  message("fixest not installed - only the within-month demeaned comparison above")
}


# ==============================================================================
# figures
# ==============================================================================

adv_col <- c("Advised trade" = "#1b7837", "Not advised trade" = "#4393c3")
est[, adv_lab := fifelse(advised, "Advised trade", "Not advised trade")]

# 1. pre and post side by side: the pre-window is the placebo. if advised and
# non-advised trades already differ before the trade, the post gap is selection
long <- rbindlist(list(
  est[, .(adv_lab, horizon = "Pre 12m",  ret = pre12_x_w)],
  est[, .(adv_lab, horizon = "Post 12m", ret = post12_x_w)]
))
long[, horizon := factor(horizon, c("Pre 12m", "Post 12m"))]

bar <- long[, .(mean = mean(ret, na.rm = TRUE),
                se   = sd(ret, na.rm = TRUE) / sqrt(sum(!is.na(ret)))),
            by = .(adv_lab, horizon)]

p_ba <- ggplot(bar, aes(x = horizon, y = mean, fill = adv_lab)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60") +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se),
                position = position_dodge(0.7), width = 0.15, linewidth = 0.4) +
  scale_fill_manual(values = adv_col) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  labs(x = NULL, y = "Return in excess of the SMI", fill = NULL,
       title = "Asset performance around the trade, by advice status",
       subtitle = paste0("12 months before and after the trade month | winsorised at ",
                         scales::label_percent(accuracy = 1)(wins_p),
                         " | 95% CI")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.major.x = element_blank())

p_ba
save_fig(p_ba, "trade_pre_post_12m_by_advice")

# 2. the distribution, not just its mean - the tails are where advice could bite.
# drawn on a subsample: a density over millions of points costs minutes and looks
# identical
set.seed(1)
den_dat <- if (nrow(est) > 2e5) est[sample(.N, 2e5)] else est

p_den <- ggplot(den_dat, aes(x = post12_x_w, color = adv_lab)) +
  geom_vline(xintercept = 0, linewidth = 0.3, color = "grey60") +
  geom_density(linewidth = 0.9) +
  scale_color_manual(values = adv_col) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(x = "12m return after the trade, in excess of the SMI", y = "Density",
       color = NULL, title = "Distribution of post-trade performance by advice status") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

p_den
save_fig(p_den, "trade_post12m_density_by_advice")

# 3. over time: the gap month by month, so a single episode driving the average
# is visible rather than buried in it
ts <- est[, .(mean = mean(post12_x_w, na.rm = TRUE), n = .N), by = .(MDate, adv_lab)]

p_ts <- ggplot(ts[n >= 30], aes(x = MDate, y = mean, color = adv_lab)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60") +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = adv_col) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(x = NULL, y = "Mean 12m return after the trade, in excess of the SMI",
       color = NULL, title = "Post-trade performance by trade month",
       subtitle = "months with at least 30 trades in the group") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

p_ts
save_fig(p_ts, "trade_post12m_by_month_advice")


# ==============================================================================
# out
# ==============================================================================

keep <- c("Bp_ID", "Asset_ID", "Cont_ID", "MDate", "mnum", "advised", "K_Aufnahme",
          "Instrumentengruppe", "trade_dir",
          "pre12", "post12", "pre12_smi", "post12_smi", "pre12_x", "post12_x",
          "pre_ok", "post_ok", "miss_reason",
          grep("^(post|pre)_ok_own$", names(trd), value = TRUE))

arrow::write_parquet(trd[, intersect(keep, names(trd)), with = FALSE],
                     "../data/trade_returns_12m.parquet")
cat("\nwrote ../data/trade_returns_12m.parquet\n")
