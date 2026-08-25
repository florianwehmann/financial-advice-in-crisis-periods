# positionen analyse

rm(list=ls());gc()

out_fig_path <- "C:/Users/FWehmann/Dropbox/Apps/Overleaf/Uncertainty and Financial Advise (1)/figures"

# every figure in this script goes out through here, so format and size stay uniform
save_fig <- function(p, name, width = 8, height = 5) {
  ggsave(file.path(out_fig_path, paste0(name, ".pdf")), p, width = width, height = height)
  invisible(p)
}

# load pos data
pos <- setDT(arrow::read_parquet("../data/pos_b_merged.parquet",
                                 as_data_frame = TRUE))
gc()


pos[,.N,by=Instrumentengruppe]

pos[,asset_ret := DA_Titelkursabweichung_CHF / Geschaeftsvolumen_CHF]
pos[is.na(asset_ret), asset_ret := 0]

pos[is.na(advised),advised:=F]


# one sample definition, two aggregations of it: split by advice status, and overall.
# both keep the CHF sums, so both get the same value-weighted return further down
# (aggregate price change / aggregate volume) and stay directly comparable.
smp  <- pos[, which(MDate >= "2011-01-01" & Instrumentengruppe %in% c("Fonds","Aktien"))]
vcol <- c("Geschaeftsvolumen_CHF","DA_Titelkursabweichung_CHF")

pos_c   <- pos[smp, lapply(.SD, mean, na.rm=T), by = c("MDate","advised"), .SDcols = vcol]
pos_all <- pos[smp, lapply(.SD, mean, na.rm=T), by = "MDate",              .SDcols = vcol]
pos_all[, asset_ret := DA_Titelkursabweichung_CHF / Geschaeftsvolumen_CHF]
# pos_c <- pos[MDate>="2010-01-01" & Instrumentengruppe %in% c("Fonds","Aktien"),lapply(.SD,mean,na.rm=T),by=c("MDate","advised"),.SDcols=c("asset_ret")]


# pos_n <- pos[MDate>="2011-01-01" & Instrumentengruppe %in% c("Fonds","Aktien") & !is.na(Geschaeftsvolumen_CHF),.N,by=c("MDate","advised")]
# ggplot(pos_n[advised==T],aes(x=MDate,y=N,color=advised))+geom_line()


setorder(pos_c, advised, MDate)   # cumprod is order-dependent!

base_pos <- as.Date("2018-01-01")   # rebasing month for asset_idx

pos_c[,asset_ret := DA_Titelkursabweichung_CHF / Geschaeftsvolumen_CHF]
pos_c[, asset_idx := {
  ci <- cumprod(1 + asset_ret)
  ci / ci[MDate == base_pos]
}, by = advised]


p_adv_full <- ggplot(pos_c[!is.na(asset_idx)],
                     aes(x = MDate, y = asset_idx,
                         color = fifelse(advised, "Advised", "Not advised"))) +
  geom_hline(yintercept = 1, linewidth = 0.3, color = "grey60") +
  geom_vline(xintercept = base_pos, linewidth = 0.3, color = "grey60",
             linetype = "dashed") +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c("Advised" = "#1b7837", "Not advised" = "#4393c3")) +
  scale_x_date(date_breaks = "2 years", date_minor_breaks = "1 year",
               date_labels = "%Y", expand = expansion(mult = c(0.01, 0.03))) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  labs(x = NULL, y = paste0("Index (", format(base_pos, "%b %Y"), " = 1)"),
       color = NULL,
       title = "Cumulative asset performance by advice status",
       subtitle = "Value-weighted monthly return on equity and fund positions") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        panel.grid.minor.y = element_blank())

p_adv_full
save_fig(p_adv_full, "asset_index_by_advice")



pos_floor <- pos_c[MDate>="2010-11-01" & MDate<="2015-01-01"]
pos_floor[,asset_idx := cumprod(1+asset_ret)/(1+asset_ret[1]),by=advised]
p_floor <- ggplot()+
  geom_line(data=pos_floor[advised==F],aes(x=MDate,y=asset_idx,color="not advised"))+
  geom_line(data=pos_floor[advised==T],aes(x=MDate,y=asset_idx,color="advised"))
p_floor
save_fig(p_floor, "asset_index_advice_chf_floor_intro")

pos_floor2 <- pos_c[MDate>="2014-11-01" & MDate<="2016-01-01"]
pos_floor2[,asset_idx := cumprod(1+asset_ret)/(1+asset_ret[1]),by=advised]
p_floor2 <- ggplot()+
  geom_line(data=pos_floor2[advised==F],aes(x=MDate,y=asset_idx,color="not advised"))+
  geom_line(data=pos_floor2[advised==T],aes(x=MDate,y=asset_idx,color="advised"))
p_floor2
save_fig(p_floor2, "asset_index_advice_chf_floor_exit")

pos_sub <- pos_c[MDate>="2015-12-01" & MDate<="2019-01-01"]
pos_sub[,asset_idx := cumprod(1+asset_ret)/(1+asset_ret[1]),by=advised]
p_sub1 <- ggplot()+
  geom_line(data=pos_sub[advised==F],aes(x=MDate,y=asset_idx,color="not advised"))+
  geom_line(data=pos_sub[advised==T],aes(x=MDate,y=asset_idx,color="advised"))
p_sub1
save_fig(p_sub1, "asset_index_advice_2016_2018")

pos_sub2 <- pos_c[MDate>="2017-12-01" & MDate<="2020-01-01"]
pos_sub2[,asset_idx := cumprod(1+asset_ret)/(1+asset_ret[1]),by=advised]
p_sub2 <- ggplot()+
  geom_line(data=pos_sub2[advised==F],aes(x=MDate,y=asset_idx,color="not advised"))+
  geom_line(data=pos_sub2[advised==T],aes(x=MDate,y=asset_idx,color="advised"))
p_sub2
save_fig(p_sub2, "asset_index_advice_2018_2019")


pos_covid <- pos_c[MDate>="2019-12-01" & MDate<="2023-01-01"]
pos_covid[,asset_idx := cumprod(1+asset_ret)/(1+asset_ret[1]),by=advised]
p_covid <- ggplot()+
  geom_line(data=pos_covid[advised==F],aes(x=MDate,y=asset_idx,color="not advised"))+
  geom_line(data=pos_covid[advised==T],aes(x=MDate,y=asset_idx,color="advised"))
p_covid
save_fig(p_covid, "asset_index_advice_covid")


pos_covid2 <- pos_c[MDate>="2020-11-01" & MDate<="2024-01-01"]
pos_covid2[,asset_idx := cumprod(1+asset_ret)/(1+asset_ret[1]),by=advised]
p_covid2 <- ggplot()+
  geom_line(data=pos_covid2[advised==F],aes(x=MDate,y=asset_idx,color="not advised"))+
  geom_line(data=pos_covid2[advised==T],aes(x=MDate,y=asset_idx,color="advised"))
p_covid2
save_fig(p_covid2, "asset_index_advice_covid_recovery")

# ==============================================================================
# SMI: daily -> monthly (month-end close, stamped to beginning of month)
# ==============================================================================

# hsmi.csv: ";"-separated, 5 header rows, dates descending, dd.mm.yyyy
# col 1 = DATE, col 2 = SMI PR (price index), col 5 = SMIC (SMI total return)
smi <- fread("../data/hsmi.csv", skip = 4, select = c(1, 2, 5))
setnames(smi, c("Date", "SMI", "SMI_TR"))

smi[, Date := as.Date(Date, format = "%d.%m.%Y")]
smi <- smi[!is.na(Date)][order(Date)]

# last trading day of each month, dated to the 1st so it merges on pos$MDate
smi[, MDate := floor_date(Date, "month")]
smi_m <- smi[, .(Date_eom = last(Date),
                 SMI      = last(SMI),
                 SMI_TR   = last(SMI_TR)), by = MDate]

# monthly returns from consecutive month-end closes
smi_m[, smi_return    := SMI    / shift(SMI)    - 1]
smi_m[, smi_tr_return := SMI_TR / shift(SMI_TR) - 1]


# ==============================================================================
# high-volatility periods: months containing an outsized daily SMI move
# ==============================================================================

vol_k <- 3   # threshold in unconditional daily SDs

smi[, ret_d := SMI / shift(SMI) - 1]
vol_thr <- vol_k * sd(smi$ret_d, na.rm = TRUE)

# a month is flagged if any trading day in it breached the threshold
vol_m <- smi[, .(n_extreme = sum(abs(ret_d) > vol_thr, na.rm = TRUE)), by = MDate][n_extreme > 0]
setorder(vol_m, MDate)

# collapse consecutive flagged months into single bands so they draw seamlessly
vol_m[, mnum := year(MDate) * 12 + month(MDate)]
vol_m[, run  := cumsum(c(1, diff(mnum) != 1))]
vol_bands <- vol_m[, .(xmin = min(MDate), xmax = ceiling_date(max(MDate), "month")), by = run]

vol_lab <- paste0("Month with a daily SMI move beyond ", vol_k, " SD (",
                  scales::label_percent(accuracy = 0.1)(vol_thr), ")")


# ==============================================================================
# one long table of comparable monthly return series, all indexed to 100 at base
# ==============================================================================

base <- as.Date("2020-01-01")
base <- as.Date("2018-01-01")

# compound the monthly returns, then normalise the chain to 100 in the base month.
# every series goes through this, so the aggregate, the advice split and the SMI
# are built the same way and differences between them are differences in the data.
idx100 <- function(ret, dates, base) {
  ci <- cumprod(1 + fifelse(is.na(ret), 0, ret))
  100 * ci / ci[dates == base]
}

# SMI enters as a return series like the others, not as a separately indexed level
cmp <- rbindlist(list(
  pos_all[, .(MDate, series = "All positions", ret = asset_ret)],
  pos_c[,   .(MDate, series = fifelse(advised, "Advised", "Not advised"), ret = asset_ret)],
  smi_m[MDate %in% pos_all$MDate, .(MDate, series = "SMI", ret = smi_return)]
))

setorder(cmp, series, MDate)
cmp[, idx := idx100(ret, MDate, base), by = series]

series_col <- c("All positions" = "#2166ac", "Advised" = "#1b7837",
                "Not advised" = "#4393c3", "SMI" = "#d6604d")

plot_idx <- function(dat, title) {
  ggplot(dat, aes(x = MDate, y = idx, color = series, linetype = series)) +
    geom_rect(data = vol_bands[xmax >= min(dat$MDate) & xmin <= max(dat$MDate)],
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = vol_lab),
              inherit.aes = FALSE, alpha = 0.13) +
    geom_hline(yintercept = 100, linewidth = 0.3, color = "grey60") +
    geom_vline(xintercept = base, linewidth = 0.3, color = "grey60", linetype = "dashed") +
    geom_line(linewidth = 0.9) +
    scale_fill_manual(values = setNames("grey35", vol_lab)) +
    scale_color_manual(values = series_col) +
    scale_linetype_manual(values = c("All positions" = "solid", "Advised" = "solid",
                                     "Not advised" = "solid", "SMI" = "22")) +
    scale_x_date(date_breaks = "2 years", date_minor_breaks = "1 year",
                 date_labels = "%Y", expand = expansion(mult = c(0.01, 0.03))) +
    labs(x = NULL, y = paste0("Index (", format(base, "%b %Y"), " = 100)"),
         color = NULL, linetype = NULL, fill = NULL, title = title,
         subtitle = "Value-weighted monthly return on equity and fund positions") +
    # color and linetype must carry an identical guide spec, otherwise ggplot
    # refuses to merge them and draws the series legend twice
    guides(color    = guide_legend(order = 1),
           linetype = guide_legend(order = 1),
           fill     = guide_legend(order = 2, override.aes = list(alpha = 0.3))) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", legend.box = "vertical", legend.spacing.y = unit(2, "pt"),
          panel.grid.minor.y = element_blank())
}

p_idx <- plot_idx(cmp[series %in% c("All positions", "SMI")],
                  "Portfolio performance vs. SMI")
p_idx
save_fig(p_idx, "portfolio_index_vs_smi")


# ==============================================================================
# same, split by advised vs. non-advised positions
# ==============================================================================

# advised was already NA -> FALSE above, so "Not advised" = no advised trade
# in that month, whether or not the client traded at all
p_adv <- plot_idx(cmp[series != "All positions"],
                  "Portfolio performance by advice status vs. SMI")
p_adv
save_fig(p_adv, "portfolio_index_by_advice_vs_smi")

