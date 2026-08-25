# positionen analyse: person-level advice labels around uncertainty periods
#
# crisis periods are the deepest SMI drawdowns inside the pos sample window: peak
# month through trough month, carried one month past the trough. within each
# period every person is labelled advised_client:
#   TRUE   at least one advised trade in that period (incl. the trailing month)
#   FALSE  active in the period, but no advised trade
#   NA     month does not belong to any crisis period

library(data.table)
library(arrow)
library(lubridate)
library(ggplot2)

rm(list=ls());gc()

out_fig_path <- "C:/Users/FWehmann/Dropbox/Apps/Overleaf/Uncertainty and Financial Advise (1)/figures"

# every figure in this script goes out through here, so format and size stay uniform
save_fig <- function(p, name, width = 8, height = 5) {
  ggsave(file.path(out_fig_path, paste0(name, ".pdf")), p, width = width, height = height)
  invisible(p)
}

# Bp_ID = business partner (the level `advised` was built on in merge_advise.R,
# contacts are matched per Bp_ID). switch to "Person_ID" to label natural persons,
# who may hold several Bp_IDs.
id_col <- "Bp_ID"

smp_start <- as.Date("2011-01-01")   # start of the pos sample; the SMI is cut to it
n_crises  <- 6      # how many of the deepest SMI drawdowns count as crisis periods
dd_min    <- 0.10   # a drawdown must reach this depth to qualify at all
unc_lag   <- 0      # months carried past the trough


instr_sel <- c("Aktien","Fonds","Obligationen","Strukturierte Prod./Zertifikate","Optionen")
instr_sel <- c("Aktien","Fonds")


# ==============================================================================
# data
# ==============================================================================

pos_file <- "../data/pos_b_merged.parquet"

# reading the whole parquet does not fit in memory, and most of it is never used:
# the wide character columns (asset names, descriptions, ...) alone dwarf the
# numeric ones. so only the columns this script touches are projected, and the
# smp_start cut is pushed into the scan - both happen inside arrow, before
# anything is materialised in R.
pos_cols <- unique(c(id_col, "Bp_ID", "Cont_ID", "Asset_ID", "MDate",
                     "advised","K_Aufnahme", "Instrumentengruppe",
                     "Geschaeftsvolumen_CHF", "DA_Titelkursabweichung_CHF","DA_Mengenabweichung_CHF",
                     "DA_Devisenkursabweichung_CHF","DA_Delta_CHF","DA_Gesamtabweichung_CHF"))

pos_ds <- arrow::open_dataset(pos_file)

miss <- setdiff(pos_cols, names(pos_ds))
if (length(miss)) stop("not in ", pos_file, ": ", paste(miss, collapse = ", "))

# the date cut can only be pushed down if MDate is stored as a date/timestamp;
# if it sits in the file as a string, it is filtered after the read instead
mdate_type <- pos_ds$schema$GetFieldByName("MDate")$type$ToString()
push_date <- grepl("date|timestamp", mdate_type, ignore.case = TRUE) &&
  requireNamespace("dplyr", quietly = TRUE)

pos <- if (push_date) {
  # arrow's dplyr backend turns this into a projected + filtered scan; nothing
  # but the surviving rows of the selected columns is ever built in memory
  dplyr::collect(dplyr::filter(dplyr::select(pos_ds, dplyr::all_of(pos_cols)),
                               MDate >= smp_start))
} else {
  # no pushdown here, but the wide columns are still never read
  arrow::read_parquet(pos_file, col_select = tidyselect::all_of(pos_cols))
}
setDT(pos)
rm(pos_ds); gc()

pos[, MDate := as.Date(MDate)]
pos <- pos[MDate >= smp_start]   # no-op when the cut was already pushed down

# a handful of distinct values over millions of rows - as a factor this costs an
# integer per row instead of a string pointer. `%in% instr_sel` is unaffected
if (is.character(pos$Instrumentengruppe)) {
  pos[, Instrumentengruppe := factor(Instrumentengruppe)]
}

# add numeric advised by
pos[,advised_init_client := ifelse(is.na(K_Aufnahme),0,ifelse(K_Aufnahme=="Durch Kunde",1,0))]
pos[,advised_init_advisor := ifelse(is.na(K_Aufnahme),0,ifelse(K_Aufnahme=="Durch Kundenberater",1,0))]

gc()

# hsmi.csv: ";"-separated, 5 header rows, dates descending, dd.mm.yyyy
# col 1 = DATE, col 2 = SMI PR (price index), col 5 = SMIC (SMI total return)
smi <- fread("../data/hsmi.csv", skip = 4, select = c(1, 2, 5))
setnames(smi, c("Date", "SMI", "SMI_TR"))

smi[, Date := as.Date(Date, format = "%d.%m.%Y")]
smi <- smi[!is.na(Date)][order(Date)]

# the SMI history reaches further back than pos. cut it to the pos sample so the
# volatility SD - and with it the uncertainty threshold - is estimated on exactly
# the window the periods are defined over
smi <- smi[Date >= smp_start]

smi[, MDate := floor_date(Date, "month")]

# last trading day of each month, dated to the 1st so it merges on pos$MDate
smi_m <- smi[, .(Date_eom = last(Date),
                 SMI      = last(SMI),
                 SMI_TR   = last(SMI_TR)), by = MDate]
smi_m[, smi_return    := SMI    / shift(SMI)    - 1]
smi_m[, smi_tr_return := SMI_TR / shift(SMI_TR) - 1]


# ==============================================================================
# crisis periods: the largest SMI drawdowns in the sample window
# ==============================================================================

# a drawdown is measured against the running peak of the sample, so every new
# high closes the running episode and opens the next one
smi[, ret_d := SMI / shift(SMI) - 1]
smi[, peak  := cummax(SMI)]
smi[, dd    := SMI / peak - 1]
smi[, epi   := cumsum(dd == 0)]

# one row per drawdown episode: the peak it fell from, how deep it got and when.
# episodes shallower than dd_min are wobbles, not crises
dd_epi <- smi[, .(peak_date   = first(Date),
                  trough_date = Date[which.min(dd)],
                  depth       = min(dd),
                  epi_end     = last(Date)), by = epi][depth <= -dd_min]

# keep the deepest few, then put them back in chronological order
setorder(dd_epi, depth)
dd_epi <- head(dd_epi, n_crises)
setorder(dd_epi, peak_date)

dd_epi

# the crisis period is the decline itself - peak month through trough month,
# carried unc_lag months past the trough. the recovery back to the old high is
# deliberately not included
unc <- dd_epi[, .(start = floor_date(peak_date, "month"),
                  end   = floor_date(trough_date, "month") %m+% months(unc_lag),
                  depth, peak_date, trough_date, epi_end)]
unc[, unc_id := .I]
setcolorder(unc, "unc_id")

# the trailing months must not run into the next crisis: every calendar month has
# to belong to at most one period, or the labelling join below would double-count
unc[, end := pmin(end, shift(start, -1, fill = as.Date("2999-01-01")) %m-% months(1))]
unc[, n_months := (year(end) * 12 + month(end)) - (year(start) * 12 + month(start)) + 1]

unc

# one row per (period, month) - the lookup that puts months into periods
unc_months <- unc[, .(MDate = seq(start, end, by = "month"), unc_start = start), by = unc_id]
stopifnot(!anyDuplicated(unc_months$MDate))


# ==============================================================================
# label persons within each uncertainty period
# ==============================================================================

# update joins rather than merge(): pos is large and this avoids the copies
drop_cols <- intersect(c("unc_id", "unc_start", "advised_client"), names(pos))
if (length(drop_cols)) pos[, (drop_cols) := NULL]           # idempotent re-runs
pos[unc_months, on = "MDate", `:=`(unc_id = i.unc_id, unc_start = i.unc_start)]

# `advised` is NA on position rows without a matching trade, so %in% TRUE keeps
# "no trade" out of the TRUE bucket without touching the source column
lab <- pos[!is.na(unc_id) & !is.na(pos[[id_col]]),
           .(advised_client = any(advised %in% TRUE),
             n_pos          = .N,
             n_advised      = sum(advised %in% TRUE)),
           by = c(id_col, "unc_id")]

pos[lab, on = c(id_col, "unc_id"), advised_client := i.advised_client]


# ==============================================================================
# checks
# ==============================================================================

# rows outside any period must be NA, rows inside must never be
pos[, .N, by = .(in_unc_period = !is.na(unc_id), advised_client)]
stopifnot(pos[is.na(unc_id), all(is.na(advised_client))],
          pos[!is.na(unc_id) & !is.na(pos[[id_col]]), !any(is.na(advised_client))])

# the label is constant within person x period, by construction - verify it held
stopifnot(pos[!is.na(unc_id) & !is.na(pos[[id_col]]),
              .(u = uniqueN(advised_client)), by = c(id_col, "unc_id")][, max(u)] == 1)

# how many persons are advised in each period
lab_sum <- lab[, .(n_persons      = .N,
                   n_advised      = sum(advised_client),
                   share_advised  = mean(advised_client)), by = unc_id]
lab_sum <- merge(unc, lab_sum, by = "unc_id")
lab_sum

# ==============================================================================
# indexed performance by trade status, against the SMI, with the uncertainty
# periods shaded
# ==============================================================================

vcol <- c("Geschaeftsvolumen_CHF","DA_Titelkursabweichung_CHF","DA_Mengenabweichung_CHF","DA_Devisenkursabweichung_CHF","DA_Delta_CHF","DA_Gesamtabweichung_CHF")

# the indicator the figures group on. all of them are logical columns on pos, so
# switching a figure over is a one-line change:
#   advised         trade level  - was this position traded on advice this month?
#                                  NA means the position was not traded at all
#   advised_client  person level - advised trade anywhere in the uncertainty period,
#                                  NA outside any period
#   Bp_advised      person level - same idea but defined per event window, built in
#                                  the covid section below (never NA)
grp_col <- "advised"

# how each indicator presents itself: labels for TRUE / FALSE / NA, the colors
# those labels take, and the wording the figure uses
grp_spec <- list(
  advised = list(
    lab  = c(t = "Advised trade", f = "Not advised trade", na = "Not traded"),
    col  = c("Advised trade" = "#1b7837", "Not advised trade" = "#4393c3",
             "Not traded" = "#762a83"),
    desc = "trade status",
    note = "advised = trade preceded by an investment contact",
    file = "by_trade_status"
  ),
  advised_client = list(
    lab  = c(t = "Advised client", f = "Not advised client",
             na = "Outside uncertainty period"),
    col  = c("Advised client" = "#1b7837", "Not advised client" = "#4393c3",
             "Outside uncertainty period" = "#762a83"),
    desc = "client advice status",
    note = "advised = at least one advised trade in the uncertainty period",
    file = "by_client_advice"
  ),
  Bp_advised = list(
    lab  = c(t = "Advised client", f = "Not advised client", na = NA_character_),
    col  = c("Advised client" = "#1b7837", "Not advised client" = "#4393c3"),
    desc = "client advice status",
    note = "advised = at least one advised trade in an uncertainty period inside the window",
    file = "by_client_advice"
  )
)

smi_col <- c(SMI = "#d6604d")

# map a logical indicator onto its series labels. a state the spec leaves NA is
# dropped from the figure rather than drawn as an unnamed group
lab_series <- function(x, spec) {
  fifelse(is.na(x), spec$lab[["na"]], fifelse(x, spec$lab[["t"]], spec$lab[["f"]]))
}

pos_c <- pos[Instrumentengruppe %in% instr_sel,
             lapply(.SD, mean, na.rm=T), by = c("MDate", grp_col), .SDcols = vcol]

base_pos <- as.Date("2022-01-01")   # rebasing month for asset_idx

pos_c[, asset_ret := DA_Titelkursabweichung_CHF / (Geschaeftsvolumen_CHF-DA_Gesamtabweichung_CHF)]
smi_m[is.na(smi_return), smi_return := 0]

# compound and normalise to 1 in the base month. every series goes through this,
# so the portfolio lines and the SMI are built the same way
idx1 <- function(ret, dates, base) {
  ci <- cumprod(1 + fifelse(is.na(ret), 0, ret))
  ci / ci[dates == base]
}

# one long table: the portfolio groups and the SMI on a shared scale and legend
spec <- grp_spec[[grp_col]]
cmp <- rbindlist(list(
  pos_c[, .(MDate, series = lab_series(get(grp_col), spec), ret = asset_ret)],
  smi_m[, .(MDate, series = "SMI", ret = smi_return)]
))
cmp <- cmp[!is.na(series)]

# hold every series to the months both sources cover, so the lines start and end
# together and the index is not built over a window one of them lacks
cmp <- cmp[MDate %between% c(max(min(pos_c$MDate), min(smi_m$MDate)),
                             min(max(pos_c$MDate), max(smi_m$MDate)))]

setorder(cmp, series, MDate)   # cumprod is order-dependent!
cmp[, idx := idx1(ret, MDate, base_pos), by = series]

# uncertainty periods as background bands. `end` is a month start, so extend it
# by one month to cover that month in full
unc_bands <- unc[, .(xmin = start, xmax = end %m+% months(1))]
# unc_lab   <- paste0("Crisis period (", n_crises, " deepest SMI drawdowns, peak to trough +",
#                     unc_lag, "m)")
unc_lab   <- paste0("Crisis period (", n_crises, " deepest SMI drawdowns)")

series_col <- c(spec$col, smi_col)

# the figure recipe: `dat` is long (MDate, series, idx), `series_col` names the
# series and fixes their colors. reused for the event windows further down.
plot_idx_unc <- function(dat, base, title, series_col,
                         subtitle    = "Value-weighted monthly return on the selected instrument groups",
                         date_breaks = "2 years", date_minor = "1 year", date_labels = "%Y") {
  # SMI dashed, portfolio series solid - identity never rests on color alone
  lt <- setNames(fifelse(names(series_col) == "SMI", "22", "solid"), names(series_col))

  # bands clipped to the plotted window, so they can never stretch the axis
  bnd <- unc_bands[xmax >= min(dat$MDate) & xmin <= max(dat$MDate)]
  bnd[, `:=`(xmin = pmax(xmin, min(dat$MDate)), xmax = pmin(xmax, max(dat$MDate)))]

  ggplot(dat, aes(x = MDate, y = idx, color = series, linetype = series)) +
    geom_rect(data = bnd, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = unc_lab),
              inherit.aes = FALSE, alpha = 0.13) +
    geom_hline(yintercept = 1, linewidth = 0.3, color = "grey60") +
    geom_vline(xintercept = base, linewidth = 0.3, color = "grey60", linetype = "dashed") +
    geom_line(linewidth = 0.9) +
    scale_fill_manual(values = setNames("grey35", unc_lab)) +
    scale_color_manual(values = series_col) +
    scale_linetype_manual(values = lt) +
    scale_x_date(date_breaks = date_breaks, date_minor_breaks = date_minor,
                 date_labels = date_labels, expand = expansion(mult = c(0.01, 0.03))) +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
    labs(x = NULL, y = paste0("Index (", format(base, "%b %Y"), " = 1)"),
         color = NULL, linetype = NULL, fill = NULL,
         title = title, subtitle = subtitle) +
    # color and linetype must carry an identical guide spec, otherwise ggplot
    # refuses to merge them and draws the series legend twice
    guides(color    = guide_legend(order = 1),
           linetype = guide_legend(order = 1),
           fill     = guide_legend(order = 2, override.aes = list(alpha = 0.3))) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", legend.box = "vertical",
          legend.spacing.y = unit(2, "pt"), panel.grid.minor.y = element_blank())
}

p_adv_unc <- plot_idx_unc(cmp, base_pos,
                          paste0("Cumulative asset performance by ", spec$desc, " vs. SMI"),
                          series_col,
                          subtitle = paste0(spec$note,
                                            " | value-weighted monthly return on the selected instrument groups"))
p_adv_unc
save_fig(p_adv_unc, paste0("asset_index_", spec$file, "_unc_periods"))


##=====
## Covid

start_covid_window <- "2019-01-01"
end_covid_window <- "2022-01-01"

pos_covid <- pos[MDate >= start_covid_window & MDate <= end_covid_window]

pos_covid[,advised_client_num := as.numeric(advised_client)]
pos_covid[is.na(advised_client_num),advised_client_num:=0]

pos_covid_pers <- pos_covid[,sum(advised_client_num),by=Bp_ID]
covid_pers_advised <- pos_covid_pers[V1>0,Bp_ID]

pos_covid[,Bp_advised := FALSE]
pos_covid[Bp_ID %in% covid_pers_advised,Bp_advised := T]

# "Bp_advised" splits by client (advised anywhere in the window), "advised" by
# trade. everything below - labels, colors, title, file name - follows from this
covid_grp <- "advised"  # "Bp_advised"
spec_c    <- grp_spec[[covid_grp]]

pos_covid_c <- pos_covid[Instrumentengruppe %in% instr_sel,
                         lapply(.SD, mean, na.rm=T), by = c("MDate", covid_grp), .SDcols = vcol]

# rebased on the first month of the window, so the lines start together and the
# plot reads as performance since the eve of the crisis
base_covid <- as.Date(start_covid_window)
base_covid <- as.Date("2020-01-01")

pos_covid_c[, asset_ret := DA_Titelkursabweichung_CHF / Geschaeftsvolumen_CHF]

cmp_covid <- rbindlist(list(
  pos_covid_c[, .(MDate, series = lab_series(get(covid_grp), spec_c), ret = asset_ret)],
  smi_m[MDate %between% c(as.Date(start_covid_window), as.Date(end_covid_window)),
        .(MDate, series = "SMI", ret = smi_return)]
))
cmp_covid <- cmp_covid[!is.na(series)]

setorder(cmp_covid, series, MDate)   # cumprod is order-dependent!
cmp_covid[, idx := idx1(ret, MDate, base_covid), by = series]

p_covid <- plot_idx_unc(cmp_covid, base_covid,
                        paste0("Covid window: asset performance by ", spec_c$desc),
                        c(spec_c$col, smi_col),
                        subtitle = paste0(spec_c$note,
                                          " | value-weighted monthly return on the selected instrument groups"),
                        date_breaks = "6 months", date_minor = "1 month",
                        date_labels = "%b %Y")
p_covid
save_fig(p_covid, paste0("asset_index_covid_", spec_c$file))


### ==========================================================================
## positions traded on advice during the crash, followed across the covid window

# a position is a client's holding of one asset in one contract. `advised` was
# attached at Bp_ID x Asset_ID x month, so it is constant across a client's
# contracts holding the same asset
pos_key <- c("Bp_ID", "Cont_ID", "Asset_ID")

crisis_start <- as.Date("2020-02-01")   # selection window: advised trade in here
crisis_end   <- as.Date("2020-05-01")
cwin_start   <- as.Date("2019-01-01")   # display window: performance shown here
cwin_end     <- as.Date("2021-12-01")

# crisis_start <- as.Date("2021-12-01")   # selection window: advised trade in here
# crisis_end   <- as.Date("2022-11-01")
# cwin_start   <- as.Date("2020-01-01")   # display window: performance shown here
# cwin_end     <- as.Date("2023-12-01")

# crisis_start <- as.Date("2018-08-01")   # selection window: advised trade in here
# crisis_end   <- as.Date("2019-01-01")
# cwin_start   <- as.Date("2017-07-01")   # display window: performance shown here
# cwin_end     <- as.Date("2020-02-01")

grp_spec$crisis_advised <- list(
  lab  = c(t = "Traded on advice in the crash", f = "All other positions",
           na = NA_character_),
  col  = c("Traded on advice in the crash" = "#1b7837",
           "All other positions" = "#4393c3"),
  desc = "advised trading during the crash",
  note = paste0("group selected on an advised trade between ", format(crisis_start, "%b %Y"),
                " and ", format(crisis_end, "%b %Y")),
  file = "by_crisis_advised"
)
spec_x <- grp_spec$crisis_advised

# the positions that saw an advised trade inside the crisis window. the group is
# fixed by that selection and then followed over the whole display window, so the
# pre-crash gap between the lines is composition, not a treatment effect
crisis_keys <- unique(pos[MDate %between% c(crisis_start, crisis_end) & advised %in% TRUE,
                          ..pos_key])

pos_cw <- pos[MDate %between% c(cwin_start, cwin_end) & Instrumentengruppe %in% instr_sel]
pos_cw[, crisis_advised := FALSE]
pos_cw[crisis_keys, on = pos_key, crisis_advised := TRUE]

# how thin the selected group is, month by month - the index is only as reliable
# as the number of positions behind it
pos_cw[, .N, by = .(MDate, crisis_advised)][order(MDate, crisis_advised)]

pos_cw_c <- pos_cw[, lapply(.SD, mean, na.rm = TRUE),
                   by = c("MDate", "crisis_advised"), .SDcols = vcol]
pos_cw_c[, asset_ret := DA_Titelkursabweichung_CHF / Geschaeftsvolumen_CHF]

base_cw <- as.Date("2020-01-01")   # rebased just before the crash, not on the window start

cmp_cw <- rbindlist(list(
  pos_cw_c[, .(MDate, series = lab_series(crisis_advised, spec_x), ret = asset_ret)],
  smi_m[MDate %between% c(cwin_start, cwin_end), .(MDate, series = "SMI", ret = smi_return)]
))
cmp_cw <- cmp_cw[!is.na(series)]

setorder(cmp_cw, series, MDate)   # cumprod is order-dependent!
cmp_cw[, idx := idx1(ret, MDate, base_cw), by = series]

p_crisis <- plot_idx_unc(cmp_cw, base_cw,
                         paste0("Covid window: asset performance by ", spec_x$desc),
                         c(spec_x$col, smi_col),
                         subtitle = paste0(spec_x$note,
                                           " | value-weighted monthly return on the selected instrument groups"),
                         date_breaks = "6 months", date_minor = "1 month",
                         date_labels = "%b %Y") +
  # the selection window itself, so the read-off is not mistaken for a pre-trend
  geom_vline(xintercept = c(crisis_start, crisis_end), linewidth = 0.3,
             color = "grey40", linetype = "dotted")

p_crisis
save_fig(p_crisis, paste0("asset_index_covid_", spec_x$file))


### ==========================================================================
## DID







ntrades <- pos[,.(N_advised = sum(advised,na.rm=T),
                  N_trades = .N,
                  N_Bp = uniqueN(Bp_ID),
                  N_advised_init_client = sum(advised_init_client,na.rm=T),
                  N_advsied_init_advisor = sum(advised_init_advisor,na.rm=T)),by=MDate]
ntrades[,advised_per_p := N_advised/N_Bp]
ntrades[,advised_init_c_per_p := N_advised_init_client/N_Bp]
ntrades[,advised_init_a_per_p := N_advsied_init_advisor/N_Bp]
ntrades[,total_per_p := N_trades/N_Bp]
bnd <- unc_bands[xmax >= min(ntrades$MDate) & xmin <= max(ntrades$MDate)]

p_nadvised_trades <- ggplot()+
  geom_rect(data = bnd, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = unc_lab),
            inherit.aes = FALSE, alpha = 0.3)+
  geom_line(data=ntrades,aes(x=MDate,y=advised_init_c_per_p,color="Initiated by Client"),size=.8)+
  geom_line(data=ntrades,aes(x=MDate,y=advised_init_a_per_p,color="Initiated by Advisor"),size=.8)+
  geom_line(data=smi_m[MDate<=max(ntrades$MDate)],aes(x=MDate,y=cumprod(1+smi_return)/10/2-0.05,color="SMI (scaled)"),linetype=2,size=1)+
  guides(fill=guide_legend(title=NULL),
         color=guide_legend(title=NULL))+
  theme_light()+
  theme(legend.position="bottom")
p_nadvised_trades
save_fig(p_nadvised_trades, paste0("n_trades_advised"))

# ggplot()+
#   geom_rect(data = bnd, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = unc_lab),
#             inherit.aes = FALSE, alpha = 0.13)+
#   geom_line(data=ntrades,aes(x=MDate,y=advised_per_p))+
#   # geom_line(data=ntrades,aes(x=MDate,y=total_per_person))+
#   geom_line(data=smi_m,aes(x=MDate,y=cumprod(1+smi_return)/10-0.1),linetype=2,color="tomato4",size=1)+
#   guides(fill=NULL)
# 
# ggplot()+
#   geom_rect(data = bnd, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = unc_lab),
#             inherit.aes = FALSE, alpha = 0.13)+
#   # geom_line(data=ntrades,aes(x=MDate,y=advised_per_person))+
#   geom_line(data=ntrades,aes(x=MDate,y=total_per_p))+
#   geom_line(data=smi_m,aes(x=MDate,y=cumprod(1+smi_return)*3+1),linetype=2,color="tomato4",size=1)+
#   guides(fill="none")



