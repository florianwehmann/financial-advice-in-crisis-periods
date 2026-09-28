# investor behavior during COVID

library(data.table)
library(arrow)
library(dplyr)
library(ggplot2)

rm(list=ls());gc()

## ===========================================================================
## FUNCTIONS

winsor <- function(x, p = 0.99) {
  q <- quantile(x[is.finite(x)], c(1 - p, p), na.rm = TRUE)
  x[is.infinite(x)] <- NA_real_
  pmin(pmax(x, q[1]), q[2])
}

## ===========================================================================


# dirs
overleaf_dir <- "C:/Users/FWehmann/Dropbox/Apps/Overleaf/Uncertainty and Financial Advise (1)"
fig_dir      <- file.path(overleaf_dir, "figures/invbeh")
tab_dir      <- file.path(overleaf_dir, "tables")

# read pos file
pos <- read_parquet("../../data/pos_pf.parquet")

# read smi file
smi <- fread("../../data/hsmi.csv",skip=4,select=c(1,2))
smi[,DATE:=as.Date(DATE,format="%d.%m.%Y")]
setorder(smi,DATE)
smi[,MDate := lubridate::rollforward(DATE)]   # ceiling_date()-1 maps the 1st of a month to the previous month
smim <- smi[,.(smi=last(Close)),by=.(MDate)]


# ---------------------------------------------------------------------------------------
# chose crisis (Covid) window
smim_c <- smim[MDate %between% c("2019-01-01","2021-04-01")]
pos_c <- pos[MDate %between% c("2019-01-01","2021-04-01")]


cl <- pos_c[MDate=="2020-01-31"]

bp_filter <- cl[tot_pf>5000,Bp_ID]

pos_c <- pos_c[Bp_ID%in%bp_filter]

# create variables for pos
setorder(pos_c,Bp_ID,MDate)
pos_c[is.na(n_trades),n_trades:=0]
pos_c[is.na(n_traded_assets),n_traded_assets:=0]

pos_c[,traded := as.numeric(n_traded_assets>0)]
pos_c[,traded2 := as.numeric(n_trades>0)]

pos_c[,net_buyer := as.numeric(chf_net>0)]

pos_c[,dq_eq_pct := dq_equity/shift(tot_pf),by=Bp_ID]
pos_c[,dq_bd_pct := dq_bond/shift(tot_pf),by=Bp_ID]
pos_c[,dq_pf_pct := dq_tot_pf/shift(tot_pf),by=Bp_ID]

pos_c[is.infinite(dq_eq_pct),dq_eq_pct := NA]
pos_c[is.infinite(dq_bd_pct),dq_bd_pct := NA]
pos_c[is.infinite(dq_pf_pct),dq_pf_pct := NA]

pos_c[,liq_share := cash_liq/tot_wealth]
pos_c[,lock_share := cash_locked/tot_wealth]
pos_c[,liq_share_fixw := cash_liq/tot_wealth[MDate=="2020-01-31"][1],by=Bp_ID]
pos_c[,lock_share_fixw := cash_locked/tot_wealth[MDate=="2020-01-31"][1],by=Bp_ID]

# ---------------------------------------------------------------------------------------
# aggregate pos
agg_cols <- c("equity","dp_equity","dq_equity","bond","dp_bond","dq_bond","reales","dp_reales","dq_reales",
              "fund_mixed","dq_fund_mixed","dp_fund_mixed","deriv","dq_deriv","dp_deriv","alt","dq_alt","dp_alt",
              "tot_pf","dp_tot_pf","dq_tot_pf","dfx_tot_pf",
              "tot_wealth","cash_liq","cash_locked","n_trades","n_buys","n_sells",
              "chf_bought","chf_sold","n_traded_assets","chf_net_equity","chf_net_bond",
              "chf_net_deriv","chf_net_fund_mixed","pf_share_of_w","eq_share_of_pf","eq_share_of_w",
              "dq_eq_pct","dq_bd_pct","dq_pf_pct","traded","traded2","liq_share","lock_share",
              "liq_share_fixw","lock_share_fixw","net_buyer")


# winsorize first
pos_c_w <- copy(pos_c)
pos_c_w[, (paste0(agg_cols,"_w")) := lapply(.SD, winsor, p = 0.999), .SDcols = agg_cols]

# aggregate
pos_c_a_sum <- pos_c_w[,lapply(.SD,sum,na.rm=T),.SDcols=c(agg_cols,paste0(agg_cols,"_w")),by=.(MDate)]
pos_c_a_mean <- pos_c_w[,lapply(.SD,mean,na.rm=T),.SDcols=c(agg_cols,paste0(agg_cols,"_w")),by=.(MDate)]

pos_c_n <- pos_c_w[,.(N=.N,
                    N_traded = sum(traded),
                    N_trades = sum(n_traded_assets),
                    N_buyers = sum(net_buyer,na.rm=T)
                    ),by=.(MDate)]
pos_c_n[,traded_sh := N_traded/N]
pos_c_n[,trds_per_inv := N_trades/N]
pos_c_n[,buyers_sh_of_traders := N_buyers/N_traded]


pos_c_a_sum[,dq_eq_pct := dq_equity/shift(tot_pf)]
pos_c_a_sum[,dq_bd_pct := dq_bond/shift(tot_pf)]
pos_c_a_sum[,dq_pf_pct := dq_tot_pf/shift(tot_pf)]
pos_c_a_sum[,dq_eq_pct_w := dq_equity_w/shift(tot_pf_w)]
pos_c_a_sum[,dq_bd_pct_w := dq_bond_w/shift(tot_pf_w)]
pos_c_a_sum[,dq_pf_pct_w := dq_tot_pf_w/shift(tot_pf_w)]

pos_c_a_mean[,liq_share_vw := cash_liq / tot_wealth]
pos_c_a_mean[,liq_share_vw_w := cash_liq_w / tot_wealth_w]
pos_c_a_mean[,liq_share_fixw_vw := cash_liq / tot_wealth[MDate=="2020-01-31"]]
pos_c_a_mean[,liq_share_fixw_vw_w := cash_liq_w / tot_wealth_w[MDate=="2020-01-31"]]

pos_c_a_mean[,pf_share_of_w_vw := tot_pf/tot_wealth]
pos_c_a_mean[,eq_share_of_pf_vw := equity/tot_pf]
pos_c_a_mean[,eq_share_of_w_vw := equity/tot_wealth]
pos_c_a_mean[,pf_share_of_w_vw_w := tot_pf_w/tot_wealth_w]
pos_c_a_mean[,eq_share_of_pf_vw_w := equity_w/tot_pf_w]
pos_c_a_mean[,eq_share_of_w_vw_w := equity_w/tot_wealth_w]

# ---------------------------------------------------------------------------------------
# FIGURES
# All figures share one theme, the same crash shading and fixed colours per entity,
# and are written as PDF to fig_dir (captions live in LaTeX, so no in-plot titles).

dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# colours: fixed per entity, never re-cycled across figures
col_blue   <- "#2a78d6"
col_orange <- "#eb6834"
col_aqua   <- "#1baf7a"
col_red    <- "#e34948"
col_ink    <- "grey25"

crash_start <- as.Date("2020-02-01")   # SMI peak 19 Feb 2020, trough 16 Mar 2020
crash_end   <- as.Date("2020-03-31")
x_lims      <- range(smim_c$MDate)

theme_ib <- theme_minimal(base_size = 10) +
  theme(panel.grid.minor      = element_blank(),
        panel.grid.major.x    = element_blank(),
        panel.grid.major.y    = element_line(colour = "grey90", linewidth = 0.3),
        axis.ticks.x          = element_line(colour = "grey60", linewidth = 0.3),
        axis.text             = element_text(colour = "grey35"),
        axis.title            = element_text(colour = "grey25"),
        strip.text            = element_text(face = "bold", hjust = 0, colour = "grey15"),
        legend.position       = "top",
        legend.justification  = "left",
        legend.margin         = margin(0, 0, 0, 0),
        legend.key.width      = unit(1.2, "lines"),
        panel.spacing         = unit(1.1, "lines"),
        plot.caption          = element_text(colour = "grey45", size = 7.5, hjust = 0),
        plot.caption.position = "plot")

# shaded crash window; drawn first so data sits on top
crash_band <- function() {
  annotate("rect", xmin = crash_start, xmax = crash_end, ymin = -Inf, ymax = Inf,
           fill = "grey88", alpha = 0.8)
}
# crash label (when faceted, only in the first panel: pass the facet var and its levels,
# as a factor so the label layer does not re-sort the panels alphabetically)
crash_label <- function(facet_var = NULL, facet_lv = NULL) {
  d <- data.frame(MDate = crash_start, y = Inf, lab = "COVID-19 crash")
  if (!is.null(facet_var)) d[[facet_var]] <- factor(facet_lv[1], levels = facet_lv)
  geom_text(data = d, aes(x = MDate, y = y, label = lab), inherit.aes = FALSE,
            hjust = 1.05, vjust = 1.6, size = 2.6, colour = "grey40")
}
x_scale <- scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y",
                        limits = x_lims, expand = expansion(mult = 0.015))
zero_line <- geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.35)

save_fig <- function(p, name, w = 6.5, h = 4) {
  ggsave(file.path(fig_dir, paste0(name, ".pdf")), p, width = w, height = h,
         device = cairo_pdf)
  invisible(p)
}

# ---------------------------------------------------------------------------------------
# 1) SMI over the window (context)
p_smi <- ggplot(smim_c, aes(MDate, smi)) +
  crash_band() + crash_label() +
  geom_line(colour = col_ink, linewidth = 0.7) +
  x_scale +
  scale_y_continuous(labels = scales::label_comma(big.mark = "'")) +
  labs(x = NULL, y = "SMI (month-end close)") +
  theme_ib
save_fig(p_smi, "ib_smi", h = 3)

p_smi
# ---------------------------------------------------------------------------------------
# 2) Trading activity: stacked panels instead of a dual axis
#    (#investors traded is ~proportional to the share: N is ~16.5k throughout)
act_lv <- c("SMI (month-end close)",
            "Investors trading (% of clients)",
            "Assets traded per client")
act <- rbind(
  smim_c[, .(MDate, panel = act_lv[1], value = smi)],
  pos_c_n[, .(MDate, panel = act_lv[2], value = 100 * traded_sh)],
  pos_c_n[, .(MDate, panel = act_lv[3], value = trds_per_inv)])
act[, panel := factor(panel, levels = act_lv)]

p_act <- ggplot(act, aes(MDate, value)) +
  crash_band() + crash_label("panel", act_lv) +
  geom_line(aes(colour = panel == act_lv[1]), linewidth = 0.7) +
  geom_point(data = act[panel != act_lv[1]], colour = col_blue, size = 1.1) +
  scale_colour_manual(values = c(`TRUE` = col_ink, `FALSE` = col_blue), guide = "none") +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  x_scale +
  scale_y_continuous(labels = scales::label_number(big.mark = "'")) +
  labs(x = NULL, y = NULL) +
  theme_ib
save_fig(p_act, "ib_trading_activity", h = 6)
p_act

# ---------------------------------------------------------------------------------------
# 3) Trade volume: CHF and number of trades, buys vs sells
side_cols <- c(Buys = col_blue, Sells = col_orange)
vol_lv <- c("Volume traded (CHF million)", "Number of trades")
vol <- rbind(
  pos_c_a_sum[, .(MDate, panel = vol_lv[1], Buys = chf_bought / 1e6, Sells = chf_sold / 1e6)],
  pos_c_a_sum[, .(MDate, panel = vol_lv[2], Buys = as.numeric(n_buys), Sells = as.numeric(n_sells))])
vol <- melt(vol, id.vars = c("MDate", "panel"), variable.name = "side")
vol[, panel := factor(panel, levels = vol_lv)]

p_vol <- ggplot(vol, aes(MDate, value, colour = side)) +
  crash_band() + crash_label("panel", vol_lv) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.1) +
  scale_colour_manual(values = side_cols, name = NULL) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  x_scale +
  scale_y_continuous(labels = scales::label_number(big.mark = "'"),
                     limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = NULL) +
  theme_ib
save_fig(p_vol, "ib_trade_volume", h = 5)
p_vol

# ---------------------------------------------------------------------------------------
# 3b) Net buyers: share of clients with net CHF purchases (chf_net > 0) among all
#     clients who traded in the month; above 50% = more net buyers than net sellers
p_nb <- ggplot(pos_c_n, aes(MDate, 100 * buyers_sh_of_traders)) +
  crash_band() + crash_label() +
  geom_hline(yintercept = 50, colour = "grey55", linewidth = 0.35, linetype = "22") +
  geom_line(colour = col_blue, linewidth = 0.7) +
  geom_point(colour = col_blue, size = 1.1) +
  x_scale +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = NULL, y = "Net buyers (% of clients who traded)",
       caption = sprintf("Net buyer: CHF bought > CHF sold in the month. Clients who traded per month: %s-%s.",
                         format(min(pos_c_n$N_traded), big.mark = "'"),
                         format(max(pos_c_n$N_traded), big.mark = "'"))) +
  theme_ib
save_fig(p_nb, "ib_net_buyers_share", h = 3.5)
p_nb
# ---------------------------------------------------------------------------------------
# 4) Decomposition of the average monthly portfolio change (winsorized, CHF per client)
dec_cols <- c("Net trading (Δq)" = col_blue,
              "Price effect (Δp)" = col_orange,
              "FX effect"              = col_aqua)
dec <- melt(pos_c_a_mean[, .(MDate, dq_tot_pf_w, dp_tot_pf_w, dfx_tot_pf_w)],
            id.vars = "MDate", variable.name = "comp")
dec[, comp := factor(comp, levels = c("dq_tot_pf_w", "dp_tot_pf_w", "dfx_tot_pf_w"),
                     labels = names(dec_cols))]

p_dec <- ggplot(dec, aes(MDate, value, colour = comp)) +
  crash_band() + crash_label() + zero_line +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.1) +
  scale_colour_manual(values = dec_cols, name = NULL) +
  x_scale +
  scale_y_continuous(labels = scales::label_comma(big.mark = "'")) +
  labs(x = NULL, y = "Mean monthly change per client (CHF)",
       caption = "Client-level values winsorized at 0.1% / 99.9%.") +
  theme_ib
save_fig(p_dec, "ib_pf_change_decomp", h = 3.8)
p_dec
# ---------------------------------------------------------------------------------------
# 5) Allocation shares as change vs Jan 2020 in pp. EW and VW levels differ by up to
#    17pp, so levels on one axis flatten the within-series movement; levels go in the strip.
wt_cols <- c("Equal-weighted" = col_blue, "Value-weighted" = col_orange)
shr_map <- list(
  "Liquid cash / wealth" = c("liq_share",      "liq_share_vw_w"),
  "Portfolio / wealth"   = c("pf_share_of_w",  "pf_share_of_w_vw_w"),
  "Equity / portfolio"   = c("eq_share_of_pf", "eq_share_of_pf_vw_w"),
  "Equity / wealth"      = c("eq_share_of_w",  "eq_share_of_w_vw_w"))
base_date <- as.Date("2020-01-31")
shr <- rbindlist(lapply(names(shr_map), function(nm) {
  v   <- shr_map[[nm]]
  d   <- pos_c_a_mean[, .(MDate, ew = get(v[1]), vw = get(v[2]))]
  b   <- d[MDate == base_date]
  lab <- sprintf("%s  (Jan 2020: EW %.0f%%, VW %.0f%%)", nm, 100 * b$ew, 100 * b$vw)
  rbind(d[, .(MDate, panel = lab, weight = names(wt_cols)[1], value = 100 * (ew - b$ew))],
        d[, .(MDate, panel = lab, weight = names(wt_cols)[2], value = 100 * (vw - b$vw))])
}))
shr[, panel := factor(panel, levels = unique(panel))]

p_shr <- ggplot(shr, aes(MDate, value, colour = weight)) +
  crash_band() + crash_label("panel", levels(shr$panel)) + zero_line +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = wt_cols, name = NULL) +
  facet_wrap(~panel, ncol = 2) +
  x_scale +
  scale_y_continuous(labels = function(x) sub("^[+-]0\\.0$", "0", sprintf("%+.1f", x))) +
  labs(x = NULL, y = "Change since Jan 2020 (pp)",
       caption = "Value-weighted shares are ratios of cross-sectional means (client-level values winsorized at 0.1% / 99.9%).") +
  theme_ib
save_fig(p_shr, "ib_allocation_shares", h = 5)
p_shr
# ---------------------------------------------------------------------------------------
# 6) + 7) Net flows and price changes by asset class: small multiples on a shared axis.
#    Real estate is dropped: its winsorized dq is identically 0 (99.9% quantile = 0).
asset_lv <- c(equity = "Equities", bond = "Bonds", fund_mixed = "Mixed funds",
              deriv = "Derivatives", alt = "Alternatives", tot_pf = "Total portfolio")

asset_bars <- function(dt, prefix, suffix, ylab, cap) {
  cols <- paste0(prefix, names(asset_lv), suffix)
  d <- melt(dt[, c("MDate", cols), with = FALSE], id.vars = "MDate", variable.name = "asset")
  d[, asset := factor(asset, levels = cols, labels = asset_lv)]
  d[, sign := fifelse(value >= 0, "pos", "neg")]
  ggplot(d, aes(MDate, value / 1e6, fill = sign)) +
    crash_band() + crash_label("asset", unname(asset_lv)) + zero_line +
    geom_col(width = 22) +
    scale_fill_manual(values = c(pos = col_blue, neg = col_red), guide = "none") +
    facet_wrap(~asset, ncol = 3) +
    scale_x_date(breaks = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01")),
                 date_labels = "%Y", limits = x_lims + c(-20, 20),
                 expand = expansion(mult = 0.01)) +
    scale_y_continuous(labels = scales::label_comma(big.mark = "'")) +
    labs(x = NULL, y = ylab, caption = cap) +
    theme_ib
}

p_dq <- asset_bars(pos_c_a_sum, "dq_", "_w", "Net flows, all clients (CHF million)",
                   "Quantity change at constant prices; client-level values winsorized at 0.1% / 99.9%. Real estate omitted (zero after winsorizing).")
save_fig(p_dq, "ib_netflows_by_asset", h = 4.5)
p_dq
p_dp <- asset_bars(pos_c_a_sum, "dp_", "", "Price change of holdings, all clients (CHF million)",
                   "Valuation change at constant quantities; not winsorized.")
save_fig(p_dp, "ib_price_change_by_asset", h = 4.5)
p_dp
