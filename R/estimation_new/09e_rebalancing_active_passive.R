# =============================================================================
# 09e_rebalancing_active_passive.R -- two follow-ups to 09_investor_behavior_in_crisis.R
#
# (a) Equity buying in the COVID drawdown: rebalancing into what clients already
#     held, or new assets? Every equity buy (trades.parquet) is matched on
#     Bp_ID x Asset_ID to the client's positions (pos_m1.parquet):
#       Top-up    asset held at the end of the previous month
#       Re-entry  not held last month, but held at some month-end in the prior 12
#       New asset not held at any month-end in the prior 12 months
#
# (b) Equity share = passive (price) drift + active (trading) change. Per month,
#       w_t  = E_t / D_t                       (D = portfolio, or wealth)
#       w^p_t = (E_t - dqE_t) / D^p_t          quantities frozen at t-1, prices move
#       passive_t = w^p_t - w_{t-1}            active_t = w_t - w^p_t
#     so  w_t - w_{t-1} = passive_t + active_t  exactly.
#     Portfolio: D^p_t = P_t - dqP_t. Wealth: D^p_t = W_{t-1} + (P_t - dqP_t - P_{t-1}),
#     i.e. only the price/FX change of the portfolio (cash has no price), so for the
#     wealth share "active" also contains deposits and withdrawals.
#     dq is the quantity change valued at prices; E - dqE is everything else (price
#     and FX), because pos_pf has no FX component for equity alone.
#     Equal-weighted: mean of client contributions; value-weighted: the same identity
#     on cross-sectional sums. Cumulated from Jan 2020 (= 0), as in the 09 allocation figure.
#
# Sample and window as in 09: clients with a portfolio > CHF 5,000 in Jan 2020,
# Jan 2019 - Mar 2021. Equity = 02a's is_equity (shares + share/ETF/index funds).
#
# Run from R/estimation_new. Reads only; writes new files (ib_eqbuy_*, ib_eqshare_*).
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(duckdb)
  library(ggplot2)
})

winsor <- function(x, p = 0.999) {
  q <- quantile(x[is.finite(x)], c(1 - p, p), na.rm = TRUE)
  x[is.infinite(x)] <- NA_real_
  pmin(pmax(x, q[1]), q[2])
}

overleaf_dir <- "C:/Users/FWehmann/Dropbox/Apps/Overleaf/Uncertainty and Financial Advise (1)"
fig_dir      <- file.path(overleaf_dir, "figures/invbeh")
tab_dir      <- file.path(overleaf_dir, "tables/invbeh")
out_dir      <- "../../output/invbeh"
for (d in c(fig_dir, tab_dir, out_dir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

WIN    <- as.Date(c("2019-01-01", "2021-04-01"))
T0     <- as.Date("2020-01-31")
MIN_PF <- 5000
MIN_D  <- 1000     # a month's share contribution needs a denominator >= CHF 1,000
PERIODS <- list(`Pre-crisis (2019)`          = as.Date(c("2019-01-01", "2019-12-31")),
                `Drawdown (Feb-Apr 2020)`    = as.Date(c("2020-02-01", "2020-04-30")),
                `Recovery (May 2020-Mar 2021)` = as.Date(c("2020-05-01", "2021-03-31")))

# ---- sample (as in 09) ---------------------------------------------------------
pos <- setDT(read_parquet("../../data/pos_pf.parquet",
                          col_select = c("Bp_ID", "MDate", "equity", "dq_equity", "tot_pf",
                                         "dq_tot_pf", "tot_wealth")))
pos <- pos[MDate %between% WIN]
bp  <- pos[MDate == T0 & tot_pf > MIN_PF, Bp_ID]
pos <- pos[Bp_ID %in% bp]
cat(sprintf("sample: %s clients with portfolio > CHF %s in Jan 2020\n",
            format(length(bp), big.mark = "'"), MIN_PF))

# ---- shared figure style (copied from 09) --------------------------------------
col_blue <- "#2a78d6"; col_orange <- "#eb6834"; col_aqua <- "#1baf7a"; col_ink <- "grey25"
crash_start <- as.Date("2020-02-01"); crash_end <- as.Date("2020-03-31")
x_lims <- c(as.Date("2019-01-31"), as.Date("2021-03-31"))
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
crash_band <- function() annotate("rect", xmin = crash_start, xmax = crash_end,
                                  ymin = -Inf, ymax = Inf, fill = "grey88", alpha = 0.8)
crash_label <- function(facet_var = NULL, facet_lv = NULL) {
  d <- data.frame(MDate = crash_start, y = Inf, lab = "COVID-19 crash")
  if (!is.null(facet_var)) d[[facet_var]] <- factor(facet_lv[1], levels = facet_lv)
  geom_text(data = d, aes(x = MDate, y = y, label = lab), inherit.aes = FALSE,
            hjust = 1.05, vjust = 1.6, size = 2.6, colour = "grey40")
}
x_scale <- scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y",
                        limits = x_lims + c(-20, 20), expand = expansion(mult = 0.01))
zero_line <- geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.35)
save_fig <- function(p, name, w = 6.5, h = 4)
  ggsave(file.path(fig_dir, paste0(name, ".pdf")), p, width = w, height = h, device = cairo_pdf)

tex_esc <- function(x) gsub("%", "\\%", x, fixed = TRUE)
tex_tab <- function(body, head, align, file, note = NULL) {
  lines <- c(sprintf("\\begin{tabular}{%s}", align), "\\toprule", head, "\\midrule", body,
             "\\bottomrule", "\\end{tabular}")
  if (!is.null(note))
    lines <- c(lines, sprintf("\\par\\smallskip\\parbox{\\linewidth}{\\footnotesize %s}", note))
  writeLines(lines, file)
}
fmt_int <- function(x) formatC(round(x), big.mark = "'", format = "d")

# =============================================================================
# (a) EQUITY BUYS: TOP-UP vs RE-ENTRY vs NEW ASSET
# =============================================================================
con <- dbConnect(duckdb(), config = list(memory_limit = "8GB"))
duckdb_register(con, "smp", data.frame(Bp_ID = bp))

EQ_SQL <- "(Instrumentengruppe = 'Aktien'
            OR (Instrumentengruppe = 'Fonds' AND Fondsart IN
                ('Fund - Shares (09)','Fund - Exchange Traded (03)','Fund - Index (04)')))"

# equity trades per client x asset x month (buys and sells; sells only validate the matching).
# Month = month of the trade date DDate, NOT trades.MDate: MDate is often the following
# month (a sale on 29 Jan is stamped February), while month-end positions already
# reflect the trade -- matching on MDate made 15% of sell volume look 'not held'.
invisible(dbExecute(con, sprintf("
  CREATE TEMP TABLE tr AS
  SELECT t.Bp_ID, t.Asset_ID, last_day(COALESCE(t.DDate, t.MDate)) AS MDate,
         SUM(CASE WHEN t.buy  = 1 THEN ABS(t.buy_chf)  ELSE 0 END) AS buy_chf,
         SUM(CASE WHEN t.sell = 1 THEN ABS(t.sell_chf) ELSE 0 END) AS sell_chf,
         SUM(t.buy) AS n_buy, SUM(t.sell) AS n_sell
  FROM read_parquet('../../data/trades.parquet') t
  JOIN smp s ON s.Bp_ID = t.Bp_ID
  WHERE %s AND COALESCE(t.DDate, t.MDate) BETWEEN DATE '%s' AND DATE '%s'
  GROUP BY 1, 2, 3", EQ_SQL, WIN[1], x_lims[2])))

# month-end holdings (any asset class: a fund bought after holding it as another class
# is still 'known'), from 12 months before the window
invisible(dbExecute(con, sprintf("
  CREATE TEMP TABLE hold AS
  SELECT DISTINCT p.Bp_ID, p.Asset_ID, last_day(p.MDate) AS MDate
  FROM read_parquet('../../data/pos_m1.parquet') p
  JOIN smp s ON s.Bp_ID = p.Bp_ID
  WHERE p.Menge > 0 AND p.MDate BETWEEN DATE '%s' AND DATE '%s'",
  seq(WIN[1], by = "-12 months", length.out = 2)[2], WIN[2])))

tr <- setDT(dbGetQuery(con, "
  SELECT t.*,
         EXISTS (SELECT 1 FROM hold h WHERE h.Bp_ID = t.Bp_ID AND h.Asset_ID = t.Asset_ID
                   AND h.MDate = last_day(t.MDate - INTERVAL 1 MONTH))               AS held_prev,
         EXISTS (SELECT 1 FROM hold h WHERE h.Bp_ID = t.Bp_ID AND h.Asset_ID = t.Asset_ID
                   AND h.MDate <  t.MDate
                   AND h.MDate >= last_day(t.MDate - INTERVAL 12 MONTH))             AS held_12m
  FROM tr t"))
dbDisconnect(con, shutdown = TRUE)
tr[, MDate := as.Date(MDate)]

buy_lv <- c("Top-up (held last month)", "Re-entry (held in past 12 months)",
            "New asset (not held in past 12 months)")
tr[, cat := factor(fcase(held_prev, buy_lv[1], held_12m, buy_lv[2], default = buy_lv[3]),
                   levels = buy_lv)]

# a sell must be of something held last month-end or bought within the same month
chk_sell <- tr[n_sell > 0, .(held_prev = weighted.mean(held_prev | n_buy > 0, sell_chf), N = .N)]
cat(sprintf("matching check: %.1f%% of equity sell volume is in assets held the month before or bought that month (N = %s)\n",
            100 * chk_sell$held_prev, fmt_int(chk_sell$N)))

buys <- tr[n_buy > 0]
buy_m <- buys[, .(chf = sum(buy_chf), n = sum(n_buy), clients = uniqueN(Bp_ID)), by = .(MDate, cat)]
buy_m <- buy_m[CJ(MDate = unique(buy_m$MDate), cat = factor(buy_lv, levels = buy_lv)),
               on = .(MDate, cat)][is.na(chf), `:=`(chf = 0, n = 0, clients = 0)]
buy_m[, share := chf / sum(chf), by = MDate]

# period table: share of equity-buy CHF by category, and clients by what they bought
per_tab <- rbindlist(lapply(names(PERIODS), function(pn) {
  b  <- buys[MDate %between% PERIODS[[pn]]]
  sh <- b[, .(chf = sum(buy_chf)), by = cat][, share := chf / sum(chf)]
  cl <- b[, .(topup = any(cat == buy_lv[1]), other = any(cat != buy_lv[1])), by = Bp_ID]
  data.table(period = pn,
             chf_m = sum(b$buy_chf) / 1e6 / uniqueN(b$MDate),
             sh_topup = sh[cat == buy_lv[1], sum(share)],
             sh_reentry = sh[cat == buy_lv[2], sum(share)],
             sh_new = sh[cat == buy_lv[3], sum(share)],
             n_clients = nrow(cl),
             cl_only_topup = mean(cl$topup & !cl$other),
             cl_only_new = mean(!cl$topup & cl$other),
             cl_both = mean(cl$topup & cl$other))
}))
print(per_tab)
fwrite(per_tab, file.path(out_dir, "ib_eqbuy_existing_vs_new.csv"))

pct <- function(x) sprintf("%.1f", 100 * x)
body <- per_tab[, sprintf("%s & %.1f & %s & %s & %s & %s & %s & %s & %s \\\\", period, chf_m,
                          pct(sh_topup), pct(sh_reentry), pct(sh_new), fmt_int(n_clients),
                          pct(cl_only_topup), pct(cl_only_new), pct(cl_both))]
tex_tab(body,
        paste("& CHF m & \\multicolumn{3}{c}{Share of equity-buy volume (\\%)} & & \\multicolumn{3}{c}{Buying clients (\\%)} \\\\",
              "\\cmidrule(lr){3-5} \\cmidrule(lr){7-9}",
              "& per month & Top-up & Re-entry & New & Clients & Only top-up & Only new/re-entry & Both \\\\"),
        "lrrrrrrrr", file.path(tab_dir, "ib_eqbuy_existing_vs_new.tex"),
        note = paste("Equity buys of clients with a portfolio above CHF 5,000 in January 2020.",
                     "Top-up: asset held at the previous month-end; re-entry: held at a month-end",
                     "in the prior 12 months but not the previous one; new: neither.",
                     "Months follow the trade date.",
                     sprintf("Matching check: %.1f\\%% of equity sell volume is in assets held the month before or bought in the same month.",
                             100 * chk_sell$held_prev)))

# figure: CHF by category (stacked) and share of volume by category
cat_cols <- setNames(c(col_blue, col_aqua, col_orange), buy_lv)
eb_lv <- c("Equity purchases (CHF million)", "Share of equity-purchase volume (%)")
eb <- rbind(buy_m[, .(MDate, cat, panel = eb_lv[1], value = chf / 1e6)],
            buy_m[, .(MDate, cat, panel = eb_lv[2], value = 100 * share)])
eb[, panel := factor(panel, levels = eb_lv)]
eb[, cat := factor(cat, levels = rev(buy_lv))]   # top-up at the bottom of the stack
p_eb <- ggplot(eb, aes(MDate, value, fill = cat)) +
  crash_band() + crash_label("panel", eb_lv) +
  geom_col(width = 22) +
  scale_fill_manual(values = cat_cols, breaks = buy_lv, name = NULL) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  x_scale +
  scale_y_continuous(labels = scales::label_number(big.mark = "'"),
                     expand = expansion(mult = c(0, 0.05))) +
  guides(fill = guide_legend(ncol = 1)) +
  labs(x = NULL, y = NULL,
       caption = paste("Equity buys matched on client x asset to month-end positions.",
                       "Sample: portfolio > CHF 5,000 in Jan 2020.")) +
  theme_ib
save_fig(p_eb, "ib_eqbuy_existing_vs_new", h = 5.5)
p_eb

# =============================================================================
# (b) EQUITY SHARE: PASSIVE (PRICE) vs ACTIVE (TRADING)
# =============================================================================
setorder(pos, Bp_ID, MDate)
pos[, `:=`(E_l = shift(equity), P_l = shift(tot_pf), W_l = shift(tot_wealth)), by = Bp_ID]
pos[, `:=`(E_p = equity - dq_equity,                     # frozen quantities, month-t prices
           P_p = tot_pf - dq_tot_pf)]
pos[, W_p := W_l + (P_p - P_l)]                           # only price/FX changes wealth

decomp <- function(E, D, E_l, D_l, E_p, D_p) {
  w_l <- E_l / D_l; w <- E / D; w_p <- E_p / D_p
  list(passive = w_p - w_l, active = w - w_p)
}
pos[P_l >= MIN_D & P_p >= MIN_D & tot_pf >= MIN_D,
    c("pf_passive", "pf_active") := decomp(equity, tot_pf, E_l, P_l, E_p, P_p)]
pos[W_l >= MIN_D & W_p >= MIN_D & tot_wealth >= MIN_D,
    c("w_passive", "w_active") := decomp(equity, tot_wealth, E_l, W_l, E_p, W_p)]
cc <- c("pf_passive", "pf_active", "w_passive", "w_active")
pos[, (cc) := lapply(.SD, winsor), .SDcols = cc]
cat(sprintf("share contributions: %.1f%% of client-months usable (portfolio), %.1f%% (wealth)\n",
            100 * mean(!is.na(pos[!is.na(P_l)]$pf_active)),
            100 * mean(!is.na(pos[!is.na(W_l)]$w_active))))

# equal-weighted: mean monthly contributions; value-weighted: identity on sums
ew <- pos[, .(pf_passive = mean(pf_passive, na.rm = TRUE), pf_active = mean(pf_active, na.rm = TRUE),
              w_passive  = mean(w_passive,  na.rm = TRUE), w_active  = mean(w_active,  na.rm = TRUE)),
          by = MDate]
vw <- pos[!is.na(P_l), .(E = sum(equity), P = sum(tot_pf), W = sum(tot_wealth),
                         E_l = sum(E_l), P_l = sum(P_l), W_l = sum(W_l),
                         E_p = sum(E_p), P_p = sum(P_p), W_p = sum(W_p)), by = MDate]
vw[, c("pf_passive", "pf_active") := decomp(E, P, E_l, P_l, E_p, P_p)]
vw[, c("w_passive",  "w_active")  := decomp(E, W, E_l, W_l, E_p, W_p)]

# cumulate from Jan 2020: after T0 sum forward, before T0 sum backward with a minus
cum_T0 <- function(d, x) {
  setorder(d, MDate)
  v <- d[[x]]; v[is.na(v)] <- 0
  fwd <- cumsum(ifelse(d$MDate > T0, v, 0))
  bwd <- -rev(cumsum(rev(ifelse(d$MDate > T0, 0, v)))) + ifelse(d$MDate > T0, 0, v)
  ifelse(d$MDate > T0, fwd, bwd)
}
comp_lv <- c("Total change", "Passive (price drift)", "Active (trading, flows)")
sh_lv   <- c("Equity / portfolio", "Equity / wealth")
wt_lv   <- c("Equal-weighted", "Value-weighted")
dd <- rbindlist(lapply(list(ew, vw), function(d) {
  d <- d[MDate > min(MDate)]                     # first month has no lag
  out <- d[, .(MDate)]
  for (x in cc) out[[x]] <- 100 * cum_T0(d, x)
  out
}), idcol = "wt")
dd[, wt := wt_lv[wt]]
dd <- rbind(dd[, .(MDate, wt, share = sh_lv[1], passive = pf_passive, active = pf_active)],
            dd[, .(MDate, wt, share = sh_lv[2], passive = w_passive,  active = w_active)])
dd[, total := passive + active]
dd <- melt(dd, id.vars = c("MDate", "wt", "share"), variable.name = "comp")
dd[, comp := factor(comp, levels = c("total", "passive", "active"), labels = comp_lv)]
dd[, panel := factor(paste(share, "-", wt), levels = c(outer(sh_lv, wt_lv, paste, sep = " - ")))]
# the series start at the first month with a lag (Feb 2019); anchor Jan 2020 = 0 is exact

comp_cols <- setNames(c(col_ink, col_orange, col_blue), comp_lv)
p_as <- ggplot(dd, aes(MDate, value, colour = comp, linewidth = comp)) +
  crash_band() + crash_label("panel", levels(dd$panel)) + zero_line +
  geom_line() +
  scale_colour_manual(values = comp_cols, name = NULL) +
  scale_linewidth_manual(values = c(0.9, 0.6, 0.6), guide = "none") +
  facet_wrap(~panel, ncol = 2, scales = "free_y") +
  x_scale +
  scale_y_continuous(labels = function(x) sub("^[+-]0\\.0$", "0", sprintf("%+.1f", x))) +
  labs(x = NULL, y = "Cumulative change since Jan 2020 (pp)",
       caption = paste("Passive: change in the equity share if last month's quantities were held (price and FX drift).",
                       "Active: the rest (trades; for the wealth share also deposits/withdrawals).",
                       "Equal-weighted: mean of client contributions, winsorized at 0.1%/99.9%.",
                       "Value-weighted: same identity on aggregate holdings.", sep = "\n")) +
  theme_ib
save_fig(p_as, "ib_eqshare_active_passive", h = 5.5)
p_as

# table: cumulative components since Jan 2020 at selected month-ends
tab_dates <- as.Date(c("2020-03-31", "2020-06-30", "2020-12-31", "2021-03-31"))
tb <- dcast(dd[MDate %in% tab_dates], share + wt + comp ~ MDate, value.var = "value")
setnames(tb, as.character(tab_dates), format(tab_dates, "%b %Y"))
print(tb)
fwrite(tb, file.path(out_dir, "ib_eqshare_active_passive.csv"))
body <- unlist(lapply(levels(dd$panel), function(pn) {
  s <- sub(" - .*", "", pn); w <- sub(".* - ", "", pn)
  d <- tb[share == s & wt == w]
  c(sprintf("\\multicolumn{5}{l}{\\textit{%s, %s}} \\\\", s, tolower(w)),
    sprintf("\\quad %s & %s \\\\", as.character(d$comp),
            apply(d[, format(tab_dates, "%b %Y"), with = FALSE], 1,
                  function(r) paste(sprintf("%+.2f", as.numeric(r)), collapse = " & "))))
}))
tex_tab(body, paste("&", paste(format(tab_dates, "%b %Y"), collapse = " & "), "\\\\"),
        "lrrrr", file.path(tab_dir, "ib_eqshare_active_passive.tex"),
        note = tex_esc(paste("Cumulative change in the equity share since January 2020, in percentage points.",
                             "Passive = price and FX drift of last month's holdings; active = trading",
                             "(and, for the wealth share, deposits and withdrawals). Total = passive + active.")))
