# =============================================================================
# 09g_savings_plans_trend.R -- how much of the active equity-share increase in the
# COVID crash is regular savings-plan money or the client's normal pace, and how
# much is above / below trend?
#
# Savings plans (Sparplaene). The data has no plan flag, but automated fund
# subscriptions are visible: Order_Type 'Zeichnung' entered via Medium 'Diverses'.
# Pre-crisis, client x asset pairs bought in 10-12 of 12 months run 96-99% through
# that channel (median CHF 250-500 a month), one-off buys only 8%. Definition:
#   plan pair  client x asset with Zeichnung/Diverses buys in >= 6 distinct months,
#              Jan 2019 - Mar 2021
#   plan buy   every Zeichnung/Diverses buy of a plan pair; all else is discretionary
# Plans started or stopped during the crisis are therefore still 'plans'; the number
# of clients with a plan buy per month shows whether plans were paused.
#
# Active equity-share change (as in 09e/09f, active_t = w_t - w^p_t) is split into
#   plan part     = effect of adding only the month's plan buys to the passive portfolio
#                   portfolio share: (E^p + B_eq)/(P^p + B_all) - E^p/P^p
#                   wealth share:    B_eq / W^p   (plans are paid from cash)
#   discretionary = active - plan part  (all other trades; for wealth also deposits)
# and the discretionary part further into
#   trend         client's mean monthly discretionary contribution, Feb 2019 - Jan 2020
#   above/below   discretionary - trend
# Months follow the trade date (see 09e). Sample as in 09: portfolio > CHF 5,000 in
# Jan 2020. Monthly contributions winsorized at 0.1%/99.9%.
# Run from R/estimation_new; writes only new files (ib_plan_*, ib_eqshare_trend_*).
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(duckdb)
  library(fixest)
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

WIN     <- as.Date(c("2019-01-01", "2021-03-31"))
T0      <- as.Date("2020-01-31")
PRE     <- as.Date(c("2019-02-01", "2020-01-31"))   # trend window (first month with a lag)
DRAW    <- as.Date(c("2020-02-01", "2020-04-30"))
DEC     <- as.Date(c("2020-02-01", "2020-12-31"))
MIN_PF  <- 5000
MIN_D   <- 1000
PLAN_K  <- 6       # distinct months with automated subscriptions to call a pair a plan

# ---- sample ------------------------------------------------------------------------
pos <- setDT(read_parquet("../../data/pos_pf.parquet",
                          col_select = c("Bp_ID", "MDate", "advisor_id", "sex", "birth_year",
                                         "anlegerprofil", "anlagepaket", "equity", "dq_equity",
                                         "tot_pf", "dq_tot_pf", "tot_wealth")))
pos <- pos[MDate %between% WIN]
bp  <- pos[MDate == T0 & tot_pf > MIN_PF, Bp_ID]
pos <- pos[Bp_ID %in% bp]
setorder(pos, Bp_ID, MDate)

# ---- savings-plan buys per client x month ------------------------------------------
con <- dbConnect(duckdb(), config = list(memory_limit = "8GB"))
duckdb_register(con, "smp", data.frame(Bp_ID = bp))
invisible(dbExecute(con, sprintf("
  CREATE TEMP TABLE zd AS
  SELECT t.Bp_ID, t.Asset_ID, last_day(COALESCE(t.DDate, t.MDate)) AS MDate, ABS(t.buy_chf) AS chf,
         (t.Instrumentengruppe = 'Aktien'
          OR (t.Instrumentengruppe = 'Fonds' AND t.Fondsart IN
              ('Fund - Shares (09)','Fund - Exchange Traded (03)','Fund - Index (04)'))) AS is_eq
  FROM read_parquet('../../data/trades.parquet') t
  JOIN smp s ON s.Bp_ID = t.Bp_ID
  WHERE t.buy = 1 AND t.Order_Type = 'Zeichnung' AND t.Medium = 'Diverses'
    AND COALESCE(t.DDate, t.MDate) BETWEEN DATE '%s' AND DATE '%s'", WIN[1], WIN[2])))
plan <- setDT(dbGetQuery(con, sprintf("
  WITH pp AS (SELECT Bp_ID, Asset_ID FROM zd GROUP BY 1, 2 HAVING COUNT(DISTINCT MDate) >= %d)
  SELECT z.Bp_ID, z.MDate, SUM(z.chf) AS B_all,
         SUM(CASE WHEN COALESCE(z.is_eq, FALSE) THEN z.chf ELSE 0 END) AS B_eq,
         COUNT(DISTINCT z.Asset_ID) AS n_plans
  FROM zd z JOIN pp USING (Bp_ID, Asset_ID)
  GROUP BY 1, 2", PLAN_K)))
dbDisconnect(con, shutdown = TRUE)
plan[, MDate := as.Date(MDate)]
cat(sprintf("savings plans: %s clients with a plan (%.1f%% of sample); plan buys CHF %.1fm, of which equity %.0f%%\n",
            format(uniqueN(plan$Bp_ID), big.mark = "'"), 100 * uniqueN(plan$Bp_ID) / length(bp),
            sum(plan$B_all) / 1e6, 100 * sum(plan$B_eq) / sum(plan$B_all)))

pos <- merge(pos, plan, by = c("Bp_ID", "MDate"), all.x = TRUE)
pos[is.na(B_all), `:=`(B_all = 0, B_eq = 0, n_plans = 0L)]
setorder(pos, Bp_ID, MDate)

# ---- active contribution and its plan / discretionary split ------------------------
pos[, `:=`(E_l = shift(equity), P_l = shift(tot_pf), W_l = shift(tot_wealth)), by = Bp_ID]
pos[, `:=`(E_p = equity - dq_equity, P_p = tot_pf - dq_tot_pf)]
pos[, W_p := W_l + (P_p - P_l)]
ok_pf <- quote(P_l >= MIN_D & P_p >= MIN_D & tot_pf >= MIN_D)
ok_w  <- quote(W_l >= MIN_D & W_p >= MIN_D & tot_wealth >= MIN_D)
pos[eval(ok_pf), `:=`(pf_active = equity / tot_pf - E_p / P_p,
                      pf_plan   = (E_p + B_eq) / (P_p + B_all) - E_p / P_p)]
pos[eval(ok_w),  `:=`(w_active  = equity / tot_wealth - E_p / W_p,
                      w_plan    = B_eq / W_p)]
cc <- c("pf_active", "pf_plan", "w_active", "w_plan")
pos[, (cc) := lapply(.SD, winsor), .SDcols = cc]
pos[, `:=`(pf_disc = pf_active - pf_plan, w_disc = w_active - w_plan)]

# client trend: mean monthly discretionary contribution over the pre-crisis year
tr <- pos[MDate %between% PRE, .(pf_trend = mean(pf_disc, na.rm = TRUE), w_trend = mean(w_disc, na.rm = TRUE),
                                 n_pre = sum(!is.na(pf_disc))), by = Bp_ID]
tr[!is.finite(pf_trend), pf_trend := 0]
tr[!is.finite(w_trend),  w_trend  := 0]
cat(sprintf("trend: %.1f%% of clients have >= 9 usable pre-crisis months\n", 100 * mean(tr$n_pre >= 9)))
pos <- merge(pos, tr[, .(Bp_ID, pf_trend, w_trend)], by = "Bp_ID", all.x = TRUE)
pos[!is.na(pf_disc), pf_excess := pf_disc - pf_trend]
pos[!is.na(w_disc),  w_excess  := w_disc  - w_trend]
pos[is.na(pf_disc), pf_trend := NA_real_]      # trend only counts where the month is usable
pos[is.na(w_disc),  w_trend  := NA_real_]

# ---- shared style (as in 09) -------------------------------------------------------
col_blue <- "#2a78d6"; col_orange <- "#eb6834"; col_aqua <- "#1baf7a"; col_ink <- "grey25"
crash_start <- as.Date("2020-02-01"); crash_end <- as.Date("2020-03-31")
theme_ib <- theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
        axis.ticks.x = element_line(colour = "grey60", linewidth = 0.3),
        axis.text = element_text(colour = "grey35"), axis.title = element_text(colour = "grey25"),
        strip.text = element_text(face = "bold", hjust = 0, colour = "grey15"),
        legend.position = "top", legend.justification = "left", legend.margin = margin(0, 0, 0, 0),
        legend.key.width = unit(1.4, "lines"), panel.spacing = unit(1.1, "lines"),
        plot.caption = element_text(colour = "grey45", size = 7.5, hjust = 0),
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
                        limits = c(as.Date("2019-01-10"), as.Date("2021-04-20")),
                        expand = expansion(mult = 0.01))
zero_line <- geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.35)
pp_lab <- function(x) sub("^[+-]0(\\.0)?$", "0", sprintf("%+.1f", x))
save_fig <- function(p, name, w = 6.5, h = 4)
  ggsave(file.path(fig_dir, paste0(name, ".pdf")), p, width = w, height = h, device = cairo_pdf)

# =============================================================================
# FIGURE 1: plan vs discretionary equity flows, against the pre-crisis average
# =============================================================================
fl_lv <- c("Savings-plan purchases, equity (CHF million)",
           "Discretionary net equity flows (CHF million)",
           "Clients with a savings-plan purchase")
fl <- pos[MDate > WIN[1], .(plan_eq = sum(B_eq) / 1e6, disc = sum(dq_equity - B_eq, na.rm = TRUE) / 1e6,
                            n_cl = as.numeric(sum(B_all > 0))), by = MDate]
fl <- melt(fl, id.vars = "MDate", variable.name = "panel")
fl[, panel := factor(panel, levels = c("plan_eq", "disc", "n_cl"), labels = fl_lv)]
fl_tr <- fl[MDate %between% PRE, .(trend = mean(value)), by = panel]
fl <- merge(fl, fl_tr, by = "panel")
p_fl <- ggplot(fl, aes(MDate, value)) +
  crash_band() + zero_line +            # no in-plot label: the Feb-2020 bar fills the top
  geom_col(fill = col_blue, width = 22) +
  geom_segment(data = fl_tr, aes(x = PRE[1], xend = WIN[2], y = trend, yend = trend),
               inherit.aes = FALSE, colour = col_orange, linewidth = 0.6, linetype = "22") +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  x_scale +
  scale_y_continuous(labels = scales::label_number(big.mark = "'")) +
  labs(x = NULL, y = NULL,
       caption = paste0("Shaded: COVID-19 crash (Feb-Mar 2020). Dashed: pre-crisis monthly average (Feb 2019 - Jan 2020).\n",
                        "Savings plan: client x asset with automated fund subscriptions in >= 6 months.\n",
                        "Discretionary net equity flows: quantity change of equity holdings minus savings-plan equity purchases.\n",
                        "Sample: portfolio > CHF 5,000 in Jan 2020.")) +
  theme_ib
save_fig(p_fl, "ib_plan_vs_discretionary_flows", h = 6)
p_fl

# =============================================================================
# FIGURE 2: cumulative active change = plans + trend + above/below trend
# =============================================================================
comp_lv <- c("Active change (total)", "Savings plans", "Discretionary: pre-crisis trend",
             "Discretionary: above / below trend")
cum_from_T0 <- function(v, m) { v[is.na(v)] <- 0; cumsum(ifelse(m > T0, v, 0)) }
ag <- pos[MDate > WIN[1], .(
  pf_active = mean(pf_active, na.rm = TRUE), pf_plan = mean(pf_plan, na.rm = TRUE),
  pf_trend  = mean(pf_trend,  na.rm = TRUE), pf_excess = mean(pf_excess, na.rm = TRUE),
  w_active  = mean(w_active,  na.rm = TRUE), w_plan  = mean(w_plan,  na.rm = TRUE),
  w_trend   = mean(w_trend,   na.rm = TRUE), w_excess  = mean(w_excess,  na.rm = TRUE)), by = MDate]
setorder(ag, MDate)
ag <- ag[MDate >= T0]
vv <- setdiff(names(ag), "MDate")
ag[, (vv) := lapply(.SD, function(v) 100 * cum_from_T0(v, MDate)), .SDcols = vv]
sh_lv <- c("Equity / portfolio", "Equity / wealth")
ad <- rbind(ag[, .(MDate, share = sh_lv[1], a = pf_active, p = pf_plan, t = pf_trend, e = pf_excess)],
            ag[, .(MDate, share = sh_lv[2], a = w_active,  p = w_plan,  t = w_trend,  e = w_excess)])
ad <- melt(ad, id.vars = c("MDate", "share"), variable.name = "comp")
ad[, comp := factor(comp, levels = c("a", "p", "t", "e"), labels = comp_lv)]
ad[, share := factor(share, levels = sh_lv)]

p_ad <- ggplot(ad, aes(MDate, value, colour = comp, linetype = comp, linewidth = comp)) +
  crash_band() + zero_line +            # axis starts at Jan 2020, so the label would be clipped
  geom_line() +
  scale_colour_manual(values = setNames(c(col_ink, col_aqua, col_orange, col_blue), comp_lv), name = NULL) +
  scale_linetype_manual(values = setNames(c("solid", "solid", "22", "solid"), comp_lv), name = NULL) +
  scale_linewidth_manual(values = setNames(c(0.9, 0.6, 0.6, 0.8), comp_lv), name = NULL) +
  facet_wrap(~share, ncol = 2, scales = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y", expand = expansion(mult = 0.02)) +
  scale_y_continuous(labels = pp_lab) +
  guides(colour = guide_legend(nrow = 2)) +
  labs(x = NULL, y = "Cumulative active change since Jan 2020 (pp)",
       caption = paste0("Shaded: COVID-19 crash (Feb-Mar 2020). Equal-weighted means of client contributions.\n",
                        "Active = trading net of price drift (wealth share: also deposits) = plans + trend + above/below trend.\n",
                        "Trend: each client's average monthly discretionary contribution, Feb 2019 - Jan 2020, continued.")) +
  theme_ib
save_fig(p_ad, "ib_eqshare_trend_decomp", h = 4.2)
p_ad

# =============================================================================
# FIGURE 3 + TABLE: above/below-trend active change by client group
# =============================================================================
cl <- pos[MDate == T0, .(Bp_ID, advisor_id, sex, birth_year, anlegerprofil, anlagepaket,
                         eq_sh0 = equity / tot_pf, tot_wealth)]
cum <- pos[MDate > T0, .(
  pf_excess_draw = sum(pf_excess[MDate %between% DRAW], na.rm = TRUE),
  pf_excess_dec  = sum(pf_excess[MDate %between% DEC],  na.rm = TRUE),
  pf_plan_draw   = sum(pf_plan[MDate %between% DRAW],   na.rm = TRUE),
  pf_trend_draw  = sum(pf_trend[MDate %between% DRAW],  na.rm = TRUE),
  w_excess_draw  = sum(w_excess[MDate %between% DRAW],  na.rm = TRUE),
  w_excess_dec   = sum(w_excess[MDate %between% DEC],   na.rm = TRUE)), by = Bp_ID]
cl <- merge(cl, cum, by = "Bp_ID")
cl[is.na(advisor_id), advisor_id := -Bp_ID]
cl[, age := 2020 - birth_year]
cl[, Sex := fcase(sex == "m", "Male", sex == "w", "Female", default = "Sex unknown")]
cl[, Age := fcase(age %between% c(18, 44), "Under 45", age %between% c(45, 64), "45-64",
                  age %between% c(65, 100), "65+", default = "Age unknown")]
cl[, `Risk type` := fcase(grepl("^Vorsicht", anlegerprofil), "Cautious",
                          grepl("^Ausgegl",  anlegerprofil), "Balanced",
                          grepl("^Risiko",   anlegerprofil), "Risk-seeking", default = "No profile")]
cl[, `Equity share` := fcase(eq_sh0 < 1/3, "Equity < 33%", eq_sh0 <= 2/3, "Equity 33-67%",
                             default = "Equity > 67%")]
cl[, Wealth := paste0("Wealth Q", cut(tot_wealth, quantile(tot_wealth, 0:4/4), include.lowest = TRUE,
                                      labels = FALSE))]
cl[, Advice := fcase(grepl("^CONSULT", anlagepaket), "CONSULT (advisory)",
                     anlagepaket == "DIRECT", "DIRECT (execution only)", default = "Package unknown")]
cl[, Plan := fifelse(Bp_ID %in% plan$Bp_ID, "Has a savings plan", "No savings plan")]
grp_lv <- list(Sex = c("Male", "Female", "Sex unknown"),
               Age = c("Under 45", "45-64", "65+", "Age unknown"),
               `Risk type` = c("Cautious", "Balanced", "Risk-seeking", "No profile"),
               `Equity share` = c("Equity < 33%", "Equity 33-67%", "Equity > 67%"),
               Wealth = paste0("Wealth Q", 1:4),
               Advice = c("DIRECT (execution only)", "CONSULT (advisory)", "Package unknown"),
               Plan = c("No savings plan", "Has a savings plan"))
grp_vars <- names(grp_lv)

OUTC <- c(pf_excess_draw = "Above/below trend,\nFeb-Apr 2020 (pp)",
          pf_excess_dec  = "Above/below trend,\nFeb-Dec 2020 (pp)")
TAB_OUT <- c("pf_plan_draw", "pf_trend_draw", "pf_excess_draw", "pf_excess_dec", "w_excess_draw", "w_excess_dec")
res <- rbindlist(lapply(grp_vars, function(g) {
  d <- copy(cl); d[, grp := factor(get(g), levels = grp_lv[[g]])]
  rbindlist(lapply(TAB_OUT, function(y) {
    ct <- coeftable(feols(as.formula(sprintf("%s ~ 0 + grp", y)), d, cluster = ~advisor_id))
    data.table(char = g, group = sub("^grp", "", rownames(ct)), outcome = y,
               mean = 100 * ct[, 1], se = 100 * ct[, 2])
  }))[, N := as.integer(table(d$grp)[group])]
}))
fwrite(res, file.path(out_dir, "ib_eqshare_trend_by_group.csv"))

n_lab <- unique(res[, .(group, N)])
lab_n <- setNames(sprintf("%s   N = %s", n_lab$group, formatC(n_lab$N, big.mark = "'", format = "d")),
                  n_lab$group)
pm <- res[outcome %in% names(OUTC)]
pm[, `:=`(char = factor(char, levels = grp_vars), measure = factor(OUTC[outcome], levels = OUTC),
          group = factor(group, levels = rev(unlist(grp_lv))),
          sig = fifelse(abs(mean) > 1.96 * se, "Different from 0 (95%)", "Not significant"))]
p_g <- ggplot(pm, aes(mean, group)) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_linerange(aes(xmin = mean - 1.96 * se, xmax = mean + 1.96 * se, colour = sig), linewidth = 0.5) +
  geom_point(aes(colour = sig), size = 1.8) +
  scale_colour_manual(values = c("Different from 0 (95%)" = col_blue, "Not significant" = "grey65"), name = NULL) +
  facet_grid(char ~ measure, scales = "free_y", space = "free_y", switch = "y") +
  scale_y_discrete(labels = lab_n) +
  scale_x_continuous(labels = pp_lab) +
  labs(x = NULL, y = NULL,
       caption = paste("Discretionary active change in the equity share of the portfolio minus the client's pre-crisis pace",
                       "(Feb 2019 - Jan 2020),\ncumulated; savings plans excluded. > 0: more risk-taking than usual;",
                       "< 0: less. 95% CI clustered by advisor.")) +
  theme_ib +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3),
        strip.placement = "outside", panel.spacing.x = unit(1.5, "lines"),
        strip.text.y.left = element_text(angle = 0, hjust = 1, vjust = 1, face = "bold"))
save_fig(p_g, "ib_eqshare_trend_by_group", w = 7.5, h = 7.5)
p_g

tex_esc <- function(x) gsub(">", "$>$", gsub("<", "$<$", gsub("%", "\\%", x, fixed = TRUE), fixed = TRUE), fixed = TRUE)
star <- function(m, s) { z <- abs(m / s); fcase(z > 2.576, "$^{***}$", z > 1.96, "$^{**}$", z > 1.645, "$^{*}$", default = "") }
res[, cell := paste0(sprintf("%.2f", mean), star(mean, se))]
tw <- dcast(res, char + group + N ~ outcome, value.var = "cell")
body <- unlist(lapply(grp_vars, function(g) {
  d <- tw[char == g][match(grp_lv[[g]], group)]
  c(sprintf("\\multicolumn{8}{l}{\\textit{%s}} \\\\", g),
    d[, sprintf("\\quad %s & %s & %s & %s & %s & %s & %s & %s \\\\", tex_esc(group), pf_plan_draw,
                pf_trend_draw, pf_excess_draw, pf_excess_dec, w_excess_draw, w_excess_dec,
                formatC(N, big.mark = "'", format = "d"))])
}))
writeLines(c("\\begin{tabular}{lrrrrrrr}", "\\toprule",
             "& \\multicolumn{4}{c}{Equity / portfolio} & \\multicolumn{2}{c}{Equity / wealth} & \\\\",
             "\\cmidrule(lr){2-5} \\cmidrule(lr){6-7}",
             "& Plans & Trend & \\multicolumn{2}{c}{Above/below trend} & \\multicolumn{2}{c}{Above/below trend} & \\\\",
             "& Feb-Apr & Feb-Apr & Feb-Apr & Feb-Dec & Feb-Apr & Feb-Dec & N \\\\",
             "\\midrule", body, "\\bottomrule", "\\end{tabular}",
             paste0("\\par\\smallskip\\parbox{\\linewidth}{\\footnotesize Cumulative active change in the equity share (pp),",
                    " split into savings plans, the client's pre-crisis trend (mean monthly discretionary contribution,",
                    " Feb 2019 - Jan 2020) and the deviation from it. Stars: different from zero (* 10\\%, ** 5\\%,",
                    " *** 1\\%), SE clustered by advisor.}")),
           file.path(tab_dir, "ib_eqshare_trend_by_group.tex"))

print(ag[MDate %in% as.Date(c("2020-04-30", "2020-12-31"))])
print(dcast(res[outcome %in% c("pf_trend_draw", "pf_excess_draw", "pf_excess_dec")], char + group ~ outcome,
            value.var = "cell"))
