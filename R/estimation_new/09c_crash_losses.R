# =============================================================================
# 09c_crash_losses.R -- how much did the average client lose in the COVID crash,
# was the loss permanent because clients left the market, and who lost most?
#
# Timeline (month-ends)
#   T0       2020-01-31  last pre-crash month: characteristics and holdings frozen here
#   crash    2020-02-29 .. 2020-03-31  (SMI peak 19 Feb, trough 16 Mar)
#   horizons 2020-03-31 (trough), 2020-06-30, 2020-12-31 (main), 2021-12-31
#
# Measures, per client
#   crash loss   sum of price+FX changes (dp + dfx) over Feb-Mar 2020, in CHF and
#                in % of the Jan-2020 portfolio / wealth
#   act_ret(H)   actual return on WEALTH (portfolio + cash) from T0 to H,
#                chain-linked monthly: (dp + dfx) / wealth_{t-1}. Cash earns ~0, and
#                deposits/withdrawals drop out because it is time-weighted.
#   pb_ret(H)    buy-and-hold: Jan-2020 securities (frozen quantities, realized
#                prices from pb_funs.R) plus Jan-2020 cash, also in % of wealth
#   gap(H)       act_ret - pb_ret = what the client's own crisis reaction cost
#                (<0) or earned (>0), in % of pre-crisis wealth; x wealth_T0 = CHF
#   Wealth, not the portfolio, is the base on purpose: selling into cash leaves the
#   return of what is still invested unchanged, so a portfolio return cannot show
#   the cost of exiting. The portfolio-level TWR gap is kept as a secondary measure.
#
#   exit         net equity sales Feb-Apr 2020 >= 50% of Jan-2020 equity
#                (quantity changes dq, so price moves do not count as selling)
#   still out    net equity sales Feb 2020 .. H still >= 50% of Jan-2020 equity
#
# Cross-sections (all measured at T0): gender, age, stated risk profile
# (anlegerprofil), revealed risk taking (equity share of portfolio), wealth.
# Education is NOT in the data (neither pos_pf nor the position file carries it).
#
# pos_pf is main-bank clients only (02a_pos_aggm.R filters Hauptbankkunde == 'Ja').
#
# Run from R/estimation_new. Reads pos_pf, pos_m1 and the cached price panel of
# passive_benchmark.R; writes figures/tables to Overleaf and CSVs to output/.
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(fixest)
  library(ggplot2)
  library(duckdb)
  library(lubridate)
})
source("pb_funs.R")

# ---- Config ------------------------------------------------------------------
overleaf_dir <- "C:/Users/FWehmann/Dropbox/Apps/Overleaf/Uncertainty and Financial Advise (1)"
fig_dir <- file.path(overleaf_dir, "figures/invbeh")
tab_dir <- file.path(overleaf_dir, "tables/invbeh")
OUT     <- "../../output/crash_losses"
for (d in c(fig_dir, tab_dir, OUT)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

posm_path <- "../../data/pos_pf.parquet"
pos_path  <- "../../data/pos_m1.parquet"
px_file   <- "../../output/passive_bm/pb_price_panel.parquet"

T0      <- as.Date("2020-01-31")
T_END   <- as.Date("2021-12-31")
CRASH   <- as.Date(c("2020-02-29", "2020-03-31"))
EXIT_W  <- as.Date(c("2020-02-29", "2020-04-30"))   # window in which exit is measured
HOR     <- c(trough = "2020-03-31", h6 = "2020-06-30", h12 = "2020-12-31", h24 = "2021-12-31")
HOR     <- setNames(as.Date(HOR), names(HOR))
H_MAIN  <- "h12"
MIN_PF  <- 1000    # CHF portfolio at T0 to enter the sample
MIN_EQ  <- 1000    # CHF equity at T0 to enter the exit analysis
EXIT_SH <- 0.5     # net equity sold (share of T0 equity) that counts as exit
RED_SH  <- 0.05    # ... that counts as a reduction
P_WIN   <- 0.99

if (!file.exists(px_file)) pb_price_panel(pos_path, px_file, "2010-01-01", "2024-12-31")

winsor <- function(x, p = P_WIN) {
  q <- quantile(x[is.finite(x)], c(1 - p, p), na.rm = TRUE)
  x[!is.finite(x)] <- NA_real_
  pmin(pmax(x, q[1]), q[2])
}

# ---- 1. Client x month panel, T0 .. T_END --------------------------------------
cols <- c("MDate", "Bp_ID", "advisor_id", "sex", "birth_year", "anlegerprofil",
          "tot_pf", "tot_wealth", "equity", "dq_equity", "dq_tot_pf",
          "dp_tot_pf", "dfx_tot_pf")
pan <- setDT(read_parquet(posm_path, col_select = all_of(cols)))
pan <- pan[MDate >= T0 & MDate <= T_END]
setorder(pan, Bp_ID, MDate)

base <- pan[MDate == T0 & tot_pf >= MIN_PF & tot_wealth > 0]
pan  <- pan[Bp_ID %in% base$Bp_ID]
months <- sort(unique(pan$MDate))
pan[, m := match(MDate, months) - 1L]                      # months since T0
pan[, gap_in_panel := m != seq_along(m) - 1L, by = Bp_ID]  # client missed a month
pan[, left := cumsum(gap_in_panel) > 0, by = Bp_ID]
pan <- pan[left == FALSE][, c("gap_in_panel", "left") := NULL]
cat(sprintf("sample: %s clients with >= CHF %s portfolio at %s\n",
            format(nrow(base), big.mark = "'"), MIN_PF, T0))

# ---- 2. Characteristics at T0 --------------------------------------------------
base[, age := 2020 - birth_year]
base[!between(age, 18, 100), age := NA]
base[, female := fcase(sex == "w", 1, sex == "m", 0, default = NA_real_)]
base[, gender := factor(fcase(sex == "m", "Male", sex == "w", "Female"),
                        levels = c("Male", "Female"))]
base[, age_grp := cut(age, c(17, 44, 64, 100), labels = c("Under 45", "45-64", "65+"))]
base[, risk := factor(fcase(
  grepl("^Vorsicht", anlegerprofil),  "Cautious",
  grepl("^Ausgegl",  anlegerprofil),  "Balanced",
  grepl("^Risiko",   anlegerprofil),  "Risk-seeking",
  default = "No profile"), levels = c("Cautious", "Balanced", "Risk-seeking", "No profile"))]
base[, eq_sh0 := pmin(pmax(equity / tot_pf, 0), 1)]
# fixed bands, not terciles: many portfolios sit at exactly 0% or 100% equity
base[, eq_grp := cut(eq_sh0, c(0, 1/3, 2/3, 1), include.lowest = TRUE,
                     labels = c("Equity < 33%", "Equity 33-67%", "Equity > 67%"))]
base[, wealth_q := cut(tot_wealth, quantile(tot_wealth, 0:4 / 4), include.lowest = TRUE,
                       labels = paste0("Wealth Q", 1:4))]
base[is.na(advisor_id), advisor_id := -Bp_ID]    # no advisor: own cluster
setnames(base, c("tot_pf", "tot_wealth", "equity"), c("pf0", "w0", "eq0"))
chars <- base[, .(Bp_ID, advisor_id, pf0, w0, eq0, eq_sh0, age, female,
                  gender, age_grp, risk, eq_grp, wealth_q)]

# ---- 3. Buy-and-hold benchmark (Jan-2020 securities at realized prices) --------
ep  <- data.table(ep_id = 1L, pre_month = T0, pre_start = T0, post_end = T_END)
smp <- data.table(Bp_ID = base$Bp_ID, ep_id = 1L)
pb  <- pb_build_passive(ep, smp, pos_path, px_file)
pb[, pb_ret_pf := pb_pf / pb_pf[MDate == T0][1] - 1, by = Bp_ID]
pb[!is.finite(pb_ret_pf), pb_ret_pf := NA_real_]
CHK_pb <- merge(pb[MDate == T0, .(Bp_ID, pb_pf)], base[, .(Bp_ID, pf0)], by = "Bp_ID")

pan <- merge(pan, pb[, .(Bp_ID, MDate, pb_ret_pf)], by = c("Bp_ID", "MDate"), all.x = TRUE)
pan <- merge(pan, chars[, .(Bp_ID, pf0, w0, eq0)], by = "Bp_ID")
setorder(pan, Bp_ID, MDate)

# ---- 4. Monthly paths: actual vs buy-and-hold, on wealth and on the portfolio ---
pan[, `:=`(w_lag = shift(tot_wealth), pf_lag = shift(tot_pf)), by = Bp_ID]
pan[, gain := dp_tot_pf + dfx_tot_pf]
pan[, r_w  := fifelse(w_lag  > 0,      gain / w_lag,  0)]
pan[, r_pf := fifelse(pf_lag >= MIN_PF, gain / pf_lag, 0)]  # (near) empty portfolio earns 0
pan[m == 0, `:=`(r_w = 0, r_pf = 0, gain = 0)]
pan[!is.finite(r_w),  r_w  := 0]
pan[!is.finite(r_pf), r_pf := 0]
pan[, `:=`(act_ret    = cumprod(1 + pmax(r_w,  -1)) - 1,
           act_ret_pf = cumprod(1 + pmax(r_pf, -1)) - 1,
           gain_cum   = cumsum(gain)), by = Bp_ID]
pan[, pb_ret    := pb_ret_pf * pf0 / w0]      # frozen securities + frozen cash
pan[, gap       := act_ret - pb_ret]
pan[, gap_pf    := act_ret_pf - pb_ret_pf]
pan[, eq_sold   := -cumsum(fifelse(m == 0, 0, dq_equity)) / eq0, by = Bp_ID]
pan[eq0 < MIN_EQ, eq_sold := NA_real_]

# ---- 5. Client-level outcomes -------------------------------------------------
at <- function(h, v) pan[MDate == HOR[[h]], c("Bp_ID", v), with = FALSE]

cl <- copy(chars)
crash <- pan[MDate %in% CRASH, .(crash_chf = sum(dp_tot_pf + dfx_tot_pf)), by = Bp_ID]
cl <- merge(cl, crash, by = "Bp_ID", all.x = TRUE)
cl[, `:=`(crash_pf = crash_chf / pf0, crash_w = crash_chf / w0)]

exit <- pan[MDate == EXIT_W[2], .(Bp_ID, eq_sold_crash = eq_sold)]
cl <- merge(cl, exit, by = "Bp_ID", all.x = TRUE)
cl[, reduced := as.numeric(eq_sold_crash >= RED_SH)]
cl[, exited  := as.numeric(eq_sold_crash >= EXIT_SH)]
cl[, reaction := factor(fcase(eq_sold_crash >= EXIT_SH, "Exited (sold >= 50% of equity)",
                              eq_sold_crash >= RED_SH,  "Reduced (sold 5-50%)",
                              !is.na(eq_sold_crash),    "Held or bought"),
                        levels = c("Held or bought", "Reduced (sold 5-50%)",
                                   "Exited (sold >= 50% of equity)"))]

for (h in names(HOR)) {
  o <- at(h, c("act_ret", "pb_ret", "gap", "gap_pf", "eq_sold"))
  setnames(o, -1, paste0(c("act_ret", "pb_ret", "gap", "gap_pf", "eq_sold"), "_", h))
  cl <- merge(cl, o, by = "Bp_ID", all.x = TRUE)
  cl[, paste0("gap_chf_", h) := get(paste0("gap_", h)) * w0]
  cl[, paste0("out_", h) := fifelse(exited == 1, as.numeric(get(paste0("eq_sold_", h)) >= EXIT_SH),
                                    NA_real_)]
}

# winsorized copies of every continuous outcome (suffix _w is avoided: crash_w exists)
wcols <- c("crash_pf", "crash_w", "crash_chf",
           grep("^(act_ret|pb_ret|gap|gap_pf|gap_chf)_", names(cl), value = TRUE))
cl[, paste0(wcols, "_win") := lapply(.SD, winsor), .SDcols = wcols]
write_parquet(cl, file.path(OUT, "cl_client_outcomes.parquet"))

# ---- 6. Checks ----------------------------------------------------------------
chk <- c(
  sprintf("clients at T0: %d; still in panel at %s: %d; at %s: %d", nrow(cl),
          HOR[["h12"]], sum(!is.na(cl$gap_h12)), HOR[["h24"]], sum(!is.na(cl$gap_h24))),
  sprintf("buy-and-hold value at T0 / pos_pf portfolio at T0: median %.3f, p5 %.3f, p95 %.3f",
          median(CHK_pb$pb_pf / CHK_pb$pf0), quantile(CHK_pb$pb_pf / CHK_pb$pf0, .05),
          quantile(CHK_pb$pb_pf / CHK_pb$pf0, .95)),
  sprintf("clients without a buy-and-hold value: %d", sum(is.na(cl$pb_ret_h12))),
  sprintf("age missing/invalid: %d; gender n/a: %d; no risk profile: %d",
          sum(is.na(cl$age)), sum(is.na(cl$gender)), sum(cl$risk == "No profile")),
  sprintf("exit analysis (equity >= CHF %d at T0): %d clients", MIN_EQ, sum(!is.na(cl$exited))),
  sprintf("crash: actual vs passive loss in %% of wealth, means %.4f vs %.4f",
          mean(cl$crash_w, na.rm = TRUE), mean(cl$pb_ret_trough, na.rm = TRUE)))
writeLines(chk, file.path(OUT, "checks.txt"))
cat(chk, sep = "\n")

# ---- 7. Summary table -----------------------------------------------------------
sum_rows <- list(
  "Crash loss, CHF (Feb-Mar 2020)"                    = "crash_chf_win",
  "Crash loss, \\% of portfolio"                      = "crash_pf_win",
  "Crash loss, \\% of wealth"                         = "crash_w_win",
  "Buy-and-hold return to Mar 2020, \\% of wealth"    = "pb_ret_trough_win",
  "Actual return to Dec 2020, \\% of wealth"          = "act_ret_h12_win",
  "Buy-and-hold return to Dec 2020, \\% of wealth"    = "pb_ret_h12_win",
  "Reaction gap Dec 2020, \\% of wealth"              = "gap_h12_win",
  "Reaction gap Dec 2020, CHF"                        = "gap_chf_h12_win",
  "Reaction gap Dec 2021, \\% of wealth"              = "gap_h24_win",
  "Reaction gap Dec 2020, portfolio TWR"              = "gap_pf_h12_win",
  "Reduced equity by $\\geq$ 5\\% (Feb-Apr 2020)"     = "reduced",
  "Exited: sold $\\geq$ 50\\% of equity (Feb-Apr 2020)" = "exited",
  "Exiters still out in Dec 2020"                     = "out_h12",
  "Exiters still out in Dec 2021"                     = "out_h24")
pct_rows <- !grepl("CHF", names(sum_rows))
summ <- rbindlist(lapply(seq_along(sum_rows), function(i) {
  x <- cl[[sum_rows[[i]]]]; x <- x[!is.na(x)]
  data.table(measure = names(sum_rows)[i], mean = mean(x), median = median(x),
             p10 = quantile(x, .1), p90 = quantile(x, .9), N = length(x))
}))
fwrite(summ, file.path(OUT, "cl_summary.csv"))

fmt <- function(x, pct) if (pct) sprintf("%.1f", 100 * x) else formatC(round(x), big.mark = "'", format = "d")
tex_esc <- function(x) {
  x <- gsub("%", "\\%", x, fixed = TRUE)
  x <- gsub("<", "$<$", x, fixed = TRUE)
  gsub(">", "$>$", x, fixed = TRUE)
}
tex_tab <- function(body, head, align, file, note = NULL) {
  lines <- c(sprintf("\\begin{tabular}{%s}", align), "\\toprule", head, "\\midrule", body,
             "\\bottomrule", "\\end{tabular}")
  if (!is.null(note))
    lines <- c(lines, sprintf("\\par\\smallskip\\parbox{\\linewidth}{\\footnotesize %s}", note))
  writeLines(lines, file)
}
body <- summ[, sprintf("%s & %s & %s & %s & %s & %s \\\\", measure,
                       mapply(fmt, mean, pct_rows), mapply(fmt, median, pct_rows),
                       mapply(fmt, p10, pct_rows), mapply(fmt, p90, pct_rows),
                       formatC(N, big.mark = "'", format = "d"))]
tex_tab(body, "& Mean & Median & P10 & P90 & N \\\\", "lrrrrr",
        file.path(tab_dir, "cl_summary.tex"),
        note = paste("Percentages in \\%, CHF amounts in CHF. Wealth = portfolio + cash at the bank.",
                     "Reaction gap = actual time-weighted return on wealth minus a buy-and-hold",
                     "of the January 2020 portfolio and cash. Continuous measures winsorized at 1\\%/99\\%."))

# ---- 8. Cross-sectional differences ----------------------------------------------
GRPS <- c(gender = "Gender", age_grp = "Age", risk = "Risk profile (stated)",
          eq_grp = "Risk taking (equity share)", wealth_q = "Wealth")
OUTC <- c(crash_w_win     = "Crash loss\n(% of wealth)",
          exited          = "Exited equity\n(% of clients)",
          out_h12         = "Exiters still out\nDec 2020 (%)",
          gap_h12_win     = "Reaction gap\nDec 2020 (% wealth)",
          gap_chf_h12_win = "Reaction gap\nDec 2020 (CHF)")
PCT_OUT <- names(OUTC) != "gap_chf_h12_win"

grp_means <- rbindlist(lapply(names(GRPS), function(g) rbindlist(lapply(names(OUTC), function(y) {
  d <- cl[!is.na(get(g)) & !is.na(get(y))]
  d[, (g) := droplevels(get(g))]
  mod <- feols(as.formula(sprintf("%s ~ 0 + %s", y, g)), d, cluster = ~advisor_id)
  dif <- feols(as.formula(sprintf("%s ~ i(%s)", y, g)), d, cluster = ~advisor_id)
  ct  <- coeftable(mod); dct <- coeftable(dif)
  lv  <- levels(d[[g]])
  data.table(char = g, group = factor(lv, levels = lv), outcome = y,
             mean = ct[, 1], se = ct[, 2], N = as.integer(table(d[[g]])[lv]),
             p_vs_ref = c(NA_real_, dct[-1, 4]),        # vs first level
             p_joint  = tryCatch(wald(dif, print = FALSE)$p, error = function(e) NA_real_))  # all equal
}))))
fwrite(grp_means, file.path(OUT, "cl_group_means.csv"))

# group-means table: one row per group, one column per outcome, stars vs reference group
stars <- function(p) fcase(is.na(p), "", p < .01, "$^{***}$", p < .05, "$^{**}$", p < .1, "$^{*}$", default = "")
gm <- copy(grp_means)
gm[, cell := fcase(outcome == "gap_chf_h12_win", formatC(round(mean), big.mark = "'", format = "d"),
                   outcome == "gap_h12_win",     sprintf("%.2f", 100 * mean),   # gaps are small
                   default = sprintf("%.1f", 100 * mean))]
gm[, cell := paste0(sub("^-(0\\.0+)$", "\\1", cell), stars(p_vs_ref))]
gm_w <- dcast(gm, char + group ~ outcome, value.var = "cell")
n_w  <- dcast(gm[outcome %in% c("crash_w_win", "out_h12")], char + group ~ outcome, value.var = "N")
setnames(n_w, c("crash_w_win", "out_h12"), c("N", "N_exit"))
gm_w <- merge(gm_w, n_w, by = c("char", "group"))
gm_w[, char := factor(char, levels = names(GRPS))]
setorder(gm_w, char, group)
body <- unlist(lapply(names(GRPS), function(g) {
  d <- gm_w[char == g]
  c(sprintf("\\multicolumn{8}{l}{\\textit{%s}} \\\\", GRPS[[g]]),
    d[, sprintf("\\quad %s & %s & %s & %s & %s & %s & %s & %s \\\\",
                tex_esc(group), crash_w_win, exited,
                out_h12, gap_h12_win, gap_chf_h12_win, formatC(N, big.mark = "'", format = "d"),
                N_exit)])
}))
tex_tab(body,
        paste("& Crash loss & Exited & Still out & Gap Dec 20 & Gap Dec 20 & N & N \\\\",
              "& (\\% wealth) & (\\%) & (\\%) & (\\% wealth) & (CHF) & & exiters \\\\"),
        "lrrrrrrr", file.path(tab_dir, "cl_group_means.tex"),
        note = paste("Group means. Stars: difference to the first group of each block",
                     "(* 10\\%, ** 5\\%, *** 1\\%), standard errors clustered by advisor.",
                     "Exited / still out: clients with at least CHF 1,000 in equity in January 2020."))

# joint regression: characteristics are correlated (age, wealth, risk)
# percent outcomes in pp so coefficients read like the group-means table
cl_reg <- copy(cl)
for (y in names(OUTC)[PCT_OUT]) set(cl_reg, j = y, value = 100 * cl_reg[[y]])
regs <- lapply(names(OUTC), function(y)
  feols(as.formula(sprintf("%s ~ female + i(age_grp) + i(risk) + i(eq_grp) + i(wealth_q)", y)),
        cl_reg, cluster = ~advisor_id))
names(regs) <- gsub("\n", " ", OUTC)
# label every factor term explicitly (etable would drop spaces and leave < > % raw)
reg_dict <- c(female = "Female")
for (g in c("age_grp", "risk", "eq_grp", "wealth_q"))
  for (lv in levels(cl[[g]])[-1])
    reg_dict[paste0(g, "::", lv)] <- paste(GRPS[[g]], "--", tex_esc(lv))
etable(regs, tex = TRUE, file = file.path(tab_dir, "cl_regressions.tex"), replace = TRUE,
       digits = 3, fitstat = ~ n + r2, style.tex = style.tex("aer"), dict = reg_dict,
       headers = list(":_:" = c("Crash loss (pp of wealth)", "Exited equity (pp)",
                                "Still out Dec 2020 (pp)", "Gap Dec 2020 (pp of wealth)",
                                "Gap Dec 2020 (CHF)")),
       depvar = FALSE,
       notes = paste("Omitted groups: male, under 45, cautious, equity share below 33\\%, wealth Q1.",
                     "Standard errors clustered by advisor."))
etable(regs, digits = 3, dict = reg_dict)

# ---- 9. Figures -------------------------------------------------------------------
col_blue <- "#2a78d6"; col_orange <- "#eb6834"; col_aqua <- "#1baf7a"; col_ink <- "grey25"
theme_cl <- theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
        axis.text = element_text(colour = "grey35"), axis.title = element_text(colour = "grey25"),
        strip.text = element_text(face = "bold", hjust = 0, colour = "grey15"),
        legend.position = "top", legend.justification = "left",
        legend.margin = margin(0, 0, 0, 0), panel.spacing = unit(1, "lines"),
        plot.caption = element_text(colour = "grey45", size = 7.5, hjust = 0),
        plot.caption.position = "plot")
save_fig <- function(p, name, w = 6.5, h = 4)
  ggsave(file.path(fig_dir, paste0(name, ".pdf")), p, width = w, height = h, device = cairo_pdf)

# (a) wealth paths by crisis reaction: actual (solid) vs buy-and-hold (dashed)
react_cols <- setNames(c(col_blue, col_aqua, col_orange), levels(cl$reaction))
pth <- merge(pan[, .(Bp_ID, MDate, act_ret, pb_ret)], cl[!is.na(reaction), .(Bp_ID, reaction)],
             by = "Bp_ID")
pth[, `:=`(act_ret = winsor(act_ret), pb_ret = winsor(pb_ret)), by = MDate]
pth <- pth[, .(Actual = mean(act_ret, na.rm = TRUE), `Buy and hold` = mean(pb_ret, na.rm = TRUE),
               N = .N), by = .(MDate, reaction)]
n_lab <- pth[MDate == T0, setNames(sprintf("%s (N = %s)", reaction, formatC(N, big.mark = "'", format = "d")),
                                   reaction)]
pth <- melt(pth, id.vars = c("MDate", "reaction", "N"), variable.name = "path")
p_path <- ggplot(pth, aes(MDate, 100 * value, colour = reaction, linetype = path)) +
  annotate("rect", xmin = as.Date("2020-02-01"), xmax = as.Date("2020-03-31"),
           ymin = -Inf, ymax = Inf, fill = "grey88", alpha = 0.8) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.35) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = react_cols, labels = n_lab, name = NULL) +
  scale_linetype_manual(values = c(Actual = "solid", `Buy and hold` = "22"), name = NULL) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y", expand = expansion(mult = 0.01)) +
  guides(colour = guide_legend(ncol = 1, order = 1), linetype = guide_legend(ncol = 1, order = 2)) +
  labs(x = NULL, y = "Cumulative return on Jan-2020 wealth (%)",
       caption = paste("Clients with at least CHF 1,000 in equity in Jan 2020, grouped by net equity sales Feb-Apr 2020.",
                       "Buy and hold: Jan-2020 securities and cash, never traded.",
                       "Returns winsorized at 1%/99% by month.", sep = "\n")) +
  theme_cl + theme(legend.box = "horizontal")
save_fig(p_path, "cl_paths_by_reaction", h = 4.6)

p_path
# (b) group means with 95% CIs: rows = characteristic, columns = outcome
gp <- copy(grp_means)
gp[, scl := fifelse(outcome == "gap_chf_h12_win", 1, 100)]
gp[, `:=`(est = mean * scl, lo = (mean - 1.96 * se) * scl, hi = (mean + 1.96 * se) * scl)]
gp[, outcome := factor(OUTC[outcome], levels = OUTC)]
gp[, char := factor(GRPS[char], levels = GRPS)]
gp[, group := factor(group, levels = rev(unique(unlist(lapply(names(GRPS), function(g) levels(cl[[g]]))))))]
p_grp <- ggplot(gp, aes(est, group)) +
  geom_vline(data = gp[, .(x = weighted.mean(est, N)), by = outcome], aes(xintercept = x),
             colour = "grey70", linewidth = 0.35, linetype = "22") +
  geom_linerange(aes(xmin = lo, xmax = hi), colour = col_blue, linewidth = 0.5) +
  geom_point(colour = col_blue, size = 1.8) +
  facet_grid(char ~ outcome, scales = "free", space = "free_y", switch = "y") +
  scale_x_continuous(labels = scales::label_comma(big.mark = "'"), n.breaks = 4) +
  labs(x = NULL, y = NULL,
       caption = paste("Group means with 95% confidence intervals (SE clustered by advisor); dashed line: sample mean.",
                       "Percent outcomes in %, CHF outcome in CHF. Characteristics measured in Jan 2020.", sep = "\n")) +
  theme_cl +
  theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0, hjust = 1, vjust = 1),
        strip.text.x = element_text(size = 8), panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3),
        panel.grid.major.y = element_blank(), axis.text.x = element_text(size = 7))
save_fig(p_grp, "cl_group_differences", w = 9, h = 6)
p_grp
cat("done: figures in", fig_dir, "| tables in", tab_dir, "| csv in", OUT, "\n")
