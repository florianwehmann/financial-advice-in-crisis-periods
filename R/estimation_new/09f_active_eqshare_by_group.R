# =============================================================================
# 09f_active_eqshare_by_group.R -- who de-risked and who added risk in the COVID crash?
#
# Takes the ACTIVE part of the equity-share decomposition of 09e (same definition,
# recomputed here so 09e and its outputs stay untouched) and splits it by client
# characteristics measured in Jan 2020.
#
#   active_t = w_t - w^p_t,  w^p_t = (E_t - dqE_t) / (P_t - dqP_t)
#   i.e. the change in the equity share of the portfolio caused by trading, net of
#   the price/FX drift of last month's holdings. Summed per client over
#     drawdown  Feb-Apr 2020
#     to Dec    Feb-Dec 2020
#   De-risked / added risk: cumulative active change over the drawdown below -1pp /
#   above +1pp; in between (mostly clients who did not trade) = no change.
#   The wealth-share version (active incl. deposits/withdrawals) is reported in the
#   table and CSV; figures use the portfolio share.
#
# Note: bounds are mechanical -- a client at 100% equity cannot add equity share and
# one at 0% cannot remove any, which matters for the equity-share groups.
#
# Sample as in 09/09e: portfolio > CHF 5,000 in Jan 2020. Monthly contributions
# winsorized at 0.1%/99.9%. Run from R/estimation_new; writes only new files.
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
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

T0      <- as.Date("2020-01-31")
WIN     <- as.Date(c("2020-01-01", "2020-12-31"))
DRAW    <- as.Date(c("2020-02-01", "2020-04-30"))
MIN_PF  <- 5000
MIN_D   <- 1000
THR     <- 0.01    # 1pp active change counts as de-risking / adding risk

# ---- panel and sample -----------------------------------------------------------
pos <- setDT(read_parquet("../../data/pos_pf.parquet",
                          col_select = c("Bp_ID", "MDate", "advisor_id", "sex", "birth_year",
                                         "anlegerprofil", "anlagepaket", "equity", "dq_equity",
                                         "tot_pf", "dq_tot_pf", "tot_wealth")))
pos <- pos[MDate %between% WIN]
bp  <- pos[MDate == T0 & tot_pf > MIN_PF, Bp_ID]
pos <- pos[Bp_ID %in% bp]
setorder(pos, Bp_ID, MDate)

# ---- monthly active contributions (as in 09e) ------------------------------------
pos[, `:=`(E_l = shift(equity), P_l = shift(tot_pf), W_l = shift(tot_wealth)), by = Bp_ID]
pos[, `:=`(E_p = equity - dq_equity, P_p = tot_pf - dq_tot_pf)]
pos[, W_p := W_l + (P_p - P_l)]
pos[P_l >= MIN_D & P_p >= MIN_D & tot_pf >= MIN_D, pf_active := equity / tot_pf - E_p / P_p]
pos[W_l >= MIN_D & W_p >= MIN_D & tot_wealth >= MIN_D, w_active := equity / tot_wealth - E_p / W_p]
pos[, `:=`(pf_active = winsor(pf_active), w_active = winsor(w_active))]

act <- pos[MDate > T0, .(
  pf_draw = sum(pf_active[MDate %between% DRAW], na.rm = TRUE),
  pf_dec  = sum(pf_active, na.rm = TRUE),
  w_draw  = sum(w_active[MDate %between% DRAW], na.rm = TRUE),
  w_dec   = sum(w_active, na.rm = TRUE)), by = Bp_ID]

# ---- characteristics in Jan 2020 ---------------------------------------------------
cl <- pos[MDate == T0, .(Bp_ID, advisor_id, sex, birth_year, anlegerprofil, anlagepaket,
                         eq_sh0 = equity / tot_pf, tot_wealth)]
cl <- merge(cl, act, by = "Bp_ID")
cl[is.na(advisor_id), advisor_id := -Bp_ID]
cl[, age := 2020 - birth_year]
cl[, Sex := fcase(sex == "m", "Male", sex == "w", "Female", default = "Sex unknown")]
cl[, Age := fcase(age %between% c(18, 44),  "Under 45",
                  age %between% c(45, 64),  "45-64",
                  age %between% c(65, 100), "65+",
                  default = "Age unknown")]
cl[, `Risk type` := fcase(grepl("^Vorsicht", anlegerprofil), "Cautious",
                          grepl("^Ausgegl",  anlegerprofil), "Balanced",
                          grepl("^Risiko",   anlegerprofil), "Risk-seeking",
                          default = "No profile")]
cl[, Wealth := paste("Wealth Q", cut(tot_wealth, quantile(tot_wealth, 0:4/4), include.lowest = TRUE,
                                     labels = FALSE), sep = "")]
cl[, `Equity share` := fcase(eq_sh0 < 1/3, "Equity < 33%", eq_sh0 <= 2/3, "Equity 33-67%",
                             default = "Equity > 67%")]
cl[, Advice := fcase(grepl("^CONSULT", anlagepaket), "CONSULT (advisory)",
                     anlagepaket == "DIRECT",       "DIRECT (execution only)",
                     default = "Package unknown")]
cl[, reaction := fcase(pf_draw < -THR, "De-risked", pf_draw > THR, "Added risk", default = "No change")]

grp_lv <- list(Sex          = c("Male", "Female", "Sex unknown"),
               Age          = c("Under 45", "45-64", "65+", "Age unknown"),
               `Risk type`  = c("Cautious", "Balanced", "Risk-seeking", "No profile"),
               `Equity share` = c("Equity < 33%", "Equity 33-67%", "Equity > 67%"),
               Wealth       = paste0("Wealth Q", 1:4),
               Advice       = c("DIRECT (execution only)", "CONSULT (advisory)", "Package unknown"))
grp_vars <- names(grp_lv)
cat(sprintf("clients: %s | drawdown: %.1f%% de-risked, %.1f%% added risk, %.1f%% no change\n",
            format(nrow(cl), big.mark = "'"), 100 * mean(cl$reaction == "De-risked"),
            100 * mean(cl$reaction == "Added risk"), 100 * mean(cl$reaction == "No change")))

# ---- group means (SE clustered by advisor) ----------------------------------------
OUTC <- c(pf_draw = "Active change,\nFeb-Apr 2020 (pp)", pf_dec = "Active change,\nFeb-Dec 2020 (pp)",
          w_draw  = "Wealth share: active,\nFeb-Apr 2020 (pp)", w_dec = "Wealth share: active,\nFeb-Dec 2020 (pp)")
cl[, `:=`(derisk = as.numeric(reaction == "De-risked"), addrisk = as.numeric(reaction == "Added risk"))]
res <- rbindlist(lapply(grp_vars, function(g) {
  d <- copy(cl); d[, grp := factor(get(g), levels = grp_lv[[g]])]
  rbindlist(lapply(c(names(OUTC), "derisk", "addrisk"), function(y) {
    m  <- feols(as.formula(sprintf("%s ~ 0 + grp", y)), d, cluster = ~advisor_id)
    ct <- coeftable(m)
    data.table(char = g, group = sub("^grp", "", rownames(ct)), outcome = y,
               mean = 100 * ct[, 1], se = 100 * ct[, 2])
  }))[, N := as.integer(table(d$grp)[group])]
}))
fwrite(res, file.path(out_dir, "ib_eqshare_active_by_group.csv"))

# ---- figure 1: mean active change by group -----------------------------------------
n_lab <- unique(res[, .(group, N)])
lab_n <- setNames(sprintf("%s   N = %s", n_lab$group, formatC(n_lab$N, big.mark = "'", format = "d")),
                  n_lab$group)
pm <- res[outcome %in% c("pf_draw", "pf_dec")]
pm[, `:=`(char = factor(char, levels = grp_vars), measure = factor(OUTC[outcome], levels = OUTC),
          group = factor(group, levels = rev(unlist(grp_lv))))]
pm[, sig := fifelse(abs(mean) > 1.96 * se, "Different from 0 (95%)", "Not significant")]

theme_grp <- theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        strip.placement = "outside", panel.spacing.x = unit(1.5, "lines"),
        strip.text.y.left = element_text(angle = 0, hjust = 1, vjust = 1, face = "bold"),
        strip.text.x = element_text(face = "bold", hjust = 0),
        legend.position = "top", legend.justification = "left",
        plot.caption = element_text(colour = "grey45", size = 7.5, hjust = 0),
        plot.caption.position = "plot")

p_m <- ggplot(pm, aes(mean, group)) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_linerange(aes(xmin = mean - 1.96 * se, xmax = mean + 1.96 * se, colour = sig), linewidth = 0.5) +
  geom_point(aes(colour = sig), size = 1.8) +
  scale_colour_manual(values = c("Different from 0 (95%)" = "#2a78d6", "Not significant" = "grey65"),
                      name = NULL) +
  facet_grid(char ~ measure, scales = "free_y", space = "free_y", switch = "y") +
  scale_y_discrete(labels = lab_n) +
  scale_x_continuous(labels = function(x) sub("^[+-]0$", "0", sprintf("%+g", x))) +
  labs(x = NULL, y = NULL,
       caption = paste("Mean cumulative active change in the equity share of the portfolio (trading, net of price drift),",
                       "with 95% CI clustered by advisor.\n> 0: clients added equity risk; < 0: clients de-risked.",
                       "Characteristics in Jan 2020; sample: portfolio > CHF 5,000.")) +
  theme_grp
ggsave(file.path(fig_dir, "ib_eqshare_active_by_group.pdf"), p_m, width = 7.5, height = 7,
       device = cairo_pdf)
p_m

# ---- figure 2: share of clients who de-risked vs added risk (drawdown) -------------
pr <- res[outcome %in% c("derisk", "addrisk")]
pr[, `:=`(char = factor(char, levels = grp_vars), group = factor(group, levels = rev(unlist(grp_lv))),
          dir = factor(fifelse(outcome == "derisk", "De-risked (active < -1pp)", "Added risk (active > +1pp)"),
                       levels = c("De-risked (active < -1pp)", "Added risk (active > +1pp)")),
          x = fifelse(outcome == "derisk", -mean, mean))]
p_r <- ggplot(pr, aes(x, group, fill = dir)) +
  geom_col(width = 0.65) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.1f", mean), hjust = fifelse(x < 0, 1.15, -0.15)),
            size = 2.4, colour = "grey30") +
  scale_fill_manual(values = c("De-risked (active < -1pp)" = "#e34948",
                               "Added risk (active > +1pp)" = "#2a78d6"), name = NULL) +
  facet_grid(char ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_y_discrete(labels = lab_n) +
  scale_x_continuous(labels = function(x) paste0(abs(x), "%"), expand = expansion(mult = 0.12)) +
  labs(x = "Share of clients, Feb-Apr 2020", y = NULL,
       caption = paste("Clients whose trading changed the equity share of their portfolio by more than 1pp",
                       "(cumulative Feb-Apr 2020, net of price drift).\nThe rest (mostly clients who did not trade) is not shown.")) +
  theme_grp + theme(panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3))
ggsave(file.path(fig_dir, "ib_eqshare_derisk_by_group.pdf"), p_r, width = 7, height = 6.5,
       device = cairo_pdf)
p_r

# ---- table: group means ------------------------------------------------------------
tex_esc <- function(x) gsub(">", "$>$", gsub("<", "$<$", gsub("%", "\\%", x, fixed = TRUE), fixed = TRUE), fixed = TRUE)
star <- function(m, s) { z <- abs(m / s); fcase(z > 2.576, "$^{***}$", z > 1.96, "$^{**}$", z > 1.645, "$^{*}$", default = "") }
res[, cell := paste0(sprintf("%.2f", mean), star(mean, se))]
res[outcome %in% c("derisk", "addrisk"), cell := sprintf("%.1f", mean)]
tw <- dcast(res, char + group + N ~ outcome, value.var = "cell")
body <- unlist(lapply(grp_vars, function(g) {
  d <- tw[char == g][match(grp_lv[[g]], group)]
  c(sprintf("\\multicolumn{8}{l}{\\textit{%s}} \\\\", g),
    d[, sprintf("\\quad %s & %s & %s & %s & %s & %s & %s & %s \\\\", tex_esc(group), pf_draw, pf_dec,
                w_draw, w_dec, derisk, addrisk, formatC(N, big.mark = "'", format = "d"))])
}))
writeLines(c("\\begin{tabular}{lrrrrrrr}", "\\toprule",
             "& \\multicolumn{2}{c}{Equity / portfolio} & \\multicolumn{2}{c}{Equity / wealth} & \\multicolumn{2}{c}{Clients (\\%), Feb-Apr} & \\\\",
             "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
             "& Feb-Apr & Feb-Dec & Feb-Apr & Feb-Dec & De-risked & Added risk & N \\\\",
             "\\midrule", body, "\\bottomrule", "\\end{tabular}",
             paste0("\\par\\smallskip\\parbox{\\linewidth}{\\footnotesize Mean cumulative active change in the equity share",
                    " in percentage points (trading net of price and FX drift; for the wealth share also deposits and",
                    " withdrawals). Stars: mean different from zero (* 10\\%, ** 5\\%, *** 1\\%), SE clustered by advisor.",
                    " De-risked / added risk: active change in the portfolio share below $-1$pp / above $+1$pp, Feb-Apr 2020.}")),
           file.path(tab_dir, "ib_eqshare_active_by_group.tex"))

# ---- joint regression: characteristics are correlated (age, wealth, advice) --------
reg_d <- copy(cl)
for (g in grp_vars) set(reg_d, j = g, value = factor(reg_d[[g]], levels = grp_lv[[g]]))
for (y in names(OUTC)) set(reg_d, j = y, value = 100 * reg_d[[y]])
regs <- lapply(names(OUTC), function(y)
  feols(as.formula(sprintf("%s ~ Sex + Age + `Risk type` + `Equity share` + Wealth + Advice", y)),
        reg_d, cluster = ~advisor_id))
names(regs) <- gsub("\n", " ", OUTC)
coef_nm <- unique(unlist(lapply(regs, function(m) names(coef(m)))))
reg_dict <- setNames(tex_esc(sub("^`?(Sex|Age|Risk type|Equity share|Wealth|Advice)`?", "\\1: ", coef_nm)), coef_nm)
etable(regs, tex = TRUE, file = file.path(tab_dir, "ib_eqshare_active_regs.tex"), replace = TRUE,
       digits = 3, fitstat = ~ n + r2, style.tex = style.tex("aer"), dict = reg_dict, depvar = FALSE,
       headers = list(":_:" = c("Portfolio, Feb-Apr", "Portfolio, Feb-Dec", "Wealth, Feb-Apr", "Wealth, Feb-Dec")),
       notes = paste("Dependent variable: cumulative active change in the equity share (pp). Omitted: male, under 45,",
                     "cautious, equity share below 33\\%, wealth Q1, DIRECT. SE clustered by advisor."))
print(etable(regs, digits = 3, dict = reg_dict))
