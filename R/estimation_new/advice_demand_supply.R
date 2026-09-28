# =============================================================================
# advice_demand_supply.R -- does investment advice go UP or DOWN in a crisis,
# and is it demand (client-initiated) or supply (advisor-initiated)?
#
# Three views, all descriptive (no regression, no controls):
#   1. event time, pooled over the seven episodes: share of clients with a
#      contact by rel_month, drawdown shaded
#   2. the same per episode (small multiples)
#   3. calendar time: monthly contact rate 2011-2024 with the crisis windows shaded
# plus a table of pre / drawdown / post means and the change against pre.
#
# Reads stk2.parquet, pos_pf.parquet and episodes_short.parquet read-only.
# Writes only into output/advice_flow/. Run from R/estimation_new.
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(ggplot2)
})

OUT <- "../../output/advice_flow"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

stk_path  <- "../../data/stk2.parquet"
posm_path <- "../../data/pos_pf.parquet"
ep_path   <- "../../data/episodes_short.parquet"

# the three contact flags, labelled by who picked up the phone
SERIES <- c(inv_a_p     = "Supply: advisor-initiated inv. advice",
            inv_c_p     = "Demand: client-initiated inv. advice",
            perfinv_a_p = "Supply: adv-init. performance review + advice")
COLS <- c("#C0392B", "#41729F", "#E8A33D")
WIN  <- c(-6L, 9L)

ep <- setDT(read_parquet(ep_path))[usable == TRUE]
setorder(ep, dd_start)
ep[, ep_lab := sprintf("%s (%s)", sub("_[0-9]{6}$", "", ep_id), format(dd_start, "%Y-%m"))]

# =============================================================================
# 1-2. EVENT TIME
# =============================================================================
d <- setDT(read_parquet(stk_path, mmap = FALSE,
                        col_select = c("Bp_ID", "ep_id", "rel_month", names(SERIES))))
d <- d[!is.na(rel_month) & between(rel_month, WIN[1], WIN[2])]
for (v in names(SERIES)) set(d, i = which(is.na(d[[v]])), j = v, value = 0)

rate <- melt(d[, lapply(.SD, mean), by = .(ep_id, rel_month), .SDcols = names(SERIES)],
             id.vars = c("ep_id", "rel_month"), variable.name = "flag", value.name = "rate")
rate <- merge(rate, ep[, .(ep_id, ep_lab, n_months)], by = "ep_id")
rate[, series := factor(SERIES[as.character(flag)], levels = SERIES)]

# pooled over episodes, weighting every episode equally
pooled <- rate[, .(rate = mean(rate)), by = .(rel_month, series)]
# drawdown band: rel_month 0 .. n_months (episodes are 2-3 months long)
dd_band <- data.table(xmin = -0.5, xmax = max(ep$n_months) + 0.5)

p1 <- ggplot(pooled, aes(rel_month, 100 * rate, colour = series)) +
  geom_rect(data = dd_band, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey70", alpha = 0.30) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.6) +
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = 0.4) +
  scale_colour_manual(values = COLS) +
  scale_x_continuous(breaks = seq(WIN[1], WIN[2], 1)) +
  expand_limits(y = 0) +
  labs(x = "Months relative to the start of the drawdown", y = "Clients with a contact (%)",
       colour = NULL,
       title = "Advice in crises: supply and demand both rise",
       subtitle = paste("Share of clients with at least one contact that month, averaged over the seven episodes;",
                        "shaded = drawdown.\nAdvisor-initiated contacts are the larger flow in levels;",
                        "client-initiated ones rise slightly more in relative terms.")) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
ggsave(file.path(OUT, "advice_event_time_pooled.pdf"), p1, width = 9, height = 5)

p2 <- ggplot(rate, aes(rel_month, 100 * rate, colour = series)) +
  geom_rect(data = unique(rate[, .(ep_lab, n_months)]), inherit.aes = FALSE,
            aes(xmin = -0.5, xmax = n_months + 0.5, ymin = -Inf, ymax = Inf),
            fill = "grey70", alpha = 0.30) +
  geom_line(linewidth = 0.6) +
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = 0.3) +
  facet_wrap(~ep_lab, ncol = 4) +
  scale_colour_manual(values = COLS) +
  scale_x_continuous(breaks = seq(WIN[1], WIN[2], 3)) +
  expand_limits(y = 0) +
  labs(x = "Months relative to the start of the drawdown", y = "Clients with a contact (%)",
       colour = NULL, title = "Advice contacts around each crisis episode",
       subtitle = "Shaded = the drawdown window that defines treatment") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
ggsave(file.path(OUT, "advice_event_time_by_episode.pdf"), p2, width = 12, height = 6)

# ---- table: pre / drawdown / post, and the change against pre ----------------
rate[, phase := fifelse(rel_month < 0, "pre",
                        fifelse(rel_month <= n_months, "drawdown", "post"))]
tab <- dcast(rate[, .(rate = mean(rate)), by = .(ep_lab, series, phase)],
             ep_lab + series ~ phase, value.var = "rate")
tab[, `:=`(chg_dd_pp = 100 * (drawdown - pre), chg_dd_pct = 100 * (drawdown / pre - 1),
           chg_post_pct = 100 * (post / pre - 1))]
pool_tab <- dcast(pooled[, phase := fifelse(rel_month < 0, "pre",
                                            fifelse(rel_month <= max(ep$n_months), "drawdown", "post"))][
                    , .(rate = mean(rate)), by = .(series, phase)],
                  series ~ phase, value.var = "rate")
pool_tab[, `:=`(ep_lab = "ALL (pooled)", chg_dd_pp = 100 * (drawdown - pre),
                chg_dd_pct = 100 * (drawdown / pre - 1), chg_post_pct = 100 * (post / pre - 1))]
tab <- rbind(tab, pool_tab, use.names = TRUE)
setcolorder(tab, c("ep_lab", "series", "pre", "drawdown", "post"))
fwrite(tab, file.path(OUT, "advice_rates_by_phase.csv"))
cat("\nContact rates by phase (share of clients per month):\n")
print(tab[, .(ep_lab, series, pre = round(100 * pre, 2), drawdown = round(100 * drawdown, 2),
              post = round(100 * post, 2), chg_dd_pct = round(chg_dd_pct, 1))])

# =============================================================================
# 3. CALENDAR TIME
# =============================================================================
cal <- setDT(read_parquet(posm_path, mmap = FALSE,
                          col_select = c("Bp_ID", "MDate", names(SERIES))))
for (v in names(SERIES)) set(cal, i = which(is.na(cal[[v]])), j = v, value = 0)
cal <- cal[MDate >= as.Date("2011-01-31")]
cal_m <- melt(cal[, lapply(.SD, mean), by = MDate, .SDcols = names(SERIES)],
              id.vars = "MDate", variable.name = "flag", value.name = "rate")
cal_m[, series := factor(SERIES[as.character(flag)], levels = SERIES)]
fwrite(cal_m, file.path(OUT, "advice_rates_calendar.csv"))

p3 <- ggplot(cal_m, aes(MDate, 100 * rate, colour = series)) +
  geom_rect(data = ep, inherit.aes = FALSE,
            aes(xmin = dd_start, xmax = dd_end, ymin = -Inf, ymax = Inf),
            fill = "#C0392B", alpha = 0.18) +
  geom_line(linewidth = 0.5) +
  scale_colour_manual(values = COLS) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  expand_limits(y = 0) +
  labs(x = NULL, y = "Clients with a contact (%)", colour = NULL,
       title = "Investment advice over time, with the seven crisis episodes shaded",
       subtitle = "Monthly share of clients with at least one contact") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
ggsave(file.path(OUT, "advice_calendar_time.pdf"), p3, width = 11, height = 5)

cat("\nFigures in", normalizePath(OUT), "\n")
