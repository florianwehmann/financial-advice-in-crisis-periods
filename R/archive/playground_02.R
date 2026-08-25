library(data.table)
library(haven)
library(lubridate)
library(readxl)
library(ggplot2)

rm(list = ls())
gc()

emp_dir <- "C:/Users/FWehmann/Dropbox/Financial Advice in Crisis Periods/Empirical Analysis/"

out_fig_path <- "C:/Users/FWehmann/Dropbox/Apps/Overleaf/Uncertainty and Financial Advise (1)/figures"

# ------------------------------------------------------------------------------
# Load (already built and saved below)
pos_c_m <- readRDS("../data/agg_pos_smi.rds")

# Baseline mean/SD (pre-2020) — derived from loaded data
pos_mean <- mean(pos_c_m[Date < as.Date("2020-01-01")]$chf_return)
pos_sd   <- sd(pos_c_m[Date < as.Date("2020-01-01")]$chf_return)

smi_mean <- mean(pos_c_m[Date < as.Date("2020-01-01")]$smi_return, na.rm = TRUE)
smi_sd   <- sd(pos_c_m[Date < as.Date("2020-01-01")]$smi_return, na.rm = TRUE)

# ------------------------------------------------------------------------------
# Figures: aggregate portfolio

# Level CHF return with mean ± 1.5 SD bands
ggplot(pos_c_m, aes(x = Date)) +
  geom_line(aes(y = chf_return)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = pos_mean, linetype = 2) +
  geom_hline(yintercept = pos_mean + 1.5 * pos_sd, linetype = 3) +
  geom_hline(yintercept = pos_mean - 1.5 * pos_sd, linetype = 3)

# Indexed cumulative return (rebased to 100 at start)
ggplot(pos_c_m, aes(x = Date)) +
  geom_line(aes(y = cumprod(1 + chf_return) / (1 + chf_return[1]) * 100)) +
  geom_hline(yintercept = 100)

# FX return
ggplot(pos_c_m, aes(x = Date)) +
  geom_line(aes(y = DA_Devisenkursabweichung_CHF)) +
  geom_hline(yintercept = 0)

# ------------------------------------------------------------------------------
# Figures: portfolio vs. SMI

clr_port <- "#2166ac"
clr_smi  <- "#d6604d"
fig_theme <- theme_minimal(base_size = 12) +
  theme(legend.position   = "bottom",
        panel.grid.minor  = element_blank(),
        axis.text.x       = element_text(angle = 45, hjust = 1),
        plot.title        = element_text(face = "bold"),
        plot.caption      = element_text(color = "grey50", size = 8))

# Figure 1: Cumulative return index (rebased to 100 at first observation)
p_cum <- ggplot(pos_c_m, aes(x = Date)) +
  geom_hline(yintercept = 100, color = "grey70", linewidth = 0.4) +
  geom_line(aes(y = cumprod(1 + chf_return) / (1 + chf_return[1]) * 100,
                color = "Bank portfolios"), linewidth = 0.9) +
  geom_line(aes(y = cumprod(1 + smi_return) / (1 + smi_return[1]) * 100,
                color = "SMI"), linewidth = 0.9) +
  scale_color_manual(values = c("Bank portfolios" = clr_port, "SMI" = clr_smi)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0.01, 0)) +
  labs(x = NULL, y = "Return index (start = 100)", color = NULL,
       title = "Cumulative returns: bank portfolios vs. SMI") +
  fig_theme

print(p_cum)
ggsave(file.path(out_fig_path, "cum_returns_portfolio_smi.pdf"), p_cum, width = 8, height = 5)

# Figure 2: Monthly returns with pre-2020 mean ± 1.5 SD bands
# (shaded band = normal range; lines = realized monthly returns)
p_ret <- ggplot(pos_c_m, aes(x = Date)) +
  annotate("rect",
           xmin = min(pos_c_m$Date), xmax = max(pos_c_m$Date),
           ymin = smi_mean - 1.5 * smi_sd, ymax = smi_mean + 1.5 * smi_sd,
           fill = clr_smi, alpha = 0.08) +
  annotate("rect",
           xmin = min(pos_c_m$Date), xmax = max(pos_c_m$Date),
           ymin = pos_mean - 1.5 * pos_sd, ymax = pos_mean + 1.5 * pos_sd,
           fill = clr_port, alpha = 0.08) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
  geom_hline(yintercept = smi_mean,  color = clr_smi,  linetype = "dashed", linewidth = 0.5) +
  geom_hline(yintercept = pos_mean,  color = clr_port, linetype = "dashed", linewidth = 0.5) +
  geom_line(aes(y = smi_return, color = "SMI"),             linewidth = 0.8, alpha = 0.85) +
  geom_line(aes(y = chf_return, color = "Bank portfolios"), linewidth = 0.8, alpha = 0.85) +
  scale_color_manual(values = c("Bank portfolios" = clr_port, "SMI" = clr_smi)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0.01, 0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = NULL, y = "Monthly return", color = NULL,
       title   = "Monthly returns: bank portfolios vs. SMI",
       caption = "Shaded bands = mean ± 1.5 SD (pre-2020 baseline). Dashed lines = means.") +
  fig_theme

print(p_ret)
ggsave(file.path(out_fig_path, "monthly_returns_portfolio_smi.pdf"), p_ret, width = 8, height = 5)

# Figure 3: Net quantity change vs. SMI return (bars = volume, line = SMI)
p_vol <- ggplot(pos_c_m, aes(x = Date)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
  geom_col(aes(y = d_vol / 2e9), fill = "grey60", alpha = 0.8, width = 20) +
  geom_line(aes(y = smi_return, color = "SMI"), linewidth = 0.9) +
  scale_color_manual(values = c("SMI" = clr_smi)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0.01, 0)) +
  labs(x = NULL, y = NULL, color = NULL,
       title   = "Net quantity change vs. SMI return",
       caption = "Bars = net quantity change in CHF (scaled ÷ 2bn). Line = SMI monthly return.") +
  fig_theme

print(p_vol)
ggsave(file.path(out_fig_path, "volume_vs_smi.pdf"), p_vol, width = 8, height = 5)

# ==============================================================================
# BUILD (run once to create ../data/agg_pos_smi.rds)
# ==============================================================================

# # Load positions (.dta -> .rds conversion already done)
# # pos <- as.data.table(read_dta(paste0(emp_dir, "/Source/Original Data/", "T30_Pos_fact.dta")))
# # saveRDS(pos, paste0(emp_dir, "/Source/Original Data/", "T30_Pos_fact.rds"))
# pos <- readRDS(paste0(emp_dir, "/Source/Original Data/", "T30_Pos_fact.rds"))
#
# # Collapse to monthly aggregates (Instrumentengruppe 1-3: equities-like)
# pos_c <- pos[Instrumentengruppe %in% c(1, 2, 3),
#              lapply(.SD, sum, na.rm = TRUE),
#              by = "Period_ID",
#              .SDcols = c("DA_Titelkursabweichung_CHF", "DA_Devisenkursabweichung_CHF",
#                          "DA_Mengenabweichung_CHF", "Geschaeftsvolumen_CHF")]
#
# pos_c[, Date := ceiling_date(
#   as.Date(paste0(substr(Period_ID, 1, 4), "-", substr(Period_ID, 5, 6), "-01")), "month") - 1]
# pos_c[, Period_ID := NULL]
# setcolorder(pos_c, "Date")
# pos_c <- pos_c[Date >= as.Date("2010-01-01")]
#
# pos_c[, chf_return := DA_Titelkursabweichung_CHF / Geschaeftsvolumen_CHF]
# pos_c[, fx_return  := DA_Devisenkursabweichung_CHF / Geschaeftsvolumen_CHF]
#
# # Merge with SMI (month-end prices)
# smi <- fread("../data/hsmi.csv", skip = 4, select = 1:2)
# smi[, DATE := as.Date(DATE, format = "%d.%m.%Y")]
# names(smi) <- c("Date", "SMI")
# smi <- smi[order(Date)]
# smi[, Date_m := ceiling_date(Date, "month") - 1]
# smi <- smi[, lapply(.SD, last), by = Date_m, .SDcols = "SMI"]
# smi[, smi_return := c(NA, diff(SMI)) / SMI]
#
# pos_c_m <- merge(pos_c, smi, by.x = "Date", by.y = "Date_m", all.x = TRUE)
#
# # Mark high-volatility months (portfolio return > mean + 1.5 SD)
# pos_mean_build <- mean(pos_c_m[Date < as.Date("2020-01-01")]$chf_return)
# pos_sd_build   <- sd(pos_c_m[Date < as.Date("2020-01-01")]$chf_return)
# pos_c_m[, above_15sd := as.integer(chf_return > pos_mean_build + 1.5 * pos_sd_build)]
#
# # Volume change
# pos_c_m[, d_vol     := DA_Mengenabweichung_CHF]
# pos_c_m[, d_log_vol := log(DA_Mengenabweichung_CHF)]
#
# saveRDS(pos_c_m, "../data/agg_pos_smi.rds")
