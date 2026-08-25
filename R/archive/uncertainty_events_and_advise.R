## uncertainty_events

library(arrow)
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(fixest)

rm(list=ls()); gc()


## ---------------------------------------------------------------------------
## advised trades (from analysis_advise.R)
## ---------------------------------------------------------------------------

dtb <- read_parquet("../data/boerse_merged.parquet") %>% setDT()
dtb <- dtb[DDate >= "2011-01-01" & DDate <= "2024-12-31"]

dtb_nadv     <- dtb[, lapply(.SD, sum, na.rm = TRUE), by = DDate, .SDcols = "advised"]
dtb_nadv_byi <- dtb[, lapply(.SD, sum, na.rm = TRUE), by = c("DDate", "K_Aufnahme"), .SDcols = "advised"]
setorder(dtb_nadv, DDate)

ggplot(dtb_nadv, aes(DDate)) +
  geom_line(aes(y = advised))

ggplot(dtb_nadv_byi, aes(DDate)) +
  geom_line(aes(y = advised, color = K_Aufnahme))


## ---------------------------------------------------------------------------
## daily SMI returns and uncertainty events
## ---------------------------------------------------------------------------

smi_daily <- fread("../data/hsmi.csv", skip = 4, select = 1:2)
setnames(smi_daily, c("Date", "SMI"))
smi_daily[, Date := as.Date(Date, format = "%d.%m.%Y")]
smi_daily <- smi_daily[order(Date)]
smi_daily[, smi_ret := SMI / shift(SMI) - 1]
smi_daily <- smi_daily[Date >= "2009-01-01"]

# uncertainty events: |daily return| > 3x its SD
smi_sd <- sd(smi_daily$smi_ret, na.rm = TRUE)
smi_daily[, uncertainty_event := abs(smi_ret) > 3 * smi_sd]

uncertainty_events <- smi_daily[uncertainty_event == TRUE]
unc_dates <- sort(unique(uncertainty_events$Date))

ggplot(smi_daily, aes(x = Date, y = smi_ret)) +
  geom_line(color = "grey50") +
  geom_point(data = uncertainty_events, color = "red", size = 1) +
  geom_hline(yintercept = c(-3, 3) * smi_sd, linetype = "dashed", color = "red") +
  theme_light() +
  labs(x = NULL, y = "SMI daily return", title = "Uncertainty events (|return| > 3 SD)")


## advised trades vs. uncertainty events, daily

dt_msmi <- merge(dtb_nadv, smi_daily, by.x = "DDate", by.y = "Date", all = TRUE)
dt_msmi <- dt_msmi[DDate >= "2011-01-01" & DDate <= "2024-12-31"]
dt_msmi[is.na(advised), advised := 0]

ggplot(dt_msmi, aes(x = DDate)) +
  geom_vline(data = uncertainty_events[Date >= "2011-01-01" & Date <= "2024-12-31"],
             aes(xintercept = Date), color = "red", linewidth = 0.5, alpha = 0.4) +
  geom_line(aes(y = advised))


## ---------------------------------------------------------------------------
## uncertainty dummies
## ---------------------------------------------------------------------------

# For each row's date, find the nearest preceding uncertainty event and flag
# which (mutually exclusive) window it falls in: same day, 1-3d, 4-7d, 8-14d,
# 15-21d, 22-28d after the event. If `contact_col` is supplied, a window is
# only flagged TRUE when that row's contact happened on or after the event
# date -- i.e. the contact behind the trade can plausibly be a response to
# that specific uncertainty event, not to something that happened before it.
add_unc_dummies <- function(dt, date_col, unc_dates, contact_col = NULL) {
  dates <- dt[[date_col]]
  idx <- findInterval(as.numeric(dates), as.numeric(unc_dates))
  idx[idx == 0] <- NA
  event_date <- unc_dates[idx]
  gap <- as.integer(dates - event_date)

  valid <- TRUE
  if (!is.null(contact_col)) {
    contact <- dt[[contact_col]]
    valid <- is.na(contact) | contact >= event_date
  }

  dt[, unc_event_date := event_date]
  dt[, `:=`(
    unc_same_day = !is.na(gap) & gap == 0                & valid,
    unc_past_3d  = !is.na(gap) & gap >= 1  & gap <= 3     & valid,
    unc_past_1w  = !is.na(gap) & gap >= 4  & gap <= 7     & valid,
    unc_past_2w  = !is.na(gap) & gap >= 8  & gap <= 14    & valid,
    unc_past_3w  = !is.na(gap) & gap >= 15 & gap <= 21    & valid,
    unc_past_4w  = !is.na(gap) & gap >= 22 & gap <= 28    & valid
  )]
  invisible(NULL)
}


## ---------------------------------------------------------------------------
## daily advised-trade counts, with uncertainty dummies
## ---------------------------------------------------------------------------

adv_trades <- dtb[advised == TRUE, .(
  n_advised_trades_client = sum(K_Aufnahme == "Durch Kunde", na.rm = TRUE),
  n_advised_trades_adv    = sum(K_Aufnahme == "Durch Kundenberater", na.rm = TRUE)
), by = DDate]
adv_trades[, n_advised_trades := n_advised_trades_client + n_advised_trades_adv]
setorder(adv_trades, DDate)

add_unc_dummies(adv_trades, "DDate", unc_dates)


## regressions: uncertainty -> advised trades (levels)

spec1 <- "n_advised_trades ~ unc_same_day + unc_past_3d + unc_past_1w + unc_past_2w + unc_past_3w"
fit1 <- lm(spec1, adv_trades)
summary(fit1)

spec2 <- "n_advised_trades ~ unc_same_day + unc_past_3d"
fit2 <- lm(spec2, adv_trades)
summary(fit2)

spec3_client <- "n_advised_trades_client ~ unc_same_day + unc_past_3d + unc_past_1w + unc_past_2w + unc_past_3w"
fit3_client <- lm(spec3_client, adv_trades)
summary(fit3_client)

spec3_adv <- "n_advised_trades_adv ~ unc_same_day + unc_past_3d + unc_past_1w + unc_past_2w + unc_past_3w"
fit3_adv <- lm(spec3_adv, adv_trades)
summary(fit3_adv)


## ---------------------------------------------------------------------------
## panel version: one row per trade, with uncertainty dummies
## ---------------------------------------------------------------------------

panel_trades <- copy(dtb)
add_unc_dummies(panel_trades, "DDate", unc_dates, contact_col = "last_contact")

panel_trades[, advised_client := as.integer(K_Aufnahme == "Durch Kunde")]
panel_trades[, advised_adv    := as.integer(K_Aufnahme == "Durch Kundenberater")]
panel_trades[is.na(advised_client), advised_client := 0]
panel_trades[is.na(advised_adv), advised_adv := 0]

panel_trades[, year_month := factor(format(DDate, "%Y-%m"))]


## panel regression: probability that a trade is advised

spec_panel        <- "advised ~ unc_same_day + unc_past_3d + unc_past_1w + unc_past_2w + unc_past_3w | year_month"
spec_panel_client <- "advised_client ~ unc_same_day + unc_past_3d + unc_past_1w + unc_past_2w + unc_past_3w | year_month"
spec_panel_adv    <- "advised_adv ~ unc_same_day + unc_past_3d + unc_past_1w + unc_past_2w + unc_past_3w | year_month"

fit_logit <- feglm(as.formula(spec_panel), data = panel_trades, family = binomial(link = "logit"))
summary(fit_logit)

fit_logit_client <- feglm(as.formula(spec_panel_client), data = panel_trades, family = binomial(link = "logit"))
summary(fit_logit_client)

fit_logit_adv <- feglm(as.formula(spec_panel_adv), data = panel_trades, family = binomial(link = "logit"))
summary(fit_logit_adv)

# fit_probit <- feglm(as.formula(spec_panel), data = panel_trades, family = binomial(link = "probit"))
# summary(fit_probit)
