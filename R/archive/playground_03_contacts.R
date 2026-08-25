library(data.table)
library(haven)
library(lubridate)
library(readxl)
library(ggplot2)
library(arrow)
library(duckdb)

rm(list = ls())
gc()

emp_dir      <- "C:/Users/FWehmann/Dropbox/Financial Advice in Crisis Periods/Empirical Analysis/"
emp_dir2     <- "C:/Users/FWehmann/Dropbox/Wehmann Household Finance Project/RawData_Stata/"
emp_dir_prep <- "C:/Users/FWehmann/Dropbox/Wehmann Household Finance Project/DataPrepared/"
out_fig_path <- "C:/Users/FWehmann/Dropbox/Apps/Overleaf/Uncertainty and Financial Advise (1)/figures"

# Contact-type indicator columns
c_cols <- c(
  "K_Physisch", "K_Reklamation", "K_Umfassendes_Beratungsgespraech",
  "K_Anlegen", "K_Finanzieren", "K_Vorsorge", "K_Basisdienstleistung",
  "K_Steuern", "K_Nachlassplanung", "K_Pensionierung", "K_Nachfolgeregelung",
  "K_Kundenzufriedenheit", "K_Marketing_Kampagne", "K_Syst_Geschaeftsmoeglichkeit",
  "K_Performancebesprechung", "K_Bilanzbesprechung", "K_Compliance"
)

# ==============================================================================
# ONE-TIME: convert posc.rds -> posc.parquet (comment out once done)
# ==============================================================================
# posc_tmp <- readRDS("../data/posc.rds")
# write_parquet(posc_tmp, "../data/posc.parquet", compression = "zstd")
# rm(posc_tmp); gc()

# ==============================================================================
# LOAD — DuckDB reads only what each analysis needs; posc never fully in RAM
# ==============================================================================

con <- dbConnect(duckdb(), dbdir = ":memory:")

# Register posc.parquet as a view so all queries below just say FROM posc
# Note: posc columns from the pos x agg_pos_smi merge carry .x/.y suffixes:
#   "DA_Titelkursabweichung_CHF.x"  = client-level (from pos)
#   "DA_Titelkursabweichung_CHF.y"  = aggregate market (from agg_pos_smi)
dbExecute(con, "CREATE VIEW posc AS SELECT * FROM read_parquet('../data/posc.parquet')")

# contacts_monthly (Analysis 2): DISTINCT on Bp_ID x month — skips position rows
contacts_monthly <- setDT(dbGetQuery(con, "
  SELECT DISTINCT Bp_ID, Date AS month,
    n_contacts, n_invest, n_finance,
    n_init_by_adv, n_init_by_client,
    n_invest_init_by_adv, n_invest_init_by_client,
    K_Physisch, K_Reklamation, K_Umfassendes_Beratungsgespraech,
    K_Anlegen, K_Finanzieren, K_Vorsorge, K_Basisdienstleistung,
    K_Steuern, K_Nachlassplanung, K_Pensionierung, K_Nachfolgeregelung,
    K_Kundenzufriedenheit, K_Marketing_Kampagne, K_Syst_Geschaeftsmoeglichkeit,
    K_Performancebesprechung, K_Bilanzbesprechung, K_Compliance
  FROM posc
"))

# ==============================================================================
# ANALYSIS 1: Covid crisis (Mar–May 2020)
# Treatment: had at least one contact in Mar, Apr, or May 2020
# ==============================================================================

# Aggregate to (Date x Instrumentengruppe x covid_advised) entirely in DuckDB —
# no client-level rows enter R; only the tiny group-mean summary lands in memory
covid_agg <- setDT(dbGetQuery(con, "
  WITH treated AS (
    SELECT DISTINCT Bp_ID
    FROM posc
    WHERE YEAR(Date) = 2020 AND MONTH(Date) IN (3, 4, 5) AND n_contacts > 0
  )
  SELECT
    p.Date,
    p.Instrumentengruppe,
    (t.Bp_ID IS NOT NULL)                        AS covid_advised,
    AVG(\"DA_Titelkursabweichung_CHF.x\")        AS DA_Titelkursabweichung_CHF,
    AVG(\"DA_Devisenkursabweichung_CHF.x\")      AS DA_Devisenkursabweichung_CHF,
    AVG(\"Geschaeftsvolumen_CHF.x\")             AS Geschaeftsvolumen_CHF
  FROM posc p
  LEFT JOIN treated t ON p.Bp_ID = t.Bp_ID
  WHERE \"DA_Titelkursabweichung_CHF.x\" != 0
  GROUP BY p.Date, p.Instrumentengruppe, covid_advised
  ORDER BY p.Date
"))

# covid_agg[, .(n_groups = .N), by = covid_advised]   # sanity check

# Cumulative returns by instrument group: advised vs. non-advised
for (i in unique(covid_agg$Instrumentengruppe)) {

  sub_c <- covid_agg[Instrumentengruppe == i]
  sub_c[, chf_return := DA_Titelkursabweichung_CHF / Geschaeftsvolumen_CHF]
  sub_c[, fx_return  := DA_Devisenkursabweichung_CHF / Geschaeftsvolumen_CHF]
  setnafill(sub_c, fill = 0, cols = c("chf_return", "fx_return"))

  g <- ggplot() +
    geom_line(data = sub_c[covid_advised == TRUE],
              aes(x = Date, y = cumprod(1 + chf_return) / (1 + chf_return[1]) * 100, color = "advised")) +
    geom_line(data = sub_c[covid_advised == FALSE],
              aes(x = Date, y = cumprod(1 + chf_return) / (1 + chf_return[1]) * 100, color = "non-advised")) +
    geom_hline(yintercept = 100) +
    labs(x = NULL, y = NULL, color = NULL, title = i)
  print(g)
}

# Instrument groups by Titelkursabweichung availability:
#   None:   Bar, Nicht zugeteilt, Money Market, Swaps, Limite, Kredit, Währung
#   Sparse: Optionen, Warrants, Anrechte, Kryptowährung, Futures
#   Active: Ansprüche, Aktien, Obligationen, Fonds, Strukt. Prod, Metall

# ==============================================================================
# ANALYSIS 2: Contact supply over time
# ==============================================================================

contacts_monthly[, marketing_init_by_client := K_Marketing_Kampagne]

contacts_m <- contacts_monthly[, lapply(.SD, mean, na.rm = TRUE),
                                by = month, .SDcols = names(contacts_monthly)[-(1:2)]]
setorderv(contacts_m, "month")

# Stacked area: advisor- vs. client-initiated investment contacts
plot_data <- melt(
  contacts_m[month > as.Date("2012-12-31")],
  id.vars      = "month",
  measure.vars = c("n_invest_init_by_adv", "n_invest_init_by_client"),
  variable.name = "type", value.name = "count"
)

p_contacts <- ggplot() +
  geom_area(data = plot_data,
            aes(x = month, y = count, fill = type),
            position = "stack", alpha = 0.75) +
  geom_line(data = contacts_m[month > as.Date("2012-12-31")],
            aes(x = month, y = n_invest),
            linewidth = 0.7, color = "black") +
  scale_fill_manual(
    values = c(n_invest_init_by_adv = "#2166ac", n_invest_init_by_client = "#d6604d"),
    labels = c(n_invest_init_by_adv = "Advisor-initiated", n_invest_init_by_client = "Client-initiated")
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0)) +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
  labs(x = NULL, y = "Number of contacts", fill = NULL,
       title   = "Monthly client contacts",
       caption = "Line = total contacts. Areas = contacts by initiating party.") +
  theme_minimal(base_size = 12) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x      = element_text(angle = 45, hjust = 1),
        plot.title       = element_text(face = "bold"),
        plot.caption     = element_text(color = "grey50", size = 8))

print(p_contacts)
ggsave(file.path(out_fig_path, "monthly_contacts_by_initiator.pdf"), p_contacts, width = 8, height = 5)

# Time series for each contact-type indicator
for (i in c_cols) {
  g <- ggplot(contacts_m, aes(x = month)) +
    geom_line(aes(y = get(i))) +
    labs(title = i, y = NULL, x = NULL)
  print(g)
}

# ==============================================================================
# ANALYSIS 3: SNB floor removal — January 2015
# Treatment: had at least one advisor-initiated contact in Jan 2015
# Sample: 2014–2015 only; client-level .x columns aliased to clean names
# ==============================================================================

sub_snb <- setDT(dbGetQuery(con, "
  SELECT Bp_ID, Date, Instrumentengruppe,
    \"DA_Titelkursabweichung_CHF.x\"   AS DA_Titelkursabweichung_CHF,
    \"DA_Devisenkursabweichung_CHF.x\" AS DA_Devisenkursabweichung_CHF,
    \"Geschaeftsvolumen_CHF.x\"        AS Geschaeftsvolumen_CHF,
    n_init_by_adv
  FROM posc
  WHERE Date >= '2014-01-01' AND Date <= '2015-12-31'
"))

snb_jan15       <- sub_snb[year(Date) == 2015 & month(Date) == 1,
                            .(had_adv_contact = any(n_init_by_adv > 0)), by = Bp_ID]
snb_advised_ids <- snb_jan15[had_adv_contact == TRUE, Bp_ID]
sub_snb[, snb_advised := Bp_ID %in% snb_advised_ids]

sub_snb[, .(n_clients = uniqueN(Bp_ID)), by = snb_advised]   # group sizes

# Overall cumulative return: advised vs. non-advised
snb_overall <- sub_snb[DA_Titelkursabweichung_CHF != 0,
                        lapply(.SD, mean, na.rm = TRUE), by = c("Date", "snb_advised"),
                        .SDcols = c("DA_Titelkursabweichung_CHF", "DA_Devisenkursabweichung_CHF",
                                    "Geschaeftsvolumen_CHF")]
snb_overall[, chf_return := DA_Titelkursabweichung_CHF / Geschaeftsvolumen_CHF]
snb_overall[, fx_return  := DA_Devisenkursabweichung_CHF / Geschaeftsvolumen_CHF]
setnafill(snb_overall, fill = 0, cols = c("chf_return", "fx_return"))

ggplot() +
  geom_vline(xintercept = as.Date("2015-01-31"), linetype = "dashed", color = "grey50") +
  geom_line(data = snb_overall[snb_advised == TRUE],
            aes(x = Date, y = cumprod(1 + chf_return) / (1 + chf_return[1]) * 100, color = "advised")) +
  geom_line(data = snb_overall[snb_advised == FALSE],
            aes(x = Date, y = cumprod(1 + chf_return) / (1 + chf_return[1]) * 100, color = "non-advised")) +
  geom_hline(yintercept = 100) +
  scale_color_manual(values = c(advised = "#2166ac", "non-advised" = "#d6604d")) +
  labs(x = NULL, y = "Cumulative return (Jan 2014 = 100)", color = NULL,
       title    = "All instruments — SNB floor removal",
       subtitle = "Treated = advisor-initiated contact in Jan 2015") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

# Per-instrument-group
for (i in unique(sub_snb$Instrumentengruppe)) {

  sub_snb_c <- sub_snb[DA_Titelkursabweichung_CHF != 0 & Instrumentengruppe == i,
                        lapply(.SD, mean, na.rm = TRUE), by = c("Date", "snb_advised"),
                        .SDcols = c("DA_Titelkursabweichung_CHF", "DA_Devisenkursabweichung_CHF",
                                    "Geschaeftsvolumen_CHF")]
  sub_snb_c[, chf_return := DA_Titelkursabweichung_CHF / Geschaeftsvolumen_CHF]
  sub_snb_c[, fx_return  := DA_Devisenkursabweichung_CHF / Geschaeftsvolumen_CHF]
  setnafill(sub_snb_c, fill = 0, cols = c("chf_return", "fx_return"))

  g <- ggplot() +
    geom_vline(xintercept = as.Date("2015-01-31"), linetype = "dashed", color = "grey50") +
    geom_line(data = sub_snb_c[snb_advised == TRUE],
              aes(x = Date, y = cumprod(1 + chf_return) / (1 + chf_return[1]) * 100, color = "advised")) +
    geom_line(data = sub_snb_c[snb_advised == FALSE],
              aes(x = Date, y = cumprod(1 + chf_return) / (1 + chf_return[1]) * 100, color = "non-advised")) +
    geom_hline(yintercept = 100) +
    scale_color_manual(values = c(advised = "#2166ac", "non-advised" = "#d6604d")) +
    labs(x = NULL, y = "Cumulative return (Jan 2014 = 100)", color = NULL,
         title    = paste0(i, " — SNB floor removal"),
         subtitle = "Treated = advisor-initiated contact in Jan 2015") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
  print(g)
}

rm(sub_snb); gc()

# ==============================================================================
# ANALYSIS 4: High-volatility months — do contacts increase?
# above_15sd = 1 when aggregate portfolio chf_return > mean + 1.5 SD (pre-2020)
# ==============================================================================

# 7 columns only, deduplicated — a fraction of full posc size
client_month <- setDT(dbGetQuery(con, "
  SELECT DISTINCT Bp_ID, Date, above_15sd,
    n_invest_init_by_adv, n_invest_init_by_client,
    n_contacts, n_invest
  FROM posc
"))

# Mean contacts by volatility regime
client_month[, .(
  n_adv    = mean(n_invest_init_by_adv,    na.rm = TRUE),
  n_client = mean(n_invest_init_by_client, na.rm = TRUE),
  n_obs    = .N
), by = above_15sd]

# Daily SMI — high-volatility day flags for vertical-line markers on the contact plot
smi_daily <- fread("../data/hsmi.csv", skip = 4, select = 1:2)
smi_daily[, DATE := as.Date(DATE, format = "%d.%m.%Y")]
setnames(smi_daily, c("Date", "SMI"))
smi_daily <- smi_daily[order(Date)]
smi_daily[, smi_ret := SMI / shift(SMI) - 1]
smi_d_mean <- mean(smi_daily[Date < as.Date("2020-01-01"), smi_ret], na.rm = TRUE)
smi_d_sd   <- sd(smi_daily[Date < as.Date("2020-01-01"), smi_ret], na.rm = TRUE)
smi_daily[, hv_day := abs(smi_ret) > smi_d_mean + 3 * smi_d_sd]

# Load raw contacts at daily granularity (date_contact = actual contact date, not EOM)
contacts <- readRDS(paste0(emp_dir2, "T20_Kundenkontakte.rds"))
contacts[, date_contact := as.Date(as_factor(Kontakt_DT))]
contacts[, K_Aufnahme   := as_factor(K_Aufnahme)]
contacts[, invest_init_by_adv    := ifelse(K_Aufnahme %in% c("Durch Kundenberater", "Zentral") &
                                             K_Marketing_Kampagne == 0, K_Anlegen, 0L)]
contacts[, invest_init_by_client := ifelse(K_Aufnahme %in% c("Durch Bevollmächtigten", "Durch Kunde") &
                                             K_Marketing_Kampagne == 0, K_Anlegen, 0L)]

contacts_daily <- contacts[, .(
  adv      = sum(invest_init_by_adv,    na.rm = TRUE),
  client   = sum(invest_init_by_client, na.rm = TRUE),
  n_invest = sum(K_Anlegen,             na.rm = TRUE)
), by = date_contact]
setorderv(contacts_daily, "date_contact")
rm(contacts); gc()

ggplot(contacts_daily, aes(x = date_contact)) +
  geom_vline(data = smi_daily[Date >= min(contacts_daily$date_contact) & hv_day == TRUE],
             aes(xintercept = Date), color = "darkgreen", linewidth = 0.8, alpha = 0.4) +
  geom_line(aes(y = adv,    color = "Advisor-initiated"), linewidth = 0.8) +
  geom_line(aes(y = client, color = "Client-initiated"),  linewidth = 0.8) +
  scale_color_manual(values = c("Advisor-initiated" = "#2166ac", "Client-initiated" = "#d6604d")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0)) +
  labs(x = NULL, y = "Daily investment contacts", color = NULL,
       title   = "Daily investment contacts by initiating party",
       caption = "Vertical lines = daily |SMI return| > mean + 3 SD (pre-2020 baseline).") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

# OLS: are contacts higher in high-vol months?
summary(lm(n_invest_init_by_adv    ~ above_15sd, data = client_month))
summary(lm(n_invest_init_by_client ~ above_15sd, data = client_month))

# ==============================================================================
# ANALYSIS 5: Performance after advice in high-vol months
# "advised" = had at least one advisor-initiated invest contact in the event month
# Compare returns in the following month for advised vs. non-advised clients
# All event-study logic (high-vol dates, next months, advised flags) runs in
# DuckDB via CTEs — posc is scanned once; only the result comes into R
# ==============================================================================

perf_events <- setDT(dbGetQuery(con, "
  WITH high_vol AS (
    SELECT DISTINCT Date AS event_dt
    FROM posc
    WHERE above_15sd = 1
  ),
  event_dates AS (
    SELECT event_dt, event_dt AS Date, 0 AS rel_month FROM high_vol
    UNION ALL
    SELECT event_dt, (event_dt + INTERVAL '1 month')::DATE AS Date, 1 AS rel_month FROM high_vol
  ),
  hv_advised AS (
    SELECT DISTINCT p.Bp_ID, h.event_dt
    FROM posc p
    JOIN high_vol h ON p.Date = h.event_dt
    WHERE p.n_invest_init_by_adv > 0
  )
  SELECT
    p.Bp_ID,
    ed.event_dt,
    ed.rel_month,
    (ha.Bp_ID IS NOT NULL)                   AS advised,
    \"DA_Titelkursabweichung_CHF.x\"         AS \"DA_Titelkursabweichung_CHF.x\",
    \"Geschaeftsvolumen_CHF.x\"              AS \"Geschaeftsvolumen_CHF.x\"
  FROM posc p
  JOIN event_dates ed ON p.Date = ed.Date
  LEFT JOIN hv_advised ha ON p.Bp_ID = ha.Bp_ID AND ed.event_dt = ha.event_dt
  WHERE \"DA_Titelkursabweichung_CHF.x\" != 0
"))

perf_events[, .(ret = mean(`DA_Titelkursabweichung_CHF.x` / `Geschaeftsvolumen_CHF.x`, na.rm = TRUE),
                n   = uniqueN(Bp_ID)),
            by = .(rel_month, advised)]

dbDisconnect(con)

# ==============================================================================
# BUILD (run once to create ../data/posc.parquet)
# Contacts are processed in R (factor decoding requires haven); the large JOIN
# runs entirely in DuckDB so T30_Pos_fact never enters R memory.
# Requires: T30_Pos_fact.parquet, T20_Kundenkontakte.rds, agg_pos_smi.rds
# ==============================================================================

# -- Step 1: contacts — process in R, save as parquet, then free memory -------

# One-time .dta -> .rds conversion:
# contacts_raw <- as.data.table(read_dta(paste0(emp_dir2, "T20_Kundenkontakte.dta")))
# saveRDS(contacts_raw, paste0(emp_dir2, "T20_Kundenkontakte.rds"))

contacts <- readRDS(paste0(emp_dir2, "T20_Kundenkontakte.rds"))
contacts[, date_contact := as.Date(as_factor(Kontakt_DT))]
contacts[, K_Aufnahme   := as_factor(K_Aufnahme)]
contacts[, K_Art        := as_factor(K_Art)]
contacts[, month        := ceiling_date(date_contact, "month") - 1]

# Sanity checks
contacts[, sum_check := rowSums(.SD, na.rm = TRUE), .SDcols = c_cols]
summary(contacts$sum_check)
contacts[, mkt_check := K_Anlegen + K_Marketing_Kampagne]
summary(contacts$mkt_check); sum(contacts$mkt_check > 1)

# Investment contacts split by who initiated (excluding marketing-driven)
contacts[, invest_init_by_adv    := ifelse(K_Aufnahme %in% c("Durch Kundenberater", "Zentral") &
                                             K_Marketing_Kampagne == 0, K_Anlegen, 0)]
contacts[, invest_init_by_client := ifelse(K_Aufnahme %in% c("Durch Bevollmächtigten", "Durch Kunde") &
                                             K_Marketing_Kampagne == 0, K_Anlegen, 0)]


contacts_n_per_date <- contacts[,.N,by=date_contact]
setorderv(contacts_n_per_date,"date_contact")
ggplot(contacts_n_per_date,aes(x=date_contact,y=N))+geom_line()



# Collapse to Bp_ID x month
contacts_monthly_build <- contacts[, c(
  list(
    n_contacts               = .N,
    n_invest                 = sum(K_Anlegen, na.rm = TRUE),
    n_finance                = sum(K_Finanzieren, na.rm = TRUE),
    n_init_by_adv            = sum(K_Aufnahme %in% c("Durch Kundenberater", "Zentral")),
    n_init_by_client         = sum(K_Aufnahme %in% c("Durch Bevollmächtigten", "Durch Kunde")),
    n_invest_init_by_adv     = sum(invest_init_by_adv, na.rm = TRUE),
    n_invest_init_by_client  = sum(invest_init_by_client, na.rm = TRUE)
  ),
  lapply(.SD, sum, na.rm = TRUE)
), by = .(Bp_ID, month), .SDcols = c_cols]
setorderv(contacts_monthly_build, c("Bp_ID", "month"))

# Save and free — contacts no longer needed in R
write_parquet(contacts_monthly_build, "../data/contacts_monthly.parquet", compression = "zstd")
rm(contacts, contacts_monthly_build); gc()

# -- Step 2: DuckDB join — T30_Pos_fact streamed from parquet, never in R -----
# agg_pos_smi is small so we load it into R and register it as a DuckDB table.
# The COPY writes posc.parquet directly; no intermediate R object is created.
# COALESCE handles months where a client had no contact (replaces setnafill).
# Period_ID -> Date conversion uses LAST_DAY(STRPTIME(...)) inside the query.

# agg_pos_smi <- readRDS("../data/agg_pos_smi.rds")
#
# con_build <- dbConnect(duckdb(), dbdir = ":memory:")
# duckdb_register(con_build, "agg_pos_smi", agg_pos_smi)
# rm(agg_pos_smi); gc()
#
# dbExecute(con_build, sprintf("
#   COPY (
#     WITH pos_dated AS (
#       SELECT *,
#         LAST_DAY(STRPTIME(CAST(Period_ID AS VARCHAR), '%%Y%%m')) AS Date
#       FROM read_parquet('%sT30_Pos_fact.parquet')
#     )
#     SELECT
#       p.Bp_ID,
#       p.Date,
#       p.Instrumentengruppe,
#       p.DA_Titelkursabweichung_CHF          AS \"DA_Titelkursabweichung_CHF.x\",
#       p.DA_Devisenkursabweichung_CHF        AS \"DA_Devisenkursabweichung_CHF.x\",
#       p.Geschaeftsvolumen_CHF               AS \"Geschaeftsvolumen_CHF.x\",
#       COALESCE(c.n_contacts, 0)                      AS n_contacts,
#       COALESCE(c.n_invest, 0)                        AS n_invest,
#       COALESCE(c.n_finance, 0)                       AS n_finance,
#       COALESCE(c.n_init_by_adv, 0)                   AS n_init_by_adv,
#       COALESCE(c.n_init_by_client, 0)                AS n_init_by_client,
#       COALESCE(c.n_invest_init_by_adv, 0)            AS n_invest_init_by_adv,
#       COALESCE(c.n_invest_init_by_client, 0)         AS n_invest_init_by_client,
#       COALESCE(c.K_Physisch, 0)                      AS K_Physisch,
#       COALESCE(c.K_Reklamation, 0)                   AS K_Reklamation,
#       COALESCE(c.K_Umfassendes_Beratungsgespraech, 0) AS K_Umfassendes_Beratungsgespraech,
#       COALESCE(c.K_Anlegen, 0)                       AS K_Anlegen,
#       COALESCE(c.K_Finanzieren, 0)                   AS K_Finanzieren,
#       COALESCE(c.K_Vorsorge, 0)                      AS K_Vorsorge,
#       COALESCE(c.K_Basisdienstleistung, 0)           AS K_Basisdienstleistung,
#       COALESCE(c.K_Steuern, 0)                       AS K_Steuern,
#       COALESCE(c.K_Nachlassplanung, 0)               AS K_Nachlassplanung,
#       COALESCE(c.K_Pensionierung, 0)                 AS K_Pensionierung,
#       COALESCE(c.K_Nachfolgeregelung, 0)             AS K_Nachfolgeregelung,
#       COALESCE(c.K_Kundenzufriedenheit, 0)           AS K_Kundenzufriedenheit,
#       COALESCE(c.K_Marketing_Kampagne, 0)            AS K_Marketing_Kampagne,
#       COALESCE(c.K_Syst_Geschaeftsmoeglichkeit, 0)  AS K_Syst_Geschaeftsmoeglichkeit,
#       COALESCE(c.K_Performancebesprechung, 0)        AS K_Performancebesprechung,
#       COALESCE(c.K_Bilanzbesprechung, 0)             AS K_Bilanzbesprechung,
#       COALESCE(c.K_Compliance, 0)                    AS K_Compliance,
#       a.chf_return,
#       a.smi_return,
#       a.above_15sd,
#       a.d_vol,
#       a.d_log_vol
#     FROM pos_dated p
#     LEFT JOIN read_parquet('../data/contacts_monthly.parquet') c
#       ON p.Bp_ID = c.Bp_ID AND p.Date = c.month
#     LEFT JOIN agg_pos_smi a
#       ON p.Date = a.Date
#   ) TO '../data/posc.parquet' (FORMAT PARQUET, COMPRESSION ZSTD)
# ", emp_dir2))
#
# dbDisconnect(con_build)
