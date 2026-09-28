## =============================================================================
## stress_events.R -- the hand-dated SMI/VIX stress episodes
##
## Single source of truth for the event windows. Sourced by
##   03_episodes.R          (builds the estimation episodes from it)
##   episodes_smi_vix.R     (draws the annotated SMI/VIX figure from it)
## so the figure in the paper and the episodes in the regressions can never
## drift apart.
##
## start / end : the daily stress window, read off the SMI and the VIX.
## label       : figure annotation.
## row / hjust : label stacking level and horizontal justification (figure only).
##
## Defines EVENTS and nothing else; it touches no data and sources no file.
## =============================================================================

EVENTS <- data.table::data.table(
  start = as.Date(c(
    "2011-07-25", "2011-09-01", "2013-05-20", "2015-01-10", "2015-08-10",
    "2015-12-01", "2016-01-04", "2016-06-01", "2016-10-10", "2018-01-20",
    "2018-11-20", "2019-05-01", "2020-02-20", "2022-01-03", "2022-02-21",
    "2022-04-20", "2022-08-15", "2023-03-01", "2023-10-07", "2024-07-25")),
  end = as.Date(c(
    "2011-10-10", "2011-09-20", "2013-06-30", "2015-02-05", "2015-10-01",
    "2015-12-31", "2016-02-20", "2016-07-15", "2016-12-15", "2018-03-30",
    "2019-01-10", "2019-06-30", "2020-04-01", "2022-02-18", "2022-03-31",
    "2022-06-25", "2022-10-05", "2023-04-05", "2023-11-03", "2024-08-20")),
  label = c(
    "2011 Aug:\nUS downgrade,\nEuro crisis",
    "2011 Sep:\nSNB floor intro.",
    "2013 May/Jun:\nTaper Tantrum",
    "2015 Jan:\nSNB floor aband.",
    "2015 Aug:\nChina flash crash",
    "2015 Dec:\nFED rate hike",
    "2016 Jan/Feb:\nChina & oil selloff",
    "2016 Jun:\nBrexit vote",
    "2016 Nov:\nTrump election",
    "2018 Feb:\nVolmageddon",
    "2018 Dec:\nXmas Eve plunge",
    "2019 May/Jun:\nUS-China trade war",
    "2020 Mar:\nCOVID crash",
    "2022 Jan:\nRising inflation",
    "2022 Feb:\nRusso-Ukrainian war",
    "2022 May:\nSupply chain",
    "2022 Aug:\nHawkish FED",
    "2023 Mar:\nSVB & Credit Suisse",
    "2023 Oct:\nGeopolitics,\nrising rates",
    "2024 Aug:\nYen carry unwind"),
  ## short tag used to name the merged episodes in the episode table
  tag = c(
    "us_downgrade", "snb_floor_in", "taper_tantrum", "snb_floor_out",
    "china_crash", "fed_hike", "china_oil", "brexit", "trump",
    "volmageddon", "xmas_plunge", "trade_war", "covid",
    "inflation", "ukraine", "supply_chain", "hawkish_fed",
    "svb_cs", "geopol_rates", "yen_carry"),
  row   = c(1, 3, 1, 2, 4, 3, 5, 1, 2, 4, 1, 3, 2, 5, 4, 3, 2, 1, 3, 5),
  hjust = c(0, 0, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5, .5, 1)
)
data.table::setorder(EVENTS, start)
