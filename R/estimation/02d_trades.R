## =============================================================================
## 02d_trades.R -- trade level data
##
## Source: data/boerse.parquet (2.63 m trades, 2009-2024, 49 columns).
##
## Direction is taken from the SIGN OF Menge, not from Order_Type: the sign is
## mechanical and agrees with Order_Type for every large category (Kauf and
## Zeichnung are 0% negative, Verkauf and Rueckzahlung are 100% negative), while
## a handful of oddly named types ("Verkauf CT", "Kauf (Closing) CT", 20 rows in
## total) contradict their own label.
##
## Unit price is Bruttowert_CHF / |Menge|, i.e. in CHF, so it is on the same
## footing as the implied prices in 02c. Kurs_Ausfuehrung is in Handelswaehrung
## and is NOT comparable to them.
##
## THREE FIELDS THAT WERE IN THE FILE ALL ALONG AND WERE NOT BEING READ
## --------------------------------------------------------------------
##  Medium -- the ORDER CHANNEL. This is the important one. It says who
##    physically entered the order: the client through e-banking, the advisor by
##    phone / in a meeting / by letter, or the discretionary mandate desk. Until
##    now every sell counted the same, so "P(sell)" pooled a client panicking at
##    3 a.m. with a portfolio manager rebalancing a mandate. Around Covid the two
##    move in opposite directions:
##        month     e-banking sells   mandate sells
##        2020-01        1'510            2'752
##        2020-03        4'275            2'662     <- the crash month
##        2020-04        1'812            5'123     <- after the trough
##    Self-directed selling nearly triples IN the crash; the mandate desk sells
##    AFTER it, which is rebalancing, not panic. Pooled, these cancel.
##
##  Kosten -- the commission actually charged, on every trade, never missing.
##    CHF 153.9 m over 2011-2024. 07 uses it as a direct transaction-revenue
##    measure; pos_m's Profit_* columns are the only other revenue field and they
##    are annual client aggregates with no product detail.
##
##  Order_Type -- was read but only printed as a sanity check. It separates the
##    exchange leg (Kauf/Verkauf, median commission CHF 25) from the fund leg
##    (Zeichnung/Rueckzahlung, 45-49% zero commission). Checked and NOT treated
##    as passive: 99.8% of the Rueckzahlung rows with a known instrument group
##    are Fonds, none redeem at par, and none fall in their own maturity year, so
##    they are fund redemptions -- a real decision to sell -- not bond maturities.
##    What IS passive is Medium == "Sec Event" (corporate actions posted through
##    the trade table) and the Titeleingang / Vorauszahlung subscription types.
##
## Output: results/cache/trades_d.parquet  (trade level)
##         results/cache/trades_m.parquet  (Bp_ID x MDate)
## =============================================================================

source("00_setup.R")
log_init("02d_trades")

BOERSE <- file.path(DATA, "boerse.parquet")
if (!file.exists(BOERSE)) stop("not found: ", BOERSE, call. = FALSE)

NEED <- c("DDate","Bp_ID","Asset_ID","Menge","Bruttowert","Bruttowert_CHF",
          "Nettowert_CHF","Kosten","Order_Type","Medium","ISIN_Key")
tr <- setDT(read_parquet(BOERSE, col_select = all_of(NEED)))
require_cols(tr, NEED, "02d_trades")
log_step("raw trades", tr)

sink(file.path(RESULTS, "02d_trades.txt"), split = TRUE)
on.exit(sink(), add = TRUE)

tr[, DDate := as.Date(DDate)]
tr[, `:=`(Order_Type = as.character(Order_Type), Medium = as.character(Medium))]

## the file contains a dozen impossible dates (years 2103, 2104, 2999)
bad <- tr[is.na(DDate) | DDate < as.Date("2005-01-01") | DDate > P$smp_end, .N]
log_step(sprintf("dropped %d trades with an impossible or out-of-sample date", bad))
tr <- tr[!is.na(DDate) & DDate >= as.Date("2005-01-01") & DDate <= P$smp_end]

tr <- tr[!is.na(Menge) & Menge != 0 & !is.na(Bruttowert_CHF)]
tr[, `:=`(
  MDate   = eom(DDate),
  sell    = as.integer(Menge < 0),
  qty     = abs(Menge),
  chf     = abs(Bruttowert_CHF)
)]
tr[, px_chf := chf / qty]
tr <- tr[is.finite(px_chf) & px_chf > 0]
log_step("clean trades", tr)

## ---------------------------------------------------------------------------
## DROP THE FUND-LAUNCH CAMPAIGN (P$ev_fund_launch_*, see 00_setup)
##
## In 2017-04 the bank launched its own "Strategie Einkommen" fund. Trades in
## that asset run 0, 0, 1, 1259, 105, 76 over 2017-01..06: 1210 clients buy
## CHF 71.5 m of it in the launch month, 23% of all buying that month. It is a
## marketing campaign, not a portfolio decision, and it is what made 2017-04 the
## fattest-tailed flow month in the sample (p99 0.794 vs a median month 0.197).
##
## The trades are dropped here, and the affected client-months are written to
## cache so 02_clean can also neutralise the pos_m FLOW field -- that field is a
## position quantity change, not a trade, so dropping rows here does not reach it.
## ---------------------------------------------------------------------------
launch <- tr[MDate == P$ev_fund_launch_month & Asset_ID == P$ev_fund_launch_asset]
if (nrow(launch)) {
  log_step(sprintf("fund launch %s asset %s: dropped %s trades, %s clients, CHF %.1f m",
                   format(P$ev_fund_launch_month), P$ev_fund_launch_asset,
                   format(nrow(launch), big.mark = "'"),
                   format(uniqueN(launch$Bp_ID), big.mark = "'"),
                   sum(launch$chf) / 1e6))
  save_dt(unique(launch[, .(Bp_ID, MDate)]), "event_fund_launch")
  tr <- tr[!(MDate == P$ev_fund_launch_month & Asset_ID == P$ev_fund_launch_asset)]
  log_step("trades after removing the launch campaign", tr)
} else {
  save_dt(data.table(Bp_ID = numeric(0), MDate = as.Date(character(0))),
          "event_fund_launch")
  log_step("fund-launch asset not found in the trade file; nothing dropped")
}

## ---------------------------------------------------------------------------
## 1. order route, channel, and whether the booking is a decision at all
## ---------------------------------------------------------------------------
## "Kauf" is matched case-sensitively so it does not swallow "Verkauf"; the
## umlaut in "Rueckzahlung" is matched on its ASCII tail so the classification
## does not depend on the locale the file is read in.
tr[, route := fcase(
  grepl("Kauf|Verkauf",        Order_Type), "exchange",
  grepl("Zeichnung|ckzahlung", Order_Type), "fund",
  default =                                 "other")]

tr[, chan := fcase(
  Medium %chin% P$chan_self,    "self",      # client entered it themselves
  Medium %chin% P$chan_advisor, "advisor",   # advisor entered it for the client
  Medium %chin% P$chan_mandate, "mandate",   # discretionary mandate desk
  default =                     "other")]

## a corporate action or a securities transfer-in is booked as a trade but is
## nobody's decision to trade, so it must not enter a behavioural outcome
tr[, decision := as.integer(!(Medium %chin% P$chan_passive |
                                grepl(P$otype_passive, Order_Type)))]

## ---------------------------------------------------------------------------
## 2. commission, converted to CHF
##    Kosten is denominated in Handelswaehrung, exactly like Bruttowert, so the
##    Bruttowert_CHF / Bruttowert ratio is the trade's own FX rate. Kosten is
##    booked negative (a charge); it is carried here as a positive amount.
## ---------------------------------------------------------------------------
tr[, fx := fifelse(is.finite(Bruttowert) & Bruttowert != 0,
                   Bruttowert_CHF / Bruttowert, NA_real_)]
tr[, kosten_chf := abs(Kosten * fx)]
tr[!is.finite(kosten_chf), kosten_chf := NA_real_]

## ---------------------------------------------------------------------------
## 3. report
## ---------------------------------------------------------------------------
cat("\n-- direction vs. Order_Type (sanity) --\n")
print(tr[, .N, by = .(Order_Type, sell)][order(-N)][1:12])

cat("\n-- trades per year --\n")
print(tr[, .(trades = .N, clients = uniqueN(Bp_ID), sells = sum(sell),
             chf_bn = round(sum(chf) / 1e9, 2),
             kosten_mn = round(sum(kosten_chf, na.rm = TRUE) / 1e6, 2)),
         by = .(y = year(DDate))][order(y)])

cat("\n-- ORDER CHANNEL (Medium), the field that was not being read --\n")
print(tr[, .(trades = .N, share = round(.N / nrow(tr), 4),
             pct_sell = round(mean(sell), 3),
             med_chf  = round(median(chf)),
             med_kosten = round(median(kosten_chf, na.rm = TRUE), 2)),
         by = chan][order(-trades)])

cat("\n-- channel availability by year (share of trades) --\n")
print(dcast(tr[, .N, by = .(y = year(DDate), chan)], y ~ chan,
            value.var = "N", fill = 0L)[order(y)])

cat("\n-- SELLS by channel, monthly, around the Covid crash --\n")
cat("   (dd window 2020-02-19 .. 2020-03-23)\n")
print(dcast(tr[sell == 1L & decision == 1L &
                 DDate %between% as.Date(c("2019-10-01","2020-09-30")),
               .N, by = .(m = format(DDate, "%Y-%m"), chan)],
            m ~ chan, value.var = "N", fill = 0L)[order(m)])
cat("Self-directed sells spike IN the crash month; mandate sells spike AFTER the\n")
cat("trough. A pooled P(sell) averages these two opposite movements away.\n")

cat("\n-- order route: exchange leg vs. fund leg --\n")
print(tr[, .(trades = .N, pct_sell = round(mean(sell), 3),
             pct_zero_cost = round(mean(kosten_chf == 0, na.rm = TRUE), 3),
             med_kosten = round(median(kosten_chf, na.rm = TRUE), 2)),
         by = route][order(-trades)])

cat("\n-- passive bookings dropped from the decision measures --\n")
print(tr[decision == 0L, .N, by = .(Medium, Order_Type)][order(-N)][1:6])
cat("total passive: ", tr[, sum(decision == 0L)], " of ", nrow(tr),
    sprintf(" (%.3f)\n", tr[, mean(decision == 0L)]), sep = "")

cat("\n-- commission (Kosten) --\n")
print(tr[, .(n_missing = sum(is.na(kosten_chf)),
             pct_zero  = round(mean(kosten_chf == 0, na.rm = TRUE), 3),
             median    = round(median(kosten_chf, na.rm = TRUE), 2),
             mean      = round(mean(kosten_chf, na.rm = TRUE), 2),
             total_mn  = round(sum(kosten_chf, na.rm = TRUE) / 1e6, 1))])
cat("as a share of trade value: ")
print(round(quantile(tr[, kosten_chf / pmax(chf, 1)], c(.25,.5,.75,.9),
                     na.rm = TRUE), 4))

save_dt(tr[, .(Bp_ID, Asset_ID, DDate, MDate, sell, qty, chf, px_chf, ISIN_Key,
               route, chan, decision, kosten_chf)],
        "trades_d")

## ---------------------------------------------------------------------------
## 4. client x month aggregates
##    The *_dec columns exclude passive bookings; the channel columns split the
##    decision sells by who entered the order. n_sells / chf_sold are kept
##    unchanged so every number produced before this revision still reproduces.
## ---------------------------------------------------------------------------
tm <- tr[, .(n_trades_d  = .N,
             n_sells     = sum(sell),
             n_buys      = sum(sell == 0L),
             chf_sold    = sum(chf[sell == 1L]),
             chf_bought  = sum(chf[sell == 0L]),
             n_assets_traded = uniqueN(Asset_ID),
             ## --- decisions only (no corporate actions / transfers in) -------
             n_sells_dec  = sum(sell == 1L & decision == 1L),
             chf_sold_dec = sum(chf[sell == 1L & decision == 1L]),
             n_buys_dec   = sum(sell == 0L & decision == 1L),
             chf_bought_dec = sum(chf[sell == 0L & decision == 1L]),
             ## --- decision sells by ORDER CHANNEL ----------------------------
             n_sells_self = sum(sell == 1L & decision == 1L & chan == "self"),
             chf_sold_self= sum(chf[sell == 1L & decision == 1L & chan == "self"]),
             n_sells_adv  = sum(sell == 1L & decision == 1L & chan == "advisor"),
             chf_sold_adv = sum(chf[sell == 1L & decision == 1L & chan == "advisor"]),
             n_sells_man  = sum(sell == 1L & decision == 1L & chan == "mandate"),
             chf_sold_man = sum(chf[sell == 1L & decision == 1L & chan == "mandate"]),
             ## --- the self-directed buy leg, for the symmetric outcome -------
             n_buys_self  = sum(sell == 0L & decision == 1L & chan == "self"),
             chf_bought_self = sum(chf[sell == 0L & decision == 1L & chan == "self"]),
             ## --- commission -------------------------------------------------
             kosten_chf      = sum(kosten_chf, na.rm = TRUE),
             kosten_sell_chf = sum(kosten_chf[sell == 1L], na.rm = TRUE)),
         by = .(Bp_ID, MDate)]
tm[, chf_net := chf_bought - chf_sold]
save_dt(tm, "trades_m")
log_step("client-month trade file", tm)

cat("\n-- client-months with a sell, by channel --\n")
print(tm[, .(any_sell = round(mean(n_sells     > 0), 4),
             decision = round(mean(n_sells_dec > 0), 4),
             self     = round(mean(n_sells_self > 0), 4),
             advisor  = round(mean(n_sells_adv  > 0), 4),
             mandate  = round(mean(n_sells_man  > 0), 4))])
sink()
