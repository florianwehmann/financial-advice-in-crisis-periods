## =============================================================================
## 02_clean.R -- build the clean client-month panel
##
## Output: results/cache/panel.parquet  (one row per Bp_ID x MDate on an
##         explicit, gap-free monthly grid)
## =============================================================================

source("00_setup.R")
log_init("02_clean")

KEEP <- c("Bp_ID", "MDate", "vol", "dprice", "dtotal", "dfx", "buysell", "sell", "buy",
          "wealth", "n_assets", "n_trades",
          "n_trades_adv_inv", "n_trades_adv_perf", "n_trades_adv_inv_perf",
          "n_trades_adv_init_a", "n_trades_adv_init_c",
          "n_meeting", "n_phone", "n_mail", "n_physical",
          "n_contacts_investment", "n_contacts_performance",
          "Hauptbetreuer_ID", "EVV", "Depotprodukt", "Anlagepaket", "main_bank",
          "MA_Kundensegment", "EBanking_YN")

pos <- setDT(read_parquet(POS_M_FILE, col_select = all_of(KEEP)))
require_cols(pos, KEEP, "02_clean")
log_step("raw pos_m", pos)

## ---------------------------------------------------------------------------
## 1. sample window
## ---------------------------------------------------------------------------
pos <- pos[MDate >= P$smp_start & MDate <= P$smp_end]
log_step(sprintf("window %s..%s", P$smp_start, P$smp_end), pos)

setorder(pos, Bp_ID, MDate)

## ---------------------------------------------------------------------------
## 2a. TRADE-derived advice indicators.
##     These are NOT the treatment any more -- they only fire when a trade
##     followed a contact within five days. They are kept under adt_* so the old
##     and the new measure can be compared directly (07 reports the difference).
##     Counts in pos_m are counts of position rows, not of contacts (see 01).
## ---------------------------------------------------------------------------
pos[, `:=`(
  adt_inv    = as.integer(n_trades_adv_inv  > 0),
  adt_perf   = as.integer(n_trades_adv_perf > 0),
  adt_any    = as.integer(n_trades_adv_inv > 0 | n_trades_adv_perf > 0),
  adt_init_a = as.integer(n_trades_adv_init_a > 0),
  adt_init_c = as.integer(n_trades_adv_init_c > 0),
  traded     = as.integer(buysell != 0 | sell != 0 | buy != 0)
)]

## ---------------------------------------------------------------------------
## 3. return: price return on beginning-of-month value
##    BoM value = vol - dtotal. Below P$min_bom_value the denominator is noise.
## ---------------------------------------------------------------------------
pos[, bom := vol - dtotal]
pos[, pf_ret := fifelse(bom >= P$min_bom_value, dprice / bom, NA_real_)]
log_step(sprintf("return NA (BoM < %s CHF or missing): %s",
                 P$min_bom_value, format(pos[is.na(pf_ret), .N], big.mark = "'")))

## winsorize WITHIN calendar month, before any compounding. Pooling months would
## clip legitimate crisis returns (the global 1%-quantile is about -10%).
pos[, pf_ret := winsor(pf_ret), by = MDate]

## ---------------------------------------------------------------------------
## 4. status flags carried at client level
## ---------------------------------------------------------------------------
pos[, discretionary := as.integer(Depotprodukt %chin% "Vermögensverwaltungsdepot" |
                                    (!is.na(EVV) & EVV == 1))]
pos[, main_bank_yn := as.integer(main_bank %chin% "Ja")]

## ---------------------------------------------------------------------------
## 5. explicit client-month grid
##    From each client's first observed month to the end of the sample, so that
##    a client who liquidates stays in the panel at zero holdings instead of
##    silently dropping out, and so that shift() moves calendar months.
## ---------------------------------------------------------------------------
months <- sort(unique(pos$MDate))
first_obs <- pos[, .(first = min(MDate), last_obs = max(MDate)), by = Bp_ID]

grid <- first_obs[, .(MDate = months[months >= first]), by = .(Bp_ID, first, last_obs)]
log_step("client-month grid (first obs .. sample end)", grid)

pan <- pos[grid, on = .(Bp_ID, MDate)]
setorder(pan, Bp_ID, MDate)

pan[, observed := as.integer(!is.na(vol))]
pan[, status := fcase(observed == 1L,            "observed",
                      MDate > last_obs,          "post_exit",
                      default =                  "interior_gap")]
log_step("panel rows by status:")
log_step(paste(capture.output(print(pan[, .N, by = status])), collapse = "\n"))

## unobserved months = no risky holdings observed => zero holdings, zero return,
## zero flow, zero advice. Flagged by `status` so every spec can drop them.
## NOTE: the contact flags are NOT zeroed here. A month with no position row can
## still carry a real advisor contact, and that is exactly the case the old
## trade-based measure was blind to. They were already zero-filled above for
## client-months with no contact record.
zero_num <- c("vol","dprice","dtotal","dfx","buysell","sell","buy","wealth","bom",
              "n_assets","n_trades","n_trades_adv_inv","n_trades_adv_perf",
              "n_trades_adv_inv_perf","n_trades_adv_init_a","n_trades_adv_init_c",
              "n_meeting","n_phone","n_mail","n_physical",
              "n_contacts_investment","n_contacts_performance",
              "adt_inv","adt_perf","adt_any","adt_init_a","adt_init_c","traded")
for (v in zero_num) set(pan, which(pan$observed == 0L), v, 0)
pan[observed == 0L, pf_ret := 0]

## ---------------------------------------------------------------------------
## 5b. the CONTACT log (built by 02b_contacts.R) -- this carries the treatment.
##     Merged AFTER the grid is completed on purpose: a client with no position
##     row in a month can still have had a real advisor contact, and that is
##     precisely the case the old trade-based measure was blind to.
## ---------------------------------------------------------------------------
## NOTE on the join style used here and in 5c/5d/5e: every one of these is an
## UPDATE JOIN (pan[x, on = , (cols) := ...]) rather than the right join
## `pan <- x[pan, on = ]`. Both give the same result -- the key is unique in each
## of contacts_m / portfolio_m / cash_m / trades_m, and the unmatched rows are
## zero-filled immediately afterwards either way -- but the right join copies all
## ~90 columns of a 5.9 m-row table each time. Four of those copies is several
## gigabytes of peak memory, and the script now runs on a 16 GB machine.
cm <- load_dt("contacts_m")
CMV <- setdiff(names(cm), c("Bp_ID","MDate"))
pan[cm, on = .(Bp_ID, MDate), (CMV) := mget(paste0("i.", CMV))]
CFLAGS <- grep("^c_", CMV, value = TRUE)
for (v in c(CFLAGS, "n_contacts", "n_perf", "n_inv")) pan[is.na(get(v)), (v) := 0L]
rm(cm); gc(verbose = FALSE)
setorder(pan, Bp_ID, MDate)

log_step(sprintf("client-months with an advice contact: %s (%.4f of grid)",
                 format(pan[c_advice == 1L, .N], big.mark = "'"),
                 pan[, mean(c_advice == 1L)]))
log_step(sprintf("  ... with no trade booked that month: %.3f",
                 pan[c_advice == 1L, mean(traded == 0L)]))
log_step(sprintf("  ... the old trade-based measure caught: %.3f of them",
                 pan[c_advice == 1L, mean(adt_any == 1L)]))
log_step(sprintf("  ... in a month with no position row at all: %.3f",
                 pan[c_advice == 1L, mean(observed == 0L)]))

## ---------------------------------------------------------------------------
## 5c. PORTFOLIO COMPOSITION (02c, from pos_full) and TRADES (02d, from boerse)
##     pos_full covers every instrument group, so wealth_full is the whole
##     securities portfolio and eq_share is a real equity weight -- neither was
##     available from pos_m. There is still no cash account anywhere in the
##     data, so a client who sells out shows eq_share -> 0 and wealth_full -> 0
##     and the proceeds are simply not observed.
## ---------------------------------------------------------------------------
pfm <- load_dt("portfolio_m")[, .(Bp_ID, MDate, wealth_full = tot_value,
                                  v_equity, v_bond, eq_share, bond_share, risky_share,
                                  n_assets_all)]
PFV <- setdiff(names(pfm), c("Bp_ID","MDate"))
pan[pfm, on = .(Bp_ID, MDate), (PFV) := mget(paste0("i.", PFV))]
rm(pfm); gc(verbose = FALSE)
## no position row anywhere = no securities = no equity
pan[is.na(wealth_full), `:=`(wealth_full = 0, v_equity = 0, v_bond = 0, n_assets_all = 0L)]
pan[is.na(eq_share) & wealth_full == 0, `:=`(eq_share = 0, bond_share = 0, risky_share = 0)]

## ---------------------------------------------------------------------------
## 5d. DEPOSIT BALANCES (02e, from the unfiltered pos_merged)
##     This is what makes risky_share a real weight and separates "sold and sat
##     in cash" from "left the bank". 99.2% of client-months with no securities
##     still have a cash account, so nearly every apparent exit is a client who
##     de-risked, not a client who left.
## ---------------------------------------------------------------------------
csh <- load_dt("cash_m")
CSV <- setdiff(names(csh), c("Bp_ID","MDate"))
pan[csh, on = .(Bp_ID, MDate), (CSV) := mget(paste0("i.", CSV))]
rm(csh); gc(verbose = FALSE)
for (v in c("cash_free","cash_locked","cash_time","cash_total","n_cash_acc"))
  pan[is.na(get(v)), (v) := 0]
pan[, has_cash := as.integer(cash_total > 0)]

## total wealth and the weights that matter
pan[, wealth_tot := wealth_full + cash_free]
pan[, `:=`(
  risky_share_c = fifelse(wealth_tot >= P$min_bom_value, wealth_full / wealth_tot, NA_real_),
  eq_share_c    = fifelse(wealth_tot >= P$min_bom_value, v_equity    / wealth_tot, NA_real_),
  cash_share    = fifelse(wealth_tot >= P$min_bom_value, cash_free   / wealth_tot, NA_real_)
)]

## an apparent exit is only a real exit if the bank relationship is gone too
pan[, left_bank := as.integer(observed == 0L & has_cash == 0L)]
pan[, in_cash   := as.integer(observed == 0L & has_cash == 1L)]

log_step(sprintf("client-months with no securities but a cash account: %s (%.4f of those)",
                 format(pan[observed == 0L & has_cash == 1L, .N], big.mark = "'"),
                 pan[observed == 0L, mean(has_cash == 1L)]))
log_step(sprintf("median risky share (observed months): %.3f",
                 pan[observed == 1L, median(risky_share_c, na.rm = TRUE)]))

## ---------------------------------------------------------------------------
## 5e. BOOKED TRADES (02d, from boerse), including the ORDER CHANNEL
##     `sold` from pos_m is a position QUANTITY change, so it also fires on
##     corporate actions, transfers and in-kind moves; `sold_d` is a trade that
##     was actually booked; `sold_self` is a trade the CLIENT entered through
##     e-banking. Those are three different questions and the last one is the
##     panic-selling measure the design is really after.
## ---------------------------------------------------------------------------
##     Only the columns the panel actually needs are pulled across, and the join
##     UPDATES pan by reference. A right join here copies all ~90 columns of a
##     5.9 m-row table, which on a 16 GB machine is the difference between
##     running and an out-of-memory abort. The rest of the trade detail stays in
##     trades_m and 05 joins it there, over the drawdown window only.
TRD0 <- c("n_trades_d","n_sells","n_buys","chf_sold","chf_bought","chf_net",
          "n_assets_traded",                                   # as before
          "n_sells_dec","n_sells_self","n_sells_adv","n_sells_man","n_buys_self",
          "chf_sold_self","kosten_chf")                        # new, from 02d
trm <- load_dt("trades_m")[, c("Bp_ID","MDate", TRD0), with = FALSE]
pan[trm, on = .(Bp_ID, MDate), (TRD0) := mget(paste0("i.", TRD0))]
for (v in TRD0) pan[is.na(get(v)), (v) := 0]
pan[, traded_d  := as.integer(n_trades_d   > 0)]
pan[, sold_d    := as.integer(n_sells      > 0)]
pan[, sold_dec  := as.integer(n_sells_dec  > 0)]   # a decision to sell
pan[, sold_self := as.integer(n_sells_self > 0)]   # ... entered by the client
pan[, sold_adv  := as.integer(n_sells_adv  > 0)]   # ... entered by the advisor
pan[, sold_man  := as.integer(n_sells_man  > 0)]   # ... by the mandate desk
pan[, bought_self := as.integer(n_buys_self > 0)]

## a client who never appears in boerse has a mechanical zero on every
## trade-based outcome; the flag lets 05/06 restrict to the covered universe
pan[, in_boerse := as.integer(any(n_trades_d > 0)), by = Bp_ID]
rm(trm); gc(verbose = FALSE)

log_step(sprintf("equity share available for %.3f of grid rows",
                 pan[, mean(!is.na(eq_share))]))
log_step(sprintf("mean equity share (observed months): %.3f",
                 pan[observed == 1L, mean(eq_share, na.rm = TRUE)]))
log_step(sprintf("trade-level months with a trade: %.4f (pos_m flow said %.4f)",
                 pan[, mean(traded_d == 1L)], pan[, mean(traded == 1L)]))
log_step(sprintf("months with a sell: flow %.4f | booked %.4f | decision %.4f | self-directed %.4f",
                 pan[, mean(sell < 0, na.rm = TRUE)], pan[, mean(sold_d == 1L)],
                 pan[, mean(sold_dec == 1L)], pan[, mean(sold_self == 1L)]))
log_step(sprintf("clients ever present in boerse: %.4f", pan[, mean(in_boerse == 1L)]))

setorder(pan, Bp_ID, MDate)

## client attributes: carry the last observed value forward through the gap
carry <- c("Hauptbetreuer_ID","EVV","Depotprodukt","Anlagepaket","main_bank",
           "MA_Kundensegment","EBanking_YN","discretionary","main_bank_yn")
pan[, (carry) := lapply(.SD, nafill_locf <- function(x) {
  i <- cumsum(!is.na(x)); i[i == 0L] <- NA_integer_; x[!is.na(x)][i]
}), by = Bp_ID, .SDcols = carry]

## ---------------------------------------------------------------------------
## 6. rolling returns on the complete grid (log -> frollsum -> expm1)
##    NA if any month in the window is missing.
## ---------------------------------------------------------------------------
pan[, ret_next12 := roll_ret_fwd(pf_ret, 12L), by = Bp_ID]
pan[, ret_past12 := roll_ret_bwd(pf_ret, 12L), by = Bp_ID]
pan[, ret_next6  := roll_ret_fwd(pf_ret,  6L), by = Bp_ID]

## ---------------------------------------------------------------------------
## 7. flows scaled by beginning-of-month value (the only trading measure pos_m
##    supports -- there is no asset level trade table here)
## ---------------------------------------------------------------------------
pan[, `:=`(
  netflow_r = fifelse(bom >= P$min_bom_value, buysell / bom, NA_real_),
  sell_r    = fifelse(bom >= P$min_bom_value, -sell   / bom, NA_real_),
  buy_r     = fifelse(bom >= P$min_bom_value,  buy    / bom, NA_real_)
)]
## the same ratio built from BOOKED TRADES the CLIENT entered, plus the
## commission the bank charged. The remaining trade detail is NOT carried on the
## panel: at 5.9 m rows another dozen double columns costs ~1 GB and 05 can join
## it straight from trades_m over the drawdown window instead.
pan[, `:=`(
  sellflow_s = fifelse(bom >= P$min_bom_value, chf_sold_self / bom, NA_real_),
  cost_r     = fifelse(bom >= P$min_bom_value, kosten_chf    / bom, NA_real_)
)]
FLOWV <- c("netflow_r","sell_r","buy_r","sellflow_s","cost_r")
for (v in FLOWV) pan[observed == 0L, (v) := 0]

## The FUND-LAUNCH campaign (2017-04, see 00_setup and 02d). Dropping the trades
## in 02d does not reach these ratios: netflow_r and sell_r/buy_r are built from
## the pos_m FLOW field, which is a position quantity change, not a trade. For
## the 1210 client-months that bought the launch fund the flow that month is a
## marketing campaign rather than a portfolio decision, and it cannot be netted
## out reliably (pos_m and boerse do not reconcile CHF for CHF). They are set to
## NA instead, so those client-months drop out of any flow regression rather
## than entering it distorted. 1210 of 5.9 m rows.
## 02d writes this; guard so 02_clean can still be run on its own
flf <- file.path(CACHE, "event_fund_launch.parquet")
fl  <- if (file.exists(flf)) load_dt("event_fund_launch") else data.table()
if (nrow(fl)) {
  pan[fl, on = .(Bp_ID, MDate), (FLOWV) := NA_real_]
  log_step(sprintf("fund-launch campaign: flow ratios set to NA for %s client-months",
                   format(nrow(fl), big.mark = "'")))
}
pan[, (FLOWV) := lapply(.SD, winsor), by = MDate, .SDcols = FLOWV]
## ... and then a POOLED cap on top. Winsorising a flow ratio within calendar
## month sets the cut at that month's own quantile, so a month in which the
## whole distribution shifts keeps its tail intact: 2017-04 has p99 = 0.794
## against a median month's 0.197 and 3.5% of clients moving more than 20% of
## their portfolio, against 0% in a normal month. That single month drove the
## step changes at rel_month -9 and +27 in the netflow event study, because it
## lands at a different rel_month in each block. The tighter of the two cuts
## binds, so an ordinary month (own p99 ~0.15) is untouched.
pan[, (FLOWV) := lapply(.SD, winsor, p = P$win_p_flow), .SDcols = FLOWV]
for (v in FLOWV) {
  q <- quantile(pan[[v]], P$win_p_flow, na.rm = TRUE)
  log_step(sprintf("%-11s pooled cap [%+.3f, %+.3f]", v, q[1], q[2]))
}

## the per-month counts and CHF amounts have done their job (the indicators and
## the two ratios above); 05 re-reads the detail from trades_m
pan[, c("n_sells_dec","n_sells_self","n_sells_adv","n_sells_man","n_buys_self",
        "chf_sold_self","kosten_chf") := NULL]
gc(verbose = FALSE)

pan[, log_w := log(pmax(wealth, 1))]

## changes in the portfolio weights, on the completed grid
pan[, d_eq_share    := eq_share      - shift(eq_share),      by = Bp_ID]
pan[, d_risky_share := risky_share_c - shift(risky_share_c), by = Bp_ID]

save_dt(pan, "panel")
log_step("panel written to results/cache/panel.parquet", pan)

cat("\n-- panel summary --\n")
print(pan[, .(rows = .N, clients = uniqueN(Bp_ID),
               mean_ret = mean(pf_ret, na.rm = TRUE),
               sd_ret = sd(pf_ret, na.rm = TRUE)), by = .(year = year(MDate))][order(year)])
