## =============================================================================
## 05_outcomes.R -- stacked event panel + the four outcome families
##
## IMPORTANT -- what pos_m does and does not allow:
##   The design asks for cf_gap built by freezing the holdings at the last
##   pre-drawdown month end and drifting them with observed SECURITY prices.
##   pos_m has no asset level panel, so that object cannot be built here.
##   Two labelled approximations are built instead:
##     cf_gap_mkt : realised wealth vs. pre-episode wealth drifted with the market
##                  index. PRIMARY. Contaminated by external cash in/outflows and
##                  by the client's allocation differing from the index.
##     cf_gap_own : realised wealth vs. pre-episode wealth drifted with the
##                  client's OWN monthly price return. This isolates the wealth
##                  effect of flows, but it stops drifting once a client is out of
##                  the market, so it understates the cost of selling.
##   Per-sale forgone return (design 4.4) needs trade and price level data and is
##   NOT built. 07_robustness prints this as an open item rather than faking it.
##
## Output: results/cache/stk.parquet   (client x episode x month)
##         results/cache/cle.parquet   (client x episode, outcomes appended)
## =============================================================================

source("00_setup.R")
log_init("05_outcomes")

pan <- load_dt("panel")
cle <- load_dt("cle")
ep  <- load_dt("episodes")[usable == TRUE]
idx <- load_dt("index_m")
setorder(pan, Bp_ID, MDate)

sink(file.path(RESULTS, "05_outcomes.txt"), split = TRUE)
on.exit(sink(), add = TRUE)

## ---------------------------------------------------------------------------
## 1. stack the episode windows
## ---------------------------------------------------------------------------

win <- cle[, .(Bp_ID, ep_id, pre_start, post_end)]

stk <- pan[win, on = .(Bp_ID, MDate >= pre_start, MDate <= post_end),
           .(Bp_ID, ep_id, MDate = x.MDate, observed, status, wealth, log_w, pf_ret,
             buysell, sell, dprice, dfx, buy, netflow_r, sell_r, buy_r, n_assets, traded,
             wealth_full, eq_share, bond_share, d_eq_share, n_assets_all,
             cash_free, cash_total, wealth_tot, risky_share_c, d_risky_share,
             cash_share, has_cash, left_bank, in_cash,
             n_trades_d, n_sells, n_buys, chf_sold, chf_bought, chf_net,
             traded_d, sold_d, sold_dec, sold_self, sold_adv, sold_man, bought_self,
             sellflow_s, cost_r, in_boerse,
             c_advice, c_advice_a, c_advice_c, c_perf, c_perf_a, c_inv,
             adt_any, adt_init_a,
             ret_next12, ret_past12, Hauptbetreuer_ID),
           allow.cartesian = TRUE]

stk <- ep[, .(ep_id, dd_start, dd_end, pre_month, post_end)][stk, on = "ep_id"]
stk[, rel_month := mdiff(MDate, dd_start)]
stk[, phase := fcase(MDate <  dd_start, "pre",
                     MDate <= dd_end,   "drawdown",
                     default =          "recovery")]
setorder(stk, Bp_ID, ep_id, MDate)
log_step("stacked event panel", stk)

stk[,`:=`(dprice_c = cumsum(dprice),
          dfx_c    = cumsum(dfx),
          dqty_c   = cumsum(buysell)),by=.(Bp_ID,ep_id)]

## carry the client x episode constants that every spec needs
stk <- cle[, .(Bp_ID, ep_id, 
               treat_perf, treat_perf_a, treat_perf_p, treat_perf_a_p,
               treat_adv, treat_advice, treat_inv, treat_perf_inv_a_p,
               treat_advice_a_p, treat_advice_c_p, treat_cli, treat_adt,
               n_contacts_adv, first_adv_day, first_adv_a_p_day, first_adv_c_day,
               first_perf_day, first_perf_a_p_day, on_review_cycle,
               wealth_pre, log_w_pre, n_assets_pre, ret_past12_pre, contacts_pre,
               adv_a_pre, perf_pre, discr_pre, main_bank_pre, advisor_id, smp_main,
               smp_mainbank, sw_type, ep_num, advisor_id, anlagepaket_pre, depot_pre,
               segment_pre, dprice_pre, dfx_pre, dqty_pre,
               wealth_full_pre, eq_share_pre, bond_share_pre,
               cash_pre, wealth_tot_pre, risky_share_pre)][stk, on = .(Bp_ID, ep_id)]

## the design's actual counterfactual, from 04b (frozen holdings drifted with
## observed security prices). Merged here so it sits alongside the two pos_m
## approximations and can be compared directly.
cfh <- load_dt("cfhold")
stk <- cfh[, .(Bp_ID, ep_id, MDate, cf_idx_hold = cf_idx, w_idx_hold = w_idx,
               cf_gap_hold, cf_gap_hold_w, w_tot_idx, cf_cover = cover)][
  stk, on = .(Bp_ID, ep_id, MDate)]

## equity share relative to its pre-episode level (0 at rel_month -1 by
## construction, so the event study has a clean reference)
stk[, d_eq_pre := eq_share - eq_share_pre]
## the REAL de-risking variable: the risky weight in total wealth, which only
## exists now that deposits are in the data
stk[, d_risky_pre := risky_share_c - risky_share_pre]

## FE and cluster identifiers
stk[, ci := paste(Bp_ID, ep_id)]              # client x episode
stk[, te := paste(MDate, ep_id)]              # calendar month x episode
stk[, ae := paste(advisor_id, ep_id)]         # advisor x episode

## ---------------------------------------------------------------------------
## 2. outcome family 1 -- rolling returns (already on the gap-free grid)
##    ret_next12 / ret_past12 come straight from 02_clean.
## ---------------------------------------------------------------------------
cat("coverage of the rolling returns inside the event windows:\n")
print(stk[, .(n = .N, na_next12 = round(mean(is.na(ret_next12)), 4),
              na_past12 = round(mean(is.na(ret_past12)), 4)), by = phase])

## ---------------------------------------------------------------------------
## 3. outcome family 2 -- wealth path and the counterfactual gaps
## ---------------------------------------------------------------------------

## returns used for compounding: an NA month (beginning-of-month value below the
## floor) is compounded as zero, and counted
stk[, ret_c := fifelse(is.na(pf_ret), 0, pf_ret)]
cat("\nmonths compounded with a substituted zero return: ",
    stk[is.na(pf_ret), .N], " of ", nrow(stk), "\n")

stk <- idx[, .(MDate, spi_ret)][stk, on = "MDate"]
stk[, mkt_ret := fifelse(is.na(spi_ret), 0, spi_ret)]

## Both counterfactual indices are compounded over the WHOLE window and then
## rebased on the pre-episode month (rel_month == -1). Compounding only from
## rel_month 0 would leave the gap undefined before the crash and the event study
## would have no pre-period, i.e. no parallel-trends evidence for the primary
## performance outcome.
setorder(stk, Bp_ID, ep_id, MDate)
stk[, `:=`(cf_own = cumprod(1 + ret_c),
           cf_mkt = cumprod(1 + mkt_ret)), by = ci]
stk[, `:=`(cf_own = cf_own / cf_own[rel_month == -1][1],
           cf_mkt = cf_mkt / cf_mkt[rel_month == -1][1]), by = ci]

## realised wealth relative to the pre-episode month
stk[, w_rel := wealth / wealth_pre]
stk[, w_tot_rel := wealth_tot / wealth_tot_pre]

## gaps: >0 means the client did better than the counterfactual
stk[, cf_gap_own := w_rel / cf_own - 1]
stk[, cf_gap_mkt := w_rel / cf_mkt - 1]

## the two legs, per client x episode
legs <- stk[, .(
  w_rel_dd      = w_rel[MDate == dd_end][1],
  w_rel_end     = w_rel[MDate == post_end][1],
  cf_gap_mkt_dd = cf_gap_mkt[MDate == dd_end][1],
  cf_gap_mkt_end= cf_gap_mkt[MDate == post_end][1],
  cf_gap_own_dd = cf_gap_own[MDate == dd_end][1],
  cf_gap_own_end= cf_gap_own[MDate == post_end][1]
), by = .(Bp_ID, ep_id)]
legs[, `:=`(cf_gap_mkt_rec = (1 + cf_gap_mkt_end) / (1 + cf_gap_mkt_dd) - 1,
            cf_gap_own_rec = (1 + cf_gap_own_end) / (1 + cf_gap_own_dd) - 1)]

## the same legs on the frozen-holdings counterfactual
legs_h <- stk[, .(
  cf_gap_hold_dd  = cf_gap_hold[MDate == dd_end][1],
  cf_gap_hold_end = cf_gap_hold[MDate == post_end][1],
  cf_gap_holdw_dd = cf_gap_hold_w[MDate == dd_end][1],
  cf_gap_holdw_end= cf_gap_hold_w[MDate == post_end][1],
  cf_cover_end    = cf_cover[MDate == post_end][1]
), by = .(Bp_ID, ep_id)]
legs_h[, `:=`(cf_gap_hold_rec  = (1 + cf_gap_hold_end)  / (1 + cf_gap_hold_dd) - 1,
              cf_gap_holdw_rec = (1 + cf_gap_holdw_end) / (1 + cf_gap_holdw_dd) - 1)]
legs <- legs_h[legs, on = .(Bp_ID, ep_id)]

## ---------------------------------------------------------------------------
## 4. outcome family 3 -- behaviour during the drawdown
##    pos_m has no equity share, so de-risking is measured on NET FLOWS out of
##    the observed (risky) perimeter, scaled by pre-episode wealth.
## ---------------------------------------------------------------------------

beh <- stk[phase == "drawdown", .(
  net_chf      = sum(buysell),
  sell_chf     = -sum(sell),
  buy_chf      = sum(buy),
  any_trade_dd = as.integer(any(traded == 1L)),
  n_months_dd  = .N,
  wealth_pre   = wealth_pre[1],
  ## --- trade level (boerse) ---------------------------------------------
  n_trades_dd  = sum(n_trades_d),
  n_sells_dd   = sum(n_sells),
  chf_sold_dd  = sum(chf_sold),
  chf_bought_dd= sum(chf_bought),
  any_trade_d_dd = as.integer(any(traded_d == 1L)),
  any_sell_d_dd  = as.integer(any(sold_d   == 1L)),
  ## --- trade level, by ORDER CHANNEL (boerse$Medium) ---------------------
  ##     who actually entered the sell order during the crash. The CHF amounts
  ##     that go with these come from trades_m below, not from the panel.
  any_sell_dec_dd  = as.integer(any(sold_dec  == 1L)),
  any_sell_self_dd = as.integer(any(sold_self == 1L)),
  any_sell_adv_dd  = as.integer(any(sold_adv  == 1L)),
  any_sell_man_dd  = as.integer(any(sold_man  == 1L)),
  in_boerse        = in_boerse[1],
  ## --- portfolio weights (pos_full) -------------------------------------
  eq_share_dd_end = eq_share[MDate == dd_end][1],
  eq_share_min    = min_na(eq_share[!is.na(eq_share)]),
  ## --- risky weight in TOTAL wealth (needs deposits) ---------------------
  risky_dd_end    = risky_share_c[MDate == dd_end][1],
  risky_min       = min_na(risky_share_c[!is.na(risky_share_c)]),
  cash_dd_end     = cash_free[MDate == dd_end][1],
  ## --- what an apparent exit actually is ---------------------------------
  ever_in_cash    = as.integer(any(in_cash   == 1L)),
  ever_left_bank  = as.integer(any(left_bank == 1L))
), by = .(Bp_ID, ep_id)]

beh[, `:=`(netflow_dd  = net_chf  / wealth_pre,
           sellflow_dd = sell_chf / wealth_pre,
           buyflow_dd  = buy_chf  / wealth_pre)]

## ---------------------------------------------------------------------------
## 4b. the CHF trade detail over the drawdown window, joined straight from
##     trades_m (02d). Kept off the client-month panel on purpose: another dozen
##     double columns on 5.9 m rows costs about a gigabyte, and nothing between
##     02_clean and here needs them at monthly frequency.
## ---------------------------------------------------------------------------
trm <- load_dt("trades_m")
tdd <- trm[cle[, .(Bp_ID, ep_id, dd_start, dd_end)],
           on = .(Bp_ID, MDate >= dd_start, MDate <= dd_end),
           .(Bp_ID, ep_id = i.ep_id, n_sells_dec, n_sells_self, n_sells_adv,
             chf_sold_dec, chf_sold_self, chf_sold_adv, chf_sold_man,
             chf_bought_dec, kosten_chf, kosten_sell_chf),
           allow.cartesian = TRUE][!is.na(n_sells_dec)]
tdd <- tdd[, .(n_sells_dec_dd   = sum(n_sells_dec),
               n_sells_self_dd  = sum(n_sells_self),
               n_sells_adv_dd   = sum(n_sells_adv),
               chf_sold_dec_dd  = sum(chf_sold_dec),
               chf_sold_self_dd = sum(chf_sold_self),
               chf_sold_adv_dd  = sum(chf_sold_adv),
               chf_sold_man_dd  = sum(chf_sold_man),
               chf_bought_dec_dd= sum(chf_bought_dec),
               kosten_dd        = sum(kosten_chf),
               kosten_sell_dd   = sum(kosten_sell_chf)),
           by = .(Bp_ID, ep_id)]
log_step("trade detail over the drawdown window (from trades_m)", tdd)
rm(trm); gc(verbose = FALSE)

beh <- tdd[beh, on = .(Bp_ID, ep_id)]
for (v in setdiff(names(tdd), c("Bp_ID","ep_id"))) beh[is.na(get(v)), (v) := 0]

## the same ratios from BOOKED TRADES, and the self-directed leg on its own.
## netflow_dd above comes from the pos_m flow field, which also moves on
## corporate actions and transfers; netflow_d_dd counts only orders that someone
## actually placed, and sellflow_s_dd only the ones the client placed.
beh[, `:=`(netflow_d_dd   = (chf_bought_dec_dd - chf_sold_dec_dd) / wealth_pre,
           sellflow_d_dd  = chf_sold_dec_dd  / wealth_pre,
           sellflow_s_dd  = chf_sold_self_dd / wealth_pre,
           sellflow_a_dd  = chf_sold_adv_dd  / wealth_pre,
           kosten_dd_r    = kosten_dd        / wealth_pre)]
## the trade-based analogue of derisk_dd: net SALES through booked orders above
## the same threshold share of pre-episode wealth
beh[, derisk_d_dd := as.integer(-netflow_d_dd > P$derisk_thresh)]
## ... and the version that only counts what the client did themselves
beh[, derisk_s_dd := as.integer(sellflow_s_dd > P$derisk_thresh)]
## "large de-risking", two measures:
##   derisk_dd    net sales above the threshold share of pre-episode wealth
##                (the only version pos_m alone could support)
##   derisk_eq_dd the design's actual definition: the EQUITY SHARE falls by more
##                than P$derisk_eq_drop between the pre-episode month and the
##                trough. Available now that pos_full gives portfolio weights.
beh[, derisk_dd := as.integer(-netflow_dd > P$derisk_thresh)]
beh <- cle[, .(Bp_ID, ep_id, eq_share_pre)][beh, on = .(Bp_ID, ep_id)]
beh[, d_eq_dd    := eq_share_dd_end - eq_share_pre]
beh[, derisk_eq_dd := as.integer(d_eq_dd < -P$derisk_eq_drop)]
beh[, derisk_eq_any := as.integer(eq_share_min - eq_share_pre < -P$derisk_eq_drop)]

## the design's de-risking measure on the RISKY weight in total wealth
beh <- cle[, .(Bp_ID, ep_id, risky_share_pre, cash_pre)][beh, on = .(Bp_ID, ep_id)]
beh[, d_risky_dd    := risky_dd_end - risky_share_pre]
beh[, derisk_rk_dd  := as.integer(d_risky_dd < -P$derisk_eq_drop)]
## where did the money go? change in deposits over the drawdown, scaled by the
## pre-episode securities portfolio
beh[, d_cash_dd     := (cash_dd_end - cash_pre) / wealth_pre]

## full liquidation: wealth collapses to (almost) nothing at any point during the
## drawdown or the following recovery, or the client leaves the panel
liq <- stk[rel_month >= 0, .(
  min_w_rel = min(w_rel, na.rm = TRUE),
  exits     = as.integer(any(status == "post_exit"))
), by = .(Bp_ID, ep_id)]
liq[, full_liq := as.integer(min_w_rel < (1 - P$liq_thresh))]


# add price, fx, and quantity changes from pos over the drawdown period
pdd <- pan[cle[, .(Bp_ID, ep_id, dd_start, dd_end)],
           on = .(Bp_ID, MDate >= dd_start, MDate <= dd_end),
           .(Bp_ID, ep_id = i.ep_id, dprice, dfx, buysell),
           allow.cartesian = TRUE]
setorder(pdd,Bp_ID,ep_id)
pdd <- pdd[, .(dprice_dd   = sum(dprice),
               dfx_dd  = sum(dfx),
               dqty_dd   = sum(buysell)),
           by = .(Bp_ID, ep_id)]

beh <- pdd[beh, on = .(Bp_ID, ep_id)]


## ---------------------------------------------------------------------------
## 5. outcome family 4 -- re-entry timing (de-riskers only)
##    months after dd_end until the cumulative net flow since dd_start is back
##    within P$reentry_tol of pre-episode wealth. Censored at post_end.
## ---------------------------------------------------------------------------

setorder(stk, Bp_ID, ep_id, MDate)
stk[rel_month >= 0, cum_flow := cumsum(buysell) / wealth_pre, by = ci]

reentry <- merge(
  stk[rel_month >= 0 & MDate > dd_end, .(Bp_ID, ep_id, MDate, dd_end, cum_flow)],
  beh[, .(Bp_ID, ep_id, derisk_dd, netflow_dd)], by = c("Bp_ID","ep_id"))[derisk_dd == 1L]

reentry <- reentry[, .(
  months_to_reentry = {
    hit <- which(cum_flow >= -P$reentry_tol)
    if (length(hit)) mdiff(MDate[min(hit)], dd_end[1]) else NA_integer_
  },
  reentered = as.integer(any(cum_flow >= -P$reentry_tol))
), by = .(Bp_ID, ep_id)]

## the design's re-entry measure: months until the EQUITY SHARE is back within
## P$reentry_tol (in share points) of its pre-episode level. Defined for the
## clients who actually cut their equity share during the drawdown.
re_eq <- merge(
  stk[MDate > dd_end, .(Bp_ID, ep_id, MDate, dd_end, d_eq_pre)],
  beh[, .(Bp_ID, ep_id, derisk_eq_dd)], by = c("Bp_ID","ep_id"))[derisk_eq_dd == 1L]
re_eq <- re_eq[, .(
  months_to_eq_reentry = {
    hit <- which(d_eq_pre >= -P$reentry_tol)
    if (length(hit)) mdiff(MDate[min(hit)], dd_end[1]) else NA_integer_
  },
  eq_reentered = as.integer(any(d_eq_pre >= -P$reentry_tol, na.rm = TRUE))
), by = .(Bp_ID, ep_id)]
reentry <- re_eq[reentry, on = .(Bp_ID, ep_id)]

## ---------------------------------------------------------------------------
## 5b. PER-SALE FORGONE RETURN (design 4.4)
##     For every sale booked inside the drawdown window, the return on the sold
##     security from the sale price to its price +6 and +12 months later. Needs
##     trade-level data and security prices, i.e. boerse + pos_full; it could
##     not be built from pos_m at all.
##     A POSITIVE number means the client gave up a gain by selling.
##
##     Restricted to DECISION sells: a corporate action posted through the trade
##     table is not a sale anybody chose, and its "forgone return" is meaningless.
##     `chan` is carried through so the cost of selling can be split by who
##     entered the order -- the client, the advisor, or the mandate desk.
## ---------------------------------------------------------------------------
td  <- load_dt("trades_d")[sell == 1L & decision == 1L]
pxm <- load_dt("prices_m")[, .(Asset_ID, MDate, px)]

sells <- td[ep[, .(ep_id, peak_date, trough_date)],
            on = .(DDate >= peak_date, DDate <= trough_date),
            .(Bp_ID, ep_id = i.ep_id, Asset_ID, DDate = x.DDate, MDate = x.MDate,
              qty, chf, px_chf, chan, route),
            allow.cartesian = TRUE][!is.na(Bp_ID)]
log_step("decision sales inside a drawdown window", sells)

for (h in P$forgone_h) {
  sells[, mtarget := madd(MDate, h)]
  sells <- pxm[sells, on = .(Asset_ID, MDate = mtarget)]
  setnames(sells, c("MDate","px","i.MDate"), c("mtarget","px_h","MDate"))
  sells[, (paste0("forgone_", h)) := px_h / px_chf - 1]
  sells[, c("mtarget","px_h") := NULL]
}
## winsorise: a share split between sale and horizon shows up as a 100x return
for (h in P$forgone_h)
  sells[, (paste0("forgone_", h)) := winsor(get(paste0("forgone_", h)), c(0.01, 0.99))]

cat("\n== per-sale forgone return, sales during a drawdown ==\n")
print(sells[, c(.(n_sales = .N, n_clients = uniqueN(Bp_ID)),
                lapply(.SD, function(x) round(mean(x, na.rm = TRUE), 4))),
            .SDcols = paste0("forgone_", P$forgone_h), by = ep_id][order(ep_id)])
cat("(positive = the security rose after the sale, i.e. selling was costly)\n")

cat("\n== ... split by WHO ENTERED THE ORDER (boerse$Medium) ==\n")
print(sells[, c(.(n_sales = .N, n_clients = uniqueN(Bp_ID),
                  chf_mn = round(sum(chf) / 1e6, 1)),
                lapply(.SD, function(x) round(mean(x, na.rm = TRUE), 4))),
            .SDcols = paste0("forgone_", P$forgone_h), by = chan][order(-n_sales)])
cat("A self-directed sale and a mandate-desk sale are not the same event; before\n")
cat("Medium was read they were pooled into one 'sale during the drawdown'.\n")

## value-weighted to the client x episode level
fg <- sells[, c(.(n_sales_dd = .N, chf_sold_trades = sum(chf)),
                lapply(.SD, function(x) sum(x * chf, na.rm = TRUE) / sum(chf))),
            .SDcols = paste0("forgone_", P$forgone_h), by = .(Bp_ID, ep_id)]
setnames(fg, paste0("forgone_", P$forgone_h), paste0("forgone_vw_", P$forgone_h))

## the same, on the client's OWN orders only: the forgone return on the sales the
## client actually decided to make, free of the mandate desk's rebalancing
fgs <- sells[chan == "self",
             c(.(n_sales_self_dd = .N),
               lapply(.SD, function(x) sum(x * chf, na.rm = TRUE) / sum(chf))),
             .SDcols = paste0("forgone_", P$forgone_h), by = .(Bp_ID, ep_id)]
setnames(fgs, paste0("forgone_", P$forgone_h), paste0("forgone_self_", P$forgone_h))
fg <- fgs[fg, on = .(Bp_ID, ep_id)]

## ---------------------------------------------------------------------------
## 6. merge everything back onto the client x episode file
## ---------------------------------------------------------------------------

cle <- legs[cle, on = .(Bp_ID, ep_id)]
cle <- beh[, .(Bp_ID, ep_id, net_chf, sell_chf, buy_chf,
               netflow_dd, sellflow_dd, buyflow_dd, derisk_dd, any_trade_dd,
               n_trades_dd, n_sells_dd, chf_sold_dd, chf_bought_dd,
               any_trade_d_dd, any_sell_d_dd,
               any_sell_dec_dd, any_sell_self_dd, any_sell_adv_dd, any_sell_man_dd,
               n_sells_dec_dd, n_sells_self_dd, n_sells_adv_dd,
               chf_sold_dec_dd, chf_sold_self_dd, chf_sold_adv_dd, chf_sold_man_dd,
               netflow_d_dd, sellflow_d_dd, sellflow_s_dd, sellflow_a_dd,
               derisk_d_dd, derisk_s_dd, kosten_dd, kosten_sell_dd, kosten_dd_r,
               in_boerse,
               eq_share_dd_end, d_eq_dd, derisk_eq_dd, derisk_eq_any,
               risky_dd_end, d_risky_dd, derisk_rk_dd, d_cash_dd,
               ever_in_cash, ever_left_bank)][cle, on = .(Bp_ID, ep_id)]
cle <- fg[cle, on = .(Bp_ID, ep_id)]
cle[, `:=`(netflow_dd_w  = winsor(netflow_dd,  c(0.01, 0.99)),
           sellflow_dd_w = winsor(sellflow_dd, c(0.00, 0.99)),
           netflow_d_dd_w  = winsor(netflow_d_dd,  c(0.01, 0.99)),
           sellflow_d_dd_w = winsor(sellflow_d_dd, c(0.00, 0.99)),
           sellflow_s_dd_w = winsor(sellflow_s_dd, c(0.00, 0.99)),
           sellflow_a_dd_w = winsor(sellflow_a_dd, c(0.00, 0.99)),
           kosten_dd_r_w   = winsor(kosten_dd_r,   c(0.00, 0.99)))]
cle <- liq[, .(Bp_ID, ep_id, min_w_rel, exits, full_liq)][cle, on = .(Bp_ID, ep_id)]
cle <- reentry[cle, on = .(Bp_ID, ep_id)]

## ---------------------------------------------------------------------------
## 6b. auxiliary samples
##   smp_traders : clients who traded during the drawdown. Under the OLD
##     trade-conditional measure this was the only defensible comparison,
##     because treatment implied trading. With the contact log that restriction
##     is no longer needed -- it is kept as a robustness sample only, and
##     conditioning on it now selects on an OUTCOME, so it is not the headline.
##   smp_cycle   : clients already on a portfolio-review cycle before the
##     episode. Within this group, treatment is closer to "your scheduled review
##     happened to fall inside the crash" than to "the advisor picked you".
## ---------------------------------------------------------------------------
cle[, smp_traders := as.integer(smp_main == 1L & any_trade_dd == 1L)]
cle[, smp_cycle   := as.integer(smp_main == 1L & on_review_cycle == 1L)]
## smp_boerse : clients who appear in the trade table at all. Every channel
##   outcome is a mechanical zero outside it, so the trade-based specifications
##   are reported on both -- the full sample (a non-trader genuinely did not
##   sell) and this one (which drops the clients the file never covered).
cle[is.na(in_boerse), in_boerse := 0L]
cle[, smp_boerse := as.integer(smp_main == 1L & in_boerse == 1L)]
## smp_mb : clients for whom THIS bank is the main bank (Hauptbankkunde at the
##   pre-episode month). Only 44% of the main sample. It matters because every
##   wealth-scaled outcome has a denominator that is only right when the money is
##   actually here: 6.3% of non-main-bank clients have NO cash account at all, so
##   risky_share_c is ~1 by construction for them, and derisk_dd (20% of
##   pre-wealth) and full_liq (5%) are measured against a fraction of true
##   wealth. It is frozen pre-episode, so restricting on it changes the estimand
##   to a LATE for main-bank clients without biasing it -- and treatment rates
##   are balanced across it (12.8 vs 11.2, 3.5 vs 3.2, 16.7 vs 17.1 by episode).
cle[, smp_mb_exdisc := as.integer(smp_main == 1L & main_bank_pre == 1L)]
cle[, smp_mb := as.integer(main_bank_pre == 1L)]

## return outcomes measured at the trough (12m forward from dd_end)
r12 <- stk[MDate == dd_end, .(Bp_ID, ep_id, ret_next12_from_dd = ret_next12)]
r12b <- stk[MDate == pre_month, .(Bp_ID, ep_id, ret_past12_pre_chk = ret_past12)]
cle <- r12[cle, on = .(Bp_ID, ep_id)]
cle <- r12b[cle, on = .(Bp_ID, ep_id)]

## winsorised versions of the continuous outcomes, within episode. The gap
## measures have very fat right tails (large external contributions) and a hard
## floor at -1 (wealth to zero), so every level regression uses these.
wcols <- c("cf_gap_mkt_dd","cf_gap_mkt_end","cf_gap_mkt_rec",
           "cf_gap_own_dd","cf_gap_own_end","cf_gap_own_rec",
           "cf_gap_hold_dd","cf_gap_hold_end","cf_gap_hold_rec",
           "cf_gap_holdw_dd","cf_gap_holdw_end","cf_gap_holdw_rec",
           "d_eq_dd","d_risky_dd","d_cash_dd",
           "w_rel_dd","w_rel_end","ret_next12_from_dd")
cle[, (paste0(wcols, "_w")) := lapply(.SD, winsor), by = ep_id, .SDcols = wcols]

## ---------------------------------------------------------------------------
## 7. report
## ---------------------------------------------------------------------------

cat("\n=============================================================\n")
cat(" OUTCOMES -- raw means by episode and treatment (main sample)\n")
cat(" (descriptive only: no controls, no fixed effects)\n")
cat("=============================================================\n")

m <- cle[smp_main == 1L, .(
  N              = .N,
  ## behaviour
  p_trade        = round(mean(any_trade_dd), 3),
  net_flow_dd    = round(mean(netflow_dd, na.rm = TRUE), 4),
  p_derisk       = round(mean(derisk_dd, na.rm = TRUE), 4),
  p_full_liq     = round(mean(full_liq, na.rm = TRUE), 4),
  ## performance
  w_rel_dd       = round(mean(w_rel_dd, na.rm = TRUE), 4),
  w_rel_end      = round(mean(w_rel_end, na.rm = TRUE), 4),
  gap_mkt_dd     = round(mean(cf_gap_mkt_dd, na.rm = TRUE), 4),
  gap_mkt_end    = round(mean(cf_gap_mkt_end, na.rm = TRUE), 4),
  gap_own_end    = round(mean(cf_gap_own_end, na.rm = TRUE), 4),
  ret_next12     = round(mean(ret_next12_from_dd, na.rm = TRUE), 4)
), by = .(ep_id, treat_adv)][order(ep_id, treat_adv)]
print(m)

cat("\nre-entry (de-riskers only):\n")
print(cle[smp_main == 1L & derisk_dd == 1L,
          .(N = .N, p_reentered = round(mean(reentered, na.rm = TRUE), 3),
            med_months = as.numeric(median(months_to_reentry, na.rm = TRUE))),
          by = .(ep_id, treat_adv)][order(ep_id, treat_adv)])

cat("\ndistribution of the primary performance outcome (cf_gap_mkt_end):\n")
print(round(quantile(cle[smp_main == 1L]$cf_gap_mkt_end, c(.01,.05,.25,.5,.75,.95,.99),
                     na.rm = TRUE), 4))

cat("\n-- treatment is no longer conditional on trading --\n")
cat("share of treated who traded during the drawdown: ",
    round(cle[smp_main == 1L & treat_adv == 1L, mean(any_trade_dd)], 3),
    "; of controls: ", round(cle[smp_main == 1L & treat_adv == 0L, mean(any_trade_dd)], 3),
    "\n(under the old trade-based measure these were 0.99 and 0.35)\n", sep = "")

cat("\nsame table, headline treatment = portfolio review meeting (treat_perf):\n")
print(cle[smp_main == 1L, .(
  N = .N, p_trade = round(mean(any_trade_d_dd), 3),
  net_flow_dd = round(mean(netflow_dd, na.rm = TRUE), 4),
  p_derisk = round(mean(derisk_dd, na.rm = TRUE), 4),
  p_full_liq = round(mean(full_liq, na.rm = TRUE), 4),
  gap_mkt_end = round(mean(cf_gap_mkt_end, na.rm = TRUE), 4),
  ret_next12 = round(mean(ret_next12_from_dd, na.rm = TRUE), 4)
), by = .(ep_id, treat_perf)][order(ep_id, treat_perf)])

cat("\n== WHAT AN APPARENT EXIT ACTUALLY IS (needs deposits) ==\n")
print(cle[smp_main == 1L, .(
  N = .N,
  p_lost_all_securities = round(mean(ever_in_cash == 1L | ever_left_bank == 1L), 4),
  p_sold_into_cash      = round(mean(ever_in_cash   == 1L), 4),
  p_left_the_bank       = round(mean(ever_left_bank == 1L), 4)
), by = .(ep_id, treat_perf)][order(ep_id, treat_perf)])
cat("Before deposits were available these two columns were pooled into 'exited'\n")
cat("and the whole group was treated as attrition.\n")

cat("\n== de-risking on the RISKY WEIGHT in total wealth ==\n")
print(cle[smp_main == 1L, .(
  N = .N,
  risky_share_pre = round(mean(risky_share_pre, na.rm = TRUE), 3),
  d_risky_dd      = round(mean(d_risky_dd, na.rm = TRUE), 4),
  p_derisk_risky  = round(mean(derisk_rk_dd, na.rm = TRUE), 4),
  d_cash_dd       = round(mean(d_cash_dd, na.rm = TRUE), 4)
), by = .(ep_id, treat_perf)][order(ep_id, treat_perf)])
cat("d_cash_dd = change in deposits over the drawdown per CHF of pre-episode\n")
cat("securities: positive means the proceeds landed in the client's own account.\n")

cat("\n== NEW OUTCOMES from pos_full + boerse ==\n")
cat("equity share and the frozen-holdings counterfactual:\n")
print(cle[smp_main == 1L, .(
  N = .N,
  eq_share_pre  = round(mean(eq_share_pre, na.rm = TRUE), 3),
  d_eq_dd       = round(mean(d_eq_dd, na.rm = TRUE), 4),
  p_derisk_eq   = round(mean(derisk_eq_dd, na.rm = TRUE), 4),
  gap_hold_dd   = round(mean(cf_gap_hold_dd, na.rm = TRUE), 4),
  gap_hold_end  = round(mean(cf_gap_hold_end, na.rm = TRUE), 4),
  forgone_12    = round(mean(forgone_vw_12, na.rm = TRUE), 4)
), by = .(ep_id, treat_perf)][order(ep_id, treat_perf)])

cat("\n== SELLING DURING THE DRAWDOWN, BY WHO ENTERED THE ORDER ==\n")
cat("(boerse$Medium; a mechanical zero for clients not covered by the trade file,\n")
cat(" so the same table is repeated on smp_boerse below)\n")
print(cle[smp_main == 1L, .(
  N = .N,
  p_sell_flow = round(mean(sellflow_dd > 0, na.rm = TRUE), 4),  # pos_m quantity change
  p_sell_book = round(mean(any_sell_d_dd), 4),                  # any booked sell
  p_sell_dec  = round(mean(any_sell_dec_dd), 4),                # ... a real decision
  p_sell_self = round(mean(any_sell_self_dd), 4),               # ... entered by the client
  p_sell_adv  = round(mean(any_sell_adv_dd), 4),                # ... by the advisor
  p_sell_man  = round(mean(any_sell_man_dd), 4)                 # ... by the mandate desk
), by = .(ep_id, treat_perf)][order(ep_id, treat_perf)])

cat("\nsame, restricted to clients covered by the trade file (smp_boerse):\n")
print(cle[smp_boerse == 1L, .(
  N = .N,
  p_sell_dec  = round(mean(any_sell_dec_dd), 4),
  p_sell_self = round(mean(any_sell_self_dd), 4),
  p_sell_adv  = round(mean(any_sell_adv_dd), 4),
  sellflow_s  = round(mean(sellflow_s_dd, na.rm = TRUE), 4),
  p_derisk_s  = round(mean(derisk_s_dd, na.rm = TRUE), 4)
), by = .(ep_id, treat_perf)][order(ep_id, treat_perf)])
cat("sellflow_s = client-entered sales during the drawdown per CHF of pre-episode\n")
cat("wealth; derisk_s = that ratio above the ", P$derisk_thresh, " threshold.\n", sep = "")
cat("coverage: ", round(cle[smp_main == 1L, mean(in_boerse)], 4),
    " of main-sample client-episodes are in the trade file.\n", sep = "")

cat("\n== MAIN-BANK STATUS: why smp_mb exists ==\n")
print(cle[smp_main == 1L, .(
  N = .N,
  med_wealth    = round(median(wealth_pre)),
  med_cash      = round(median(cash_pre, na.rm = TRUE)),
  risky_share   = round(mean(risky_share_pre, na.rm = TRUE), 3),
  p_no_cash_acc = round(mean(cash_pre == 0, na.rm = TRUE), 4),
  p_full_liq    = round(mean(full_liq, na.rm = TRUE), 4),
  p_left_bank   = round(mean(ever_left_bank), 4)
), by = .(main_bank_pre)][order(main_bank_pre)])
cat("Non-main-bank clients are 30x more likely to have no cash account here, so\n")
cat("risky_share_c is ~1 by construction for them, and they are 3x more likely to\n")
cat("'leave the bank' -- which for a satellite custody account is consolidation,\n")
cat("not panic. 06 reports every specification on smp_mb as well.\n")

cat("\n== TRANSACTION REVENUE (boerse$Kosten) during the drawdown ==\n")
print(cle[smp_boerse == 1L, .(
  N = .N,
  kosten_chf   = round(mean(kosten_dd), 1),
  kosten_bp    = round(1e4 * mean(kosten_dd_r, na.rm = TRUE), 1),
  kosten_sell  = round(mean(kosten_sell_dd), 1)
), by = .(ep_id, treat_perf)][order(ep_id, treat_perf)])
cat("kosten_bp = commission charged during the drawdown, in basis points of\n")
cat("pre-episode wealth. This is a booked fee per trade, not an annual aggregate.\n")

cat("\nthe three counterfactuals side by side (main sample, at post_end):\n")
print(cle[smp_main == 1L, .(
  N = .N,
  hold = round(mean(cf_gap_hold_end, na.rm = TRUE), 4),   # frozen holdings, TRUE version
  mkt  = round(mean(cf_gap_mkt_end,  na.rm = TRUE), 4),   # market drift
  own  = round(mean(cf_gap_own_end,  na.rm = TRUE), 4)    # own price return
), by = ep_id][order(ep_id)])
cat("cf_gap_hold is the design's definition; the other two are the pos_m-only\n")
cat("approximations and are kept so the size of the approximation error is visible.\n")

cat("\nequity-share re-entry (clients who cut equity by >", 100 * P$derisk_eq_drop, "pp):\n")
print(cle[smp_main == 1L & derisk_eq_dd == 1L,
          .(N = .N, p_reentered = round(mean(eq_reentered, na.rm = TRUE), 3),
            med_months = as.numeric(median(months_to_eq_reentry, na.rm = TRUE))),
          by = .(ep_id, treat_perf)][order(ep_id, treat_perf)])

cat("\nrestricted to clients already on a review cycle before the episode:\n")
print(cle[smp_cycle == 1L, .(
  N = .N, net_flow_dd = round(mean(netflow_dd, na.rm = TRUE), 4),
  p_derisk = round(mean(derisk_dd, na.rm = TRUE), 4),
  gap_mkt_end = round(mean(cf_gap_mkt_end, na.rm = TRUE), 4),
  ret_next12 = round(mean(ret_next12_from_dd, na.rm = TRUE), 4)
), by = .(ep_id, treat_perf)][order(ep_id, treat_perf)])

save_dt(stk, "stk")
save_dt(cle, "cle")
log_step("outcomes written", cle)
sink()
