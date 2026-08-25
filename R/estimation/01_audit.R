## =============================================================================
## 01_audit.R -- data audit. Runs first, prints a report, changes nothing.
##
## Written against what pos_m.parquet ACTUALLY contains. Where the design in
## R/estimation.R asks for a field that does not exist, the audit says so
## explicitly instead of substituting something that looks similar.
## =============================================================================

source("00_setup.R")
log_init("01_audit")

sink(file.path(RESULTS, "01_audit_report.txt"), split = TRUE)
on.exit(sink(), add = TRUE)

cat("=============================================================\n")
cat(" DATA AUDIT -- pos_m.parquet\n ", format(Sys.time()), "\n")
cat("=============================================================\n\n")

pos <- setDT(read_parquet(POS_M_FILE))
require_cols(pos, c("Bp_ID", "MDate", "vol", "dprice", "dtotal", "buysell", "sell", "buy",
                    "wealth", "n_assets", "Hauptbetreuer_ID", "EVV", "Depotprodukt"),
             "01_audit")
setorder(pos, Bp_ID, MDate)

cat(sprintf("rows %s | clients %s | months %s..%s\n\n",
            format(nrow(pos), big.mark = "'"), format(uniqueN(pos$Bp_ID), big.mark = "'"),
            min(pos$MDate), max(pos$MDate)))

## ---------------------------------------------------------------------------
## A. Fields the design asks for -- present / absent
## ---------------------------------------------------------------------------

cat("-------------------------------------------------------------\n")
cat("A. FIELD AVAILABILITY vs. the design in R/estimation.R\n")
cat("-------------------------------------------------------------\n")

avail <- data.table(
  design_field = c("pf_ret", "wealth", "advisor_id", "eq_share",
                   "cash / deposit balance", "contact log (all contacts)",
                   "contact initiator", "contact channel",
                   "trade level data (ISIN, direction, amount)",
                   "security level prices/returns", "discretionary mandate flag",
                   "market index"),
  in_pos_m = c("derived: dprice/(vol-dtotal)", "wealth", "Hauptbetreuer_ID", "NO",
               "NO", "NO -- only contacts that preceded a trade",
               "n_trades_adv_init_a / _init_c (trade rows, not contacts)",
               "n_meeting / n_phone / n_mail (trade rows, not contacts)",
               "NO -- only monthly net flow (buysell / sell / buy)",
               "NO", "Depotprodukt == 'Vermoegensverwaltungsdepot' | EVV == 1",
               "external: data/hspitr_2.csv, data/hsmi.csv")
)
print(avail, right = FALSE)

cat("\nCONSEQUENCES (the design cannot be followed literally):\n")
cat("  1. eq_share and any equity-share outcome are NOT computable from pos_m.\n")
cat("     Behavioural outcomes are built on net flows (buysell/sell) instead.\n")
cat("  2. cf_gap as specified (freeze holdings, drift with security prices) is NOT\n")
cat("     computable: no asset-level panel here. 05_outcomes builds two labelled\n")
cat("     approximations (own price return / market index) instead.\n")
cat("  3. Per-sale forgone return is NOT computable (no trade or price level data).\n")
cat("  4. Treatment is 'advised TRADE', not 'advisor contact': the contact fields\n")
cat("     in pos_m only exist on rows that carry a trade (see section D).\n\n")

## ---------------------------------------------------------------------------
## B. Panel completeness
## ---------------------------------------------------------------------------

cat("-------------------------------------------------------------\n")
cat("B. PANEL COMPLETENESS\n")
cat("-------------------------------------------------------------\n")

sp <- pos[, .(first = min(MDate), last = max(MDate), n = .N), by = Bp_ID]
sp[, nmonths := mdiff(last, first) + 1L]
sp[, gaps := nmonths - n]

cat(sprintf("clients                                : %s\n", format(nrow(sp), big.mark = "'")))
cat(sprintf("clients with interior gaps             : %s (%.1f%%)\n",
            format(sp[gaps > 0, .N], big.mark = "'"), 100 * sp[gaps > 0, .N] / nrow(sp)))
cat("gap length quantiles (months):\n"); print(quantile(sp$gaps, c(.5, .9, .95, .99, 1)))

cat("\nentries and exits per year (a client 'exits' at its last observed month):\n")
ee <- merge(sp[, .(entries = .N), by = .(year = year(first))],
            sp[, .(exits   = .N), by = .(year = year(last))], by = "year", all = TRUE)
print(ee[order(year)])
cat("\nNOTE: pos_m has one row per client-month ONLY when the client holds at least\n")
cat("      one position. A client who liquidates completely DISAPPEARS -- it cannot\n")
cat("      be told apart from a client who closes the relationship. 02_clean\n")
cat("      re-inserts these months on an explicit grid at zero holdings and flags\n")
cat("      them (exited_after / interior_gap), so lags shift calendar months.\n\n")

cat("clients going to zero-ish wealth in their last month vs. simply ending:\n")
lastrow <- pos[sp, on = .(Bp_ID, MDate = last)]
print(lastrow[, .(N = .N,
                  wealth_lt_1k = sum(wealth < 1000, na.rm = TRUE),
                  wealth_lt_10k = sum(wealth < 10000, na.rm = TRUE)),
              by = .(censored = MDate == max(pos$MDate))])

## ---------------------------------------------------------------------------
## C. Data perimeter -- cash
## ---------------------------------------------------------------------------

cat("\n-------------------------------------------------------------\n")
cat("C. DATA PERIMETER -- IS CASH INSIDE?\n")
cat("-------------------------------------------------------------\n")
cat("pos_m is built from the position table after dropping the instrument groups\n")
cat("'Bar', 'Money Market Deposit', 'Waehrung', 'Limite', 'Swaps' (R/2_merge_...R)\n")
cat("and after keeping only Aktien/Fonds/Obligationen/Strukturierte/Optionen\n")
cat("(R/3_pos_agg_advisors.R).\n\n")
cat("*** SALE PROCEEDS LEAVE THE OBSERVED PERIMETER. ***\n")
cat("A client who sells everything shows wealth -> 0 and then vanishes; the money\n")
cat("is not observed sitting in cash. Consequences:\n")
cat("  - a return index computed on survivors is upward biased for sellers;\n")
cat("  - the primary performance outcome must therefore be a WEALTH-PATH gap\n")
cat("    (realised value vs. frozen-portfolio value), not a chained return.\n\n")

cat("wealth <= 0 client-months : ", pos[wealth <= 0, .N], "\n")
cat("wealth  < 1'000 CHF       : ", pos[wealth < 1000, .N], "\n")
bom <- pos[, vol - dtotal]
cat("BoM value (vol-dtotal) <= 0        : ", sum(bom <= 0, na.rm = TRUE), "\n")
cat("BoM value < min_bom_value (", P$min_bom_value, "): ", sum(bom < P$min_bom_value, na.rm = TRUE), "\n")

cat("\nflow identity  dtotal ~ dprice + dfx + buysell :\n")
res <- pos[, dtotal - (dprice + dfx + buysell)]
cat("  share |resid| < 1 CHF            : ", round(mean(abs(res) < 1, na.rm = TRUE), 4), "\n")
cat("  share |resid| <= 1% of |dtotal|  : ", round(mean(abs(res) <= 0.01 * abs(dtotal <- pos$dtotal) + 1, na.rm = TRUE), 4), "\n")
cat("  (the remainder is DA_Delta_CHF, which was not carried into pos_m)\n")

## ---------------------------------------------------------------------------
## D. Contact / advice log completeness
## ---------------------------------------------------------------------------

cat("\n-------------------------------------------------------------\n")
cat("D. CONTACT / ADVICE LOG\n")
cat("-------------------------------------------------------------\n")
cat("In pos_m the advice fields come from the trade table (Boerse) rolled onto a\n")
cat("contact within the 5 days BEFORE the trade, then aggregated over position\n")
cat("rows. Two implications:\n")
cat("  (i)  a contact is only visible if a trade followed it;\n")
cat("  (ii) the counts are counts of POSITION ROWS, not of contacts -- a client\n")
cat("       with 50 holdings can score n_meeting = 50 for one meeting.\n")
cat("=> all treatment variables below are built as MONTHLY INDICATORS.\n\n")

pos[, traded := buysell != 0 | sell != 0 | buy != 0]
pos[, any_adv := (n_trades_adv_inv + n_trades_adv_perf) > 0]
cat("cross-tab traded x any advice flag:\n")
print(dcast(pos[, .N, by = .(traded, any_adv)], traded ~ any_adv, value.var = "N"))

cat("\nadvice flags by year (share of client-months) -- note the 2009/2010 hole:\n")
print(pos[, .(sh_adv_inv  = round(mean(n_trades_adv_inv  > 0), 5),
              sh_adv_perf = round(mean(n_trades_adv_perf > 0), 5),
              sh_init_a   = round(mean(n_trades_adv_init_a > 0), 5),
              sh_init_c   = round(mean(n_trades_adv_init_c > 0), 5),
              sh_traded   = round(mean(traded), 4)), by = .(year = year(MDate))][order(year)])
cat("\n*** The contact/advice source starts in 2011. The sample therefore starts\n")
cat("    ", format(P$smp_start), " (P$smp_start). ***\n", sep = "")

cat("\nlogging intensity by advisor (share of that advisor's client-months with an\n")
cat("advisor-initiated advised trade), 2011+:\n")
adv <- pos[MDate >= P$smp_start, .(client_months = .N,
                                   sh_init_a = mean(n_trades_adv_init_a > 0)),
           by = Hauptbetreuer_ID][client_months >= 100]
print(summary(adv$sh_init_a))
cat("advisors with literally zero logged advised trades: ",
    adv[sh_init_a == 0, .N], " of ", nrow(adv), "\n")

cat("\nis logging depressed during crisis months? (monthly share, 2011+)\n")
mon <- pos[MDate >= P$smp_start, .(sh_init_a = mean(n_trades_adv_init_a > 0),
                                   sh_adv = mean(any_adv),
                                   sh_traded = mean(traded)), by = MDate][order(MDate)]
print(mon[MDate %in% as.Date(c("2011-08-31","2011-09-30","2018-10-31","2018-12-31",
                               "2020-02-29","2020-03-31","2020-04-30",
                               "2022-06-30","2022-09-30"))])
cat("(read together with the episode table in 03_episodes: logging goes UP in\n")
cat(" crisis months, i.e. treatment intensity co-moves with market stress.)\n")

## ---------------------------------------------------------------------------
## E. Return distribution
## ---------------------------------------------------------------------------

cat("\n-------------------------------------------------------------\n")
cat("E. RETURN DISTRIBUTION (raw, before any cleaning)\n")
cat("-------------------------------------------------------------\n")
pos[, pf_ret_raw := dprice / (vol - dtotal)]
print(round(quantile(pos$pf_ret_raw, c(0, .001, .01, .05, .25, .5, .75, .95, .99, .999, 1),
                     na.rm = TRUE), 4))
cat("NA          : ", pos[is.na(pf_ret_raw), .N], "\n")
cat("|pf_ret|>0.5: ", pos[is.finite(pf_ret_raw) & abs(pf_ret_raw) > 0.5, .N], "\n")
cat("|pf_ret|>1  : ", pos[is.finite(pf_ret_raw) & abs(pf_ret_raw) > 1, .N], "\n")

## ---------------------------------------------------------------------------
## F. Discretionary mandates
## ---------------------------------------------------------------------------

cat("\n-------------------------------------------------------------\n")
cat("F. DISCRETIONARY MANDATES (excluded from the main sample)\n")
cat("-------------------------------------------------------------\n")
pos[, discretionary := Depotprodukt %chin% "Vermögensverwaltungsdepot" |
      (!is.na(EVV) & EVV == 1)]
print(pos[MDate >= P$smp_start, .(client_months = .N, clients = uniqueN(Bp_ID)),
          by = discretionary])
cat("\nby product:\n")
print(pos[MDate >= P$smp_start, .N, by = .(Depotprodukt, EVV)][order(-N)])
cat("\nclients that switch in/out of a discretionary mandate over the sample:\n")
sw <- pos[MDate >= P$smp_start, .(any = any(discretionary), all = all(discretionary)), by = Bp_ID]
print(sw[, .(never = sum(!any), always = sum(all), switching = sum(any & !all))])

## ---------------------------------------------------------------------------
## G. Advisors
## ---------------------------------------------------------------------------

cat("\n-------------------------------------------------------------\n")
cat("G. ADVISORS (clustering unit)\n")
cat("-------------------------------------------------------------\n")
ac <- pos[MDate >= P$smp_start, .(clients = uniqueN(Bp_ID)), by = Hauptbetreuer_ID]
cat("advisors: ", nrow(ac), "\n"); print(summary(ac$clients))
cat("largest 5 (likely pooled/team IDs -- check before clustering):\n")
print(head(ac[order(-clients)], 5))
cat("\nadvisor switches per client (mean number of distinct Hauptbetreuer_ID):\n")
print(summary(pos[MDate >= P$smp_start, uniqueN(Hauptbetreuer_ID), by = Bp_ID]$V1))

cat("\n=============================================================\n")
cat(" END OF AUDIT -- read results/01_audit_report.txt before running 02+\n")
cat("=============================================================\n")

sink()
log_step("audit written to results/01_audit_report.txt")
rm(pos); gc()
