## =============================================================================
## 04_treatment.R -- client x episode grid, treatment variants, frozen covariates
##
## Treatment is an ADVISOR CONTACT during the drawdown, taken from the contact
## log (data/contacts.parquet via 02b). It is measured on the exact DAILY window
## [peak_date, trough_date], not on whole months: the Covid drawdown is 23
## trading days, so a monthly window would count calls made after the trough as
## if they were made during the crash.
##
## Every variant is built separately and never pooled. The old trade-based
## measure is kept as treat_adt for comparison.
##
## Output: results/cache/cle.parquet (one row per client x episode)
##         results/04_treatment.txt
## =============================================================================

source("00_setup.R")
log_init("04_treatment")

pan <- load_dt("panel")
ep  <- load_dt("episodes")[usable == TRUE]
setorder(pan, Bp_ID, MDate)
setkey(pan, Bp_ID, MDate)

sink(file.path(RESULTS, "04_treatment.txt"), split = TRUE)
on.exit(sink(), add = TRUE)

## ---------------------------------------------------------------------------
## 1. full client x episode grid, then restrict to clients present pre-episode
## ---------------------------------------------------------------------------

cle <- CJ(Bp_ID = unique(pan$Bp_ID), ep_id = ep$ep_id, unique = TRUE)
cle <- ep[, .(ep_id, dd_start, dd_end, pre_month, pre_start, post_end,
              peak_date, trough_date)][cle, on = "ep_id"]
log_step("client x episode grid (unrestricted)", cle)

## presence: an OBSERVED row in the pre-episode month. A client who joined the
## bank in 2021 must not count as "never advised in 2015".
pre <- pan[, .(Bp_ID, MDate, observed, wealth, log_w, n_assets, ret_past12,
               discretionary, main_bank_yn, Hauptbetreuer_ID, Anlagepaket,
               Depotprodukt, MA_Kundensegment, EBanking_YN,
               wealth_full, eq_share, bond_share, n_assets_all,
               cash_free, wealth_tot, risky_share_c)]
setnames(pre, c("observed","wealth","log_w","n_assets","ret_past12","discretionary",
                "main_bank_yn","Hauptbetreuer_ID","Anlagepaket","Depotprodukt",
                "MA_Kundensegment","EBanking_YN",
                "wealth_full","eq_share","bond_share","n_assets_all",
                "cash_free","wealth_tot","risky_share_c"),
         c("obs_pre","wealth_pre","log_w_pre","n_assets_pre","ret_past12_pre",
           "discr_pre","main_bank_pre","advisor_id","anlagepaket_pre",
           "depot_pre","segment_pre","ebanking_pre",
           "wealth_full_pre","eq_share_pre","bond_share_pre","n_assets_all_pre",
           "cash_pre","wealth_tot_pre","risky_share_pre"))

cle <- pre[cle, on = .(Bp_ID, MDate = pre_month)]
setnames(cle, "MDate", "pre_month")

cle <- cle[!is.na(obs_pre) & obs_pre == 1L]
log_step("present (observed) in the pre-episode month", cle)

cle <- cle[wealth_pre >= P$min_wealth_pre]
log_step(sprintf("pre-episode wealth >= %s CHF", P$min_wealth_pre), cle)

## ---------------------------------------------------------------------------
## 2. treatment variants, measured strictly inside [dd_start, dd_end]
## ---------------------------------------------------------------------------

cd <- load_dt("contacts_d")

## exact daily window: [peak_date, trough_date]
dd_c <- cd[cle[, .(Bp_ID, ep_id, peak_date, trough_date)],
           on = .(Bp_ID, ContactDate >= peak_date, ContactDate <= trough_date),
           .(Bp_ID, ep_id, ContactDate = x.ContactDate, MDate,
             perf, perf_a, perf_p, perf_a_p, inv, inv_a, inv_c,
             advice, advice_a, advice_c),
           allow.cartesian = TRUE][!is.na(ContactDate)]
dd_c <- ep[, .(ep_id, peak_date, dd_start)][dd_c, on = "ep_id"]

trt <- dd_c[, .(
  ## --- headline treatment: portfolio review meeting, advisor-initiated ----
  treat_perf     = as.integer(any(perf   == 1L)),
  treat_perf_a   = as.integer(any(perf_a == 1L)),
  treat_perf_p   = as.integer(any(perf_p == 1L)),   # ... meeting or phone, no mail
  ## --- broader: any investment or review contact ---------------  -----------
  treat_adv      = as.integer(any(advice_a == 1L)), # advisor-initiated advice contact
  treat_advice   = as.integer(any(advice   == 1L)), # either initiator
  treat_inv      = as.integer(any(inv_a    == 1L)),
  ## --- client-initiated: PANIC PROXY, a control/outcome, never treatment ---
  treat_cli      = as.integer(any(advice_c == 1L)),
  ## --- intensive margin ---------------------------------------------------
  n_contacts_adv = sum(advice_a == 1L),
  n_contacts_cli = sum(advice_c == 1L),
  n_contacts_perf= sum(perf     == 1L),
  n_months_adv   = uniqueN(MDate[advice_a == 1L]),
  ## --- timing, in DAYS from the market peak -------------------------------
  ##     min() of an empty selection returns Inf (double) while a non-empty one
  ##     returns an integer; force numeric so the column type is stable
  first_adv_day  = min_na(as.numeric(ContactDate - peak_date)[advice_a == 1L]),
  first_perf_day = min_na(as.numeric(ContactDate - peak_date)[perf     == 1L]),
  first_adv_rel_month = min_na(as.numeric(mdiff(MDate, dd_start))[advice_a == 1L])
), by = .(Bp_ID, ep_id)]

## window bookkeeping from the portfolio panel
bk <- pan[cle[, .(Bp_ID, ep_id, dd_start, dd_end)],
          on = .(Bp_ID, MDate >= dd_start, MDate <= dd_end),
          .(Bp_ID, ep_id, traded, observed, adt_init_a, adt_any),
          allow.cartesian = TRUE][
  , .(n_months_dd = .N, n_months_obs_dd = sum(observed == 1L),
      traded_dd   = as.integer(any(traded == 1L)),
      treat_adt   = as.integer(any(adt_init_a == 1L)),   # the OLD trade-based measure
      treat_adt_any = as.integer(any(adt_any == 1L))), by = .(Bp_ID, ep_id)]

cle <- trt[cle, on = .(Bp_ID, ep_id)]
cle <- bk[cle,  on = .(Bp_ID, ep_id)]
cle[is.na(n_months_dd), `:=`(n_months_dd = 0L, n_months_obs_dd = 0L)]
for (v in c("treat_perf","treat_perf_a","treat_perf_p","treat_adv","treat_advice",
            "treat_inv","treat_cli","n_contacts_adv","n_contacts_cli",
            "n_contacts_perf","n_months_adv","traded_dd","treat_adt","treat_adt_any"))
  cle[is.na(get(v)), (v) := 0L]

## ---------------------------------------------------------------------------
## 3. frozen pre-episode covariates measured over the 12 months BEFORE dd_start
##    (never anything contemporaneous: contemporaneous wealth is an outcome)
## ---------------------------------------------------------------------------

pre_win <- pan[cle[, .(Bp_ID, ep_id, pre_start, pre_month)],
               on = .(Bp_ID, MDate >= pre_start, MDate <= pre_month),
               .(Bp_ID, ep_id, c_advice, c_advice_a, c_advice_c, c_perf,
                 traded, observed),
               allow.cartesian = TRUE][
  , .(contacts_pre   = sum(c_advice   == 1L),   # months with an advice contact
      adv_a_pre      = sum(c_advice_a == 1L),
      adv_c_pre      = sum(c_advice_c == 1L),
      perf_pre       = sum(c_perf     == 1L),
      months_traded_pre = sum(traded == 1L),
      months_obs_pre = sum(observed == 1L)), by = .(Bp_ID, ep_id)]

cle <- pre_win[cle, on = .(Bp_ID, ep_id)]
for (v in c("contacts_pre","adv_a_pre","adv_c_pre","perf_pre",
            "months_traded_pre","months_obs_pre"))
  cle[is.na(get(v)), (v) := 0L]

## has this client ever had a portfolio review before the episode? A client who
## is on a review cycle is a very different control than one who never is.
cle[, on_review_cycle := as.integer(perf_pre > 0)]

## ---------------------------------------------------------------------------
## 4. sample flags (restrictions are applied in 06, not here, so that the
##    excluded groups can be analysed separately)
## ---------------------------------------------------------------------------

cle[, `:=`(
  smp_main      = as.integer(discr_pre == 0L),          # non-discretionary only
  smp_mainbank  = as.integer(main_bank_pre == 1L),
  ep_num        = as.integer(factor(ep_id, levels = ep$ep_id))
)]

cat("=============================================================\n")
cat(" TREATMENT CONSTRUCTION\n")
cat("=============================================================\n\n")

cat("treatment rates by episode (daily window [peak_date, trough_date]):\n")
print(cle[, .(N = .N,
              perf     = round(mean(treat_perf), 4),
              perf_adv_init = round(mean(treat_perf_a), 4),
              any_adv  = round(mean(treat_adv), 4),
              client_init = round(mean(treat_cli), 4),
              traded_dd = round(mean(traded_dd), 4),
              discretionary = round(mean(discr_pre), 3),
              med_wealth = round(median(wealth_pre))), by = .(ep_id, dd_start)][order(dd_start)])

cat("\nmain sample only (non-discretionary at the pre-episode month):\n")
print(cle[smp_main == 1L, .(N = .N,
              n_perf = sum(treat_perf), rate_perf = round(mean(treat_perf), 4),
              n_adv  = sum(treat_adv),  rate_adv  = round(mean(treat_adv), 4),
              n_cli  = sum(treat_cli)), by = .(ep_id)][order(ep_id)])

## ---------------------------------------------------------------------------
## OLD vs. NEW measure -- how bad was the trade-conditional proxy?
## ---------------------------------------------------------------------------
cat("\n-------------------------------------------------------------\n")
cat("MEASUREMENT: contact log vs. the old trade-conditional proxy\n")
cat("-------------------------------------------------------------\n")
print(cle[smp_main == 1L, .N, by = .(contact = treat_adv, old_proxy = treat_adt)][
  order(-contact, -old_proxy)])
cat(sprintf("\nof clients with a real advisor-initiated advice contact during the\n"))
cat(sprintf("drawdown, the old measure caught  %.3f\n",
            cle[smp_main == 1L & treat_adv == 1L, mean(treat_adt == 1L)]))
cat(sprintf("of clients the old measure flagged, %.3f had a contact on record\n",
            cle[smp_main == 1L & treat_adt == 1L, mean(treat_adv == 1L)]))
cat(sprintf("share of contacted clients who did NOT trade during the drawdown: %.3f\n",
            cle[smp_main == 1L & treat_adv == 1L, mean(traded_dd == 0L)]))
cat("=> that last group was invisible before; it is the group the paper is about.\n")

cat("\nintensive margin, treated clients (number of advice contacts in the window):\n")
print(cle[smp_main == 1L & treat_adv == 1L,
          .N, by = .(ep_id, n = pmin(n_contacts_adv, 5L))][order(ep_id, n)])

cat("\ntiming: days from the market peak to the first advisor-initiated contact\n")
print(cle[smp_main == 1L & treat_adv == 1L,
          .(N = .N, p10 = quantile(first_adv_day, .1), med = median(first_adv_day),
            p90 = quantile(first_adv_day, .9),
            window_days = as.integer(trough_date[1] - peak_date[1])), by = ep_id][order(ep_id)])

cat("\npre-episode portfolio composition (main sample), by treatment:\n")
print(cle[smp_main == 1L, .(N = .N,
          med_wealth_full = round(median(wealth_full_pre)),
          eq_share = round(mean(eq_share_pre, na.rm = TRUE), 3),
          risky_share = round(mean(risky_share_pre, na.rm = TRUE), 3),
          med_cash = round(median(cash_pre)),
          n_assets = round(mean(n_assets_all_pre), 1)),
      by = .(ep_id, treat_perf)][order(ep_id, treat_perf)])

cat("\nreview-cycle status before the episode (perf contact in the 12m pre-window):\n")
print(cle[smp_main == 1L, .(N = .N, rate_perf_in_dd = round(mean(treat_perf), 4)),
          by = .(ep_id, on_review_cycle)][order(ep_id, on_review_cycle)])

## ---------------------------------------------------------------------------
## 5. switcher table -- this decides which specification is the headline
## ---------------------------------------------------------------------------

cat("\n-------------------------------------------------------------\n")
cat("SWITCHER TABLE (main sample)\n")
cat("-------------------------------------------------------------\n")

sw <- cle[smp_main == 1L, .(n_ep = .N, n_treated = sum(treat_adv)), by = Bp_ID]
sw[, type := fcase(n_treated == 0L,      "never",
                   n_treated == n_ep,    "always",
                   default =             "switcher")]
print(sw[, .(clients = .N, share = round(.N / nrow(sw), 4)), by = .(n_ep, type)][order(n_ep, type)])
cat("\noverall:\n")
print(sw[, .(clients = .N, share = round(.N / nrow(sw), 4)), by = type][order(-clients)])

sw_share <- sw[n_ep > 1L, mean(type == "switcher")]
cat(sprintf("\nswitcher share among clients present in >1 episode: %.3f\n", sw_share))
if (sw_share < 0.02) {
  cat("=> TOO FEW SWITCHERS. Specification (b) (Bp_ID FE across episodes) is not\n")
  cat("   identified in any meaningful way; the headline must be (a), the stacked\n")
  cat("   event study with client x episode FE.\n")
} else {
  cat("=> switcher share is material; report (b) alongside (a).\n")
}

cle <- sw[, .(Bp_ID, sw_type = type, n_ep_present = n_ep)][cle, on = "Bp_ID"]

save_dt(cle, "cle")
log_step("client x episode file written", cle)
sink()
