## =============================================================================
## 07_robustness.R -- diagnostics and robustness
##
##  1. pre-period coefficients / formal parallel-trends test
##  2. episode by episode, alongside the pooled estimate
##  3. episode weighting: unweighted vs. equal weight per episode
##  4. dose response in the number of advice contacts
##  5. attrition: treated vs. control exit rates after each episode
##  6. placebo: the same cross section on N random non-drawdown windows
##  7. bank revenue side (the Profit_* columns of pos_m)
##
## Output: results/07_robustness.txt, results/tables/07_*.tex
## =============================================================================

source("00_setup.R")
log_init("07_robustness")

pan <- load_dt("panel")
cle <- load_dt("cle")
stk <- load_dt("stk")
idx <- load_dt("index_m")
ep  <- load_dt("episodes")[usable == TRUE]
mods_es <- readRDS(file.path(CACHE, "models_es.rds"))

sink(file.path(RESULTS, "07_robustness.txt"), split = TRUE)
on.exit(sink(), add = TRUE)

C <- cle[smp_main == 1L]

TRT <- "treat_perf"   # must match 06_specs.R

xs_fml <- function(y, fe = "| ep_id", trt = TRT) as.formula(sprintf(
  "%s ~ %s + treat_cli + log_w_pre + n_assets_pre + contacts_pre +
   ret_past12_pre + main_bank_pre %s", y, trt, fe))

cat("=============================================================\n")
cat(" ROBUSTNESS AND DIAGNOSTICS\n")
cat("=============================================================\n")

## ---------------------------------------------------------------------------
## 1. parallel trends: joint test on the pre-period interactions
## ---------------------------------------------------------------------------
cat("\n-------------------------------------------------------------\n")
cat("1. PRE-PERIOD COEFFICIENTS (rel_month -12 .. -2)\n")
cat("-------------------------------------------------------------\n")
cat(sprintf("A joint F test of 'all pre-period %s interactions are zero'.\n", TRT))
cat("Rejection means the treated and control paths were already diverging.\n\n")

pretrend <- function(m, label) {
  if (is.null(m$wald_pre)) {
    cat(sprintf("%-34s  no pre-period coefficients estimated\n", label)); return(invisible(NULL))
  }
  cat(sprintf("%-34s  F = %8.3f   df1 = %3.0f   p = %.4f\n",
              label, m$wald_pre[["stat"]], m$wald_pre[["df1"]], m$wald_pre[["p"]]))
}
for (nm in names(mods_es$beh))  pretrend(mods_es$beh[[nm]],  paste("behaviour:", nm))
for (nm in names(mods_es$perf)) pretrend(mods_es$perf[[nm]], paste("performance:", nm))

cat("\npre-period coefficients, one line per rel_month, primary outcomes:\n")
show_pre <- function(m, label) {
  ct <- copy(m$ct); setnames(ct, 2:5, c("est","se","t","p"))
  ct <- ct[grepl(paste0(":", TRT, "$"), term)]
  ct[, rel_month := as.integer(sub("^rel_month::(-?\\d+):.*$", "\\1", term))]
  setorder(ct, rel_month)
  cat("\n---", label, "---\n")
  print(ct[rel_month < 0, .(rel_month, est = round(est, 5), se = round(se, 5),
                            t = round(t, 2))])
}
show_pre(mods_es$beh[["net flow / BoM"]], "net flow / BoM")
show_pre(mods_es$perf[["wealth / pre-wealth"]], "wealth / pre-wealth")
show_pre(mods_es$perf[["gap vs. market"]], "gap vs. market")

## ---------------------------------------------------------------------------
## 2. episode by episode
## ---------------------------------------------------------------------------
cat("\n-------------------------------------------------------------\n")
cat("2. EPISODE BY EPISODE vs. POOLED\n")
cat("-------------------------------------------------------------\n")

by_ep <- function(y) {
  l <- list(pooled = feols(xs_fml(y), C, vcov = ~ advisor_id, notes = FALSE))
  for (e in ep$ep_id)
    l[[e]] <- feols(xs_fml(y, ""), C[ep_id == e], vcov = ~ advisor_id, notes = FALSE)
  l
}
for (y in c("netflow_dd_w", "derisk_dd", "derisk_eq_dd", "cf_gap_hold_end_w",
            "cf_gap_mkt_end_w", "ret_next12_from_dd_w",
            "any_sell_self_dd", "sellflow_s_dd_w")) {
  cat("\n--- outcome:", y, "---\n")
  print(etable(by_ep(y), digits = 3, fitstat = ~ n + r2, keep = "treat"))
}

## ---------------------------------------------------------------------------
## 3. episode weighting
## ---------------------------------------------------------------------------
cat("\n-------------------------------------------------------------\n")
cat("3. EPISODE WEIGHTING (long episodes otherwise dominate)\n")
cat("-------------------------------------------------------------\n")
C[, w_ep := 1 / .N, by = ep_id]     # equal weight per episode
for (y in c("netflow_dd_w", "derisk_dd", "cf_gap_mkt_end_w")) {
  cat("\n--- outcome:", y, "---\n")
  print(etable(list(unweighted = feols(xs_fml(y), C, vcov = ~ advisor_id, notes = FALSE),
                    `equal per episode` = feols(xs_fml(y), C, weights = ~ w_ep,
                                                vcov = ~ advisor_id, notes = FALSE)),
               digits = 3, fitstat = ~ n, keep = "treat"))
}

## ---------------------------------------------------------------------------
## 4. dose response
## ---------------------------------------------------------------------------
cat("\n-------------------------------------------------------------\n")
cat("4. DOSE RESPONSE in the number of advice contacts during the drawdown\n")
cat("-------------------------------------------------------------\n")
C[, dose := factor(pmin(n_contacts_adv, 3L), levels = 0:3,
                   labels = c("0", "1", "2", "3+"))]
dose_fml <- function(y) as.formula(paste(
  y, "~ i(dose, ref = '0') + treat_cli + log_w_pre + n_assets_pre + contacts_pre +",
  "ret_past12_pre + main_bank_pre | ep_id"))
print(etable(list(
  `net flow`   = feols(dose_fml("netflow_dd_w"),     C, vcov = ~ advisor_id, notes = FALSE),
  `P(de-risk)` = feols(dose_fml("derisk_dd"),        C, vcov = ~ advisor_id, notes = FALSE),
  `P(eq cut)`  = feols(dose_fml("derisk_eq_dd"),     C, vcov = ~ advisor_id, notes = FALSE),
  `gap hold`   = feols(dose_fml("cf_gap_hold_end_w"),C, vcov = ~ advisor_id, notes = FALSE)
), digits = 3, fitstat = ~ n, keep = "dose"))

## ---------------------------------------------------------------------------
## 5. attrition
## ---------------------------------------------------------------------------
cat("\n-------------------------------------------------------------\n")
cat("5. ATTRITION after each episode (treated vs. control)\n")
cat("-------------------------------------------------------------\n")
att <- stk[MDate > dd_end, .(exited = as.integer(any(status == "post_exit"))),
           by = .(Bp_ID, ep_id)]
att <- cle[, .(Bp_ID, ep_id, treat_perf, treat_adv, smp_main, advisor_id, log_w_pre,
               n_assets_pre, contacts_pre, ret_past12_pre, main_bank_pre,
               treat_cli)][att, on = .(Bp_ID, ep_id)]
print(att[smp_main == 1L, .(N = .N, exit_rate = round(mean(exited), 4)),
          by = .(ep_id, treat_perf)][order(ep_id, treat_perf)])
cat("\nregression-adjusted:\n")
print(etable(feols(xs_fml("exited"), att[smp_main == 1L], vcov = ~ advisor_id, notes = FALSE),
             digits = 3, fitstat = ~ n, keep = "treat"))
cat("\nNOTE: exit here means 'no observed position row afterwards'. In pos_m that\n")
cat("mixes full liquidation with leaving the bank -- they cannot be separated.\n")

## ---------------------------------------------------------------------------
## 6. placebo windows
## ---------------------------------------------------------------------------
cat("\n-------------------------------------------------------------\n")
cat("6. PLACEBO: the same cross section on random non-drawdown windows\n")
cat("-------------------------------------------------------------\n")

## a compact rebuild of 04+05 for an arbitrary (dd_start, dd_end) pair
trm <- load_dt("trades_m")[, .(Bp_ID, MDate, chf_sold_self)]
build_xs <- function(dd_start, dd_end, tag) {
  pre_month <- madd(dd_start, -1L)
  pre_start <- madd(dd_start, -P$pre_months)
  post_end  <- madd(dd_end,    P$post_months)
  if (pre_start < min(pan$MDate) || post_end > max(pan$MDate)) return(NULL)

  base <- pan[MDate == pre_month & observed == 1L & wealth >= P$min_wealth_pre &
                discretionary == 0L,
              .(Bp_ID, wealth_pre = wealth, log_w_pre = log_w, n_assets_pre = n_assets,
                ret_past12_pre = ret_past12, main_bank_pre = main_bank_yn,
                advisor_id = Hauptbetreuer_ID, eq_share_pre = eq_share,
                risky_share_pre = risky_share_c)]
  if (!nrow(base)) return(NULL)

  dd <- pan[Bp_ID %in% base$Bp_ID & MDate >= dd_start & MDate <= dd_end,
            .(treat_perf = as.integer(any(c_perf == 1L)),
              treat_adv  = as.integer(any(c_advice_a == 1L)),
              treat_cli  = as.integer(any(c_advice_c == 1L)),
              net_chf   = sum(buysell),
              ## boerse: did the CLIENT enter a sell order in this window?
              any_sell_self = as.integer(any(sold_self == 1L))), by = Bp_ID]
  ## the CHF amount behind it comes from trades_m, which is where the trade
  ## detail lives (it is deliberately not carried on the 5.9 m-row panel)
  ss <- trm[Bp_ID %in% base$Bp_ID & MDate >= dd_start & MDate <= dd_end,
            .(chf_sold_self = sum(chf_sold_self)), by = Bp_ID]
  dd <- ss[dd, on = "Bp_ID"]
  pw <- pan[Bp_ID %in% base$Bp_ID & MDate >= pre_start & MDate <= pre_month,
            .(contacts_pre = sum(c_advice == 1L)), by = Bp_ID]

  mkt <- idx[MDate >= dd_start & MDate <= post_end, prod(1 + fifelse(is.na(spi_ret), 0, spi_ret))]
  wend <- pan[MDate == post_end, .(Bp_ID, wealth_end = wealth)]
  eqend <- pan[MDate == dd_end, .(Bp_ID, eq_share_end = eq_share,
                                  risky_share_end = risky_share_c)]

  d <- Reduce(function(a, b) b[a, on = "Bp_ID"], list(base, dd, pw, wend, eqend))
  for (v in c("treat_perf","treat_adv","treat_cli","contacts_pre","net_chf",
              "any_sell_self","chf_sold_self"))
    d[is.na(get(v)), (v) := 0]
  d[is.na(wealth_end), wealth_end := 0]
  d[, `:=`(netflow_dd_w   = winsor(net_chf / wealth_pre),
           cf_gap_mkt_end_w = winsor(wealth_end / wealth_pre / mkt - 1),
           derisk_dd      = as.integer(-(net_chf / wealth_pre) > P$derisk_thresh),
           derisk_eq_dd   = as.integer(eq_share_end - eq_share_pre < -P$derisk_eq_drop),
           derisk_rk_dd   = as.integer(risky_share_end - risky_share_pre < -P$derisk_eq_drop),
           any_sell_self_dd = any_sell_self,
           sellflow_s_dd_w  = winsor(chf_sold_self / wealth_pre, c(0.00, 0.99)),
           tag = tag)]
  d[]
}

set.seed(P$seed)
## candidate pseudo-starts: months that are not inside any episode window
## seq(by = "month") from a month-END date overshoots (31 Aug + 1 month = 1 Oct),
## so step over month starts and snap back to month ends
in_ep <- unique(unlist(lapply(seq_len(nrow(ep)), function(i)
  as.character(eom(seq(floor_date(ep$pre_start[i], "month"),
                       floor_date(ep$post_end[i],  "month"), by = "month"))))))
cand <- pan[, sort(unique(MDate))]
cand <- cand[!(as.character(cand) %in% in_ep)]
cand <- cand[cand >= madd(min(pan$MDate), P$pre_months) &
               cand <= madd(max(pan$MDate), -(P$post_months + 3L))]

plac <- list()
for (k in seq_len(min(P$n_placebo, length(cand)))) {
  s <- cand[sample.int(length(cand), 1L)]
  d <- build_xs(s, madd(s, 2L), tag = format(s))     # 3-month pseudo drawdown
  if (is.null(d) || d[, sum(get(TRT))] < 50) next
  pfml <- function(y) as.formula(sprintf(
    "%s ~ %s + treat_cli + log_w_pre + n_assets_pre + contacts_pre +
     ret_past12_pre + main_bank_pre", y, TRT))
  plac[[format(s)]] <- data.table(
    start    = s,
    b_flow   = coef(feols(pfml("netflow_dd_w"),     d, vcov = ~ advisor_id, notes = FALSE))[TRT],
    b_derisk = coef(feols(pfml("derisk_dd"),        d, vcov = ~ advisor_id, notes = FALSE))[TRT],
    b_deq    = coef(feols(pfml("derisk_eq_dd"),     d, vcov = ~ advisor_id, notes = FALSE))[TRT],
    b_drk    = coef(feols(pfml("derisk_rk_dd"),     d, vcov = ~ advisor_id, notes = FALSE))[TRT],
    b_self   = coef(feols(pfml("any_sell_self_dd"), d, vcov = ~ advisor_id, notes = FALSE))[TRT],
    b_sflow  = coef(feols(pfml("sellflow_s_dd_w"),  d, vcov = ~ advisor_id, notes = FALSE))[TRT],
    b_gap    = coef(feols(pfml("cf_gap_mkt_end_w"), d, vcov = ~ advisor_id, notes = FALSE))[TRT])
}
plac <- rbindlist(plac)
if (nrow(plac)) {
  setorder(plac, start)
  cat("placebo windows drawn:", nrow(plac), "\n\n")
  print(plac[, .(start, b_flow = round(b_flow, 4), b_derisk = round(b_derisk, 4),
                 b_deq = round(b_deq, 4), b_drk = round(b_drk, 4),
                 b_self = round(b_self, 4), b_sflow = round(b_sflow, 4),
                 b_gap = round(b_gap, 4))])
  cat("\nplacebo distribution vs. the real episodes:\n")
  real <- c(net_flow       = coef(feols(xs_fml("netflow_dd_w"),      C, notes = FALSE))[[TRT]],
            P_derisk_flow  = coef(feols(xs_fml("derisk_dd"),         C, notes = FALSE))[[TRT]],
            P_derisk_eq    = coef(feols(xs_fml("derisk_eq_dd"),      C, notes = FALSE))[[TRT]],
            P_derisk_risky = coef(feols(xs_fml("derisk_rk_dd"),      C, notes = FALSE))[[TRT]],
            P_sell_self    = coef(feols(xs_fml("any_sell_self_dd"),  C, notes = FALSE))[[TRT]],
            sellflow_self  = coef(feols(xs_fml("sellflow_s_dd_w"),   C, notes = FALSE))[[TRT]],
            gap_vs_market  = coef(feols(xs_fml("cf_gap_mkt_end_w"),  C, notes = FALSE))[[TRT]])
  pl <- list(plac$b_flow, plac$b_derisk, plac$b_deq, plac$b_drk,
             plac$b_self, plac$b_sflow, plac$b_gap)
  print(data.table(
    coef              = names(real),
    real              = round(real, 4),
    placebo_mean      = round(sapply(pl, mean), 4),
    placebo_sd        = round(sapply(pl, sd), 4),
    pct_placebo_more_extreme =
      round(mapply(function(p, r) mean(if (r < 0) p <= r else p >= r), pl, real), 3)))
  cat("\nRead this as the crisis-specificity test. A real coefficient that sits inside\n")
  cat("the placebo distribution is NOT a drawdown effect -- it is what an advisor\n")
  cat("contact does in any month, and the paper cannot claim it is about crises.\n")
} else {
  cat("no placebo window produced a usable sample.\n")
}

## ---------------------------------------------------------------------------
## 7. bank revenue side
## ---------------------------------------------------------------------------
cat("\n-------------------------------------------------------------\n")
cat("7. BANK REVENUE SIDE\n")
cat("-------------------------------------------------------------\n")
cat("pos_m does carry per-client bank profit: Profit_Wertschriften, Profit_Fonds,\n")
cat("Profit_Depot, Profit_Vermoegensverwaltung. There is no product-level margin,\n")
cat("so 'rotation into higher-margin products' can only be tested as 'did bank\n")
cat("revenue from this client rise after an advised crisis trade'.\n\n")
cat("boerse$Kosten adds the other half of the picture: the commission charged on\n")
cat("each individual trade, never missing, CHF 153.9 m over 2011-2024. It is the\n")
cat("only revenue field in the data that is dated to the day and attached to a\n")
cat("specific order, so it is the one that can be measured INSIDE the drawdown\n")
cat("rather than over the year around it.\n\n")

cat("-- transaction revenue booked during the drawdown (clients in boerse) --\n")
CBR <- cle[smp_boerse == 1L]
print(CBR[, .(N = .N,
              kosten_chf = round(mean(kosten_dd), 1),
              kosten_bp  = round(1e4 * mean(kosten_dd_r, na.rm = TRUE), 1),
              sell_side  = round(mean(kosten_sell_dd), 1)),
          by = .(ep_id, treat_perf)][order(ep_id, treat_perf)])
print(etable(list(
  `commission / wealth (dd)` = feols(xs_fml("kosten_dd_r_w"), CBR,
                                     vcov = ~ advisor_id, notes = FALSE)),
  digits = 5, fitstat = ~ n, keep = "treat"))
cat("\nPositive here would mean advised clients trade MORE in the crash and pay for\n")
cat("it in commission; the Profit_* result below is the annual-aggregate version.\n\n")

pcols <- c("Bp_ID","MDate","Profit_Wertschriften","Profit_Fonds","Profit_Depot",
           "Profit_Vermoegensverwaltung")
prof <- setDT(read_parquet(POS_M_FILE, col_select = all_of(pcols)))
prof[, profit := rowSums(.SD, na.rm = TRUE),
     .SDcols = c("Profit_Wertschriften","Profit_Fonds","Profit_Depot",
                 "Profit_Vermoegensverwaltung")]

rev <- cle[smp_main == 1L, .(Bp_ID, ep_id, dd_start, dd_end, post_end, pre_start,
                             pre_month, wealth_pre, treat_perf, treat_adv, treat_cli, log_w_pre,
                             n_assets_pre, contacts_pre, ret_past12_pre, main_bank_pre,
                             advisor_id)]
pre_p  <- prof[rev, on = .(Bp_ID, MDate >= pre_start, MDate <= pre_month),
               .(Bp_ID, ep_id = i.ep_id, profit), allow.cartesian = TRUE][
                 , .(profit_pre = sum(profit, na.rm = TRUE)), by = .(Bp_ID, ep_id)]
post_p <- prof[rev, on = .(Bp_ID, MDate > dd_end, MDate <= post_end),
               .(Bp_ID, ep_id = i.ep_id, profit), allow.cartesian = TRUE][
                 , .(profit_post = sum(profit, na.rm = TRUE)), by = .(Bp_ID, ep_id)]
rev <- post_p[pre_p[rev, on = .(Bp_ID, ep_id)], on = .(Bp_ID, ep_id)]
rev[is.na(profit_pre), profit_pre := 0][is.na(profit_post), profit_post := 0]
rev[, `:=`(profit_post_r = winsor(profit_post / wealth_pre),
           profit_chg_r  = winsor((profit_post - profit_pre) / wealth_pre))]

print(rev[, .(N = .N,
              profit_pre = round(mean(profit_pre), 1),
              profit_post = round(mean(profit_post), 1),
              profit_post_bp = round(1e4 * mean(profit_post_r, na.rm = TRUE), 1)),
          by = .(ep_id, treat_perf)][order(ep_id, treat_perf)])
cat("\n(profit_post_bp = post-episode bank profit in basis points of pre-episode wealth)\n\n")
print(etable(list(
  `profit post / wealth` = feols(xs_fml("profit_post_r"), rev, vcov = ~ advisor_id, notes = FALSE),
  `change in profit`     = feols(xs_fml("profit_chg_r"),  rev, vcov = ~ advisor_id, notes = FALSE)
), digits = 4, fitstat = ~ n, keep = "treat"))

## ---------------------------------------------------------------------------
## 8. open items that pos_m cannot answer
## ---------------------------------------------------------------------------
cat("\n-------------------------------------------------------------\n")
cat("8. STILL NOT ANSWERABLE\n")
cat("-------------------------------------------------------------\n")
cat("  - a SCHEDULED vs. ad hoc flag on contacts. K_Performancebesprechung was\n")
cat("    tested as a proxy and is not one: only 18% of consecutive reviews are\n")
cat("    ~12 months apart, and review volume peaks in the crash month itself.\n")
cat("    Without it there is no exogenous variation in who gets called, and\n")
cat("    this is now the binding constraint on the whole project.\n")
cat("  - a bulk / mass-mailing flag (mail is excluded from treatment instead)\n")
cat("  - product-level MARGINS. boerse$Kosten now gives the commission on every\n")
cat("    individual trade, so transaction revenue is measured; what is still\n")
cat("    missing is the recurring margin per product, which is what 'rotation\n")
cat("    into higher-margin products' would need.\n")
cat("  - the liability side: all 4.4 m Kredit rows carry Vermoegen_CHF = 0, so\n")
cat("    mortgages exist as rows but not as values, and net wealth and leverage\n")
cat("    cannot be measured.\n")
cat("\nRESOLVED since the first version, all from the raw files:\n")
cat("  contact log (02b), equity share (02c), security prices and the frozen-\n")
cat("  holdings counterfactual (02c/04b), trade level and per-sale forgone\n")
cat("  return (02d), deposit balances (02e), and -- last -- the ORDER CHANNEL\n")
cat("  and per-trade COMMISSION, both of which were sitting unread in boerse.\n")

log_step("robustness written to results/07_robustness.txt")
sink()
