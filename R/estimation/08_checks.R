## =============================================================================
## 08_checks.R -- two targeted checks
##
##  A. P(sell) measured from the FLOW field (pos_m) vs. from BOOKED TRADES
##     (boerse), and then the booked trades split by ORDER CHANNEL. The flow
##     field is a position quantity change, so corporate actions, redemptions
##     and transfers register as "sells" in it. P(sell) is one of the outcomes
##     that fails the parallel-trends test, so it matters which source it comes
##     from -- and, once boerse$Medium is read, WHO entered the order: the
##     client through e-banking, the advisor, or the mandate desk.
##
##  B. Forensics on the equity-share pre-trend. The pooled event study shows a
##     STEP between rel_month -9 (-0.0157) and -8 (+0.0024), flat on either
##     side. A step, not a slope, points at sample composition rather than
##     behaviour.
##
## Output: results/08_checks.txt, results/figures/08_*.pdf
## =============================================================================

source("00_setup.R")
log_init("08_checks")

stk <- load_dt("stk")
cle <- load_dt("cle")
ep  <- load_dt("episodes")[usable == TRUE]
stk <- cle[, .(Bp_ID, ep_id, smp_cycle)][stk, on = .(Bp_ID, ep_id)]

TRT <- "treat_perf"
S <- stk[smp_main == 1L]

sink(file.path(RESULTS, "08_checks.txt"), split = TRUE)
on.exit(sink(), add = TRUE)

mk_fml <- function(y, fe = "ci + te", trt = TRT) as.formula(sprintf(
  "%s ~ i(rel_month, %s, ref = -1) + i(rel_month, treat_cli, ref = -1) +
   (log_w_pre + n_assets_pre + contacts_pre) : factor(rel_month) | %s", y, trt, fe))

es_tab <- function(m, var = TRT) {
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, 2:5, c("est","se","t","p"))
  ct <- ct[grepl(paste0(":", var, "$"), term)]
  ct[, rel_month := as.integer(sub("^rel_month::(-?\\d+):.*$", "\\1", term))]
  setorder(ct, rel_month)
  ct[, .(rel_month, est = round(est, 5), se = round(se, 5), t = round(t, 2),
         sig = fcase(p < .01, "***", p < .05, "**", p < .1, "*", default = ""))]
}
PRE_RX <- sprintf("^rel_month::-(1[0-2]|[2-9]):%s$", TRT)
pretrend <- function(m, label) {
  w <- tryCatch(wald(m, keep = PRE_RX, print = FALSE), error = function(e) NULL)
  if (is.list(w) && !is.null(w$stat))
    cat(sprintf("%-40s F = %8.3f  df1 = %3.0f  p = %.4f\n", label, w$stat, w$df1, w$p))
  else cat(sprintf("%-40s (no pre-period test available)\n", label))
}

## =============================================================================
cat("=============================================================\n")
cat(" A. P(sell): FLOW FIELD vs. BOOKED TRADES\n")
cat("=============================================================\n")

S[, sold_flow  := as.integer(sell < 0)]      # pos_m: DA_Mengenabweichung < 0
S[, sold_trade := as.integer(n_sells > 0)]   # boerse: a sell was actually booked
## sold_dec / sold_self / sold_adv / sold_man come from 02d via 02_clean: a
## booked sell that is a real decision, and then split by boerse$Medium, i.e. by
## who physically entered the order.

cat("\nhow often each measure fires (client-months, main sample):\n")
print(S[, .(flow  = round(mean(sold_flow), 4),
            trade = round(mean(sold_trade), 4),
            both  = round(mean(sold_flow == 1L & sold_trade == 1L), 4),
            flow_only  = round(mean(sold_flow == 1L & sold_trade == 0L), 4),
            trade_only = round(mean(sold_flow == 0L & sold_trade == 1L), 4))])
cat("\nflow_only rows are quantity changes with no trade behind them: corporate\n")
cat("actions, fund redemptions, transfers, in-kind moves.\n")

cat("\nagreement by phase:\n")
print(S[, .(n = .N, flow = round(mean(sold_flow), 4), trade = round(mean(sold_trade), 4),
            disagree = round(mean(sold_flow != sold_trade), 4)), by = phase])

cat("\nthe booked sells split by ORDER CHANNEL, by phase:\n")
print(S[, .(n = .N,
            decision = round(mean(sold_dec),  4),
            self     = round(mean(sold_self), 4),
            advisor  = round(mean(sold_adv),  4),
            mandate  = round(mean(sold_man),  4)), by = phase])
cat("Only `self` is unambiguously the client's own decision to sell. The flow\n")
cat("field pools all four with corporate actions on top.\n")

m_flow  <- feols(mk_fml("sold_flow"),  S, vcov = ~ advisor_id, notes = FALSE)
m_trade <- feols(mk_fml("sold_trade"), S, vcov = ~ advisor_id, notes = FALSE)
m_dec   <- feols(mk_fml("sold_dec"),   S, vcov = ~ advisor_id, notes = FALSE)
m_self  <- feols(mk_fml("sold_self"),  S, vcov = ~ advisor_id, notes = FALSE)
m_adv   <- feols(mk_fml("sold_adv"),   S, vcov = ~ advisor_id, notes = FALSE)
m_man   <- feols(mk_fml("sold_man"),   S, vcov = ~ advisor_id, notes = FALSE)

cat("\n--- P(sell), FLOW field ---\n");        print(es_tab(m_flow))
cat("\n--- P(sell), BOOKED TRADES ---\n");     print(es_tab(m_trade))
cat("\n--- P(sell), DECISION only ---\n");     print(es_tab(m_dec))
cat("\n--- P(sell), CLIENT-ENTERED (e-banking) ---\n"); print(es_tab(m_self))
cat("\n--- P(sell), ADVISOR-ENTERED ---\n");   print(es_tab(m_adv))
cat("\n--- P(sell), MANDATE DESK ---\n");      print(es_tab(m_man))

cat("\nparallel-trends test on the pre-period:\n")
pretrend(m_flow,  "P(sell), flow field")
pretrend(m_trade, "P(sell), booked trades")
pretrend(m_dec,   "P(sell), decision only")
pretrend(m_self,  "P(sell), client-entered")
pretrend(m_adv,   "P(sell), advisor-entered")
pretrend(m_man,   "P(sell), mandate desk")

if (pdf_ok(file.path(FIG_DIR, "08_es_psell_compare.pdf"), width = 9, height = 5)) {
  iplot(list(m_flow, m_trade), main = "P(sell): flow field vs. booked trades",
        xlab = "months since dd_start (-1 = last pre-drawdown month)")
  legend("topleft", c("flow field (pos_m)", "booked trades (boerse)"),
         col = 1:2, pch = 16, bty = "n")
  dev.off()
}

if (pdf_ok(file.path(FIG_DIR, "08_es_psell_channel.pdf"), width = 9, height = 5)) {
  iplot(list(m_self, m_adv, m_man), main = "P(sell) by who entered the order",
        xlab = "months since dd_start (-1 = last pre-drawdown month)")
  legend("topleft", c("client (e-banking)", "advisor", "mandate desk"),
         col = 1:3, pch = 16, bty = "n")
  abline(v = 0, lty = 3); abline(h = 0, col = "grey60")
  dev.off()
}

cat("\n-- raw monthly P(sell) by channel and treatment, drawdown months only --\n")
print(S[phase == "drawdown", .(n = .N,
        self_ctrl = round(mean(sold_self[get(TRT) == 0L]), 4),
        self_trt  = round(mean(sold_self[get(TRT) == 1L]), 4),
        adv_ctrl  = round(mean(sold_adv[get(TRT)  == 0L]), 4),
        adv_trt   = round(mean(sold_adv[get(TRT)  == 1L]), 4)), by = ep_id][order(ep_id)])

## =============================================================================
cat("\n\n=============================================================\n")
cat(" B. THE EQUITY-SHARE PRE-TREND STEP\n")
cat("=============================================================\n")

cat("\n-- B1. is the estimation sample changing by rel_month? --\n")
cat("non-missing d_eq_pre, by rel_month and episode:\n")
comp <- S[, .(n_rows = .N, n_obs = sum(!is.na(d_eq_pre)),
              share_obs = round(mean(!is.na(d_eq_pre)), 4)),
          by = .(ep_id, rel_month)][order(ep_id, rel_month)]
print(dcast(comp, rel_month ~ ep_id, value.var = "share_obs"))

cat("\n-- B2. RAW mean of d_eq_pre by rel_month, episode and treatment --\n")
cat("(no controls, no fixed effects: this shows the step directly)\n")
raw <- S[!is.na(d_eq_pre), .(mean_d_eq = round(mean(d_eq_pre), 5), n = .N),
         by = .(ep_id, rel_month, treat = get(TRT))]
print(dcast(raw[rel_month <= 0], rel_month ~ ep_id + treat, value.var = "mean_d_eq"))

cat("\n-- B3. event study run separately per episode --\n")
for (e in ep$ep_id) {
  m <- feols(mk_fml("d_eq_pre", fe = "Bp_ID + MDate"), S[ep_id == e],
             vcov = ~ advisor_id, notes = FALSE)
  cat("\n===", e, "===\n")
  print(es_tab(m)[rel_month <= 0])
  pretrend(m, paste("equity share,", e))
}

cat("\n-- B4. the same in CALENDAR time: mean eq_share by month --\n")
cat("if the step is a data break it sits at a fixed DATE, not a fixed rel_month\n")
cal <- S[!is.na(eq_share), .(eq = round(mean(eq_share), 4), n = .N),
         by = .(ep_id, MDate)][order(ep_id, MDate)]
for (e in ep$ep_id) {
  cat("\n---", e, "---\n")
  print(cal[ep_id == e & MDate <= ep[ep_id == e, dd_start]])
}

cat("\n-- B5. is it in the source data? monthly composition from portfolio_m --\n")
pfm <- load_dt("portfolio_m")
agg <- pfm[, .(clients = .N,
               eq_share = round(mean(eq_share, na.rm = TRUE), 4),
               share_v_equity_zero = round(mean(v_equity == 0), 4),
               mean_n_assets = round(mean(n_assets_all), 2)), by = MDate][order(MDate)]
print(agg[MDate %between% as.Date(c("2014-06-30","2015-06-30"))])
print(agg[MDate %between% as.Date(c("2019-01-31","2019-12-31"))])
print(agg[MDate %between% as.Date(c("2020-10-31","2021-10-31"))])

## B6. do consecutive episode windows overlap? Written over CONSECUTIVE PAIRS
## rather than against hard-coded ep_ids: ep_id is built from the episode's ROW
## POSITION in 03 (sprintf("ep%d_...", .I)), so changing P$smp_start renumbers
## every episode and any hard-coded name silently stops matching -- cat() prints
## nothing for a zero-length lookup rather than failing. See NOTES section 8.
cat("\n-- B6. do consecutive episode windows overlap? --\n")
setorder(ep, dd_start)
print(ep[, .(ep_id, pre_start, pre_month, dd_start, dd_end, post_end)])
if (nrow(ep) > 1L) for (i in 2:nrow(ep)) {
  prev <- ep[i - 1L]; cur <- ep[i]
  cat(sprintf("\n%s post_end = %s | %s pre_start = %s -> %s\n",
              prev$ep_id, format(prev$post_end), cur$ep_id, format(cur$pre_start),
              if (cur$pre_start <= prev$post_end) "OVERLAP" else "clean"))
  cat(sprintf("   %s rel_month at %s's post_end: %d\n",
              cur$ep_id, prev$ep_id, mdiff(prev$post_end, cur$dd_start)))
}

log_step("checks written to results/08_checks.txt")
sink()
