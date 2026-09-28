## =============================================================================
## 06_specs.R -- the four specifications
##
## Order matters: behavioural outcomes run FIRST. If advice does not change
## trading, any return result is selection and the paper has to say so.
##
## Output: results/tables/*.tex, results/figures/*.pdf, results/06_specs.txt
##         results/cache/models_*.rds
## =============================================================================

source("00_setup.R")
log_init("06_specs")

stk <- load_dt("stk")
cle <- load_dt("cle")
ep  <- load_dt("episodes")[usable == TRUE]

## flags created in 05 after stk was written
## smp_mb belongs to 05, alongside smp_cycle / smp_boerse. It is recomputed here
## if absent so that 06 can be re-run on its own against an older cle -- the
## definition is one line and both inputs are already frozen at the pre-episode
## month, so there is no way for the two to disagree.
if (!"smp_mb" %in% names(cle))
  cle[, smp_mb := as.integer(smp_main == 1L & main_bank_pre == 1L)]

stk <- cle[, .(Bp_ID, ep_id, smp_traders, smp_cycle, smp_boerse, smp_mb,smp_mb_exdisc,
               derisk_dd, any_trade_dd)][stk, on = .(Bp_ID, ep_id)]

## HEADLINE TREATMENT: a portfolio review meeting during the drawdown.
## Set TRT to "treat_adv" (any advisor-initiated advice contact) or "treat_advice"
## to re-run everything on the broader definitions -- they are never pooled.
TRT <- "treat_perf"

## monthly behavioural outcomes
## `sold` is the pos_m FLOW field: a position quantity change, so it also fires
## on corporate actions, transfers and in-kind moves. The boerse measures below
## are what someone actually did, and `sold_self` is what the CLIENT did.
stk[, sold   := as.integer(sell < 0)]
stk[, bought := as.integer(buy  > 0)]
stk[, sold_trade := sold_d]              # from boerse, not from the flow field

sink(file.path(RESULTS, "06_specs.txt"), split = TRUE)
on.exit(sink(), add = TRUE)

cat("=============================================================\n")
cat(" SPECIFICATIONS\n")
cat(" clustering: advisor_id (Hauptbetreuer_ID at the pre-episode month)\n")
cat("=============================================================\n\n")

## ---------------------------------------------------------------------------
## estimation samples
## ---------------------------------------------------------------------------
S_main   <- stk[smp_main  == 1L]   # non-discretionary at the pre-episode month
S_cycle  <- stk[smp_cycle == 1L]   # ... and already on a portfolio-review cycle
S_mb     <- stk[smp_mb    == 1L]   # ... and this bank is their MAIN bank
S_mb_exdisc <- stk[smp_mb_exdisc == 1L]
## ---------------------------------------------------------------------------
## EVENT WINDOW -- keep the stack BALANCED across rel_month
##
## Blocks have very different lengths, so the stacked panel thins out as
## rel_month grows: with the current blocks all 6 contribute only at -7..+9,
## two reach +27 and one reaches +35. Any coefficient outside the balanced core
## is identified off a shrinking subset of EPISODES, so the estimate steps
## whenever a block drops out -- which is exactly what the kinks in
## 06a_es_netflow.pdf were (visible at +9/+10, +14/+15 and +27/+28).
## P$es_balance keeps only the rel_months where every usable block is present;
## P$es_window is an extra hard cap. Both are reported below, and the dropped
## range stays in the cached coefficient tables, so nothing is hidden.
## ---------------------------------------------------------------------------
keep_rel <- es_window(stk, ep)
cat("
")

S_main  <- S_main [rel_month %in% keep_rel]
S_cycle <- S_cycle[rel_month %in% keep_rel]
S_mb    <- S_mb   [rel_month %in% keep_rel]
S_mb_exdisc    <- S_mb_exdisc[rel_month %in% keep_rel]

## smp_boerse (covered by the trade file) is reported in the cross section only,
## so it is counted here rather than materialised as a fourth 1 m-row copy.

cat("estimation samples (headline treatment: ", TRT, ")\n", sep = "")
print(data.table(
  sample = c("main (non-discretionary)", "... main bank (Hauptbankkunde)",
             "... already on a review cycle", "... covered by boerse (xs only)"),
  rows = c(nrow(S_main), nrow(S_mb), nrow(S_cycle), stk[smp_boerse == 1L, .N]),
  client_episodes = c(uniqueN(S_main$ci), uniqueN(S_mb$ci), uniqueN(S_cycle$ci),
                      stk[smp_boerse == 1L, uniqueN(ci)]),
  treated = c(uniqueN(S_main[get(TRT) == 1L]$ci), uniqueN(S_mb[get(TRT) == 1L]$ci),
              uniqueN(S_cycle[get(TRT) == 1L]$ci),
              stk[smp_boerse == 1L & get(TRT) == 1L, uniqueN(ci)])))
cat("\nsmp_mb halves the sample. It is the sample in which every wealth-SCALED\n")
cat("outcome has the right denominator: 6.3% of non-main-bank clients hold no\n")
cat("cash account here at all, so risky_share_c is ~1 by construction for them\n")
cat("and the 20%/5% thresholds are measured against a fraction of true wealth.\n\n")

## ---------------------------------------------------------------------------
## (a) stacked event study -- main
##     All covariates frozen at the pre-episode month and interacted with
##     rel_month. Contemporaneous wealth is NEVER a control: it is an outcome.
## ---------------------------------------------------------------------------

## the treatment dummies are constant within ci, so they enter only through the
## rel_month interactions; the level terms would be absorbed by the ci fixed effect
mk_fml <- function(y, fe, trt = TRT) as.formula(sprintf(
  "%s ~ i(rel_month, %s, ref = -1) + i(rel_month, treat_advice_c_p, ref = -1) +
   (log_w_pre + n_assets_pre + contacts_pre + anlagepaket_pre) : factor(rel_month) | %s", y, trt, fe))

mk_fml <- function(y, fe, trt = TRT) as.formula(sprintf(
  "%s ~ i(rel_month, %s, ref = -1) +
   (treat_advice_c_p + log_w_pre + n_assets_pre + contacts_pre + anlagepaket_pre) : factor(rel_month) | %s", y, trt, fe))

mk_fml <- function(y, fe, trt = TRT) as.formula(sprintf(
  "%s ~ i(rel_month, %s, ref = -1) +
   (treat_cli + log_w_pre + n_assets_pre + contacts_pre + anlagepaket_pre + segment_pre ) : factor(rel_month) | %s", y, trt, fe))
 
# mk_fml <- function(y, fe, trt = TRT) as.formula(sprintf(
#   "%s ~ i(rel_month, %s, ref = -1) +
#    (treat_cli + log_w_pre + n_assets_pre + contacts_pre + anlagepaket_pre) : factor(rel_month) | %s", y, trt, fe))


# mk_fml <- function(y, fe, trt = TRT) as.formula(sprintf(
#   "%s ~ i(rel_month, %s, ref = -1) +
#    (log_w_pre + n_assets_pre + contacts_pre) : factor(rel_month) | %s", y, trt, fe))


es_fml <- function(y) mk_fml(y, "ci + te")

es_fml <- function(y) mk_fml(y, "Bp_ID + ep_id")


## compact printer: only the rel_month x treatment coefficients, which is all the
## event study is about (the 100+ covariate interactions are never read)
es_print <- function(m, var = TRT) {
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, 2:5, c("est", "se", "t", "p"))
  ct <- ct[grepl(paste0(":", var, "$"), term)]
  ct[, rel_month := as.integer(sub("^rel_month::(-?\\d+):.*$", "\\1", term))]
  setorder(ct, rel_month)
  ct[, .(rel_month, est = round(est, 5), se = round(se, 5),
         t = round(t, 2), sig = fcase(p < .01, "***", p < .05, "**", p < .1, "*", default = ""))]
}

## a fitted fixest object on 1.4m rows is ~150 MB, so nothing bigger than a
## coefficient table is ever kept: each model is printed, plotted and reduced to a
## compact summary in one go, then dropped.
## The pre-period regex is built AT CALL TIME, from the CURRENT TRT and the
## rel_months actually in the window. It used to be a constant, fixed to
## "treat_perf" and to rel_month -12..-2. Both assumptions now break: TRT is
## reassigned further down, and the balanced window starts at -7. A `keep`
## pattern that matches nothing makes wald() return an empty test, `compact()`
## stores wald_pre = NULL, and 07 prints "no pre-period coefficients estimated"
## for every outcome -- a silent loss of the parallel-trends test, not an error.
pre_rx <- function(trt = TRT) {
  pre <- sort(keep_rel[keep_rel < -1L])
  if (!length(pre)) return(NULL)
  sprintf("^rel_month::(%s):%s$", paste(pre, collapse = "|"), trt)
}
compact <- function(m) {
  ## wald() returns a list normally, but degrades to an atomic vector when the
  ## clustered VCOV had to be repaired (not positive semi-definite)
  rx <- pre_rx()
  w <- if (is.null(rx)) NULL else
    tryCatch(wald(m, keep = rx, print = FALSE), error = function(e) NULL)
  ok <- is.list(w) && !is.null(w$stat) && !is.null(w$p)
  list(ct = as.data.table(coeftable(m), keep.rownames = "term"),
       n = nobs(m), r2 = fitstat(m, "r2")$r2,
       wald_pre = if (ok) c(stat = w$stat, df1 = w$df1, p = w$p) else NULL)
}

SKIPPED_FIGS <- character(0)
try_plot <- function(plot_file, plot_title, m) {
  if (!pdf_ok(file.path(FIG_DIR, plot_file), width = 8, height = 5)) {
    SKIPPED_FIGS <<- c(SKIPPED_FIGS, plot_file)
    return(invisible(FALSE))
  }
  on.exit(dev.off(), add = TRUE)
  iplot(m, main = plot_title,
        xlab = "months since dd_start (-1 = last pre-drawdown month)")
  abline(v = 0, lty = 3); abline(h = 0, col = "grey60")
  invisible(TRUE)
}

run_es <- function(y, dat, label, fml = es_fml, plot_file = NULL, plot_title = NULL) {
  m <- feols(fml(y), data = dat, vcov = ~ advisor_id, notes = FALSE)
  cat("\n--- event study:", y, "|", label, "  (n =", format(nobs(m), big.mark = "'"), ") ---\n")
  cat("coefficients on rel_month x ", TRT, " (ref = -1):\n", sep = "")
  print(es_print(m, TRT))
  cat("rel_month x treat_cli (client-initiated contact = panic proxy):\n")
  print(es_print(m, "treat_cli"))
  ## A figure open in a PDF viewer holds a Windows lock, and pdf() then aborts.
  ## That must not destroy a 9-minute regression run: warn, skip the plot, keep
  ## the coefficients. The end of the script lists anything that was skipped.
  if (!is.null(plot_file)) try_plot(plot_file, plot_title, m)
  out <- compact(m)
  rm(m); gc(verbose = FALSE)
  out
}

cat("-------------------------------------------------------------\n")
cat("STEP 1 -- BEHAVIOUR (run before any return regression)\n")
cat("-------------------------------------------------------------\n")

TRT <- "treat_perf_inv_a_p"
TRT <- "treat_inv"
# s_beh <- S_mb
s_beh <- S_mb_exdisc
# s_beh <- S_main

# s_beh[,treat := as.numeric(treat_perf_inv_a_p==1 | smp_main==1)]

eps <- c("ep1_201305", "ep2_201501", "ep3_201508", "ep6_201801", "ep7_201811", "ep9_202002", "ep10_202201", "ep12_202310")

s_beh <- s_beh[ep_id %in% eps]

s_beh <- s_beh[segment_pre %in% c("Beratungszentrum","PK-Team","natürliche Personen PB")]

# TRT <- "treat"

es_beh <- list()
es_beh[["net flow / BoM"]]    <- run_es("netflow_r", s_beh, "main",
                                        plot_file = "06a_es_netflow.pdf",
                                        plot_title = "Net flow / beginning-of-month value")
## P(sell), four ways. They are NOT the same outcome and 08_checks shows the
## first one is the least defensible: the flow field is a position quantity
## change, so it fires on corporate actions, redemptions and transfers too.
##   sold        pos_m flow field    -- kept for comparison, was the headline
##   sold_trade  a booked trade      -- boerse
##   sold_dec    ... a real decision -- boerse, corporate actions removed
##   sold_self   ... entered by the CLIENT through e-banking (boerse$Medium)
## The last one is the panic-selling measure: it is the only one that cannot be
## an advisor or a mandate desk acting on the client's behalf.
## Three of the four are run here; 08_checks.R runs all four side by side with
## the parallel-trends test, so `sold_trade` is not repeated at this cost
## (each of these is a 1.4 m-row model).
# es_beh[["P(sell), flow field"]] <- run_es("sold", s_beh, "main",
#                                         plot_file = "06a_es_psell.pdf",
#                                         plot_title = "P(sell in month), pos_m flow field")
# es_beh[["P(sell), self-directed"]] <- run_es("sold_self", s_beh, "main",
#                                         plot_file = "06a_es_psell_self.pdf",
#                                         plot_title = "P(client sells through e-banking)")
# es_beh[["P(sell), advisor channel"]] <- run_es("sold_adv", s_beh, "main",
#                                         plot_file = "06a_es_psell_adv.pdf",
#                                         plot_title = "P(sell entered by the advisor)")
# es_beh[["sell flow, self-directed"]] <- run_es("sellflow_s", s_beh, "main",
#                                         plot_file = "06a_es_sellflow_self.pdf",
#                                         plot_title = "Client-entered sales / beginning-of-month value")
# es_beh[["P(buy)"]]            <- run_es("bought", s_beh, "main",
#                                         plot_file = "06a_es_pbuy.pdf",
#                                         plot_title = "P(buy in month)")
## the design's own de-risking measure: the equity WEIGHT, not the flow.
## Only available since pos_full was added.
es_beh[["equity share"]]      <- run_es("d_eq_pre", s_beh, "main",
                                        plot_file = "06a_es_eqshare.pdf",
                                        plot_title = "Equity share, change from the pre-episode level")
## the risky weight in TOTAL wealth -- only defined once deposits are in
es_beh[["risky share"]]       <- run_es("d_risky_pre", s_beh, "main",
                                        plot_file = "06a_es_riskyshare.pdf",
                                        plot_title = "Risky share of total wealth, change from pre-episode")
## commission paid, month by month: the revenue side dated to the day rather
## than aggregated over the year (boerse$Kosten)
# es_beh[["commission / BoM"]]  <- run_es("cost_r", s_beh, "main",
#                                         plot_file = "06a_es_cost.pdf",
#                                         plot_title = "Commission paid / beginning-of-month value")
# es_beh[["net flow, on cycle"]] <- run_es("netflow_r", s_beh, "review-cycle clients")
# ## the panic measure on the sample where it is best measured: main-bank clients.
# ## The cross section says the effect is roughly twice as large there.
# es_beh[["P(sell) self, main bank"]] <- run_es("sold_self", S_mb, "main-bank clients",
#                                         plot_file = "06a_es_psell_self_mb.pdf",
#                                         plot_title = "P(client sells through e-banking), main-bank clients")

cat("\n-------------------------------------------------------------\n")
cat("STEP 2 -- PERFORMANCE\n")
cat("-------------------------------------------------------------\n")

es_perf <- list()
es_perf[["d price pf"]] <- run_es("dprice", s_beh, "main",
                                           plot_file = "06a_es_dprice.pdf",
                                           plot_title = "pf d price")
es_perf[["wealth / pre-wealth"]] <- run_es("w_tot_rel", s_beh, "main",
                                           plot_file = "06a_es_wrel.pdf",
                                           plot_title = "Wealth relative to pre-episode wealth")
es_perf[["gap vs. market"]]      <- run_es("cf_gap_mkt", s_beh, "main",
                                           plot_file = "06a_es_gapmkt.pdf",
                                           plot_title = "Wealth gap vs. market-drifted counterfactual")
# es_perf[["gap, frozen holdings"]] <- run_es("cf_gap_hold", S_main, "main",
#                                            plot_file = "06a_es_gaphold.pdf",
#                                            plot_title = "Wealth gap vs. the frozen-holdings counterfactual")
# es_perf[["monthly return"]]      <- run_es("pf_ret", S_main, "main")
# es_perf[["gap, on cycle"]]       <- run_es("cf_gap_mkt", S_cycle, "review-cycle clients")

saveRDS(list(beh = es_beh, perf = es_perf), file.path(CACHE, "models_es.rds"))

## ---------------------------------------------------------------------------
## (b) within client across episodes -- identified off switchers
## ---------------------------------------------------------------------------

cat("\n-------------------------------------------------------------\n")
cat("(b) WITHIN CLIENT ACROSS EPISODES  | Bp_ID + te\n")
cat("-------------------------------------------------------------\n")
sw_share <- cle[smp_main == 1L & n_ep_present > 1L, mean(sw_type == "switcher")]
cat(sprintf("switcher share among clients present in >1 episode: %.3f\n", sw_share))
cat(if (sw_share < 0.02)
  "-> too few switchers; (b) is reported for completeness only, (a) is the headline.\n"
  else "-> switcher share is material; (b) is a genuine within-client check.\n")

b_fml <- function(y) mk_fml(y, "Bp_ID + te")

es_b <- list()
es_b[["net flow"]]   <- run_es("netflow_r",  S_main, "within client", fml = b_fml)
es_b[["wealth/pre"]] <- run_es("w_rel",      S_main, "within client", fml = b_fml)
es_b[["gap vs mkt"]] <- run_es("cf_gap_mkt", S_main, "within client", fml = b_fml)

## ---------------------------------------------------------------------------
## (c) within advisor
## ---------------------------------------------------------------------------

cat("\n-------------------------------------------------------------\n")
cat("(c) WITHIN ADVISOR  | ci + te + advisor x episode\n")
cat("-------------------------------------------------------------\n")

c_fml <- function(y) mk_fml(y, "ci + te + ae")

es_c <- list()
es_c[["net flow"]]   <- run_es("netflow_r",  S_main, "within advisor", fml = c_fml)
es_c[["gap vs mkt"]] <- run_es("cf_gap_mkt", S_main, "within advisor", fml = c_fml)

saveRDS(list(b = es_b, c = es_c), file.path(CACHE, "models_bc.rds"))
rm(es_b, es_c, es_beh, es_perf, S_main, S_cycle, S_mb); gc()

## ---------------------------------------------------------------------------
## (d) cross section at dd_end -- one row per client x episode
##     free of the overlapping-window correlation of the stacked panel
## ---------------------------------------------------------------------------

cat("\n-------------------------------------------------------------\n")
cat("(d) CROSS SECTION, one row per client x episode\n")
cat("-------------------------------------------------------------\n")
# 
# C  <- cle[smp_main  == 1L]
# CY <- cle[smp_cycle == 1L]
# CT <- cle[smp_traders == 1L]
# CB <- cle[smp_boerse  == 1L]
# CM <- cle[smp_mb      == 1L]   # main-bank clients
# CX <- cle[smp_main == 1L & main_bank_pre == 0L]   # the complement, for contrast
# 
# xs_fml <- function(y, fe = "| ep_id + Bp_ID", trt = TRT) as.formula(sprintf(
#   "%s ~ %s + treat_cli + log_w_pre + n_assets_pre + contacts_pre +
#    ret_past12_pre + main_bank_pre %s", y, trt, fe))
# 
# C  <- cle[smp_main  == 1L]
# C <- CM
# CYM <- cle[smp_cycle==1L & smp_mb==1L]
# C <- CYM
# 
# TRT <- "treat_perf_a"
# TRT <- "treat_adv"
# 
# xs <- list()
# ## behaviour first
# xs[["net flow (dd)"]]    <- feols(xs_fml("netflow_dd_w"),  C,  vcov = ~ advisor_id, notes = FALSE)
# xs[["P(de-risk)"]]       <- feols(xs_fml("derisk_dd"),     C,  vcov = ~ advisor_id, notes = FALSE)
# xs[["P(full liq.)"]]     <- feols(xs_fml("full_liq"),      C,  vcov = ~ advisor_id, notes = FALSE)
# ## de-risking on portfolio WEIGHTS (pos_full) rather than on flows
# xs[["d equity share"]]   <- feols(xs_fml("d_eq_dd_w"),     C,  vcov = ~ advisor_id, notes = FALSE)
# xs[["P(equity cut)"]]    <- feols(xs_fml("derisk_eq_dd"),  C,  vcov = ~ advisor_id, notes = FALSE)
# ## the same on the risky weight in TOTAL wealth, and where the money went
# xs[["d risky share"]]    <- feols(xs_fml("d_risky_dd_w"), C,  vcov = ~ advisor_id, notes = FALSE)
# xs[["P(risky cut)"]]     <- feols(xs_fml("derisk_rk_dd"), C,  vcov = ~ advisor_id, notes = FALSE)
# xs[["cash inflow"]]      <- feols(xs_fml("d_cash_dd_w"),  C,  vcov = ~ advisor_id, notes = FALSE)
# xs[["P(sold into cash)"]]<- feols(xs_fml("ever_in_cash"), C,  vcov = ~ advisor_id, notes = FALSE)
# xs[["P(left the bank)"]] <- feols(xs_fml("ever_left_bank"), C, vcov = ~ advisor_id, notes = FALSE)
# ## de-risking measured on BOOKED ORDERS rather than the flow field, and split by
# ## who entered them. P(sell, self) is the panic-selling outcome proper.
# xs[["net flow, booked"]] <- feols(xs_fml("netflow_d_dd_w"), C, vcov = ~ advisor_id, notes = FALSE)
# xs[["P(de-risk, booked)"]]<- feols(xs_fml("derisk_d_dd"),   C, vcov = ~ advisor_id, notes = FALSE)
# xs[["P(sell)"]]     <- feols(xs_fml("any_sell_dec_dd"), C, vcov = ~ advisor_id, notes = FALSE)
# # xs[["P(sell, self)"]]    <- feols(xs_fml("any_sell_self_dd"),C, vcov = ~ advisor_id, notes = FALSE)
# # xs[["P(sell, advisor)"]] <- feols(xs_fml("any_sell_adv_dd"), C, vcov = ~ advisor_id, notes = FALSE)
# # xs[["sell flow, self"]]  <- feols(xs_fml("sellflow_s_dd_w"), C, vcov = ~ advisor_id, notes = FALSE)
# xs[["sell flow"]]  <- feols(xs_fml("sellflow_dd_w"), C, vcov = ~ advisor_id, notes = FALSE)
# # xs[["P(de-risk, self)"]] <- feols(xs_fml("derisk_s_dd"),     C, vcov = ~ advisor_id, notes = FALSE)
# ## then performance
# xs[["gap mkt, dd leg"]]  <- feols(xs_fml("cf_gap_mkt_dd_w"),  C, vcov = ~ advisor_id, notes = FALSE)
# xs[["gap mkt, recovery"]]<- feols(xs_fml("cf_gap_mkt_rec_w"), C, vcov = ~ advisor_id, notes = FALSE)
# xs[["gap mkt, total"]]   <- feols(xs_fml("cf_gap_mkt_end_w"), C, vcov = ~ advisor_id, notes = FALSE)
# xs[["ret next 12m"]]     <- feols(xs_fml("ret_next12_from_dd_w"), C, vcov = ~ advisor_id, notes = FALSE)
# ## the design's counterfactual, at last: frozen holdings drifted with prices
# xs[["gap hold, dd leg"]] <- feols(xs_fml("cf_gap_hold_dd_w"),  C, vcov = ~ advisor_id, notes = FALSE)
# xs[["gap hold, recov."]] <- feols(xs_fml("cf_gap_hold_rec_w"), C, vcov = ~ advisor_id, notes = FALSE)
# xs[["gap hold, total"]]  <- feols(xs_fml("cf_gap_hold_end_w"), C, vcov = ~ advisor_id, notes = FALSE)
# ## the same gap on TOTAL wealth, i.e. crediting a seller with the cash they hold
# xs[["gap hold+cash, dd"]]   <- feols(xs_fml("cf_gap_holdw_dd_w"),  C, vcov = ~ advisor_id, notes = FALSE)
# xs[["gap hold+cash, total"]]<- feols(xs_fml("cf_gap_holdw_end_w"), C, vcov = ~ advisor_id, notes = FALSE)
# # ## per-sale forgone return, sellers during the drawdown only
# # CS <- cle[smp_main == 1L & !is.na(forgone_vw_12)]
# # xs[["forgone 6m"]]       <- feols(xs_fml("forgone_vw_6"),  CS, vcov = ~ advisor_id, notes = FALSE)
# # xs[["forgone 12m"]]      <- feols(xs_fml("forgone_vw_12"), CS, vcov = ~ advisor_id, notes = FALSE)
# # ## ... on the client's OWN orders only
# # CSS <- cle[smp_main == 1L & !is.na(forgone_self_12)]
# # xs[["forgone 12m, self"]] <- feols(xs_fml("forgone_self_12"), CSS, vcov = ~ advisor_id, notes = FALSE)
# 

## ---------------------------------------------------
## playground

xs_fml <- function(y, trt = TRT, spec=SPEC) as.formula(sprintf(
  spec, y, trt))


# SPEC <- "%s ~ %s + treat_advice_c_p + log_w_pre + n_assets_pre + contacts_pre + ret_past12_pre + anlagepaket_pre | ep_id + Bp_ID"
# SPEC <- "%s ~ %s + treat_advice_c_p + log_w_pre + n_assets_pre + contacts_pre + ret_past12_pre + anlagepaket_pre | ep_id"

# SPEC <- "%s ~ %s + treat_advice_c_p + log_w_pre | ep_id + Bp_ID"

SPEC <- "%s ~ %s + treat_cli + log_w_pre + n_assets_pre + contacts_pre + anlagepaket_pre + segment_pre | ep_id + Bp_ID"
SPEC <- "%s ~ %s + treat_cli + log_w_pre + n_assets_pre + contacts_pre + anlagepaket_pre + segment_pre | ep_id"

# SPEC <- "%s ~ %s + treat_cli + log_w_pre + n_assets_pre + contacts_pre + ret_past12_pre + main_bank_pre | ep_id + Bp_ID"
# SPEC <- "%s ~ %s + treat_cli + log_w_pre + n_assets_pre + contacts_pre + ret_past12_pre + main_bank_pre | ep_id"
# SPEC <- "%s ~ %s + log_w_pre + n_assets_pre + contacts_pre + ret_past12_pre + main_bank_pre | ep_id + Bp_ID"
# SPEC <- "%s ~ %s + log_w_pre + n_assets_pre + contacts_pre + ret_past12_pre + main_bank_pre | ep_id"
# SPEC <- "%s ~ %s + treat_cli + log_w_pre | ep_id + Bp_ID"

eps <- c("ep1_201305", "ep2_201501", "ep3_201508", "ep6_201801", "ep7_201811", "ep9_202002", "ep10_202201", "ep12_202310")

# C  <- cle[smp_main  == 1L]
# C <- cle[smp_mb      == 1L] 
C <- cle[smp_mb_exdisc      == 1L] 

# C[,discr_pre := as.numeric(!smp_main==0)]

C <- C[ep_id %in% eps]

C <- C[segment_pre %in% c("Beratungszentrum","PK-Team","natürliche Personen PB")]

# C <- C[Bp_ID %in% bp_perf_win3]

# CYM <- cle[smp_cycle==1L & smp_mb==1L]
# C <- CYM

# TRT <- "treat_perf_a"
# TRT <- "treat_perf_p"
# TRT <- "treat_perf_a_p"
TRT <- "treat_perf_inv_a_p"

# TRT <- "treat_perf"
# TRT <- "treat_adv"
# TRT <- "treat_advice"
# TRT <- "treat_advice_a_p"

xs <- list()
## behavior
xs[["net flow (dd)"]]    <- feols(xs_fml("netflow_dd_w"),  C,  vcov = ~ advisor_id, notes = FALSE)
# xs[["net flow dec (dd)"]]    <- feols(xs_fml("netflow_d_dd_w"),  C,  vcov = ~ advisor_id, notes = FALSE)
xs[["d equity share"]]   <- feols(xs_fml("d_eq_dd_w"),     C,  vcov = ~ advisor_id, notes = FALSE)
xs[["d risky share"]]    <- feols(xs_fml("d_risky_dd_w"), C,  vcov = ~ advisor_id, notes = FALSE)
xs[["cash inflow"]]      <- feols(xs_fml("d_cash_dd_w"),  C,  vcov = ~ advisor_id, notes = FALSE)
xs[["sell flow"]]  <- feols(xs_fml("sellflow_dd_w"), C, vcov = ~ advisor_id, notes = FALSE)
## performance
xs[["gap mkt, dd leg"]]  <- feols(xs_fml("cf_gap_mkt_dd_w"),  C, vcov = ~ advisor_id, notes = FALSE)
xs[["gap mkt, recovery"]]<- feols(xs_fml("cf_gap_mkt_rec_w"), C, vcov = ~ advisor_id, notes = FALSE)
xs[["gap mkt, total"]]   <- feols(xs_fml("cf_gap_mkt_end_w"), C, vcov = ~ advisor_id, notes = FALSE)
xs[["ret next 12m"]]     <- feols(xs_fml("ret_next12_from_dd_w"), C, vcov = ~ advisor_id, notes = FALSE)

etable(xs)

# 
# xs_C_adv_p_fe <- xs
# xs_C_adv_fe <- xs
# xs_CM_adv_p_fe <- xs
# xs_CM_adv_fe <- xs
# 
# xs_C_adv_p <- xs
# xs_C_adv <- xs
# xs_CM_adv_p <- xs
# xs_CM_adv <- xs
# 
# etable(xs_C_adv_p_fe)
# etable(xs_C_adv_fe)
# etable(xs_CM_adv_p_fe)
# etable(xs_CM_adv_fe)
# etable(xs_C_adv_p)
# etable(xs_C_adv)
# etable(xs_CM_adv_p)
# etable(xs_CM_adv)
# 
# 
# xs_C_perf_p_fe <- xs
# xs_CM_perf_p_fe <- xs
# 
# xs_C_perf_p <- xs
# xs_CM_perf_p <- xs
# 
# xs_CM_perf_inv_p <- xs
# 
# etable(xs_C_perf_p_fe)
# etable(xs_CM_perf_p_fe)
# etable(xs_C_perf_p)
# etable(xs_CM_perf_p)


cat("================================================================================ \n
================================================================================")


# 
# etable(xs_CM_adv_p_fe)
# etable(xs_CM_perf_p_fe)
# etable(xs_CM_adv_p)
# etable(xs_CM_perf_p)
# etable(xs_CM_perf_inv_p)


# etable(xs_CM_adv_p,file = file.path(overleaf_tab,"cross_section_main_advp.tex"),replace = TRUE, headers=names(xs),depvar=F,
#        digits = 3, fitstat = ~ n + r2 + wr2, title = "Cross section at the episode level, main sample")
# etable(xs_CM_perf_p,file = file.path(overleaf_tab,"cross_section_main_perfp.tex"),replace = TRUE, headers=names(xs),depvar=F,
#        digits = 3, fitstat = ~ n + r2 + wr2, title = "Cross section at the episode level, main sample")


etable(xs,file = file.path(overleaf_tab,"cross_section_main_perfinvp_fe.tex"),replace = TRUE, headers=names(xs),depvar=F,
       digits = 3, fitstat = ~ n + r2 + wr2, title = "Cross section at the episode level, main sample")
etable(xs,file = file.path(overleaf_tab,"cross_section_main_perfinvp.tex"),replace = TRUE, headers=names(xs),depvar=F,
       digits = 3, fitstat = ~ n + r2 + wr2, title = "Cross section at the episode level, main sample")
# 
#   etable(xs,file = file.path(overleaf_tab,"cross_section_main_perfinvp_sub_persistent_perfa.tex"),replace = TRUE, headers=names(xs),depvar=F,
#        digits = 3, fitstat = ~ n + r2 + wr2, title = "Cross section at the episode level, main sample")

## ---------------------------------------------------------------------------
## MAIN-BANK SPLIT. main_bank_pre is constant inside each half, so it drops out
## of the formula. Reported as a SPLIT, not just a restriction: the two halves
## answer different questions and the difference between them is the result.
##   - the panic measures are roughly TWICE as large among main-bank clients,
##     where the client's money actually is and the denominator is right;
##   - the retention measures (full liquidation, left the bank) are roughly HALF
##     as large, because a non-main-bank client "liquidating" is often just
##     closing a satellite custody account. That is consolidation, not panic,
##     and it inflates the pooled retention result.
## ---------------------------------------------------------------------------
xs_split <- function(y) as.formula(sprintf(
  "%s ~ %s + treat_cli + log_w_pre + n_assets_pre + contacts_pre +
   ret_past12_pre | ep_id", y, TRT))
MB_Y <- c("netflow_dd_w","derisk_dd","derisk_rk_dd","derisk_eq_dd","full_liq",
          "ever_left_bank","ever_in_cash","any_sell_self_dd","sellflow_s_dd_w",
          "cf_gap_mkt_end_w","cf_gap_hold_end_w","ret_next12_from_dd_w")
xs_m <- lapply(setNames(MB_Y, MB_Y), function(y)
  feols(xs_split(y), CM, vcov = ~ advisor_id, notes = FALSE))
xs_x <- lapply(setNames(MB_Y, MB_Y), function(y)
  feols(xs_split(y), CX, vcov = ~ advisor_id, notes = FALSE))

## the trade-based outcomes again on the clients boerse actually covers, where
## they are not mechanically zero
xs_b <- list(
  `P(sell, any)`      = feols(xs_fml("any_sell_dec_dd"),  CB, vcov = ~ advisor_id, notes = FALSE),
  `P(sell, self)`     = feols(xs_fml("any_sell_self_dd"), CB, vcov = ~ advisor_id, notes = FALSE),
  `P(sell, advisor)`  = feols(xs_fml("any_sell_adv_dd"),  CB, vcov = ~ advisor_id, notes = FALSE),
  `sell flow, self`   = feols(xs_fml("sellflow_s_dd_w"),  CB, vcov = ~ advisor_id, notes = FALSE),
  `P(de-risk, self)`  = feols(xs_fml("derisk_s_dd"),      CB, vcov = ~ advisor_id, notes = FALSE),
  `commission (bp)`   = feols(xs_fml("kosten_dd_r_w"),    CB, vcov = ~ advisor_id, notes = FALSE))

## clients already on a review cycle: treatment is closer to "the scheduled
## review happened to land inside the crash" than to advisor selection
xs_y <- list()
xs_y[["net flow (dd)"]]  <- feols(xs_fml("netflow_dd_w"),      CY, vcov = ~ advisor_id, notes = FALSE)
xs_y[["P(de-risk)"]]     <- feols(xs_fml("derisk_dd"),         CY, vcov = ~ advisor_id, notes = FALSE)
xs_y[["P(full liq.)"]]   <- feols(xs_fml("full_liq"),          CY, vcov = ~ advisor_id, notes = FALSE)
xs_y[["P(equity cut)"]]  <- feols(xs_fml("derisk_eq_dd"),       CY, vcov = ~ advisor_id, notes = FALSE)
xs_y[["gap mkt, total"]] <- feols(xs_fml("cf_gap_mkt_end_w"),  CY, vcov = ~ advisor_id, notes = FALSE)
xs_y[["gap hold, total"]]<- feols(xs_fml("cf_gap_hold_end_w"), CY, vcov = ~ advisor_id, notes = FALSE)
xs_y[["ret next 12m"]]   <- feols(xs_fml("ret_next12_from_dd_w"), CY, vcov = ~ advisor_id, notes = FALSE)

xs_t <- list()
xs_t[["net flow (dd)"]]  <- feols(xs_fml("netflow_dd_w"),      CT, vcov = ~ advisor_id, notes = FALSE)
xs_t[["P(de-risk)"]]     <- feols(xs_fml("derisk_dd"),         CT, vcov = ~ advisor_id, notes = FALSE)
xs_t[["gap mkt, total"]] <- feols(xs_fml("cf_gap_mkt_end_w"),  CT, vcov = ~ advisor_id, notes = FALSE)
xs_t[["ret next 12m"]]   <- feols(xs_fml("ret_next12_from_dd_w"), CT, vcov = ~ advisor_id, notes = FALSE)

## treatment definitions side by side, never pooled. treat_adt is the old
## trade-conditional proxy -- the gap between it and the contact measures is the
## measurement error the contact log removes.
xs_v <- lapply(c(review = "treat_perf", review_advisor_init = "treat_perf_a",
                 any_advice_advisor_init = "treat_adv", any_advice = "treat_advice",
                 old_trade_proxy = "treat_adt"),
               function(v) feols(xs_fml("netflow_dd_w", trt = v), C,
                                 vcov = ~ advisor_id, notes = FALSE))
xs_v2 <- lapply(c(review = "treat_perf", review_advisor_init = "treat_perf_a",
                  any_advice_advisor_init = "treat_adv", any_advice = "treat_advice",
                  old_trade_proxy = "treat_adt"),
                function(v) feols(xs_fml("derisk_dd", trt = v), C,
                                  vcov = ~ advisor_id, notes = FALSE))

## advisor fixed effects version.
## NOTE: xs_fml's second argument is `trt`, not the fixed effects -- the fixed
## effects live in SPEC. Passing "| ep_id + advisor_id" positionally used to put
## it where the treatment belongs and built "y ~ | ep_id + advisor_id + ...",
## which is a parse error, not a silently wrong model. The FE are swapped in the
## spec string instead, so this stays correct if SPEC changes again.
SPEC_ADV <- sub("[|][^|]*$", "| ep_id + advisor_id", SPEC)
xs_a <- list(
  `net flow (dd)`  = feols(xs_fml("netflow_dd_w",     spec = SPEC_ADV), C, vcov = ~ advisor_id, notes = FALSE),
  `P(de-risk)`     = feols(xs_fml("derisk_dd",        spec = SPEC_ADV), C, vcov = ~ advisor_id, notes = FALSE),
  `gap mkt, total` = feols(xs_fml("cf_gap_mkt_end_w", spec = SPEC_ADV), C, vcov = ~ advisor_id, notes = FALSE)
)

cat("\n-- main sample --\n");            print(etable(xs,   digits = 3, fitstat = ~ n + r2))
cat("\n-- already on a review cycle --\n"); print(etable(xs_y, digits = 3, fitstat = ~ n + r2))
cat("\n-- traders only (robustness; conditions on an outcome) --\n")
print(etable(xs_t, digits = 3, fitstat = ~ n + r2))
cat("\n-------------------------------------------------------------\n")
cat("MAIN-BANK SPLIT (Hauptbankkunde at the pre-episode month)\n")
cat("-------------------------------------------------------------\n")
cat("n: main bank ", nrow(CM), " | not main bank ", nrow(CX), "\n\n", sep = "")
mb_cmp <- rbindlist(lapply(MB_Y, function(y) {
  f <- function(m) { ct <- coeftable(m)
    sprintf("%+.4f%s (%.4f)", ct[TRT,1],
            fcase(ct[TRT,4] < .01, "***", ct[TRT,4] < .05, "** ",
                  ct[TRT,4] < .1, "*  ", default = "   "), ct[TRT,2]) }
  data.table(outcome = y, main_bank = f(xs_m[[y]]), not_main_bank = f(xs_x[[y]]))
}))
print(mb_cmp, row.names = FALSE)
cat("\nThe panic measures (any_sell_self_dd, sellflow_s_dd_w) are about twice as\n")
cat("large among main-bank clients; the retention measures (full_liq,\n")
cat("ever_left_bank) are about half. Both differences go the way the measurement\n")
cat("problem predicts, so the split is informative rather than a nuisance.\n")

cat("\n-- trade-channel outcomes, clients covered by boerse --\n")
print(etable(xs_b, digits = 3, fitstat = ~ n + r2))
cat("P(sell, self) is the outcome that cannot be the advisor or the mandate desk\n")
cat("acting for the client; commission is boerse$Kosten per CHF of pre-episode\n")
cat("wealth, i.e. transaction revenue booked during the drawdown itself.\n")
cat("\n-- advisor FE --\n");             print(etable(xs_a, digits = 3, fitstat = ~ n + r2))

cat("\n-- treatment definitions side by side, outcome: net flow --\n")
print(etable(xs_v, digits = 3, fitstat = ~ n, keep = "treat"))
cat("\n-- ... outcome: P(de-risk) --\n")
print(etable(xs_v2, digits = 3, fitstat = ~ n, keep = "treat"))

## two-way clustering, as promised in the design
cat("\n-- two-way clustering (advisor + episode x month is not defined in the\n")
cat("   cross section; advisor + episode is used instead) --\n")
print(etable(list(
  `net flow (dd)`  = feols(xs_fml("netflow_dd_w"),     C, cluster = ~ advisor_id + ep_id, notes = FALSE),
  `P(de-risk)`     = feols(xs_fml("derisk_dd"),        C, cluster = ~ advisor_id + ep_id, notes = FALSE),
  `gap mkt, total` = feols(xs_fml("cf_gap_mkt_end_w"), C, cluster = ~ advisor_id + ep_id, notes = FALSE)
), digits = 3, fitstat = ~ n + r2))

saveRDS(list(xs = xs, xs_y = xs_y, xs_t = xs_t, xs_a = xs_a, xs_v = xs_v,
             xs_b = xs_b, xs_m = xs_m, xs_x = xs_x),
        file.path(CACHE, "models_xs.rds"))

## ---------------------------------------------------------------------------
## LaTeX output
## ---------------------------------------------------------------------------
etable(xs,   file = file.path(TAB_DIR, "06d_cross_section_main.tex"),    replace = TRUE, headers=names(xs),depvar=F,
       digits = 3, fitstat = ~ n + r2 + wr2, title = "Cross section at the episode level, main sample")
etable(xs_y, file = file.path(TAB_DIR, "06d_cross_section_reviewcycle.tex"), replace = TRUE,
       digits = 3, fitstat = ~ n + r2, title = "Cross section, clients already on a review cycle")
etable(xs_t, file = file.path(TAB_DIR, "06d_cross_section_traders.tex"), replace = TRUE,
       digits = 3, fitstat = ~ n + r2, title = "Cross section, clients who traded during the drawdown")
etable(xs_v, file = file.path(TAB_DIR, "06d_treatment_definitions.tex"), replace = TRUE,
       digits = 3, fitstat = ~ n, keep = "treat", title = "Treatment definitions side by side")
etable(xs_a, file = file.path(TAB_DIR, "06d_cross_section_advisorFE.tex"), replace = TRUE,
       digits = 3, fitstat = ~ n + r2, title = "Cross section with advisor fixed effects")
etable(xs_b, file = file.path(TAB_DIR, "06d_cross_section_channel.tex"), replace = TRUE,
       digits = 3, fitstat = ~ n + r2,
       title = "Selling by order channel and transaction revenue, clients covered by the trade file")
etable(xs_m, file = file.path(TAB_DIR, "06d_cross_section_mainbank.tex"), replace = TRUE,
       digits = 3, fitstat = ~ n + r2, keep = "treat",
       title = "Cross section, main-bank clients (Hauptbankkunde)")
etable(xs_x, file = file.path(TAB_DIR, "06d_cross_section_notmainbank.tex"), replace = TRUE,
       digits = 3, fitstat = ~ n + r2, keep = "treat",
       title = "Cross section, clients whose main bank this is not")
## the event studies are 130+ coefficients each and are read off the figures, not
## a table; their coefficient tables are cached in results/cache/models_es.rds

if (length(SKIPPED_FIGS)) {
  cat("\n!! ", length(SKIPPED_FIGS), " FIGURE(S) NOT WRITTEN -- the file was locked by\n", sep = "")
  cat("   another process (a PDF viewer). Every coefficient above is current; only\n")
  cat("   these plots are stale on disk. Close the viewer and re-run 06.\n")
  print(SKIPPED_FIGS)
}
log_step("specifications estimated; tables in results/tables, figures in results/figures")
sink()
