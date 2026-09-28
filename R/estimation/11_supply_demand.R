## =============================================================================
## 11_supply_demand.R -- advisor contacts as SUPPLY and DEMAND in stress episodes
##
## The contact log records who initiated every contact, so both sides of the
## advice market are separately observable -- unusual, and what makes this
## estimable at all:
##
##   SUPPLY  = advisor-initiated advice contacts (init_a). The advisor spends
##             their own scarce time reaching out. 246031 contacts.
##   DEMAND  = client-initiated advice contacts (init_c). The client picks up the
##             phone, which in a crash is the panic margin. 143239 contacts.
##
## The split is clean: of 390405 advice contacts only 1135 are neither, none both.
##
## MAIL IS EXCLUDED FROM THE HEADLINE MEASURES, AND THIS IS NOT COSMETIC.
## A bulk mailing is recorded as an advisor-initiated investment contact for
## every client it reaches, so it is indistinguishable from outreach in the raw
## flags. December 2014 is one: 31108 contacts against ~6000 in the neighbouring
## months, 26109 of them K_Anlegen, 28334 advisor-initiated, and 26080 by MAIL.
## 94.1% of that month's advice contacts are mail-only, against 20.5% over the
## sample. It lands on rel_month -1 of the 2015-01 block -- the reference period
## -- where it pushes the raw advisor-initiated contact rate to 0.899 against
## ~0.05 in every other block-month.
## The headline therefore uses c_advice_*_p (meeting or phone), which 02b already
## builds. The all-channel version is reported next to it so the mailing stays
## visible rather than being quietly dropped.
##
## FOUR QUESTIONS, IN ORDER
##   1. Aggregate: is the market-level response to stress supply or demand?
##   2. Event time: within a client, how do the two move around a stress block?
##   3. CAPACITY: an advisor has finite hours. When demand spikes, does proactive
##      outreach get crowded out? Estimated within advisor AND within calendar
##      month, so the common crisis shock is not doing the work.
##   4. Incidence: who is served, and who has to ask.
##
## Output: results/11_supply_demand.txt, results/tables/11_*.tex,
##         results/figures/11_*.pdf
## =============================================================================

source("00_setup.R")
log_init("11_supply_demand")

cd <- load_dt("contacts_d")
cm <- load_dt("contacts_m")
ep <- load_dt("episodes")[usable == TRUE]

eps <- c("ep1_201305", "ep2_201501", "ep3_201508", "ep6_201801", "ep7_201811", "ep9_202002", "ep10_202201", "ep12_202310")

ep <- ep[ep_id %chin% eps]

sink(file.path(RESULTS, "11_supply_demand.txt"), split = TRUE)
on.exit(sink(), add = TRUE)

cat("=============================================================\n")
cat(" ADVISOR CONTACTS: SUPPLY AND DEMAND IN STRESS EPISODES\n")
cat(" supply = advisor-initiated (init_a); demand = client-initiated (init_c)\n")
cat(" headline excludes mail: a bulk mailing is not outreach (see header)\n")
cat("=============================================================\n\n")

## ---------------------------------------------------------------------------
## 0. BULK-MAIL DETECTOR -- run first, because it decides what is measurable
## ---------------------------------------------------------------------------
cd[, m := eom(ContactDate)]
mail <- cd[inv == 1L, .(advice = .N, mail_only = sum(personal == 0L),
                           clients = uniqueN(Bp_ID)), by = m][order(m)]
mail[, mail_share := mail_only / advice]
med <- median(mail$mail_only)
mail[, bulk := mail_only > 5 * med]
cat("-- months where mail-only advice contacts exceed 5x the median month --\n")
cat("   (median month: ", med, " mail-only advice contacts)\n", sep = "")
print(mail[bulk == TRUE, .(m, advice, mail_only, mail_share = round(mail_share, 3),
                           clients)])
if (!nrow(mail[bulk == TRUE])) cat("   none\n")
cat("\nThese are mass mailings, not advisory interaction. They inflate any count\n")
cat("built on the all-channel flags -- including contacts_pre and adv_a_pre,\n")
cat("which are CONTROLS in 06 and 07. See the note at the end of this report.\n")

## ---------------------------------------------------------------------------
## 1. aggregate monthly series, per 100 active clients
## ---------------------------------------------------------------------------
pan <- setDT(read_parquet(file.path(CACHE, "panel.parquet"),
       col_select = c("Bp_ID", "MDate", "Hauptbetreuer_ID", "observed")))
act <- pan[observed == 1L, .(n_clients = .N), by = MDate]

agg <- cd[inv == 1L, .(supply_all = sum(init_a),
                          demand_all = sum(init_c),
                          supply = sum(init_a * personal),
                          demand = sum(init_c * personal)), by = .(MDate = m)]
agg <- act[agg, on = "MDate"][!is.na(n_clients)]
for (v in c("supply", "demand", "supply_all", "demand_all"))
  agg[, (paste0(v, "_r")) := 100 * get(v) / n_clients]
setorder(agg, MDate)

agg[, in_dd := FALSE]
for (i in seq_len(nrow(ep)))
  agg[MDate >= ep$dd_start[i] & MDate <= ep$dd_end[i], in_dd := TRUE]

cat("\n-------------------------------------------------------------\n")
cat("1. AGGREGATE: contacts per 100 active clients per month\n")
cat("-------------------------------------------------------------\n")
print(agg[, .(months = .N,
              supply = round(mean(supply_r), 3),
              demand = round(mean(demand_r), 3),
              demand_share = round(sum(demand) / sum(supply + demand), 3),
              supply_allchan = round(mean(supply_all_r), 3)),
          by = .(period = fifelse(in_dd, "drawdown month", "calm month"))])
cat("\nsupply / demand are meeting-or-phone; supply_allchan adds mail.\n")

if (pdf_ok(file.path(FIG_DIR, "11_supply_demand_series.pdf"), width = 9, height = 4.5)) {
  op <- par(mar = c(4, 4, 3, 1))
  plot(agg$MDate, agg$supply_r, type = "n",
       ylim = c(0, max(agg$supply_r, agg$demand_r) * 1.05),
       xlab = NULL, ylab = "contacts per 100 active clients",
       main = "Advice contacts: supply and demand (meeting or phone)")
  for (i in seq_len(nrow(ep)))
    rect(ep$dd_start[i], -1, ep$dd_end[i], 1e3, col = "grey88", border = NA)
  lines(agg$MDate, agg$supply_r, lwd = 2)
  lines(agg$MDate, agg$demand_r, lwd = 2, col = "#C0392B")
  legend("topright", c("supply (advisor-initiated)", "demand (client-initiated)"),
         col = c("black", "#C0392B"), lwd = 2, bty = "n")
  box(); par(op); dev.off()
}

## ---------------------------------------------------------------------------
## 2. EVENT TIME -- the core estimate
##    Within client x episode, with calendar-month-of-year fixed effects: advice
##    contacts are strongly seasonal, so an uncontrolled path is partly that.
##    `te` cannot be used: rel_month is a deterministic function of the calendar
##    month within an episode, so month x episode would absorb the whole path.
## ---------------------------------------------------------------------------
stk <- load_dt("stk")
keep_rel <- es_window(stk, ep)

S <- stk[smp_main == 1L & rel_month %in% keep_rel,
         .(Bp_ID, ci, ep_id, MDate, rel_month, phase, advisor_id,
           log_w_pre, n_assets_pre, main_bank_pre,
           c_advice_a, c_advice_c)]
rm(stk); gc(verbose = FALSE)
S[cm, on = .(Bp_ID, MDate), `:=`(sup_p = i.c_advice_a_p, dem_p = i.c_advice_c_p)]
S[is.na(sup_p), sup_p := 0L][is.na(dem_p), dem_p := 0L]
S[, moy := month(MDate)]

cat("\n-------------------------------------------------------------\n")
cat("2. EVENT TIME  (rel_month ", min(keep_rel), " .. ", max(keep_rel), ", ",
    format(nrow(S), big.mark = "'"), " client-months)\n", sep = "")
cat("-------------------------------------------------------------\n")
cat("\n-- raw monthly probability of a contact --\n")
raw <- S[, .(supply = round(mean(sup_p), 4), demand = round(mean(dem_p), 4),
             supply_allchan = round(mean(c_advice_a), 4)),
         by = rel_month][order(rel_month)]
raw[, ratio := round(demand / supply, 3)]
print(raw)
cat("\nsupply_allchan spikes at the block whose rel_month -1 is Dec 2014; the\n")
cat("meeting-or-phone series does not. That is the mailing, isolated.\n")

es <- function(y) feols(
  as.formula(sprintf("%s ~ i(rel_month, ref = -1) | ci + moy", y)),
  S, vcov = ~ advisor_id, notes = FALSE)

show_es <- function(m, label) {
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, 2:5, c("est", "se", "t", "p"))
  ct <- ct[grepl("^rel_month::", term)]
  ct[, rel_month := as.integer(sub("^rel_month::(-?[0-9]+)$", "\\1", term))]
  setorder(ct, rel_month)
  cat("\n--- ", label, " ---\n", sep = "")
  print(ct[, .(rel_month, est = round(est, 5), se = round(se, 5), t = round(t, 2),
               sig = fcase(p < .01, "***", p < .05, "**", p < .1, "*", default = ""))])
  invisible(ct)
}

## PHASE specification -- the one to read.
## rel_month is a poor unit here: blocks run from 1 to 23 drawdown months, so
## rel_month +6 is mid-crisis for the 2018-11 block and well into the recovery
## for the 2013-05 one. It is also collinear with the calendar month -- three of
## the six blocks start in January, so their rel_month -> month-of-year map is
## identical and the month-of-year FE is identified off half the sample.
## `phase` (pre / drawdown / recovery) is invariant to block length. Calendar
## MONTH fixed effects cannot be combined with it: the blocks barely overlap in
## calendar time, so MDate effectively pins down the block and phase is absorbed
## -- it collapses, with a standard error of 153. Month-of-YEAR FE does work,
## because it varies within a block across years, and it is what controls the
## strong seasonality in contact volume.
ph <- function(y) feols(
  as.formula(sprintf("%s ~ i(phase, ref = 'pre') | ci + moy", y)),
  S, vcov = ~ advisor_id, notes = FALSE)
cat("
-- PHASE specification (client x episode FE + month-of-year FE) --
")
print(etable(list(supply = ph("sup_p"), demand = ph("dem_p")),
             digits = 5, fitstat = ~ n + r2))
cat("Read against the pre-period. This is the estimate that is robust to blocks
")
cat("having very different drawdown lengths.
")
etable(list(supply = ph("sup_p"), demand = ph("dem_p")),
       file = file.path(TAB_DIR, "11_phase_supply_demand.tex"), replace = TRUE,
       digits = 5, fitstat = ~ n + r2,
       title = "Advice supply and demand by phase of a stress block")

m_sup <- es("sup_p"); show_es(m_sup, "SUPPLY: P(advisor-initiated, meeting or phone)")
m_dem <- es("dem_p"); show_es(m_dem, "DEMAND: P(client-initiated, meeting or phone)")

if (pdf_ok(file.path(FIG_DIR, "11_es_supply_demand.pdf"), width = 9, height = 5)) {
  iplot(list(m_sup, m_dem), main = "Advice contacts around a stress block",
        xlab = "months since dd_start (-1 = last pre-drawdown month)")
  legend("topleft", c("supply (advisor-initiated)", "demand (client-initiated)"),
         col = 1:2, pch = 16, bty = "n")
  abline(v = 0, lty = 3); abline(h = 0, col = "grey60")
  dev.off()
}
etable(list(supply = m_sup, demand = m_dem),
       file = file.path(TAB_DIR, "11_event_supply_demand.tex"), replace = TRUE,
       digits = 4, fitstat = ~ n + r2,
       title = "Advice contact supply and demand around stress blocks")
rm(m_sup, m_dem); gc(verbose = FALSE)

## ---------------------------------------------------------------------------
## 2b. BINARY-CHOICE ESTIMATES -- logit and probit
##
## Both outcomes are rare (supply ~3.8%, demand ~2.5% of client-months), which is
## exactly where a linear probability model is weakest: it puts fitted mass
## outside [0,1] and overstates marginal effects as the base rate falls. The FE
## structure does not force LPM here either -- ep_id and month-of-year are a
## handful of levels, so there is no incidental-parameters problem and a logit
## is clean.
##
## Reported as AVERAGE MARGINAL EFFECTS, not raw coefficients, so the numbers are
## directly comparable to the linear model and to each other: the AME is the
## sample average of P(y=1 | drawdown) - P(y=1 | pre), holding everything else at
## its observed value.
##
## Client x episode fixed effects are deliberately NOT in the headline. A
## conditional logit drops every group whose outcome never varies, and with a
## 2-4% monthly base rate that is the large majority of client-episodes -- it
## would silently redefine the sample as "clients who are sometimes contacted".
## The FE version is reported at the end WITH that attrition stated.
## ---------------------------------------------------------------------------
cat("\n-------------------------------------------------------------\n")
cat("2b. LOGIT / PROBIT: P(contact) in a drawdown vs the pre-period\n")
cat("-------------------------------------------------------------\n")
cat("base rates: supply ", round(mean(S$sup_p), 4),
    "  demand ", round(mean(S$dem_p), 4), "\n", sep = "")

S[, crisis   := as.integer(phase == "drawdown")]
S[, recovery := as.integer(phase == "recovery")]

bin_fml <- function(y) as.formula(sprintf(
  "%s ~ crisis + recovery + log_w_pre + n_assets_pre + main_bank_pre | ep_id + moy", y))

## average marginal effect of a binary regressor: the counterfactual difference
## averaged over the estimation sample. That is the ATE analogue of the LPM
## coefficient, rather than a log-odds coefficient.
## AME by the counterfactual method, done IN PLACE. Copying a 1.5 m-row table
## twice per model is what made this step time out; here `crisis` is flipped on
## S itself and restored afterwards, so no copy is ever made.
ame_bin <- function(m, var, dat) {
  keep <- dat[[var]]
  set(dat, j = var, value = 0L); p0 <- predict(m, newdata = dat, type = "response")
  set(dat, j = var, value = 1L); p1 <- predict(m, newdata = dat, type = "response")
  set(dat, j = var, value = keep)
  mean(p1 - p0, na.rm = TRUE)
}

fitb <- function(y, fam) feglm(bin_fml(y), S, family = fam,
                               vcov = ~ advisor_id, notes = FALSE)

## fit ONCE and reuse -- previously each model was fitted twice, once for the
## summary table and again for the etable below
LG <- list(sup_p = fitb("sup_p", "logit"),  dem_p = fitb("dem_p", "logit"))
PB <- list(sup_p = fitb("sup_p", "probit"), dem_p = fitb("dem_p", "probit"))
LP <- list(sup_p = feols(bin_fml("sup_p"), S, vcov = ~ advisor_id, notes = FALSE),
           dem_p = feols(bin_fml("dem_p"), S, vcov = ~ advisor_id, notes = FALSE))

res <- rbindlist(lapply(c("sup_p", "dem_p"), function(y) {
  lg <- LG[[y]]; pb <- PB[[y]]; lp <- LP[[y]]
  ctl <- coeftable(lg)
  data.table(
    outcome    = fifelse(y == "sup_p", "SUPPLY (advisor-initiated)",
                                       "DEMAND (client-initiated)"),
    base_rate  = round(mean(S[[y]]), 4),
    logit_coef = round(ctl["crisis", 1], 4),
    logit_se   = round(ctl["crisis", 2], 4),
    logit_p    = round(ctl["crisis", 4], 4),
    logit_AME  = round(ame_bin(lg, "crisis", S), 5),
    probit_AME = round(ame_bin(pb, "crisis", S), 5),
    LPM        = round(coeftable(lp)["crisis", 1], 5))
}))
cat("\n-- effect of being IN a drawdown month, relative to the pre-period --\n")
print(res, row.names = FALSE)
cat("\nlogit_coef is in log-odds; the AME columns are in probability points and are\n")
cat("what should be quoted. LPM is the linear model on the same sample.\n")

m_lg_sup <- LG$sup_p
m_lg_dem <- LG$dem_p
cat("\n-- full logit output --\n")
print(etable(list(supply = m_lg_sup, demand = m_lg_dem),
             digits = 4, fitstat = ~ n + pr2))
etable(list(supply = m_lg_sup, demand = m_lg_dem),
       file = file.path(TAB_DIR, "11_logit_supply_demand.tex"), replace = TRUE,
       digits = 4, fitstat = ~ n + pr2,
       title = "Logit: probability of an advice contact in a drawdown month")

## robustness: conditional logit with client x episode FE. The attrition is the
## whole story -- groups with no outcome variation drop out entirely.
cl_sup <- feglm(sup_p ~ crisis + recovery | ci, S, family = "logit",
                vcov = ~ advisor_id, notes = FALSE)
cl_dem <- feglm(dem_p ~ crisis + recovery | ci, S, family = "logit",
                vcov = ~ advisor_id, notes = FALSE)
cat("\n-- conditional logit, client x episode FE (robustness) --\n")
print(etable(list(supply = cl_sup, demand = cl_dem), digits = 4, fitstat = ~ n))
cat(sprintf("\nsample: %s of %s client-months survive (%.1f%%). The rest sit in\n",
            format(nobs(cl_sup), big.mark = "'"), format(nrow(S), big.mark = "'"),
            100 * nobs(cl_sup) / nrow(S)))
cat("client-episodes that were never contacted, so the fixed effect explains them\n")
cat("perfectly and they carry no information about the crisis effect. That is why\n")
cat("the pooled specification above is the one to read.\n")
rm(m_lg_sup, m_lg_dem, cl_sup, cl_dem, LG, PB, LP); gc(verbose = FALSE)

## ---------------------------------------------------------------------------
## 3. CAPACITY -- does demand crowd out proactive outreach?
##    advisor x month. advisor FE takes out how active an advisor is in general;
##    MDate FE takes out the common crisis shock, so the coefficient compares
##    advisors facing unusually high demand THAT MONTH against their peers.
## ---------------------------------------------------------------------------
book <- pan[observed == 1L & !is.na(Hauptbetreuer_ID),
            .(n_clients = .N), by = .(advisor_id = Hauptbetreuer_ID, MDate)]

ac <- cd[advice == 1L & personal == 1L, .(Bp_ID, MDate = m, init_a, init_c)]
ac[pan, on = .(Bp_ID, MDate), advisor_id := i.Hauptbetreuer_ID]
rm(pan); gc(verbose = FALSE)

am <- ac[!is.na(advisor_id), .(supply = sum(init_a), demand = sum(init_c)),
         by = .(advisor_id, MDate)]
am <- am[book, on = .(advisor_id, MDate)]
am[is.na(supply), `:=`(supply = 0L, demand = 0L)]
am <- am[n_clients >= 5]
am[, `:=`(supply_r = supply / n_clients, demand_r = demand / n_clients,
          total_r  = (supply + demand) / n_clients)]
am[, in_dd := FALSE]
for (i in seq_len(nrow(ep)))
  am[MDate >= ep$dd_start[i] & MDate <= ep$dd_end[i], in_dd := TRUE]

cat("\n-------------------------------------------------------------\n")
cat("3. CAPACITY: advisor x month (", format(nrow(am), big.mark = "'"), " rows, ",
    uniqueN(am$advisor_id), " advisors)\n", sep = "")
cat("-------------------------------------------------------------\n")
print(am[, .(advisor_months = .N, med_book = median(n_clients),
             supply_per_client = round(mean(supply_r), 4),
             demand_per_client = round(mean(demand_r), 4),
             total_per_client  = round(mean(total_r), 4)),
         by = .(period = fifelse(in_dd, "drawdown month", "calm month"))])

cap <- list(
  `supply ~ demand`      = feols(supply_r ~ demand_r | advisor_id, am,
                                 vcov = ~ advisor_id, notes = FALSE),
  `+ month FE`           = feols(supply_r ~ demand_r | advisor_id + MDate, am,
                                 vcov = ~ advisor_id, notes = FALSE),
  `+ book size`          = feols(supply_r ~ demand_r + n_clients | advisor_id + MDate,
                                 am, vcov = ~ advisor_id, notes = FALSE),
  `drawdown months only` = feols(supply_r ~ demand_r + n_clients | advisor_id + MDate,
                                 am[in_dd == TRUE], vcov = ~ advisor_id, notes = FALSE))
print(etable(cap, digits = 4, fitstat = ~ n + r2))
etable(cap, file = file.path(TAB_DIR, "11_capacity_crowdout.tex"), replace = TRUE,
       digits = 4, fitstat = ~ n + r2,
       title = "Does client-initiated demand crowd out advisor-initiated outreach?")
cat("\nA negative coefficient means an advisor facing unusually high demand in a\n")
cat("given month does LESS proactive outreach -- the capacity constraint binding.\n")
cat("advisor FE removes how active an advisor is in general; MDate FE removes the\n")
cat("common crisis shock, so this compares advisors WITHIN a calendar month.\n")
cat("It is descriptive: demand is not randomly assigned across advisors.\n")

## ---------------------------------------------------------------------------
## 4. INCIDENCE -- who is served, who has to ask
## ---------------------------------------------------------------------------
cat("\n-------------------------------------------------------------\n")
cat("4. INCIDENCE during the drawdown, by pre-episode wealth quintile\n")
cat("-------------------------------------------------------------\n")
cle <- load_dt("cle")[smp_main == 1L]
cle[, wq := cut(wealth_pre, quantile(wealth_pre, 0:5/5, na.rm = TRUE),
                labels = paste0("Q", 1:5), include.lowest = TRUE)]
print(cle[!is.na(wq), .(N = .N,
          med_wealth = round(median(wealth_pre)),
          supplied = round(mean(treat_adv), 4),
          demanded = round(mean(treat_cli), 4),
          ratio    = round(mean(treat_cli) / pmax(mean(treat_adv), 1e-9), 3)),
      by = wq][order(wq)])
cat("\nsupplied = any advisor-initiated advice contact inside the daily window;\n")
cat("demanded = any client-initiated one. Both from 04_treatment, all channels.\n")

cat("\n-------------------------------------------------------------\n")
cat("NOTE FOR THE PIPELINE\n")
cat("-------------------------------------------------------------\n")
cat("contacts_pre and adv_a_pre in 04_treatment count c_advice / c_advice_a,\n")
cat("which include mail. The Dec 2014 mailing therefore inflates them for the\n")
cat("block whose pre-window covers it: mean adv_a_pre is 1.47 there against\n")
cat("0.26-0.57 in the other blocks. Both are CONTROLS in 06 and 07. Switching\n")
cat("them to the _p (meeting or phone) flags would remove it; on_review_cycle is\n")
cat("unaffected because it is built on K_Performancebesprechung, and Dec 2014 was\n")
cat("a K_Anlegen mailing.\n")

log_step("supply/demand written", am)
sink()
