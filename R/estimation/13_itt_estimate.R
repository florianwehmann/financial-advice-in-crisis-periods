## =============================================================================
## 13_itt_estimate.R -- balance, first stage, reduced form (ITT), 2SLS (LATE)
##
## Reads results/cache/itt_window.parquet from 12_itt_window.R.
##
## Pre-specified primary outcomes, in this order and no other:
##   1. d_risky_dd_w      change in risky share over the drawdown
##   2. netflow_dd_w      net flow over the drawdown, scaled by pre-episode wealth
##   3. d_cash_dd_w       change in cash
##   4. cf_gap_mkt_dd_w   counterfactual gap vs the market, drawdown leg
## Everything else is an appendix table.
##
## Output: results/itt/tab_balance.tex, tab_firststage.tex, tab_itt.tex,
##         tab_2sls.tex, tab_appendix.tex, tab_se_variants.tex
##         results/13_itt_estimate.txt
## =============================================================================

source("00_setup.R")
log_init("13_itt_estimate")

ITT_DIR <- file.path(RESULTS, "itt")
dir.create(ITT_DIR, showWarnings = FALSE, recursive = TRUE)

sink(file.path(RESULTS, "13_itt_estimate.txt"), split = TRUE)
on.exit(sink(), add = TRUE)

X <- load_dt("itt_window")
D0 <- X[base == 1L]

## ---------------------------------------------------------------------------
## controls
##
## main_bank_pre and discr_pre are constant inside smp_mb by construction
## (smp_mb = discr_pre == 0 & main_bank_pre == 1), so they cannot be controls
## here. Asserted rather than assumed.
## ---------------------------------------------------------------------------

for (v in c("main_bank_pre", "discr_pre"))
  if (uniqueN(D0[[v]]) != 1L)
    stop(sprintf("%s is not constant in this sample -- it should be a control", v))

CTRL <- c("log_w_pre", "n_assets_pre", "contacts_pre", "ret_past12_pre")
XCTL <- paste(CTRL, collapse = " + ")

cat("controls:", XCTL, "\n")
cat("main_bank_pre and discr_pre are constant in smp_mb and are excluded.\n")
cat("age and tenure do not exist in this data.\n\n")

## covariate missingness -- logged, never silent
cat("rows before dropping covariate NAs:", nrow(D0), "\n")
for (v in CTRL) cat(sprintf("  %-16s NA: %d (%.2f%%)\n", v, sum(is.na(D0[[v]])),
                            100 * mean(is.na(D0[[v]]))))
D0 <- D0[complete.cases(D0[, ..CTRL])]
cat("rows after :", nrow(D0), " clients:", uniqueN(D0$Bp_ID),
    " advisors:", uniqueN(D0$advisor_id), "\n\n")

CL <- ~ advisor_id
n_adv <- uniqueN(D0$advisor_id)
cat("advisor clusters:", n_adv,
    if (n_adv < 50) "-- FEWER THAN 50, wild cluster bootstrap required\n"
    else "-- above 50, asymptotic cluster-robust inference is appropriate\n", "\n")

PRIMARY <- c(d_risky_dd_w    = "d risky share",
             netflow_dd_w    = "net flow",
             d_cash_dd_w     = "cash inflow",
             cf_gap_mkt_dd_w = "gap vs mkt, dd leg")

## ---------------------------------------------------------------------------
## Step 4 -- balance
##
## (a) ep_id FE only. Tests whether clients whose habitual review falls in
##     March differ from clients whose habitual review falls in October.
##     Failing here is EXPECTED and does not kill the design, because the
##     client-FE spec does not use that variation.
## (b) ep_id + client FE. Tests balance on the within-client variation, which
##     is what the client-FE spec is identified from. Failing here kills it.
## ---------------------------------------------------------------------------

cat("\n================ Step 4: balance ================\n\n")

BAL <- c("log_w_pre", "n_assets_pre", "contacts_pre", "ret_past12_pre",
         "risky_share_pre", "eq_share_pre", "ebanking_pre")

bal_a <- lapply(BAL, function(v)
  feols(as.formula(sprintf("%s ~ Z | ep_id", v)), D0, vcov = CL, notes = FALSE))
bal_b <- lapply(BAL, function(v)
  feols(as.formula(sprintf("%s ~ Z | ep_id + Bp_ID", v)), D0, vcov = CL, notes = FALSE))
names(bal_a) <- names(bal_b) <- BAL

cat("---- (a) ep_id FE only ----\n")
print(etable(bal_a, headers = BAL, depvar = FALSE, digits = 4, fitstat = ~ n + r2))
cat("\n---- (b) ep_id + client FE ----\n")
print(etable(bal_b, headers = BAL, depvar = FALSE, digits = 4, fitstat = ~ n + r2))

## joint test: put every covariate on the right of Z and test them together
jf <- function(fe) {
  m <- feols(as.formula(sprintf("Z ~ %s | %s", paste(BAL, collapse = " + "), fe)),
             D0, vcov = CL, notes = FALSE)
  w <- fixest::wald(m, keep = BAL, print = FALSE)
  list(model = m, stat = w$stat, p = w$p, df1 = w$df1, df2 = w$df2)
}
J <- list(a = jf("ep_id"), b = jf("ep_id + Bp_ID"))
cat(sprintf("\njoint F, all %d covariates on Z:\n", length(BAL)))
cat(sprintf("  (a) ep_id FE        : F = %.3f  p = %.4f\n", J$a$stat, J$a$p))
cat(sprintf("  (b) ep_id + client FE: F = %.3f  p = %.4f\n", J$b$stat, J$b$p))
if (J$b$p < 0.05)
  cat("\n!!! BALANCE FAILS ON THE WITHIN-CLIENT VARIATION (spec b).\n",
      "!!! The client-FE design is not credible as it stands.\n", sep = "")
if (J$a$p < 0.05 && J$b$p >= 0.05)
  cat("\nSpec (a) unbalanced, spec (b) balanced: as expected. Habitual-month\n",
      "clients differ in levels, but the within-client variation is clean.\n",
      "Read the client-FE column as the design; (a) is descriptive.\n", sep = "")

etable(c(bal_a, bal_b), file = file.path(ITT_DIR, "tab_balance.tex"), replace = TRUE,
       headers = list("^:_:ep FE" = setNames(rep(1, length(BAL)), BAL),
                      ":_:ep + client FE" = setNames(rep(1, length(BAL)), BAL)),
       depvar = FALSE, digits = 4, fitstat = ~ n + r2,
       title = "Balance of pre-crisis covariates on the instrument",
       label = "tab:itt_balance", notes = sprintf(
         "Each column is a separate regression of the named pre-crisis covariate on Z. Sample: %s, baseline habitual-window sample. Standard errors clustered on advisor. Joint F across all covariates: %.2f (p = %.3f) with episode FE, %.2f (p = %.3f) with episode and client FE.",
         "smp\\_mb", J$a$stat, J$a$p, J$b$stat, J$b$p))

## ---------------------------------------------------------------------------
## Step 5 -- first stage, reduced form, 2SLS
## ---------------------------------------------------------------------------

cat("\n\n================ Step 5: first stage ================\n\n")

fs_fe  <- feols(as.formula(sprintf("D ~ Z + %s | ep_id", XCTL)), D0, vcov = CL, notes = FALSE)
fs_cfe <- feols(as.formula(sprintf("D ~ Z + %s | ep_id + Bp_ID", XCTL)), D0, vcov = CL, notes = FALSE)
## the same on the month-window treatment, as a diagnostic on the daily window
fsm_fe  <- feols(as.formula(sprintf("D_m ~ Z + %s | ep_id", XCTL)), D0, vcov = CL, notes = FALSE)
fsm_cfe <- feols(as.formula(sprintf("D_m ~ Z + %s | ep_id + Bp_ID", XCTL)), D0, vcov = CL, notes = FALSE)

FS <- list("D | ep FE" = fs_fe, "D | ep+client FE" = fs_cfe,
           "D_m | ep FE" = fsm_fe, "D_m | ep+client FE" = fsm_cfe)
print(etable(FS, headers = names(FS), depvar = FALSE, digits = 4,
             fitstat = ~ n + r2 + wr2))

fstat <- function(m) {
  w <- fixest::wald(m, keep = "Z", print = FALSE)
  c(coef = coef(m)["Z"], se = se(m)["Z"], F = w$stat, p = w$p)
}
FSTAB <- as.data.table(t(sapply(FS, fstat)), keep.rownames = "spec")
setnames(FSTAB, c("spec", "coef", "se", "F", "p"))
cat("\nfirst-stage F on Z (advisor-clustered):\n")
print(FSTAB[, .(spec, coef = round(coef, 4), se = round(se, 4),
                F = round(F, 2), p = round(p, 4))])

WEAK <- FSTAB[spec == "D | ep+client FE", F] < 10
if (WEAK)
  cat("\n!!! WEAK FIRST STAGE (F < 10) on the primary D / client-FE spec.\n",
      "!!! 2SLS point estimates below are reported for completeness only and\n",
      "!!! must NOT be read as LATEs. The ITT is the estimate to read.\n", sep = "")

## ---------------------------------------------------------------------------
## reduced form (ITT) and 2SLS (LATE)
## ---------------------------------------------------------------------------

cat("\n\n================ Step 5: reduced form (ITT) ================\n\n")

rf_fe  <- lapply(names(PRIMARY), function(y)
  feols(as.formula(sprintf("%s ~ Z + %s | ep_id", y, XCTL)), D0, vcov = CL, notes = FALSE))
rf_cfe <- lapply(names(PRIMARY), function(y)
  feols(as.formula(sprintf("%s ~ Z + %s | ep_id + Bp_ID", y, XCTL)), D0, vcov = CL, notes = FALSE))
names(rf_fe) <- names(rf_cfe) <- PRIMARY

print(etable(rf_fe, headers = PRIMARY, depvar = FALSE, digits = 4, fitstat = ~ n + r2))
cat("\n---- with client FE ----\n")
print(etable(rf_cfe, headers = PRIMARY, depvar = FALSE, digits = 4, fitstat = ~ n + r2))

etable(c(rf_fe, rf_cfe), file = file.path(ITT_DIR, "tab_itt.tex"), replace = TRUE,
       headers = c(PRIMARY, PRIMARY), depvar = FALSE, digits = 4, fitstat = ~ n + r2,
       title = "Reduced form: effect of the habitual review window overlapping the crisis",
       label = "tab:itt", notes = "Columns 1-4 have episode fixed effects, columns 5-8 episode and client fixed effects. Standard errors clustered on advisor.")

cat("\n\n================ Step 5: 2SLS (LATE) ================\n\n")

iv_fe  <- lapply(names(PRIMARY), function(y)
  feols(as.formula(sprintf("%s ~ %s | ep_id | D ~ Z", y, XCTL)), D0, vcov = CL, notes = FALSE))
iv_cfe <- lapply(names(PRIMARY), function(y)
  feols(as.formula(sprintf("%s ~ %s | ep_id + Bp_ID | D ~ Z", y, XCTL)), D0, vcov = CL, notes = FALSE))
names(iv_fe) <- names(iv_cfe) <- PRIMARY

print(etable(iv_fe, headers = PRIMARY, depvar = FALSE, digits = 4, fitstat = ~ n + ivf))
cat("\n---- with client FE ----\n")
print(etable(iv_cfe, headers = PRIMARY, depvar = FALSE, digits = 4, fitstat = ~ n + ivf))

etable(c(iv_fe, iv_cfe), file = file.path(ITT_DIR, "tab_2sls.tex"), replace = TRUE,
       headers = c(PRIMARY, PRIMARY), depvar = FALSE, digits = 4, fitstat = ~ n + ivf,
       title = "2SLS: effect of a review meeting during the drawdown, instrumented by the habitual window",
       label = "tab:itt_2sls", notes = sprintf(
         "Columns 1-4 have episode fixed effects, columns 5-8 episode and client fixed effects. Standard errors clustered on advisor. First-stage F on Z is %.1f (episode FE) and %.1f (episode and client FE).%s",
         FSTAB[spec == "D | ep FE", F], FSTAB[spec == "D | ep+client FE", F],
         if (WEAK) " The first stage is weak by the conventional F>10 rule; these point estimates should not be read as LATEs." else ""))

etable(FS, file = file.path(ITT_DIR, "tab_firststage.tex"), replace = TRUE,
       headers = names(FS), depvar = FALSE, digits = 4, fitstat = ~ n + r2 + wr2,
       title = "First stage: habitual window overlap and a realized review meeting",
       label = "tab:itt_firststage", notes = "D is measured on the exact daily window [peak, trough]; D_m on the calendar-month window. Standard errors clustered on advisor.")

## ---------------------------------------------------------------------------
## SE variants -- advisor (main), client, two-way (advisor, episode)
## ---------------------------------------------------------------------------

cat("\n\n================ SE variants, ITT coefficient on Z ================\n\n")

se_var <- rbindlist(lapply(seq_along(PRIMARY), function(k) {
  y <- names(PRIMARY)[k]
  m <- feols(as.formula(sprintf("%s ~ Z + %s | ep_id + Bp_ID", y, XCTL)), D0, notes = FALSE)
  g <- function(vc) { s <- summary(m, vcov = vc); c(se(s)["Z"], pvalue(s)["Z"]) }
  a <- g(~ advisor_id); b <- g(~ Bp_ID); d <- g(~ advisor_id + ep_id)
  data.table(outcome = PRIMARY[k], coef = coef(m)["Z"],
             se_advisor = a[1], p_advisor = a[2],
             se_client  = b[1], p_client  = b[2],
             se_twoway  = d[1], p_twoway  = d[2])
}))
print(se_var[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 4) else x)])

sv_rows <- sprintf("%s & %s & %s & %s & %s \\\\",
  se_var$outcome, sprintf("%.4f", se_var$coef),
  sprintf("%.4f", se_var$se_advisor), sprintf("%.4f", se_var$se_client),
  sprintf("%.4f", se_var$se_twoway))
cat("\\begin{table}[htbp]\n\\centering\n",
    "\\caption{ITT coefficient on $Z$ under alternative clustering}\n",
    "\\label{tab:itt_se}\n\\begin{tabular}{lcccc}\n\\hline\\hline\n",
    "Outcome & Coef. & SE advisor & SE client & SE two-way \\\\\n\\hline\n",
    paste(sv_rows, collapse = "\n"), "\n\\hline\\hline\n\\end{tabular}\n",
    "\\begin{minipage}{\\linewidth}\\footnotesize\n",
    sprintf("Notes: episode and client fixed effects, %d advisor clusters. Two-way clustering is on advisor and episode.\n", n_adv),
    "\\end{minipage}\n\\end{table}\n",
    sep = "", file = file.path(ITT_DIR, "tab_se_variants.tex"))

## ---------------------------------------------------------------------------
## MDE -- reported alongside every null, so a wide CI is never read as a zero
##
## Minimum detectable effect at 80% power, 5% two-sided: 2.802 * SE.
## ---------------------------------------------------------------------------

cat("\n\n================ MDE (80% power, 5% two-sided) ================\n\n")

mde <- rbindlist(lapply(seq_along(PRIMARY), function(k) {
  y <- names(PRIMARY)[k]
  m <- rf_cfe[[k]]
  s <- se(m)["Z"]; b <- coef(m)["Z"]
  sdy <- sd(D0[[y]], na.rm = TRUE)
  ci <- confint(m)["Z", ]
  data.table(outcome = PRIMARY[k], coef = b, se = s,
             ci_lo = ci[[1]], ci_hi = ci[[2]], p = pvalue(m)["Z"],
             sd_y = sdy, mde = 2.802 * s, mde_in_sd = 2.802 * s / sdy,
             coef_in_sd = b / sdy)
}))
print(mde[, .(outcome, coef = round(coef, 4), se = round(se, 4),
              ci = sprintf("[%.4f, %.4f]", ci_lo, ci_hi), p = round(p, 4),
              mde = round(mde, 4), mde_in_sd = round(mde_in_sd, 3))])
cat("\nRead: an effect smaller than the MDE column could not have been detected\n",
    "at 80% power, so a null there is 'too imprecise to say', not 'no effect'.\n", sep = "")

## ---------------------------------------------------------------------------
## Step 9 -- multiple testing across the 4 primary outcomes
##
## Sharpened two-stage BH q-values (Benjamini-Krieger-Yekutieli 2006, as used by
## Anderson 2008). fwildclusterboot is not installed and, with 312 advisor
## clusters, is not needed; Romano-Wolf would require a cluster bootstrap of the
## whole system, so the deterministic sharpened-BH q-value is reported instead.
## ---------------------------------------------------------------------------

cat("\n\n================ Step 9: multiple testing ================\n\n")

sharpened_bh <- function(p) {
  m <- length(p); o <- order(p); ps <- p[o]
  qgrid <- seq(0.001, 0.999, by = 0.001)
  qval <- rep(NA_real_, m)
  for (q in qgrid) {
    ## stage 1: BH at q/(1+q) to estimate the number of true nulls
    a1 <- q / (1 + q)
    r1 <- suppressWarnings(max(c(0, which(ps <= a1 * seq_len(m) / m))))
    m0 <- m - r1
    if (m0 == 0) { rej <- seq_len(m) } else {
      a2 <- q * m / (m0 * (1 + q))
      r2 <- suppressWarnings(max(c(0, which(ps <= a2 * seq_len(m) / m))))
      rej <- if (r2 == 0) integer(0) else seq_len(r2)
    }
    new <- rej[is.na(qval[o][rej])]
    if (length(new)) qval[o[new]] <- q
  }
  qval[is.na(qval)] <- 1
  qval
}

mt <- mde[, .(outcome, coef, p)]
mt[, p_bh := p.adjust(p, "BH")]
mt[, q_sharpened := sharpened_bh(p)]
mt[, p_bonferroni := p.adjust(p, "bonferroni")]
print(mt[, .(outcome, coef = round(coef, 4), p = round(p, 4),
             p_bh = round(p_bh, 4), q_sharpened = round(q_sharpened, 3),
             p_bonf = round(p_bonferroni, 4))])

## ---------------------------------------------------------------------------
## appendix outcomes
## ---------------------------------------------------------------------------

cat("\n\n================ appendix outcomes ================\n\n")

APP <- c(d_eq_dd_w = "d equity share", sellflow_dd_w = "sell flow",
         cf_gap_mkt_rec_w = "gap mkt, recovery", cf_gap_mkt_end_w = "gap mkt, total",
         ret_next12_from_dd_w = "ret next 12m", full_liq = "full liquidation",
         exits = "left the bank")
APP <- APP[names(APP) %in% names(D0)]
app <- lapply(names(APP), function(y)
  feols(as.formula(sprintf("%s ~ Z + %s | ep_id + Bp_ID", y, XCTL)), D0, vcov = CL, notes = FALSE))
names(app) <- APP
print(etable(app, headers = APP, depvar = FALSE, digits = 4, fitstat = ~ n + r2))
etable(app, file = file.path(ITT_DIR, "tab_appendix.tex"), replace = TRUE,
       headers = APP, depvar = FALSE, digits = 4, fitstat = ~ n + r2,
       title = "Reduced form, secondary outcomes", label = "tab:itt_appendix",
       notes = "Episode and client fixed effects. Standard errors clustered on advisor.")

## ---------------------------------------------------------------------------
## per-episode estimates, for the forest plot drawn in 14
## ---------------------------------------------------------------------------

byep <- rbindlist(lapply(names(PRIMARY), function(y)
  rbindlist(lapply(sort(unique(D0$ep_id)), function(e) {
    d <- D0[ep_id == e]
    if (uniqueN(d$Z) < 2L || nrow(d) < 50L)
      return(data.table(outcome = y, ep_id = e, coef = NA_real_, se = NA_real_, n = nrow(d)))
    m <- feols(as.formula(sprintf("%s ~ Z + %s", y, XCTL)), d, vcov = CL, notes = FALSE)
    data.table(outcome = y, ep_id = e, coef = coef(m)["Z"], se = se(m)["Z"], n = nrow(d))
  }))))
save_dt(byep, "itt_by_episode")
save_dt(mde,  "itt_primary_mde")
save_dt(mt,   "itt_primary_mt")

cat("\n\nwrote tables to", ITT_DIR, "\n")
print(list.files(ITT_DIR))
