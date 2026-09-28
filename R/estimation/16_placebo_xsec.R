## =============================================================================
## 16_placebo_xsec.R -- the 06_specs cross section on PLACEBO windows
##
## 06_specs.R (the "playground" block, lines ~400-459) runs one cross-sectional
## regression per outcome on eight stress episodes:
##
##   y ~ TRT + treat_cli + log_w_pre + n_assets_pre + contacts_pre
##            + anlagepaket_pre | ep_id + Bp_ID          cluster: advisor_id
##
## The question here is whether that coefficient is about CRISES or simply
## about what an advisor contact does in ANY window. The script therefore
## re-runs the identical regression on placebo episode sets: the same number of
## windows, each with the same daily length and the same pre/post month
## geometry as the real episode it stands in for, but placed at a random date
## in the sample.
##
## A placebo DRAW is a whole SET of windows, not a single window. The
## specification carries ep_id and Bp_ID fixed effects, so the real estimate is
## identified off variation within client across episodes; a placebo that did
## not reproduce that stacked structure would not be the same regression.
##
## Window GEOMETRY is held fixed and only the calendar position is randomised.
## That matters: the treatment rate scales with window length (1.4% in the
## 27-day ep12 against 12.7% in the 275-day ep10), so drawing a common length
## would compare the real estimate against a differently-powered regression.
##
## Two window designs are run (PL$modes):
##   "any"  -- start dates uniform over the whole sample, crises included.
##             The literal placebo: what does the coefficient look like on an
##             arbitrary window?
##   "calm" -- the same, except a placebo window may not touch ANY hand-dated
##             stress window from stress_events.R (including the events that
##             form no estimation episode). The sharper test: the effect is
##             crisis-specific only if the real coefficient sits outside THIS
##             distribution.
##
## 04 and 05 are NOT re-run. The parts of 04_treatment.R and 05_outcomes.R that
## this specification actually needs are rebuilt from the panel, the daily
## contact log and the monthly index by build_cle() in placebo_funs.R, which
## 17_es_wrel_placebo.R shares. That rebuild is VERIFIED
## before any placebo is drawn: run on the real episode dates it must reproduce
## the coefficients 06 gets off results/cache/cle.parquet, and the script stops
## if it does not.
##
## Output: results/16_placebo_xsec.txt
##         results/cache/placebo_xsec.parquet   (one row per draw x outcome)
##         results/tables/tab_placebo_xsec.tex
##         results/figures/16_placebo_xsec.pdf
## =============================================================================

source("00_setup.R")
source("placebo_funs.R")
log_init("16_placebo_xsec")

sink(file.path(RESULTS, "16_placebo_xsec.txt"), split = TRUE)
on.exit(sink(), add = TRUE)

## ---------------------------------------------------------------------------
## 0. configuration -- mirrors the 06_specs playground block exactly.
##    Change SPEC / TRT / SAMPLE / EPS here and the placebo follows; the
##    verification in step 2 will refuse to run if the rebuild can no longer
##    reproduce whatever 06 currently does.
## ---------------------------------------------------------------------------

SPEC <- "%s ~ %s + treat_cli + log_w_pre + n_assets_pre + contacts_pre + anlagepaket_pre | ep_id + Bp_ID"
TRT    <- "treat_perf_inv_a_p"
SAMPLE <- "smp_mb_exdisc"
EPS    <- c("ep1_201305", "ep2_201501", "ep3_201508", "ep6_201801",
            "ep7_201811", "ep9_202002", "ep10_202201", "ep12_202310")

## outcome -> label, in the order 06 reports them
YS <- c(netflow_dd_w         = "net flow (dd)",
        d_eq_dd_w            = "d equity share",
        d_risky_dd_w         = "d risky share",
        d_cash_dd_w          = "cash inflow",
        sellflow_dd_w        = "sell flow",
        cf_gap_mkt_dd_w      = "gap mkt, dd leg",
        cf_gap_mkt_rec_w     = "gap mkt, recovery",
        cf_gap_mkt_end_w     = "gap mkt, total",
        ret_next12_from_dd_w = "ret next 12m")

PL <- list(
  n_draws      = 200L,           # placebo episode SETS per mode
  modes        = c("any", "calm"),
  seed         = 20260907L,
  ## a draw is kept only if the treatment still has support. Without this the
  ## distribution is dominated by a few near-collinear draws whose coefficient
  ## is an artefact of three treated clients, not a placebo effect.
  min_treated  = 100L,
  max_attempts = 200L,           # rejection-sampling attempts per draw
  ## keep the MONTHLY geometry identical too, not just the daily length:
  ## eom(start)..eom(start + len) must span the same number of months as the
  ## real window. Set FALSE to widen the candidate sets at the cost of windows
  ## that straddle one month more or less than the episode they stand in for.
  match_n_months = TRUE
)

xs_fml <- function(y, trt = TRT, spec = SPEC) as.formula(sprintf(spec, y, trt))

cat("=============================================================\n")
cat(" PLACEBO CROSS SECTION\n")
cat("=============================================================\n")
cat("spec      : ", SPEC, "\n", sep = "")
cat("treatment : ", TRT, "\n", sep = "")
cat("sample    : ", SAMPLE, "\n", sep = "")
cat("episodes  : ", paste(EPS, collapse = ", "), "\n", sep = "")
cat("draws     : ", PL$n_draws, " per mode (",
    paste(PL$modes, collapse = ", "), ")\n\n", sep = "")

## ---------------------------------------------------------------------------
## 1. the REAL estimate -- read straight off cle.parquet, nothing rebuilt
## ---------------------------------------------------------------------------

ep_all <- load_dt("episodes")[usable == TRUE]      # the 12 estimation blocks
ep_raw <- load_dt("episodes_raw")                  # every hand-dated window

## one row per outcome: the coefficient on TRT and its clustered SE
fit_one <- function(d, y) {
  if (!y %in% names(d)) return(NULL)
  if (uniqueN(d[[TRT]]) < 2L) return(NULL)
  tryCatch(feols(xs_fml(y), d, vcov = ~ advisor_id, notes = FALSE),
           error = function(e) NULL)
}
row_of <- function(d, tag) {
  rbindlist(lapply(names(YS), function(y) {
    m <- fit_one(d, y)
    if (is.null(m) || !TRT %in% names(coef(m)))
      return(data.table(tag = tag, outcome = y, label = YS[[y]],
                        coef = NA_real_, se = NA_real_, p = NA_real_,
                        n = NA_integer_))
    data.table(tag = tag, outcome = y, label = YS[[y]],
               coef = coef(m)[[TRT]], se = se(m)[[TRT]], p = pvalue(m)[[TRT]],
               n = nobs(m))
  }))
}

cle0 <- load_dt("cle")
C0   <- cle0[get(SAMPLE) == 1L][ep_id %in% EPS]
real <- row_of(C0, "real")

cat("-------------------------------------------------------------\n")
cat("1. THE REAL ESTIMATE (results/cache/cle.parquet, exactly as in 06)\n")
cat("-------------------------------------------------------------\n")
print(real[, .(label, coef = round(coef, 4), se = round(se, 4),
               p = round(p, 4), n)])
n_cells_real   <- nrow(C0)
n_treated_real <- C0[, sum(get(TRT))]
cat("\nclient x episode cells: ", n_cells_real, ", treated: ", n_treated_real,
    " (", round(100 * n_treated_real / n_cells_real, 2), "%)\n", sep = "")

rm(cle0); gc(verbose = FALSE)

## ---------------------------------------------------------------------------
## 2. the rebuild -- the slice of 04 + 05 this specification needs
##
## build_cle() lives in placebo_funs.R, shared with 17_es_wrel_placebo.R, so the
## client x episode file rebuilt here and the stacked panel rebuilt there cannot
## drift apart. Step 3 checks it against the real cle before anything is drawn.
## ---------------------------------------------------------------------------

pl_load_inputs()

## ---------------------------------------------------------------------------
## 3. VERIFY the rebuild against the real cle before drawing anything.
##    Same episode dates in, same coefficients out -- otherwise every number
##    below measures the rebuild rather than the window.
##    Note this runs on all 12 usable blocks, not just the 8 in EPS, because
##    that is the set 05 pools over when it winsorises netflow / sellflow.
## ---------------------------------------------------------------------------

cat("\n-------------------------------------------------------------\n")
cat("2. VERIFICATION: build_cle() on the REAL episode dates\n")
cat("-------------------------------------------------------------\n")

chk     <- build_cle(ep_all)
rebuilt <- row_of(chk[get(SAMPLE) == 1L][ep_id %in% EPS], "rebuilt")

cmp <- merge(real[, .(outcome, label, coef_real = coef, n_real = n)],
             rebuilt[, .(outcome, coef_rebuilt = coef, n_rebuilt = n)],
             by = "outcome", sort = FALSE)
cmp[, rel_diff := (coef_rebuilt - coef_real) / abs(coef_real)]
print(cmp[, .(label, coef_real = round(coef_real, 5),
              coef_rebuilt = round(coef_rebuilt, 5),
              rel_diff = signif(rel_diff, 3), n_real, n_rebuilt)])

TOL <- 1e-6
bad <- cmp[!is.finite(rel_diff) | abs(rel_diff) > TOL]
if (nrow(bad)) {
  cat("\n!! the rebuild does not reproduce 06 for: ",
      paste(bad$label, collapse = ", "), "\n", sep = "")
  cat("   Every placebo coefficient would inherit that difference, so the run\n")
  cat("   stops here. Compare build_cle() against 04_treatment.R and\n")
  cat("   05_outcomes.R for the outcomes listed above.\n")
  stop("build_cle() does not reproduce the 06 cross section", call. = FALSE)
}
cat("\nrebuild reproduces every 06 coefficient to within ", TOL,
    " relative -- placebo windows can be trusted to differ only in DATE.\n",
    sep = "")
rm(chk, rebuilt, cmp); gc(verbose = FALSE)

## ---------------------------------------------------------------------------
## 4. candidate start dates, one set per real episode
## ---------------------------------------------------------------------------

geo <- pl_geometry(ep_all)

## every hand-dated stress window, INCLUDING the events that form no estimation
## episode -- a placebo landing on one of those is not a calm window either
STRESS <- ep_raw[, .(s = peak_date, e = trough_date)]

cand_for <- function(g, mode)
  pl_candidates(g, mode, STRESS, PLD$min, PLD$max, PL$match_n_months)

draw_set <- function(cands) pl_draw(cands, geo, PL$max_attempts)


## the placebo ids standing in for the eight episodes the regression uses
PL_EPS <- sprintf("pl%02d", which(geo$src_ep %in% EPS))

## ---------------------------------------------------------------------------
## 5. the placebo runs
## ---------------------------------------------------------------------------

res  <- list()
wins <- list()

for (md in PL$modes) {
  cat("\n-------------------------------------------------------------\n")
  cat("3. PLACEBO DRAWS -- mode '", md, "'\n", sep = "")
  cat("-------------------------------------------------------------\n")

  cands <- lapply(seq_len(nrow(geo)), function(j) cand_for(geo[j], md))
  cat("window geometry and candidate start dates:\n")
  print(data.table(src_ep = geo$src_ep, ep_label = geo$ep_label,
                   len_days = geo$len_days, n_months = geo$n_months,
                   n_pre = geo$n_pre, n_post = geo$n_post,
                   n_cand = lengths(cands)))
  if (any(lengths(cands) == 0L)) {
    cat("\n!! no feasible placebo start for: ",
        paste(geo$src_ep[lengths(cands) == 0L], collapse = ", "),
        "\n   mode '", md, "' skipped. The binding constraint is normally\n",
        "   PL$match_n_months together with the calm requirement on a long\n",
        "   window: a 9-month stretch that touches no hand-dated stress window\n",
        "   and still has its pre and post months inside the sample may not\n",
        "   exist. Set PL$match_n_months <- FALSE to widen the candidate set.\n",
        sep = "")
    next
  }

  set.seed(PL$seed + match(md, PL$modes))
  kept <- 0L; miss_win <- 0L; miss_supp <- 0L
  t0 <- Sys.time()
  for (r in seq_len(PL$n_draws)) {
    epx <- draw_set(cands)
    if (is.null(epx)) { miss_win <- miss_win + 1L; next }
    d <- build_cle(epx)
    if (is.null(d)) { miss_win <- miss_win + 1L; next }
    Cp <- d[get(SAMPLE) == 1L][ep_id %in% PL_EPS]
    if (!nrow(Cp) || sum(Cp[[TRT]]) < PL$min_treated) {
      miss_supp <- miss_supp + 1L; next
    }
    rr <- row_of(Cp, md)
    rr[, `:=`(mode = md, draw = r, n_cells = nrow(Cp),
              n_treated = sum(Cp[[TRT]]), trt_rate = mean(Cp[[TRT]]))]
    res[[length(res) + 1L]]  <- rr
    wins[[length(wins) + 1L]] <- epx[, .(mode = md, draw = r, ep_id, src_ep,
                                         peak_date, trough_date, dd_start,
                                         dd_end, pre_start, post_end)]
    kept <- kept + 1L
    if (r %% 25L == 0L)
      cat(sprintf("  ... %d/%d drawn, %d kept, %.1f min\n", r, PL$n_draws, kept,
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  }
  cat(sprintf(
    "mode '%s': %d kept, %d unplaceable, %d without treatment support (< %d treated), %.1f min\n",
    md, kept, miss_win, miss_supp, PL$min_treated,
    as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}

PLC <- rbindlist(res)

if (!nrow(PLC)) {

  cat("\nno placebo draw produced a usable sample -- nothing to report.\n")
  log_step("16_placebo_xsec: no usable placebo draw")

} else {

WIN <- rbindlist(wins)
save_dt(PLC, "placebo_xsec")

## ---------------------------------------------------------------------------
## 6. report
## ---------------------------------------------------------------------------

cat("\n-------------------------------------------------------------\n")
cat("4. SAMPLE SIZE AND TREATMENT SUPPORT, real vs placebo\n")
cat("-------------------------------------------------------------\n")
cat("real   : ", n_cells_real, " cells, ", n_treated_real, " treated (",
    round(100 * n_treated_real / n_cells_real, 2), "%)\n", sep = "")
print(unique(PLC[, .(mode, draw, n_cells, n_treated, trt_rate)])[
  , .(draws = .N, cells = round(mean(n_cells)),
      treated = round(mean(n_treated)),
      trt_rate = round(mean(trt_rate), 4)), by = mode])
cat("\nA placebo treatment rate far below the real one means the placebo\n")
cat("regressions are less powered than the real one, so a real coefficient\n")
cat("outside the placebo spread is a weaker claim than it looks. Read these\n")
cat("rates before reading the p-values below.\n")

summ <- PLC[, {
  b  <- coef[is.finite(coef)]
  rb <- real[outcome == .BY$outcome, coef]
  .(label   = YS[[.BY$outcome]],
    real    = rb,
    n_draws = length(b),
    pl_mean = mean(b),
    pl_sd   = sd(b),
    pl_p05  = quantile(b, 0.05, names = FALSE),
    pl_p50  = quantile(b, 0.50, names = FALSE),
    pl_p95  = quantile(b, 0.95, names = FALSE),
    ## two-sided: how often is a placebo at least as large in absolute value
    p_two   = mean(abs(b) >= abs(rb)),
    ## one-sided, in the direction the real coefficient actually points
    p_one   = if (rb >= 0) mean(b >= rb) else mean(b <= rb),
    ## the size of the test itself, on the clustered SE
    size_05 = mean(p[is.finite(p)] < 0.05))
}, by = .(mode, outcome)]
summ[, outcome := factor(outcome, levels = names(YS))]
setorder(summ, mode, outcome)

cat("\n-------------------------------------------------------------\n")
cat("5. THE CRISIS-SPECIFICITY TEST\n")
cat("-------------------------------------------------------------\n")
for (m in unique(summ$mode)) {
  cat("\n== mode '", m, "' ==\n", sep = "")
  print(summ[mode == m, .(label, real = round(real, 4),
                          pl_mean = round(pl_mean, 4), pl_sd = round(pl_sd, 4),
                          pl_p05 = round(pl_p05, 4), pl_p95 = round(pl_p95, 4),
                          p_two = round(p_two, 3), p_one = round(p_one, 3),
                          size_05 = round(size_05, 3), n_draws)])
}

cat("\nHOW TO READ THIS\n")
cat("  real      the 06_specs coefficient on ", TRT, "\n", sep = "")
cat("  pl_mean   the same coefficient averaged over placebo window sets\n")
cat("  p_two     share of placebo draws at least as large in ABSOLUTE value as\n")
cat("            the real one -- the empirical p-value of the placebo test\n")
cat("  p_one     share at least as extreme in the real coefficient's OWN\n")
cat("            direction; the honest number when the sign is the claim\n")
cat("  size_05   share of placebo draws themselves significant at 5% on the\n")
cat("            clustered SE. This is the SIZE of the specification. Well\n")
cat("            above 0.05 means the reported standard errors are too small\n")
cat("            for this design and the real p-value is optimistic whatever\n")
cat("            the placebo spread says.\n\n")
cat("An effect is crisis-specific when p_two is small AND pl_mean sits near\n")
cat("zero. A real coefficient inside the placebo spread is not a crisis effect;\n")
cat("it is what an advisor contact does in any window of that length. A pl_mean\n")
cat("far from zero says the coefficient is picking up who gets contacted rather\n")
cat("than what the contact does in a crisis.\n")

## ---------------------------------------------------------------------------
## 7. table + figure
## ---------------------------------------------------------------------------

texf <- file.path(TAB_DIR, "tab_placebo_xsec.tex")
writeLines(c(
  "\\begin{tabular}{llrrrrrrr}",
  "\\hline\\hline",
  paste("mode & outcome & real & placebo mean & placebo sd & p5 & p95 &",
        "$p$ (2-sided) & size(5\\%) \\\\"),
  "\\hline",
  summ[, sprintf("%s & %s & %.4f & %.4f & %.4f & %.4f & %.4f & %.3f & %.3f \\\\",
                 mode, label, real, pl_mean, pl_sd, pl_p05, pl_p95, p_two,
                 size_05)],
  "\\hline\\hline",
  "\\end{tabular}"), texf)
cat("\nwritten: ", texf, "\n", sep = "")

pl <- merge(PLC[is.finite(coef), .(mode, draw, outcome, coef)],
            real[, .(outcome, label, real = coef)], by = "outcome")
pl[, label := factor(label, levels = unname(YS))]
gg <- ggplot(pl, aes(coef, fill = mode)) +
  geom_histogram(bins = 30, alpha = 0.55, position = "identity", colour = NA) +
  geom_vline(aes(xintercept = real), linewidth = 0.7, colour = "#C0392B") +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey40", linetype = 2) +
  facet_wrap(~ label, scales = "free", ncol = 3) +
  scale_fill_manual(values = c(any = "grey45", calm = "#2E86C1")) +
  labs(x = sprintf("coefficient on %s", TRT), y = "placebo draws", fill = NULL,
       title = "The 06_specs cross section on placebo windows",
       subtitle = paste("red line = the real estimate on the eight stress",
                        "episodes; dashed = zero")) +
  theme_light() + theme(legend.position = "bottom")

figf <- file.path(FIG_DIR, "16_placebo_xsec.pdf")
if (pdf_ok(figf, width = 11, height = 8)) {
  print(gg); dev.off()
  cat("written: ", figf, "\n", sep = "")
}

cat("\nthe first placebo window set of each mode, for the record:\n")
print(WIN[, .SD[draw == min(draw)], by = mode][
  , .(mode, src_ep, peak_date, trough_date, dd_start, dd_end, pre_start, post_end)])

log_step(sprintf("placebo cross section: %d draws over %d mode(s)",
                 uniqueN(PLC[, .(mode, draw)]), uniqueN(PLC$mode)))
}

sink()
