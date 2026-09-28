## =============================================================================
## 14_itt_placebo_robustness.R -- placebo episodes, permutation test, and the
##                                robustness grid for the habitual-window ITT
##
## Reads results/cache/itt_window.parquet (12) and results/cache/itt_by_episode
## (13). Rebuilds windows from scratch wherever the grid changes a parameter
## that feeds the window itself, using the shared code in itt_funs.R.
##
## Output: results/itt/tab_placebo.tex, tab_robustness.tex
##         results/itt/fig_robustness_<outcome>.pdf
##         results/itt/fig_episode_forest.pdf
##         results/14_itt_placebo_robustness.txt
## =============================================================================

source("00_setup.R")
source("itt_funs.R")
log_init("14_itt_placebo_robustness")

ITT_DIR <- file.path(RESULTS, "itt")
dir.create(ITT_DIR, showWarnings = FALSE, recursive = TRUE)

sink(file.path(RESULTS, "14_itt_placebo_robustness.txt"), split = TRUE)
on.exit(sink(), add = TRUE)

set.seed(20260831)

CTRL <- c("log_w_pre", "n_assets_pre", "contacts_pre", "ret_past12_pre")
XCTL <- paste(CTRL, collapse = " + ")
CL   <- ~ advisor_id
PRIMARY <- c(d_risky_dd_w    = "d risky share",
             netflow_dd_w    = "net flow",
             d_cash_dd_w     = "cash inflow",
             cf_gap_mkt_dd_w = "gap vs mkt, dd leg")
EP_DROP <- c("ep1_201305", "ep10_202201")
BASE    <- list(arc_w = 3L, leave_out = 12L, min_years = 3L, min_reg = 0.6)

cd  <- load_dt("contacts_d")
ep  <- load_dt("episodes")[usable == TRUE][!ep_id %in% EP_DROP]
cle <- load_dt("cle")
idx <- load_dt("index_m")
Q   <- cd[perf_inv_a_p == 1L, .(Bp_ID, ContactDate,
                                mo = month(ContactDate), yr = year(ContactDate))]

## a small helper: run the four primary reduced forms and return a tidy table
rf_row <- function(d, zvar = "Z", fe = "ep_id + Bp_ID", label = "", dim = "") {
  rbindlist(lapply(seq_along(PRIMARY), function(k) {
    y <- names(PRIMARY)[k]
    if (!y %in% names(d) || uniqueN(d[[zvar]]) < 2L || nrow(d) < 100L)
      return(data.table(dim = dim, label = label, outcome = PRIMARY[k],
                        coef = NA_real_, se = NA_real_, p = NA_real_, n = nrow(d)))
    m <- feols(as.formula(sprintf("%s ~ %s + %s | %s", y, zvar, XCTL, fe)),
               d, vcov = CL, notes = FALSE)
    data.table(dim = dim, label = label, outcome = PRIMARY[k],
               coef = coef(m)[zvar], se = se(m)[zvar], p = pvalue(m)[zvar],
               n = nobs(m))
  }))
}

## =============================================================================
## Step 7 -- robustness grid
## =============================================================================

cat("\n================ Step 7: robustness grid ================\n\n")

## windows have to be rebuilt whenever arc width changes; the other dimensions
## are filters on an already-built window
Wcache <- list()
get_W <- function(arc_w) {
  key <- as.character(arc_w)
  if (is.null(Wcache[[key]])) {
    W <- build_window(Q, ep, arc_w, BASE$leave_out)
    W <- add_Z(W, ep, "dd_start", "dd_end",   "Z",      arc_w)
    W <- add_Z(W, ep, "dd_start", "post_end", "Z_post", arc_w)
    e6 <- copy(ep)[, dd_p6 := madd(dd_start, 6L)]
    W <- add_Z(W, e6, "dd_start", "dd_p6",    "Z_tau6", arc_w)
    Wcache[[key]] <<- W
  }
  Wcache[[key]]
}

make_sample <- function(arc_w = BASE$arc_w, min_years = BASE$min_years,
                        min_reg = BASE$min_reg, ep_keep = ep$ep_id) {
  W <- get_W(arc_w)
  d <- merge(cle[smp_mb == 1L & ep_id %in% ep_keep], W, by = c("Bp_ID", "ep_id"))
  d <- d[n_years_pre >= min_years & regularity >= min_reg]
  d[, D := treat_perf_inv_a_p]
  d[complete.cases(d[, ..CTRL])]
}

GRID <- list()
add_grid <- function(dt) GRID[[length(GRID) + 1L]] <<- dt

## baseline, repeated so every plot has its anchor
add_grid(rf_row(make_sample(), label = "baseline", dim = "baseline"))

## 1. arc width
for (w in c(2L, 3L, 4L))
  add_grid(rf_row(make_sample(arc_w = w),
                  label = sprintf("arc = %d mo", w), dim = "arc width"))

## 2. regularity threshold
for (r in c(0.5, 0.6, 0.75))
  add_grid(rf_row(make_sample(min_reg = r),
                  label = sprintf("regularity >= %.2f", r), dim = "regularity"))

## 3. years of pre-episode history
for (ny in c(2L, 3L, 4L))
  add_grid(rf_row(make_sample(min_years = ny),
                  label = sprintf("n_years_pre >= %d", ny), dim = "pre-years"))

## 4. overlap definition
##    Z      : [dd_start, dd_end]  -- peak month to trough month (baseline)
##    Z_post : [dd_start, post_end]-- through the recovery leg
##    Z_tau6 : [dd_start, +6]      -- tau in [0, +6]
for (z in c("Z", "Z_post", "Z_tau6"))
  add_grid(rf_row(make_sample(), zvar = z,
                  label = switch(z, Z = "[start, trough]",
                                 Z_post = "[start, recovery end]",
                                 Z_tau6 = "tau in [0, +6]"),
                  dim = "overlap window"))

## 5. episode exclusions
add_grid(rf_row(make_sample(ep_keep = setdiff(ep$ep_id, "ep9_202002")),
                label = "drop Covid", dim = "episodes"))
nbig <- make_sample()[, .N, by = ep_id][order(-N)][1, ep_id]
add_grid(rf_row(make_sample(ep_keep = setdiff(ep$ep_id, nbig)),
                label = paste("drop", nbig), dim = "episodes"))
cat("largest baseline episode dropped in the grid:", nbig, "\n")

## 6. episode fixed effects only (no client FE)
add_grid(rf_row(make_sample(), fe = "ep_id", label = "episode FE only",
                dim = "fixed effects"))

G <- rbindlist(GRID)
print(G[, .(dim, label, outcome, coef = round(coef, 4), se = round(se, 4),
            p = round(p, 3), n)])
save_dt(G, "itt_robustness_grid")

## ---- coefficient-stability plot, one per primary outcome ------------------

G[, label := factor(label, levels = rev(unique(label)))]
for (k in seq_along(PRIMARY)) {
  g <- G[outcome == PRIMARY[k] & !is.na(coef)]
  p <- ggplot(g, aes(x = coef, y = label, color = dim)) +
    geom_vline(xintercept = 0, linetype = 2, colour = "grey40") +
    geom_errorbarh(aes(xmin = coef - 1.96 * se, xmax = coef + 1.96 * se),
                   height = 0.25) +
    geom_point(size = 2) +
    labs(x = "ITT coefficient on Z (95% CI)", y = NULL, color = NULL,
         title = paste0("Coefficient stability: ", PRIMARY[k]),
         subtitle = "episode and client fixed effects unless stated; advisor-clustered SE") +
    theme_light() + theme(legend.position = "bottom")
  fn <- file.path(ITT_DIR, sprintf("fig_robustness_%s.pdf", names(PRIMARY)[k]))
  ggsave(fn, p, width = 8, height = 6)
  cat("wrote", fn, "\n")
}

## ---- episode-by-episode forest plot ---------------------------------------

byep <- load_dt("itt_by_episode")
byep <- byep[!is.na(coef)]
byep[, outcome_lab := PRIMARY[outcome]]
pf <- ggplot(byep, aes(x = coef, y = ep_id)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey40") +
  geom_errorbarh(aes(xmin = coef - 1.96 * se, xmax = coef + 1.96 * se), height = 0.25) +
  geom_point(size = 2) +
  facet_wrap(~ outcome_lab, scales = "free_x") +
  labs(x = "ITT coefficient on Z (95% CI)", y = NULL,
       title = "Episode-by-episode reduced form",
       subtitle = "one regression per episode, no fixed effects, advisor-clustered SE") +
  theme_light()
ggsave(file.path(ITT_DIR, "fig_episode_forest.pdf"), pf, width = 10, height = 7)
cat("wrote", file.path(ITT_DIR, "fig_episode_forest.pdf"), "\n")

## =============================================================================
## Step 6 -- placebo
##
## Two placebos, because the one the brief asks for cannot be built as specified
## in this sample and the reason is worth showing rather than hiding.
##
## A. PSEUDO-EPISODES in calm periods. Same number of episodes, same window
##    lengths. The brief asks for no real episode within +/- 12 months; the 18
##    hand-dated stress events in episodes_raw are so densely packed that the
##    +/- 12 month rule leaves no admissible month at all. The gap is therefore
##    relaxed step by step and the value actually used is reported.
##
## B. PERMUTATION of the habitual arc across clients. Each client is given a
##    random arc drawn from the empirical arc distribution, held fixed across
##    episodes so the panel structure survives, and Z is recomputed. This needs
##    no reconstruction of outcomes and is the cleaner test of the two: if the
##    real Z carries information, its coefficient should sit in the tail of the
##    permutation distribution.
## =============================================================================

cat("\n\n================ Step 6: placebo ================\n\n")

## ---------------------------------------------------------------------------
## B. permutation placebo (cheap, exact, no reconstruction)
## ---------------------------------------------------------------------------

D0 <- make_sample()
real <- rf_row(D0, label = "real Z", dim = "real")

NPERM <- 400L
arcs  <- unique(D0[, .(Bp_ID, arc_start)])
cmL   <- lapply(seq_len(nrow(ep)), function(i) months_between(ep$dd_start[i], ep$dd_end[i]))
names(cmL) <- ep$ep_id
A3 <- arc_mat(BASE$arc_w)

perm <- rbindlist(lapply(seq_len(NPERM), function(b) {
  pa <- data.table(Bp_ID = arcs$Bp_ID, arc_p = sample(arcs$arc_start))
  d  <- merge(D0, pa, by = "Bp_ID")
  d[, Zp := as.integer(mapply(function(s, e) any(A3[cmL[[e]], s] == 1L), arc_p, ep_id))]
  r  <- rf_row(d, zvar = "Zp", label = paste0("perm", b), dim = "permutation")
  r[, b := b][]
}))

cat("permutation placebo:", NPERM, "draws, client-level arc reassignment\n\n")
pp <- merge(perm[, .(outcome, b, coef)], real[, .(outcome, real = coef)], by = "outcome")
psum <- pp[, .(perm_mean = mean(coef, na.rm = TRUE),
               perm_sd   = sd(coef, na.rm = TRUE),
               real      = real[1],
               p_perm    = mean(abs(coef) >= abs(real[1]), na.rm = TRUE)), by = outcome]
print(psum[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 4) else x)])
cat("\np_perm is the share of permutation draws with |coef| at least as large as",
    "\nthe real one. A value near 1 means the real Z is indistinguishable from",
    "\na randomly assigned window -- which, for a set of null results, is exactly",
    "\nwhat should happen.\n")

## ---------------------------------------------------------------------------
## A. pseudo-episodes in calm periods
## ---------------------------------------------------------------------------

## realized volatility of the daily SPI TR, by month. The brief asks for the
## VIX; there is no VIX file in this repo (vix.R pulls it from Yahoo at run
## time) so the calm criterion uses the realized volatility of the index that
## defines the episodes in the first place.
spi <- fread(SPI_FILE, skip = 5L, select = c(1L, 2L), header = FALSE,
             col.names = c("Date", "spi"))
spi[, Date := as.Date(Date, format = "%d.%m.%Y")]
spi <- spi[!is.na(Date) & !is.na(spi)][order(Date)]
spi[, r := spi / shift(spi) - 1]
vol <- spi[!is.na(r), .(rv = sd(r)), by = .(MDate = eom(Date))]
vol <- vol[MDate >= P$smp_start & MDate <= P$smp_end]
vol[, calm_vol := rv < median(rv)]
cat("\nmonthly realized volatility of the SPI TR: median =",
    round(median(vol$rv), 5), "\n")

epr <- load_dt("episodes_raw")
all_months <- vol$MDate

stress_months <- unique(unlist(lapply(seq_len(nrow(epr)), function(i)
  mseq(epr$dd_start[i], epr$dd_end[i]))))
stress_months <- as.Date(stress_months, origin = "1970-01-01")

## months at least `gap` months away from every stress month
calm_at <- function(gap) {
  bad <- unique(unlist(lapply(stress_months, function(m) madd(m, -gap:gap))))
  setdiff(all_months, as.Date(bad, origin = "1970-01-01"))
}

lens <- sapply(seq_len(nrow(ep)), function(i)
  length(months_between(ep$dd_start[i], ep$dd_end[i])))
cat("pseudo-episode lengths to place (months):", paste(lens, collapse = ", "), "\n")

place <- function(gap) {
  ## the gap rule applies month by month; the volatility rule applies to the
  ## WINDOW. Requiring every single month of a 4-month window to sit below the
  ## median individually is a much stronger condition than "this is a calm
  ## stretch", and with 12 years of data it admits nothing.
  cm     <- as.Date(calm_at(gap), origin = "1970-01-01")
  med_rv <- median(vol$rv)
  rvm    <- setNames(vol$rv, format(vol$MDate))
  ok <- function(st, L) {
    win <- madd(st, 0:(L - 1L))
    all(win %in% cm) && (st %m-% months(24)) >= P$smp_start &&
      max(win) <= P$smp_end &&
      mean(rvm[format(win)], na.rm = TRUE) < med_rv
  }
  used <- as.Date(character(0)); out <- list()
  for (i in order(-lens)) {
    ## index the Date vector rather than sapply over it: sapply strips the Date
    ## class from each element and the predicate then returns a ragged list
    cand <- cm[vapply(seq_along(cm), function(j) ok(cm[j], lens[i]), logical(1))]
    if (length(cand))
      cand <- cand[vapply(seq_along(cand), function(j)
        !any(madd(cand[j], 0:(lens[i] - 1L)) %in% used), logical(1))]
    if (!length(cand)) return(NULL)
    st <- cand[sample.int(length(cand), 1L)]
    used <- c(used, madd(st, 0:(lens[i] - 1L)))
    out[[length(out) + 1L]] <- data.table(
      ep_id = sprintf("pl%d_%s", i, format(st, "%Y%m")),
      ps_start = st, ps_end = madd(st, lens[i] - 1L), len = lens[i])
  }
  rbindlist(out)
}

GAPS <- c(12L, 9L, 6L, 3L, 1L, 0L)
PS <- NULL; gap_used <- NA_integer_
for (g in GAPS) {
  nm <- length(calm_at(g))
  got <- if (is.null(PS)) place(g) else NULL
  cat(sprintf("  gap +/- %2d months: %2d months outside every stress window -- %s\n",
              g, nm, if (is.null(PS) && is.null(got)) "cannot place all windows"
                     else if (is.null(PS)) "all windows placed" else "not needed"))
  if (is.null(PS) && !is.null(got)) { PS <- got; gap_used <- g }
}

if (is.null(PS)) {
  cat("\n!!! No set of pseudo-episodes can be placed at any gap. Placebo A is",
      "\n!!! not reported. Placebo B (permutation) stands on its own.\n")
} else {
  if (gap_used < 12L)
    cat(sprintf(paste0("\n!!! The +/- 12 month rule leaves no admissible month: the 18 hand-dated\n",
                       "!!! stress events cover the sample too densely. The pseudo-episodes below\n",
                       "!!! use a +/- %d month gap instead. They therefore sit CLOSER to real\n",
                       "!!! stress than the design asks for, which biases the placebo TOWARD\n",
                       "!!! finding an effect -- read a null here as reassuring, not as proof.\n"),
               gap_used))
  setorder(PS, ps_start)
  PS[, `:=`(pre_month = madd(ps_start, -1L), pre_start = madd(ps_start, -12L))]
  print(PS)

  ## ---- rebuild covariates and outcomes on the pseudo windows --------------
  pcols <- c("Bp_ID", "MDate", "observed", "wealth", "log_w", "n_assets",
             "ret_past12", "discretionary", "main_bank_yn", "Hauptbetreuer_ID",
             "cash_free", "risky_share_c", "chf_net", "c_advice")
  pan <- setDT(arrow::read_parquet(file.path(CACHE, "panel.parquet"),
                                   col_select = all_of(pcols)))
  setkey(pan, Bp_ID, MDate)

  build_pseudo <- function(EPX, s_col, e_col, pm_col, ps_col) {
    grid <- CJ(Bp_ID = unique(pan$Bp_ID), ep_id = EPX$ep_id, unique = TRUE)
    grid <- merge(grid, EPX, by = "ep_id")
    ## pre-period snapshot
    pre <- pan[, .(Bp_ID, MDate, obs_pre = observed, wealth_pre = wealth,
                   log_w_pre = log_w, n_assets_pre = n_assets,
                   ret_past12_pre = ret_past12, discr_pre = discretionary,
                   main_bank_pre = main_bank_yn, advisor_id = Hauptbetreuer_ID,
                   cash_pre = cash_free, risky_share_pre = risky_share_c)]
    g <- merge(grid, pre, by.x = c("Bp_ID", pm_col), by.y = c("Bp_ID", "MDate"))
    g <- g[obs_pre == 1L & wealth_pre >= P$min_wealth_pre &
           discr_pre == 0L & main_bank_pre == 1L]
    ## contacts over the 12 pre-months
    cp <- pan[g[, .(Bp_ID, ep_id, a = get(ps_col), b = get(pm_col))],
              on = .(Bp_ID, MDate >= a, MDate <= b),
              .(Bp_ID, ep_id, c_advice), allow.cartesian = TRUE][
      , .(contacts_pre = sum(c_advice == 1L, na.rm = TRUE)), by = .(Bp_ID, ep_id)]
    g <- merge(g, cp, by = c("Bp_ID", "ep_id"), all.x = TRUE)
    g[is.na(contacts_pre), contacts_pre := 0L]
    ## window aggregates
    wn <- pan[g[, .(Bp_ID, ep_id, a = get(s_col), b = get(e_col))],
              on = .(Bp_ID, MDate >= a, MDate <= b),
              .(Bp_ID, ep_id, MDate = x.MDate, chf_net, wealth,
                risky_share_c, cash_free), allow.cartesian = TRUE]
    wn <- wn[!is.na(MDate)]
    setorder(wn, Bp_ID, ep_id, MDate)
    agg <- wn[, .(net_chf = sum(chf_net, na.rm = TRUE),
                  wealth_end = last(wealth),
                  risky_end  = last(risky_share_c),
                  cash_end   = last(cash_free)), by = .(Bp_ID, ep_id)]
    g <- merge(g, agg, by = c("Bp_ID", "ep_id"))
    ## index growth over the same window, for the market counterfactual
    ix <- merge(EPX[, .(ep_id, a = get(s_col), b = get(e_col), pm = get(pm_col))],
                idx[, .(pm = MDate, ix_pre = spi_tr)], by = "pm")
    ix <- merge(ix, idx[, .(b = MDate, ix_end = spi_tr)], by = "b")
    g <- merge(g, ix[, .(ep_id, ix_pre, ix_end)], by = "ep_id")
    ## the four primary outcomes, same formulas as 05_outcomes.R
    g[, `:=`(netflow_dd    = net_chf / wealth_pre,
             d_risky_dd    = risky_end - risky_share_pre,
             d_cash_dd     = (cash_end - cash_pre) / wealth_pre,
             cf_gap_mkt_dd = (wealth_end / wealth_pre) / (ix_end / ix_pre) - 1)]
    for (v in c("netflow_dd", "d_risky_dd", "d_cash_dd", "cf_gap_mkt_dd"))
      g[, paste0(v, "_w") := winsor(get(v), P$win_p)]
    g[]
  }

  ## the real episodes, rebuilt with the SAME simplified code, so the placebo
  ## and the benchmark differ only in where the window sits
  EPR <- ep[, .(ep_id, ps_start = dd_start, ps_end = dd_end,
                pre_month, pre_start)]
  REALX <- build_pseudo(EPR, "ps_start", "ps_end", "pre_month", "pre_start")
  PSX   <- build_pseudo(PS[, .(ep_id, ps_start, ps_end, pre_month, pre_start)],
                        "ps_start", "ps_end", "pre_month", "pre_start")

  ## sanity: does the simplified reconstruction reproduce the pipeline?
  chk <- merge(REALX[, .(Bp_ID, ep_id, netflow_dd_w, d_risky_dd_w,
                         d_cash_dd_w, cf_gap_mkt_dd_w)],
               cle[, .(Bp_ID, ep_id, p_net = netflow_dd_w, p_rsk = d_risky_dd_w,
                       p_csh = d_cash_dd_w, p_gap = cf_gap_mkt_dd_w)],
               by = c("Bp_ID", "ep_id"))
  cat("\nreconstruction check against the pipeline (correlation on real episodes):\n")
  cat(sprintf("  net flow      %.3f\n", cor(chk$netflow_dd_w, chk$p_net, use = "pair")))
  cat(sprintf("  d risky share %.3f\n", cor(chk$d_risky_dd_w, chk$p_rsk, use = "pair")))
  cat(sprintf("  cash inflow   %.3f\n", cor(chk$d_cash_dd_w, chk$p_csh, use = "pair")))
  cat(sprintf("  gap vs mkt    %.3f\n", cor(chk$cf_gap_mkt_dd_w, chk$p_gap, use = "pair")))

  ## ---- windows and Z on the pseudo episodes -------------------------------
  attach_Z <- function(g, EPX, s_col, e_col) {
    W <- build_window(Q, EPX, BASE$arc_w, BASE$leave_out, start_col = s_col)
    W <- add_Z(W, EPX, s_col, e_col, "Z", BASE$arc_w)
    d <- merge(g, W, by = c("Bp_ID", "ep_id"))
    d <- d[n_years_pre >= BASE$min_years & regularity >= BASE$min_reg]
    d[complete.cases(d[, ..CTRL])]
  }
  PL <- attach_Z(PSX,   PS[, .(ep_id, ps_start, ps_end)],   "ps_start", "ps_end")
  RB <- attach_Z(REALX, EPR[, .(ep_id, ps_start, ps_end)],  "ps_start", "ps_end")

  cat("\nplacebo sample:", nrow(PL), "rows,", uniqueN(PL$Bp_ID), "clients,",
      "P(Z=1) =", round(mean(PL$Z), 3), "\n")
  cat("real-window benchmark, rebuilt the same way:", nrow(RB), "rows,",
      uniqueN(RB$Bp_ID), "clients, P(Z=1) =", round(mean(RB$Z), 3), "\n\n")

  plc  <- rf_row(PL, label = "placebo episodes", dim = "placebo")
  benc <- rf_row(RB, label = "real episodes, rebuilt", dim = "benchmark")
  PLAC <- rbind(benc, plc)
  print(PLAC[, .(dim, outcome, coef = round(coef, 4), se = round(se, 4),
                 p = round(p, 3), n)])
  save_dt(PLAC, "itt_placebo")

  ## ---- tab_placebo.tex ----------------------------------------------------
  wide <- dcast(PLAC, outcome ~ dim, value.var = c("coef", "se", "n"))
  rows <- sprintf("%s & %s & %s & %s & %s \\\\",
                  wide$outcome,
                  sprintf("%.4f", wide$coef_benchmark),
                  sprintf("(%.4f)", wide$se_benchmark),
                  sprintf("%.4f", wide$coef_placebo),
                  sprintf("(%.4f)", wide$se_placebo))
  cat("\\begin{table}[htbp]\n\\centering\n",
      "\\caption{Placebo: pseudo-episodes drawn from calm periods}\n",
      "\\label{tab:itt_placebo}\n\\begin{tabular}{lcccc}\n\\hline\\hline\n",
      " & \\multicolumn{2}{c}{Real episodes} & \\multicolumn{2}{c}{Placebo episodes} \\\\\n",
      "\\cline{2-3}\\cline{4-5}\n",
      "Outcome & Coef. & SE & Coef. & SE \\\\\n\\hline\n",
      paste(rows, collapse = "\n"), "\n\\hline\\hline\n\\end{tabular}\n",
      "\\begin{minipage}{\\linewidth}\\footnotesize\n",
      sprintf(paste0("Notes: both columns use outcomes rebuilt with the same simplified code, ",
                     "so they differ only in where the window sits. Pseudo-episodes match the ",
                     "real ones in number and length and are drawn from months at least %d ",
                     "months from any hand-dated stress event with below-median realized ",
                     "volatility. Episode and client fixed effects; standard errors clustered ",
                     "on advisor. Real-episode $N$ = %s, placebo $N$ = %s.\n"),
              gap_used, format(wide$n_benchmark[1], big.mark = ","),
              format(wide$n_placebo[1], big.mark = ",")),
      "\\end{minipage}\n\\end{table}\n",
      sep = "", file = file.path(ITT_DIR, "tab_placebo.tex"))
  cat("\nwrote", file.path(ITT_DIR, "tab_placebo.tex"), "\n")
}

## ---- robustness table ------------------------------------------------------

gw <- dcast(G[!is.na(coef)], dim + label ~ outcome, value.var = c("coef", "se"))
rw <- sprintf("%s & %s & %s & %s & %s \\\\", gsub("_", "\\\\_", gw$label),
              sprintf("%.4f", gw[["coef_d risky share"]]),
              sprintf("%.4f", gw[["coef_net flow"]]),
              sprintf("%.4f", gw[["coef_cash inflow"]]),
              sprintf("%.4f", gw[["coef_gap vs mkt, dd leg"]]))
cat("\\begin{table}[htbp]\n\\centering\n",
    "\\caption{Robustness of the ITT coefficient on $Z$}\n",
    "\\label{tab:itt_robustness}\n\\begin{tabular}{lcccc}\n\\hline\\hline\n",
    "Specification & $\\Delta$ risky & Net flow & Cash inflow & Gap vs mkt \\\\\n\\hline\n",
    paste(rw, collapse = "\n"), "\n\\hline\\hline\n\\end{tabular}\n",
    "\\begin{minipage}{\\linewidth}\\footnotesize\n",
    "Notes: each row varies one dimension of the design and holds the rest at the baseline. Episode and client fixed effects unless the row says otherwise; standard errors clustered on advisor.\n",
    "\\end{minipage}\n\\end{table}\n",
    sep = "", file = file.path(ITT_DIR, "tab_robustness.tex"))

cat("\n\nfiles in", ITT_DIR, ":\n")
print(list.files(ITT_DIR))
