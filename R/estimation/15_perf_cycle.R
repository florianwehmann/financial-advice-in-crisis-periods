## =============================================================================
## 15_perf_cycle.R -- is there a per-client CYCLE in advisor-initiated review
##                    meetings, and does it differ across Anlagepaket?
##
## The eyeball impression is that review meetings are cyclical but that the
## cycle is client-specific in BOTH dimensions: different clients sit in
## different quarters, and different clients run on different frequencies
## (annual, semi-annual, every two years). This script measures both, per
## client, and then compares packages.
##
## Two independent measurements, because they can disagree and the disagreement
## is informative:
##
##   FREQUENCY  the gap between consecutive meetings. Median gap gives the
##              period, the IQR of gaps gives how metronomic the client is.
##              Robust, but it says nothing if meetings are missed.
##
##   PHASE      treat the meeting dates as a point process and compute, for each
##              candidate period P, the circular resultant length
##                  R(P) = |mean over meetings of exp(2*pi*i*t/P)|
##              where t is months since the start of the sample. R(P) = 1 means
##              every meeting falls at exactly the same phase of the P-month
##              cycle; R(P) = 0 means the phases are spread out. This is the
##              periodogram of the point process evaluated at frequency 1/P,
##              and it survives missed meetings, which the gap measure does not.
##
## Choosing P from R(P) needs care: a client seen every 12 months is ALSO
## perfectly aligned modulo 6, 4 and 3, because those are harmonics of 12. The
## shortest period with high R is therefore always wrong. The rule used here is
## the LONGEST period at which the Rayleigh test rejects -- which returns 12 for
## an annual client (R is 0 at 24, since alternate meetings land antipodally)
## and 24 for a biennial one. The harmonics run one way only, so the longest
## significant period cannot overshoot the true one.
##
## HEADLINE: the gap measure is the primary classification of FREQUENCY, because
## it works at three meetings where no phase test can. The phase measure answers
## the separate question of whether a client is locked to a calendar month --
## and the two disagree for most clients, which is the substantive finding.
##
## Output: results/cycle/*.tex, results/cycle/*.pdf
##         results/15_perf_cycle.txt
## =============================================================================

source("00_setup.R")
log_init("15_perf_cycle")

CYC_DIR <- file.path(RESULTS, "cycle")
dir.create(CYC_DIR, showWarnings = FALSE, recursive = TRUE)

sink(file.path(RESULTS, "15_perf_cycle.txt"), split = TRUE)
on.exit(sink(), add = TRUE)

## ---------------------------------------------------------------------------
## parameters
## ---------------------------------------------------------------------------

CY <- list(
  ## candidate periods in months: quarterly, 4-monthly, semi-annual, annual,
  ## 18-monthly, biennial, three-yearly
  periods   = c(3L, 4L, 6L, 12L, 18L, 24L, 36L),
  ## a period is only admissible for a client if at least this many full cycles
  ## fit inside the span of that client's meetings. Without it, P = 36 wins
  ## trivially for anyone whose meetings span less than 36 months.
  min_cycles = 2,
  ## a client needs at least this many distinct meeting months to be typed
  min_meet   = 3L,
  ## a period counts as real when the Rayleigh test rejects "phases are spread
  ## evenly" at this level. NOT a fixed threshold on R: R falls mechanically as
  ## the number of meetings grows, so any absolute cut-off marks the clients
  ## with the MOST meetings -- the ones whose cycle is best documented -- as
  ## acyclical. The Rayleigh statistic n*R^2 is the sample-size-aware version.
  ## 7 periods are tested per client, so the level is tightened accordingly
  ## rather than left at 0.05.
  alpha      = 0.01
)
str(CY); cat("\n")

ORIGIN <- P$smp_start   # months are counted from here

## ---------------------------------------------------------------------------
## per-client cycle statistics
## ---------------------------------------------------------------------------

cycle_stats <- function(M) {
  ## M: Bp_ID, MDate (one row per client-month with a meeting)
  M <- unique(M[, .(Bp_ID, MDate)])
  setorder(M, Bp_ID, MDate)
  M[, t := mdiff(MDate, ORIGIN)]

  base <- M[, .(n_meet = .N,
                first  = min(MDate), last = max(MDate),
                span   = max(t) - min(t)), by = Bp_ID]

  ## ---- frequency: gaps between consecutive meetings ----------------------
  M[, gap := t - shift(t), by = Bp_ID]
  gaps <- M[!is.na(gap), .(gap_med = as.numeric(median(gap)),
                           gap_iqr = as.numeric(IQR(gap)),
                           gap_min = min(gap), gap_max = max(gap),
                           n_gaps  = .N), by = Bp_ID]
  base <- merge(base, gaps, by = "Bp_ID", all.x = TRUE)

  ## ---- phase: resultant length at each candidate period ------------------
  RS <- rbindlist(lapply(CY$periods, function(p)
    M[, .(P = p, n = .N,
          C = mean(cos(2 * pi * t / p)),
          S = mean(sin(2 * pi * t / p))), by = Bp_ID]))
  RS[, R := sqrt(C^2 + S^2)]
  ## phase in months into the cycle, and the Rayleigh test of "no concentration"
  ## (Zar's approximation, exact enough for n >= 3)
  RS[, phase := ((atan2(S, C)) %% (2 * pi)) / (2 * pi) * P]
  RS[, p_rayleigh := {
    Rs <- n * R
    exp(sqrt(1 + 4 * n + 4 * (n^2 - Rs^2)) - (1 + 2 * n))
  }]

  ## admissibility: enough full cycles inside the client's own span
  RS <- merge(RS, base[, .(Bp_ID, span, n_meet)], by = "Bp_ID")
  RS <- RS[n_meet >= CY$min_meet & span >= CY$min_cycles * P]

  ## ---- pick the dominant period -----------------------------------------
  ## The longest SIGNIFICANT period, and nothing else. Requiring R to also be
  ## near the client's maximum looks sensible and is not: with a month or two
  ## of jitter a harmonic can edge past the true period on R alone, and the
  ## true period is then knocked out -- an annual client whose meetings drift
  ## by a month gets labelled quarterly. Significance is enough on its own,
  ## because the harmonics run one way only: an annual client IS concentrated
  ## modulo 3, 4 and 6, but a quarterly client is spread evenly modulo 12, so
  ## the longest significant period cannot overshoot.
  RS[, maxR := max(R), by = Bp_ID]
  win <- RS[p_rayleigh < CY$alpha]
  star <- win[order(Bp_ID, -P)][, .SD[1L], by = Bp_ID,
                                .SDcols = c("P", "R", "phase", "p_rayleigh", "maxR")]
  setnames(star, c("Bp_ID", "P_star", "R_star", "phase_star", "p_star", "maxR"))

  ## the ANNUAL component, kept for every client regardless of P_star: "which
  ## quarter is this client seen in" is a calendar question, so it is always
  ## the 12-month resultant that answers it
  a12 <- RS[P == 12L, .(Bp_ID, R12 = R, phase12 = phase, p12 = p_rayleigh)]
  ## phase12 is months after ORIGIN within the year -> calendar month.
  ## Wrap BEFORE rounding: phase12 lives on [0, 12) and a value of 11.9 must
  ## come back as January, not as month 13.
  a12[, phase_cal := (month(ORIGIN) - 1L + phase12) %% 12]
  a12[, month12 := (round(phase_cal) %% 12L) + 1L]
  a12[, qtr12 := ceiling(month12 / 3)]

  out <- Reduce(function(x, y) merge(x, y, by = "Bp_ID", all.x = TRUE),
                list(base, star, a12))
  out[, typed := as.integer(!is.na(P_star))]
  ## the gap-based classification, which survives PHASE DRIFT: an advisor who
  ## books "about a year later" each time walks the meeting month slowly around
  ## the calendar, so the gaps stay at 12 while the phase concentration decays.
  ## The two measures disagreeing is itself a finding, so both are kept.
  out[, gap_class := fifelse(is.na(gap_med), NA_character_,
                      fifelse(gap_med <= 4.5, "quarterly",
                       fifelse(gap_med <= 8.5, "semi-annual",
                        fifelse(gap_med <= 15.5, "annual",
                         fifelse(gap_med <= 21.5, "18-monthly",
                          fifelse(gap_med <= 30.5, "biennial", "3-yearly"))))))]
  ## metronomic = the gaps barely vary
  out[, metronomic := as.integer(!is.na(gap_iqr) & gap_iqr <= 2)]
  out[, cycle := fifelse(is.na(P_star), "none",
                  fifelse(P_star <= 4L, "quarterly",
                   fifelse(P_star == 6L, "semi-annual",
                    fifelse(P_star == 12L, "annual",
                     fifelse(P_star == 18L, "18-monthly",
                      fifelse(P_star == 24L, "biennial", "3-yearly"))))))]
  ## how many of the cycles that COULD have happened actually did
  out[!is.na(P_star), coverage := n_meet / (floor(span / P_star) + 1)]
  out[]
}

## ---------------------------------------------------------------------------
## data
## ---------------------------------------------------------------------------

cd <- load_dt("contacts_d")

## the client's package: time-varying in the panel, so take the modal one over
## the months the client is actually observed
pan <- setDT(arrow::read_parquet(file.path(CACHE, "panel.parquet"),
        col_select = c("Bp_ID", "MDate", "Anlagepaket", "Depotprodukt","MA_Kundensegment","EVV","main_bank",
                       "observed", "discretionary")))
pan <- pan[main_bank=="Ja"]
pkg <- pan[observed == 1L & !is.na(Anlagepaket),
           .N, by = .(Bp_ID, Anlagepaket)][order(Bp_ID, -N)][
           , .(pkg = Anlagepaket[1]), by = Bp_ID]
obs <- pan[observed == 1L, .(obs_months = .N,
                             obs_first  = min(MDate),
                             obs_last   = max(MDate)), by = Bp_ID]
rm(pan); invisible(gc())

## ---------------------------------------------------------------------------
## main run: advisor-initiated review meetings
## ---------------------------------------------------------------------------

cat("\n================ advisor-initiated review meetings (perf_a) ================\n\n")

M <- cd[perf_a == 1L, .(Bp_ID, MDate = eom(ContactDate))]
cat("perf_a contact-days:", format(cd[perf_a == 1L, .N], big.mark = "'"),
    "-> distinct client-months:", format(nrow(unique(M)), big.mark = "'"),
    "on", uniqueN(M$Bp_ID), "clients\n")

CS <- cycle_stats(M)
CS <- merge(CS, pkg, by = "Bp_ID", all.x = TRUE)
CS <- merge(CS, obs, by = "Bp_ID", all.x = TRUE)
CS[is.na(pkg), pkg := "unknown"]

cat("\nclients with at least", CY$min_meet, "meeting months and a long enough span:",
    CS[!is.na(P_star) | n_meet >= CY$min_meet, .N], "\n")
cat("of those, typed with a cycle:", CS[typed == 1L, .N],
    sprintf("(%.1f%%)\n", 100 * CS[n_meet >= CY$min_meet, mean(typed)]))

cat("\n---- how many meetings do clients have at all ----\n")
print(CS[, .(clients = .N), by = .(n_meet = pmin(n_meet, 10L))][order(n_meet)])

cat("\n---- dominant period, clients with >= 3 meeting months ----\n")
tab <- CS[n_meet >= CY$min_meet, .(clients = .N), by = cycle][order(-clients)]
tab[, share := round(clients / sum(clients), 3)]
print(tab)

cat("\n---- how much the phase-test level matters ----\n")
al0 <- CY$alpha
print(rbindlist(lapply(c(0.10, 0.05, 0.01), function(a) {
  CY$alpha <<- a
  s <- cycle_stats(M)[n_meet >= CY$min_meet]
  data.table(alpha = a, clients = nrow(s), sh_typed = round(mean(s$typed), 3),
             sh_annual = round(mean(s$cycle == "annual"), 3))
})))
CY$alpha <- al0
cat("Phase locking is rare at any conventional level, and the level only\n",
    "moves how rare. The frequency result below does not depend on it.\n", sep = "")

cat("\n---- median gap between consecutive meetings (months) ----\n")
print(CS[n_meet >= CY$min_meet, .(clients = .N,
        p10 = quantile(gap_med, .10, na.rm = TRUE),
        p25 = quantile(gap_med, .25, na.rm = TRUE),
        med = median(gap_med, na.rm = TRUE),
        p75 = quantile(gap_med, .75, na.rm = TRUE),
        p90 = quantile(gap_med, .90, na.rm = TRUE))])
cat("\ndistribution of the median gap, rounded to whole months:\n")
print(CS[n_meet >= CY$min_meet & !is.na(gap_med),
         .(clients = .N), by = .(gap_med = round(gap_med))][order(gap_med)][1:20])

ggplot(CS[n_meet >= CY$min_meet & !is.na(gap_med),
          .(clients = .N), by = .(gap_med = round(gap_med))][order(gap_med)][1:20],aes(x=gap_med,y=clients))+geom_col()
## ---------------------------------------------------------------------------
## the two dimensions the eyeball noticed
## ---------------------------------------------------------------------------

cat("\n---- detection power: a cycle cannot be seen in three points ----\n")
print(CS[n_meet >= CY$min_meet, .(clients = .N,
        sh_typed   = round(mean(typed), 3),
        sh_metron  = round(mean(metronomic, na.rm = TRUE), 3),
        mean_R12   = round(mean(R12, na.rm = TRUE), 3)),
        by = .(n_meet_grp = cut(n_meet, c(2, 3, 4, 6, 9, 14, Inf),
                                labels = c("3", "4", "5-6", "7-9", "10-14", "15+")))][
        order(n_meet_grp)])
cat("\nThe typed share rises steeply with the number of meetings. That is\n",
    "detection power, not behaviour: with three meetings no test can separate\n",
    "a cycle from chance. Compare packages at a fixed meeting count.\n", sep = "")

cat("\n\n================ dimension 1: frequency differs across clients ================\n\n")
cat("---- phase-based period ----\n")
print(dcast(CS[n_meet >= CY$min_meet], pkg ~ cycle, value.var = "Bp_ID",
            fun.aggregate = length))

cat("\n---- gap-based period (median gap between consecutive meetings) ----\n")
print(dcast(CS[n_meet >= CY$min_meet & !is.na(gap_class)], pkg ~ gap_class,
            value.var = "Bp_ID", fun.aggregate = length))
cat("\nsame, as row shares:\n")
gsh <- dcast(CS[n_meet >= CY$min_meet & !is.na(gap_class)], pkg ~ gap_class,
             value.var = "Bp_ID", fun.aggregate = length)
gnum <- setdiff(names(gsh), "pkg")
gsh[, (gnum) := lapply(.SD, function(x) round(x / rowSums(as.matrix(.SD)), 3)), .SDcols = gnum]
print(gsh)

cat("\n---- do the two classifications agree? ----\n")
print(dcast(CS[n_meet >= 5L & !is.na(gap_class)], gap_class ~ cycle,
            value.var = "Bp_ID", fun.aggregate = length))

cat("\nsame, as row shares:\n")
sh <- dcast(CS[n_meet >= CY$min_meet], pkg ~ cycle, value.var = "Bp_ID",
            fun.aggregate = length)
num <- setdiff(names(sh), "pkg")
sh[, (num) := lapply(.SD, function(x) round(x / rowSums(as.matrix(.SD)), 3)), .SDcols = num]
print(sh)

cat("\n\n================ dimension 2: phase differs across clients ================\n\n")
cat("annual phase concentration R12 (1 = always the same month of the year):\n")
print(CS[n_meet >= CY$min_meet, .(clients = .N,
        mean_R12 = round(mean(R12, na.rm = TRUE), 3),
        med_R12  = round(median(R12, na.rm = TRUE), 3),
        sh_R12_gt_0.8 = round(mean(R12 > 0.8, na.rm = TRUE), 3),
        sh_rayleigh_sig = round(mean(p12 < 0.05, na.rm = TRUE), 3)), by = pkg][
        order(-clients)])

cat("\nIF every client sat in the same quarter, this next table would have one\n",
    "big column. It does not:\n", sep = "")
print(dcast(CS[n_meet >= CY$min_meet & R12 > 0.8], pkg ~ qtr12,
            value.var = "Bp_ID", fun.aggregate = length))

cat("\npooled distribution of the dominant quarter, clients with R12 > 0.8:\n")
print(CS[n_meet >= CY$min_meet & R12 > 0.8, .(clients = .N),
         by = .(quarter = qtr12)][order(quarter)])
cat("\n... and of the dominant MONTH:\n")
print(CS[n_meet >= CY$min_meet & R12 > 0.8, .(clients = .N),
         by = .(month = month12)][order(month)])

## ---------------------------------------------------------------------------
## worked examples, so the classification can be eyeballed the way you did
## ---------------------------------------------------------------------------

cat("\n\n================ example clients, by detected cycle ================\n")
Mu <- unique(M)[, .(Bp_ID, MDate)]
setorder(Mu, Bp_ID, MDate)
set.seed(1)
for (cy in c("annual", "semi-annual", "biennial", "quarterly", "none")) {
  ids <- CS[cycle == cy & n_meet >= 5L, Bp_ID]
  if (!length(ids)) next
  ids <- sample(ids, min(3L, length(ids)))
  cat("\n--", cy, "--\n")
  for (id in ids) {
    r <- CS[Bp_ID == id]
    cat(sprintf("  Bp_ID %s  n=%d  gap %.0f (IQR %.0f)  R*=%.2f  R12=%.2f  %s\n    %s\n",
                format(id), r$n_meet, r$gap_med, r$gap_iqr, r$R_star, r$R12, r$pkg,
                paste(format(Mu[Bp_ID == id, MDate], "%Y-%m"), collapse = " ")))
  }
}

## ---------------------------------------------------------------------------
## sensitivity: the meeting definition
## ---------------------------------------------------------------------------

cat("\n\n================ sensitivity to the meeting definition ================\n\n")
FLAGS <- c(perf_a = "advisor-initiated review",
           perf_a_p = "... and personal (no mail)",
           perf_inv_a_p = "... and K_Anlegen too",
           perf = "any review contact")
sens <- rbindlist(lapply(names(FLAGS), function(f) {
  s <- cycle_stats(cd[get(f) == 1L, .(Bp_ID, MDate = eom(ContactDate))])
  s <- s[n_meet >= CY$min_meet]
  data.table(flag = FLAGS[f], clients = nrow(s),
             sh_typed = round(mean(s$typed), 3),
             sh_annual = round(mean(s$cycle == "annual"), 3),
             sh_semi = round(mean(s$cycle == "semi-annual"), 3),
             sh_biennial = round(mean(s$cycle == "biennial"), 3),
             med_gap = round(median(s$gap_med, na.rm = TRUE), 1),
             mean_R12 = round(mean(s$R12, na.rm = TRUE), 3))
}))
print(sens)

## ---------------------------------------------------------------------------
## figures
## ---------------------------------------------------------------------------

PK <- CS[n_meet >= CY$min_meet & pkg %in% CS[n_meet >= CY$min_meet,
         .N, by = pkg][N >= 100, pkg]]
PK[, cycle := factor(cycle, levels = c("quarterly", "semi-annual", "annual",
                                       "18-monthly", "biennial", "3-yearly", "none"))]

p1 <- ggplot(PK[, .(n = .N), by = .(pkg, cycle)][, sh := n / sum(n), by = pkg],
             aes(x = pkg, y = sh, fill = cycle)) +
  geom_col() + scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = NULL, fill = NULL,
       title = "Dominant review-meeting cycle, by investment package",
       subtitle = "clients with at least 3 meeting months; period = longest with high phase concentration") +
  theme_light() + theme(legend.position = "bottom",
                        axis.text.x = element_text(angle = 30, hjust = 1))
ggsave(file.path(CYC_DIR, "fig_cycle_by_package.pdf"), p1, width = 9, height = 6)

p2 <- ggplot(PK[!is.na(gap_med) & gap_med <= 42], aes(x = gap_med)) +
  geom_histogram(binwidth = 1, fill = "grey30") +
  facet_wrap(~ pkg, scales = "free_y") +
  labs(x = "median gap between review meetings (months)", y = NULL,
       title = "Meeting frequency is client-specific, and package-specific") +
  theme_light()
ggsave(file.path(CYC_DIR, "fig_gap_hist_by_package.pdf"), p2, width = 10, height = 7)

p3 <- ggplot(PK[R12 > 0.8], aes(x = factor(month12, levels = 1:12))) +
  geom_bar(fill = "grey30") +
  facet_wrap(~ pkg, scales = "free_y") +
  labs(x = "dominant calendar month of the annual cycle", y = NULL,
       title = "Clients with a sharp annual phase sit in different months",
       subtitle = "clients with annual phase concentration R12 > 0.8") +
  theme_light()
ggsave(file.path(CYC_DIR, "fig_phase_month_by_package.pdf"), p3, width = 10, height = 7)

p4 <- ggplot(PK, aes(x = R12, fill = pkg)) +
  geom_density(alpha = 0.35, colour = NA) +
  labs(x = "annual phase concentration R12", y = NULL, fill = NULL,
       title = "How metronomic is each package?") +
  theme_light() + theme(legend.position = "bottom")
ggsave(file.path(CYC_DIR, "fig_R12_density.pdf"), p4, width = 9, height = 6)

## ---------------------------------------------------------------------------
## table for the paper
## ---------------------------------------------------------------------------

tb <- PK[, .(clients = .N,
             sh_typed = mean(typed),
             sh_annual = mean(gap_class == "annual", na.rm = TRUE),
             sh_semi = mean(gap_class == "semi-annual", na.rm = TRUE),
             sh_bien = mean(gap_class == "biennial", na.rm = TRUE),
             med_gap = median(gap_med, na.rm = TRUE),
             mean_R12 = mean(R12, na.rm = TRUE)), by = pkg][order(-clients)]
rows <- sprintf("%s & %s & %s & %s & %s & %s & %s & %s \\\\",
  gsub("_", "\\\\_", tb$pkg), formatC(tb$clients, format = "d", big.mark = ","),
  sprintf("%.3f", tb$sh_typed), sprintf("%.3f", tb$sh_annual),
  sprintf("%.3f", tb$sh_semi), sprintf("%.3f", tb$sh_bien),
  sprintf("%.1f", tb$med_gap), sprintf("%.3f", tb$mean_R12))
cat("\\begin{table}[htbp]\n\\centering\n",
    "\\caption{Cyclicality of advisor-initiated review meetings, by investment package}\n",
    "\\label{tab:perf_cycle}\n\\begin{tabular}{lccccccc}\n\\hline\\hline\n",
    "Package & Clients & Cyclical & Annual & Semi-ann. & Biennial & Median gap & $R_{12}$ \\\\\n\\hline\n",
    paste(rows, collapse = "\n"), "\n\\hline\\hline\n\\end{tabular}\n",
    "\\begin{minipage}{\\linewidth}\\footnotesize\n",
    sprintf(paste0("Notes: clients with at least %d months containing an advisor-initiated ",
                   "review contact. Cyclical means the Rayleigh test rejects evenly spread ",
                   "phases at the %.2f level for some period with at least %g full cycles ",
                   "observed; the reported period is the longest such, so harmonics of the ",
                   "true period do not win. The annual, semi-annual and biennial columns are ",
                   "the GAP-based classification (median months between consecutive ",
                   "meetings), which is unaffected by phase drift. $R_{12}$ is the annual ",
                   "phase concentration: 1 means every meeting falls in the same month of ",
                   "the year, 0 means the months are spread evenly.\n"),
            CY$min_meet, CY$alpha, CY$min_cycles),
    "\\end{minipage}\n\\end{table}\n",
    sep = "", file = file.path(CYC_DIR, "tab_perf_cycle.tex"))

save_dt(CS, "perf_cycle")
cat("\n\nwrote", CYC_DIR, "\n")
print(list.files(CYC_DIR))
