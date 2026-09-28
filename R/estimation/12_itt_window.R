## =============================================================================
## 12_itt_window.R -- habitual review-meeting window, and the instrument Z
##
## The existing treatment (treat_perf_inv_a_p) is 1 when a qualifying review
## meeting ACTUALLY happened inside the drawdown. That is endogenous: an advisor
## calls the clients who are worth calling, and a client who phones in a panic
## gets a meeting. The alternative used so far -- restricting to smp_cycle,
## clients with a review contact in the 12 pre-months -- is endogenous in the
## same way and throws away 66-95% of the sample.
##
## This script builds an INSTRUMENT instead. Swiss retail review meetings run on
## an annual cycle: a client who is seen every March is seen every March because
## that is when their advisor books them, not because of anything happening in
## March. So:
##
##   Z[i,c] = 1 if client i's HABITUAL 3-month review window overlaps the crisis
##            window of episode c by at least one calendar month.
##
## The habitual window is estimated LEAVE-ONE-EPISODE-OUT, from qualifying
## meetings dated strictly before dd_start[c] - 12 months. Nothing from the
## episode's own window, and nothing after it, may enter. That is asserted in
## code below, not just intended.
##
## Qualifying meeting = advisor-initiated AND personal (in-person or phone) AND
## K_Performancebesprechung AND K_Anlegen. That combination is already built as
## perf_inv_a_p in 02b_contacts.R.
##
## Output: results/cache/itt_window.parquet  (client x episode)
##         results/itt/tab_composition.tex
##         results/12_itt_window.txt
## =============================================================================

source("00_setup.R")
source("itt_funs.R")   # arc_mat, months_between, build_window, add_Z
log_init("12_itt_window")

ITT_DIR <- file.path(RESULTS, "itt")
dir.create(ITT_DIR, showWarnings = FALSE, recursive = TRUE)

sink(file.path(RESULTS, "12_itt_window.txt"), split = TRUE)
on.exit(sink(), add = TRUE)

## ---------------------------------------------------------------------------
## parameters
## ---------------------------------------------------------------------------

I <- list(
  ## arc width in months. 3 is the pre-specified baseline; 2 and 4 are the
  ## robustness grid in 14.
  arc_w      = 3L,
  ## a meeting may enter the window only if it is dated strictly before
  ## dd_start - leave_out months. 12 keeps a full year between the last
  ## usable meeting and the crisis, so nothing anticipating the crisis
  ## can shape the window.
  leave_out  = 12L,
  ## baseline sample restrictions on the ESTIMATED window
  min_years  = 3L,
  min_reg    = 0.6,
  ## episodes excluded from the design, with the reason.
  ##   ep1_201305  -- contacts start 2011-03, so the leave-out cutoff is
  ##                  2012-05 and at most two calendar years of history exist.
  ##                  n_years_pre >= 3 is arithmetically impossible: 0 rows.
  ##   ep10_202201 -- the crisis window spans 6 distinct calendar months, so
  ##                  ALL 12 arcs overlap it. Z = 1 for every client and the
  ##                  episode carries no identifying variation.
  ep_drop    = c("ep1_201305", "ep10_202201"),
  ## sample flag. smp_mb = non-discretionary AND the bank is the client's main
  ## bank, so the wealth denominators are the client's actual portfolio.
  smp        = "smp_mb"
)

str(I)
cat("\n")

## ---------------------------------------------------------------------------
## filter log -- the guardrail is "never drop rows silently"
## ---------------------------------------------------------------------------

FLT <- data.table(step = character(), rows = integer(), clients = integer(),
                  dropped = integer())
flt <- function(dt, step) {
  prev <- if (nrow(FLT)) FLT$rows[nrow(FLT)] else NA_integer_
  FLT <<- rbind(FLT, data.table(step = step, rows = nrow(dt),
                                clients = uniqueN(dt$Bp_ID),
                                dropped = if (is.na(prev)) 0L else prev - nrow(dt)))
  invisible(dt)
}

## ---------------------------------------------------------------------------
## 1. inputs
## ---------------------------------------------------------------------------

cd  <- load_dt("contacts_d")
ep  <- load_dt("episodes")[usable == TRUE]
cle <- load_dt("cle")

ep <- ep[!ep_id %in% I$ep_drop]
cat("episodes kept:", nrow(ep), "--", paste(ep$ep_id, collapse = ", "), "\n")
cat("episodes dropped:", paste(I$ep_drop, collapse = ", "), "\n\n")

## the qualifying meeting
Q <- cd[perf_inv_a_p == 1L, .(Bp_ID, ContactDate,
                              mo = month(ContactDate), yr = year(ContactDate))]
cat("qualifying meetings (perf & inv & advisor-initiated & personal):",
    format(nrow(Q), big.mark = "'"), "on", uniqueN(Q$Bp_ID), "clients\n")
cat("date range:", format(min(Q$ContactDate)), "..", format(max(Q$ContactDate)), "\n\n")

W <- build_window(Q, ep, I$arc_w, I$leave_out)
cat("windows estimated:", format(nrow(W), big.mark = "'"), "client x episode rows,",
    uniqueN(W$Bp_ID), "clients\n")

## ---- GUARDRAIL, restated on the finished table ----------------------------
chk <- merge(W[, .(Bp_ID, ep_id, last_pre, cutoff)],
             ep[, .(ep_id, dd_start, dd_end)], by = "ep_id")
stopifnot(
  "post-treatment leakage: last usable meeting is not before the cutoff" =
    all(chk$last_pre < chk$cutoff),
  "post-treatment leakage: last usable meeting is not before dd_start" =
    all(chk$last_pre < chk$dd_start),
  "cutoff is not 12 months before dd_start" =
    all(chk$cutoff == chk$dd_start %m-% months(I$leave_out))
)
cat("leave-one-out assertions passed: no meeting at or after dd_start -",
    I$leave_out, "months entered any window.\n\n")

## the baseline instrument, plus the two alternative overlap windows used in 14
W <- add_Z(W, ep, "dd_start", "dd_end",   "Z",      I$arc_w)        # crisis: peak month -> trough month
W <- add_Z(W, ep, "dd_start", "post_end", "Z_post", I$arc_w)   # through the recovery leg
ep6 <- copy(ep)[, dd_p6 := madd(dd_start, 6L)]
W <- add_Z(W, ep6, "dd_start", "dd_p6",   "Z_tau6", I$arc_w)   # tau in [0, +6]

## ---------------------------------------------------------------------------
## 4. merge onto the client x episode table
## ---------------------------------------------------------------------------

X <- cle[ep_id %in% ep$ep_id]
flt(X, "cle, episodes kept")
X <- X[get(I$smp) == 1L]
flt(X, paste0(I$smp, " == 1"))

X <- merge(X, W, by = c("Bp_ID", "ep_id"), all.x = TRUE)
X[, estimable := as.integer(!is.na(arc_start))]
X[, base := as.integer(estimable == 1L &
                       n_years_pre >= I$min_years &
                       regularity  >= I$min_reg)]

## D is the realized treatment. Since the NA -> 0 fix in 04_treatment.R this is
## 0 for a client x episode with no contact in the drawdown, not NA.
X[, D := treat_perf_inv_a_p]
stopifnot("D still has NAs -- is the 04_treatment.R fill fix in place?" = !anyNA(X$D))

## D is measured on the exact DAILY window [peak_date, trough_date] (04_treatment
## line 70), which for Covid is 23 trading days. Z is a month-level object, so a
## habitual review can fall in the crisis MONTH and still miss the daily window.
## D_m re-measures the same qualifying meeting on the month window
## [first day of the dd_start month, dd_end] and is carried as a diagnostic and
## as a robustness row -- it is the upper bound on how strong the first stage
## can be given a month-level instrument.
qm <- Q[X[, .(Bp_ID, ep_id, mw_start = floor_date(dd_start, "month"), dd_end)],
        on = .(Bp_ID, ContactDate >= mw_start, ContactDate <= dd_end),
        .(Bp_ID, ep_id, hit = x.ContactDate), allow.cartesian = TRUE][!is.na(hit)]
X[, D_m := 0L]
X[unique(qm[, .(Bp_ID, ep_id)]), on = .(Bp_ID, ep_id), D_m := 1L]
cat("\nD (daily window) rate:", round(mean(X$D), 4),
    " | D_m (month window) rate:", round(mean(X$D_m), 4), "\n")

flt(X[estimable == 1L], "has an estimable window")
flt(X[base == 1L], sprintf("baseline: n_years_pre >= %d & regularity >= %.2f",
                           I$min_years, I$min_reg))

cat("\n---- filter log ----\n")
print(FLT)

save_dt(X, "itt_window")

## ---------------------------------------------------------------------------
## 5. Step 3 -- sample composition, BEFORE any regression
## ---------------------------------------------------------------------------

cat("\n\n================ sample composition ================\n\n")

comp <- X[, .(
  clients      = .N,
  sh_estimable = mean(estimable),
  n_base       = sum(base),
  sh_base      = mean(base),
  sh_Z1        = mean(Z[base == 1L]),
  D_rate       = mean(D[base == 1L]),
  n_advisors   = uniqueN(advisor_id[base == 1L])
), by = ep_id]
comp <- merge(comp, ep[, .(ep_id, dd_start, n_crisis_mo =
  sapply(seq_len(.N), function(i) length(months_between(dd_start[i], dd_end[i]))))],
  by = "ep_id")
setorder(comp, dd_start)
print(comp[, .(ep_id, n_crisis_mo, clients, sh_estimable = round(sh_estimable, 3),
               n_base, sh_base = round(sh_base, 3), sh_Z1 = round(sh_Z1, 3),
               D_rate = round(D_rate, 3), n_advisors)])

pool <- X[base == 1L]
cat("\npooled baseline:", nrow(pool), "client x episode rows,",
    uniqueN(pool$Bp_ID), "clients,", uniqueN(pool$advisor_id), "advisors\n")
cat("P(Z=1) =", round(mean(pool$Z), 4), "\n")

## ---- switchers: what identifies the client-FE specification ---------------
sw <- pool[, .(n_ep = .N, nZ = sum(Z)), by = Bp_ID]
sw[, type := fifelse(n_ep == 1L, "single episode",
              fifelse(nZ == 0L | nZ == n_ep, "never switches", "switcher"))]
cat("\nclient-FE identification:\n")
print(sw[, .(clients = .N, rows = sum(n_ep)), by = type][order(-clients)])
n_sw   <- sw[type == "switcher", .N]
row_sw <- sw[type == "switcher", sum(n_ep)]
cat(sprintf("\nSWITCHERS: %d clients (%.1f%% of baseline clients), %d rows.\n",
            n_sw, 100 * n_sw / nrow(sw), row_sw))
if (n_sw < 100 || row_sw < 400) {
  cat("\n!!! WARNING: too few Z-switchers for a credible client-FE specification.\n",
      "!!! The ep_id-FE spec is the only one that can be read; treat the\n",
      "!!! client-FE column as descriptive.\n", sep = "")
} else {
  cat("Switchers are plentiful: the client-FE specification is viable.\n")
}

## raw first stage, before any control
cat("\nraw first stage, pooled baseline:\n")
print(pool[, .(N = .N, D_rate = round(mean(D), 4), D_m_rate = round(mean(D_m), 4)),
           by = Z][order(Z)])
cat("difference, D  (daily window):",
    round(pool[Z == 1L, mean(D)] - pool[Z == 0L, mean(D)], 4), "\n")
cat("difference, D_m (month window):",
    round(pool[Z == 1L, mean(D_m)] - pool[Z == 0L, mean(D_m)], 4), "\n")

## ---------------------------------------------------------------------------
## 6. composition table for the paper
## ---------------------------------------------------------------------------

tex_rows <- sprintf("%s & %d & %s & %s & %s & %s & %s \\\\",
  gsub("_", "\\\\_", comp$ep_id), comp$n_crisis_mo,
  formatC(comp$clients, format = "d", big.mark = ","),
  sprintf("%.3f", comp$sh_estimable),
  formatC(comp$n_base, format = "d", big.mark = ","),
  sprintf("%.3f", comp$sh_Z1),
  sprintf("%.3f", comp$D_rate))

cat(
  "\\begin{table}[htbp]\n\\centering\n",
  "\\caption{Sample composition for the habitual-window design}\n",
  "\\label{tab:itt_composition}\n",
  "\\begin{tabular}{lcccccc}\n\\hline\\hline\n",
  "Episode & Crisis & Clients & Share with & $N$ & $P(Z=1)$ & $P(D=1)$ \\\\\n",
  " & months & in sample & window & baseline & & \\\\\n\\hline\n",
  paste(tex_rows, collapse = "\n"), "\n\\hline\n",
  sprintf("Pooled & & %s & %s & %s & %s & %s \\\\\n",
          formatC(nrow(X), format = "d", big.mark = ","),
          sprintf("%.3f", mean(X$estimable)),
          formatC(nrow(pool), format = "d", big.mark = ","),
          sprintf("%.3f", mean(pool$Z)),
          sprintf("%.3f", mean(pool$D))),
  "\\hline\\hline\n\\end{tabular}\n",
  "\\begin{minipage}{\\linewidth}\\footnotesize\n",
  sprintf(paste0("Notes: sample is %s. The habitual window is the modal %d-month ",
                 "circular arc of a client's advisor-initiated personal review ",
                 "meetings, estimated from meetings dated strictly before ",
                 "$dd\\_start - %d$ months. Baseline requires at least %d distinct ",
                 "pre-episode years with a meeting and a modal-arc share of at ",
                 "least %.2f. $Z=1$ when the arc overlaps the crisis window. ",
                 "%s and %s are excluded (see text).\n"),
          gsub("_", "\\\\_", I$smp), I$arc_w, I$leave_out, I$min_years, I$min_reg,
          gsub("_", "\\\\_", I$ep_drop[1]), gsub("_", "\\\\_", I$ep_drop[2])),
  "\\end{minipage}\n\\end{table}\n",
  sep = "", file = file.path(ITT_DIR, "tab_composition.tex"))

log_step("itt window file", X)
cat("\nwrote", file.path(ITT_DIR, "tab_composition.tex"), "\n")
