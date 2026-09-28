## =============================================================================
## 10_timing.R -- the collapsed pre/post design around the FIRST advisor call
##
## PROTOTYPE. Not yet in run_all.R. This is the cheap, one-row-per-client-episode
## version of a staggered-timing design, built to answer one question before the
## full daily event study is worth writing:
##
##   does an advisor's investment call reduce the client's OWN selling in the
##   days that follow, relative to a client who has not been called yet?
##
## DESIGN
##   unit       : client x episode (directly comparable to 06's cross section)
##   treatment  : the FIRST ADVISOR-INITIATED K_Anlegen contact (inv_a) inside
##                the exact daily window [peak_date, trough_date]
##   outcome    : CHF the CLIENT entered themselves through e-banking, in the H
##                days AFTER minus the H days BEFORE that date, over pre-episode
##                wealth. The advisor and mandate channels are reported too, but
##                only as validity checks -- see below.
##   comparison : clients never called in the window, given a PSEUDO contact date
##                drawn from the treated distribution of the SAME episode, so
##                both groups' windows sit in comparable calendar territory.
##   estimator  : first difference. d_y = a + b*treated + X'g + episode FE.
##                b is the DiD: (post - pre)_treated - (post - pre)_control.
##
## FIVE THINGS THIS DESIGN GETS RIGHT, AND WHY EACH MATTERS
##
##  1. OUTCOME IS THE CLIENT'S OWN CHANNEL. A contact mechanically produces
##     ADVISOR-channel trades (06 puts that at +5.1pp). "Any trade after the
##     call" would therefore measure "the advisor entered an order after
##     speaking to the client", which is true by construction. Only chan=="self"
##     answers the behavioural question. The advisor channel is kept as a
##     POSITIVE CONTROL: if the date alignment is right it must jump after t0.
##
##  2. ADVISOR-INITIATED ONLY. K_Anlegen pools both initiators, and a
##     client-initiated call is this project's panic proxy (treat_cli is
##     +0.049 on client-entered selling). Including it would partly measure
##     "client panicked -> phoned -> sold", i.e. reverse causation wearing the
##     sign of a treatment effect.
##
##  3. DAY 0 IS DROPPED. boerse carries a real execution timestamp, but the
##     contact log is DATE ONLY (Kontakt_DT has 5'797 distinct values, i.e.
##     dates). So a same-day trade cannot be ordered against the call. One day
##     out of a 33-272 day window is cheap insurance.
##
##  4. CONTROLS GET A CALENDAR-MATCHED PSEUDO DATE. Comparing a treated client's
##     post-call window against a control's whole-episode average would confound
##     the treatment with where in the crash the window falls. Drawing the
##     pseudo date from the treated distribution WITHIN episode fixes that.
##
##  5. THE PRE-WINDOW IS THE TEST, NOT A NUISANCE. If advisors call the clients
##     who are already bolting, that shows up as treated clients selling MORE
##     than controls BEFORE t0. That comparison is printed first and it is the
##     single most informative number in this script.
##
## WHAT IT STILL DOES NOT FIX. The advisor chooses WHEN to call. If they call
## when they sense a client is about to bolt, selling falls afterwards through
## mean reversion alone. The pre-window comparison makes that testable; it does
## not make it go away.
##
## Output: results/10_timing.txt, results/tables/10_timing_*.tex
## =============================================================================

source("00_setup.R")
log_init("10_timing")

H_LIST <- c(20L, 40L)      # calendar days each side of the first contact
set.seed(P$seed)

cd  <- load_dt("contacts_d")
td  <- load_dt("trades_d")
cle <- load_dt("cle")
ep  <- load_dt("episodes")[usable == TRUE]

sink(file.path(RESULTS, "10_timing.txt"), split = TRUE)
on.exit(sink(), add = TRUE)

cat("=============================================================\n")
cat(" COLLAPSED PRE/POST AROUND THE FIRST ADVISOR CALL\n")
cat(" treatment: first advisor-initiated K_Anlegen inside [peak, trough]\n")
cat(" outcome:   client-entered (e-banking) sales, post minus pre\n")
cat("=============================================================\n\n")

## ---------------------------------------------------------------------------
## 1. treatment date
## ---------------------------------------------------------------------------
C <- cle[smp_main == 1L, .(Bp_ID, ep_id, peak_date, trough_date, post_end,
                           wealth_pre, log_w_pre, n_assets_pre, contacts_pre,
                           ret_past12_pre, main_bank_pre, advisor_id,
                           treat_cli, smp_mb, smp_boerse, in_boerse)]

hit <- cd[C[, .(Bp_ID, ep_id, peak_date, trough_date)],
          on = .(Bp_ID, ContactDate >= peak_date, ContactDate <= trough_date),
          .(Bp_ID, ep_id = i.ep_id, d = x.ContactDate, inv_a, inv, advice_a),
          allow.cartesian = TRUE][!is.na(d)]

first <- hit[inv_a == 1L, .(t0 = min(d)), by = .(Bp_ID, ep_id)]
## how many calls follow the first one -- an OUTCOME / dose, never a control:
## it is post-treatment, so conditioning on it would absorb part of the effect
n_after <- hit[inv_a == 1L][first, on = .(Bp_ID, ep_id)][
  d > t0, .(n_calls_after = .N), by = .(Bp_ID, ep_id)]

C <- first[C, on = .(Bp_ID, ep_id)]
C <- n_after[C, on = .(Bp_ID, ep_id)]
C[is.na(n_calls_after), n_calls_after := 0L]
C[, treated := as.integer(!is.na(t0))]

cat("treated (first advisor-initiated K_Anlegen in the window):\n")
C[, dfp := as.numeric(t0 - peak_date)]
print(C[, .(N = .N, treated = sum(treated), rate = round(mean(treated), 4),
            window_days = as.numeric(trough_date[1] - peak_date[1]),
            med_day = as.numeric(median(dfp, na.rm = TRUE)),
            med_calls_after = as.numeric(median(n_calls_after[treated == 1L]))),
        by = ep_id][order(ep_id)])

## ---------------------------------------------------------------------------
## 2. pseudo dates for the never-called, drawn WITHIN episode from the treated
##    distribution, so the two groups' windows sit in the same calendar space
## ---------------------------------------------------------------------------
for (e in unique(C$ep_id)) {
  pool <- C[ep_id == e & treated == 1L, dfp]
  idx  <- C[, which(ep_id == e & treated == 0L)]
  if (length(pool) && length(idx))
    set(C, idx, "dfp", sample(pool, length(idx), replace = TRUE))
}
C[treated == 0L, t0 := peak_date + dfp]
cat("\npseudo-date check -- days from peak, treated vs control:\n")
print(C[, .(n = .N, p25 = quantile(dfp, .25), med = median(dfp),
            p75 = quantile(dfp, .75)), by = .(ep_id, treated)][order(ep_id, treated)])

## ---------------------------------------------------------------------------
## 3. traded CHF in the H days each side of t0, by channel. Day 0 excluded.
## ---------------------------------------------------------------------------
sells <- td[sell == 1L & decision == 1L, .(Bp_ID, DDate, chf, chan)]

window_sum <- function(keys, from, to, tag) {
  k <- copy(keys)[, `:=`(w_from = from, w_to = to)]
  j <- sells[k, on = .(Bp_ID, DDate >= w_from, DDate <= w_to),
             .(Bp_ID, ep_id = i.ep_id, chf, chan), allow.cartesian = TRUE][!is.na(chf)]
  out <- dcast(j[, .(chf = sum(chf)), by = .(Bp_ID, ep_id, chan)],
               Bp_ID + ep_id ~ chan, value.var = "chf", fill = 0)
  setnames(out, setdiff(names(out), c("Bp_ID","ep_id")),
           paste0(tag, "_", setdiff(names(out), c("Bp_ID","ep_id"))))
  out
}

keys <- C[, .(Bp_ID, ep_id, t0)]
for (H in H_LIST) {
  pre  <- window_sum(keys, keys$t0 - H, keys$t0 - 1L, sprintf("pre%d",  H))
  post <- window_sum(keys, keys$t0 + 1L, keys$t0 + H, sprintf("post%d", H))
  C <- pre[C,  on = .(Bp_ID, ep_id)]
  C <- post[C, on = .(Bp_ID, ep_id)]
}
CH <- c("self", "advisor", "mandate", "other")
for (H in H_LIST) for (ch in CH) {
  for (side in c("pre","post")) {
    v <- sprintf("%s%d_%s", side, H, ch)
    if (!v %in% names(C)) C[, (v) := 0]
    C[is.na(get(v)), (v) := 0]
  }
  ## scaled by pre-episode wealth, winsorised at the top (a hard floor at 0)
  C[, (sprintf("d_%s_%d", ch, H)) :=
      winsor((get(sprintf("post%d_%s", H, ch)) - get(sprintf("pre%d_%s", H, ch))) /
               wealth_pre, c(0.01, 0.99))]
  C[, (sprintf("dp_%s_%d", ch, H)) :=
      as.integer(get(sprintf("post%d_%s", H, ch)) > 0) -
      as.integer(get(sprintf("pre%d_%s", H, ch)) > 0)]
}

## ---------------------------------------------------------------------------
## 4. THE PRE-WINDOW TEST -- run and read this before anything else
## ---------------------------------------------------------------------------
cat("\n-------------------------------------------------------------\n")
cat("STEP 1 -- WERE THE TREATED ALREADY SELLING BEFORE THE CALL?\n")
cat("-------------------------------------------------------------\n")
cat("If advisors call the clients who are already bolting, treated clients sell\n")
cat("MORE than controls in the PRE window. That would invalidate the design.\n\n")
for (H in H_LIST) {
  cat("-- H =", H, "calendar days --\n")
  print(C[, .(N = .N,
              pre_self_chf  = round(mean(get(sprintf("pre%d_self",  H)))),
              pre_self_r    = round(mean(get(sprintf("pre%d_self",  H)) / wealth_pre), 5),
              p_pre_self    = round(mean(get(sprintf("pre%d_self",  H)) > 0), 4),
              p_pre_advisor = round(mean(get(sprintf("pre%d_advisor",H)) > 0), 4)),
          by = treated][order(treated)])
}

## ---------------------------------------------------------------------------
## 5. raw pre/post means, and the positive control
## ---------------------------------------------------------------------------
cat("\n-------------------------------------------------------------\n")
cat("STEP 2 -- RAW PRE/POST, BY CHANNEL\n")
cat("-------------------------------------------------------------\n")
cat("The ADVISOR row is a positive control: a call must be followed by advisor-\n")
cat("entered orders. If it is not, the date alignment is wrong.\n")
for (H in H_LIST) {
  cat("\n-- H =", H, "calendar days: P(any sale in the window) --\n")
  print(C[, .(N = .N,
              self_pre  = round(mean(get(sprintf("pre%d_self",   H)) > 0), 4),
              self_post = round(mean(get(sprintf("post%d_self",  H)) > 0), 4),
              adv_pre   = round(mean(get(sprintf("pre%d_advisor",H)) > 0), 4),
              adv_post  = round(mean(get(sprintf("post%d_advisor",H)) > 0), 4)),
          by = treated][order(treated)])
}

## ---------------------------------------------------------------------------
## 6. the first difference: b = (post - pre)_treated - (post - pre)_control
## ---------------------------------------------------------------------------
cat("\n-------------------------------------------------------------\n")
cat("STEP 3 -- FIRST DIFFERENCE (episode FE, clustered by advisor)\n")
cat("-------------------------------------------------------------\n")

fd <- function(y, dat = C) feols(
  as.formula(sprintf(
    "%s ~ treated + treat_cli + log_w_pre + n_assets_pre + contacts_pre +
     ret_past12_pre + main_bank_pre | ep_id", y)),
  dat, vcov = ~ advisor_id, notes = FALSE)

for (H in H_LIST) {
  cat("\n== H =", H, "calendar days ==\n")
  m <- list(
    `d CHF self / W`    = fd(sprintf("d_self_%d", H)),
    `d P(sell) self`    = fd(sprintf("dp_self_%d", H)),
    `d CHF advisor / W` = fd(sprintf("d_advisor_%d", H)),
    `d P(sell) advisor` = fd(sprintf("dp_advisor_%d", H)))
  print(etable(m, digits = 4, fitstat = ~ n + r2, keep = "treated|treat_cli"))
  if (H == H_LIST[1])
    etable(m, file = file.path(TAB_DIR, "10_timing_firstdiff.tex"), replace = TRUE,
           digits = 4, fitstat = ~ n + r2, keep = "treated|treat_cli",
           title = sprintf(
             "First difference around the first advisor call, +/- %d days", H))
}

## restricted to the samples where the outcome is best measured
cat("\n-- main-bank clients covered by the trade file --\n")
CB <- C[smp_mb == 1L & in_boerse == 1L]
cat("n =", nrow(CB), " treated =", CB[, sum(treated)], "\n")
print(etable(list(
  `d CHF self / W` = fd(sprintf("d_self_%d", H_LIST[1]), CB),
  `d P(sell) self` = fd(sprintf("dp_self_%d", H_LIST[1]), CB)),
  digits = 4, fitstat = ~ n, keep = "treated|treat_cli"))

save_dt(C, "timing_prepost")
log_step("collapsed pre/post written", C)
sink()
