## =============================================================================
## 17_es_wrel_placebo.R -- the 06_specs event study on PLACEBO windows
##
## 06_specs.R line ~276 plots the stacked event study for w_rel:
##
##   run_es("w_rel", s_beh, "main", plot_file = "06a_es_wrel.pdf", ...)
##
## with s_beh = S_mb_exdisc restricted to the eight episodes and to the three
## advised segments, and
##
##   w_rel ~ i(rel_month, TRT, ref = -1)
##         + (treat_cli + log_w_pre + n_assets_pre + contacts_pre
##            + anlagepaket_pre + segment_pre) : factor(rel_month)
##         | Bp_ID + ep_id                              cluster: advisor_id
##
## This script runs that regression unchanged on placebo episode sets: the same
## number of windows, each with the same daily length and the same pre/post
## month geometry as the real episode it stands in for, but placed at a random
## date in the sample. The output is the same picture, with the real path drawn
## against the distribution of paths a random window produces.
##
## Read it as the dynamic version of the crisis-specificity test in
## 16_placebo_xsec.R. That script asks whether one number survives random
## dating; this one asks whether the SHAPE does -- flat before rel_month 0, a
## jump at 0, a widening gap after. A placebo band that reproduces the shape
## says the path is what an advisor contact is followed by in any window, not
## what advice does in a crisis.
##
## A placebo DRAW is a whole SET of windows, not a single window: the spec
## carries Bp_ID and ep_id fixed effects, so the real estimate is identified off
## variation within client across episodes.
##
## The window geometry is preserved episode by episode, so the balanced
## rel_month range that es_window() returns is the same for every placebo draw
## as for the real episodes. That is checked, not assumed (step 3).
##
## 05 is not re-run: build_cle() and build_stk() in placebo_funs.R rebuild the
## slice of 04 + 05 this specification reads, and step 3 verifies the whole
## coefficient path against results/cache/stk.parquet before anything is drawn.
##
## ES_Y selects the outcome. build_stk() carries w_rel, w_tot_rel, cf_gap_mkt,
## d_eq_pre and d_risky_pre, so every event study in 06 can be placebo-tested by
## changing that one string; the output names follow it.
##
## Output: results/17_es_wrel_placebo.txt
##         results/cache/es_placebo_<ES_TAG>.parquet  (draw x rel_month coefs)
##         results/figures/17_es_<ES_TAG>_placebo.pdf        (band vs real path)
##         results/figures/17_es_<ES_TAG>_placebo_draws.pdf  (every draw)
##         results/tables/tab_es_<ES_TAG>_placebo.tex
## =============================================================================

source("00_setup.R")
source("placebo_funs.R")
log_init("17_es_wrel_placebo")

sink(file.path(RESULTS, "17_es_wrel_placebo.txt"), split = TRUE)
on.exit(sink(), add = TRUE)

## ---------------------------------------------------------------------------
## 0. configuration -- mirrors 06_specs STEP 1/STEP 2 exactly
## ---------------------------------------------------------------------------

## the outcome plotted at 06_specs line ~276.
ES_Y   <- "w_tot_rel"

## the label and the output file names FOLLOW ES_Y, so switching the outcome can
## neither mislabel the figure nor overwrite the previous outcome's plots with
## wrongly named ones (see the orphan-figure warning in the README).
ES_LABS <- c(
  w_rel       = "Wealth relative to pre-episode wealth",
  w_tot_rel   = "Total wealth incl. deposits, relative to pre-episode",
  cf_gap_mkt  = "Wealth gap vs. the market-drifted counterfactual",
  d_eq_pre    = "Equity share, change from the pre-episode level",
  d_risky_pre = "Risky share of total wealth, change from pre-episode")
ES_LAB <- if (ES_Y %in% names(ES_LABS)) ES_LABS[[ES_Y]] else ES_Y
ES_TAG <- sub("^w_", "", ES_Y)

TRT    <- "treat_perf_inv_a_p"
SAMPLE <- "smp_mb_exdisc"
EPS    <- c("ep1_201305", "ep2_201501", "ep3_201508", "ep6_201801",
            "ep7_201811", "ep9_202002", "ep10_202201", "ep12_202310")
SEGS   <- c("Beratungszentrum", "PK-Team", "natürliche Personen PB")

## exactly mk_fml() / es_fml() as they stand in 06_specs.R
mk_fml <- function(y, fe, trt = TRT) as.formula(sprintf(
  "%s ~ i(rel_month, %s, ref = -1) +
   (treat_cli + log_w_pre + n_assets_pre + contacts_pre + anlagepaket_pre + segment_pre ) : factor(rel_month) | %s",
  y, trt, fe))
es_fml <- function(y) mk_fml(y, "Bp_ID + ep_id")

PL <- list(
  ## each draw is a full event study on ~500 k rows and ~90 covariate
  ## interactions, roughly 8 s. 100 draws per mode is about 35 minutes and is
  ## enough for a 5-95 band; raise it if the band is what goes in the paper.
  n_draws        = 100L,
  modes          = c("any", "calm"),
  seed           = 20260907L,
  min_treated    = 100L,       # treated client x episode cells in the sample
  max_attempts   = 200L,
  match_n_months = TRUE,
  ## Each draw is expensive and the whole run is long enough to be interrupted.
  ## Completed draws are therefore written to the cache every `checkpoint_every`
  ## draws, and a re-run picks up where the last one stopped instead of starting
  ## from zero. Set resume = FALSE to discard the checkpoint and redraw.
  resume           = TRUE,
  ## PARALLELISM. fixest scales badly across threads on this fit (2 -> 9 threads
  ## is only a 2x speedup), so running several draws at once beats giving one
  ## draw more threads. Each worker holds its own copy of the pre-filtered panel
  ## (~450 MB), so `workers` is bounded by memory, not by cores; it is capped
  ## against free RAM below. 1 disables the cluster entirely.
  workers          = 3L,
  threads_per_worker = 3L,
  chunk            = 12L        # draws dispatched between checkpoints
)

cat("=============================================================\n")
cat(" PLACEBO EVENT STUDY -- ", ES_Y, "\n", sep = "")
cat("=============================================================\n")
cat("outcome   : ", ES_Y, "  (", ES_LAB, ")\n", sep = "")
cat("treatment : ", TRT, "\n", sep = "")
cat("sample    : ", SAMPLE, ", segments ", paste(SEGS, collapse = " / "), "\n", sep = "")
cat("episodes  : ", paste(EPS, collapse = ", "), "\n", sep = "")
cat("draws     : ", PL$n_draws, " per mode (",
    paste(PL$modes, collapse = ", "), ")\n", sep = "")

## ---------------------------------------------------------------------------
## 1. the balanced event window, and the REAL path off stk.parquet
## ---------------------------------------------------------------------------

ep_all <- load_dt("episodes")[usable == TRUE]
ep_raw <- load_dt("episodes_raw")

SCOLS <- c("Bp_ID", "ep_id", "rel_month", "smp_main", "advisor_id",
           "treat_cli", TRT, "log_w_pre", "n_assets_pre", "contacts_pre",
           "anlagepaket_pre", "segment_pre", ES_Y)
stk0 <- setDT(arrow::read_parquet(file.path(CACHE, "stk.parquet"),
                                  col_select = all_of(SCOLS)))
stk0 <- load_dt("cle")[, .(Bp_ID, ep_id, smp = get(SAMPLE))][
  stk0, on = .(Bp_ID, ep_id)]

## es_window() is 06's own helper: it drops P$es_exclude and then keeps only the
## rel_months every remaining episode covers
keep_rel <- es_window(stk0, ep_all)

## the coefficient path on rel_month x TRT, the only part of the model the plot
## and this whole script are about. The implementation lives in placebo_funs.R
## so the workers get it by sourcing that file rather than by serialising a
## closure out of this script.
ES_FML <- as.formula(gsub("
", " ", deparse1(es_fml(ES_Y))), env = globalenv())
es_path <- function(d, tag) pl_es_path(d, tag, ES_FML, TRT)

S_real <- stk0[smp == 1L][rel_month %in% keep_rel][ep_id %in% EPS][
  segment_pre %in% SEGS]
real <- es_path(S_real, "real")

cat("\n-------------------------------------------------------------\n")
cat("1. THE REAL PATH (results/cache/stk.parquet, exactly as in 06)\n")
cat("-------------------------------------------------------------\n")
print(real[, .(rel_month, est = round(est, 5), se = round(se, 5),
               sig = fcase(p < .01, "***", p < .05, "**", p < .1, "*",
                           is.na(p), "ref", default = ""))])
cat("\nrows: ", nrow(S_real), ", clients: ", uniqueN(S_real$Bp_ID),
    ", n used: ", real$n[1], "\n", sep = "")

rm(stk0, S_real); gc(verbose = FALSE)

## ---------------------------------------------------------------------------
## 2. inputs for the rebuild
## ---------------------------------------------------------------------------

## Every filter that decides whether a client x episode enters this regression
## is frozen at a pre-episode month, so a client who satisfies none of them in
## ANY month of the panel can enter no placebo draw. Dropping those clients up
## front halves the panel and is what lets several workers hold one each. The
## verification below is run AFTER the filter, so a filter that is too
## aggressive shows up as a broken rebuild rather than as a quiet bias.
eligible_clients <- function(pan)
  pan[observed == 1L & wealth >= P$min_wealth_pre & discretionary == 0L &
        main_bank_yn == 1L & MA_Kundensegment %in% SEGS, unique(Bp_ID)]

pl_load_inputs(light = TRUE, keep_clients = eligible_clients)

## the estimation sample, in the order 06 applies it. Kept as one function so
## the real rebuild and every placebo draw are filtered identically.
es_sample <- function(cle, pl_eps) {
  d <- cle[get(SAMPLE) == 1L][ep_id %in% pl_eps][segment_pre %in% SEGS]
  if (!nrow(d)) return(NULL)
  build_stk(d, keep_rel)
}

## ---------------------------------------------------------------------------
## 3. VERIFY the rebuild against the real stk before drawing anything.
##    Same episode dates in, same coefficient path out -- otherwise the placebo
##    band measures the rebuild rather than the window.
## ---------------------------------------------------------------------------

cat("\n-------------------------------------------------------------\n")
cat("2. VERIFICATION: build_cle() + build_stk() on the REAL episode dates\n")
cat("-------------------------------------------------------------\n")

chk  <- build_cle(ep_all[ep_id %in% EPS])
srb  <- es_sample(chk, EPS)
reb  <- es_path(srb, "rebuilt")

cmp <- merge(real[, .(rel_month, est_real = est, n_real = n)],
             reb[,  .(rel_month, est_rebuilt = est, n_rebuilt = n)],
             by = "rel_month")
cmp[, abs_diff := abs(est_rebuilt - est_real)]
print(cmp[, .(rel_month, est_real = round(est_real, 5),
              est_rebuilt = round(est_rebuilt, 5),
              abs_diff = signif(abs_diff, 3), n_real, n_rebuilt)])

TOL <- 1e-6
if (nrow(cmp) != nrow(real) || cmp[, max(abs_diff)] > TOL) {
  cat("\n!! the rebuild does not reproduce the 06 event study.\n")
  cat("   Every placebo path would inherit that difference, so the run stops\n")
  cat("   here. Compare build_cle() / build_stk() in placebo_funs.R against\n")
  cat("   04_treatment.R and 05_outcomes.R.\n")
  stop("build_stk() does not reproduce the 06 event study", call. = FALSE)
}
cat("\nrebuild reproduces the whole path to within ", TOL,
    " absolute -- placebo windows differ only in DATE.\n", sep = "")
rm(chk, srb, reb, cmp); gc(verbose = FALSE)

## ---------------------------------------------------------------------------
## 4. window geometry and candidate start dates
## ---------------------------------------------------------------------------

geo    <- pl_geometry(ep_all)
STRESS <- ep_raw[, .(s = peak_date, e = trough_date)]
PL_EPS <- sprintf("pl%02d", which(geo$src_ep %in% EPS))

## the balanced window is a function of the geometry alone -- each episode
## covers rel_month -n_pre .. n_months + n_post, and es_window keeps the
## rel_months every non-excluded episode covers. Preserving the geometry
## therefore preserves keep_rel, which is what lets one keep_rel serve the real
## path and every placebo draw. Assumption, so it is checked.
geo_keep <- {
  use <- geo[!ep_label %in% P$es_exclude]
  lo  <- use[, max(-n_pre)]
  hi  <- use[, min(n_months + n_post)]
  k   <- lo:hi
  k[k >= P$es_window[1] & k <= P$es_window[2]]
}
if (!identical(sort(as.integer(geo_keep)), sort(as.integer(keep_rel))))
  stop("the balanced window implied by the episode geometry (",
       min(geo_keep), "..", max(geo_keep), ") is not the one es_window() ",
       "returned (", min(keep_rel), "..", max(keep_rel), "); a placebo draw ",
       "would not be estimated on the same rel_months as the real episodes",
       call. = FALSE)
cat("\nbalanced window: rel_month ", min(keep_rel), " .. ", max(keep_rel),
    " (", length(keep_rel), " months), preserved by construction in every draw.\n",
    sep = "")

## ---------------------------------------------------------------------------
## 5. the placebo runs
## ---------------------------------------------------------------------------

## the checkpoint: completed draws survive an interrupted run
CKPT   <- paste0("es_placebo_", ES_TAG, "_ckpt")
CKPT_W <- paste0("es_placebo_", ES_TAG, "_ckpt_win")
ckpt_f <- file.path(CACHE, paste0(CKPT, ".parquet"))

res <- list(); wins <- list(); done <- list()
if (isTRUE(PL$resume) && file.exists(ckpt_f)) {
  old <- load_dt(CKPT)
  ## a checkpoint written for a different outcome or a different balanced window
  ## is not this run's and must not be resumed into it
  if (identical(sort(unique(old$rel_month)), sort(as.integer(keep_rel)))) {
    ## only reuse the modes this run is actually producing -- a checkpoint left
    ## by a two-mode run must not inject a thinly-drawn mode into a one-mode
    ## figure, where it would be plotted as if it were a real band
    old <- old[mode %in% PL$modes]
    res[[1L]]  <- old
    wins[[1L]] <- load_dt(CKPT_W)[mode %in% PL$modes]
    done <- lapply(split(old$draw, old$mode), unique)
    cat("\ncheckpoint found: ", uniqueN(old[, .(mode, draw)]),
        " draw(s) reused (",
        paste(sprintf("%s=%d", names(done), lengths(done)), collapse = ", "),
        ")\n", sep = "")
  } else {
    cat("\ncheckpoint found but its rel_months do not match this run",
        " -- ignored, drawing from scratch.\n", sep = "")
  }
}
for (md in PL$modes) if (is.null(done[[md]])) done[[md]] <- integer(0)

## ---------------------------------------------------------------------------
## the worker pool. Each worker sources the same two files and loads the same
## pre-filtered inputs, so one_draw() there is the same function it is here.
## Workers are capped so the panel copies cannot exhaust the machine -- 02_clean
## already runs close to the ceiling on 16 GB, and a swapping run is slower than
## a serial one.
## ---------------------------------------------------------------------------
free_gb <- tryCatch({
  x <- system2("powershell", c("-NoProfile", "-Command",
    "(Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory"), stdout = TRUE)
  as.numeric(x[nzchar(x)][1]) / 1024 / 1024
}, error = function(e) NA_real_)
gb_per_worker <- 1.6
n_work <- PL$workers
if (is.finite(free_gb)) n_work <- max(1L, min(n_work, floor(free_gb / gb_per_worker)))
n_work <- min(n_work, max(1L, parallel::detectCores() - 1L))
cat("
free RAM: ", round(free_gb, 1), " GB -> ", n_work, " worker(s) x ",
    PL$threads_per_worker, " fixest threads
", sep = "")

## everything a draw needs, as PLAIN data -- no environments, so nothing here
## drags the panel through serialisation
K <- list(seed = PL$seed, modes = PL$modes, max_attempts = PL$max_attempts,
          geo = geo, pl_eps = PL_EPS, sample = SAMPLE, segs = SEGS, trt = TRT,
          min_treated = PL$min_treated, keep_rel = keep_rel, fml = ES_FML,
          threads = PL$threads_per_worker)

## nothing to draw (a resumed run that is already complete) needs no pool
work_left <- sum(vapply(PL$modes, function(m)
  length(setdiff(seq_len(PL$n_draws), done[[m]])), 0L))

CL <- NULL
if (n_work > 1L && work_left > 0L) {
  CL <- parallel::makePSOCKcluster(n_work)
  wd <- getwd()
  parallel::clusterExport(CL, c("wd", "K", "SEGS"), envir = environment())
  invisible(parallel::clusterEvalQ(CL, {
    setwd(wd)
    source("00_setup.R"); source("placebo_funs.R")
    setFixest_nthreads(K$threads)
    pl_load_inputs(quiet = TRUE, light = TRUE, keep_clients = function(pan)
      pan[observed == 1L & wealth >= P$min_wealth_pre & discretionary == 0L &
            main_bank_yn == 1L & MA_Kundensegment %in% SEGS, unique(Bp_ID)])
    TRUE
  }))
  cat("worker pool ready (", n_work, " processes)
", sep = "")
}

## created in a bare environment on purpose: a closure defined at script level
## would carry this script's environment, and that is where the panel lives
WFUN <- eval(quote(function(r, md, cands, K) pl_one_draw(md, r, cands, K)),
             envir = new.env(parent = globalenv()))

for (md in PL$modes) {
  cat("\n-------------------------------------------------------------\n")
  cat("3. PLACEBO DRAWS -- mode '", md, "'\n", sep = "")
  cat("-------------------------------------------------------------\n")

  cands <- lapply(seq_len(nrow(geo)), function(j)
    pl_candidates(geo[j], md, STRESS, PLD$min, PLD$max, PL$match_n_months))
  cat("window geometry and candidate start dates:\n")
  print(data.table(src_ep = geo$src_ep, ep_label = geo$ep_label,
                   len_days = geo$len_days, n_months = geo$n_months,
                   n_pre = geo$n_pre, n_post = geo$n_post,
                   n_cand = lengths(cands)))
  if (any(lengths(cands) == 0L)) {
    cat("\n!! no feasible placebo start for: ",
        paste(geo$src_ep[lengths(cands) == 0L], collapse = ", "),
        "\n   mode '", md, "' skipped. Set PL$match_n_months <- FALSE to widen\n",
        "   the candidate sets.\n", sep = "")
    next
  }

  todo <- setdiff(seq_len(PL$n_draws), done[[md]])
  if (length(todo) < PL$n_draws)
    cat("resuming: ", PL$n_draws - length(todo), " draw(s) already in the ",
        "checkpoint, ", length(todo), " to go\n", sep = "")
  kept <- 0L; dropped <- 0L
  t0 <- Sys.time()

  ## Draws are independent and each is seeded from (mode, draw), so they can run
  ## in any order and in any process. They are dispatched in CHUNKS rather than
  ## all at once so the checkpoint is written as the run proceeds -- an
  ## interrupted run then costs at most one chunk.
  chunks <- split(todo, ceiling(seq_along(todo) / max(1L, PL$chunk)))
  for (ch in chunks) {
    got <- if (is.null(CL))
      lapply(ch, WFUN, md = md, cands = cands, K = K)
    else
      parallel::clusterApply(CL, ch, WFUN, md = md, cands = cands, K = K)
    got <- Filter(Negate(is.null), got)
    dropped <- dropped + (length(ch) - length(got))
    if (length(got)) {
      res  <- c(res,  lapply(got, `[[`, "path"))
      wins <- c(wins, lapply(got, `[[`, "win"))
      kept <- kept + length(got)
      save_dt(rbindlist(res),  CKPT)
      save_dt(rbindlist(wins), CKPT_W)
    }
    gc(verbose = FALSE)
    cat(sprintf("  ... %d/%d drawn, %d kept, %.1f min  [checkpoint]\n",
                max(ch), PL$n_draws, kept,
                as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  }
  cat(sprintf(
    "mode '%s': %d kept, %d dropped (unplaceable or < %d treated), %.1f min\n",
    md, kept, dropped, PL$min_treated,
    as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
if (!is.null(CL)) { parallel::stopCluster(CL); CL <- NULL }

ESP <- rbindlist(res)

if (!nrow(ESP)) {

  cat("\nno placebo draw produced a usable sample -- nothing to report.\n")
  log_step("17_es_wrel_placebo: no usable placebo draw")

} else {

WIN <- rbindlist(wins)
save_dt(ESP, paste0("es_placebo_", ES_TAG))

## ---------------------------------------------------------------------------
## 6. report
## ---------------------------------------------------------------------------

cat("\n-------------------------------------------------------------\n")
cat("4. SAMPLE SIZE AND TREATMENT SUPPORT, real vs placebo\n")
cat("-------------------------------------------------------------\n")
cat("real   : ", real$n[1], " rows used\n", sep = "")
print(unique(ESP[, .(mode, draw, n_cells, n_treated, trt_rate, n)])[
  , .(draws = .N, cells = round(mean(n_cells)), treated = round(mean(n_treated)),
      trt_rate = round(mean(trt_rate), 4), rows = round(mean(n))), by = mode])

## the path test, rel_month by rel_month
summ <- ESP[rel_month != -1L, {
  b  <- est[is.finite(est)]
  rb <- real[rel_month == .BY$rel_month, est]
  .(real    = rb,
    n_draws = length(b),
    pl_mean = mean(b),
    pl_sd   = sd(b),
    pl_p05  = quantile(b, 0.05, names = FALSE),
    pl_p50  = quantile(b, 0.50, names = FALSE),
    pl_p95  = quantile(b, 0.95, names = FALSE),
    p_two   = mean(abs(b) >= abs(rb)),
    p_one   = if (rb >= 0) mean(b >= rb) else mean(b <= rb),
    size_05 = mean(p[is.finite(p)] < 0.05))
}, by = .(mode, rel_month)]
setorder(summ, mode, rel_month)

cat("\n-------------------------------------------------------------\n")
cat("5. THE PATH AGAINST THE PLACEBO DISTRIBUTION\n")
cat("-------------------------------------------------------------\n")
for (m in unique(summ$mode)) {
  cat("\n== mode '", m, "' ==\n", sep = "")
  print(summ[mode == m, .(rel_month, real = round(real, 5),
                          pl_mean = round(pl_mean, 5), pl_sd = round(pl_sd, 5),
                          pl_p05 = round(pl_p05, 5), pl_p95 = round(pl_p95, 5),
                          p_two = round(p_two, 3), p_one = round(p_one, 3),
                          size_05 = round(size_05, 3), n_draws)])
}

cat("\nHOW TO READ THIS\n")
cat("  real      the 06_specs coefficient at that rel_month (ref = -1)\n")
cat("  pl_mean   the same coefficient averaged over placebo window sets\n")
cat("  p_two     share of placebo draws at least as large in ABSOLUTE value as\n")
cat("            the real one, at that rel_month\n")
cat("  size_05   share of placebo draws significant at 5% at that rel_month --\n")
cat("            the SIZE of the specification. On the pre-period rel_months\n")
cat("            this is the parallel-trends test applied to a window where\n")
cat("            nothing happened: well above 0.05 there means the design finds\n")
cat("            a pre-trend wherever it looks.\n\n")
cat("The picture to look for is not one coefficient but the SHAPE. If the\n")
cat("placebo mean path is flat before 0 and jumps after, the jump is what an\n")
cat("advisor contact is followed by in any window -- reverse causality (a\n")
cat("contact is scheduled around money moving) or selection into who gets one --\n")
cat("and not what advice does in a crisis. Only the part of the real path that\n")
cat("clears the placebo band is a crisis effect.\n")

## ---------------------------------------------------------------------------
## 7. figures
## ---------------------------------------------------------------------------

## the real path with its own 95% CI, against the placebo band
band <- summ[, .(mode, rel_month, pl_mean, pl_p05, pl_p95)]
band <- rbind(band, data.table(mode = unique(band$mode), rel_month = -1L,
                               pl_mean = 0, pl_p05 = 0, pl_p95 = 0))
setorder(band, mode, rel_month)
rl <- real[, .(rel_month, est, lo = est - 1.96 * se, hi = est + 1.96 * se)]

gg1 <- ggplot(band, aes(rel_month)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = 3, colour = "grey40") +
  geom_ribbon(aes(ymin = pl_p05, ymax = pl_p95), fill = "#2E86C1", alpha = 0.22) +
  geom_line(aes(y = pl_mean), colour = "#2E86C1", linewidth = 0.7) +
  geom_errorbar(data = rl, aes(ymin = lo, ymax = hi), width = 0.12,
                colour = "#C0392B", linewidth = 0.5, inherit.aes = TRUE) +
  geom_line(data = rl, aes(y = est), colour = "#C0392B", linewidth = 0.8) +
  geom_point(data = rl, aes(y = est), colour = "#C0392B", size = 1.7) +
  facet_wrap(~ mode, ncol = 2) +
  scale_x_continuous(breaks = keep_rel) +
  labs(x = "months since dd_start (-1 = last pre-drawdown month)",
       y = sprintf("coefficient on rel_month x %s", TRT),
       title = paste0(ES_LAB, " -- real episodes against placebo windows"),
       subtitle = paste("red = the real event study with its 95% CI;",
                        "blue = placebo mean and 5-95% band")) +
  theme_light()

f1 <- file.path(FIG_DIR, sprintf("17_es_%s_placebo.pdf", ES_TAG))
if (pdf_ok(f1, width = 11, height = 5)) { print(gg1); dev.off()
  cat("\nwritten: ", f1, "\n", sep = "") }

## every draw, so a band that hides a bimodal set of paths cannot pass unnoticed
gg2 <- ggplot(ESP, aes(rel_month, est, group = draw)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = 3, colour = "grey40") +
  geom_line(colour = "#2E86C1", alpha = 0.18, linewidth = 0.35) +
  geom_line(data = rl, aes(rel_month, est), inherit.aes = FALSE,
            colour = "#C0392B", linewidth = 0.9) +
  geom_point(data = rl, aes(rel_month, est), inherit.aes = FALSE,
             colour = "#C0392B", size = 1.7) +
  facet_wrap(~ mode, ncol = 2) +
  scale_x_continuous(breaks = keep_rel) +
  labs(x = "months since dd_start (-1 = last pre-drawdown month)",
       y = sprintf("coefficient on rel_month x %s", TRT),
       title = paste0(ES_LAB, " -- one line per placebo window set"),
       subtitle = "red = the real event study") +
  theme_light()

f2 <- file.path(FIG_DIR, sprintf("17_es_%s_placebo_draws.pdf", ES_TAG))
if (pdf_ok(f2, width = 11, height = 5)) { print(gg2); dev.off()
  cat("written: ", f2, "\n", sep = "") }

## ---------------------------------------------------------------------------
## 8. table
## ---------------------------------------------------------------------------

texf <- file.path(TAB_DIR, sprintf("tab_es_%s_placebo.tex", ES_TAG))
writeLines(c(
  "\\begin{tabular}{llrrrrrr}",
  "\\hline\\hline",
  paste("mode & rel. month & real & placebo mean & placebo sd & p5 & p95 &",
        "$p$ (2-sided) \\\\"),
  "\\hline",
  summ[, sprintf("%s & %d & %.4f & %.4f & %.4f & %.4f & %.4f & %.3f \\\\",
                 mode, rel_month, real, pl_mean, pl_sd, pl_p05, pl_p95, p_two)],
  "\\hline\\hline",
  "\\end{tabular}"), texf)
cat("written: ", texf, "\n", sep = "")

cat("\nthe first placebo window set of each mode, for the record:\n")
print(WIN[, .SD[draw == min(draw)], by = mode][
  , .(mode, src_ep, peak_date, trough_date, dd_start, dd_end, pre_start, post_end)])

log_step(sprintf("placebo event study (%s): %d draws over %d mode(s)",
                 ES_Y, uniqueN(ESP[, .(mode, draw)]), uniqueN(ESP$mode)))
}

sink()
