## =============================================================================
## run_all.R -- the whole pipeline, in order
##
## Run from R/estimation/ :   Rscript run_all.R
## or interactively        :   setwd("R/estimation"); source("run_all.R")
##
## 01 prints an audit and does not change anything. READ IT before trusting
## anything downstream. 06 is the slow step (a few minutes).
## =============================================================================

steps <- c("01_audit.R",        # audit pos_m
           "02b_contacts.R",    # contact log      -> treatment
           "02c_positions.R",   # pos_full         -> equity share + asset prices
           "02d_trades.R",      # boerse           -> trade level
           "02e_cash.R",        # pos_merged       -> deposit balances
           "02_clean.R",        # the client-month panel
           "03_episodes.R",
           "04_treatment.R",
           "04b_cfgap.R",       # frozen-holdings counterfactual (needs 04)
           "05_outcomes.R", "06_specs.R", "07_robustness.R",
           "08_checks.R",      # P(sell) source/channel forensics, eq-share pre-trend
           "11_supply_demand.R", # advice supply vs demand; advisor capacity
           "09_export.R")      # publish tables + figures to the paper folder

## Each script is run with eval(parse()), NOT source(), and this matters.
## source(f, local = new.env()) evaluates the file one top-level expression at a
## time, each in its own frame, so an on.exit() registered at the top level of
## the script fires IMMEDIATELY -- at the end of the expression that registered
## it -- instead of at the end of the script. Every script here opens a report
## with sink(...); on.exit(sink()), and 02c / 02e / 04b open a duckdb connection
## with on.exit(dbDisconnect(con, shutdown = TRUE)). Under source() the sink
## closed at once (04_treatment.txt and 04b_cfgap.txt came out 0 bytes and the
## report went to the console instead) and 04b's connection was shut down one
## line after it was opened, so the script died with "Invalid connection".
## eval(parse(f)) evaluates the whole file inside this function's frame, so
## on.exit defers to the end of the step, which is what the scripts assume.
run_step <- function(f) eval(parse(f), envir = new.env(parent = globalenv()))

t0 <- Sys.time()
for (s in steps) {
  message("\n############ ", s, " ############")
  ti <- Sys.time()
  run_step(s)
  message(sprintf("   %s done in %.1f min", s,
                  as.numeric(difftime(Sys.time(), ti, units = "mins"))))
  gc()
}
message(sprintf("\npipeline finished in %.1f min",
                as.numeric(difftime(Sys.time(), t0, units = "mins"))))
