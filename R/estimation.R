### Claude Code Instructions
#
# IMPLEMENTED IN  R/estimation/  --  run with:  setwd("R/estimation"); source("run_all.R")
# Read R/estimation/README.md first: it lists which parts of the design pos_m can
# and cannot support, and how every TODO below was resolved (all parameters live
# in the list `P` in R/estimation/00_setup.R).
#
# Treatment comes from data/contacts.parquet (02b_contacts.R), measured on the
# exact daily window [peak_date, trough_date]. The old pos_m advice fields only
# fired when a trade followed a contact within 5 days: they caught 32% of real
# advice contacts and missed the 36% of contacted clients who did not trade.
#
# Still NOT available, and flagged rather than faked:
#   - no equity share and no cash    => de-risking is measured on net flows
#   - no security-level prices       => cf_gap is approximated, not frozen holdings
#   - no trade-level data            => per-sale forgone return is not built
#   - contact topics start in 2011   => sample 2011-01..2024-12, 3 usable episodes
#
# Tested and NOT true: K_Performancebesprechung is not a clean scheduled meeting.
# Only 18% of consecutive reviews are ~12 months apart and review volume peaks in
# the crash month itself. See "On the scheduled review meetings idea" in
# R/estimation/README.md.


# # Project: Effect of financial advice on retail portfolios during market drawdowns
# 
# ## Goal
# Estimate whether clients who receive advisor contact during a market drawdown
# perform better in the 12 months after the drawdown, primarily because advice
# prevents panic selling and improves re-entry timing.
# 
# Write R code using data.table + fixest. Produce a single reproducible pipeline
# split into numbered scripts. Do not silently invent column names — if a required
# field is missing, stop and print a clear message listing what's needed.
# 
# ## Data (TODO: confirm names/paths)
# - `pos_m`   : client-month portfolio panel. Keys: Bp_ID, MDate.
#               Fields: pf_ret (monthly return), wealth, eq_share, advisor_id,
#               plus TODO: cash/deposit balance? cost basis? asset-level holdings?
# - `contacts`: contact log. Fields: Bp_ID, contact_date, initiator
#               (advisor/client), channel (meeting/phone/email/bulk), TODO: scheduled flag?
# - `trades`  : transaction level. Fields: Bp_ID, trade_date, ISIN, direction, amount.
# - `prices`  : security-level monthly prices/returns for counterfactual construction.
# - Market index for defining drawdowns (TODO: which index).
# 
# ## 0. Data audit (run first, print a report, do not proceed silently)
# - Panel completeness: gaps per Bp_ID, clients entering/exiting, clients who go to
#   zero holdings vs. disappear entirely. Report both counts.
# - Is cash/deposit inside the data perimeter? If sale proceeds leave the observed
#   perimeter, flag this loudly — it invalidates the return index for sellers.
# - Contact log completeness: logging intensity by advisor and by month. Flag if
#   logging drops during crisis months (measurement error correlated with treatment).
# - Distribution of pf_ret; count of |pf_ret| > 0.5; count of wealth <= 0.
# - Share of clients with discretionary mandates (TODO: which flag) — these must be
#   excluded from the main sample, analyzed separately.
# 
# ## 1. Cleaning
# - Winsorize pf_ret at 1/99 BEFORE any compounding.
# - Drop client-months with pre-episode portfolio value below TODO (e.g. CHF 10,000).
# - Keep liquidators in the panel at zero risky-asset weight; never let them drop out.
# - Complete the client-month grid explicitly (CJ), so lag/lead shift calendar months,
#   not rows.
# 
# ## 2. Drawdown episodes
# - Define episodes on the MARKET INDEX, never on the client's own portfolio
# (client-triggered windows produce mechanical mean reversion).
# - Peak-to-trough declines of >= 15% (TODO: confirm threshold), one row per episode
# with dd_start, dd_end, pre_start = dd_start - 12m, post_end = dd_end + 12m.
# - Truncate post windows at the next dd_start so recovery windows never overlap the
# next crisis.
# - Print the resulting episode table for manual inspection before continuing.
# 
# ## 3. Treatment construction
# Build a full client x episode grid (CJ), then left-join contact counts and zero-fill.
# Restrict to clients actually present in the panel during that episode (otherwise
#                                                                        "never contacted in 2008" wrongly includes clients who joined in 2015).
# 
# Build these variants separately, never pooled:
#   - treat_adv : any advisor-initiated meeting/phone contact within [dd_start, dd_end]
# - treat_cli : any client-initiated contact in the same window (panic proxy — this is
#                                                                a control/outcome, NOT treatment)
# - n_contacts_adv : intensive margin
# - first_contact_rel_month : timing within the window
# Exclude bulk email / mass mailings from all treatment definitions.
# 
# Then print the switcher table: per client, count episodes present vs. episodes
# treated; classify as always / never / switcher. This determines which specification
# is the headline.
# 
# ## 4. Outcomes (build all four)
# 1. `ret_next12`, `ret_past12`: rolling cumulative returns via log returns
# (log1p -> frollsum -> expm1), grouped by Bp_ID, NA if any month missing.
# 2. `cf_gap`: PRIMARY performance outcome. Freeze holdings at the last pre-drawdown
# month-end, drift them with observed security prices through post_end with no
# trading, compare to the realized value path. Decompose into the drawdown leg and
# the recovery leg.
# 3. Behavioural outcomes: P(large de-risking during dd) = equity share drop above
# TODO threshold; net flow out of risky assets; P(any trade during dd);
# P(full liquidation).
# 4. Re-entry: for clients who de-risked, months until equity share returns to within
# TODO% of pre-crisis level; and per-sale forgone return (return on the sold
#                                                         security from sale date to +6/+12m).
# 
# ## 5. Specifications
# Stack episodes; create ci = Bp_ID x episode, te = MDate x episode,
# rel_month = months since dd_start.
# 
# (a) Stacked event study — main:
#   feols(y ~ i(rel_month, treat_adv, ref = -1) + treat_cli +
#           (log_w_pre + eq_share_pre + contacts_pre) : factor(rel_month)
#         | ci + te, vcov = ~ advisor_id)
# All covariates frozen at the last pre-drawdown month. Never control for
# contemporaneous wealth — it is an outcome.
# 
# (b) Within-client across episodes (identified off switchers):
#   same, but | Bp_ID + te. Only lead with this if the switcher share is material.
# 
# (c) Within-advisor: add advisor_id FE or advisor x episode FE.
# 
# (d) Cross-section at dd_end, one row per client x episode, as a robustness check
# free of overlapping-window correlation.
# 
# Clustering: advisor_id throughout; also report two-way advisor + te.
# Run behavioural outcomes FIRST — if advice doesn't change trading, any return
# result is selection, and the paper should say so.
# 
# ## 6. Robustness / diagnostics
# - Episode-by-episode coefficients alongside the pooled estimate.
# - Pre-period coefficients (rel_month -12..-2) as parallel-trends evidence.
# - Placebo: identical pipeline on N randomly drawn non-drawdown windows.
# - Episode weighting: unweighted vs. equal-weight-per-episode (long episodes
#   otherwise dominate).
# - Dose-response in n_contacts_adv.
# - Attrition: compare treated/control exit rates after each episode.
# - Bank revenue side: did crisis contacts coincide with rotation into
#   higher-margin products? (TODO: is product margin/fee data available?)
# 
# ## 7. Output
# - All tables via etable() to LaTeX, all event studies via iplot() to PDF.
# - One `results/` folder, one log file recording sample sizes at every filter step.


