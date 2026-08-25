# Estimation pipeline — advice during market drawdowns

Implements the design written in `R/estimation.R` against the data that actually
exists. Five data files are used: `pos_m.parquet` (client x month aggregate),
`pos_full.parquet` (client x month x position, all instrument groups),
`contacts.parquet` (contact log), `boerse.parquet` (trades), and the two local
index files `hspitr_2.csv` / `hsmi.csv`. Run with:

```r
setwd("R/estimation"); source("run_all.R")     # or: Rscript run_all.R
```

Everything is written to `results/`: one text report per step, LaTeX tables in
`results/tables/`, figures in `results/figures/`, intermediate parquet files in
`results/cache/`, and a running sample-size log in `results/pipeline_log.txt`.

**For a genuinely clean run, clear `results/figures/` and `results/tables/` too,
not just `results/cache/`.** Nothing deletes a figure whose script stopped
producing it, so orphans accumulate: the 2026-08-24 rebuild carried four
five-day-old `*_exCinit.pdf` plots from a spec variant that no longer exists in
`06_specs.R`, and published them to the paper folder. `09_export.R` mirrors, so
it removed them from Overleaf on the next run — but only once they were gone
from `results/`.

```r
unlink(c("../../results/cache", "../../results/figures",
         "../../results/tables"), recursive = TRUE)
```

`09_export.R` then mirrors the tables and figures into the Overleaf project, so
the paper picks them up as `\input{estimation/tables/…}` and
`\includegraphics{estimation/figures/…}`. `results/` stays the source of truth;
the export never edits it and never stops the pipeline if the folder is absent.

| script | what it does |
|---|---|
| `00_setup.R` | paths, all parameters (`P`), helpers, logging. Every `TODO` from the design is resolved here. |
| `01_audit.R` | data audit. Prints a report and changes nothing. **Read it first.** |
| `02b_contacts.R` | the contact log: contact-day and client-month files. Carries the treatment. |
| `02c_positions.R` | `pos_full` (36 m rows) → portfolio composition / equity share, plus an implied CHF unit price per asset-month. duckdb, never loaded into R. |
| `02d_trades.R` | `boerse` (2.6 m trades, 49 columns) → trade-level and client-month trade files, including the **order channel** (`Medium`) and the **commission** (`Kosten`). |
| `02e_cash.R` | `pos_merged` (72.8 m rows, unfiltered) → **deposit balances**. duckdb. |
| `02_clean.R` | client-month panel on an explicit gap-free grid, winsorised returns, flow ratios. |
| `03_episodes.R` | drawdown episodes from the SPI total-return index. Prints the episode table. |
| `04_treatment.R` | client × episode grid, treatment variants, frozen covariates, switcher table. |
| `04b_cfgap.R` | the design's real counterfactual: holdings frozen at the pre-episode month end, drifted with observed security prices. |
| `05_outcomes.R` | stacked event panel + the four outcome families. |
| `06_specs.R` | specifications (a)–(d). Behavioural outcomes run first. |
| `07_robustness.R` | pre-trends, per-episode, weighting, dose, attrition, placebo, bank revenue. |
| `08_checks.R` | forensics: P(sell) by data source and by order channel, and the equity-share pre-trend step. |
| `09_export.R` | publishes `results/tables/*.tex` and `results/figures/*.pdf` to the paper folder (`EXPORT_DIR` in `00_setup.R`, override with `CRISIS_EXPORT_DIR`). Mirrors — files the pipeline stops producing are deleted there. Never aborts the run. |

## What the design asked for and what the data allows

The design assumed five things that `pos_m` alone does not contain. Four are now
supplied by `pos_full`, `contacts` and `boerse`; the fifth is not, and the
pipeline says so rather than substituting a lookalike:

1. ~~**No contact log.**~~ **Fixed** by `data/contacts.parquet` (1.27 m contacts,
   2011–2024). Treatment is now a real **advisor contact**, taken from the log and
   measured on the exact daily window `[peak_date, trough_date]`.
   The old trade-conditional proxy caught only **32.4 %** of clients who actually
   had an advisor-initiated advice contact during a drawdown, and **35.8 %** of
   contacted clients never traded — that group was coded as untreated and is
   precisely the group the paper is about. It is kept as `treat_adt` so the two
   measures can be compared (`04_treatment.txt`, and `06d_treatment_definitions`).
2. ~~**No equity share.**~~ **Fixed** by `data/pos_full.parquet` (36.0 m rows,
   client x month x position, every instrument group). `eq_share` is a real
   equity weight and `derisk_eq_dd` is the design's own de-risking measure.
   **Cash is still missing** — see the warning below.
3. ~~**No security-level panel.**~~ **Fixed.** `02c` derives an implied CHF unit
   price per asset-month (`sum(Vermoegen_CHF)/sum(Menge)` across holders) and
   `04b` uses it for `cf_gap_hold`: holdings frozen at the pre-episode month end
   and drifted with observed prices, exactly as the design specified. Price
   coverage of the frozen portfolios is 100%. The two pos_m-only approximations
   (`cf_gap_mkt`, `cf_gap_own`) are kept so the approximation error stays visible.
4. ~~**No trade-level data.**~~ **Fixed** by `data/boerse.parquet` (2.6 m trades).
   Direction comes from the sign of `Menge`; `forgone_vw_6` / `forgone_vw_12` are
   the per-sale forgone returns the design asked for.
4a. ~~**Every sell counted the same.**~~ **Fixed** by `boerse$Medium`, the order
   channel — see below. It says who physically entered the order.
4b. ~~**No cash.**~~ **Fixed** by `data/pos_merged.parquet` — see below.
5. **Advice fields are empty before 2011**, so the sample starts 2011-03 and the
   2008–09 and 2010–11 drawdowns are unusable (their pre-windows fall outside).

Counts of `n_meeting`, `n_phone`, … in `pos_m` are counts of **position rows**,
not of contacts (a client with 50 holdings scores 50 for one meeting). They are
kept only under `adt_*` for the old-vs-new comparison and are never treatment.

### Deposits (added last, and the biggest single change)

`data/pos_merged.parquet` is the join written by `R/1_merge_pos.R` **before** the
`exclude` filter, and it contains 32.8 m `Bar` rows over 142'479 clients
(Privatkonto, Sparkonto, Sparen 3, Kontokorrent, Depotkonto, …) plus 0.18 m
`Money Market Deposit` rows. Everything downstream of that filter — `pos_m1`,
`pos_mb`, `pos_m` and `pos_full` — had cash stripped out, which is why it was
invisible until now. Median balance CHF 40'166.

Three things this fixed:

1. **`risky_share` became a real weight.** Securities-only it had a median of
   1.000; with cash in the denominator, median 0.316.
2. **"Sold and sat in cash" and "left the bank" became separable.** 99.24 % of
   client-months with no securities still have a cash account. Advice reduces
   both (−1.3pp and −1.2pp).
3. **The counterfactual gap got its correct denominator.** Charging a seller a
   100 % loss on money still sitting on deposit inflated the performance
   advantage roughly 2.3× (+0.068*** → +0.029***).

Pillar 3a and rental-deposit accounts are held apart as `cash_locked`: locked
retirement money is not dry powder for market timing.

### The order channel (added last, and it is what P(sell) was missing)

`boerse` has 49 columns; `02d` was reading 8 of them. Two of the unread ones
carry the behaviour the paper is about.

**`Medium` — who physically entered the order.** Until now every sell counted
the same, so `P(sell)` pooled a client panicking at 3 a.m. with a portfolio
manager rebalancing a discretionary mandate. They move in *opposite* directions
around a crash — decision sells, monthly:

| month | client (e-banking) | advisor | mandate desk |
|---|---|---|---|
| 2020-01 | 1 641 | 839 | 3 701 |
| 2020-02 | 2 889 | 1 103 | 6 936 |
| **2020-03** (crash) | **4 814** | **1 499** | 5 130 |
| 2020-04 (after trough) | 1 923 | 608 | **8 995** |

Self-directed selling nearly triples *in* the crash month; the mandate desk
sells *after* the trough, which is rebalancing, not panic. Pooled, these
partly cancel. `sold_self` is therefore the panic-selling measure proper: it is
the only sell indicator that cannot be an advisor or a desk acting for the
client. Channel shares of all trades: mandate 41.7 %, self 23.5 %, advisor
12.3 %, other 22.6 %. `Medium` is only well populated from 2013 (mandate is
near-empty in 2011–12), which does not bind: every usable episode window starts
in 2014-08 or later.

**`Kosten` — the commission charged on each trade.** Never missing, CHF 164 m
over the sample, median CHF 8.73, 0.16 % of trade value at the median. The
exchange leg (`Kauf`/`Verkauf`) carries a median CHF 29.71; the fund leg
(`Zeichnung`/`Rückzahlung`) is 46 % zero-commission. This is the only revenue
field in the data dated to the day and attached to a specific order, so `07`
can now measure fee income *inside* the drawdown instead of over the year
around it. Product-level recurring margins are still missing.

**`Order_Type` was read but only printed.** Checked, and `Rückzahlung` is *not*
passive: 99.8 % of the rows with a known instrument group are `Fonds`, none
redeem at par, none fall in their own maturity year. They are fund redemptions —
a real decision to sell — not bond maturities, so they stay in. What is passive
is `Medium == "Sec Event"` (corporate actions posted through the trade table)
and the `Titeleingang` / `Vorauszahlung` subscription types: 12 336 rows,
0.5 %, excluded from every `*_dec` measure.

Four `P(sell)` variables now exist and they are not interchangeable:

| variable | source | what it fires on |
|---|---|---|
| `sold` | `pos_m` flow field | any position quantity fall — trades, corporate actions, transfers, in-kind moves |
| `sold_d` | `boerse` | a trade was booked |
| `sold_dec` | `boerse` | … and it was somebody's decision |
| `sold_self` | `boerse$Medium` | … and the **client** entered it |

They disagree materially: in the main sample the flow field fires in 8.7 % of
client-months, a booked trade in 6.2 %, and 40 % of flow-sells have no trade
behind them at all. `08_checks.R` runs the event study on all of them side by
side, including the parallel-trends test — `P(sell)` on the flow field is one
of the outcomes that fails it.

`smp_boerse` restricts to the 76 % of main-sample clients the trade file covers,
where the channel outcomes are not mechanically zero; every channel result is
reported on both samples.

Still missing: a scheduled/ad-hoc contact flag (the binding constraint on
identification), product-level margins, and the liability side — all 4.4 m
`Kredit` rows carry `Vermoegen_CHF = 0`.

## Treatment variants (never pooled — `TRT` in `06_specs.R` selects the headline)

| variable | definition |
|---|---|
| `treat_perf` | **headline.** `K_Performancebesprechung` contact inside the daily drawdown window |
| `treat_perf_a` | … advisor-initiated |
| `treat_perf_p` | … by meeting or phone (mail excluded) |
| `treat_adv` | any advisor-initiated `K_Anlegen`/`K_Performancebesprechung` contact |
| `treat_advice` | … either initiator |
| `treat_cli` | client-initiated advice contact — a **panic proxy**, a control/outcome, never treatment |
| `treat_adt` | the old trade-conditional proxy, for comparison only |

`on_review_cycle` flags clients with a review contact in the 12 pre-episode
months; `smp_cycle` restricts to them, where treatment is closer to "the review
happened to fall inside the crash" than to advisor selection.

### On the "scheduled review meetings are exogenous" idea

Tested, and it does **not** hold as stated (see the note in `04_treatment.txt`):

- only **18 %** of consecutive review meetings fall within ±1 month of a 12-month
  cadence, and 7.5 % land exactly at 12 months;
- a client's meetings concentrate in their modal month **33 %** of the time
  against a permuted benchmark of **28.6 %** — barely above chance;
- review-meeting counts *peak in the crash month itself* (Mar 2020: 1 064, the
  highest month of that year, against 831 in January and 585 in April).

Review meetings are still the **better** treatment — 72–77 % advisor-initiated,
56–64 % face-to-face, and unlike `K_Anlegen` their volume is not significantly
predicted by the market return (t = −1.29 vs t = −2.68) — but they are not an
instrument. `smp_cycle` narrows the selection problem; it does not remove it.

## Key parameters (`P` in `00_setup.R`)

| parameter | value | note |
|---|---|---|
| `smp_start` / `smp_end` | 2011-03-31 / 2024-12-31 | start forced by the advice data |
| `dd_threshold` | 0.15 | peak-to-trough on **daily** SPI TR |
| `pre_months` / `post_months` | 12 / 12 | post truncated at the next `dd_start` |
| `min_wealth_pre` | 10 000 CHF | at the pre-episode month |
| `min_bom_value` | 1 000 CHF | below this the return denominator is noise |
| `derisk_thresh` | 0.20 | net sales during the drawdown, share of pre-episode wealth |
| `liq_thresh` / `reentry_tol` | 0.95 / 0.10 | full liquidation / "back in" |
| `derisk_eq_drop` | 0.10 | equity-share fall, in share points, that counts as de-risking |
| `forgone_h` | 6, 12 | horizons for the per-sale forgone return |

Episodes are dated on **daily** closes: month-end closes would miss the Covid
crash entirely (−26 % daily vs. −13 % month-end to month-end).

## Usable episodes

| id | peak | trough | depth | pre window | post window |
|---|---|---|---|---|---|
| `ep2_201508` | 2015-08-05 | 2016-02-11 | −19.5 % | 2014-08 … 2015-07 | 2016-03 … 2017-02 |
| `ep3_202002` | 2020-02-19 | 2020-03-23 | −26.3 % | 2019-02 … 2020-01 | 2020-04 … 2021-03 |
| `ep4_202112` | 2021-12-28 | 2022-09-26 | −21.8 % | 2020-12 … 2021-11 | 2022-10 … 2023-09 |

`rel_month == -1` is the last month end before the decline: the reference period
of the event study and the freeze date of every covariate.
