# =============================================================================
# Data diagnostics for the final estimation sample (stk1 + sample filters of 07)
# Runs stand-alone, or after 07_estim_v3.R in the same session: config objects
# that already exist (win, TRT, drop_nonpos_wealth, ...) are reused.
# Output: console + CSVs (and a few .tex tables / .png figures) in diag_dir.
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(ggplot2)
})

# ---- Config (mirrors 07_estim_v3.R; keep in sync) ----------------------------
if (!exists("data_path"))          data_path <- "../../data/stk1.parquet"
if (!exists("win"))                win <- c(-6L, 9L)
if (!exists("REF"))                REF <- -1L
if (!exists("TRT"))                TRT <- "treat_perfinv_a_p"
if (!exists("drop_nonpos_wealth")) drop_nonpos_wealth <- TRUE

diag_dir <- "../../results/diagnostics"
dir.create(diag_dir, showWarnings = FALSE, recursive = TRUE)

TRTS     <- c(inv_a = "treat_inv_a_p", perfinv_a = "treat_perfinv_a_p", inv_c = "treat_inv_c_p")
contacts <- c("inv_a_p", "perfinv_a_p", "inv_c_p")   # monthly contact dummies
stopifnot(TRT %in% TRTS)

# ---- Helpers -----------------------------------------------------------------
mdiff <- function(a, b) as.integer((year(a) - year(b)) * 12L + (month(a) - month(b)))

show <- function(d, name, title = name) {
  cat("\n", strrep("=", 78), "\n", title, "\n", strrep("=", 78), "\n", sep = "")
  print(d, nrows = 500, class = FALSE)
  # a CSV open in Excel is locked on Windows -> warn instead of aborting the script
  tryCatch(fwrite(d, file.path(diag_dir, paste0(name, ".csv"))),
           error = \(e) warning(sprintf("%s.csv not written: %s", name, conditionMessage(e))))
  invisible(d)
}

# Minimal LaTeX tabular writer (no extra packages)
write_tex <- function(d, file, digits = 2) {
  d <- copy(d)
  for (v in names(d)) {
    x <- d[[v]]
    if (is.numeric(x)) {
      is_int <- all(x == round(x), na.rm = TRUE)
      set(d, j = v, value = formatC(x, format = "f", digits = if (is_int) 0 else digits,
                                    big.mark = "'"))
    } else {
      set(d, j = v, value = as.character(x))
    }
  }
  esc <- function(s) gsub("([_%&#])", "\\\\\\1", s)
  lines <- c(sprintf("\\begin{tabular}{l%s}", strrep("r", ncol(d) - 1)),
             "\\hline",
             paste(esc(names(d)), collapse = " & ") |> paste("\\\\"),
             "\\hline",
             apply(d, 1, \(r) paste(esc(r), collapse = " & ") |> paste("\\\\")),
             "\\hline", "\\end{tabular}")
  writeLines(lines, file.path(diag_dir, file))
}

nd <- function(x1, x0) {  # normalized difference (Imbens & Rubin)
  (mean(x1, na.rm = TRUE) - mean(x0, na.rm = TRUE)) /
    sqrt((var(x1, na.rm = TRUE) + var(x0, na.rm = TRUE)) / 2)
}

# ---- Load --------------------------------------------------------------------
diag_cols <- c("Bp_ID", "ep_id", "MDate", "rel_month", "dd_start", "dd_end", "advisor_id",
               "tot_wealth", "tot_pf", "cash_liq", "cash_locked", "hypo",
               "equity", "bond", "reales", "fund_mixed", "deriv", "alt",
               "dp_tot_pf", "dq_tot_pf", "dfx_tot_pf",
               contacts, TRTS,
               "tot_wealth_pre", "tot_wealth_pre_mean", "tot_pf_pre",
               "cash_liq_pre", "cash_locked_pre", "hypo_pre", "equity_pre", "bond_pre",
               "anlagepaket_pre", "adv_segment_pre",
               "pf_share_of_w", "eq_share_of_pf", "eq_share_of_w")

dt <- setDT(read_parquet(data_path, col_select = all_of(diag_cols), mmap = FALSE))
dt[, rel_month := as.integer(rel_month)]

dt <- dt[tot_pf_pre>5000]

# ---- 1. Sample funnel (same filters as 07) -----------------------------------
step <- function(d, label) data.table(step = label, rows = nrow(d),
                                      clients = uniqueN(d$Bp_ID),
                                      client_eps = uniqueN(d, by = c("Bp_ID", "ep_id")),
                                      episodes = uniqueN(d$ep_id))
funnel <- list(step(dt, "stk1 (all months pre_start..post_end)"))
dt <- dt[between(rel_month, win[1], win[2])]
funnel <- c(funnel, list(step(dt, sprintf("event window [%d, %d]", win[1], win[2]))))
dt <- dt[tot_wealth_pre > 0 & tot_wealth_pre_mean > 0]
funnel <- c(funnel, list(step(dt, "tot_wealth_pre > 0 & pre_mean > 0")))
if (drop_nonpos_wealth) {
  dt <- dt[tot_wealth > 0]
  funnel <- c(funnel, list(step(dt, "tot_wealth > 0 (conditions on outcome)")))
}
show(rbindlist(funnel), "01_funnel", "1. Sample funnel")

# ---- Client x episode table --------------------------------------------------
const_cols <- c("dd_start", "dd_end", TRTS, "tot_wealth_pre", "tot_pf_pre", "cash_liq_pre",
                "cash_locked_pre", "hypo_pre", "equity_pre", "bond_pre",
                "anlagepaket_pre", "adv_segment_pre")
setorder(dt, Bp_ID, ep_id, MDate)
ce <- dt[, c(lapply(.SD, first),
             .(advisor_id = advisor_id[which.min(abs(rel_month - REF))],
               n_months   = .N,
               has_ref    = any(rel_month == REF))),
         by = .(Bp_ID, ep_id), .SDcols = const_cols]
# balanced = observed in every month that stk1 actually covers within win
# (stk1 may be shorter than win, e.g. pre_start..post_end = -6..10)
ce[, `:=`(balanced     = n_months == diff(range(dt$rel_month)) + 1L,
          pf_share_pre = tot_pf_pre / tot_wealth_pre,
          eq_share_pre = fifelse(tot_pf_pre > 0, equity_pre / tot_pf_pre, NA_real_),
          bd_share_pre = fifelse(tot_pf_pre > 0, bond_pre   / tot_pf_pre, NA_real_),
          log_w_pre    = log(tot_wealth_pre))]
ep_order <- ce[, .(dd_start = first(dd_start)), by = ep_id][order(dd_start), ep_id]

# ---- 2. Per-episode overview: clients, treatment, wealth ---------------------
summ_ep <- function(d) d[, .(
  client_eps     = .N,
  clients        = uniqueN(Bp_ID),
  advisors       = uniqueN(advisor_id),
  sh_balanced    = mean(balanced),
  sh_no_ref      = mean(!has_ref),                    # REF month missing -> weaker anchor
  n_inv_a        = sum(treat_inv_a_p,     na.rm = TRUE),
  n_perfinv_a    = sum(treat_perfinv_a_p, na.rm = TRUE),
  n_both_a       = sum(treat_inv_a_p == 1 & treat_perfinv_a_p == 1, na.rm = TRUE),
  n_inv_c        = sum(treat_inv_c_p,     na.rm = TRUE),
  n_inv_a_and_c  = sum(treat_inv_a_p == 1 & treat_inv_c_p == 1, na.rm = TRUE),
  sh_inv_a       = mean(treat_inv_a_p,     na.rm = TRUE),
  sh_perfinv_a   = mean(treat_perfinv_a_p, na.rm = TRUE),
  sh_inv_c       = mean(treat_inv_c_p,     na.rm = TRUE),
  w_mean_k       = mean(tot_wealth_pre) / 1e3,
  w_med_k        = median(tot_wealth_pre) / 1e3,
  w_mean_k_treat = mean(tot_wealth_pre[treat_perfinv_a_p==1L]) / 1e3,
  w_med_k_treat  = median(tot_wealth_pre[treat_perfinv_a_p==1L]) / 1e3,
  w_mean_k_untreat = mean(tot_wealth_pre[treat_perfinv_a_p==0L]) / 1e3,
  w_med_k_untreat  = median(tot_wealth_pre[treat_perfinv_a_p==0L]) / 1e3,
  pf_mean_k      = mean(tot_pf_pre, na.rm = TRUE) / 1e3,
  pf_med_k       = median(tot_pf_pre, na.rm = TRUE) / 1e3,
  pf_mean_k_treat = mean(tot_pf_pre[treat_perfinv_a_p==1L]) / 1e3,
  pf_med_k_treat  = median(tot_pf_pre[treat_perfinv_a_p==1L]) / 1e3,
  pf_mean_k_untreat = mean(tot_pf_pre[treat_perfinv_a_p==0L]) / 1e3,
  pf_med_k_untreat  = median(tot_pf_pre[treat_perfinv_a_p==0L]) / 1e3,
  sh_pf_zero     = mean(tot_pf_pre <= 0, na.rm = TRUE),
  cash_med_k     = median(cash_liq_pre, na.rm = TRUE) / 1e3,
  cash_med_k_treat     = median(cash_liq_pre[treat_perfinv_a_p==1L], na.rm = TRUE) / 1e3,
  cash_med_k_untreat     = median(cash_liq_pre[treat_perfinv_a_p==0L], na.rm = TRUE) / 1e3,
  cash_mean_k     = mean(cash_liq_pre, na.rm = TRUE) / 1e3,
  cash_mean_k_treat     = mean(cash_liq_pre[treat_perfinv_a_p==1L], na.rm = TRUE) / 1e3,
  cash_mean_k_untreat     = mean(cash_liq_pre[treat_perfinv_a_p==0L], na.rm = TRUE) / 1e3,
  pf_share_med   = median(pf_share_pre, na.rm = TRUE),
  eq_share_med   = median(eq_share_pre, na.rm = TRUE),
  pf_share_med_treat   = median(pf_share_pre[treat_perfinv_a_p==1L], na.rm = TRUE),
  eq_share_med_treat   = median(eq_share_pre[treat_perfinv_a_p==1L], na.rm = TRUE),
  pf_share_med_untreat   = median(pf_share_pre[treat_perfinv_a_p==0L], na.rm = TRUE),
  eq_share_med_untreat   = median(eq_share_pre[treat_perfinv_a_p==0L], na.rm = TRUE)
)]

ep_tab <- rbind(
  merge(ce[, .(dd_start = first(dd_start), dd_end = first(dd_end)), by = ep_id],
        ce[, summ_ep(.SD), by = ep_id], by = "ep_id")[, dd_months := mdiff(dd_end, dd_start) + 1L],
  cbind(data.table(ep_id = "All", dd_start = as.Date(NA), dd_end = as.Date(NA)),
        summ_ep(ce), data.table(dd_months = NA_integer_)),
  use.names = TRUE)
ep_tab[, ep_id := factor(ep_id, levels = c(ep_order, "All"))]
setorder(ep_tab, ep_id)
setcolorder(ep_tab, c("ep_id", "dd_start", "dd_end", "dd_months"))
show(ep_tab, "02_episodes", "2. Per episode: clients, treatment counts, pre-period wealth (CHF k)")
write_tex(ep_tab[, .(ep_id, dd_start, dd_months, clients, advisors,
                     n_inv_a, n_perfinv_a, n_inv_c, w_med_k_treat, w_med_k_untreat,
                     pf_med_k_treat, pf_med_k_untreat,cash_med_k_treat,cash_med_k_untreat)],
          "02_episodes.tex", digits = 0)

# ---- 3. Treatment overlap (pooled client-episodes) ---------------------------
overlap <- ce[, .N, by = .(inv_a = treat_inv_a_p, perfinv_a = treat_perfinv_a_p, inv_c = treat_inv_c_p)]
overlap[, share := N / sum(N)]
setorder(overlap, -N)
show(overlap, "03_overlap", "3. Treatment overlap (client-episodes)")

# ---- 4. Covariate balance treated vs control (pre-period) --------------------
bal_vars <- c("tot_wealth_pre", "log_w_pre", "tot_pf_pre", "cash_liq_pre", "cash_locked_pre",
              "hypo_pre", "pf_share_pre", "eq_share_pre", "bd_share_pre", "n_months")
balance <- rbindlist(lapply(TRTS[1:2], \(tr) {
  g1 <- ce[get(tr) == 1]; g0 <- ce[get(tr) == 0]
  rbindlist(lapply(bal_vars, \(v) data.table(
    treatment = tr, variable = v,
    mean_T = mean(g1[[v]], na.rm = TRUE), mean_C = mean(g0[[v]], na.rm = TRUE),
    med_T  = median(g1[[v]], na.rm = TRUE), med_C = median(g0[[v]], na.rm = TRUE),
    norm_diff = nd(g1[[v]], g0[[v]]))))
}))
show(balance, "04_balance", "4. Balance (pooled). |norm_diff| > 0.25 is a red flag")
write_tex(balance[treatment == TRT, !"treatment"], sprintf("04_balance_%s.tex", TRT))

# ---- 5. Categorical composition by treatment ---------------------------------
cat_comp <- function(var) {
  x <- ce[, .N, by = .(level = get(var), treated = get(TRT))]
  x[, share := N / sum(N), by = treated]
  dcast(x, level ~ paste0("treated_", treated), value.var = c("N", "share"), fill = 0)
}
show(cat_comp("anlagepaket_pre"), "05a_anlagepaket", sprintf("5a. Anlagepaket by %s", TRT))
show(cat_comp("adv_segment_pre"), "05b_adv_segment", sprintf("5b. Advisor segment by %s", TRT))

# ---- 6. Advisors: clusters and concentration of treatment --------------------
adv <- ce[, .(n = .N, n_tr = sum(get(TRT), na.rm = TRUE)), by = .(ep_id, advisor_id)]
top_share <- function(n_tr, p = 0.1) {
  s <- sort(n_tr, decreasing = TRUE)
  if (sum(s) == 0) return(NA_real_)
  sum(s[seq_len(ceiling(p * length(s)))]) / sum(s)
}
adv_tab <- adv[, .(advisors          = .N,
                   adv_with_treated  = sum(n_tr > 0),
                   adv_mixed         = sum(n_tr > 0 & n_tr < n),  # treated and control clients
                   clients_per_adv_med = as.numeric(median(n)),
                   top10pct_adv_share_of_treated = top_share(n_tr)), by = ep_id]
adv_tab <- rbind(adv_tab[match(ep_order, ep_id)],
                 data.table(ep_id = "All (clusters in 07)", advisors = uniqueN(ce$advisor_id)),
                 fill = TRUE)
show(adv_tab, "06_advisors", sprintf("6. Advisors per episode (treatment = %s)", TRT))

# ---- 7. Repeated presence / treatment across episodes -------------------------
rep_tab <- ce[, .(n_eps = .N, n_treated = sum(get(TRT), na.rm = TRUE)), by = Bp_ID][
  , .N, by = .(n_eps, n_treated)][order(n_eps, n_treated)]
rep_tab[, share := N / sum(N)]
show(rep_tab, "07_repeat", sprintf("7. Clients by #episodes present and #episodes treated (%s)", TRT))

# ---- 8. Distribution of pre-period levels (CHF) ------------------------------
qs <- c(0, .01, .05, .25, .5, .75, .95, .99, 1)
dist_tab <- rbindlist(lapply(c("tot_wealth_pre", "tot_pf_pre", "cash_liq_pre", "hypo_pre"), \(v)
  as.data.table(as.list(quantile(ce[[v]], qs, na.rm = TRUE)))[, variable := v]))
setcolorder(dist_tab, "variable")
show(dist_tab, "08_dist_pre", "8. Quantiles of pre-period levels (client-episodes, CHF)")

# ---- 9. Data quality ---------------------------------------------------------
# 9a. Missing values in the panel
miss <- dt[, lapply(.SD, \(x) mean(is.na(x)))]
miss <- melt(miss, measure.vars = names(miss), variable.name = "column", value.name = "share_na")
show(miss[share_na > 0][order(-share_na)], "09a_missing", "9a. Share missing (only columns with NAs)")

# 9b. Shares outside [0, 1]
shares_bad <- rbindlist(lapply(c("pf_share_of_w", "eq_share_of_pf", "eq_share_of_w"), \(v)
  data.table(variable = v, n_na = sum(is.na(dt[[v]])),
             n_outside = sum(!between(dt[[v]], 0, 1), na.rm = TRUE))))
show(shares_bad, "09b_shares", "9b. Share variables outside [0, 1]")

# 9c. Portfolio identity month-on-month: d tot_pf = dp + dq + dfx ?
#     (only consecutive months; gaps drive the residual in the PF decomposition of 07)
dt[, `:=`(d_pf = tot_pf - shift(tot_pf), gap_m = rel_month - shift(rel_month)), by = .(Bp_ID, ep_id)]
dt[gap_m == 1L, pf_gap := d_pf - (dp_tot_pf + dq_tot_pf + dfx_tot_pf)]
pf_id <- dt[!is.na(pf_gap), .(
  rows               = .N,
  sh_gap_gt_100chf   = mean(abs(pf_gap) > 100),
  sh_gap_gt_1pct_pf  = mean(abs(pf_gap) > 0.01 * pmax(abs(tot_pf - d_pf), 1)),
  sum_abs_gap_rel    = sum(abs(pf_gap)) / sum(abs(d_pf)),
  med_gap            = median(pf_gap)), by = ep_id]
pf_id <- pf_id[match(ep_order, ep_id)]
show(pf_id, "09c_pf_identity", "9c. PF identity: d tot_pf - (dp + dq + dfx), consecutive months")

# 9d. Wealth identity: tot_wealth vs tot_pf + cash_liq + cash_locked
dt[, w_gap := tot_wealth - (tot_pf + cash_liq + cash_locked)]
w_id <- dt[, .(sh_gap_gt_1pct_w = mean(abs(w_gap) > 0.01 * tot_wealth, na.rm = TRUE),
               med_rel_gap      = median(w_gap / tot_wealth, na.rm = TRUE),
               p99_abs_rel_gap  = quantile(abs(w_gap / tot_wealth), .99, na.rm = TRUE)), by = ep_id]
show(w_id[match(ep_order, ep_id)], "09d_wealth_identity",
     "9d. Wealth identity: tot_wealth - (tot_pf + cash_liq + cash_locked)")

# 9e. Asset classes vs tot_pf (classes are non-exclusive in 02a; deriv/alt/unmapped incl.)
dt[, cls_sum := rowSums(.SD, na.rm = TRUE),
   .SDcols = c("equity", "bond", "reales", "fund_mixed", "deriv", "alt")]
cls_id <- dt[tot_pf > 0, .(sh_cls_neq_pf_1pct = mean(abs(cls_sum - tot_pf) > 0.01 * tot_pf),
                           agg_cls_over_pf    = sum(cls_sum) / sum(tot_pf),
                           agg_eq   = sum(equity) / sum(tot_pf), agg_bond = sum(bond) / sum(tot_pf),
                           agg_reales = sum(reales) / sum(tot_pf), agg_mixed = sum(fund_mixed) / sum(tot_pf),
                           agg_deriv = sum(deriv) / sum(tot_pf), agg_alt = sum(alt) / sum(tot_pf)),
             by = ep_id]
show(cls_id[match(ep_order, ep_id)], "09e_asset_classes", "9e. Asset classes as share of tot_pf")

# ---- 10. Raw paths around the event (treated vs control) ---------------------
dt[, grp := fifelse(get(TRT) == 1, "Treated", "Control")]

# 10a. Monthly contact rates: treatment is defined over the drawdown -> should spike there
cont <- melt(dt[, lapply(.SD, mean, na.rm = TRUE), by = .(rel_month, grp), .SDcols = contacts],
             id.vars = c("rel_month", "grp"), variable.name = "contact", value.name = "rate")
show(dcast(cont, rel_month ~ contact + grp, value.var = "rate"), "10a_contact_rates",
     "10a. Monthly contact rates by rel_month")
p_cont <- ggplot(cont, aes(rel_month, rate, colour = grp)) +
  geom_line() + geom_point(size = 1) +
  geom_vline(xintercept = REF + 0.5, linetype = "dashed") +
  facet_wrap(~contact, scales = "free_y") +
  labs(x = "Months relative to drawdown start", y = "Share of clients with contact",
       colour = NULL, title = sprintf("Contact rates, groups by %s", TRT)) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
print(p_cont)
ggsave(file.path(diag_dir, sprintf("10a_contact_rates_%s.png", TRT)), p_cont, width = 9, height = 4)

# 10b. Median / mean wealth and PF relative to pre-period (raw, no FE)
paths <- dt[, .(w_rel_med  = median(tot_wealth / tot_wealth_pre),
                w_rel_mean = mean(tot_wealth / tot_wealth_pre),
                pf_rel_med = median((tot_pf / tot_pf_pre)[tot_pf_pre > 0]),
                n = .N), by = .(rel_month, grp)][order(grp, rel_month)]
show(paths, "10b_raw_paths", "10b. Raw wealth / PF relative to pre-period, by group")
p_paths <- ggplot(melt(paths, id.vars = c("rel_month", "grp"),
                       measure.vars = c("w_rel_med", "pf_rel_med")),
                  aes(rel_month, value, colour = grp)) +
  geom_line() + geom_point(size = 1) +
  geom_hline(yintercept = 1, linewidth = 0.3) +
  geom_vline(xintercept = REF + 0.5, linetype = "dashed") +
  facet_wrap(~variable, scales = "free_y") +
  labs(x = "Months relative to drawdown start", y = "Median ratio to pre-period",
       colour = NULL, title = sprintf("Raw paths, groups by %s", TRT)) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
print(p_paths)
ggsave(file.path(diag_dir, sprintf("10b_raw_paths_%s.png", TRT)), p_paths, width = 9, height = 4)

# 10c. Panel attrition within the window
attr_tab <- dcast(dt[, .N, by = .(rel_month, grp)], rel_month ~ grp, value.var = "N")
show(attr_tab, "10c_attrition", "10c. Observations per rel_month (attrition)")

# ---- 11. Descriptive table for the paper (per episode) -----------------------
# Panel A: clients, advisors, treated counts (each treatment on its own; perfinv_a is a
#          subset of inv_a, so the two are not mutually exclusive), client-initiated inv.
# Panel B/C: pre-crisis total wealth / liquid cash (CHF k), mean and median by group.
#          Untreated = neither adv.-initiated treatment.
# "All": clients/advisors are unique across episodes, treated counts are client-episodes.
if (!exists("tab_dir")) {
  tab_dir <- file.path("C:/Users/FWehmann/Dropbox/Apps/Overleaf/Uncertainty and Financial Advise (1)",
                       "tables")
}
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)

ep_labels <- c(taper_tantrum_201305 = "Taper tantrum",
               snb_floor_out_201501 = "SNB floor exit",
               china_crash_201508   = "China crash",
               volmageddon_201801   = "Volmageddon",
               xmas_plunge_201811   = "Christmas plunge",
               covid_202002         = "COVID-19",
               svb_cs_202303        = "SVB / Credit Suisse")

paper_row <- function(d) {
  out <- data.table(clients     = uniqueN(d$Bp_ID),
                    advisors    = uniqueN(d$advisor_id),
                    n_inv_a     = sum(d$treat_inv_a_p     %in% 1),
                    n_perfinv_a = sum(d$treat_perfinv_a_p %in% 1),
                    n_inv_c     = sum(d$treat_inv_c_p     %in% 1))
  masks <- list(inv_a     = d$treat_inv_a_p %in% 1,
                perfinv_a = d$treat_perfinv_a_p %in% 1,
                untreated = d$treat_inv_a_p %in% 0 & d$treat_perfinv_a_p %in% 0)
  vars <- c(w = "tot_wealth_pre", cash = "cash_liq_pre")
  for (vn in names(vars)) {
    for (gn in names(masks)) {
      x <- d[[vars[[vn]]]][masks[[gn]]]
      set(out, j = paste(vn, gn, "mean", sep = "_"), value = mean(x, na.rm = TRUE) / 1e3)
      set(out, j = paste(vn, gn, "med",  sep = "_"), value = median(x, na.rm = TRUE) / 1e3)
    }
  }
  out
}

ep_start <- ce[, .(dd_start = first(dd_start)), by = ep_id]
paper <- rbind(
  rbindlist(lapply(ep_order, \(e) {
    lab <- if (e %in% names(ep_labels)) ep_labels[[e]] else e
    cbind(episode = sprintf("%s (%s)", lab, format(ep_start[ep_id == e, dd_start], "%Y-%m")),
          paper_row(ce[ep_id == e]))
  })),
  cbind(episode = "All", paper_row(ce)))
show(paper, "11_paper_table", "11. Paper table: per episode (CHF k)")

fmt <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")
tex_rows <- function(cols) {
  body <- vapply(seq_len(nrow(paper)), \(i) paste(
    c(gsub("&", "\\\\&", paper$episode[i]), vapply(cols, \(v) fmt(paper[[v]][i]), "")),
    collapse = " & "), "")
  body <- paste0(body, " \\\\")
  c(head(body, -1), "\\midrule", tail(body, 1))   # "All" row below a rule
}
grp_header <- function(title) c(
  "\\addlinespace",
  sprintf("\\multicolumn{7}{l}{\\textit{%s}} \\\\", title),
  "\\addlinespace",
  " & \\multicolumn{2}{c}{Adv.-init. inv.} & \\multicolumn{2}{c}{Adv.-init. perf.\\,+\\,inv.} & \\multicolumn{2}{c}{Untreated} \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}",
  "Episode & Mean & Median & Mean & Median & Mean & Median \\\\",
  "\\midrule")
stat_cols <- function(vn) as.vector(outer(c("inv_a", "perfinv_a", "untreated"), c("mean", "med"),
                                          \(g, s) paste(vn, g, s, sep = "_")) |> t())

tex <- c(
  "% requires \\usepackage{booktabs}",
  "\\begin{tabular}{l*{6}{r}}",
  "\\toprule",
  "\\multicolumn{7}{l}{\\textit{Panel A: Sample and treatment}} \\\\",
  "\\addlinespace",
  "Episode & Clients & Advisors & Adv.-init. inv. & Adv.-init. perf.\\,+\\,inv. & Client-init. inv. & \\\\",
  "\\midrule",
  sub(" \\\\\\\\$", " & \\\\\\\\", tex_rows(c("clients", "advisors", "n_inv_a", "n_perfinv_a", "n_inv_c"))),
  grp_header("Panel B: Pre-crisis total wealth (CHF thousand)"),
  tex_rows(stat_cols("w")),
  grp_header("Panel C: Pre-crisis liquid cash (CHF thousand)"),
  tex_rows(stat_cols("cash")),
  "\\bottomrule",
  "\\end{tabular}")
writeLines(tex, file.path(tab_dir, "desc_episodes.tex"))
cat("\nPaper table written to", file.path(tab_dir, "desc_episodes.tex"), "\n")

cat("\nDiagnostics written to", normalizePath(diag_dir), "\n")
