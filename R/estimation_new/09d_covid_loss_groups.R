# COVID crash loss on the portfolio by client group (sex, age, risk type, wealth)
#
# loss = price + FX change of the portfolio over Feb-Mar 2020 (dp_tot_pf + dfx_tot_pf),
#        in CHF and in % of the Jan-2020 portfolio; groups are measured in Jan 2020.
# Computed once raw and once winsorized at 0.1% / 99.9% (client level).

library(data.table)
library(arrow)
library(ggplot2)

rm(list=ls());gc()

winsor <- function(x, p = 0.999) {
  q <- quantile(x[is.finite(x)], c(1 - p, p), na.rm = TRUE)
  x[is.infinite(x)] <- NA_real_
  pmin(pmax(x, q[1]), q[2])
}

overleaf_dir <- "C:/Users/FWehmann/Dropbox/Apps/Overleaf/Uncertainty and Financial Advise (1)"
fig_dir      <- file.path(overleaf_dir, "figures/invbeh")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

pos <- setDT(read_parquet("../../data/pos_pf.parquet"))

# pre-covid filter
cl <- pos[MDate == "2020-01-31"]
bp_filter <- cl[tot_pf>5000,Bp_ID]

pos <- pos[Bp_ID%in%bp_filter]
# ---------------------------------------------------------------------------------------
# loss per client over the crash months
loss <- pos[MDate %in% as.Date(c("2020-02-29","2020-03-31")),
            .(loss_chf = sum(dp_tot_pf + dfx_tot_pf, na.rm = TRUE)), by = Bp_ID]
loss[loss_chf==0,loss_chf := NA]


# groups in Jan 2020
cl <- pos[MDate == "2020-01-31", .(Bp_ID, sex, birth_year, anlegerprofil, tot_pf, tot_wealth)]
cl <- merge(cl, loss, by = "Bp_ID", all.x = TRUE)
cl[, loss_pct := 100 * loss_chf / tot_pf]      # NA/Inf for clients without a portfolio

cl[, age := 2020 - birth_year]
cl[, Sex := fcase(sex == "m", "Male", sex == "w", "Female", default = "Sex unknown")]
cl[, Age := fcase(age %between% c(18, 44),  "Under 45",
                  age %between% c(45, 64),  "45-64",
                  age %between% c(65, 100), "65+",
                  default = "Age unknown")]
cl[, `Risk type` := fcase(grepl("^Vorsicht", anlegerprofil), "Cautious",
                          grepl("^Ausgegl",  anlegerprofil), "Balanced",
                          grepl("^Risiko",   anlegerprofil), "Risk-seeking",
                          default = "No profile")]
cl[, Wealth := paste0("Q", cut(tot_wealth, quantile(tot_wealth, 0:4/4, na.rm = TRUE),
                               include.lowest = TRUE, labels = FALSE))]

cl[, loss_chf_w := winsor(loss_chf)]
cl[, loss_pct_w := winsor(loss_pct)]

# ---------------------------------------------------------------------------------------
# mean loss (with 95% CI) by group, raw and winsorized
grp_vars <- c("Sex", "Age", "Risk type", "Wealth")
grp_lv   <- list(Sex = c("Male", "Female", "Sex unknown"),
                 Age = c("Under 45", "45-64", "65+", "Age unknown"),
                 `Risk type` = c("Cautious", "Balanced", "Risk-seeking", "No profile"),
                 Wealth = c("Q1", "Q2", "Q3", "Q4"))
meas <- c(loss_chf = "Loss in CHF", loss_pct = "Loss in % of portfolio")

res <- rbindlist(lapply(grp_vars, function(g) rbindlist(lapply(names(meas), function(m) {
  rbind(cl[, .(version = "Raw",                  v = get(m)),                  by = .(group = get(g))],
        cl[, .(version = "Winsorized 0.1/99.9%", v = get(paste0(m, "_w"))), by = .(group = get(g))]
  )[is.finite(v), .(mean = mean(v), se = sd(v) / sqrt(.N), N = .N), by = .(group, version)
  ][, `:=`(char = g, measure = meas[[m]])]
}))))
res[, char    := factor(char, levels = grp_vars)]
res[, measure := factor(measure, levels = meas)]
res[, group   := factor(group, levels = rev(unlist(grp_lv)))]
print(dcast(res, char + group ~ measure + version, value.var = "mean"))

# ---------------------------------------------------------------------------------------
# plot (winsorized only; raw means are in the printed table above)
# y labels carry N from the CHF loss, right-aligned so the numbers form a column
pw    <- res[version == "Winsorized 0.1/99.9%"]
n_chf <- pw[measure == "Loss in CHF", setNames(N, as.character(group))]
lab_n <- setNames(sprintf("%s   (N = %s)", names(n_chf), formatC(n_chf, big.mark = "'", format = "d")),
                  names(n_chf))

p <- ggplot(pw, aes(mean, group)) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_linerange(aes(xmin = mean - 1.96 * se, xmax = mean + 1.96 * se),
                 colour = "#2a78d6", linewidth = 0.5) +
  geom_point(colour = "#2a78d6", size = 1.8) +
  facet_grid(char ~ measure, scales = "free", space = "free_y", switch = "y") +
  scale_y_discrete(labels = lab_n) +
  scale_x_continuous(labels = scales::label_comma(big.mark = "'"),
                     expand = expansion(mult = 0.08)) +
  labs(x = NULL, y = NULL,
       caption = paste("Mean price + FX change of the portfolio, Feb-Mar 2020, with 95% CI;",
                       "client-level values winsorized at 0.1%/99.9%.\nGroups measured in Jan 2020.",
                       "N: clients with a non-zero CHF loss.")) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        strip.placement = "outside", panel.spacing.x = unit(1.5, "lines"),
        strip.text.y.left = element_text(angle = 0, hjust = 1, vjust = 1, face = "bold"),
        strip.text.x = element_text(face = "bold", hjust = 0),
        legend.position = "top", legend.justification = "left",
        plot.caption = element_text(colour = "grey45", size = 7.5, hjust = 0),
        plot.caption.position = "plot")
p

ggsave(file.path(fig_dir, "covid_loss_by_group.pdf"), p, width = 7.5, height = 5.5,
       device = cairo_pdf)
