
library(ggplot2)


out_dir_fig <- "C:/Users/FWehmann/Dropbox/Apps/Overleaf/Uncertainty and Financial Advise (1)/figures/"
out_dir_tab <- "C:/Users/FWehmann/Dropbox/Apps/Overleaf/Uncertainty and Financial Advise (1)/tables/"



contacts[,init := ifelse(K_Aufnahme %in% c("Durch Kunde","Durch bevollmächtigten"),"Client",
                         ifelse(K_Aufnahme %in% c("Durch Kundenberater","Zentral"),"Advisor",NA))]
contacts[,init_client := ifelse(K_Aufnahme %in% c("Durch Kunde","Durch bevollmächtigten"),1,0)]
contacts[,init_advisor := ifelse(K_Aufnahme %in% c("Durch Kundenberater","Zentral"),1,0)]

contacts[,Kontakt_DT := as.Date(Kontakt_DT)]

cm <- contacts[,.(
  n_invest = sum(K_Anlegen,na.rm=T),
  n_perf = sum(K_Performancebesprechung,na.rm=T),
  n_physical = sum(K_Physisch,na.rm=T),
  n_init_c = sum(init_client,na.rm=T),
  n_init_a = sum(init_advisor,na.rm=T),
  n_basis = sum(K_Basisdienstleistung,na.rm=T),
  n_syst = sum(K_Syst_Geschaeftsmoeglichkeit,na.rm=T),
  n_satisf = sum(K_Kundenzufriedenheit,na.rm=T),
  n_compilance = sum(K_Compliance,na.rm=T),
  n_balance = sum(K_Bilanzbesprechung,na.rm=T),
  n_tax = sum(K_Steuern,na.rm=T),
  n_pensions = sum(K_Pensionierung,na.rm=T),
  n_comprehensive = sum(K_Umfassendes_Beratungsgespraech,na.rm=T),
  n_save = sum(K_Vorsorge,na.rm=T),
  n_finance = sum(K_Finanzieren,na.rm=T)
),by=.(ym = zoo::as.yearmon(Kontakt_DT))]

setorder(cm,ym)

ggplot(cm,aes(x=ym,y=n_invest))+geom_line()
ggplot(cm,aes(x=ym,y=n_perf))+geom_line()
ggplot(cm,aes(x=ym,y=n_physical))+geom_line()
ggplot(cm,aes(x=ym,y=n_init_c))+geom_line()
ggplot(cm,aes(x=ym,y=n_init_a))+geom_line()
ggplot(cm,aes(x=ym,y=n_basis))+geom_line()
ggplot(cm,aes(x=ym,y=n_satisf))+geom_line()
ggplot(cm,aes(x=ym,y=n_compilance))+geom_line()
ggplot(cm,aes(x=ym,y=n_balance))+geom_line()
ggplot(cm,aes(x=ym,y=n_tax))+geom_line()
ggplot(cm,aes(x=ym,y=n_pensions))+geom_line()
ggplot(cm,aes(x=ym,y=n_comprehensive))+geom_line()
ggplot(cm,aes(x=ym,y=n_save))+geom_line()
ggplot(cm,aes(x=ym,y=n_finance))+geom_line()




cm <- contacts[,.(
  n_invest = sum(K_Anlegen,na.rm=T),
  n_perf = sum(K_Performancebesprechung,na.rm=T),
  n_physical = sum(K_Physisch,na.rm=T),
  n_basis = sum(K_Basisdienstleistung,na.rm=T),
  n_syst = sum(K_Syst_Geschaeftsmoeglichkeit,na.rm=T),
  n_satisf = sum(K_Kundenzufriedenheit,na.rm=T),
  n_compilance = sum(K_Compliance,na.rm=T),
  n_balance = sum(K_Bilanzbesprechung,na.rm=T),
  n_tax = sum(K_Steuern,na.rm=T),
  n_pensions = sum(K_Pensionierung,na.rm=T),
  n_comprehensive = sum(K_Umfassendes_Beratungsgespraech,na.rm=T),
  n_save = sum(K_Vorsorge,na.rm=T),
  n_finance = sum(K_Finanzieren,na.rm=T),
  n_invest_perf = sum(K_Anlegen*K_Performancebesprechung,na.rm=T)
),by=.(ym = zoo::as.yearmon(Kontakt_DT),init)]

setorder(cm,ym)
cm[,init := as.factor(init)]
cm <- cm[!is.na(init)]

ggplot(cm,aes(x=ym,y=n_invest_perf,color=init))+geom_line()
ggplot(cm,aes(x=ym,y=n_invest,color=init))+geom_line()
ggplot(cm[ym>=zoo::as.yearmon("2011-03-01")],aes(x=ym,y=n_perf,color=init))+geom_line()
ggplot(cm,aes(x=ym,y=n_physical,color=init))+geom_line()
ggplot(cm,aes(x=ym,y=n_syst,color=init))+geom_line()
ggplot(cm,aes(x=ym,y=n_satisf,color=init))+geom_line()
ggplot(cm,aes(x=ym,y=n_basis,color=init))+geom_line()
ggplot(cm,aes(x=ym,y=n_compilance,color=init))+geom_line()
ggplot(cm,aes(x=ym,y=n_balance,color=init))+geom_line()
ggplot(cm,aes(x=ym,y=n_tax,color=init))+geom_line()
ggplot(cm,aes(x=ym,y=n_pensions,color=init))+geom_line()
ggplot(cm,aes(x=ym,y=n_comprehensive,color=init))+geom_line()
ggplot(cm,aes(x=ym,y=n_save,color=init))+geom_line()
ggplot(cm,aes(x=ym,y=n_finance,color=init))+geom_line()



ggplot(cm[ym>=zoo::as.yearmon("2011-03-01")],aes(x=ym,y=n_perf,color=init))+
  geom_line()+
  guides(color=guide_legend(title="Initiation"))+
  theme_light()+
  theme(legend.position="bottom")+
  labs(x=NULL,y=NULL,title="Number of Performance-Evaluation Contacts")
ggsave(paste0(out_dir_fig,"n_perf_contacts.png"))

ggplot(cm[ym>=zoo::as.yearmon("2011-03-01") & n_invest <3000],aes(x=ym,y=n_invest,color=init))+
  geom_line()+
  guides(color=guide_legend(title="Initiation"))+
  theme_light()+
  theme(legend.position="bottom")+
  labs(x=NULL,y=NULL,title="Number of Investment Contacts")
ggsave(paste0(out_dir_fig,"n_invest_contacts.png"))



ggplot(cm[ym>=zoo::as.yearmon("2011-03-01")],aes(x=ym,y=n_invest_perf,color=init))+
  geom_line()+
  guides(color=guide_legend(title="Initiation"))+
  theme_light()+
  theme(legend.position="bottom")+
  labs(x=NULL,y=NULL,title="Number of Performance-Evaluation Contacts")
ggsave(paste0(out_dir_fig,"n_invest_x_perf_contacts.png"))


# ============================================================================
cm <- load_dt("contacts_m")


names(cm)

cm_m <- cm[,.(
  n_clients_perf = sum(c_perf),
  n_clients_perf_a = sum(c_perf_a),
  n_clients_perf_p = sum(c_perf_p),
  n_clients_perf_inv = sum(c_perf_inv),
  n_clients_perf_inv_a = sum(c_perf_inv_a)
),by=.(month=month(MDate))]

setorder(cm_m,month)

cm_m[, `:=`(
  rel_n_clients_perf       = n_clients_perf       / sum(n_clients_perf),
  rel_n_clients_perf_a     = n_clients_perf_a     / sum(n_clients_perf_a),
  rel_n_clients_perf_p     = n_clients_perf_p     / sum(n_clients_perf_p),
  rel_n_clients_perf_inv   = n_clients_perf_inv   / sum(n_clients_perf_inv),
  rel_n_clients_perf_inv_a = n_clients_perf_inv_a / sum(n_clients_perf_inv_a)
)]

cm_m

ggplot(cm_m,aes(x=month))+
  geom_col(aes(y=rel_n_clients_perf))

rel_cols <- grep("^rel_", names(cm_m), value = TRUE)
rel_cols <- "rel_n_clients_perf"


cm_long <- melt(cm_m, id.vars = "month", measure.vars = rel_cols,
                variable.name = "metric", value.name = "share")

ggplot(cm_long, aes(x = as.factor(month), y = share, fill = metric)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7,show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Month", y = NULL, fill = NULL,
       title="Share of Performance Meetings per Month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(paste0(out_dir_fig,"share_perf_per_month.png"))

# =========================================================

cd <- load_dt("contacts_d")

cd <- cd[Bp_ID %in% bp_ids]

# cdy <- cd[,.(n_perf=sum(perf),n_perf_a=sum(perf_a),n_perf_inv=sum(perf_inv)),by=.(year = year(MDate),Bp_ID)]
cdy <- cd[,.(perf=ifelse(sum(perf)>0,1,0)),by=.(year = year(MDate),Bp_ID)]


## Transition matrix
lagdt <- cdy[, .(Bp_ID, year = year + 1L, perf_lag = perf)]
cdy   <- merge(cdy, lagdt, by = c("Bp_ID", "year"), all.x = TRUE)

tm <- cdy[!is.na(perf_lag), .N, by = .(perf_lag, perf)]
tm[, share := N / sum(N), by = perf_lag]

M  <- dcast(tm, perf_lag ~ perf, value.var = "share", fill = 0)
Ns <- tm[, .(N = sum(N)), by = perf_lag]
M  <- merge(M, Ns, by = "perf_lag")
setorder(M, perf_lag)


fmt <- function(x) sprintf("%.3f", x)

rows <- sprintf("%s & %s & %s & %s \\\\",
                c("No meeting $(t-1)$", "Meeting $(t-1)$"),
                fmt(M[["0"]]), fmt(M[["1"]]),
                formatC(M$N, format = "d", big.mark = ","))

cat(
  "\\begin{table}[htbp]\n\\centering\n",
  "\\caption{Year-to-year transition probabilities of performance meetings}\n",
  "\\label{tab:transition}\n",
  "\\begin{tabular}{lccc}\n\\hline\\hline\n",
  " & \\multicolumn{2}{c}{State in $t$} & \\\\\n",
  "\\cline{2-3}\n",
  "State in $t-1$ & No meeting & Meeting & $N$ \\\\\n\\hline\n",
  paste(rows, collapse = "\n"), "\n",
  "\\hline\\hline\n\\end{tabular}\n",
  "\n\\end{table}\n",
  sep = "", file = paste0(out_dir_tab,"transition.tex")
)


# =========================================================
## Month stability: is the annual review always in the same month?
##
## The transition matrix says ~75% of clients with a meeting in t-1 also have
## one in t. This asks the sharper question: conditional on meeting, is it
## always the *same* calendar month? For every client we search the anchor month
## that covers the largest number of its meeting-years (circular distance, so
## Dec <-> Jan is one month) and call the client stable if that anchor covers
## every year in which the client had a meeting. Years in which the client has
## no contact at all are absent from cd and are not counted against the client.

mo_shift <- function(m, k) ((m - 1L + k) %% 12L) + 1L

month_stability <- function(cd, flag = "perf") {
  cy <- unique(cd[get(flag) == 1L,
                  .(Bp_ID, year = year(MDate), month = month(MDate))])

  ny <- cy[, .(n_years = uniqueN(year)), by = Bp_ID]

  ## tolerance 0: the anchor month itself
  c0 <- unique(cy[, .(Bp_ID, year, a = month)])[, .N, by = .(Bp_ID, a)][
        , .(cov0 = max(N)), by = Bp_ID]

  ## tolerance 1: a meeting in a-1, a or a+1 counts as hitting anchor a
  c1 <- unique(rbindlist(lapply(-1:1, function(k)
          cy[, .(Bp_ID, year, a = mo_shift(month, k))])))[, .N, by = .(Bp_ID, a)][
        , .(cov1 = max(N)), by = Bp_ID]

  out <- merge(merge(ny, c0, by = "Bp_ID"), c1, by = "Bp_ID")
  out[, `:=`(share0  = cov0 / n_years,
             share1  = cov1 / n_years,
             stable0 = as.integer(cov0 == n_years),
             stable1 = as.integer(cov1 == n_years))]
  out[]
}

st <- month_stability(cd, "perf")

stab_summary <- function(st, mins = c(2L, 3L, 5L)) {
  rbindlist(lapply(mins, function(k) st[n_years >= k, .(
    min_years          = k,
    n_clients          = .N,
    n_same_month       = sum(stable0),
    n_within_1m        = sum(stable1),
    sh_same_month      = mean(stable0),
    sh_within_1m       = mean(stable1),
    mean_share_same    = mean(share0),
    mean_share_within1 = mean(share1))]))
}

ss <- stab_summary(st)
print(ss)

## same statistics for advisor-initiated reviews only
print(stab_summary(month_stability(cd, "perf_a")))

## how stability erodes with the number of meeting years
by_ny <- st[n_years >= 2, .(n_clients = .N,
                            same_month = mean(stable0),
                            within_1m  = mean(stable1)), by = n_years]
setorder(by_ny, n_years)
print(by_ny)

ggplot(melt(by_ny[n_years <= 12], id.vars = "n_years",
            measure.vars = c("same_month", "within_1m"),
            variable.name = "tol", value.name = "share"),
       aes(x = as.factor(n_years), y = share, fill = tol)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(labels = c(same_month = "Same month",
                                 within_1m  = "Within +/- 1 month")) +
  labs(x = "Years with a performance meeting", y = NULL, fill = NULL,
       title = "Share of Clients Meeting in the Same Month Every Year") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave(paste0(out_dir_fig, "perf_month_stability.png"))

rows <- sprintf("$\\geq %d$ & %s & %s & %s & %s & %s & %s & %s \\\\",
                ss$min_years,
                formatC(ss$n_clients,    format = "d", big.mark = ","),
                formatC(ss$n_same_month, format = "d", big.mark = ","),
                fmt(ss$sh_same_month),
                fmt(ss$mean_share_same),
                formatC(ss$n_within_1m,  format = "d", big.mark = ","),
                fmt(ss$sh_within_1m),
                fmt(ss$mean_share_within1))

cat(
  "\\begin{table}[htbp]\n\\centering\n",
  "\\caption{Clients holding the performance meeting in the same calendar month every year}\n",
  "\\label{tab:perf_month_stability}\n",
  "\\begin{tabular}{lccccccc}\n\\hline\\hline\n",
  " & & \\multicolumn{3}{c}{Same month} & \\multicolumn{3}{c}{Within $\\pm 1$ month} \\\\\n",
  "\\cline{3-5}\\cline{6-8}\n",
  "Years with meeting & Clients & $N$ & Share & Years hit & $N$ & Share & Years hit \\\\\n\\hline\n",
  paste(rows, collapse = "\n"), "\n",
  "\\hline\\hline\n\\end{tabular}\n",
  "\\begin{minipage}{\\linewidth}\\footnotesize\n",
  "Notes: $N$ and Share count clients whose meeting month is stable in \\emph{every}\n",
  "year with a meeting; ``Years hit'' is the average share of a client's meeting-years\n",
  "covered by its best anchor month. Distances are circular, so December and January\n",
  "are one month apart.\n\\end{minipage}\n",
  "\n\\end{table}\n",
  sep = "", file = paste0(out_dir_tab, "perf_month_stability.tex")
)


## ---------------------------------------------------------------------------
## Clients whose meeting always falls inside the same three consecutive months
## (the anchor month plus/minus one) in every year in which they meet. The
## window is circular, so Nov-Dec-Jan is a valid window. Clients with only one
## or two meeting-years qualify almost mechanically, hence the min_years floor.

min_years <- 3L
bp_perf_win3 <- st[stable1 == 1L & n_years >= min_years, Bp_ID]

length(bp_perf_win3)
length(bp_perf_win3) / st[n_years >= min_years, .N]
head(bp_perf_win3, 20)
