
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
