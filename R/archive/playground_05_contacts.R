


emp_dir <- "C:/Users/FWehmann/Dropbox/Financial Advice in Crisis Periods/Empirical Analysis/"
emp_dir_raw <- "C:/Users/FWehmann/Dropbox/Wehmann Household Finance Project/RawData_Stata/"


contacts <- readRDS(paste0(emp_dir_raw,"/T20_Kundenkontakte.rds"))

contacts[,Kontakt_DT := as.Date(as_factor(Kontakt_DT))]


contacts_d <- contacts[,.N,by=Kontakt_DT]
setorder(contacts_d, Kontakt_DT)
ggplot(contacts_d,aes(x=Kontakt_DT,y=N))+geom_line()


contacts_d_inv <- contacts[,.(inv_n = sum(K_Anlegen,na.rm=T)),by=Kontakt_DT]
setorder(contacts_d_inv, Kontakt_DT)
ggplot(contacts_d_inv,aes(x=Kontakt_DT,y=inv_n))+geom_line()

ggplot(contacts_d_inv[year(Kontakt_DT) %in% c("2022") & inv_n >0],aes(x=Kontakt_DT,y=inv_n))+geom_line()
