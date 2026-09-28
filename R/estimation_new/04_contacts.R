# load packages
library(data.table)
library(haven)
library(arrow)
library(lubridate)
library(dplyr)
library(duckdb)
library(ggplot2)


rm(list=ls()); gc()

emp_dir_raw <- "C:/Users/FWehmann/Dropbox/Wehmann Household Finance Project/RawData_Stata/"


# Contacts
contacts <- read_parquet(paste0(emp_dir_raw,"T20_Kundenkontakte.parquet"))

contacts[, MDate := as.IDate(lubridate::ceiling_date(as.Date(paste0(Period_ID, "01"), "%Y%m%d"), "month") - 1)]
contacts[, DDate := as.IDate(as.Date(Kontakt_DT))]
contacts[, contact_type := fcase(
  K_Art == "Telefonkontakt","phone",
  K_Art == "E-Mail, Brief, Fax", "mail",
  K_Art %in% c("Kundentermin in der Bank","Kundentermin beim Kunden / extern"), "physical_meeting",
  K_Art == "Videoberatung","virtual_meeting",
  K_Art == "Veranstaltung", "event",
  default = "other"
)]
contacts[,personal := contact_type %chin% c("phone","physical_meeting","virtual_meeting","event")]
contacts[,`:=`(
  inv = K_Anlegen,
  perf = K_Performancebesprechung,
  perfinv = K_Anlegen * K_Performancebesprechung
)]
contacts[,init := fcase(
  # K_Aufnahme == "Durch Kunde","client",
  K_Aufnahme %in% c("Durch Kunde","Durch Bevollmächtigten"),"client",
  K_Aufnahme == "Durch Kundenberater", "advisor",
  K_Aufnahme == "Zentral", "central",
  # K_Aufnahme == "Durch Bevollmächtigten","representative",
  default = "unkown"
)]


keep <- c("MDate","Bp_ID","DDate","personal","contact_type","inv","perf","perfinv","init")
contacts <- contacts[,..keep]
contacts <- contacts[init %in% c("client","advisor")]

contacts <- contacts[inv>0|perf>0]


# -----------------------------------------------------------------------------
# aggregate to monthly

setorder(contacts, MDate, Bp_ID, DDate)   

contacts[, `:=`(adv = personal & init == "advisor",
                cli = personal & init == "client")]

contacts_m <- contacts[, {
  i  <- inv > 0; p <- perf > 0; pv <- perfinv > 0
  ai <- which(adv & i);  ci <- which(cli & i)
  ap <- which(adv & p);  cp <- which(cli & p)
  apv <- which(adv & pv);
  ai_np <- which(init=="advisor" & i)
  ci_np <- which(init=="client" & i)
  ip <- which(personal & i)
  pp <- which(personal & p)
  pvp <- which(personal & pv)
  .(
    first_a_inv    = DDate[ai_np][1L],
    first_c_inv    = DDate[ci_np][1L],
    first_a_p_inv  = DDate[ai][1L],
    first_c_p_inv  = DDate[ci][1L],
    first_a_p_perf = DDate[ap][1L],
    first_c_p_perfinv = DDate[apv][1L],          
    inv         = as.numeric(any(i)),
    perf        = as.numeric(any(p)),
    perfinv     = as.numeric(any(pv)),
    inv_p         = as.numeric(any(ip)),
    perf_p        = as.numeric(any(pp)),
    perfinv_p     = as.numeric(any(pvp)),
    inv_a_p     = as.numeric(length(ai) > 0),
    perf_a_p    = as.numeric(length(ap) > 0),
    perfinv_a_p = as.numeric(any(adv & pv)),
    inv_c_p     = as.numeric(length(ci) > 0),
    advise      = as.numeric(any(i | p)),
    advise_a_p  = as.numeric(length(ai) > 0 || length(ap) > 0),
    advise_c_p  = as.numeric(length(ci) > 0 || length(cp) > 0)
  )
}, by = .(MDate, Bp_ID)]

contacts[, c("adv", "cli") := NULL]

write_parquet(contacts_m,"../../data/contacts_aggm.parquet")


## merge with pos
# pos <- read_parquet("../../data/pos_aggm.parquet")
pos <- read_parquet("../../data/pos_agg_tr.parquet")

pos <- merge(pos,contacts_m,by=c("MDate","Bp_ID"),all.x=T)


write_parquet(pos,"../../data/pos_aggm_c.parquet")


# contacts_m <- contacts[,.(
#   first_a_p_inv  = DDate[personal & init=="advisor" & inv  == 1][1L],
#   first_c_p_inv  = DDate[personal & init=="client" & inv  == 1][1L],
#   first_a_p_perf = DDate[personal & init=="advisor" & perf == 1][1L],
#   first_a_p_perfinv = DDate[personal & init=="advisor" & perfinv == 1][1L],
#   inv = as.numeric(any(inv>0)),
#   perf = as.numeric(any(perf>0)),
#   perfinv = as.numeric(any(perfinv>0)),
#   inv_a_p = as.numeric(any(ifelse(init=="advisor"&personal&inv>0,1,0))),
#   perf_a_p = sum(ifelse(init=="advisor"&personal&perf>0,1,0)),
#   perfinv_a_p = sum(ifelse(init=="advisor"&personal&perfinv>0,1,0)),
#   advise = as.numeric(any(inv>0|perf>0)),
#   advise_a_p = as.numeric(any(ifelse(personal&init=="advisor"&(inv>0|perf>0),1,0))),
#   advise_c_p = as.numeric(any(ifelse(personal&init=="client"&(inv>0|perf>0),1,0))),
#   inv_c_p = as.numeric(any(ifelse(personal&init=="client"&(inv>0),1,0)))
# ),by=.(MDate,Bp_ID)]




# =================================
# contactsm <- contacts[personal==T,.(
#   n_inv = sum(inv),
#   n_perf = sum(perf),
#   n_perfinv = sum(perfinv)
# ),by=.(DDate,init)]
# 
# ggplot(contactsm[year(DDate)>=2011],aes(x=DDate))+
#   geom_line(aes(y=n_inv,color=init))+
#   ylim(NA,100)
# 
# ggplot(contactsm[DDate %between% c("2019-05-15","2019-05-26")],aes(x=DDate))+
#   geom_line(aes(y=n_inv,color=init))
# 
# 
# 
# ## remove "unknown" and "central"
# 
# 
# contacts[DDate %between% c("2019-05-15","2019-05-26")] %>% View()
# 
