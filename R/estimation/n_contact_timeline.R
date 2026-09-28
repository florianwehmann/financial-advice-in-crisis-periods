ep

ct <- load_dt("contacts_d")

cta <- ct[,lapply(.SD,sum),by=ContactDate,.SDcols=c("perf_inv_a_p","advice_a_p","advice_c_p","inv_a_p","inv_c_p","perf_a_p")]
ctam <- ct[,lapply(.SD,sum),by=MDate,.SDcols=c("perf_inv_a_p","advice_a_p","advice_c_p","inv_a_p","inv_c_p","perf_a_p")]

## drawdown periods to shade in the background
dd <- ep[,.(dd_start,dd_end)]

dd <- rbind(dd,data.table(dd_start=c(as.Date("2014-12-31")),dd_end=c(as.Date("2015-01-31"))))



ggplot(cta,aes(x=ContactDate))+
  geom_rect(data=dd,inherit.aes=FALSE,
            aes(xmin=dd_start,xmax=dd_end,ymin=-Inf,ymax=Inf),
            fill="grey50",alpha=0.25)+
  geom_line(aes(y=inv_a_p,color="inv_a"))+
  geom_line(aes(y=inv_c_p,color="inv_c"))+
  geom_line(aes(y=perf_a_p,color="perf_a"))

ggplot(ctam,aes(x=MDate))+
  # geom_rect(data=ep,inherit.aes=FALSE,
  #           aes(xmin=dd_start,xmax=dd_end,ymin=-Inf,ymax=Inf),
  #           fill="grey50",alpha=0.25)+
  geom_line(aes(y=inv_a_p,color="inv_a"))+
  geom_line(aes(y=inv_c_p,color="inv_c"))+
  geom_line(aes(y=perf_a_p,color="perf_a"))+
  theme_light()+
  theme(legend.position = "bottom")+
  labs(x=NULL,y=NULL,title="Number of Contacts")+
  guides(color=guide_legend(title=NULL))
ggsave(paste0(overleaf_fig,"n_contacts.jpg"))


ggplot(cta[ContactDate %between% c("2017-11-15","2017-11-30")],aes(x=ContactDate))+
  geom_line(aes(y=inv_a_p,color="inv_a"))+
  geom_line(aes(y=inv_c_p,color="inv_c"))+
  geom_line(aes(y=perf_a_p,color="perf_a"))



subct <- ct[ContactDate %between% c("2019-05-13","2019-05-30")]
subct <- ct[ContactDate %between% c("2017-11-18","2017-11-30")]


subct[, .(N          = .N,
          n_advisor  = sum(init_by_advisor == 1),
          n_client   = sum(init_by_client  == 1),
          n_invest   = sum(inv==1),
          n_perf     = sum(perf==1),
          n_personal = sum(personal==1)),
      by = ContactDate][order(-N)]








tr <- load_dt("trades_d")

tr_ex_man <- tr[chan!="mandate"]
# tr_ex_man <- tr

tr_isin <- tr[ISIN_Key=="CH0334714620"]

tra_isin <- tr_isin[,.(
  n_trades = .N,
  chf = sum(chf)
),by=DDate]

ggplot(tra_isin,aes(x=DDate))+
  geom_line(aes(y=n_trades))


tra <- tr_ex_man[,.(
  n_trades = .N,
  chf = sum(chf)
),by=DDate]
tram <- tr_ex_man[,.(
  n_trades = .N,
  chf = sum(chf)
),by=MDate]

ggplot(tra[year(DDate)>=2011],aes(x=DDate))+
  geom_rect(data=ep,inherit.aes=FALSE,
            aes(xmin=dd_start,xmax=dd_end,ymin=-Inf,ymax=Inf),
            fill="grey50",alpha=0.25)+
  geom_line(aes(y=n_trades))

ggplot(tram[year(MDate)>=2011],aes(x=MDate))+
  geom_rect(data=ep,inherit.aes=FALSE,
            aes(xmin=dd_start,xmax=dd_end,ymin=-Inf,ymax=Inf),
            fill="grey50",alpha=0.25)+
  geom_line(aes(y=n_trades))


ggplot(tra[year(DDate)>=2011],aes(x=DDate))+
  # geom_rect(data=ep,inherit.aes=FALSE,
  #           aes(xmin=dd_start,xmax=dd_end,ymin=-Inf,ymax=Inf),
  #           fill="grey50",alpha=0.25)+
  geom_line(aes(y=chf))

ggplot(tram[year(MDate)>=2011],aes(x=MDate))+
  geom_rect(data=ep,inherit.aes=FALSE,
            aes(xmin=dd_start,xmax=dd_end,ymin=-Inf,ymax=Inf),
            fill="grey50",alpha=0.25)+
  geom_line(aes(y=chf))


ggplot()+
  # geom_rect(data=ep,inherit.aes=FALSE,
  #           aes(xmin=dd_start,xmax=dd_end,ymin=-Inf,ymax=Inf),
  #           fill="grey50",alpha=0.25)+
  geom_line(data=spi[year(Date)==2023 & month(Date) == 10],aes(x=Date,y=spi_tr/spi_tr[1],color="spi"))+
  geom_line(data=smi[year(Date)==2023 & month(Date) == 10],aes(x=Date,y=smi/smi[1],color="smi"))




subtr <- tr_ex_man[DDate %between% c("2017-11-10","2017-11-20")] # forget (only mandate)

subtr <- tr_ex_man[DDate %between% c("2017-03-31","2017-04-05")] # forget (SKGB fund pushed)

subtr <- tr_ex_man[DDate %between% c("2014-01-30","2014-02-03")] # forget (single invesotr, huge trade)





