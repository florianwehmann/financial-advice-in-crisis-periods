
library(quantmod)

getSymbols("^VIX", src = "yahoo", from = "2011-01-01", to = "2024-12-31")

vix <- as.data.table(Cl(VIX))

dd <- ep[,.(dd_start,dd_end)]

ggplot(ctam,aes(x=MDate))+
  geom_rect(data=dd,inherit.aes=FALSE,
            aes(xmin=dd_start,xmax=dd_end,ymin=-Inf,ymax=Inf),
            fill="grey50",alpha=0.25)+
  geom_line(data=vix,aes(x=index,y=VIX.Close*40-100,color="VIX"),size=1,alpha=.5)+
  geom_line(aes(y=inv_a_p,color="inv_a"),size=1)+
  geom_line(aes(y=inv_c_p,color="inv_c"),size=1)+
  geom_line(aes(y=perf_a_p,color="perf_a"),size=1)+
  theme_light()+
  theme(legend.position = "bottom")+
  labs(x=NULL,y=NULL,title="Number of Contacts")+
  guides(color=guide_legend(title=NULL))
ggsave(paste0(overleaf_fig,"n_contacts.jpg"))



ggplot(smi[year(Date)%between%c(2011,2024)],aes(x=Date,y=smi))+geom_line()+
  annotate("rect",
           xmin = as.Date("2019-05-01"), xmax = as.Date("2019-05-30"),
           ymin = -Inf, ymax = Inf,
           fill = "grey60", alpha = 0.3)+
  annotate("rect",
           xmin = as.Date("2015-01-10"), xmax = as.Date("2015-02-05"),
           ymin = -Inf, ymax = Inf,
           fill = "grey60", alpha = 0.3)+
  annotate("rect",
           xmin = as.Date("2013-05-20"), xmax = as.Date("2013-06-30"),
           ymin = -Inf, ymax = Inf,
           fill = "grey60", alpha = 0.3)+
  annotate("rect",
           xmin = as.Date("2015-08-10"), xmax = as.Date("2015-10-01"),
           ymin = -Inf, ymax = Inf,
           fill = "grey60", alpha = 0.3)+
  annotate("rect",
           xmin = as.Date("2015-12-01"), xmax = as.Date("2016-02-20"),
           ymin = -Inf, ymax = Inf,
           fill = "grey60", alpha = 0.3)+
  annotate("rect",
           xmin = as.Date("2016-09-25"), xmax = as.Date("2016-11-20"),
           ymin = -Inf, ymax = Inf,
           fill = "grey60", alpha = 0.3)+
  annotate("rect",
           xmin = as.Date("2018-01-20"), xmax = as.Date("2018-03-30"),
           ymin = -Inf, ymax = Inf,
           fill = "grey60", alpha = 0.3)+
  annotate("rect",
           xmin = as.Date("2018-11-20"), xmax = as.Date("2019-01-10"),
           ymin = -Inf, ymax = Inf,
           fill = "grey60", alpha = 0.3)+
  annotate("rect",
           xmin = as.Date("2020-02-20"), xmax = as.Date("2020-04-01"),
           ymin = -Inf, ymax = Inf,
           fill = "grey60", alpha = 0.3)+
  annotate("rect",
           xmin = as.Date("2021-12-31"), xmax = as.Date("2022-03-10"),
           ymin = -Inf, ymax = Inf,
           fill = "grey60", alpha = 0.3)+
  annotate("rect",
           xmin = as.Date("2022-04-20"), xmax = as.Date("2022-06-25"),
           ymin = -Inf, ymax = Inf,
           fill = "grey60", alpha = 0.3)+
  annotate("rect",
           xmin = as.Date("2022-08-15"), xmax = as.Date("2022-10-05"),
           ymin = -Inf, ymax = Inf,
           fill = "grey60", alpha = 0.3)+
  geom_line(data=vix,aes(x=index,y=VIX.Close*100+4000,color="VIX"),size=1,alpha=.5)+
  theme_light()+
  labs(x=NULL,y=NULL,title="SMI")+
  guides(color=guide_legend(title=NULL))+
  theme(legend.position="bottom")



ggplot()+
  geom_line(data=vix[index%between%c("2015-07-01","2015-09-30")],aes(x=index,y=VIX.Close*100+4000,color="VIX"),size=1,alpha=.5)


ggplot(cta[year(ContactDate)==2019 & month(ContactDate) %in% c(5,6)],aes(x=ContactDate))+
  geom_line(aes(y=perf_inv_a_p,color="perf_inf_a_p"))+
  geom_line(aes(y=inv_a_p,color="inv_a_p"))+
  geom_line(aes(y=inv_c_p,collr="inv_c_p"))
