install.packages("quantmod")
library(quantmod)

# Daily data
getSymbols("UNH", src = "yahoo", from = "2011-01-01", to = Sys.Date())
head(UNH)   # Open, High, Low, Close, Volume, Adjusted

# Monthly aggregation
unh_monthly <- to.monthly(UNH, indexAt = "lastof", OHLC = TRUE)
unh_dt <- setDT(data.frame(date=index(unh_monthly),unh_monthly))


# from xts directly
unh_dt <- data.table(date = index(UNH), coredata(unh_monthly))
setnames(unh_dt, c("date","open","high","low","close","volume","adjusted"))
setkey(unh_dt,date)

unh_mf <- unh_dt[,.(date = first(date),
                    close = first(close)),
                 by = .(ym = format(date, "%Y-%m"))]
unh_ml <- unh_dt[,.(date = last(date),
                    close = last(close)),
                 by = .(ym = format(date, "%Y-%m"))]
unh_mf[,date:=floor_date(date,"months")]
unh_ml[,date:=ceiling_date(date,"months")-1]
unh_mf[,ym:=NULL]
unh_ml[,ym:=NULL]

unh_m <- unh_dt[,.(date = first(date),
                    close = mean(close,na.rm=T)),
                 by = .(ym = format(date, "%Y-%m"))]
unh_m[,date:=floor_date(date,"months")]
unh_m[,ym:=NULL]

# unh_dt[,close_chf:=to_chf(close,"USD",date,"monthly")]

# Or pull monthly directly
getSymbols("UNH", src = "yahoo", periodicity = "monthly", from = "2000-01-01")



### =================

fxrates <- fx_rates_chf()

usdchf <- fxrates[currency=="USD"] %>% select(c("Date","rate_chf"))
names(usdchf) <- c("date","usdchf")

usdchf_mf <- usdchf[,.(date = first(date),
                        usdchf = first(usdchf)),
                     by= .(ym=format(date,"%Y-%m"))]
usdchf_ml <- usdchf[,.(date = last(date),
                       usdchf = last(usdchf)),
                    by= .(ym=format(date,"%Y-%m"))]
usdchf_mf[,date:=floor_date(date,"months")]
usdchf_ml[,date:=ceiling_date(date,"months")-1]
usdchf_mf[,ym:=NULL]
usdchf_ml[,ym:=NULL]


usdchf_m <- usdchf[,.(date = first(date),
                       usdchf = mean(usdchf,na.rm=T)),
                    by= .(ym=format(date,"%Y-%m"))]
usdchf_m[,date:=floor_date(date,"months")]
usdchf_m[,ym:=NULL]

unh_l <- merge(unh_dt,usdchf_ml,by="date")

unh_l[,close_chf := UNH.Close * usdchf]


ggplot(unh_l,aes(x=date))+
  geom_line(aes(x=date,y=close_chf/close_chf[1],color="chf"))+
  geom_line(aes(x=date,y=UNH.Close/UNH.Close[1],color="usd"))


ggplot(fxrates[currency=="USD"],aes(x=Date,y=rate_chf))+geom_line()

