library(quantmod); library(data.table); library(ggplot2)

getSymbols("ORCL", src = "yahoo", from = "2011-01-01", to = Sys.Date())

unh_dt <- data.table(date = index(ORCL), coredata(ORCL))
setnames(unh_dt, c("date","open","high","low","close","volume","adjusted"))
setkey(unh_dt, date)

# month-end on the actual last TRADING day
unh_ml <- unh_dt[, .(date     = last(date),
                     close    = last(close),
                     adjusted = last(adjusted)),
                 by = .(ym = format(date, "%Y-%m"))]


xxxchf <- fxrates[currency=="EUR"] %>% select(c("Date","rate_chf"))
names(xxxchf) <- c("date","xxxchf")

usdchf_ml <- xxxchf[, .(fx_date = last(date),
                        xxxchf  = last(xxxchf)),
                    by = .(ym = format(date, "%Y-%m"))]

# join on the MONTH, not the date
unh_l <- merge(unh_ml, usdchf_ml, by = "ym", all.x = TRUE)
unh_l <- unh_l[date<"2026-08-01"]
stopifnot(!anyNA(unh_l$xxxchf))          # catches missing FX months loudly

unh_l[, `:=`(close_chf = close    * xxxchf,
             adj_chf   = adjusted * xxxchf)]

ggplot(unh_l, aes(x = date)) +
  geom_line(aes(y = adj_chf  / first(adj_chf),  colour = "CHF")) +
  geom_line(aes(y = adjusted / first(adjusted), colour = "USD")) +
  labs(y = "Indexed (first obs = 1)", colour = NULL)


ggplot()+
  geom_line(data=am[Asset_ID == 275804 & MDate>="2014-01-01"],aes(x=MDate,y=cumprod(1+ret_adj2)/(1+ret_adj2[1]),color="pos"),size=1)+
  geom_line(data=am[Asset_ID == 275804 & MDate>="2014-01-01"],aes(x=MDate,y=cumprod(1+ret_1m)/(1+ret_1m[1]),color="kurs"),size=1)+
  # geom_line(data=am[Asset_ID == 276086 & MDate>="2014-01-01"],aes(x=MDate,y=cumprod(1+ret_1m_fxconv)/(1+ret_1m_fxconv[1]),color="kurs fx"))+
  geom_line(data=unh_l[date>="2014-01-01" & date < "2025-01-01"],aes(x=date,y=adj_chf/adj_chf[1],color="yahoo in chf"),size=1)#+
# geom_line(data=unh_l[date>="2014-01-01" & date < "2025-01-01"],aes(x=date,y=adjusted/adjusted[1],color="yahoo in xxx"))

