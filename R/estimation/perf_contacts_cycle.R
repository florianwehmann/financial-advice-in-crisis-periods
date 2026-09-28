


stk <- load_dt("stk")
cle <- load_dt("cle")

pan <- load_dt("panel")

cd <- load_dt("contacts_d")



## find recurrence of perf contacts

names(pan)

treat_var <- "c_perf_a_p"

perf_month <- pan[which(pan[[treat_var]] == 1L & (Anlagepaket %in% c("COMFORT") | MA_Kundensegment %in% c("EVV-Team","natürliche Personen PB") | EVV==1)), .(Bp_ID, MDate, Anlagepaket)]
perf_month <- pan[which(pan[[treat_var]] == 1L & main_bank == "Ja" & (Anlagepaket %in% c("COMFORT") | MA_Kundensegment %in% c("EVV-Team","natürliche Personen PB") | EVV==1)), .(Bp_ID, MDate, Anlagepaket)]
perf_month <- pan[which(pan[[treat_var]] == 1L & main_bank == "Ja"), .(Bp_ID, MDate, Anlagepaket)]
perf_month[, `:=`(month = month(MDate), quarter = quarter(MDate), year = year(MDate))]
perf_month[, perf := .N, by = .(Bp_ID, year, month, Anlagepaket)]
setorder(perf_month, Bp_ID)


# ggplot(perf_month[Anlagepaket%in%c("COMFORT")],aes(x=MDate,y=perf))+
#   geom_col()+
#   theme(legend.position="none")
# 
# unique(perf_month$Anlagepaket)

perf_month_a <- perf_month[,.(perf=sum(perf)),by=c("month","Anlagepaket")]
setorder(perf_month_a,Anlagepaket,month)

ggplot(perf_month_a,aes(x=month,y=perf))+
  geom_col(aes(fill=Anlagepaket))


perf_qtr_a <- perf_month[,.(perf=sum(perf)),by=c("quarter","Anlagepaket")]
setorder(perf_qtr_a,Anlagepaket,quarter)

ggplot(perf_qtr_a,aes(x=quarter,y=perf))+
  geom_col(aes(fill=Anlagepaket))

for (nm in unique(perf_qtr_a$Anlagepaket)) {
  if (is.na(nm)) {
    gg <- ggplot(perf_qtr_a[is.na(Anlagepaket)],aes(x=quarter,y=perf))+
      geom_col()+labs(title=nm)
    print(gg)
  } else {
  gg <- ggplot(perf_qtr_a[Anlagepaket==nm],aes(x=quarter,y=perf))+
    geom_col()+labs(title=nm)
  print(gg)
  }
}


perf_y <- perf_month[,.(perf=sum(perf)),by=c("year","Anlagepaket","Bp_ID")]
setorder(perf_y,Anlagepaket,year,Bp_ID)

perf_y_a <- perf_y[year>2017,mean(perf),by=.(year,Anlagepaket)][,mean(V1),by=Anlagepaket]

lvl <- c("DIRECT", 
         "CONSULT basic", "CONSULT plus", 
         "CONSULT top", "CONSULT expert", "CONSULT international",
         "COMFORT",
         "n/a", "NA")

perf_y_a[, Anlagepaket := factor(Anlagepaket, levels = lvl)]

ggplot(perf_y_a,aes(x=Anlagepaket,y=V1))+geom_col()


evv_trades <- pan[,.(mean_trades = mean(buysell)),by=.(MDate,EVV)]
setorder(evv_trades,EVV,MDate)

ggplot(evv_trades,aes(x=MDate,mean_trades,color=as.factor(EVV)))+geom_line()


ma_segment <- pan[,.(
  .N,
  wealth = mean(wealth_tot)
),by=.(MDate,MA_Kundensegment)]
ma_segment <- pan[main_bank=="Ja",.(
  .N,
  wealth = mean(wealth_tot)
),by=.(MDate,MA_Kundensegment)]
ma_segment[, N_total := sum(N), by = MDate]
ma_segment[, share := N / N_total]
ggplot(ma_segment,aes(x=MDate))+
  geom_line(aes(y=share,color=MA_Kundensegment),size=1)+
  theme_light()
ggplot(ma_segment,aes(x=MDate))+
  geom_line(aes(y=wealth,color=MA_Kundensegment),size=1)+
  theme_light()

ggplot(ma_segment[MA_Kundensegment=="natürliche Personen PB"],aes(x=MDate))+
  geom_line(aes(y=N))



apakete <- pan[main_bank=="Ja" & MA_Kundensegment=="Beratungszentrum",.N,by=.(MDate,Anlagepaket)]
apakete[, N_total := sum(N), by = MDate]
apakete[, share := N / N_total]

ggplot(apakete,aes(x=MDate))+
  geom_line(aes(y=share,color=Anlagepaket),size=1)+
  theme_light()

ggplot(apakete[Anlagepaket=="DIRECT"],aes(x=MDate))+
  geom_line(aes(y=N),size=1)+theme_light()
ggplot(apakete[Anlagepaket=="CONSULT basic"],aes(x=MDate))+
  geom_line(aes(y=N),size=1)+theme_light()
ggplot(apakete[Anlagepaket=="CONSULT plus"],aes(x=MDate))+
  geom_line(aes(y=N),size=1)+theme_light()
ggplot(apakete[Anlagepaket=="CONSULT top"],aes(x=MDate))+
  geom_line(aes(y=N),size=1)+theme_light()
ggplot(apakete[Anlagepaket=="CONSULT expert"],aes(x=MDate))+
  geom_line(aes(y=N),size=1)+theme_light()
ggplot(apakete[Anlagepaket=="CONSULT international"],aes(x=MDate))+
  geom_line(aes(y=N),size=1)+theme_light()
ggplot(apakete[Anlagepaket=="COMFORT"],aes(x=MDate))+
  geom_line(aes(y=N),size=1)+theme_light()
ggplot(apakete[Anlagepaket=="n/a"],aes(x=MDate))+
  geom_line(aes(y=N),size=1)+theme_light()
ggplot(apakete[is.na(Anlagepaket)],aes(x=MDate))+
  geom_line(aes(y=N),size=1)+theme_light()

ggplot(apakete,aes(x=MDate,y=N))+geom_line()

uniqueN(perf_month$Bp_ID)
perf_month[month<=3,uniqueN(Bp_ID)]

bps_perf_1q <- perf_month[month<=3]$Bp_ID
bps_perf_not1q <- perf_month[month>3]$Bp_ID

length(bps_perf_1q)
length(bps_perf_not1q)

bps_perf_only1q <- setdiff(bps_perf_1q,bps_perf_not1q)
bps_perf_1q_and_other <- intersect(bps_perf_1q,bps_perf_not1q)

perf_month[(Bp_ID %in% bps_perf_only1q),.(perf=sum(perf)),by=.(Bp_ID,year)][,lapply(.SD,first),by=.(Bp_ID,year)][,.N,by=Bp_ID][N>2]

perf_month[(Bp_ID %in% bps_perf_not1q)]



perf_month_a <- perf_month[(Bp_ID %in% bps_perf_1q),.(perf=sum(perf)),by=c("month","Anlagepaket")]
setorder(perf_month_a,Anlagepaket,month)
ggplot(perf_month_a,aes(x=month,y=perf))+
  geom_col(aes(fill=Anlagepaket))



View(pan[Anlagepaket=="COMFORT" & c_perf_a==1L, .N , by=.(Bp_ID,year=year(MDate),qtr=quarter(MDate),month=month(MDate),MDate)])



ggplot(perf_month_a[Anlagepaket%in%c("DIRECT")],aes(x=month,y=perf))+
  geom_col()+labs(title="DIRECT")
ggplot(perf_month_a[Anlagepaket%in%c("COMFORT")],aes(x=month,y=perf))+
  geom_col()+labs(title="COMFORT")
ggplot(perf_month_a[Anlagepaket%in%c("CONSULT basic")],aes(x=month,y=perf))+
  geom_col()+labs(title="CONSULT basic")
ggplot(perf_month_a[Anlagepaket%in%c("CONSULT plus")],aes(x=month,y=perf))+
  geom_col()+labs(title="CONSULT plus")
ggplot(perf_month_a[Anlagepaket%in%c("CONSULT top")],aes(x=month,y=perf))+
  geom_col()+labs(title="CONSULT top")
ggplot(perf_month_a[Anlagepaket%in%c("n/a")],aes(x=month,y=perf))+
  geom_col()+labs(title="n/a")
ggplot(perf_month_a[is.na(Anlagepaket)],aes(x=month,y=perf))+
  geom_col()+labs(title="NA")


ggplot(perf_month[,.(perf=sum(perf)),by=Anlagepaket],aes(x=Anlagepaket,y=perf))+geom_col()+ylim(0,15000)
ggplot(perf_month[(Bp_ID %in% bps_perf_1q),.(perf=sum(perf)),by=Anlagepaket],aes(x=Anlagepaket,y=perf))+geom_col()+ylim(0,15000)
ggplot(perf_month[!(Bp_ID %in% bps_perf_1q),.(perf=sum(perf)),by=Anlagepaket],aes(x=Anlagepaket,y=perf))+geom_col()+ylim(0,15000)



perf_month[!(Bp_ID %in% bps_perf_1q)]





min_years <- 3

bp_ids_perf_min_y <- unique(perf_month[,c("Bp_ID","year")])[,.N,by=Bp_ID][N>=min_years]$Bp_ID


perf_month_miny <- perf_month[Bp_ID %in% bp_ids_perf_min_y]
# 
# anchor_month <- perf_month_miny[
#   ,.N,
#   by=c("Bp_ID","month")
# ][
#   , .SD[N == max(N) & max(N)>1], by= Bp_ID
# ]
# 
# perf_month_miny_anch <- merge(perf_month_miny,anchor_month[,c("Bp_ID","month")],by="Bp_ID",all.x=T)
# 

# =====================================


# counts per Bp_ID x month, over all years
cnt <- perf_month_miny[, .N, by = .(Bp_ID, month)]

# fill months with zero occurrences — needed so indexing 1:12 is positional
cnt <- cnt[CJ(Bp_ID = unique(Bp_ID), month = 1:12), on = .(Bp_ID, month)]
cnt[is.na(N), N := 0L]
setorder(cnt, Bp_ID, month)

# circular window: window m covers m, m+1, m+2 (12 -> 1 -> 2)
cnt[, win := N + N[(month %% 12) + 1L] + N[((month + 1L) %% 12) + 1L], by = Bp_ID]


best_window <- cnt[, {
  mx    <- max(win)
  ties  <- which(win == mx)
  # tie-break: window containing the strongest single month; then earliest start
  peak  <- vapply(ties, function(s) max(N[c(s, (s %% 12) + 1L, ((s + 1L) %% 12) + 1L)]), numeric(1))
  s     <- ties[which.max(peak)][1L]
  .(m1 = s,
    m2 = (s %% 12) + 1L,
    m3 = ((s + 1L) %% 12) + 1L,
    n_win     = mx,
    n_total   = sum(N),
    share     = mx / sum(N),
    n_ties    = length(ties))
}, by = Bp_ID]


mode_info <- cnt[, .(peak_n = max(N), n_modes = sum(N == max(N))), by = Bp_ID]
result <- merge(mode_info, best_window, by = "Bp_ID")
result[, anchor_clear := n_modes == 1L & peak_n > 1L]


tie_info <- cnt[, {
  mx    <- max(win)
  ties  <- which(win == mx)
  # are the tied starts contiguous on the circle?
  d     <- diff(sort(ties))
  gaps  <- c(d, 12L - sum(d))          # includes wrap-around gap
  contig <- length(ties) == 1L || sum(gaps > 1L) <= 1L
  # months covered by every tied window = the core
  cov   <- lapply(ties, function(s) c(s, (s %% 12) + 1L, ((s + 1L) %% 12) + 1L))
  core  <- Reduce(intersect, cov)
  span  <- sort(Reduce(union, cov))
  .(n_ties = length(ties),
    contiguous = contig,
    core   = paste(sort(core), collapse = ","),
    span   = paste(span, collapse = ","),
    n_win  = mx,
    share  = mx / sum(N))
}, by = Bp_ID]
