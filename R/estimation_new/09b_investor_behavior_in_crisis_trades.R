# investor behavior with trades



trades <- read_parquet("../../data/trades.parquet")


eq_funds   <- c("Fund - Shares (09)", "Fund - Exchange Traded (03)", "Fund - Index (04)")
bd_funds   <- "Fund - Bond (12)"
re_funds   <- "Fund - Real Estate (01)"
deriv_grp  <- c("Strukturierte Prod./Zertifikate", "Optionen", "Warrants", "Futures")
# deriv_grp  <- c("Strukturierte Prod./Zertifikate", "Warrants", "Futures")
alt_grp    <- c("Metall", "Kryptowaehrung", "Kryptowährung",
                "Ansprueche", "Ansprüche", "Anrechte", "Waehrung", "Währung")

trades[, asset_class := fcase(
  Instrumentengruppe == "Aktien",                                      "equity",
  Instrumentengruppe == "Fonds" & Fondsart %in% eq_funds,              "equity",
  Instrumentengruppe == "Obligationen",                                "bond",
  Instrumentengruppe == "Fonds" & Fondsart %in% bd_funds,              "bond",
  Instrumentengruppe == "Fonds" & Fondsart %in% re_funds,              "reales",
  Instrumentengruppe == "Fonds" & !is.na(Fondsart),                    "fund_mixed",
  Instrumentengruppe %in% deriv_grp,                                   "deriv",
  Instrumentengruppe %in% alt_grp,                                     "alt",
  default = NA_character_)]


trades[is.na(asset_class),Instrumentengruppe] %>% unique()
trades[is.na(asset_class)] %>% View()

trades_c <- trades[MDate %between% c("2019-06-01","2021-06-01")]

trades_c[Instrumentengruppe=="Obligationen"]
trades_c[asset_class=="bond"] %>% View()

agg_cols <- c("sell","buy","abs_chf","sell_chf","buy_chf","px_chf")


trades_c_l <- trades_c[,lapply(.SD,sum,na.rm=T),.SDcols=agg_cols,by=.(MDate,asset_class)]

ggplot(trades_c_l[MDate>"2019-06-30"],aes(x=MDate,color=asset_class))+
  geom_line(aes(y=-buy_chf))+
  geom_vline(xintercept=as.Date("2020-03-31"),linetype=2,alpha=.5)


ggplot(trades_c_l[MDate>"2019-06-30"],aes(x=MDate,color=asset_class))+
  geom_line(aes(y=sell_chf))+
  geom_vline(xintercept=as.Date("2020-03-31"),linetype=2,alpha=.5)

ggplot(trades_c_l[MDate>"2019-06-30"],aes(x=MDate,color=asset_class))+
  geom_line(aes(y=-abs_chf))+
  geom_vline(xintercept=as.Date("2020-03-31"),linetype=2,alpha=.5)


