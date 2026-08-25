library(data.table)
library(haven)
library(lubridate)
library(readxl)

# Directory containing the data (--> adjust this line)
data_dir <- "C:/Users/FWehmann/Dropbox/Financial Advice in Crisis Periods/Empirical Analysis/"

parse_period_id <- function(period_id) {
  s <- as.character(period_id)
  as.Date(paste0(substr(s, 1, 4), "-", substr(s, 5, 6), "-01"))
}

ret_account <- function(account_type, account_securities_type) {
  account_type %in% "Sparen 3-Konto" |
    account_securities_type %in% c("Sparen 3-Depot", "Sparen 3-Konto")
}

#------------------------------------------------------------------------------
# (1) Portfolio holdings dataset
#------------------------------------------------------------------------------

portfolio <- as.data.table(read_dta(
  file.path(data_dir, "Source/Original Data/T30_Pos_fact.dta")
))

# Drop variables not used
portfolio[, c(
  "KassObli_Zert", "Nebenbetreuer_ID", "StruktProd_Klasse", "Ausgabejahr",
  "Faelligkeitsjahr", "Land_Emittent", "Asset_Waehrung", "BilanzpositionGL2",
  "BilanzpositionGL1", "Bilanzposition", "KontoproduktGL2", "KontoproduktGL1",
  "Referenzwaehrung", "Positionswaehrung", "Forderungen_ggue_Kunden_CHF",
  "Hypothekarforderungen_CHF", "Kundenausleihungen_CHF", "Kundeneinlagen_CHF",
  "Depotvol_Beratungsmandat_CHF", "Depotvol_Verwaltungsmandat_C",
  "Bestand_Treuhandanlagen_CHF", "AUM_CHF", "Geschaeftsvolumen_CHF",
  "DA_Titelkursabweichung_CHF", "DA_Devisenkursabweichung_CHF",
  "DA_Mengenabweichung_CHF", "DA_Delta_CHF", "DA_Gesamtabweichung_CHF",
  "Kredit_Limite_CHF"
) := NULL]

# Rename variables
setnames(portfolio,
  old = c("Bp_ID", "Cont_ID", "Pos_ID", "ISIN_Key", "Asset_ID", "Asset",
          "Kontoprodukt", "Hauptbetreuer_ID", "Depotprodukt", "Instrumentengruppe",
          "Fondsart", "Limitenart", "Menge", "Vermoegen_CHF"),
  new = c("Client_Nr", "Account_Nr", "Position_Nr", "Security_ISIN", "Security_Nr",
          "Security_Name", "Account_Type", "Advisor_Nr", "Account_SecuritiesAccountType",
          "Security_AssetClass", "Security_FundType", "Account_LimitType",
          "Position_Amount", "Position_ValueCHF")
)

# Date
portfolio[, Month     := parse_period_id(Period_ID)]
portfolio[, Date      := ceiling_date(Month, "month") - days(1L)]
portfolio[, Period_ID := NULL]

# Decode labeled variables
portfolio[, Security_ISIN                 := as.character(as_factor(Security_ISIN))]
portfolio[, Account_Type                  := as.character(as_factor(Account_Type))]
portfolio[, Account_SecuritiesAccountType := as.character(as_factor(Account_SecuritiesAccountType))]
portfolio[, Security_AssetClass           := as.character(as_factor(Security_AssetClass))]
portfolio[, Security_FundType             := as.character(as_factor(Security_FundType))]

# Drop credit limits and unclassified asset/account types
portfolio <- portfolio[Security_AssetClass != "Limite"]
portfolio <- portfolio[is.na(Account_LimitType)]
portfolio <- portfolio[Security_AssetClass != "Nicht zugeteilt (Dummy)"]
portfolio <- portfolio[!Account_Type %in% c(
  "Abwicklungskonto WEF", "Baukonto", "CWO Verwertungskonti", "Callgeld",
  "Dokumentarinkasso und Akkreditiv", "Edelmetallkonto", "Fondskonto",
  "Fondssparkonto", "Grabunterhaltskonto", "Konto 35 D", "Konto 35 D EUR",
  "PVE-Konto 35 D", "PVE-Konto 35 D EUR", "Vermittlung",
  "Vermögensverwaltungskonto", "Wiederanlagekonto"
)]

# Add negative positions
portfolio[Position_ValueCHF == 0 & Position_Amount != 0, Position_ValueCHF := Position_Amount]

# Account type vectors
credit_account_types <- c(
  "COVID-19-Kredit", "Festdarlehen/Festzinskredit", "Fester Vorschuss",
  "Festhypothek", "Festhypothek (E-Hypothek)", "Geldmarkt-Hypothek mit CAP",
  "Geldmarkt-Hypothek ohne CAP", "Investitionsdarlehen", "Risikoprivatkredit",
  "Rollover-Hypothek", "Rollover-Kredit", "SARON-Hypothek", "SARON-Kredit",
  "Variable Hypothek", "Variables Darlehen"
)
checking_account_types <- c(
  "Ausbildungskonto", "Depotkonto", "EURO-Kontokorrent", "EURO-Privatkonto",
  "Fremdwährungs-Kontokorrent", "Hypozinskonto", "Jugendkonto", "Konto 25",
  "Kontokorrent", "Kontokorrent Callgeld-Aufnahme", "Liegenschaftskonto",
  "OerK-Kontokorrent", "Personalkonto", "Privatkonto", "Privatkonto (ab 60)",
  "SGKB You", "USD-Kontokorrent", "Vereinskonto"
)
savings_account_types <- c(
  "Aktionärs-Sparkonto Unica", "Geschenksparkonto", "HäschCash",
  "Inhabersparkonto", "Jugendsparkonto/-heft", "Mieterkautionssparkonto",
  "Sparkonto/-heft", "Sparkonto/-heft (ab 60)", "Sparplankonto"
)
mortgage_account_types <- c(
  "Festhypothek", "Festhypothek (E-Hypothek)", "Geldmarkt-Hypothek mit CAP",
  "Geldmarkt-Hypothek ohne CAP", "Rollover-Hypothek", "SARON-Hypothek", "Variable Hypothek"
)
loan_account_types <- c(
  "COVID-19-Kredit", "Festdarlehen/Festzinskredit", "Fester Vorschuss",
  "Investitionsdarlehen", "Risikoprivatkredit", "Rollover-Kredit",
  "SARON-Kredit", "Variables Darlehen"
)

# All client-level wealth aggregations in a single grouped pass
portfolio[, `:=`(
  Client_BankWealth = sum(fifelse(
    !Security_AssetClass %in% "Kredit" & !Account_Type %in% credit_account_types,
    Position_ValueCHF, 0), na.rm = TRUE),
  Client_ValueChecking = sum(fifelse(
    Security_AssetClass == "Bar" & Account_Type %in% checking_account_types,
    Position_ValueCHF, 0), na.rm = TRUE),
  Client_ValueSavings = sum(fifelse(
    Security_AssetClass == "Bar" & Account_Type %in% savings_account_types,
    Position_ValueCHF, 0), na.rm = TRUE),
  Client_ValueRetSavings = sum(fifelse(
    ret_account(Account_Type, Account_SecuritiesAccountType),
    Position_ValueCHF, 0), na.rm = TRUE),
  Client_ValueSecurities = sum(fifelse(
    !Security_AssetClass %in% c("Bar", "Kredit") &
      !ret_account(Account_Type, Account_SecuritiesAccountType),
    Position_ValueCHF, 0), na.rm = TRUE),
  Client_ValueStocks = sum(fifelse(
    Security_AssetClass == "Aktien" &
      !ret_account(Account_Type, Account_SecuritiesAccountType),
    Position_ValueCHF, 0), na.rm = TRUE),
  Client_ValueEquityMF = sum(fifelse(
    Security_AssetClass == "Fonds" & Security_FundType == "Fund - Shares (09)" &
      !ret_account(Account_Type, Account_SecuritiesAccountType),
    Position_ValueCHF, 0), na.rm = TRUE),
  Client_ValueMortgage = sum(fifelse(
    Security_AssetClass == "Kredit" & Account_Type %in% mortgage_account_types,
    Position_ValueCHF, 0), na.rm = TRUE),
  Client_ValueLoan = sum(fifelse(
    Security_AssetClass == "Kredit" & Account_Type %in% loan_account_types,
    Position_ValueCHF, 0), na.rm = TRUE)
), by = .(Client_Nr, Date)]

# Portfolio weights (no grouping needed — values already broadcast to all rows)
portfolio[, `:=`(
  Client_WeightChecking   = Client_ValueChecking   / Client_BankWealth,
  Client_WeightSavings    = Client_ValueSavings    / Client_BankWealth,
  Client_WeightRetSavings = Client_ValueRetSavings / Client_BankWealth,
  Client_WeightSecurities = Client_ValueSecurities / Client_BankWealth,
  Client_WeightStocks     = Client_ValueStocks     / Client_BankWealth,
  Client_WeightEquityMF   = Client_ValueEquityMF   / Client_BankWealth
)]

keep_cols <- c(
  "Client_Nr", "Date", "Month", "Account_Nr", "Account_Type", "Account_SecuritiesAccountType",
  "Advisor_Nr", "Position_Nr", "Security_Nr", "Security_ISIN", "Security_Name",
  "Security_AssetClass", "Security_FundType", "Position_Amount", "Position_ValueCHF",
  "Client_BankWealth",
  "Client_ValueChecking", "Client_WeightChecking",
  "Client_ValueSavings", "Client_WeightSavings",
  "Client_ValueRetSavings", "Client_WeightRetSavings",
  "Client_ValueSecurities", "Client_WeightSecurities",
  "Client_ValueStocks", "Client_WeightStocks",
  "Client_ValueEquityMF", "Client_WeightEquityMF",
  "Client_ValueMortgage", "Client_ValueLoan"
)
portfolio <- portfolio[, ..keep_cols]
setorder(portfolio, Client_Nr, Date, Position_Nr)

saveRDS(portfolio, file.path(data_dir, "Build/Data/PortfolioHoldings.rds"))


# Prepare for master: first observation per client-month
setorder(portfolio, Client_Nr, Month, Date, Position_Nr)
portfolio_master <- unique(portfolio, by = c("Client_Nr", "Month"))

keep_master <- c(
  "Client_Nr", "Date", "Month",
  "Client_BankWealth",
  "Client_ValueChecking", "Client_WeightChecking",
  "Client_ValueSavings", "Client_WeightSavings",
  "Client_ValueRetSavings", "Client_WeightRetSavings",
  "Client_ValueSecurities", "Client_WeightSecurities",
  "Client_ValueStocks", "Client_WeightStocks",
  "Client_ValueEquityMF", "Client_WeightEquityMF",
  "Client_ValueMortgage", "Client_ValueLoan"
)
portfolio_master <- portfolio_master[, ..keep_master]
setorder(portfolio_master, Client_Nr, Month)

saveRDS(portfolio_master, file.path(data_dir, "Build/Data/PortfolioHoldingsForMaster.rds"))


#------------------------------------------------------------------------------
# (2) Securities transactions dataset
#------------------------------------------------------------------------------

# Security characteristics
security_chars <- as.data.table(read_dta(
  file.path(data_dir, "Source/Original Data/T31_Asset_Stammdaten.dta")
))
setnames(security_chars,
  old = c("ISIN_Key", "Asset_ID", "Asset", "Instrumentengruppe", "Fondsart"),
  new = c("Security_ISIN", "Security_Nr", "Security_Name", "Security_AssetClass", "Security_FundType")
)
security_chars[, Security_ISIN       := as.character(as_factor(Security_ISIN))]
security_chars[, Security_AssetClass := as.character(as_factor(Security_AssetClass))]
security_chars[, Security_FundType   := as.character(as_factor(Security_FundType))]
security_chars <- security_chars[, .(Security_Nr, Security_ISIN, Security_Name,
                                     Security_AssetClass, Security_FundType)]
setorder(security_chars, Security_Nr)

saveRDS(security_chars, file.path(data_dir, "Build/Data/SecurityCharacteristics.rds"))


# Account type lookup (first row per account-month from portfolio holdings)
setorder(portfolio, Account_Nr, Month)
account_type <- unique(
  portfolio[, .(Account_Nr, Month, Account_Type, Account_SecuritiesAccountType)],
  by = c("Account_Nr", "Month")
)
setorder(account_type, Account_Nr, Month)

saveRDS(account_type, file.path(data_dir, "Build/Data/AccountType.rds"))


# Exchange rates (source: Refinitiv Workspace)
exchange_raw <- as.data.table(read_excel(
  file.path(data_dir, "Source/Original Data/Exchange Rates.xlsx"),
  col_names = TRUE
))
setnames(exchange_raw, "A", "Date")
exchange_raw[, Date := as.Date(Date)]

currency_map <- c(
  EUROTOCHF            = "EUR", USTOCHF               = "USD",
  JAPANESEYENTOCHF     = "JPY", UKTOCHF               = "GBP",
  AUSTRALIANTOCHF      = "AUD", BRAZILIANREALTOCHF    = "BRL",
  CANADIANTOCHF        = "CAD", CHINESEYUANTOCHF      = "CNY",
  CZECHKORUNATOCHF     = "CZK", DANISHKRONETOCHF      = "DKK",
  HONGKONGTOCHF        = "HKD", CROATIANKUNATOCHF     = "HRK",
  HUNGARIANFORINTTOCHF = "HUF", ICELANDICKRONATOCHF   = "ISK",
  MEXICANPESOTOCHF     = "MXN", NORWEGIANKRONETOCHF   = "NOK",
  NEWZEALANDTOCHF      = "NZD", POLISHZLOTYTOCHF      = "PLN",
  NEWROMANIANLEUTOCHF  = "RON", CISROUBLEMARKETTOCHF  = "RUB",
  SWEDISHKRONATOCHF    = "SEK", SINGAPORETOCHF        = "SGD",
  NEWTURKISHLIRATOCHF  = "TRY", SOUTHAFRICARANDTOCHF  = "ZAR"
)

fx_cols <- names(currency_map)
exchange_raw[, (fx_cols) := lapply(.SD, function(x) 1 / x), .SDcols = fx_cols]

exchange_rates <- melt(
  exchange_raw,
  id.vars       = "Date",
  measure.vars  = fx_cols,
  variable.name = "Currency_Col",
  value.name    = "Currency_ExchangeRate"
)
exchange_rates[, Trade_Currency := currency_map[as.character(Currency_Col)]]
exchange_rates[, Currency_Col   := NULL]
setorder(exchange_rates, Trade_Currency, Date)

saveRDS(exchange_rates, file.path(data_dir, "Build/Data/ExchangeRates.rds"))


# Securities transactions
buy_types <- c(
  "!Kauf", "!Kauf Ausübung (aktiv)", "!Kauf Zession (passiv)",
  "Kauf", "Kauf (Closing)", "Kauf (Closing) CT", "Kauf (Opening)",
  "Kauf (Opening) CT", "Kauf CT", "Zeichnung"
)
sell_types <- c(
  "!Sell Assignment", "!Verkauf", "Verkauf",
  "Verkauf (Closing)", "Verkauf (Opening)", "Verkauf CT"
)

transactions <- as.data.table(read_dta(
  file.path(data_dir, "Source/Original Data/T40_Bkg_Boerse.dta")
))
transactions[, c("Konto_Pos_ID", "Titel_Pos_ID", "Menge", "Kurs_Ausfuehrung", "Kosten",
                  "Order_Typisierung", "Medium", "Boersenplatz") := NULL]
setnames(transactions,
  old = c("Doc_ID", "Bp_ID", "Cont_ID", "Asset_ID", "Bruttowert", "ISIN_Key",
          "Order_Type", "Kontowaehrung", "Handelswaehrung"),
  new = c("Trade_Nr", "Client_Nr", "Account_Nr", "Security_Nr", "Trade_Value",
          "Security_ISIN", "Trade_Type", "Account_Currency", "Trade_Currency")
)
transactions[, Date                := as.Date(substr(as.character(Ausfuehrungszeit_DT), 1, 11),
                                              format = "%m/%d/%Y")]
transactions[, Month               := floor_date(Date, "month")]
transactions[, Ausfuehrungszeit_DT := NULL]
transactions <- transactions[Month <= as.Date("2021-06-01")]

transactions[, Security_ISIN    := as.character(as_factor(Security_ISIN))]
transactions[, Trade_Type       := as.character(as_factor(Trade_Type))]
transactions[, Account_Currency := as.character(as_factor(Account_Currency))]
transactions[, Trade_Currency   := as.character(as_factor(Trade_Currency))]

transactions[, Trade_ExchangeRate := fifelse(
  Account_Currency == "CHF", Netto_in_Konto_Waehrung / Nettowert, NA_real_
)]
transactions[, c("Netto_in_Konto_Waehrung", "Nettowert") := NULL]

# Merge with exchange rates then fill CHF and missing rates
transactions <- merge(transactions, exchange_rates, by = c("Trade_Currency", "Date"), all.x = TRUE)
transactions[Trade_Currency == "CHF",       Currency_ExchangeRate := 1]
transactions[is.na(Trade_ExchangeRate),     Trade_ExchangeRate    := Currency_ExchangeRate]
transactions[, Trade_ValueCHF        := Trade_Value * Trade_ExchangeRate]
transactions[, Currency_ExchangeRate := NULL]

# Buy/sell classification and sign convention (buys positive, sells negative)
transactions[, Trade_BuySell := fcase(
  Trade_Type %in% buy_types,  "Buy",
  Trade_Type %in% sell_types, "Sell",
  default = NA_character_
)]
transactions[, Trade_Value    := -Trade_Value]
transactions[, Trade_ValueCHF := -Trade_ValueCHF]

# Merge with account type and security characteristics
transactions <- merge(transactions, account_type,   by = c("Account_Nr", "Month"), all.x = TRUE)
transactions <- merge(transactions, security_chars, by = "Security_Nr",            all.x = TRUE)

# All monthly trade counts and volumes in a single grouped pass
transactions[, `:=`(
  Client_NumberTradesPerM          = .N,
  Client_NumberStockTradesPerM     = sum(
    Security_AssetClass == "Aktien" & !ret_account(Account_Type, Account_SecuritiesAccountType),
    na.rm = TRUE),
  Client_NumberStockBuysPerM       = sum(
    Security_AssetClass == "Aktien" & Trade_BuySell == "Buy" &
      !ret_account(Account_Type, Account_SecuritiesAccountType),
    na.rm = TRUE),
  Client_NumberStockSellsPerM      = sum(
    Security_AssetClass == "Aktien" & Trade_BuySell == "Sell" &
      !ret_account(Account_Type, Account_SecuritiesAccountType),
    na.rm = TRUE),
  Client_NumberEquityMFTradesPerM  = sum(
    Security_AssetClass == "Fonds" & Security_FundType == "Fund - Shares (09)" &
      !ret_account(Account_Type, Account_SecuritiesAccountType),
    na.rm = TRUE),
  Client_NumberEquityMFBuysPerM    = sum(
    Security_AssetClass == "Fonds" & Security_FundType == "Fund - Shares (09)" &
      Trade_BuySell == "Buy" & !ret_account(Account_Type, Account_SecuritiesAccountType),
    na.rm = TRUE),
  Client_NumberEquityMFSellsPerM   = sum(
    Security_AssetClass == "Fonds" & Security_FundType == "Fund - Shares (09)" &
      Trade_BuySell == "Sell" & !ret_account(Account_Type, Account_SecuritiesAccountType),
    na.rm = TRUE),
  Client_TradingVolumePerM         = sum(abs(Trade_ValueCHF), na.rm = TRUE),
  Client_TradingVolumeStocksPerM   = sum(fifelse(
    Security_AssetClass == "Aktien" & !ret_account(Account_Type, Account_SecuritiesAccountType),
    abs(Trade_ValueCHF), 0), na.rm = TRUE),
  Client_TradingVolumeEquityMFPerM = sum(fifelse(
    Security_AssetClass == "Fonds" & Security_FundType == "Fund - Shares (09)" &
      !ret_account(Account_Type, Account_SecuritiesAccountType),
    abs(Trade_ValueCHF), 0), na.rm = TRUE),
  Client_NetInvestmentPerM         = sum(Trade_ValueCHF, na.rm = TRUE),
  Client_NetInvestmentStocksPerM   = sum(fifelse(
    Security_AssetClass == "Aktien" & !ret_account(Account_Type, Account_SecuritiesAccountType),
    Trade_ValueCHF, 0), na.rm = TRUE),
  Client_NetInvestmentEquityMFPerM = sum(fifelse(
    Security_AssetClass == "Fonds" & Security_FundType == "Fund - Shares (09)" &
      !ret_account(Account_Type, Account_SecuritiesAccountType),
    Trade_ValueCHF, 0), na.rm = TRUE)
), by = .(Client_Nr, Month)]

# All daily trade counts in a single grouped pass
transactions[, `:=`(
  Client_NumberTradesPerD         = .N,
  Client_NumberStockTradesPerD    = sum(
    Security_AssetClass == "Aktien" & !ret_account(Account_Type, Account_SecuritiesAccountType),
    na.rm = TRUE),
  Client_NumberStockBuysPerD      = sum(
    Security_AssetClass == "Aktien" & Trade_BuySell == "Buy" &
      !ret_account(Account_Type, Account_SecuritiesAccountType),
    na.rm = TRUE),
  Client_NumberStockSellsPerD     = sum(
    Security_AssetClass == "Aktien" & Trade_BuySell == "Sell" &
      !ret_account(Account_Type, Account_SecuritiesAccountType),
    na.rm = TRUE),
  Client_NumberEquityMFTradesPerD = sum(
    Security_AssetClass == "Fonds" & Security_FundType == "Fund - Shares (09)" &
      !ret_account(Account_Type, Account_SecuritiesAccountType),
    na.rm = TRUE),
  Client_NumberEquityMFBuysPerD   = sum(
    Security_AssetClass == "Fonds" & Security_FundType == "Fund - Shares (09)" &
      Trade_BuySell == "Buy" & !ret_account(Account_Type, Account_SecuritiesAccountType),
    na.rm = TRUE),
  Client_NumberEquityMFSellsPerD  = sum(
    Security_AssetClass == "Fonds" & Security_FundType == "Fund - Shares (09)" &
      Trade_BuySell == "Sell" & !ret_account(Account_Type, Account_SecuritiesAccountType),
    na.rm = TRUE)
), by = .(Client_Nr, Date)]

keep_trans <- c(
  "Client_Nr", "Date", "Month", "Account_Nr", "Account_Type", "Account_SecuritiesAccountType",
  "Trade_Nr", "Security_Nr", "Security_ISIN", "Security_Name",
  "Security_AssetClass", "Security_FundType",
  "Trade_BuySell", "Trade_ValueCHF", "Trade_Currency", "Account_Currency", "Trade_Type",
  "Client_NumberTradesPerM", "Client_NumberTradesPerD",
  "Client_NumberStockTradesPerM", "Client_NumberStockTradesPerD",
  "Client_NumberStockBuysPerM", "Client_NumberStockBuysPerD",
  "Client_NumberStockSellsPerM", "Client_NumberStockSellsPerD",
  "Client_NumberEquityMFTradesPerM", "Client_NumberEquityMFTradesPerD",
  "Client_NumberEquityMFBuysPerM", "Client_NumberEquityMFBuysPerD",
  "Client_NumberEquityMFSellsPerM", "Client_NumberEquityMFSellsPerD",
  "Client_TradingVolumePerM", "Client_TradingVolumeStocksPerM", "Client_TradingVolumeEquityMFPerM",
  "Client_NetInvestmentPerM", "Client_NetInvestmentStocksPerM", "Client_NetInvestmentEquityMFPerM"
)
transactions <- transactions[, ..keep_trans]
setorder(transactions, Client_Nr, Date, Trade_Nr)

saveRDS(transactions, file.path(data_dir, "Build/Data/SecuritiesTransactions.rds"))


# Prepare for master: first observation per client-month
setorder(transactions, Client_Nr, Month, Date, Trade_Nr)
transactions_master <- unique(transactions, by = c("Client_Nr", "Month"))
transactions_master[, Date := ceiling_date(Date, "month") - days(1L)]

keep_trans_master <- c(
  "Client_Nr", "Date", "Month",
  "Client_NumberTradesPerM",
  "Client_NumberStockTradesPerM", "Client_NumberStockBuysPerM", "Client_NumberStockSellsPerM",
  "Client_NumberEquityMFTradesPerM", "Client_NumberEquityMFBuysPerM", "Client_NumberEquityMFSellsPerM",
  "Client_TradingVolumePerM", "Client_TradingVolumeStocksPerM", "Client_TradingVolumeEquityMFPerM",
  "Client_NetInvestmentPerM", "Client_NetInvestmentStocksPerM", "Client_NetInvestmentEquityMFPerM"
)
transactions_master <- transactions_master[, ..keep_trans_master]
setorder(transactions_master, Client_Nr, Month)

saveRDS(transactions_master, file.path(data_dir, "Build/Data/SecuritiesTransactionsForMaster.rds"))


# Daily transaction counts: first observation per client-day
setorder(transactions, Client_Nr, Date, Security_Nr, Trade_Nr)
transactions_daily <- unique(transactions, by = c("Client_Nr", "Date"))

keep_trans_daily <- c(
  "Client_Nr", "Date",
  "Client_NumberTradesPerD",
  "Client_NumberStockTradesPerD", "Client_NumberStockBuysPerD", "Client_NumberStockSellsPerD",
  "Client_NumberEquityMFTradesPerD", "Client_NumberEquityMFBuysPerD", "Client_NumberEquityMFSellsPerD"
)
transactions_daily <- transactions_daily[, ..keep_trans_daily]
setorder(transactions_daily, Client_Nr, Date)

saveRDS(transactions_daily, file.path(data_dir, "Build/Data/SecuritiesTransactionsPerDay.rds"))


#------------------------------------------------------------------------------
# (3) Client-advisor contacts dataset
#------------------------------------------------------------------------------

contacts <- as.data.table(read_dta(
  file.path(data_dir, "Source/Original Data/T20_Kundenkontakte.dta")
))
setnames(contacts,
  old = c("Bp_ID", "Crm_Issue_ID", "K_Physisch", "K_Anlegen", "K_Art", "K_Aufnahme"),
  new = c("Client_Nr", "Contact_Nr", "Contact_InPerson", "Contact_Investment",
          "Contact_Type", "Contact_Initiation")
)
contacts[, Date               := as.Date(as.character(as_factor(Kontakt_DT)), format = "%Y%m%d")]
contacts[, Month              := floor_date(Date, "month")]
contacts[, Contact_Type       := as.character(as_factor(Contact_Type))]
contacts[, Contact_Initiation := as.character(as_factor(Contact_Initiation))]
contacts[, Kontakt_DT         := NULL]

contacts[, `:=`(
  Client_NumberOfContactsPerM    = .N,
  Client_NumberOfInPersonPerM    = sum(Contact_InPerson,                             na.rm = TRUE),
  Client_NumberOfCallsPerM       = sum(Contact_Type       == "Telefonkontakt",       na.rm = TRUE),
  Client_NumberOfInvestPerM      = sum(Contact_Investment == 1L,                     na.rm = TRUE),
  Client_NumberOfAdvisorInitPerM = sum(Contact_Initiation == "Durch Kundenberater",  na.rm = TRUE),
  Client_NumberOfClientInitPerM  = sum(Contact_Initiation == "Durch Kunde",          na.rm = TRUE)
), by = .(Client_Nr, Month)]

contacts <- contacts[, .(
  Client_Nr, Date, Month,
  Contact_Nr, Contact_Type, Contact_Initiation, Contact_InPerson, Contact_Investment,
  Client_NumberOfContactsPerM, Client_NumberOfInPersonPerM, Client_NumberOfCallsPerM,
  Client_NumberOfInvestPerM, Client_NumberOfAdvisorInitPerM, Client_NumberOfClientInitPerM
)]
setorder(contacts, Client_Nr, Date, Contact_Nr)

saveRDS(contacts, file.path(data_dir, "Build/Data/Contacts.rds"))


# Prepare for master: first observation per client-month
setorder(contacts, Client_Nr, Month, Date, Contact_Nr)
contacts_master <- unique(contacts, by = c("Client_Nr", "Month"))
contacts_master[, Date := ceiling_date(Date, "month") - days(1L)]

contacts_master <- contacts_master[, .(
  Client_Nr, Date, Month,
  Client_NumberOfContactsPerM, Client_NumberOfInPersonPerM, Client_NumberOfCallsPerM,
  Client_NumberOfInvestPerM, Client_NumberOfAdvisorInitPerM, Client_NumberOfClientInitPerM
)]
setorder(contacts_master, Client_Nr, Month)

saveRDS(contacts_master, file.path(data_dir, "Build/Data/ContactsForMaster.rds"))


#------------------------------------------------------------------------------
# (4) Client characteristics dataset
#------------------------------------------------------------------------------

client_chars <- as.data.table(read_dta(
  file.path(data_dir, "Source/Original Data/T01_Bp_Snapshot.dta")
))
setnames(client_chars,
  old = c("Bp_ID", "Geburtsjahr", "Selektionsgrund_Name"),
  new = c("Client_Nr", "Client_YearOfBirth", "Client_WhyInData")
)
client_chars[, Client_WhyInData := as.character(as_factor(Client_WhyInData))]
client_chars <- client_chars[, .(Client_Nr, Client_WhyInData, Client_YearOfBirth)]
setorder(client_chars, Client_Nr)

saveRDS(client_chars, file.path(data_dir, "Build/Data/ClientCharacteristics.rds"))


#------------------------------------------------------------------------------
# (5) Master dataset
#------------------------------------------------------------------------------

master <- as.data.table(read_dta(
  file.path(data_dir, "Source/Original Data/T02_Bp_Zeitreihe.dta")
))
setnames(master,
  old = c("Bp_ID", "Hauptbetreuer_ID", "Wohnland"),
  new = c("Client_Nr", "Advisor_Nr", "Client_Country")
)
master[, Month     := parse_period_id(Period_ID)]
master[, Date      := ceiling_date(Month, "month") - days(1L)]
master[, Year      := year(Date)]
master[, Period_ID := NULL]
master <- master[Month >= as.Date("2010-01-01") & Month <= as.Date("2021-06-01")]

master[, Client_Country            := as.character(as_factor(Client_Country))]
master[, Client_LivesInSwitzerland := fcase(
  Client_Country == "CH",  1,
  Client_Country == "n/a", NA_real_,
  default = 0
)]

# Merge with client characteristics
master <- merge(master, client_chars, by = "Client_Nr", all.x = TRUE)
master[Client_YearOfBirth %in% c(1800L, 1884L, 1885L, 2999L, 9999L), Client_YearOfBirth := NA_integer_]
master[, Client_Age := Year - Client_YearOfBirth]

# Merge with portfolio holdings
master <- merge(master, portfolio_master, by = c("Client_Nr", "Month"), all.x = TRUE)
wealth_cols <- c(
  "Client_BankWealth", "Client_ValueChecking", "Client_ValueSavings",
  "Client_ValueRetSavings", "Client_ValueSecurities", "Client_ValueStocks",
  "Client_ValueEquityMF", "Client_ValueMortgage", "Client_ValueLoan"
)
for (col in wealth_cols) master[is.na(get(col)), (col) := 0]

# Merge with securities transactions (drop Date to avoid conflict)
master <- merge(master, transactions_master[, !"Date"], by = c("Client_Nr", "Month"), all.x = TRUE)
trade_cols <- c(
  "Client_NumberTradesPerM",
  "Client_NumberStockTradesPerM", "Client_NumberStockBuysPerM", "Client_NumberStockSellsPerM",
  "Client_NumberEquityMFTradesPerM", "Client_NumberEquityMFBuysPerM", "Client_NumberEquityMFSellsPerM",
  "Client_TradingVolumePerM", "Client_TradingVolumeStocksPerM", "Client_TradingVolumeEquityMFPerM",
  "Client_NetInvestmentPerM", "Client_NetInvestmentStocksPerM", "Client_NetInvestmentEquityMFPerM"
)
for (col in trade_cols) master[is.na(get(col)), (col) := 0]

# Merge with client-advisor contacts (drop Date to avoid conflict)
master <- merge(master, contacts_master[, !"Date"], by = c("Client_Nr", "Month"), all.x = TRUE)
contact_cols <- c(
  "Client_NumberOfContactsPerM", "Client_NumberOfInPersonPerM", "Client_NumberOfCallsPerM",
  "Client_NumberOfInvestPerM", "Client_NumberOfAdvisorInitPerM", "Client_NumberOfClientInitPerM"
)
for (col in contact_cols) master[is.na(get(col)), (col) := 0]

keep_master_final <- c(
  "Client_Nr", "Date", "Month", "Year", "Client_WhyInData",
  "Client_YearOfBirth", "Client_Age", "Client_Country", "Client_LivesInSwitzerland",
  "Client_BankWealth",
  "Client_ValueChecking", "Client_WeightChecking",
  "Client_ValueSavings", "Client_WeightSavings",
  "Client_ValueRetSavings", "Client_WeightRetSavings",
  "Client_ValueSecurities", "Client_WeightSecurities",
  "Client_ValueStocks", "Client_WeightStocks",
  "Client_ValueEquityMF", "Client_WeightEquityMF",
  "Client_ValueMortgage", "Client_ValueLoan",
  "Client_NumberTradesPerM",
  "Client_NumberStockTradesPerM", "Client_NumberStockBuysPerM", "Client_NumberStockSellsPerM",
  "Client_NumberEquityMFTradesPerM", "Client_NumberEquityMFBuysPerM", "Client_NumberEquityMFSellsPerM",
  "Client_TradingVolumePerM", "Client_TradingVolumeStocksPerM", "Client_TradingVolumeEquityMFPerM",
  "Client_NetInvestmentPerM", "Client_NetInvestmentStocksPerM", "Client_NetInvestmentEquityMFPerM",
  "Client_NumberOfContactsPerM", "Client_NumberOfInPersonPerM", "Client_NumberOfCallsPerM",
  "Client_NumberOfInvestPerM", "Client_NumberOfAdvisorInitPerM", "Client_NumberOfClientInitPerM"
)
master <- master[, ..keep_master_final]
setorder(master, Client_Nr, Month)

saveRDS(master, file.path(data_dir, "Build/Analysis/Master.rds"))
