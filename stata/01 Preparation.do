set more off
  
* Directory containing the data (--> adjust this line)
* Nic
*global DataDir "C:/Users/nic.schaub/Dropbox/Financial Advice in Crisis Periods/Empirical Analysis/" // office
global DataDir "C:/Users/FWehmann/Dropbox/Financial Advice in Crisis Periods/Empirical Analysis/"

* T01_Bp_Snapshot: Client characteristics (time-invariant)
* T02_Bp_Zeitreihe: Client characteristics (time varying)
* T05_Cont_Snapshot: Financial advice
* T10_Mitarbeiter_Merkmale: Advisor characteristics
* T20_Kundenkontakte: Client-advisor contacts
* T21_Kundenevents: Events
* T30_Pos_fact: All products (incl. portfolio holdings)
* T31_Asset_Stammdaten: Security characteristics
* T40_Bkg_Boerse: Transactions
* T41_Bkg_ZV: Payments
* T42_Bkg_Diverses: Various
* T50_Person_Person_Relationen_ab_2015: Family ties

*------------------------------------------------------------------------------
*(1) Portfolio holdings dataset
*------------------------------------------------------------------------------

cd "${DataDir}"
use "Source/Original Data/T30_Pos_fact.dta", clear

* Drop varibales not used
drop KassObli_Zert Nebenbetreuer_ID  StruktProd_Klasse Ausgabejahr ///
Faelligkeitsjahr Land_Emittent Asset_Waehrung BilanzpositionGL2 ///
BilanzpositionGL1 Bilanzposition KontoproduktGL2 KontoproduktGL1 ///
Referenzwaehrung Positionswaehrung Forderungen_ggue_Kunden_CHF ///
Hypothekarforderungen_CHF Kundenausleihungen_CHF Kundeneinlagen_CHF ///
Depotvol_Beratungsmandat_CHF Depotvol_Verwaltungsmandat_C ///
Bestand_Treuhandanlagen_CHF AUM_CHF Geschaeftsvolumen_CHF ///
DA_Titelkursabweichung_CHF DA_Devisenkursabweichung_CHF ///
DA_Mengenabweichung_CHF DA_Delta_CHF DA_Gesamtabweichung_CHF Kredit_Limite_CHF

* Rename variables
rename Bp_ID Client_Nr
rename Cont_ID Account_Nr
rename Pos_ID Position_Nr
rename ISIN_Key Security_ISIN
rename Asset_ID Security_Nr
rename Asset Security_Name
rename Kontoprodukt Account_Type
rename Hauptbetreuer_ID Advisor_Nr
rename Depotprodukt Account_SecuritiesAccountType
rename Instrumentengruppe Security_AssetClass 
rename Fondsart Security_FundType
rename Limitenart Account_LimitType
rename Menge Position_Amount
rename Vermoegen_CHF Position_ValueCHF

* Date
tostring Period_ID, replace
gen tmp = date(Period_ID, "YM")
egen Date = eomd(tmp), format(%tdD_m_Y)
drop Period_ID tmp

* Month
gen Month = mofd(Date)
format %tm Month

* Decode variables
decode Security_ISIN, gen(tmp)
drop Security_ISIN
rename tmp Security_ISIN
decode Account_Type, gen(tmp)
drop Account_Type
rename tmp Account_Type
decode Account_SecuritiesAccountType, gen(tmp)
drop Account_SecuritiesAccountType
rename tmp Account_SecuritiesAccountType
decode Security_AssetClass, gen(tmp)
drop Security_AssetClass
rename tmp Security_AssetClass
decode Security_FundType, gen(tmp)
drop Security_FundType
rename tmp Security_FundType

* Drop credit limits
tab Security_AssetClass
drop if Security_AssetClass == "Limite"
tab Account_LimitType
keep if mi(Account_LimitType)

* Drop if asset class not clear
drop if Security_AssetClass == "Nicht zugeteilt (Dummy)"

* Drop if account type not clear
tab Account_Type
*Abwicklungskonto WEF
*Baukonto
*CWO Verwertungskonti
*Callgeld
*Dokumentarinkasso und Akkreditiv
*Edelmetallkonto
*Festgeld -> Bonds
*Fondskonto
*Fondssparkonto
*Grabunterhaltskonto
*Kassenobligation -> Bonds
*Konto 35 D
*Konto 35 D EUR
*PVE-Konto 35 D
*PVE-Konto 35 D EUR
*Treuhand Call -> Bonds
*Treuhand Fest -> Bonds
*Vermittlung
*Vermögensverwaltungskonto
*Wiederanlagekonto
drop if Account_Type == "Abwicklungskonto WEF" ///
| Account_Type == "Baukonto" ///
| Account_Type == "CWO Verwertungskonti" ///
| Account_Type == "Callgeld" ///
| Account_Type == "Dokumentarinkasso und Akkreditiv" ///
| Account_Type == "Edelmetallkonto" ///
| Account_Type == "Fondskonto" ///
| Account_Type == "Fondssparkonto" ///
| Account_Type == "Grabunterhaltskonto" ///
| Account_Type == "Konto 35 D" ///
| Account_Type == "Konto 35 D EUR" ///
| Account_Type == "PVE-Konto 35 D" ///
| Account_Type == "PVE-Konto 35 D EUR" ///
| Account_Type == "Vermittlung" ///
| Account_Type == "Vermögensverwaltungskonto" ///
| Account_Type == "Wiederanlagekonto"

* Add negative positions
sum Position_ValueCHF, det
replace Position_ValueCHF = Position_Amount if Position_ValueCHF == 0 & Position_Amount ~= 0
sum Position_ValueCHF, det

* Bank wealth
sort Client_Nr Date Position_Nr
by Client_Nr Date: egen Client_BankWealth = total(Position_ValueCHF) if Security_AssetClass ~= "Kredit" ///
& Account_Type ~= "COVID-19-Kredit" ///
& Account_Type ~= "Festdarlehen/Festzinskredit" ///
& Account_Type ~= "Fester Vorschuss" ///
& Account_Type ~= "Festhypothek" ///
& Account_Type ~= "Festhypothek (E-Hypothek)" ///
& Account_Type ~= "Geldmarkt-Hypothek mit CAP" ///
& Account_Type ~= "Geldmarkt-Hypothek ohne CAP" ///
& Account_Type ~= "Investitionsdarlehen" ///
& Account_Type ~= "Risikoprivatkredit" ///
& Account_Type ~= "Rollover-Hypothek" ///
& Account_Type ~= "Rollover-Kredit" /// 
& Account_Type ~= "SARON-Hypothek" ///
& Account_Type ~= "SARON-Kredit" ///
& Account_Type ~= "Variable Hypothek" ///
& Account_Type ~= "Variables Darlehen"
by Client_Nr Date: egen tmp = max(Client_BankWealth)
replace Client_BankWealth = tmp
drop tmp
replace Client_BankWealth = 0 if mi(Client_BankWealth)

* Amount held in checking account
*https://www.sgkb.ch/de/privatkunden/konten-zum-zahlen
*Privatkonto
*Jugendkonto
*Ausbildungskonto
*Konto 25
*SGKB You
*Euro-Privatkonto
*Liegenschaftskonto
*Hypozinskonto
*Depotkonto
*Kontokorrent
*Euro-Kontokorrent
*Fremdwährungskonto
*Vereinskonto
by Client_Nr Date: egen Client_ValueChecking = total(Position_ValueCHF) if Security_AssetClass == "Bar" ///
& (Account_Type == "Ausbildungskonto" ///
| Account_Type == "Depotkonto" ///
| Account_Type == "EURO-Kontokorrent" ///
| Account_Type == "EURO-Privatkonto" ///
| Account_Type == "Fremdwährungs-Kontokorrent" ///
| Account_Type == "Hypozinskonto" ///
| Account_Type == "Jugendkonto" ///
| Account_Type == "Konto 25" ///
| Account_Type == "Kontokorrent" ///
| Account_Type == "Kontokorrent Callgeld-Aufnahme" ///
| Account_Type == "Liegenschaftskonto" ///
| Account_Type == "OerK-Kontokorrent" ///
| Account_Type == "Personalkonto" ///
| Account_Type == "Privatkonto" ///
| Account_Type == "Privatkonto (ab 60)" ///
| Account_Type == "SGKB You" ///
| Account_Type == "USD-Kontokorrent" ///
| Account_Type == "Vereinskonto")
by Client_Nr Date: egen tmp = max(Client_ValueChecking)
replace Client_ValueChecking = tmp
drop tmp
replace Client_ValueChecking = 0 if mi(Client_ValueChecking)

* Amount held in savings account
*https://www.sgkb.ch/de/privatkunden/konten-zum-sparen
*Sparkonto
*Aktionärssparkonto
*Jugendsparkonto
*Geschenksparkonto
*#HäschCash
*Sparen 3 Konto
*Freizügigkeitskonto
*Mieterkautionssparkonto
by Client_Nr Date: egen Client_ValueSavings = total(Position_ValueCHF) if Security_AssetClass == "Bar" ///
& (Account_Type == "Aktionärs-Sparkonto Unica" ///
| Account_Type == "Geschenksparkonto" ///
| Account_Type == "HäschCash" ///
| Account_Type == "Inhabersparkonto" ///
| Account_Type == "Jugendsparkonto/-heft" ///
| Account_Type == "Mieterkautionssparkonto" ///
| Account_Type == "Sparkonto/-heft" ///
| Account_Type == "Sparkonto/-heft (ab 60)" ///
| Account_Type == "Sparplankonto")
by Client_Nr Date: egen tmp = max(Client_ValueSavings)
replace Client_ValueSavings = tmp
drop tmp
replace Client_ValueSavings = 0 if mi(Client_ValueSavings)

* Amount held in retirement account
*https://www.sgkb.ch/de/privatkunden/konten-zum-sparen
*Sparen 3 Konto
tab Account_SecuritiesAccountType
tab Account_SecuritiesAccountType if Account_Type == "Sparen 3-Konto" 
tab Account_Type if Account_SecuritiesAccountType == "Sparen 3-Depot" | Account_SecuritiesAccountType == "Sparen 3-Konto"
tab Security_AssetClass if Account_Type == "Sparen 3-Konto" | Account_SecuritiesAccountType == "Sparen 3-Depot" | Account_SecuritiesAccountType == "Sparen 3-Konto"
by Client_Nr Date: egen Client_ValueRetSavings = total(Position_ValueCHF) if Account_Type == "Sparen 3-Konto" | Account_SecuritiesAccountType == "Sparen 3-Depot" | Account_SecuritiesAccountType == "Sparen 3-Konto"
by Client_Nr Date: egen tmp = max(Client_ValueRetSavings)
replace Client_ValueRetSavings = tmp
drop tmp
replace Client_ValueRetSavings = 0 if mi(Client_ValueRetSavings)

* Amount held in securities portfolio
tab Account_Type
tab Account_Type if Security_ISIN ~= "n/a" // account type is missing für securities
tab Security_AssetClass
tab Security_AssetClass Account_Type
by Client_Nr Date: egen Client_ValueSecurities = total(Position_ValueCHF) if Security_AssetClass ~= "Bar" & Security_AssetClass ~= "Kredit" ///
& Account_Type ~= "Sparen 3-Konto" & Account_SecuritiesAccountType ~= "Sparen 3-Depot" & Account_SecuritiesAccountType ~= "Sparen 3-Konto"
by Client_Nr Date: egen tmp = max(Client_ValueSecurities)
replace Client_ValueSecurities = tmp
drop tmp
replace Client_ValueSecurities = 0 if mi(Client_ValueSecurities)

* Amount invested in stocks
by Client_Nr Date: egen Client_ValueStocks = total(Position_ValueCHF) if Security_AssetClass == "Aktien" ///
& Account_Type ~= "Sparen 3-Konto" & Account_SecuritiesAccountType ~= "Sparen 3-Depot" & Account_SecuritiesAccountType ~= "Sparen 3-Konto"
by Client_Nr Date: egen tmp = max(Client_ValueStocks)
replace Client_ValueStocks = tmp
drop tmp
replace Client_ValueStocks = 0 if mi(Client_ValueStocks)

* Amount invested in equity mutual funds
tab Security_FundType
by Client_Nr Date: egen Client_ValueEquityMF = total(Position_ValueCHF) if Security_AssetClass == "Fonds" & Security_FundType == "Fund - Shares (09)" ///
& Account_Type ~= "Sparen 3-Konto" & Account_SecuritiesAccountType ~= "Sparen 3-Depot" & Account_SecuritiesAccountType ~= "Sparen 3-Konto"
by Client_Nr Date: egen tmp = max(Client_ValueEquityMF)
replace Client_ValueEquityMF = tmp
drop tmp
replace Client_ValueEquityMF = 0 if mi(Client_ValueEquityMF)

* Mortgage amount
by Client_Nr Date: egen Client_ValueMortgage = total(Position_ValueCHF) if Security_AssetClass == "Kredit" ///
& (Account_Type == "Festhypothek" ///
| Account_Type == "Festhypothek (E-Hypothek)" ///
| Account_Type == "Geldmarkt-Hypothek mit CAP" ///
| Account_Type == "Geldmarkt-Hypothek ohne CAP" ///
| Account_Type == "Rollover-Hypothek" ///
| Account_Type == "SARON-Hypothek" ///
| Account_Type == "Variable Hypothek")
by Client_Nr Date: egen tmp = max(Client_ValueMortgage)
replace Client_ValueMortgage = tmp
drop tmp
replace Client_ValueMortgage = 0 if mi(Client_ValueMortgage)

* Loan amount 
by Client_Nr Date: egen Client_ValueLoan = total(Position_ValueCHF) if Security_AssetClass == "Kredit" ///
& (Account_Type == "COVID-19-Kredit" ///
| Account_Type == "Festdarlehen/Festzinskredit" ///
| Account_Type == "Fester Vorschuss" ///
| Account_Type == "Investitionsdarlehen" ///
| Account_Type == "Risikoprivatkredit" ///
| Account_Type == "Rollover-Kredit" ///
| Account_Type == "SARON-Kredit" ///
| Account_Type == "Variables Darlehen")
by Client_Nr Date: egen tmp = max(Client_ValueLoan)
replace Client_ValueLoan = tmp
drop tmp
replace Client_ValueLoan = 0 if mi(Client_ValueLoan)

* Percentage held in checking account
gen Client_WeightChecking = Client_ValueChecking / Client_BankWealth
*replace Client_WeightChecking = 0 if mi(Client_WeightChecking) 
sum Client_WeightChecking, det

* Percentage held in savings account
gen Client_WeightSavings = Client_ValueSavings / Client_BankWealth
*replace Client_WeightSavings = 0 if mi(Client_WeightSavings) 
sum Client_WeightSavings, det

* Percentage held in retirement account
gen Client_WeightRetSavings = Client_ValueRetSavings / Client_BankWealth
*replace Client_WeightRetSavings = 0 if mi(Client_WeightRetSavings) 
sum Client_WeightRetSavings, det

* Percentage held in securities portfolio
gen Client_WeightSecurities = Client_ValueSecurities / Client_BankWealth
*replace Client_WeightSecurities = 0 if mi(Client_WeightSecurities) 
sum Client_WeightSecurities, det

* Percentage invested in stocks
gen Client_WeightStocks = Client_ValueStocks / Client_BankWealth
*replace Client_WeightStocks = 0 if mi(Client_WeightStocks) 
sum Client_WeightStocks, det

* Percentage invested in equity mutual funds
gen Client_WeightEquityMF = Client_ValueEquityMF / Client_BankWealth
*replace Client_WeightEquityMF = 0 if mi(Client_WeightEquityMF) 
sum Client_WeightEquityMF, det

* Drop variables not used
keep Client_Nr Date Month Account_Nr Account_Type Account_SecuritiesAccountType ///
Advisor_Nr ///
Position_Nr Security_Nr Security_ISIN Security_Name ///
Security_AssetClass Security_FundType ///
Position_Amount Position_ValueCHF ///
///
Client_BankWealth ///
Client_ValueChecking Client_WeightChecking ///
Client_ValueSavings Client_WeightSavings ///
Client_ValueRetSavings Client_WeightRetSavings ///
Client_ValueSecurities Client_WeightSecurities ///
Client_ValueStocks Client_WeightStocks ///
Client_ValueEquityMF Client_WeightEquityMF ///
Client_ValueMortgage ///
Client_ValueLoan

* Reorder variables
order Client_Nr Date Month Account_Nr Account_Type Account_SecuritiesAccountType ///
Advisor_Nr ///
Position_Nr Security_Nr Security_ISIN Security_Name ///
Security_AssetClass Security_FundType ///
Position_Amount Position_ValueCHF ///
///
Client_BankWealth ///
Client_ValueChecking Client_WeightChecking ///
Client_ValueSavings Client_WeightSavings ///
Client_ValueRetSavings Client_WeightRetSavings ///
Client_ValueSecurities Client_WeightSecurities ///
Client_ValueStocks Client_WeightStocks ///
Client_ValueEquityMF Client_WeightEquityMF ///
Client_ValueMortgage ///
Client_ValueLoan

* Save
compress
sort Client_Nr Date Position_Nr
save "Build/Data/PortfolioHoldings.dta", replace



* Prepare for master
cd "${DataDir}"
use "Build/Data/PortfolioHoldings.dta", clear

* Keep only one observation per client and month
sort Client_Nr Month Date Position_Nr
by Client_Nr Month: gen tmp = _n
drop if tmp ~= 1
drop tmp

* Drop variables not used
keep Client_Nr Date Month ///
///
Client_BankWealth ///
Client_ValueChecking Client_WeightChecking ///
Client_ValueSavings Client_WeightSavings ///
Client_ValueRetSavings Client_WeightRetSavings ///
Client_ValueSecurities Client_WeightSecurities ///
Client_ValueStocks Client_WeightStocks ///
Client_ValueEquityMF Client_WeightEquityMF ///
Client_ValueMortgage ///
Client_ValueLoan

* Reorder variables
order Client_Nr Date Month ///
///
Client_BankWealth ///
Client_ValueChecking Client_WeightChecking ///
Client_ValueSavings Client_WeightSavings ///
Client_ValueRetSavings Client_WeightRetSavings ///
Client_ValueSecurities Client_WeightSecurities ///
Client_ValueStocks Client_WeightStocks ///
Client_ValueEquityMF Client_WeightEquityMF ///
Client_ValueMortgage ///
Client_ValueLoan

* Save
compress
sort Client_Nr Month
save "Build/Data/PortfolioHoldingsForMaster.dta", replace



*------------------------------------------------------------------------------
*(2) Securities transactions dataset
*------------------------------------------------------------------------------

* Security characteristics
cd "${DataDir}"
use "Source/Original Data/T31_Asset_Stammdaten.dta", clear

* Rename variables
rename ISIN_Key Security_ISIN
rename Asset_ID Security_Nr
rename Asset Security_Name
rename Instrumentengruppe Security_AssetClass 
rename Fondsart Security_FundType

* Decode variables
decode Security_ISIN, gen(tmp)
drop Security_ISIN
rename tmp Security_ISIN
decode Security_AssetClass, gen(tmp)
drop Security_AssetClass
rename tmp Security_AssetClass
decode Security_FundType, gen(tmp)
drop Security_FundType
rename tmp Security_FundType

* Drop variables not used
keep Security_Nr Security_ISIN Security_Name ///
Security_AssetClass Security_FundType

* Reorder variables
order Security_Nr Security_ISIN Security_Name ///
Security_AssetClass Security_FundType

* Save
compress
sort Security_Nr 
save "Build/Data/SecurityCharacteristics.dta", replace



* Account type
cd "${DataDir}"
use "Build/Data/PortfolioHoldings.dta", clear

* Keep only one observation per account and month
sort Account_Nr Month
by Account_Nr Month: gen tmp = _n
keep if tmp == 1
drop tmp

* Drop variables not used
keep Account_Nr Month Account_Type Account_SecuritiesAccountType

* Reorder variables
order Account_Nr Month Account_Type Account_SecuritiesAccountType

* Save
compress
sort Account_Nr Month
save "Build/Data/AccountType.dta", replace



* Exchange rates
* Source: Refinitiv Workspace
clear
cd "${DataDir}"
* Import Excel-file
import excel "Source/Original Data/Exchange Rates.xlsx", firstrow

/*
. tab Trade_Currency

      T40 - |
 Handelswäh |
       rung |
  (Missing: |
     'n/a') |      Freq.     Percent        Cum.
------------+-----------------------------------
        AUD |      8,045        0.53        0.53
        BRL |          1        0.00        0.53
        CAD |      8,751        0.58        1.11
        CHF |    910,674       60.16       61.27
        CNY |         11        0.00       61.27
        CZK |         25        0.00       61.27
        DEM |          1        0.00       61.27
        DKK |        988        0.07       61.34
        EUR |    288,090       19.03       80.37
        GBP |      9,499        0.63       81.00
        HKD |        745        0.05       81.05
        HRK |          2        0.00       81.05
        HUF |         14        0.00       81.05
        ISK |         65        0.00       81.05
        JPY |        784        0.05       81.10
        MXN |         42        0.00       81.11
        NOK |      3,071        0.20       81.31
        NZD |      2,230        0.15       81.46
        PLN |          1        0.00       81.46
        RON |          3        0.00       81.46
        RUB |        108        0.01       81.46
        SEK |      1,093        0.07       81.54
        SGD |         73        0.00       81.54
        TRY |        162        0.01       81.55
        USD |    278,718       18.41       99.96
        XAU |          2        0.00       99.97
        ZAR |        529        0.03      100.00
------------+-----------------------------------
      Total |  1,513,727      100.00
*/

* Rename variables
rename A Date

* Date
format %tdD_m_CY Date

* Rescale variables
replace EUROTOCHF = 1 / EUROTOCHF
replace USTOCHF = 1 / USTOCHF
replace JAPANESEYENTOCHF = 1 / JAPANESEYENTOCHF
replace UKTOCHF = 1 / UKTOCHF
replace AUSTRALIANTOCHF = 1 / AUSTRALIANTOCHF
replace BRAZILIANREALTOCHF = 1 / BRAZILIANREALTOCHF
replace CANADIANTOCHF = 1 / CANADIANTOCHF
replace CHINESEYUANTOCHF = 1 / CHINESEYUANTOCHF
replace CZECHKORUNATOCHF = 1 / CZECHKORUNATOCHF
replace DANISHKRONETOCHF = 1 / DANISHKRONETOCHF
replace HONGKONGTOCHF = 1 / HONGKONGTOCHF
replace CROATIANKUNATOCHF = 1 / CROATIANKUNATOCHF
replace HUNGARIANFORINTTOCHF = 1 / HUNGARIANFORINTTOCHF
replace ICELANDICKRONATOCHF = 1 / ICELANDICKRONATOCHF
replace MEXICANPESOTOCHF = 1 / MEXICANPESOTOCHF
replace NORWEGIANKRONETOCHF = 1 / NORWEGIANKRONETOCHF
replace NEWZEALANDTOCHF = 1 / NEWZEALANDTOCHF
replace POLISHZLOTYTOCHF = 1 / POLISHZLOTYTOCHF
replace NEWROMANIANLEUTOCHF = 1 / NEWROMANIANLEUTOCHF
replace CISROUBLEMARKETTOCHF = 1 / CISROUBLEMARKETTOCHF
replace SWEDISHKRONATOCHF = 1 / SWEDISHKRONATOCHF
replace SINGAPORETOCHF = 1 / SINGAPORETOCHF
replace NEWTURKISHLIRATOCHF = 1 / NEWTURKISHLIRATOCHF
replace SOUTHAFRICARANDTOCHF = 1 / SOUTHAFRICARANDTOCHF

* Reshape
rename EUROTOCHF tmp1
rename USTOCHF tmp2
rename JAPANESEYENTOCHF tmp3
rename UKTOCHF tmp4
rename AUSTRALIANTOCHF tmp5
rename BRAZILIANREALTOCHF tmp6
rename CANADIANTOCHF tmp7
rename CHINESEYUANTOCHF tmp8
rename CZECHKORUNATOCHF tmp9
rename DANISHKRONETOCHF tmp10
rename HONGKONGTOCHF tmp11
rename CROATIANKUNATOCHF tmp12
rename HUNGARIANFORINTTOCHF tmp13
rename ICELANDICKRONATOCHF tmp14
rename MEXICANPESOTOCHF tmp15
rename NORWEGIANKRONETOCHF tmp16
rename NEWZEALANDTOCHF tmp17
rename POLISHZLOTYTOCHF tmp18
rename NEWROMANIANLEUTOCHF tmp19
rename CISROUBLEMARKETTOCHF tmp20
rename SWEDISHKRONATOCHF tmp21
rename SINGAPORETOCHF tmp22
rename NEWTURKISHLIRATOCHF tmp23
rename SOUTHAFRICARANDTOCHF tmp24
reshape long tmp, i(Date) j(Trade_Currency)
gen tmptmp = "EUR" if Trade_Currency == 1
replace tmptmp = "USD" if Trade_Currency == 2
replace tmptmp = "JPY" if Trade_Currency == 3
replace tmptmp = "GBP" if Trade_Currency == 4
replace tmptmp = "AUD" if Trade_Currency == 5
replace tmptmp = "BRL" if Trade_Currency == 6
replace tmptmp = "CAD" if Trade_Currency == 7
replace tmptmp = "CNY" if Trade_Currency == 8
replace tmptmp = "CZK" if Trade_Currency == 9
replace tmptmp = "DKK" if Trade_Currency == 10
replace tmptmp = "HKD" if Trade_Currency == 11
replace tmptmp = "HRK" if Trade_Currency == 12
replace tmptmp = "HUF" if Trade_Currency == 13
replace tmptmp = "ISK" if Trade_Currency == 14
replace tmptmp = "MXN" if Trade_Currency == 15
replace tmptmp = "NOK" if Trade_Currency == 16
replace tmptmp = "NZD" if Trade_Currency == 17
replace tmptmp = "PLN" if Trade_Currency == 18
replace tmptmp = "RON" if Trade_Currency == 19
replace tmptmp = "RUB" if Trade_Currency == 20
replace tmptmp = "SEK" if Trade_Currency == 21
replace tmptmp = "SGD" if Trade_Currency == 22
replace tmptmp = "TRY" if Trade_Currency == 23
replace tmptmp = "ZAR" if Trade_Currency == 24
drop Trade_Currency
rename tmp Currency_ExchangeRate
rename tmptmp Trade_Currency

* Drop variables not used
keep Trade_Currency Date Currency_ExchangeRate

* Reorder variables
order Trade_Currency Date Currency_ExchangeRate

* Save
sort Trade_Currency Date
compress
save "Build/Data/ExchangeRates.dta", replace



* Security transactions
cd "${DataDir}"
use "Source/Original Data/T40_Bkg_Boerse.dta", clear

* Drop variables not used
drop Konto_Pos_ID Titel_Pos_ID Menge Kurs_Ausfuehrung Kosten ///
Order_Typisierung Medium Boersenplatz

* Rename variables
rename Doc_ID Trade_Nr
rename Bp_ID Client_Nr
rename Cont_ID Account_Nr
rename Asset_ID Security_Nr
rename Bruttowert Trade_Value
rename ISIN_Key Security_ISIN
rename Order_Type Trade_Type
rename Kontowaehrung Account_Currency
rename Handelswaehrung Trade_Currency

* Date
gen tmp = substr(Ausfuehrungszeit_DT, 1, 11)
gen Date = date(tmp, "MDY")
format %tdD_m_Y Date
drop Ausfuehrungszeit_DT tmp

* Month
gen Month = mofd(Date)
format %tm Month

* Drop dates that are not meaningful
tab Month
drop if Month > ym(2021,6)
tab Month

* Decode variables
decode Security_ISIN, gen(tmp)
drop Security_ISIN
rename tmp Security_ISIN
decode Trade_Type, gen(tmp)
drop Trade_Type
rename tmp Trade_Type
decode Account_Currency, gen(tmp)
drop Account_Currency
rename tmp Account_Currency
decode Trade_Currency, gen(tmp)
drop Trade_Currency
rename tmp Trade_Currency

* Exchange rates of trade
gen Trade_ExchangeRate = Netto_in_Konto_Waehrung / Nettowert if Account_Currency == "CHF"
drop Netto_in_Konto_Waehrung Nettowert

* Merge with exchange rates
merge m:1 Trade_Currency Date using "Build/Data/ExchangeRates.dta"
drop if _merge == 2
tab Trade_Currency if _merge == 1
drop _merge
replace Currency_ExchangeRate = 1 if Trade_Currency == "CHF"

* Add missing exchange rates of trades
replace Trade_ExchangeRate = Currency_ExchangeRate if mi(Trade_ExchangeRate)

* Trade value in Swiss Franc
gen Trade_ValueCHF = Trade_Value * Trade_ExchangeRate

* Buy/sell
tab Trade_Type
gen Trade_BuySell = "Buy" if Trade_Type == "!Kauf" ///
| Trade_Type == "!Kauf Ausübung (aktiv)" ///
| Trade_Type == "!Kauf Zession (passiv)" ///
| Trade_Type == "Kauf" ///
| Trade_Type == "Kauf (Closing)" ///
| Trade_Type == "Kauf (Closing) CT" ///
| Trade_Type == "Kauf (Opening)" ///
| Trade_Type == "Kauf (Opening) CT" ///
| Trade_Type == "Kauf CT" ///
| Trade_Type == "Zeichnung"
replace Trade_BuySell = "Sell" if Trade_Type == "!Sell Assignment" ///
| Trade_Type == "!Verkauf" ///
| Trade_Type == "Verkauf" ///
| Trade_Type == "Verkauf (Closing)" ///
| Trade_Type == "Verkauf (Opening)" ///
| Trade_Type == "Verkauf CT"
tab Trade_BuySell

*| Trade_Type == "Rückzahlung" ///

* Buys should be positive, sells should be negative
replace Trade_Value = -Trade_Value
replace Trade_ValueCHF = -Trade_ValueCHF

* Merge with account type
merge m:1 Account_Nr Month using "Build/Data/AccountType.dta"
drop if _merge == 2
drop _merge

* Merge with security characteristics
merge m:1 Security_Nr using "Build/Data/SecurityCharacteristics.dta"
drop if _merge == 2
drop _merge

* Number of trades in securities per month
tab Security_AssetClass
sort Client_Nr Month Date Security_Nr
by Client_Nr Month: egen Client_NumberTradesPerM = count(Date)

* Number of trades in securities per day
sort Client_Nr Date Security_Nr
by Client_Nr Date: egen Client_NumberTradesPerD = count(Date)

* Number of trades in stocks per month
sort Client_Nr Month Date Security_Nr
by Client_Nr Month: egen Client_NumberStockTradesPerM = count(Date) ///
if Security_AssetClass == "Aktien" ///
& Account_Type ~= "Sparen 3-Konto" & Account_SecuritiesAccountType ~= "Sparen 3-Depot" & Account_SecuritiesAccountType ~= "Sparen 3-Konto"
by Client_Nr Month: egen tmp = max(Client_NumberStockTradesPerM)
replace Client_NumberStockTradesPerM = tmp
drop tmp
replace Client_NumberStockTradesPerM = 0 if mi(Client_NumberStockTradesPerM)

* Number of trades in stocks per day
sort Client_Nr Date Security_Nr
by Client_Nr Date: egen Client_NumberStockTradesPerD = count(Date) ///
if Security_AssetClass == "Aktien" ///
& Account_Type ~= "Sparen 3-Konto" & Account_SecuritiesAccountType ~= "Sparen 3-Depot" & Account_SecuritiesAccountType ~= "Sparen 3-Konto"
by Client_Nr Date: egen tmp = max(Client_NumberStockTradesPerD)
replace Client_NumberStockTradesPerD = tmp
drop tmp
replace Client_NumberStockTradesPerD = 0 if mi(Client_NumberStockTradesPerD)

* Number of buys in stocks per month
sort Client_Nr Month Date Security_Nr
by Client_Nr Month: egen Client_NumberStockBuysPerM = count(Date) ///
if Security_AssetClass == "Aktien" ///
& Trade_BuySell == "Buy" ///
& Account_Type ~= "Sparen 3-Konto" & Account_SecuritiesAccountType ~= "Sparen 3-Depot" & Account_SecuritiesAccountType ~= "Sparen 3-Konto"
by Client_Nr Month: egen tmp = max(Client_NumberStockBuysPerM)
replace Client_NumberStockBuysPerM = tmp
drop tmp
replace Client_NumberStockBuysPerM = 0 if mi(Client_NumberStockBuysPerM)

* Number of buys in stocks per day
sort Client_Nr Date Security_Nr
by Client_Nr Date: egen Client_NumberStockBuysPerD = count(Date) ///
if Security_AssetClass == "Aktien" ///
& Trade_BuySell == "Buy" ///
& Account_Type ~= "Sparen 3-Konto" & Account_SecuritiesAccountType ~= "Sparen 3-Depot" & Account_SecuritiesAccountType ~= "Sparen 3-Konto"
by Client_Nr Date: egen tmp = max(Client_NumberStockBuysPerD)
replace Client_NumberStockBuysPerD = tmp
drop tmp
replace Client_NumberStockBuysPerD = 0 if mi(Client_NumberStockBuysPerD)

* Number of sells in stocks per month
sort Client_Nr Month Date Security_Nr
by Client_Nr Month: egen Client_NumberStockSellsPerM = count(Date) ///
if Security_AssetClass == "Aktien" ///
& Trade_BuySell == "Sell" ///
& Account_Type ~= "Sparen 3-Konto" & Account_SecuritiesAccountType ~= "Sparen 3-Depot" & Account_SecuritiesAccountType ~= "Sparen 3-Konto"
by Client_Nr Month: egen tmp = max(Client_NumberStockSellsPerM)
replace Client_NumberStockSellsPerM = tmp
drop tmp
replace Client_NumberStockSellsPerM = 0 if mi(Client_NumberStockSellsPerM)

* Number of sells in stocks per day
sort Client_Nr Date Security_Nr
by Client_Nr Date: egen Client_NumberStockSellsPerD = count(Date) ///
if Security_AssetClass == "Aktien" ///
& Trade_BuySell == "Sell" ///
& Account_Type ~= "Sparen 3-Konto" & Account_SecuritiesAccountType ~= "Sparen 3-Depot" & Account_SecuritiesAccountType ~= "Sparen 3-Konto"
by Client_Nr Date: egen tmp = max(Client_NumberStockSellsPerD)
replace Client_NumberStockSellsPerD = tmp
drop tmp
replace Client_NumberStockSellsPerD = 0 if mi(Client_NumberStockSellsPerD)

* Number of trades in equity mutual funds per month
sort Client_Nr Month Date Security_Nr
by Client_Nr Month: egen Client_NumberEquityMFTradesPerM = count(Date) ///
if Security_AssetClass == "Fonds" & Security_FundType == "Fund - Shares (09)" ///
& Account_Type ~= "Sparen 3-Konto" & Account_SecuritiesAccountType ~= "Sparen 3-Depot" & Account_SecuritiesAccountType ~= "Sparen 3-Konto"
by Client_Nr Month: egen tmp = max(Client_NumberEquityMFTradesPerM)
replace Client_NumberEquityMFTradesPerM = tmp
drop tmp
replace Client_NumberEquityMFTradesPerM = 0 if mi(Client_NumberEquityMFTradesPerM)

* Number of trades in equity mutual funds per day
sort Client_Nr Date Security_Nr
by Client_Nr Date: egen Client_NumberEquityMFTradesPerD = count(Date) ///
if Security_AssetClass == "Fonds" & Security_FundType == "Fund - Shares (09)" ///
& Account_Type ~= "Sparen 3-Konto" & Account_SecuritiesAccountType ~= "Sparen 3-Depot" & Account_SecuritiesAccountType ~= "Sparen 3-Konto"
by Client_Nr Date: egen tmp = max(Client_NumberEquityMFTradesPerD)
replace Client_NumberEquityMFTradesPerD = tmp
drop tmp
replace Client_NumberEquityMFTradesPerD = 0 if mi(Client_NumberEquityMFTradesPerD)

* Number of buys in equity mutual funds per month
sort Client_Nr Month Date Security_Nr
by Client_Nr Month: egen Client_NumberEquityMFBuysPerM = count(Date) ///
if Security_AssetClass == "Fonds" & Security_FundType == "Fund - Shares (09)" ///
& Trade_BuySell == "Buy" ///
& Account_Type ~= "Sparen 3-Konto" & Account_SecuritiesAccountType ~= "Sparen 3-Depot" & Account_SecuritiesAccountType ~= "Sparen 3-Konto"
by Client_Nr Month: egen tmp = max(Client_NumberEquityMFBuysPerM)
replace Client_NumberEquityMFBuysPerM = tmp
drop tmp
replace Client_NumberEquityMFBuysPerM = 0 if mi(Client_NumberEquityMFBuysPerM)

* Number of buys in equity mutual funds per day
sort Client_Nr Date Security_Nr
by Client_Nr Date: egen Client_NumberEquityMFBuysPerD = count(Date) ///
if Security_AssetClass == "Fonds" & Security_FundType == "Fund - Shares (09)" ///
& Trade_BuySell == "Buy" ///
& Account_Type ~= "Sparen 3-Konto" & Account_SecuritiesAccountType ~= "Sparen 3-Depot" & Account_SecuritiesAccountType ~= "Sparen 3-Konto"
by Client_Nr Date: egen tmp = max(Client_NumberEquityMFBuysPerD)
replace Client_NumberEquityMFBuysPerD = tmp
drop tmp
replace Client_NumberEquityMFBuysPerD = 0 if mi(Client_NumberEquityMFBuysPerD)

* Number of sells in equity mutual funds per month
sort Client_Nr Month Date Security_Nr
by Client_Nr Month: egen Client_NumberEquityMFSellsPerM = count(Date) ///
if Security_AssetClass == "Fonds" & Security_FundType == "Fund - Shares (09)" ///
& Trade_BuySell == "Sell" ///
& Account_Type ~= "Sparen 3-Konto" & Account_SecuritiesAccountType ~= "Sparen 3-Depot" & Account_SecuritiesAccountType ~= "Sparen 3-Konto"
by Client_Nr Month: egen tmp = max(Client_NumberEquityMFSellsPerM)
replace Client_NumberEquityMFSellsPerM = tmp
drop tmp
replace Client_NumberEquityMFSellsPerM = 0 if mi(Client_NumberEquityMFSellsPerM)

* Number of sells in equity mutual funds per day
sort Client_Nr Date Security_Nr
by Client_Nr Date: egen Client_NumberEquityMFSellsPerD = count(Date) ///
if Security_AssetClass == "Fonds" & Security_FundType == "Fund - Shares (09)" ///
& Trade_BuySell == "Sell" ///
& Account_Type ~= "Sparen 3-Konto" & Account_SecuritiesAccountType ~= "Sparen 3-Depot" & Account_SecuritiesAccountType ~= "Sparen 3-Konto"
by Client_Nr Date: egen tmp = max(Client_NumberEquityMFSellsPerD)
replace Client_NumberEquityMFSellsPerD = tmp
drop tmp
replace Client_NumberEquityMFSellsPerD = 0 if mi(Client_NumberEquityMFSellsPerD)

* Trading volume in securities per month
sort Client_Nr Month Date Security_Nr
by Client_Nr Month: egen Client_TradingVolumePerM = total(abs(Trade_ValueCHF))

* Trading volume in stocks per month
sort Client_Nr Month Date Security_Nr
by Client_Nr Month: egen Client_TradingVolumeStocksPerM = total(abs(Trade_ValueCHF)) ///
if Security_AssetClass == "Aktien" ///
& Account_Type ~= "Sparen 3-Konto" & Account_SecuritiesAccountType ~= "Sparen 3-Depot" & Account_SecuritiesAccountType ~= "Sparen 3-Konto"
by Client_Nr Month: egen tmp = max(Client_TradingVolumeStocksPerM)
replace Client_TradingVolumeStocksPerM = tmp
drop tmp
replace Client_TradingVolumeStocksPerM = 0 if mi(Client_TradingVolumeStocksPerM)

* Trading volume in equity mutual funds per month
sort Client_Nr Month Date Security_Nr
by Client_Nr Month: egen Client_TradingVolumeEquityMFPerM = total(abs(Trade_ValueCHF)) ///
if Security_AssetClass == "Fonds" & Security_FundType == "Fund - Shares (09)" ///
& Account_Type ~= "Sparen 3-Konto" & Account_SecuritiesAccountType ~= "Sparen 3-Depot" & Account_SecuritiesAccountType ~= "Sparen 3-Konto"
by Client_Nr Month: egen tmp = max(Client_TradingVolumeEquityMFPerM)
replace Client_TradingVolumeEquityMFPerM = tmp
drop tmp
replace Client_TradingVolumeEquityMFPerM = 0 if mi(Client_TradingVolumeEquityMFPerM)

* Net investments in securities per month
sort Client_Nr Month Date Security_Nr
by Client_Nr Month: egen Client_NetInvestmentPerM = total(Trade_ValueCHF)

* Net investments in stocks per month
sort Client_Nr Month Date Security_Nr
by Client_Nr Month: egen Client_NetInvestmentStocksPerM = total(Trade_ValueCHF) ///
if Security_AssetClass == "Aktien" ///
& Account_Type ~= "Sparen 3-Konto" & Account_SecuritiesAccountType ~= "Sparen 3-Depot" & Account_SecuritiesAccountType ~= "Sparen 3-Konto"
by Client_Nr Month: egen tmp = max(Client_NetInvestmentStocksPerM)
replace Client_NetInvestmentStocksPerM = tmp
drop tmp
replace Client_NetInvestmentStocksPerM = 0 if mi(Client_NetInvestmentStocksPerM)

* Net investments in equity mutual funds per month
sort Client_Nr Month Date Security_Nr
by Client_Nr Month: egen Client_NetInvestmentEquityMFPerM = total(Trade_ValueCHF) ///
if Security_AssetClass == "Fonds" & Security_FundType == "Fund - Shares (09)" ///
& Account_Type ~= "Sparen 3-Konto" & Account_SecuritiesAccountType ~= "Sparen 3-Depot" & Account_SecuritiesAccountType ~= "Sparen 3-Konto"
by Client_Nr Month: egen tmp = max(Client_NetInvestmentEquityMFPerM)
replace Client_NetInvestmentEquityMFPerM = tmp
drop tmp
replace Client_NetInvestmentEquityMFPerM = 0 if mi(Client_NetInvestmentEquityMFPerM)

* Drop variables not used
keep Client_Nr Date Month Account_Nr Account_Type Account_SecuritiesAccountType ///
Trade_Nr Security_Nr Security_ISIN Security_Name ///
Security_AssetClass Security_FundType ///
Trade_BuySell Trade_ValueCHF Trade_Currency Account_Currency Trade_Type ///
///
Client_NumberTradesPerM Client_NumberTradesPerD ///
Client_NumberStockTradesPerM Client_NumberStockTradesPerD Client_NumberStockBuysPerM Client_NumberStockBuysPerD Client_NumberStockSellsPerM Client_NumberStockSellsPerD /// 
Client_NumberEquityMFTradesPerM Client_NumberEquityMFTradesPerD Client_NumberEquityMFBuysPerM Client_NumberEquityMFBuysPerD Client_NumberEquityMFSellsPerM Client_NumberEquityMFSellsPerD ///
Client_TradingVolumePerM ///
Client_TradingVolumeStocksPerM ///
Client_TradingVolumeEquityMFPerM ///
Client_NetInvestmentPerM ///
Client_NetInvestmentStocksPerM ///
Client_NetInvestmentEquityMFPerM

* Reorder variables
order Client_Nr Date Month Account_Nr Account_Type Account_SecuritiesAccountType ///
Trade_Nr Security_Nr Security_ISIN Security_Name ///
Security_AssetClass Security_FundType ///
Trade_BuySell Trade_ValueCHF Trade_Currency Account_Currency Trade_Type ///
///
Client_NumberTradesPerM Client_NumberTradesPerD ///
Client_NumberStockTradesPerM Client_NumberStockTradesPerD Client_NumberStockBuysPerM Client_NumberStockBuysPerD Client_NumberStockSellsPerM Client_NumberStockSellsPerD /// 
Client_NumberEquityMFTradesPerM Client_NumberEquityMFTradesPerD Client_NumberEquityMFBuysPerM Client_NumberEquityMFBuysPerD Client_NumberEquityMFSellsPerM Client_NumberEquityMFSellsPerD ///
Client_TradingVolumePerM ///
Client_TradingVolumeStocksPerM ///
Client_TradingVolumeEquityMFPerM ///
Client_NetInvestmentPerM ///
Client_NetInvestmentStocksPerM ///
Client_NetInvestmentEquityMFPerM

* Save
compress
sort Client_Nr Date Trade_Nr
save "Build/Data/SecuritiesTransactions.dta", replace



* Prepare for master
cd "${DataDir}"
use "Build/Data/SecuritiesTransactions.dta", clear

* Keep only one observation per client and month
sort Client_Nr Month Date Trade_Nr
by Client_Nr Month: gen tmp = _n
drop if tmp ~= 1
drop tmp

* Date
egen tmp = eomd(Date), format(%tdD_m_Y)
drop Date
rename tmp Date

* Drop variables not used
keep Client_Nr Date Month ///
///
Client_NumberTradesPerM ///
Client_NumberStockTradesPerM Client_NumberStockBuysPerM Client_NumberStockSellsPerM /// 
Client_NumberEquityMFTradesPerM Client_NumberEquityMFBuysPerM Client_NumberEquityMFSellsPerM ///
Client_TradingVolumePerM ///
Client_TradingVolumeStocksPerM ///
Client_TradingVolumeEquityMFPerM ///
Client_NetInvestmentPerM ///
Client_NetInvestmentStocksPerM ///
Client_NetInvestmentEquityMFPerM

* Reorder variables
order Client_Nr Date Month ///
///
Client_NumberTradesPerM ///
Client_NumberStockTradesPerM Client_NumberStockBuysPerM Client_NumberStockSellsPerM /// 
Client_NumberEquityMFTradesPerM Client_NumberEquityMFBuysPerM Client_NumberEquityMFSellsPerM ///
Client_TradingVolumePerM ///
Client_TradingVolumeStocksPerM ///
Client_TradingVolumeEquityMFPerM ///
Client_NetInvestmentPerM ///
Client_NetInvestmentStocksPerM ///
Client_NetInvestmentEquityMFPerM

* Save
compress
sort Client_Nr Month
save "Build/Data/SecuritiesTransactionsForMaster.dta", replace



cd "${DataDir}"
use "build/data/SecuritiesTransactions.dta", clear

* Keep only one observation per client and day
sort Client_Nr Date Security_Nr Trade_Nr
by Client_Nr Date: gen tmp = _n
keep if tmp == 1
drop tmp

* Drop variables not used
keep Client_Nr Date ///
///
Client_NumberTradesPerD ///
Client_NumberStockTradesPerD Client_NumberStockBuysPerD Client_NumberStockSellsPerD /// 
Client_NumberEquityMFTradesPerD Client_NumberEquityMFBuysPerD Client_NumberEquityMFSellsPerD

* Reorder variables
order Client_Nr Date ///
///
Client_NumberTradesPerD ///
Client_NumberStockTradesPerD Client_NumberStockBuysPerD Client_NumberStockSellsPerD /// 
Client_NumberEquityMFTradesPerD Client_NumberEquityMFBuysPerD Client_NumberEquityMFSellsPerD

* Save
compress
sort Client_Nr Date
save "Build/Data/SecuritiesTransactionsPerDay.dta", replace



*------------------------------------------------------------------------------
*(3) Client-advisor contacts dataset
*------------------------------------------------------------------------------

cd "${DataDir}"
use "Source/Original Data/T20_Kundenkontakte.dta", clear

* Rename variables
rename Bp_ID Client_Nr 
rename Crm_Issue_ID Contact_Nr 
rename K_Physisch Contact_InPerson
rename K_Anlegen Contact_Investment 
rename K_Art Contact_Type
rename K_Aufnahme Contact_Initiation

* Date
decode Kontakt_DT, gen(tmp)
gen Date = date(tmp, "YMD")
format %tdD_m_Y Date
drop Kontakt_DT tmp

* Month
gen Month = mofd(Date)
format %tm Month

* Decode variables
decode Contact_Type, gen(tmp)
drop Contact_Type
rename tmp Contact_Type
decode Contact_Initiation, gen(tmp)
drop Contact_Initiation
rename tmp Contact_Initiation

* Number of contacts per month
sort Client_Nr Month Date
by Client_Nr Month: egen Client_NumberOfContactsPerM = count(Date)

* Number of in-person contacts per month
tab Contact_InPerson
sort Client_Nr Month Date
by Client_Nr Month: egen Client_NumberOfInPersonPerM = total(Contact_InPerson)

* Number of phone calls per month
tab Contact_Type
sort Client_Nr Month Date
by Client_Nr Month: egen Client_NumberOfCallsPerM = count(Date) if Contact_Type == "Telefonkontakt"
by Client_Nr Month: egen tmp = max(Client_NumberOfCallsPerM)
replace Client_NumberOfCallsPerM = tmp
drop tmp
replace Client_NumberOfCallsPerM = 0 if mi(Client_NumberOfCallsPerM)

* Number of investment-related contacts per month
sort Client_Nr Month Date
by Client_Nr Month: egen Client_NumberOfInvestPerM = count(Date) if Contact_Investment == 1
by Client_Nr Month: egen tmp = max(Client_NumberOfInvestPerM)
replace Client_NumberOfInvestPerM = tmp
drop tmp
replace Client_NumberOfInvestPerM = 0 if mi(Client_NumberOfInvestPerM)

* Number of advisor-initiated contacts per month
tab Contact_Initiation
sort Client_Nr Month Date
by Client_Nr Month: egen Client_NumberOfAdvisorInitPerM = count(Date) if Contact_Initiation == "Durch Kundenberater"
by Client_Nr Month: egen tmp = max(Client_NumberOfAdvisorInitPerM)
replace Client_NumberOfAdvisorInitPerM = tmp
drop tmp
replace Client_NumberOfAdvisorInitPerM = 0 if mi(Client_NumberOfAdvisorInitPerM)

* Number of client-initiated contacts per month
sort Client_Nr Month Date
by Client_Nr Month: egen Client_NumberOfClientInitPerM = count(Date) if Contact_Initiation == "Durch Kunde"
by Client_Nr Month: egen tmp = max(Client_NumberOfClientInitPerM)
replace Client_NumberOfClientInitPerM = tmp
drop tmp
replace Client_NumberOfClientInitPerM = 0 if mi(Client_NumberOfClientInitPerM)

* Drop variables not used
keep Client_Nr Date Month ///
Contact_Nr Contact_Type Contact_Initiation Contact_InPerson ///
Contact_Investment ///
///
Client_NumberOfContactsPerM ///
Client_NumberOfInPersonPerM ///
Client_NumberOfCallsPerM ///
Client_NumberOfInvestPerM ///
Client_NumberOfAdvisorInitPerM Client_NumberOfClientInitPerM

* Reorder variables
order Client_Nr Date Month ///
Contact_Nr Contact_Type Contact_Initiation Contact_InPerson ///
Contact_Investment ///
///
Client_NumberOfContactsPerM ///
Client_NumberOfInPersonPerM ///
Client_NumberOfCallsPerM ///
Client_NumberOfInvestPerM ///
Client_NumberOfAdvisorInitPerM Client_NumberOfClientInitPerM

* Save
compress
sort Client_Nr Date Contact_Nr
save "Build/Data/Contacts.dta", replace



* Prepare for master
cd "${DataDir}"
use "Build/Data/Contacts.dta", clear

* Keep only one observation per client and month
sort Client_Nr Month Date Contact_Nr
by Client_Nr Month: gen tmp = _n
drop if tmp ~= 1
drop tmp

* Date
egen tmp = eomd(Date), format(%tdD_m_Y)
drop Date
rename tmp Date

* Drop variables not used
keep Client_Nr Date Month ///
Client_NumberOfContactsPerM ///
Client_NumberOfInPersonPerM ///
Client_NumberOfCallsPerM ///
Client_NumberOfInvestPerM ///
Client_NumberOfAdvisorInitPerM Client_NumberOfClientInitPerM

* Reorder variables
order Client_Nr Date Month ///
Client_NumberOfContactsPerM ///
Client_NumberOfInPersonPerM ///
Client_NumberOfCallsPerM ///
Client_NumberOfInvestPerM ///
Client_NumberOfAdvisorInitPerM Client_NumberOfClientInitPerM

* Save
compress
sort Client_Nr Month
save "Build/Data/ContactsForMaster.dta", replace



*------------------------------------------------------------------------------
*(4) Client characteristics dataset
*------------------------------------------------------------------------------

cd "${DataDir}"
use "Source/Original Data/T01_Bp_Snapshot.dta", clear

* Rename variables
rename Bp_ID Client_Nr
rename Geburtsjahr Client_YearOfBirth
rename Selektionsgrund_Name Client_WhyInData

* Decode variables
decode Client_WhyInData, gen(tmp)
drop Client_WhyInData
rename tmp Client_WhyInData

* Drop variables not used
keep Client_Nr Client_WhyInData ///
Client_YearOfBirth 

* Reorder variables
order Client_Nr Client_WhyInData ///
Client_YearOfBirth 

* Save
compress
sort Client_Nr
save "Build/Data/ClientCharacteristics.dta", replace



*------------------------------------------------------------------------------
*(5) Master
*------------------------------------------------------------------------------

cd "${DataDir}"
use "Source/Original Data/T02_Bp_Zeitreihe.dta", clear

* Rename variables
rename Bp_ID Client_Nr 
rename Hauptbetreuer_ID Advisor_Nr
rename Wohnland Client_Country

* Date
tostring Period_ID, replace
gen tmp = date(Period_ID, "YM")
egen Date = eomd(tmp), format(%tdD_m_Y)
drop Period_ID tmp

* Month
gen Month = mofd(Date)
format %tm Month

* Year
gen Year = year(Date)

* Restrict sample to January 2010 to June 2021
tab Month
drop if Month < ym(2010,1) | Month > ym(2021,6)

* Decode variables
decode Client_Country, gen(tmp)
drop Client_Country
rename tmp Client_Country

* Lives in Switzerland
tab Client_Country
gen Client_LivesInSwitzerland = 1 if Client_Country == "CH" //| Client_Country == "LI"
label var Client_LivesInSwitzerland "Switzerland (d)"
replace Client_LivesInSwitzerland = 0 if mi(Client_LivesInSwitzerland) & Client_Country ~= "n/a"



* Merge with client characteristics
merge m:1 Client_Nr using "Build/Data/ClientCharacteristics.dta"
drop if _merge == 2
drop _merge

* Age
sum Client_YearOfBirth, det
replace Client_YearOfBirth = . if Client_YearOfBirth == 2999 | Client_YearOfBirth == 1800 | Client_YearOfBirth == 1884 | Client_YearOfBirth == 1885 | Client_YearOfBirth == 9999
sum Client_YearOfBirth, det
sort Client_Nr Date
gen Client_Age = Year - Client_YearOfBirth
label var Client_Age "Age$^{}_{i,t}$"
sum Client_Age, det



* Merge with portfolio holdings
merge 1:1 Client_Nr Month using "Build/Data/PortfolioHoldingsForMaster.dta"
drop if _merge == 2
drop _merge
replace Client_BankWealth = 0 if mi(Client_BankWealth) 
replace Client_ValueChecking = 0 if mi(Client_ValueChecking) 
replace Client_ValueSavings = 0 if mi(Client_ValueSavings) 
replace Client_ValueRetSavings = 0 if mi(Client_ValueRetSavings) 
replace Client_ValueSecurities = 0 if mi(Client_ValueSecurities) 
replace Client_ValueStocks = 0 if mi(Client_ValueStocks) 
replace Client_ValueEquityMF = 0 if mi(Client_ValueEquityMF) 
replace Client_ValueMortgage = 0 if mi(Client_ValueMortgage) 
replace Client_ValueLoan = 0 if mi(Client_ValueLoan) 



* Merge with securities transactions
merge 1:1 Client_Nr Month using "Build/Data/SecuritiesTransactionsForMaster.dta"
drop if _merge == 2
drop _merge
replace Client_NumberTradesPerM = 0 if mi(Client_NumberTradesPerM)
replace Client_NumberStockTradesPerM = 0 if mi(Client_NumberStockTradesPerM)
replace Client_NumberStockBuysPerM = 0 if mi(Client_NumberStockBuysPerM)
replace Client_NumberStockSellsPerM = 0 if mi(Client_NumberStockSellsPerM)
replace Client_NumberEquityMFTradesPerM = 0 if mi(Client_NumberEquityMFTradesPerM)
replace Client_NumberEquityMFBuysPerM = 0 if mi(Client_NumberEquityMFBuysPerM)
replace Client_NumberEquityMFSellsPerM = 0 if mi(Client_NumberEquityMFSellsPerM)
replace Client_TradingVolumePerM = 0 if mi(Client_TradingVolumePerM)
replace Client_TradingVolumeStocksPerM = 0 if mi(Client_TradingVolumeStocksPerM)
replace Client_TradingVolumeEquityMFPerM = 0 if mi(Client_TradingVolumeEquityMFPerM)
replace Client_NetInvestmentPerM = 0 if mi(Client_NetInvestmentPerM)
replace Client_NetInvestmentStocksPerM = 0 if mi(Client_NetInvestmentStocksPerM)
replace Client_NetInvestmentEquityMFPerM = 0 if mi(Client_NetInvestmentEquityMFPerM)



* Merge with client-advisor contacts
merge 1:1 Client_Nr Month using "Build/Data/ContactsForMaster.dta"
drop if _merge == 2
drop _merge
replace Client_NumberOfContactsPerM = 0 if mi(Client_NumberOfContactsPerM)
replace Client_NumberOfInPersonPerM = 0 if mi(Client_NumberOfInPersonPerM)
replace Client_NumberOfCallsPerM = 0 if mi(Client_NumberOfCallsPerM)
replace Client_NumberOfInvestPerM = 0 if mi(Client_NumberOfInvestPerM)
replace Client_NumberOfAdvisorInitPerM = 0 if mi(Client_NumberOfAdvisorInitPerM)
replace Client_NumberOfClientInitPerM = 0 if mi(Client_NumberOfClientInitPerM)



* Drop variables not used
keep Client_Nr Date Month Year Client_WhyInData ///
Client_YearOfBirth Client_Age ///
Client_Country Client_LivesInSwitzerland ///
///
Client_BankWealth ///
Client_ValueChecking Client_WeightChecking ///
Client_ValueSavings Client_WeightSavings ///
Client_ValueRetSavings Client_WeightRetSavings ///
Client_ValueSecurities Client_WeightSecurities ///
Client_ValueStocks Client_WeightStocks ///
Client_ValueEquityMF Client_WeightEquityMF ///
Client_ValueMortgage ///
Client_ValueLoan ///
///
Client_NumberTradesPerM ///
Client_NumberStockTradesPerM Client_NumberStockBuysPerM Client_NumberStockSellsPerM /// 
Client_NumberEquityMFTradesPerM Client_NumberEquityMFBuysPerM Client_NumberEquityMFSellsPerM ///
Client_TradingVolumePerM ///
Client_TradingVolumeStocksPerM ///
Client_TradingVolumeEquityMFPerM ///
Client_NetInvestmentPerM ///
Client_NetInvestmentStocksPerM ///
Client_NetInvestmentEquityMFPerM ///
///
Client_NumberOfContactsPerM ///
Client_NumberOfInPersonPerM ///
Client_NumberOfCallsPerM ///
Client_NumberOfInvestPerM ///
Client_NumberOfAdvisorInitPerM Client_NumberOfClientInitPerM

* Reorder variables
order Client_Nr Date Month Year Client_WhyInData ///
Client_YearOfBirth Client_Age ///
Client_Country Client_LivesInSwitzerland ///
///
Client_BankWealth ///
Client_ValueChecking Client_WeightChecking ///
Client_ValueSavings Client_WeightSavings ///
Client_ValueRetSavings Client_WeightRetSavings ///
Client_ValueSecurities Client_WeightSecurities ///
Client_ValueStocks Client_WeightStocks ///
Client_ValueEquityMF Client_WeightEquityMF ///
Client_ValueMortgage ///
Client_ValueLoan ///
///
Client_NumberTradesPerM ///
Client_NumberStockTradesPerM Client_NumberStockBuysPerM Client_NumberStockSellsPerM /// 
Client_NumberEquityMFTradesPerM Client_NumberEquityMFBuysPerM Client_NumberEquityMFSellsPerM ///
Client_TradingVolumePerM ///
Client_TradingVolumeStocksPerM ///
Client_TradingVolumeEquityMFPerM ///
Client_NetInvestmentPerM ///
Client_NetInvestmentStocksPerM ///
Client_NetInvestmentEquityMFPerM ///
///
Client_NumberOfContactsPerM ///
Client_NumberOfInPersonPerM ///
Client_NumberOfCallsPerM ///
Client_NumberOfInvestPerM ///
Client_NumberOfAdvisorInitPerM Client_NumberOfClientInitPerM

* Save
compress
sort Client_Nr Month
save "Build/Analysis/Master.dta", replace




