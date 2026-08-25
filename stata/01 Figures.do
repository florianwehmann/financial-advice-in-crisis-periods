set more off
  
* Directory containing the data (--> adjust this line)
* Nic
*global DataDir "C:/Users/nic.schaub/Dropbox/Financial Advice in Crisis Periods/Empirical Analysis/" // office
global DataDir "C:/Users/FWehmann/Dropbox/Financial Advice in Crisis Periods/Empirical Analysis/"

*------------------------------------------------------------------------------
* Figure 1: Percentage invested in stocks and equity mutual funds
*------------------------------------------------------------------------------

cd "${DataDir}"
use "Build/Analysis/Master.dta", clear

* Month of year
gen MonthOfYear = month(Date)

* Percentage invested in stocks and equity mutual funds
gen Client_WeightStocksEquityMF = Client_WeightStocks + Client_WeightEquityMF

* Collapse by month
collapse (mean) WeightStocksEquityMF = Client_WeightStocksEquityMF, by(Year MonthOfYear)

* Restrict sample to 2019 and 2020
keep if Year == 2019 | Year == 2020

* Rescale variables
replace WeightStocksEquityMF = WeightStocksEquityMF * 100

* Adjusted month of year
replace MonthOfYear = MonthOfYear - 0.2 if Year == 2019
replace MonthOfYear = MonthOfYear + 0.2 if Year == 2020

graph twoway (bar WeightStocksEquityMF MonthOfYear if Year == 2019, barw(0.35)) ///
(bar WeightStocksEquityMF MonthOfYear if Year == 2020, barw(0.35)), ///
xscale(range(1 12)) xlabel(1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10" 11 "11" 12 "12") ///
xtitle("Month of year") ///
yscale(range(6 8)) ylabel(6 "6" 6.5 "6.5" 7 "7" 7.5 "7.5" 8 "8") ///
ytitle("% invested in stocks and equity mutual funds") ///
legend(order(1 "2019" 2 "2020")) ///
graphregion(color(white))
graph export "Build/Graphs/Figure1.png", replace width(4000)



*------------------------------------------------------------------------------
* Figure 2: Number of trades in stocks and equity mutual funds
*------------------------------------------------------------------------------

cd "${DataDir}"
use "Build/Analysis/Master.dta", clear

* Month of year
gen MonthOfYear = month(Date)

* Number of trades in stocks and equity mutual funds
gen Client_NumStockEquiMFTradesPerM = Client_NumberStockTradesPerM + Client_NumberEquityMFTradesPerM

* Number of buys in stocks and equity mutual funds
gen Client_NumStockEquityMFBuysPerM = Client_NumberStockBuysPerM + Client_NumberEquityMFBuysPerM

* Number of sells in stocks and equity mutual funds
gen Client_NumStockEquityMFSellsPerM = Client_NumberStockSellsPerM + Client_NumberEquityMFSellsPerM

* Collapse by month
collapse (sum) NumStockEquityMFTradesPerM = Client_NumStockEquiMFTradesPerM ///
(sum) NumStockEquityMFBuysPerM = Client_NumStockEquityMFBuysPerM ///
(sum) NumStockEquityMFSellsPerM = Client_NumStockEquityMFSellsPerM ///
, by(Year MonthOfYear)

* Restrict sample to 2019 and 2020
keep if Year == 2019 | Year == 2020

* Adjusted month of year
replace MonthOfYear = MonthOfYear - 0.2 if Year == 2019
replace MonthOfYear = MonthOfYear + 0.2 if Year == 2020

graph twoway (bar NumStockEquityMFTradesPerM MonthOfYear if Year == 2019, barw(0.35)) ///
(bar NumStockEquityMFTradesPerM MonthOfYear if Year == 2020, barw(0.35)), ///
xscale(range(1 12)) xlabel(1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10" 11 "11" 12 "12") ///
xtitle("Month of year") ///
yscale(range(0 25000)) ylabel(0 "0" 5000 "5,000" 10000 "10,000" 15000 "15,000" 20000 "20,000" 25000 "25,000") ///
ytitle("# trades in stocks and equity mutual funds") ///
legend(order(1 "2019" 2 "2020")) ///
graphregion(color(white))
graph export "Build/Graphs/Figure2A.png", replace width(4000)

graph twoway (bar NumStockEquityMFBuysPerM MonthOfYear if Year == 2019, barw(0.35)) ///
(bar NumStockEquityMFBuysPerM MonthOfYear if Year == 2020, barw(0.35)), ///
xscale(range(1 12)) xlabel(1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10" 11 "11" 12 "12") ///
xtitle("Month of year") ///
yscale(range(0 25000)) ylabel(0 "0" 5000 "5,000" 10000 "10,000" 15000 "15,000" 20000 "20,000" 25000 "25,000") ///
ytitle("# purchases in stocks and equity mutual funds") ///
legend(order(1 "2019" 2 "2020")) ///
graphregion(color(white))
graph export "Build/Graphs/Figure2B.png", replace width(4000)

graph twoway (bar NumStockEquityMFSellsPerM MonthOfYear if Year == 2019, barw(0.35)) ///
(bar NumStockEquityMFSellsPerM MonthOfYear if Year == 2020, barw(0.35)), ///
xscale(range(1 12)) xlabel(1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10" 11 "11" 12 "12") ///
xtitle("Month of year") ///
yscale(range(0 25000)) ylabel(0 "0" 5000 "5,000" 10000 "10,000" 15000 "15,000" 20000 "20,000" 25000 "25,000") ///
ytitle("# sales in stocks and equity mutual funds") ///
legend(order(1 "2019" 2 "2020")) ///
graphregion(color(white))
graph export "Build/Graphs/Figure2C.png", replace width(4000)



*------------------------------------------------------------------------------
* Figure 3: Net investments in stocks and equity mutual funds
*------------------------------------------------------------------------------

cd "${DataDir}"
use "Build/Analysis/Master.dta", clear

* Month of year
gen MonthOfYear = month(Date)

* Net investments in stocks and equity mutual funds
gen Client_NetInvestStockEquitMFPerM = Client_NetInvestmentStocksPerM + Client_NetInvestmentEquityMFPerM

* Collapse by month
collapse (sum) NetInvestStockEquityMFPerM = Client_NetInvestStockEquitMFPerM, by(Year MonthOfYear)

* Restrict sample to 2019 and 2020
keep if Year == 2019 | Year == 2020

* Rescale variables
replace NetInvestStockEquityMFPerM = NetInvestStockEquityMFPerM / 1000000

* Adjusted month of year
replace MonthOfYear = MonthOfYear - 0.2 if Year == 2019
replace MonthOfYear = MonthOfYear + 0.2 if Year == 2020

graph twoway (bar NetInvestStockEquityMFPerM MonthOfYear if Year == 2019, barw(0.35)) ///
(bar NetInvestStockEquityMFPerM MonthOfYear if Year == 2020, barw(0.35)), ///
xscale(range(1 12)) xlabel(1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10" 11 "11" 12 "12") ///
xtitle("Month of year") ///
ytitle("Net investments in stocks and equity mutual" "funds (in CHFmn)") ///
legend(order(1 "2019" 2 "2020")) ///
graphregion(color(white))
graph export "Build/Graphs/Figure3.png", replace width(4000)



*------------------------------------------------------------------------------
* Figure 4: Number of client-advisor contacts
*------------------------------------------------------------------------------

cd "${DataDir}"
use "Build/Analysis/Master.dta", clear

* Month of year
gen MonthOfYear = month(Date)

* Collapse by month
collapse (sum) NumberOfContactsPerM = Client_NumberOfContactsPerM ///
(sum) NumberOfAdvisorInitPerM = Client_NumberOfAdvisorInitPerM ///
(sum) NumberOfClientInitPerM = Client_NumberOfClientInitPerM ///
, by(Year MonthOfYear)

* Restrict sample to 2019 and 2020
keep if Year == 2019 | Year == 2020

* Adjusted month of year
replace MonthOfYear = MonthOfYear - 0.2 if Year == 2019
replace MonthOfYear = MonthOfYear + 0.2 if Year == 2020

graph twoway (bar NumberOfContactsPerM MonthOfYear if Year == 2019, barw(0.35)) ///
(bar NumberOfContactsPerM MonthOfYear if Year == 2020, barw(0.35)), ///
xscale(range(1 12)) xlabel(1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10" 11 "11" 12 "12") ///
xtitle("Month of year") ///
yscale(range(0 10000)) ylabel(0 "0" 2000 "2,000" 4000 "4,000" 6000 "6,000" 8000 "8,000" 10000 "10,000") ///
ytitle("# client-advisor contacts") ///
legend(order(1 "2019" 2 "2020")) ///
graphregion(color(white))
graph export "Build/Graphs/Figure4A.png", replace width(4000)

graph twoway (bar NumberOfAdvisorInitPerM MonthOfYear if Year == 2019, barw(0.35)) ///
(bar NumberOfAdvisorInitPerM MonthOfYear if Year == 2020, barw(0.35)), ///
xscale(range(1 12)) xlabel(1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10" 11 "11" 12 "12") ///
xtitle("Month of year") ///
yscale(range(0 10000)) ylabel(0 "0" 2000 "2,000" 4000 "4,000" 6000 "6,000" 8000 "8,000" 10000 "10,000") ///
ytitle("# advisor-initiated client-advisor contacts") ///
legend(order(1 "2019" 2 "2020")) ///
graphregion(color(white))
graph export "Build/Graphs/Figure4B.png", replace width(4000)

graph twoway (bar NumberOfClientInitPerM MonthOfYear if Year == 2019, barw(0.35)) ///
(bar NumberOfClientInitPerM MonthOfYear if Year == 2020, barw(0.35)), ///
xscale(range(1 12)) xlabel(1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10" 11 "11" 12 "12") ///
xtitle("Month of year") ///
yscale(range(0 10000)) ylabel(0 "0" 2000 "2,000" 4000 "4,000" 6000 "6,000" 8000 "8,000" 10000 "10,000") ///
ytitle("# client-initiated client-advisor contacts") ///
legend(order(1 "2019" 2 "2020")) ///
graphregion(color(white))
graph export "Build/Graphs/Figure4C.png", replace width(4000)
