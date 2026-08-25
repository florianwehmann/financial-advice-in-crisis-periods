library(data.table)
library(ggplot2)
library(scales)
library(lubridate)

# Directory containing the data (--> adjust this line)
data_dir <- "C:/Users/FWehmann/Dropbox/Financial Advice in Crisis Periods/Empirical Analysis/"

bar_colors <- c("2019" = "#4472C4", "2020" = "#ED7D31")

month_axis <- scale_x_continuous(breaks = 1:12, labels = 1:12, limits = c(0.5, 12.5))

bar_theme <- list(
  theme_bw(),
  theme(legend.position = "bottom", legend.title = element_blank()),
  scale_fill_manual(values = bar_colors)
)

#------------------------------------------------------------------------------
# Figure 1: Percentage invested in stocks and equity mutual funds
#------------------------------------------------------------------------------

master <- readRDS(file.path(data_dir, "Build/Analysis/Master.rds"))

master[, MonthOfYear := month(Date)]
master[, Client_WeightStocksEquityMF := Client_WeightStocks + Client_WeightEquityMF]

fig1 <- master[Year %in% c(2019L, 2020L),
               .(WeightStocksEquityMF = mean(Client_WeightStocksEquityMF, na.rm = TRUE)),
               by = .(Year, MonthOfYear)]
fig1[, WeightStocksEquityMF := WeightStocksEquityMF * 100]
fig1[, x := fifelse(Year == 2019L, MonthOfYear - 0.2, MonthOfYear + 0.2)]

ggplot(fig1, aes(x = x, y = WeightStocksEquityMF, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.35) +
  month_axis +
  # scale_y_continuous(breaks = c(6, 6.5, 7, 7.5, 8)) +
  # ylim(6,8)+
  labs(x = "Month of year", y = "% invested in stocks and equity mutual funds") +
  bar_theme

ggsave(file.path(data_dir, "Build/Graphs/Figure1.png"), width = 10, height = 6, dpi = 300)


#------------------------------------------------------------------------------
# Figure 2: Number of trades in stocks and equity mutual funds
#------------------------------------------------------------------------------

master[, Client_NumStockEquityMFTradesPerM := Client_NumberStockTradesPerM + Client_NumberEquityMFTradesPerM]
master[, Client_NumStockEquityMFBuysPerM   := Client_NumberStockBuysPerM   + Client_NumberEquityMFBuysPerM]
master[, Client_NumStockEquityMFSellsPerM  := Client_NumberStockSellsPerM  + Client_NumberEquityMFSellsPerM]

fig2 <- master[Year %in% c(2019L, 2020L), .(
  NumTradesPerM = sum(Client_NumStockEquityMFTradesPerM, na.rm = TRUE),
  NumBuysPerM   = sum(Client_NumStockEquityMFBuysPerM,   na.rm = TRUE),
  NumSellsPerM  = sum(Client_NumStockEquityMFSellsPerM,  na.rm = TRUE)
), by = .(Year, MonthOfYear)]
fig2[, x := fifelse(Year == 2019L, MonthOfYear - 0.2, MonthOfYear + 0.2)]

trade_y_axis <- scale_y_continuous(
  breaks = seq(0, 25000, 5000), labels = comma, limits = c(0, 25000)
)

# Figure 2A: Total trades
ggplot(fig2, aes(x = x, y = NumTradesPerM, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.35) +
  month_axis + trade_y_axis +
  labs(x = "Month of year", y = "# trades in stocks and equity mutual funds") +
  bar_theme
# ggsave(file.path(data_dir, "Build/Graphs/Figure2A.png"), width = 10, height = 6, dpi = 300)

# Figure 2D: Stacked buys and sells
fig2_stacked <- melt(fig2, id.vars = c("Year", "MonthOfYear", "x"),
                     measure.vars = c("NumBuysPerM", "NumSellsPerM"),
                     variable.name = "TradeType", value.name = "NumTradesPerM")
fig2_stacked[, TradeType := fifelse(TradeType == "NumBuysPerM", "Buys", "Sells")]
fig2_stacked[, Group := factor(paste(Year, TradeType, sep = " - "),
                               levels = c("2019 - Buys", "2019 - Sells", "2020 - Buys", "2020 - Sells"))]

stacked_colors <- c(
  "2019 - Buys"  = "#4472C4",
  "2019 - Sells" = "#A9C0E8",
  "2020 - Buys"  = "#ED7D31",
  "2020 - Sells" = "#F4B183"
)

ggplot(fig2_stacked, aes(x = x, y = NumTradesPerM, fill = Group)) +
  geom_bar(stat = "identity", width = 0.35, position = "stack") +
  scale_fill_manual(values = stacked_colors) +
  month_axis + trade_y_axis +
  labs(x = "Month of year", y = "# trades in stocks and equity mutual funds") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank())
# ggsave(file.path(data_dir, "Build/Graphs/Figure2D.png"), width = 10, height = 6, dpi = 300)

# Figure 2B: Purchases
ggplot(fig2, aes(x = x, y = NumBuysPerM, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.35) +
  month_axis + trade_y_axis +
  labs(x = "Month of year", y = "# purchases in stocks and equity mutual funds") +
  bar_theme
# ggsave(file.path(data_dir, "Build/Graphs/Figure2B.png"), width = 10, height = 6, dpi = 300)

# Figure 2C: Sales
ggplot(fig2, aes(x = x, y = NumSellsPerM, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.35) +
  month_axis + trade_y_axis +
  labs(x = "Month of year", y = "# sales in stocks and equity mutual funds") +
  bar_theme
# ggsave(file.path(data_dir, "Build/Graphs/Figure2C.png"), width = 10, height = 6, dpi = 300)


#------------------------------------------------------------------------------
# Figure 3: Net investments in stocks and equity mutual funds
#------------------------------------------------------------------------------

master[, Client_NetInvestStockEquityMFPerM := Client_NetInvestmentStocksPerM + Client_NetInvestmentEquityMFPerM]

fig3 <- master[Year %in% c(2019L, 2020L),
               .(NetInvestPerM = sum(Client_NetInvestStockEquityMFPerM, na.rm = TRUE)),
               by = .(Year, MonthOfYear)]
fig3[, NetInvestPerM := NetInvestPerM / 1e6]
fig3[, x := fifelse(Year == 2019L, MonthOfYear - 0.2, MonthOfYear + 0.2)]

ggplot(fig3, aes(x = x, y = NetInvestPerM, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.35) +
  month_axis +
  labs(x = "Month of year",
       y = "Net investments in stocks and equity mutual\nfunds (in CHFmn)") +
  bar_theme
ggsave(file.path(data_dir, "Build/Graphs/Figure3.png"), width = 10, height = 6, dpi = 300)


#------------------------------------------------------------------------------
# Figure 4: Number of client-advisor contacts
#------------------------------------------------------------------------------

fig4 <- master[Year %in% c(2019L, 2020L), .(
  ContactsPerM    = sum(Client_NumberOfContactsPerM,    na.rm = TRUE),
  AdvisorInitPerM = sum(Client_NumberOfAdvisorInitPerM, na.rm = TRUE),
  ClientInitPerM  = sum(Client_NumberOfClientInitPerM,  na.rm = TRUE)
), by = .(Year, MonthOfYear)]
fig4[, x := fifelse(Year == 2019L, MonthOfYear - 0.2, MonthOfYear + 0.2)]

contact_y_axis <- scale_y_continuous(
  breaks = seq(0, 10000, 2000), labels = comma, limits = c(0, 10000)
)

# Figure 4A: Total contacts
ggplot(fig4, aes(x = x, y = ContactsPerM, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.35) +
  month_axis + contact_y_axis +
  labs(x = "Month of year", y = "# client-advisor contacts") +
  bar_theme
# ggsave(file.path(data_dir, "Build/Graphs/Figure4A.png"), width = 10, height = 6, dpi = 300)

# Figure 4D: Stacked advisor- and client-initiated contacts
fig4_stacked <- melt(fig4, id.vars = c("Year", "MonthOfYear", "x"),
                     measure.vars = c("AdvisorInitPerM", "ClientInitPerM"),
                     variable.name = "ContactType", value.name = "ContactsPerM")
fig4_stacked[, ContactType := fifelse(ContactType == "AdvisorInitPerM", "Advisor-initiated", "Client-initiated")]
fig4_stacked[, Group := factor(paste(Year, ContactType, sep = " - "),
                               levels = c("2019 - Advisor-initiated", "2019 - Client-initiated",
                                          "2020 - Advisor-initiated", "2020 - Client-initiated"))]

stacked_colors_contacts <- c(
  "2019 - Advisor-initiated" = "#4472C4",
  "2019 - Client-initiated"  = "#A9C0E8",
  "2020 - Advisor-initiated" = "#ED7D31",
  "2020 - Client-initiated"  = "#F4B183"
)

ggplot(fig4_stacked, aes(x = x, y = ContactsPerM, fill = Group)) +
  geom_bar(stat = "identity", width = 0.35, position = "stack") +
  scale_fill_manual(values = stacked_colors_contacts) +
  month_axis + contact_y_axis +
  labs(x = "Month of year", y = "# client-advisor contacts") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank())
# ggsave(file.path(data_dir, "Build/Graphs/Figure4D.png"), width = 10, height = 6, dpi = 300)

# Figure 4B: Advisor-initiated contacts
ggplot(fig4, aes(x = x, y = AdvisorInitPerM, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.35) +
  month_axis + contact_y_axis +
  labs(x = "Month of year", y = "# advisor-initiated client-advisor contacts") +
  bar_theme
ggsave(file.path(data_dir, "Build/Graphs/Figure4B.png"), width = 10, height = 6, dpi = 300)

# Figure 4C: Client-initiated contacts
ggplot(fig4, aes(x = x, y = ClientInitPerM, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.35) +
  month_axis + contact_y_axis +
  labs(x = "Month of year", y = "# client-initiated client-advisor contacts") +
  bar_theme
ggsave(file.path(data_dir, "Build/Graphs/Figure4C.png"), width = 10, height = 6, dpi = 300)
