#VWAP Trading still updating
library(tseries)
library(rugarch)
library(quantmod) #for getSymbols
library(PerformanceAnalytics)
library(forecast)
library(quantstrat)
# Create initdate, from, and to strings
initdate <- "1999-01-01"
from <- "2003-01-01"
to <- "2015-12-31"
#get data
getSymbols("SPY",from = from, to = to, src = "yahoo",
           adjust = TRUE)
#-----------
# Set the timezone to UTC
Sys.setenv(TZ="UTC")
# Set the currency to USD 
currency("USD")
#Treat as basic equity
stock("SPY",currency = "USD")
# Define your trade size and initial equity
tradesize <- 100000
initeq <- 100000
# Define the names of your strategy, portfolio and account
strategy.st <- "vwapstrat"
portfolio.st <- "vwapstrat"
account.st <- "vwapstrat"
rm.strat(strategy.st)
#initialize port, account, orders, store strategy-----------
# Initialize the portfolio
initPortf(portfolio.st, symbols = "SPY", initDate = initdate, currency = "USD")

# Initialize the account
initAcct(account.st, portfolios = portfolio.st, initDate = initdate, currency = "USD", initEq = initeq)

# Initialize the orders
initOrders(portfolio.st, initDate = initdate)
# Store the strategy
strategy(strategy.st, store = TRUE)

#Create Indicator for VWAP trading--------- 
#Create Signal > use SigThreshold for VWAP---------
#Entry above Vwap