#VWAP Trading still updating
library(tseries)
library(rugarch)
library(quantmod) #for getSymbols
library(PerformanceAnalytics)
library(forecast)
library(quantstrat)
library(TTR)
library(blotter)

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
# Define your trade size and initial equity----------
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
#Create Indicator for VWAP trading*********---------
#need to calculate VWAP---------
#use look back data = 21 days
vwap <- VWAP(SPY$SPY.Adjusted,SPY$SPY.Volume, n = 21)
#vwap <- sum(SPY$SPY.Adjusted * SPY$SPY.Volume) / sum(SPY$SPY.Volume)
add.indicator(strategy.st, name = "VWAP", arguments = list(HLC = quote(HLC(mktdata)), n = 21), label = "vwap")
#opening_price calculate & GARCH*****---------

#Create Signal > use SigThreshold for VWAP---------
#entry_short opening price above VWAP

add.signal(strategy.st, name = "sigThreshold", 
           
           # compare VWAP
           arguments = list(column = "opening_price", 
                            
                            # The threshold is v
                            threshold = VWAP, 
                            
                            # We want the opening_price to be over this value
                            relationship = "gt", 
                            
                            # We are interested only in the cross
                            cross = TRUE), 
           
           # Label it entry_short
           label = "entry_short")
#entry_long opening price below Vwap
add.signal(strategy.st, name = "sigThreshold", 
           
           # compare VWAP
           arguments = list(column = "opening_price", 
                            
                            # The threshold is v
                            threshold = VWAP, 
                            
                            # We want the opening_price to be below this value
                            relationship = "lt", 
                            
                            # We are interested only in the cross
                            cross = TRUE), 
           
           # Label it entry_long
           label = "entry_long")
#RULE--------------
#make trade when over VWAP
# Create an entry rule of 1 share when all conditions line up to enter into a position
#entry_short
add.rule(strategy.st, name = "ruleSignal", 
         
         # Use the shortentry column as the sigcol
         arguments=list(sigcol = "entry_short", 
                        
                        # Set sigval to TRUE
                        sigval = TRUE, 
                        
                        # Set orderqty to tradesize
                        orderqty = tradesize,
                        
                        # Use a market type of order
                        ordertype = "market",
                        
                        # Take the short orderside
                        orderside = "short",
                        
                        # Do not replace other signals
                        replace = FALSE, 
                        
                        # Buy at the next day's opening price
                        prefer = "Open"),
         
         # This is an enter type rule, not an exit
         type = "enter")
#entry_long
add.rule(strategy.st, name = "ruleSignal", 
         
         # Use the shortentry column as the sigcol
         arguments=list(sigcol = "entry_long", 
                        
                        # Set sigval to TRUE
                        sigval = TRUE, 
                        
                        # Set orderqty to tradesize
                        orderqty = tradesize,
                        
                        # Use a market type of order
                        ordertype = "market",
                        
                        # Take the long orderside
                        orderside = "long",
                        
                        # Do not replace other signals
                        replace = FALSE, 
                        
                        # Buy at the next day's opening price
                        prefer = "Open"),
         
         # This is an enter type rule, not an exit
         type = "enter")
#building strategy--------------
# Use applyStrategy() to apply your strategy. Save this to out
out <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st)
# Update your portfolio (portfolio.st)
updatePortf(portfolio.st)
daterange <- time(getPortfolio(portfolio.st)$summary)[-1]

# Update your account (account.st)
updateAcct(account.st, daterange)
updateEndEq(account.st)