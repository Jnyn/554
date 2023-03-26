library(quantstrat)
library(quantmod)
library(PerformanceAnalytics)
library(rugarch)
library(TTR)
# Import the SPY data from Yahoo Finance
#ticker <- "SPY"
#getSymbols(ticker, src = "yahoo",from = "2000-01-01", to = "2003-09-30", periodicity = "daily")
#SPY <- adjustOHLC(SPY, use.Adjusted = TRUE)
#SPY<-na.omit(SPY)
#colnames(SPY) <- gsub("^SPY.", "", colnames(SPY))

# Calculate the log returns and fit a GARCH model
# Reformat data to be compatible with quantstrat
#SPY$returns <- diff(log(SPY$Close))
SPY<-na.omit(SPY)
.orderqty <- 100
Sys.setenv(TZ="UTC")
currency("USD")
stock("SPY",currency = "USD")
SPY$VWAP<-VWAP(Cl(SPY),Vo(SPY),22)
SPY <- na.omit(SPY)

if (!exists('.blotter')) .blotter <- new.env()
if (!exists('.strategy')) .strategy <- new.env()
# Define the strategy
strategy_name <- "VWAP_Short_Long"
rm.strat(strategy_name)
#initPortf(strategy_name, symbols = ticker, initDate = index(SPY)[1], currency = "USD")
initPortf(strategy_name, symbols = ticker, currency = "USD")
#initAcct(strategy_name, portfolios = strategy_name, initDate = index(SPY)[1], currency = "USD", initEq = 1000000)
initAcct(strategy_name, portfolios = strategy_name, currency = "USD", initEq = 10000000)
initDate = index(SPY)[1]

initOrders(portfolio = strategy_name, initDate = index(SPY)[1])

# Initialize the strategy with the modified SPY data
strategy(strategy_name, store = TRUE)

# Indicators: VWAP and volatility
add.indicator(strategy_name, name = "VWAP", 
              arguments = list(price = quote(Cl(mktdata)), volume = quote(Vo(mktdata))), label = "VWAP")
# add.indicator(strategy_name, name = "volatility",arguments = list(returns = quote(mktdata$returns), n = 252), label = "volatility")

# Reformat data to be compatible with quantstrat
colnames(SPY) <- gsub("^SPY.", "", colnames(SPY))

# # Signals: Open price crossing VWAP
# add.signal(strategy_name, name = "sigCrossover", arguments = list(columns = c("Open", "VWAP"), relationship = "gt"), label = "Open_gt_VWAP")
# add.signal(strategy_name, name = "sigCrossover", arguments = list(columns = c("Open", "VWAP"), relationship = "lt"), label = "Open_lt_VWAP")

crossoverVWAP <- function(x, label, col1 = "Open", col2 = "VWAP") {
  Col1 <- grep(pattern = col1, colnames(x), value = TRUE)[1]
  Col2 <- grep(pattern = col2, colnames(x), value = TRUE)[1]
  
  stopifnot(length(Col1) == 1)
  stopifnot(length(Col2) == 1)
  
  columns <- c(Col1, Col2)
  
  gt_relation <- sigCrossover(data = x, label = "Open_gt_VWAP", columns = columns, relationship = "gt")
  lt_relation <- sigCrossover(data = x, label = "Open_lt_VWAP", columns = columns, relationship = "lt")
  
  result <- xts(merge(gt_relation, lt_relation), order.by = index(x))
  
  colnames(result) <- c("Open_gt_VWAP", "Open_lt_VWAP")
  
  return(result)
}

add.signal(strategy_name, name = "crossoverVWAP", arguments = list(x = quote(mktdata), label = "VWAP"), label = "VWAP_crossover")

# Entry rules
# Add a rule for entering a short position when the open price is above the VWAP
add.rule(strategy_name,                  # Name of the strategy
         name = "ruleSignal",             # Name of the rule
         arguments = list(                # List of arguments for the rule
           sigcol = "Open_gt_VWAP.VWAP_crossover",       # Name of the signal column
           sigval = TRUE,                 # Value of the signal to trigger the rule
           ordertype = "market",          # Type of order to place (market or limit)
           orderside = "short",           # Side of the order (long or short)
           replace = TRUE,               # Whether to replace existing orders
           TxnFees = 0,                   # Transaction fees
           prefer = "Open",               # Price preference for limit orders
           osFUN = osMaxPos,             # Function for order sizing
           orderqty= -.orderqty,
           orderset = "ocoshort"
         ),
         type = "enter",                   # Type of rule (enter, exit, or chain)
         path.dep = TRUE,                  # Whether to include dependent path data
         label = "Enter_Short",            # Label for the rule
         debug = TRUE                      # Whether to enter debug mode when the rule is triggered
)

# Add a rule for entering a long position when the open price is below the VWAP
add.rule(strategy_name,                  # Name of the strategy
         name = "ruleSignal",             # Name of the rule
         arguments = list(                # List of arguments for the rule
           sigcol = "Open_lt_VWAP.VWAP_crossover",       # Name of the signal column
           sigval = TRUE,                 # Value of the signal to trigger the rule
           ordertype = "market",          # Type of order to place (market or limit)
           orderside = "long",            # Side of the order (long or short)
           replace = TRUE,               # Whether to replace existing orders
           TxnFees = 0,                   # Transaction fees
           prefer = "Open",               # Price preference for limit orders
           osFUN = osMaxPos,               # Function for order sizing
           orderqty= .orderqty,
           orderset = "ocolong"
         ),
         type = "enter",                   # Type of rule (enter, exit, or chain)
         path.dep = TRUE,                  # Whether to include dependent path data
         label = "Enter_Long",            # Label for the rule
         debug = TRUE
)
#Enter to another------------
add.rule(strategy_name, 
         name = "ruleSignal",
         arguments = list(sigcol = "Open_lt_VWAP.VWAP_crossover", 
                          sigval = TRUE,
                          replace = TRUE,
                          orderside = "short" ,
                          ordertype = "market",
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocoshort"),
         type = "exit",
         label = "Exit2LONG")

add.rule(strategy_name, 
         name = "ruleSignal",
         arguments = list(sigcol = "Open_gt_VWAP.VWAP_crossover", 
                          sigval = TRUE,
                          replace = TRUE,
                          orderside = "long" ,
                          ordertype = "market",
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocoslong"),
         type = "exit",
         label = "Exit2SHORT")


#add stop loss------------
#.stoploss <- 3e-3 # 0.003 or 0.3%
#mktdata_filtered <- mktdata[complete.cases(mktdata$VWAP), ]
#.stoploss <-mktdata_filtered$Volatility/mktdata_filtered$Open
.stoploss <-SPY$Volatility/SPY$Open

add.rule(strategy_name,
         name = "ruleSignal",
         arguments = list(sigcol = "Open_lt_VWAP.VWAP_crossover",
                          sigval = TRUE,
                          orderside = "long",
                          ordertype = "stoplimit",
                          orderqty = "all",
                          threshold = quote(.stoploss), 
                          tmult = TRUE, 
                          TxnFees = 0,
                          replace = FALSE,
                          orderset = "ocolong"),
         type = "chain",
         parent = "Enter_lONG",
         enabled = FALSE,
         label = "StopLOSSLONG")

add.rule(strategy_name,
         name = "ruleSignal",
         arguments = list(sigcol = "Open_gt_VWAP.VWAP_crossover",
                          sigval = TRUE,
                          orderside = "short",
                          ordertype = "stoplimit",
                          orderqty = "all",
                          threshold = quote(.stoploss), 
                          tmult = TRUE, 
                          TxnFees = 0,
                          replace = FALSE,
                          orderset = "ocoshort"),
         type = "chain",
         parent = "Enter_Short",
         enabled = FALSE,
         label = "StopLOSSShort")
#osMaxPos--------
addPosLimit(portfolio = strategy_name,
            symbol = ticker,
            timestamp = initDate,
            maxpos = .orderqty)
#enable rules
enable.rule(strategy_name, 
            type = "chain", 
            label = "StopLoss")

# Run the backtest and analyze the results-------------
applyStrategy(strategy_name, portfolios = strategy_name)

traceback() # Analyze performance
updatePortf(strategy_name)
updateAcct(strategy_name)
updateEndEq(strategy_name)

# Get trade statistics
tradeStats(Portfolio = strategy_name, Symbol = ticker)

#plot
chart_Series(SPY$Open)
add_TA(SPY$VWAP,on=1)

#check if indicator is working
indicator_data <- applyIndicators(strategy = strategy_name, mktdata = SPY)
signal_data <- applySignals(strategy = strategy_name, mktdata = indicator_data)
# print order
orders <- getOrderBook(portfolio = strategy_name)
print(orders)
