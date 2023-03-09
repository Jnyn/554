# Load the required libraries
library(rugarch)
library(quantmod)
library(PerformanceAnalytics)
# Load the data
getSymbols("SPY",
           from = "2011-01-01",
           to = "2011-12-05",
           src = "yahoo",
           adjust = TRUE)
return = CalculateReturns(SPY$SPY.Adjusted)
data = return[-c(1),]
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0)))
fit <- ugarchfit(spec = spec, data = data, solver = "solnp")
vol<-sigma(fit)
