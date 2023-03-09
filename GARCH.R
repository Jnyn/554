# Load the required libraries
library(rugarch)
library(quantmod)
library(PerformanceAnalytics)
library(forecast)
# Load the data
getSymbols("SPY",
           from = "2011-01-01",
           to = "2011-12-05",
           src = "yahoo",
           adjust = TRUE)
return = CalculateReturns(SPY$SPY.Adjusted)
data = return[-c(1),]
arima_model <- auto.arima(data)
mean_spec <- list(armaOrder = c(arima_model$arma[1], arima_model$arma[2]))
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = mean_spec)
fit <- ugarchfit(spec = spec, data = data, solver = "solnp")
vol <- as.numeric(sigma(fit)[1, ])
