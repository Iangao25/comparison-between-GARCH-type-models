library(rugarch)
library(xlsx)
library(xts)
library(olsrr)
df <- read.xlsx("D:/NYU/Engle- Financial Econometrics/research/AAPL.xlsx",1)
date <- as.POSIXct(strptime(df[,1], "%Y-%m-%d"))
df = xts(df[,c(2,3)], order.by=date)
returns <- 100 * diff(log(df$AAPL_Price))
IV_insample <- df$AAPL_IV['2008-04-30/2018-01-29']

######################################################################################################
# GARCH(1,1) IN-SAMPLE ESTIMATION
prediction_garch <- vector()
spec_garch = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))
fit_garch = ugarchfit(spec_garch, returns['2008-04-29/2018-01-26'], solver = 'hybrid')
show(fit_garch)
plot(sigma(fit_garch)*sqrt(255))

# GARCH(1,1) ROLLING
for (i in (length(returns)-63):(length(returns)-1)) {
spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                                        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))
fit = ugarchfit(spec, returns[index(returns)[2:i]], solver = 'hybrid')
pre <- ugarchforecast(fit, n.ahead = 22)
prediction_garch <- c(prediction_garch, mean(sigma(pre)*sqrt(255)))
}
IV <- tail(df$AAPL_IV, 63)
mae_garch <- sum(abs(prediction_garch - IV))
mae_garch

# REGRESSION
pred_garch <- vector()
for (i in 1005:1259){
  lr_garch <- lm(log(df_prediction$AAPL_IV[index(df_prediction$AAPL_IV)[1:i]])~log(df_prediction$GARCH[index(df_prediction$GARCH)[1:i]])+lag(log(df_prediction$AAPL_IV[index(df_prediction$AAPL_IV)[1:i]])))
  pred_log_garch <- cbind(1,log(df_prediction$GARCH[index(df_prediction$GARCH)[i+1]]))%*%lr_garch$coefficients
  pred_garch <- c(pred_garch, exp(pred_log_garch))
}
mae_garch_lr <- sum(abs(pred_garch - tail(IV, 255)))/255
mae_garch_lr
mae_garch

lr_garch <- lm(log(df_prediction$AAPL_IV[index(df_prediction$AAPL_IV)[1:1005]])~log(df_prediction$GARCH[index(df_prediction$GARCH)[1:1005]]))
pred_log_garch <- cbind(1,log(df_prediction$GARCH[index(df_prediction$GARCH)[1006:1260]]))%*%lr_garch$coefficients
pred_garch <- exp(pred_log_garch)
mae_garch_lr <- sum(abs(pred_garch - tail(IV, 255)))/255
mae_garch_lr

lr_egarch <- lm(log(df_prediction$AAPL_IV[index(df_prediction$AAPL_IV)[1:1005]])~log(df_prediction$EGARCH[index(df_prediction$EGARCH)[1:1005]]))
pred_log_egarch <- cbind(1,log(df_prediction$EGARCH[index(df_prediction$EGARCH)[1006:1260]]))%*%lr_egarch$coefficients
pred_egarch <- exp(pred_log_egarch)
mae_egarch_lr <- sum(abs(pred_egarch - tail(IV, 255)))/255
mae_egarch_lr

lr_gjrgarch <- lm(log(df_prediction$AAPL_IV[index(df_prediction$AAPL_IV)[1:1005]])~log(df_prediction$GJRGARCH[index(df_prediction$GJRGARCH)[1:1005]]))
pred_log_gjrgarch <- cbind(1,log(df_prediction$GJRGARCH[index(df_prediction$GJRGARCH)[1006:1260]]))%*%lr_gjrgarch$coefficients
pred_gjrgarch <- exp(pred_log_gjrgarch)
mae_gjrgarch_lr <- sum(abs(pred_gjrgarch - tail(IV, 255)))/255
mae_gjrgarch_lr

lr_tgarch <- lm(log(df_prediction$AAPL_IV[index(df_prediction$AAPL_IV)[1:1005]])~log(df_prediction$TGARCH[index(df_prediction$TGARCH)[1:1005]]))
pred_log_tgarch <- cbind(1,log(df_prediction$TGARCH[index(df_prediction$TGARCH)[1006:1260]]))%*%lr_tgarch$coefficients
pred_tgarch <- exp(pred_log_tgarch)
mae_tgarch_lr <- sum(abs(pred_tgarch - tail(IV, 255)))/255
mae_tgarch_lr

plot(lr_garch)
ols_test_normality(lr_tgarch)
summary(lr_garch)
a <- log(df_prediction$AAPL_IV[index(df_prediction$AAPL_IV)[1:1006]])
a_lag <- lag(a)
b <- log(df_prediction$GARCH[index(df_prediction$TGARCH)[1:1006]])
b_lag <- lag(b)
ar(a)
ar(b)
lr <- lm(a~b+a_lag)
summary(lr)
ols_test_normality(lr)
ols_plot_resid_hist(lr_egarch)
plot(lr)


mod.resi <- resid(lr)
qqnorm(mod.resi)
qqline(mod.resi)
######################################################################################################
# GJRGARCH(1,1,1) IN-SAMPLE ESTIMATION
prediction_gjrgarch <- vector()
spec_gjrgarch = ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                         distribution.model = "norm")
fit_gjrgarch = ugarchfit(spec_gjrgarch, returns['2008-04-29/2018-01-26'], solver = 'hybrid')
show(fit_gjrgarch)
plot(sigma(fit_gjrgarch)*sqrt(255))

# GJRGARCH(1,1,1) ROLLING
for (i in (length(returns)-63):(length(returns)-1)) {
  spec = ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
                    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                    distribution.model = "norm")
  fit = ugarchfit(spec, returns[index(returns)[2:i]], solver = 'hybrid')
  pre <- ugarchforecast(fit, n.ahead = 22)
  prediction_gjrgarch <- c(prediction_gjrgarch, mean(sigma(pre)*sqrt(255)))
}
prediction_gjrgarch
IV <- tail(df$AAPL_IV, 63)
mae_gjrGARCH <- sum(abs(prediction_gjrgarch - IV))
mae_gjrGARCH


# REGRESSION 
pred_gjrgarch <- vector()
for (i in 1005:1259){
lr_gjrgarch <- lm(log(df_prediction$AAPL_IV[index(df_prediction$AAPL_IV)[1:i]])~log(df_prediction$GJRGARCH[index(df_prediction$GJRGARCH)[1:i]]))
pred_log_gjrgarch <- cbind(1,log(df_prediction$GJRGARCH[index(df_prediction$GJRGARCH)[i+1]]))%*%lr_gjrgarch$coefficients
pred_gjrgarch <- c(pred_gjrgarch, exp(pred_log_gjrgarch))
}
mae_gjrgarch_lr <- sum(abs(pred_gjrgarch - tail(IV, 255)))/255
mae_gjrgarch_lr
mae_gjrgarch
summary(lr_gjrgarch)
######################################################################################################
# TGARCH(1,1) IN-SAMPLE ESTIMATION
prediction_tgarch <- vector()
spec_tgarch = ugarchspec(variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                         distribution.model = "norm")
fit_tgarch = ugarchfit(spec_tgarch, returns['2008-04-29/2018-01-26'], solver = 'hybrid')
show(fit_tgarch)
plot(sigma(fit_tgarch)*sqrt(255))

# TGARCH(1,1) ROLLING
for (i in (length(returns)-63):(length(returns)-1)) {
  spec = ugarchspec(variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(1, 1)),
                    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                    distribution.model = "norm")
  fit = ugarchfit(spec, returns[index(returns)[2:i]], solver = 'hybrid')
  pre <- ugarchforecast(fit, n.ahead = 22)
  prediction_tgarch <- c(prediction_tgarch, mean(sigma(pre)*sqrt(255)))
}
prediction_tgarch
IV <- tail(df$AAPL_IV, 63)
mae_tgarch <- sum(abs(prediction_tgarch - IV))
mae_tgarch

# REGRESSION
pred_tgarch <- vector()
for (i in 1005:1259){
  lr_tgarch <- lm(log(df_prediction$AAPL_IV[index(df_prediction$AAPL_IV)[1:i]])~log(df_prediction$TGARCH[index(df_prediction$TGARCH)[1:i]]))
  pred_log_tgarch <- cbind(1,log(df_prediction$TGARCH[index(df_prediction$TGARCH)[i+1]]))%*%lr_tgarch$coefficients
  pred_tgarch <- c(pred_tgarch, exp(pred_log_tgarch))
}
mae_tgarch_lr <- sum(abs(pred_tgarch - tail(IV, 255)))/255
mae_tgarch_lr
mae_tgarch
summary(lr_tgarch)
######################################################################################################
# EGARCH(1,1,1) IN-SAMPLE ESTIMATION
prediction_egarch <- vector()
spec_egarch = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                           mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                           distribution.model = "norm")
fit_egarch = ugarchfit(spec_egarch, returns['2008-04-29/2018-01-26'], solver = 'hybrid')
show(fit_egarch)
plot(sigma(fit_egarch)*sqrt(255))
title(main = "conditional volatility estimated in-sample using EGARCH model")

# EGARCH(1,1,1) ROLLING
for (i in (length(returns)-63):(length(returns)-1)) {
  spec = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                    distribution.model = "norm")
  fit = ugarchfit(spec, returns[index(returns)[2:i]], solver = 'hybrid')
  pre <- ugarchforecast(fit, n.ahead = 22)
  prediction_egarch <- c(prediction_egarch, mean(sigma(pre)*sqrt(255)))
}
prediction_egarch
IV <- tail(df$AAPL_IV, 63)

# REGRESSION 
pred_egarch <- vector()
for (i in 1005:1259){
  lr_egarch <- lm(log(df_prediction$AAPL_IV[index(df_prediction$AAPL_IV)[1:i]])~log(df_prediction$EGARCH[index(df_prediction$EGARCH)[1:i]]))
  pred_log_egarch <- cbind(1,log(df_prediction$EGARCH[index(df_prediction$EGARCH)[i+1]]))%*%lr_egarch$coefficients
  pred_egarch <- c(pred_egarch, exp(pred_log_egarch))
}
mae_egarch_lr <- sum(abs(pred_egarch - tail(IV, 255)))/255
mae_egarch_lr
mae_egarch
summary(lr_egarch)



#PCA REGRESSION 


#####################################################################################################
df_insample <- data.frame("IV"=IV_insample, "GARCH"=sigma(fit_garch)*sqrt(255), "EGARCH"=sigma(fit_egarch)*sqrt(255),
                          "GJRGARCH"=sigma(fit_gjrgarch)*sqrt(255), "TGARCH"=sigma(fit_tgarch)*sqrt(255))
df_prediction <- data.frame("IV"=IV, "GARCH"=prediction_garch, "EGARCH"=prediction_egarch, 
                            "GJRGARCH"=prediction_gjrgarch, "TGARCH"=prediction_tgarch)
df_simple_reg <- data.frame("IV"=tail(IV, 255), "GARCH"=pred_garch, "EGARCH"=pred_egarch, 
                         "GJRGARCH"=pred_gjrgarch, "TGARCH"=pred_tgarch)

pairs(~AAPL_IV+GARCH+EGARCH+GJRGARCH+TGARCH, data=df_insample, main="Graph 1 Estimated In-Sample Conditional Volatility Scatterplot Matrix")
pairs(~AAPL_IV+GARCH+EGARCH+GJRGARCH+TGARCH, data=df_prediction, main="Estimated Out-Of-Sample Volatility Scatterplot Matrix")
pairs(~AAPL_IV+GARCH+EGARCH+GJRGARCH+TGARCH, data=df_simple_reg, main="Graph 5 Out-of-Sample Regression Volatility Forecast Scatterplot Matrix")

myColors <- c("red", "darkgreen", "goldenrod", "darkblue", "darkviolet")
plot(x = IV_insample, type="l", xlab = "Time", ylab = "Volatility(%)",ylim = c(10, 150), major.ticks="auto", minor.ticks=TRUE, main = "Graph 2 in-sample estimate of conditional volatility", col = "red")
lines(x = as.xts(df_insample["GARCH"]), col = "darkgreen")
lines(x = as.xts(df_insample["EGARCH"]), col = "goldenrod")
lines(x = as.xts(df_insample["GJRGARCH"]), col = "darkblue")
lines(x = as.xts(df_insample["TGARCH"]), col = "darkviolet")
legend(x = 'right', legend = c("IV", "GARCH", "EGARCH", "GJR-GARCH", "TGARCH"),
       lty = 1, col = myColors)

myColors <- c("red", "darkgreen", "goldenrod", "darkblue", "darkviolet")
plot(x = IV, type="l", xlab = "Time", ylab = "Volatility(%)",ylim = c(15, 40), major.ticks="auto", minor.ticks=TRUE, main = "out-of-sample estimate of volatility", col = "red")
lines(x = as.xts(df_prediction["GARCH"]), col = "darkgreen")
lines(x = as.xts(df_prediction["EGARCH"]), col = "goldenrod")
lines(x = as.xts(df_prediction["GJRGARCH"]), col = "darkblue")
lines(x = as.xts(df_prediction["TGARCH"]), col = "darkviolet")
legend(x = 'bottomright', legend = c("IV", "GARCH", "EGARCH", "GJR-GARCH", "TGARCH"),
       lty = 1, col = myColors)

myColors <- c("red", "darkgreen", "goldenrod", "darkblue", "darkviolet")
plot(x = tail(IV, 255), type="l", xlab = "Time", ylab = "Volatility(%)",ylim = c(10, 60), major.ticks="auto", minor.ticks=TRUE, main = "Graph 6 out-of-sample regression forcast of volatility", col = "red")
lines(x = as.xts(df_simple_reg["GARCH"]), col = "darkgreen")
lines(x = as.xts(df_simple_reg["EGARCH"]), col = "goldenrod")
lines(x = as.xts(df_simple_reg["GJRGARCH"]), col = "darkblue")
lines(x = as.xts(df_simple_reg["TGARCH"]), col = "darkviolet")
legend(x = 'right', legend = c("IV", "GARCH", "EGARCH", "GJR-GARCH", "TGARCH"),
       lty = 1, col = myColors)

length(IV)
library(stats)
ar(IV)
ar(prediction_garch)
