library(rugarch)
library(xlsx)
library(xts)
df <- read.xlsx("D:/NYU/Engle- Financial Econometrics/research/AAPL.xlsx",1)
date <- as.POSIXct(strptime(df[,1], "%Y-%m-%d"))
df = xts(df[,c(2,3)], order.by=date)
returns <- 100 * diff(log(df$AAPL_Price))
IV_insample <- df$AAPL_IV['2008-04-29/2013-04-26']
IV_pred <- df$AAPL_IV['2013-04-27/2018-04-27']

######################################################################################################
# GARCH(1,1) IN-SAMPLE ESTIMATION
prediction_garch <- vector()
spec_garch = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))
fit_garch = ugarchfit(spec_garch, returns['2008-04-29/2013-04-26'], solver = 'hybrid')
show(fit_garch)
plot(sigma(fit_garch)*sqrt(255))

RANG <- df$AAPL_IV['2013-04-27/2018-04-27']
length(RANG)


# GARCH(1,1) ROLLING
for (i in (length(returns)-1260):(length(returns)-1)) {
spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                                        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))
fit = ugarchfit(spec, returns[index(returns)[2:i]], solver = 'hybrid')
pre <- ugarchforecast(fit, n.ahead = 22)
prediction_garch <- c(prediction_garch, mean(sigma(pre)*sqrt(255)))
}
prediction_garch
IV <- tail(df$AAPL_IV, 1260)
index(IV)[1005:1006]

# ROLLING REGRESSION WITH GARCH ERROR TERM
pred_vv <- vector()
for (i in (length(IV)-255):(length(IV)-1)) {
spec_GARCH_reg = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                            mean.model = list(armaOrder = c(0, 0), 
                                              external.regressors = cbind(log(prediction_garch[index(prediction_garch)[1:i]]))))
fit_GARCH_reg = ugarchfit(spec=spec_GARCH_reg, data=log(IV[index(IV)[1:i]]), solver='hybrid')
pre <- ugarchforecast(fit_GARCH_reg, n.ahead = 1)
pred_vv <- c(pred_vv, sigma(pre))
}
length(pred_vv)

# OUT OF SAMPLE REGRESSION PREDICTION
spec_GARCH_reg = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                            mean.model = list(armaOrder = c(0, 0), 
                                              external.regressors = cbind(log(prediction_garch[index(prediction_garch)[1:1005]]))))
fit_GARCH_reg = ugarchfit(spec=spec_GARCH_reg, data=log(IV[index(IV)[1:1005]]))
show(fit_GARCH_reg)
rand <- rnorm(255, 0, 1)
pre_loggarch_reg <- 1.348545 + 0.543384 * log(prediction_garch[index(prediction_garch)[1006:1260]]) + rand * pred_vv
pre_garch_reg <- exp(pre_loggarch_reg)
mae_garch_reg <- sum(abs(pre_garch_reg - tail(IV, 255)))/255
mae_garch_reg

mae_garch <- sum(abs(prediction_garch[index(prediction_garch)[1006:1260]] - tail(IV, 255)))/255
mae_garch

######################################################################################################
# GJRGARCH(1,1,1) IN-SAMPLE ESTIMATION
prediction_gjrgarch <- vector()
spec_gjrgarch = ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                         distribution.model = "norm")
fit_gjrgarch = ugarchfit(spec_gjrgarch, returns['2008-04-29/2013-04-26'], solver = 'hybrid')
show(fit_gjrgarch)
plot(sigma(fit_gjrgarch)*sqrt(255))

# GJRGARCH(1,1,1) ROLLING
for (i in (length(returns)-1260):(length(returns)-1)) {
  spec = ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
                    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                    distribution.model = "norm")
  fit = ugarchfit(spec, returns[index(returns)[2:i]], solver = 'hybrid')
  pre <- ugarchforecast(fit, n.ahead = 22)
  prediction_gjrgarch <- c(prediction_gjrgarch, mean(sigma(pre)*sqrt(255)))
}
prediction_gjrgarch


# ROLLING REGRESSION WITH GARCH ERROR TERM
pred_vv <- vector()
for (i in (length(IV)-255):(length(IV)-1)) {
  spec_gjrGARCH_reg = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                              mean.model = list(armaOrder = c(0, 0), 
                                                external.regressors = cbind(log(prediction_gjrgarch[index(prediction_gjrgarch)[1:i]]))))
  fit_gjrGARCH_reg = ugarchfit(spec=spec_gjrGARCH_reg, data=log(IV[index(IV)[1:i]]), solver='hybrid')
  pre <- ugarchforecast(fit_gjrGARCH_reg, n.ahead = 1)
  pred_vv <- c(pred_vv, sigma(pre))
}
length(pred_vv)

# OUT OF SAMPLE REGRESSION PREDICTION
spec_gjrGARCH_reg = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                            mean.model = list(armaOrder = c(0, 0), 
                                              external.regressors = cbind(log(prediction_gjrgarch[index(prediction_gjrgarch)[1:1006]]))))
fit_gjrGARCH_reg = ugarchfit(spec=spec_gjrGARCH_reg, data=log(IV[index(IV)[1:1006]]))
show(fit_gjrGARCH_reg)
rand <- rnorm(255, 0, 1)
pre_loggjrgarch_reg <- 0.832368 + 0.697903 * log(prediction_gjrgarch[index(prediction_gjrgarch)[1006:1260]]) + rand * pred_vv
pre_gjrgarch_reg <- exp(pre_loggjrgarch_reg)
mae_gjrgarch_reg <- sum(abs(pre_gjrgarch_reg - tail(IV, 255)))/255
mae_gjrgarch_reg

mae_gjrgarch <- sum(abs(prediction_gjrgarch[index(prediction_gjrgarch)[1006:1260]] - tail(IV, 255)))/255
mae_gjrgarch
######################################################################################################
# TGARCH(1,1) IN-SAMPLE ESTIMATION
prediction_tgarch <- vector()
spec_tgarch = ugarchspec(variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                         distribution.model = "norm")
fit_tgarch = ugarchfit(spec_tgarch, returns['2008-04-29/2013-04-26'], solver = 'hybrid')
show(fit_tgarch)
plot(sigma(fit_tgarch)*sqrt(255))

# TGARCH(1,1) ROLLING
for (i in (length(returns)-1260):(length(returns)-1)) {
  spec = ugarchspec(variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(1, 1)),
                    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                    distribution.model = "norm")
  fit = ugarchfit(spec, returns[index(returns)[2:i]], solver = 'hybrid')
  pre <- ugarchforecast(fit, n.ahead = 22)
  prediction_tgarch <- c(prediction_tgarch, mean(sigma(pre)*sqrt(255)))
}
prediction_tgarch

# ROLLING REGRESSION WITH GARCH ERROR TERM
pred_vv <- vector()
for (i in (length(IV)-255):(length(IV)-1)) {
  spec_tGARCH_reg = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                                 mean.model = list(armaOrder = c(0, 0), 
                                                   external.regressors = cbind(log(prediction_tgarch[index(prediction_tgarch)[1:i]]))))
  fit_tGARCH_reg = ugarchfit(spec=spec_tGARCH_reg, data=log(IV[index(IV)[1:i]]), solver='hybrid')
  pre <- ugarchforecast(fit_tGARCH_reg, n.ahead = 1)
  pred_vv <- c(pred_vv, sigma(pre))
}
length(pred_vv)

# OUT OF SAMPLE REGRESSION PREDICTION
spec_tGARCH_reg = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                               mean.model = list(armaOrder = c(0, 0), 
                                                 external.regressors = cbind(log(prediction_tgarch[index(prediction_tgarch)[1:1006]]))))
fit_tGARCH_reg = ugarchfit(spec=spec_tGARCH_reg, data=log(IV[index(IV)[1:1006]]))
show(fit_tGARCH_reg)
rand <- rnorm(255, 0, 1)
pre_logtgarch_reg <- 0.999438 + 0.654764 * log(prediction_tgarch[index(prediction_tgarch)[1006:1260]]) + rand * pred_vv
pre_tgarch_reg <- exp(pre_logtgarch_reg)
mae_tgarch_reg <- sum(abs(pre_tgarch_reg - tail(IV, 255)))/255
mae_tgarch_reg

mae_tgarch <- sum(abs(prediction_tgarch[index(prediction_tgarch)[1006:1260]] - tail(IV, 255)))/255
mae_tgarch

######################################################################################################
# EGARCH(1,1,1) IN-SAMPLE ESTIMATION
prediction_egarch <- vector()
spec_egarch = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                           mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                           distribution.model = "norm")
fit_egarch = ugarchfit(spec_egarch, returns['2008-04-29/2013-04-26'], solver = 'hybrid')
show(fit_egarch)
plot(sigma(fit_egarch)*sqrt(255))
title(main = "conditional volatility estimated in-sample using EGARCH model")

# EGARCH(1,1,1) ROLLING
for (i in (length(returns)-1260):(length(returns)-1)) {
  spec = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                    distribution.model = "norm")
  fit = ugarchfit(spec, returns[index(returns)[2:i]], solver = 'hybrid')
  pre <- ugarchforecast(fit, n.ahead = 22)
  prediction_egarch <- c(prediction_egarch, mean(sigma(pre)*sqrt(255)))
}
prediction_egarch

# ROLLING REGRESSION WITH GARCH ERROR TERM
pred_vv <- vector()
for (i in (length(IV)-255):(length(IV)-1)) {
  spec_eGARCH_reg = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                                 mean.model = list(armaOrder = c(0, 0), 
                                                   external.regressors = cbind(log(prediction_egarch[index(prediction_egarch)[1:i]]))))
  fit_eGARCH_reg = ugarchfit(spec=spec_eGARCH_reg, data=log(IV[index(IV)[1:i]]), solver='hybrid')
  pre <- ugarchforecast(fit_eGARCH_reg, n.ahead = 1)
  pred_vv <- c(pred_vv, sigma(pre))
}
length(pred_vv)

# OUT OF SAMPLE REGRESSION PREDICTION
spec_eGARCH_reg = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                               mean.model = list(armaOrder = c(0, 0), 
                                                 external.regressors = cbind(log(prediction_egarch[index(prediction_egarch)[1:1006]]))))
fit_eGARCH_reg = ugarchfit(spec=spec_eGARCH_reg, data=log(IV[index(IV)[1:1006]]))
show(fit_eGARCH_reg)
rand <- rnorm(255, 0, 1)
pre_logegarch_reg <- 1.41585 + 0.53543 * log(prediction_egarch[index(prediction_egarch)[1006:1260]]) + rand * pred_vv
pre_egarch_reg <- exp(pre_logegarch_reg)
mae_egarch_reg <- sum(abs(pre_egarch_reg - tail(IV, 255)))/255
mae_egarch_reg

mae_egarch <- sum(abs(prediction_egarch[index(prediction_egarch)[1006:1260]] - tail(IV, 255)))/255
mae_egarch

#####################################################################################################
df_insample <- data.frame("IV"=IV_insample, "GARCH"=sigma(fit_garch)*sqrt(255), "EGARCH"=sigma(fit_egarch)*sqrt(255),
                          "GJRGARCH"=sigma(fit_gjrgarch)*sqrt(255), "TGARCH"=sigma(fit_tgarch)*sqrt(255))
df_prediction <- data.frame("IV"=IV_pred, "GARCH"=prediction_garch, "EGARCH"=prediction_egarch, 
                            "GJRGARCH"=prediction_gjrgarch, "TGARCH"=prediction_tgarch)
df_pre_reg <- data.frame("IV"=tail(IV, 255), "GARCH"=pre_garch_reg, "EGARCH"=pre_egarch_reg, 
                            "GJRGARCH"=pre_gjrgarch_reg, "TGARCH"=pre_tgarch_reg)

pairs(~AAPL_IV+GARCH+EGARCH+GJRGARCH+TGARCH, data=df_insample, main="Graph 1 Estimated In-Sample Conditional Volatility Scatterplot Matrix")
pairs(~AAPL_IV+GARCH+EGARCH+GJRGARCH+TGARCH, data=df_prediction, main="Graph 3 Out-Of-Sample Volatility Forecast Scatterplot Matrix")
pairs(~AAPL_IV+GARCH+EGARCH+GJRGARCH+TGARCH, data=df_pre_reg, main="Graph 5 GARCH Regression Volatility Forecast Scatterplot Matrix")

myColors <- c("red", "darkgreen", "goldenrod", "darkblue", "darkviolet")
plot(x = IV_insample, type="l", xlab = "Time", ylab = "Volatility(%)",ylim = c(10, 140), major.ticks="auto", minor.ticks=TRUE, main = "Graph 2 in-sample estimates of conditional volatility", col = "red")
lines(x = as.xts(df_insample["GARCH"]), col = "darkgreen")
lines(x = as.xts(df_insample["EGARCH"]), col = "goldenrod")
lines(x = as.xts(df_insample["GJRGARCH"]), col = "darkblue")
lines(x = as.xts(df_insample["TGARCH"]), col = "darkviolet")
legend(x = 'right', legend = c("IV", "GARCH", "EGARCH", "GJR-GARCH", "TGARCH"),
       lty = 1, col = myColors)

myColors <- c("red", "darkgreen", "goldenrod", "darkblue", "darkviolet")
plot(x = IV_pred, type="l", xlab = "Time", ylab = "Volatility(%)",ylim = c(10, 80), major.ticks="auto", minor.ticks=TRUE, main = "Graph 4 out-of-sample estimate of volatility", col = "red")
lines(x = as.xts(df_prediction["GARCH"]), col = "darkgreen")
lines(x = as.xts(df_prediction["EGARCH"]), col = "goldenrod")
lines(x = as.xts(df_prediction["GJRGARCH"]), col = "darkblue")
lines(x = as.xts(df_prediction["TGARCH"]), col = "darkviolet")
abline(v = as.POSIXct("2017-04-25"), lheight=0.5, lwd=2)
legend(x = 'right', legend = c("IV", "GARCH", "EGARCH", "GJR-GARCH", "TGARCH"),
       lty = 1, col = myColors)

myColors <- c("red", "darkgreen", "goldenrod", "darkblue", "darkviolet")
plot(x = tail(IV, 255), type="l", xlab = "Time", ylab = "Volatility(%)",ylim = c(10, 80), major.ticks="auto", minor.ticks=TRUE, main = "Graph 6 GARCH regression estimate of volatility", col = "red")
lines(x = as.xts(df_pre_reg["GARCH"]), col = "darkgreen")
lines(x = as.xts(df_pre_reg["EGARCH"]), col = "goldenrod")
lines(x = as.xts(df_pre_reg["GJRGARCH"]), col = "darkblue")
lines(x = as.xts(df_pre_reg["TGARCH"]), col = "darkviolet")
legend(x = 'right', legend = c("IV", "GARCH", "EGARCH", "GJR-GARCH", "TGARCH"),
       lty = 1, col = myColors)

mae_garch <- sum(abs(prediction_garch - IV))/1260
mae_garch

mae_egarch <- sum(abs(prediction_egarch - IV))/1260
mae_egarch

mae_gjrgarch <- sum(abs(prediction_gjrgarch - IV))/1260
mae_gjrgarch

mae_tgarch <- sum(abs(prediction_tgarch - IV))/1260
mae_tgarch

acf(diff(log(IV))[2: length(IV)], lag=1)
dm.test(prediction_egarch - IV, prediction_tgarch - IV)

p_ca <- prcomp(df_prediction[, 2:5], center=TRUE, scale=TRUE )
predd1 <- scale(as.matrix(df_prediction[,2:5]),
               center = p_ca$center, scale = p_ca$scale)%*% p_ca$rotation[, 1]
predd2 <- scale(as.matrix(df_prediction[,2:5]),
                center = p_ca$center, scale = p_ca$scale)%*% p_ca$rotation[, 2]
# ROLLING REGRESSION WITH GARCH ERROR TERM
pred_vv <- vector()
for (i in (length(IV)-255):(length(IV)-1)) {
  spec_pcaGARCH_reg = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                               mean.model = list(armaOrder = c(0, 0), 
                                                 external.regressors = cbind(predd1[1:i])))
  fit_pcaGARCH_reg = ugarchfit(spec=spec_pcaGARCH_reg, data=log(IV[index(IV)[1:i]]), solver='hybrid')
  pre <- ugarchforecast(fit_pcaGARCH_reg, n.ahead = 1)
  pred_vv <- c(pred_vv, sigma(pre))
}
length(pred_vv)

# OUT OF SAMPLE REGRESSION PREDICTION
spec_pcaGARCH_reg = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                             mean.model = list(armaOrder = c(0, 0), 
                                               external.regressors = cbind(predd1[1:1006])))

fit_pcaGARCH_reg = ugarchfit(spec=spec_pcaGARCH_reg, data=log(IV[index(IV)[1:1006]]))
show(fit_pcaGARCH_reg)
pre_logpcagarch_reg <-  3.160514 - 0.048081 * predd1[1006:1260] + rand * pred_vv
pre_pcagarch_reg <- exp(pre_logpcagarch_reg)
mae_pcagarch_reg <- sum(abs(pre_pcagarch_reg - tail(IV, 255)))/255
mae_pcagarch_reg

