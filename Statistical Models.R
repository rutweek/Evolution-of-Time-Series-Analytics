## Installing Libraries

install.packages("forecast")
library(forecast)

# Create data frame.
df <- read.csv(file.choose() , header = TRUE, row.names = "Date")
head(df)
tail(df)
View(df)

# creating Time seriies Data
dfts <- ts(df$High, start = c(2006, 1), end = c(2013, 12), frequency = 365 )
head(dfts)
tail(dfts)
length(dfts)
View(dfts)
summary(dfts)

#stl plot
stlplot <- stl(dfts, s.window = "periodic")
plot(stlplot)
autoplot(stlplot, main = "American Express Time Series Component")

#autocoreelation plot

autocor <- Acf(dfts, lag.max = 365, main = "Autocorrelation forAmerican Express")

#display lags
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)



######################################Train | valiation split####################################################

len<-length(dfts)
len*0.20
len
tail(dfts)
View(dfts)

nValid <- 513
nTrain <- length(dfts) - nValid
nTrain <- length(dfts) - nValid
train.ts <- window(dfts, start = c(2006, 1), end = c(2006, nTrain))
valid.ts <- window(dfts, start = c(2006, nTrain + 1), 
                   end = c(2006, nTrain + nValid))

plot(train.ts, xlab = "Time", ylab = "Stock Prices", ylim = c(0, 110), bty = "l",
     xaxt = "n", xlim = c(2006, 2015.25), main = "", lwd = 2) 
axis(1, at = seq(2006, 2014, 1), labels = format(seq(2006, 2014, 1)))
lines(valid.ts, col = "black", lty = 1, lwd = 2)

lines(c(2012, 2012), c(0, 110))
lines(c(2013.05,2013.05), c(0, 110))
text(2008.50, 110, "Training")
text(2012.5, 110, "Validation")
text(2014, 110, "Future")
arrows(2006, 105, 2010.75 ,105, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2012.25, 105, 2012.75, 105, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.25, 105, 2014, 105, code = 3, length = 0.1,
       lwd = 1, angle = 30)


############################# BASE Model - Naive and Seasonal naive ###########################

df.naive.pred <- naive(train.ts, h = nValid)
df.snaive.pred <- snaive(train.ts, h = nValid)

# plot the predictions for naive forecast.
plot(df.naive.pred$mean, 
     xlab = "Time", ylab = "Stock Value", ylim = c(0, 100), bty = "l",
     xaxt = "n", xlim = c(2006, 2015.25), main = "Naive Forecast", col = "blue", lwd =2) 
axis(1, at = seq(2006, 2015, 1), labels = format(seq(2006, 2015, 1)))
lines(df.naive.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)


lines(c(2012, 2012), c(0, 110))
lines(c(2013.05,2013.05), c(0, 110))
text(2008.50, 110, "Training")
text(2012, 110, "Validation")
text(2014, 110, "Future")
arrows(2006, 105, 2010.75 ,105, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2011.25, 105, 2012.75, 105, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.25, 105, 2015, 105, code = 3, length = 0.1,
       lwd = 1, angle = 30)

plot(df.naive.pred$mean, 
     xlab = "Time", ylab = "Stock Price", ylim = c(0, 110), bty = "l",
     xaxt = "n", xlim = c(2006, 2015.25), main = " Naive Forecast", col = "blue", lwd =2) 
axis(1, at = seq(2006, 2015, 1), labels = format(seq(2006, 2015, 1)))
lines(df.naive.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)


# plot the predictions for seasonal naive forecast.
plot(df.snaive.pred$mean, 
     xlab = "Time", ylab = "Stock Value", ylim = c(0, 100), bty = "l",
     xaxt = "n", xlim = c(2006, 2015.25), main = "Seasonal Naive Forecast", col = "blue", lwd =2) 
axis(1, at = seq(2006, 2015, 1), labels = format(seq(2006, 2015, 1)))
lines(df.snaive.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)


lines(c(2011, 2011), c(0, 110))
lines(c(2013.05,2013.05), c(0, 110))
text(2008.50, 110, "Training")
text(2012, 110, "Validation")
text(2014, 110, "Future")
arrows(2006, 105, 2010.75 ,105, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2011.25, 105, 2012.75, 105, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.25, 105, 2015, 105, code = 3, length = 0.1,
       lwd = 1, angle = 30)




round(accuracy(df.naive.pred$mean, valid.ts), 3)
round(accuracy(df.snaive.pred$mean, valid.ts), 3)


############################### SES ###############################################


ses.orig <- ets(dfts, model = "ANN", alpha = 0.2)

ses.orig.pred <- forecast(ses.orig, h = 600, level = 0)
ses.orig.pred



# & no seasonality (N). Use optimal alpha to fit SES over the training period.
ses.opt <- ets(dfts, model = "ANN")
ses.opt


ses.opt.pred <- forecast(ses.opt, h = 600, level = 0)
ses.opt.pred



plot(ses.orig.pred, 
     xlab = "Time", ylab = "Stocks Prices", ylim = c(0, 150), bty = "l",
     xaxt = "n", xlim = c(2006, 2015.25), lwd = 2,
     main = "Original Data and SES Forecast, Alpha = 0.2", 
     flty = 5) 
axis(1, at = seq(2006, 2015, 1), labels = format(seq(2006, 2015, 1)))
lines(ses.orig.pred$fitted, col = "blue", lwd = 2)


lines(c(2013.05, 2013.05), c(0, 150))
text(2009.50, 150, "Training")
text(2014, 150, "Future")
arrows(2006.5 - 0.5, 145, 2012.75, 145, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.25, 145, 2015, 145, code = 3, length = 0.1,
       lwd = 1, angle = 30)

plot(ses.opt.pred, 
     xlab = "Time", ylab = "Stocks Prices", ylim = c(0, 150), bty = "l",
     xaxt = "n", xlim = c(2006, 2015.05), lwd = 2,
     main = "Original Data and SES Optimal Forecast, Alpha = 0.1456", 
     flty = 5) 
axis(1, at = seq(2006, 2015, 1), labels = format(seq(2006, 2015, 1)))
lines(ses.opt.pred$fitted, col = "blue", lwd = 2)

lines(c(2013.05, 2013.05), c(0, 150))
text(2009.50, 150, "Training")
text(2014, 150, "Future")
arrows(2006.5 - 0.5, 145, 2012.75, 145, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.25, 145, 2015, 145, code = 3, length = 0.1,
       lwd = 1, angle = 30)


round(accuracy(ses.orig.pred$fitted, dfts), 3)
round(accuracy(ses.opt.pred$fitted, dfts),3)



################################## Holt's Model #############################################

h.AAN <- ets(train.ts, model = "MMN", alpha = 0.1, beta = 0.1)
h.AAN


h.AAN.pred <- forecast(h.AAN, h = nValid, level = 0)
h.AAN.pred
 
h.AAN.opt <- ets(train.ts, model = "AAN")
h.AAN.opt

  
h.AAN.opt.pred <- forecast(h.AAN, h = nValid, level = 0)
h.AAN.opt.pred

plot(h.AAN.opt.pred, 
     xlab = "Time", ylab = "Stock", ylim = c(0, 150), bty = "l",
     xaxt = "n", xlim = c(2006, 2014.25), 
     main = "Holt's Multiplicative  Model with Optimal Smoothing Parameters", flty = 2) 
axis(1, at = seq(2006, 2014, 1), labels = format(seq(2006, 2014, 1)))
lines(h.AAN.opt.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts)


lines(c(2012, 2012), c(0, 150))
lines(c(2013.05,2013.05), c(0, 150))
text(2009, 150, "Training")
text(2012.5, 150, "Validation")
text(2013.60, 150, "Future")
arrows(2006, 145, 2011.75 ,145, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2012.15, 145, 2012.85, 145, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.25, 145, 2014, 145, code = 3, length = 0.1,
       lwd = 1, angle = 30)


################################## Holt's Winter Model #############################################

hw.AAA <- ets(train.ts, model = "AAN")
hw.AAA

hw.AAA.pred <- forecast(hw.AAA, h = nValid, level = 0)
hw.AAA.pred

h.AAN.opt.pred <-hw.AAA.pred
plot(h.AAN.opt.pred, 
     xlab = "Time", ylab = "Stock", ylim = c(0, 150), bty = "l",
     xaxt = "n", xlim = c(2006, 2014.25), 
     main = "Holt's Multiplicative  Model with Optimal Smoothing Parameters", flty = 2) 
axis(1, at = seq(2006, 2014, 1), labels = format(seq(2006, 2014, 1)))
lines(h.AAN.opt.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts)

lines(c(2012, 2012), c(0, 150))
lines(c(2013.05,2013.05), c(0, 150))
text(2009, 150, "Training")
text(2012.5, 150, "Validation")
text(2013.60, 150, "Future")
arrows(2006, 145, 2011.75 ,145, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2012.15, 145, 2012.85, 145, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.25, 145, 2014, 145, code = 3, length = 0.1,
       lwd = 1, angle = 30)

hw.ZZZ <- ets(train.ts, model = "ZZN")
hw.ZZZ #


hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred


plot(hw.ZZZ.pred, 
     xlab = "Time", ylab = "Stock", ylim = c(0, 150), bty = "l",
     xaxt = "n", xlim = c(2006, 2014.25), 
     main = "Holt's Winter Multiplicative  Model with Optimal Smoothing Parameters", flty = 2) 
axis(1, at = seq(2006, 2014, 1), labels = format(seq(2006, 2014, 1)))
lines(hw.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts)


lines(c(2012, 2012), c(0, 150))
lines(c(2013.05,2013.05), c(0, 150))
text(2009, 150, "Training")
text(2012.5, 150, "Validation")
text(2013.60, 150, "Future")
arrows(2006, 145, 2011.75 ,145, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2012.15, 145, 2012.85, 145, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.25, 145, 2014, 145, code = 3, length = 0.1,
       lwd = 1, angle = 30)




round(accuracy(ses.orig.pred, valid.ts), 3)
round(accuracy(h.AAN.opt.pred, valid.ts), 3)
round(accuracy(hw.ZZZ.pred, valid.ts), 3)





######################################### Regression ###################################
######################################### Linear Trend##################################



nValid <- 513 
nTrain <- length(dfts) - nValid
train.ts <- window(dfts, start = c(2006, 1), end = c(2006, nTrain))
valid.ts <- window(dfts, start = c(2006, nTrain + 1), 
                   end = c(2006, nTrain + nValid))

train.lin <- tslm(train.ts ~ trend)

summary(train.lin)

train.lin.pred <- forecast(train.lin, h = nValid, level = 0)

plot(train.lin.pred, 
     xlab = "Time", ylab = "Stock Price", ylim = c(0, 120), bty = "l",
     xlim = c(2006, 2014.25), main = "Linear Trend for Training and Validation Data", flty = 2) 
axis(1, at = seq(2006, 2014, 1), labels = format(seq(2006, 2014, 1)) )
lines(train.lin.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lty = 1)
legend(1992,2300, legend = c("Stock Price", "Linear Regression for Training Data",
                             "Linear Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")


lines(c(2012, 2012 ), c(0, 120))
lines(c(2013, 2013), c(0, 120))
text(2009, 120, "Training")
text(2012.5, 120, "Val")
text(2013.60, 120, "Future")
arrows(2006 , 115, 2011.75, 115, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2012.25, 115, 2012.75, 115, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.25, 115, 2014, 115, code = 3, length = 0.1,
       lwd = 1, angle = 30)


round(accuracy(train.lin.pred, valid.ts), 3)


######################### Quadratic Trend #############################

train.quad <- tslm(train.ts ~ trend + I(trend^2))

summary(train.quad)

train.quad.pred <- forecast(train.quad, h = nValid, level = 0)

plot(train.quad.pred, 
     xlab = "Time", ylab = "Stock Price", ylim = c(0, 120), bty = "l",
     xlim = c(2006, 2014.25), main = "Quadratic Trend for Training and Validation Data", flty = 2) 
axis(1, at = seq(2006, 2014, 1), labels = format(seq(2006, 2014, 1)) )
lines(train.lin.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lty = 1)
legend(1992,2300, legend = c("Stock Price ", "Linear Regression for Training Data",
                             "Linear Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")
lines(c(2012, 2012 ), c(0, 120))
lines(c(2013, 2013), c(0, 120))
text(2009, 120, "Training")
text(2012.5, 120, "Val")
text(2013.60, 120, "Future")
arrows(2006 , 115, 2011.75, 115, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2012.25, 115, 2012.75, 115, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.25, 115, 2014, 115, code = 3, length = 0.1,
       lwd = 1, angle = 30)

round(accuracy(train.lin.pred, valid.ts), 3)
round(accuracy(train.quad.pred, valid.ts), 3)



####################### Linear Trend + Seasonality #################################

train.season <- tslm(train.ts ~ season)

summary(train.season)

train.season$data 


train.season.pred <- forecast(train.season, h = nValid, level = 0)


plot(train.season.pred, 
     xlab = "Time", ylab = "Stock Price", ylim = c(0, 120), bty = "l",
     xlim = c(2006, 2014.25), main = "Linear Trend with Seasonality for Training and Validation Data", flty = 2) 
axis(1, at = seq(2006, 2014, 1), labels = format(seq(2006, 2014, 1)) )
lines(train.lin.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lty = 1)
legend(1992,2300, legend = c("Stock Price ", "Linear Regression for Training Data",
                             "Linear Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")


lines(c(2012, 2012 ), c(0, 120))
lines(c(2013, 2013), c(0, 120))
text(2009, 120, "Training")
text(2012.5, 120, "Val")
text(2013.60, 120, "Future")
arrows(2006 , 115, 2011.75, 115, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2012.25, 115, 2012.75, 115, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.25, 115, 2014, 115, code = 3, length = 0.1,
       lwd = 1, angle = 30)




round(accuracy(train.lin.pred, valid.ts), 3)
round(accuracy(train.quad.pred, valid.ts), 3)
round(accuracy(train.season.pred, valid.ts), 3)





######################### Quadratic Trend + Seasonality #########################################

train.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)

summary(train.trend.season)


train.trend.season.pred <- forecast(train.trend.season, h = nValid, level = 0)

plot(train.trend.season.pred, 
     xlab = "Time", ylab = "Stock Price", ylim = c(0, 120), bty = "l",
     xlim = c(2006, 2014.25), main = "Quadratic Trend for Training and Validation Data", flty = 2) 
axis(1, at = seq(2006, 2014, 1), labels = format(seq(2006, 2014, 1)) )
lines(train.lin.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lty = 1)
legend(1992,2300, legend = c("Stock Price ", "Linear Regression for Training Data",
                             "Linear Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(2012, 2012 ), c(0, 120))
lines(c(2013, 2013), c(0, 120))
text(2009, 120, "Training")
text(2012.5, 120, "Val")
text(2013.60, 120, "Future")
arrows(2006 , 115, 2011.75, 115, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2012.25, 115, 2012.75, 115, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.25, 115, 2014, 115, code = 3, length = 0.1,
       lwd = 1, angle = 30)




round(accuracy(train.lin.pred, valid.ts),3)
round(accuracy(train.quad.pred, valid.ts),3)
round(accuracy(train.season.pred, valid.ts),3)
round(accuracy(train.trend.season.pred, valid.ts),3)


##################################### ARIMA ###########################################



train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

 
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
summary(train.auto.arima.pred)

train.auto.arima.pred

plot(train.auto.arima.pred, 
     xlab = "Time", ylab = "Stock Prices)", ylim = c(0, 150), bty = "l",
     xaxt = "n", xlim = c(2006, 2014.25), 
     main = "Auto ARIMA Model", lwd = 2, flty = 5) 
axis(1, at = seq(2006, 2014, 1), labels = format(seq(2006, 2014, 1)))
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(1992,2300, legend = c("Stock Price", "Auto ARIMA Model for Training Period",
                             "Auto ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")


lines(c(2013,2013), c(0, 150))
lines(c(2012, 2012), c(0, 150))
text(2009.5, 150, "Training")
text(2012.5, 150, "Validation")
text(2013.6, 150, "Future")
arrows(2006, 145, 2011.75, 145, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2012.15, 145, 2012.85, 145, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.15, 145, 2014, 145, code = 3, length = 0.1,
       lwd = 1, angle = 30)

round(accuracy(train.auto.arima.pred, valid.ts), 3)

