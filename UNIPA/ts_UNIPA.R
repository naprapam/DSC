library(fpp)
library(astsa)
library(DT)
library(dygraphs)
library(TSA)

data(rwalk)
rwalk
rwalk.ts = ts(rwalk, frequency = 12)
rwalk.ts

#Time Series Plot

rwalk.ts.qtr = aggregate(rwalk.ts, nfrequency=4)
rwalk.ts.qtr
rwalk.ts.yr = aggregate(rwalk.ts, nfrequency=1)
rwalk.ts.yr
win.graph()
par(mfrow=c(2,2))
plot.ts(rwalk.ts, main = "Monthly Rwalk", xlab = "Year", ylab = "ML")
plot.ts(rwalk.ts.qtr, main = "Quarterly Rwalk", xlab = "Year", ylab = "ML", type="o")
plot.ts(rwalk.ts.yr, main = "Yearly Rwalk", xlab = "Year", ylab = "ML")

win.graph()
par(mfrow=c(2,1))
monthplot(rwalk.ts, main = "Monthly Rwlk-Monthplot", xlab="Month", ylab = "ML")
boxplot(rwalk.ts~cycle(rwalk.ts), xlab="Month", ylab="ML", main="Monthly Rwalk-Boxplot")

#MA plot
win.graph()
par(mfrow=c(2,2))
plot(rwalk.ts, col="gray", main="1 Year Moving Average Smoothing")
lines(ma(rwalk.ts, order = 12), col="red", lwd=3)
plot(rwalk.ts, col="gray", main="2 Year Moving Average Smoothing")
lines(ma(rwalk.ts, order = 24), col="blue", lwd=3)
plot(rwalk.ts, col="gray", main="3 Year Moving Average Smoothing")
lines(ma(rwalk.ts, order = 36), col="green", lwd=3)
plot(rwalk.ts, col="gray", main="4 Year Moving Average Smoothing")
lines(ma(rwalk.ts, order = 48), col="yellow4", lwd=3)

#Decomposition
win.graph()
plot(decompose(rwalk.ts))

#Basic Models Forecast
rwalk.fit.a = meanf(rwalk.ts, h=60)
rwalk.fit.n = naive(rwalk.ts, h=60)
rwalk.fit.sn = snaive(rwalk.ts, h=60)
rwalk.fit.dri = rwf(rwalk.ts, h=60, drift = TRUE)
win.graph()
plot.ts(rwalk.ts, main="Monthly rwalk", xlab="Year", ylab="ML",
        xlim=c(1,10))
lines(rwalk.fit.a$mean, col="blue")
lines(rwalk.fit.n$mean, col="yellow4")
lines(rwalk.fit.sn$mean, col="seagreen4")
lines(rwalk.fit.dri$mean, col="red")
legend("topleft",lty=1, col = c("blue","yellow4","seagreen4","red"),
       legend = c("Mean Method","Naive Method","Seasonal Naive Method",
                  "Drift Naive Method"))

#Regression Analysis (Trend)
rwalk.fit.lm = tslm(rwalk.ts~trend)
f = forecast(rwalk.fit.lm, h=60, level = c(5,10))
win.graph()
plot.ts(rwalk.ts, main = "Monthly rwalk", xlab="Year", ylab="ML",
        xlim=c(1,8))
lines(f$fitted, col="blue")
lines(f$mean, col="red")
summary(rwalk.fit.lm)

#Regression Analysis (Trend + Seasonal)
rwalk.fit.lm2 = tslm(rwalk.ts~trend+season)
f2 = forecast(rwalk.fit.lm2, h=60, level = c(5,10))
win.graph()
plot.ts(rwalk.ts, main = "Monthly rwalk", xlab="Year", ylab="ML", 
        xlim=c(1,8))
lines(f2$fitted, col="blue")
lines(f2$mean, col="red")
summary(rwalk.fit.lm2)

#Single Exponential Smoothing
rwalk.fit.ses1=ses(rwalk.ts, initial = "simple", h=12)
rwalk.fit.ses2=ses(rwalk.ts, alpha = 0.2, initial = "simple", h=12)
rwalk.fit.ses3=ses(rwalk.ts, alpha = 0.6, initial = "simple", h=12)
win.graph()
plot(rwalk.fit.ses1, plot.conf=FALSE, type = "o", 
     main = "Monthly rwalk", xlab = "Year", ylab = "ML")
lines(rwalk.fit.ses1$fitted, col="blue", type = "o")
lines(rwalk.fit.ses2$fitted, col="green", type = "o")
lines(rwalk.fit.ses3$fitted, col="red", type = "o")
lines(rwalk.fit.ses1$mean, col="blue", type = "o")
lines(rwalk.fit.ses2$mean, col="green", type = "o")
lines(rwalk.fit.ses3$mean, col="red", type = "o")
legend("topleft", lty = 1, col=c(1,"blue","green","red"),
       c("data",expression(alpha==0.87), expression(alpha==0.2),
         expression(alpha==0.6)), pch = 1)

#Double Exponential Smoothing
rwalk.fit.des1=holt(rwalk.ts, alpha = 0.2, beta = 0.2, initial = "simple", h=12)
rwalk.fit.des2=holt(rwalk.ts, alpha = 0.2, beta = 0.2, initial = "simple", h=12,
                    exponential = T)
rwalk.fit.des3=holt(rwalk.ts, alpha = 0.2, beta = 0.2, h=12, damped = T)
win.graph()
plot(rwalk.fit.des1, type = "o", fcol="white", main = "Monthly rwalk",
     xlab = "Year", ylab = "ML", plot.conf=F)
lines(rwalk.fit.des1$fitted, col="blue", type = "o")
lines(rwalk.fit.des2$fitted, col="green", type = "o")
lines(rwalk.fit.des3$fitted, col="red", type = "o")
lines(rwalk.fit.des1$mean, col="blue", type = "o")
lines(rwalk.fit.des2$mean, col="green", type = "o")
lines(rwalk.fit.des3$mean, col="red", type = "o")
legend("topleft", lty = 1, col=c(1,"blue","green","red"),
       c("Data","Linear", "Exponential", "Damped"), pch = 1)

#Winter's
rwalk.fit.w1=hw(window(rwalk.ts,start=3), h=12, seasonal = "additive")
rwalk.fit.w2=hw(window(rwalk.ts,start=3), h=12, seasonal = "multiplicative")
win.graph()
plot(rwalk.fit.w1, type = "o", fcol="white", main = "Monthly rwalk",
     xlab = "Year", ylab = "ML", plot.conf=F)
lines(rwalk.fit.w1$fitted, col="blue", type = "o")
lines(rwalk.fit.w2$fitted, col="green", type = "o")
lines(rwalk.fit.w1$mean, col="blue", type = "o")
lines(rwalk.fit.w2$mean, col="green", type = "o")
legend("topleft", lty = 1, col=c(1,"blue","green"),
       c("Data","Winter's Additive", "Winter's Multiplicative"), pch = 1)

