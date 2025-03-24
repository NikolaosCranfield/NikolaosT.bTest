acfma1=ARMAacf(ma=c(0.7), lag.max=10)
lags=0:10
plot(lags,acfma1,xlim=c(1,10), ylab="r",type="h", main = "ACF for MA(1) with theta1 = 0.7")
abline(h=0)
xc=arima.sim(n=150, list(ma=c(0.7)))
x=xc+10
plot(x,type="b", main="Simulated MA(1) data")
acf(x, xlim=c(1,10), main="ACF for simulated sample data") 
acfma2=ARMAacf(ma=c(0.5,0.3), lag.max=10)
acfma2
lags=0:10
plot(lags,acfma2,xlim=c(1,10), ylab="r",type="h", main = "ACF for MA(2) with theta1 = 0.5,theta2=0.3")
abline(h=0)
xc=arima.sim(n=150, list(ma=c(0.5, 0.3)))
x=xc+10
plot(x, type="b", main = "Simulated MA(2) Series")
acf(x, xlim=c(1,10), main="ACF for simulated MA(2) Data")
