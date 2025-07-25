c3.in$dt <- as.Date(c3.in$Date, "%d/%m/%y")
min <- min(c3.in$dt)
max <- max(c3.in$dt)
new.df <- data.frame(dt=seq(min, max, by = "1 day"))
final.df <-merge(new.df, c3.in, by="dt", all.x = TRUE)
ggplot(c3.in, aes(dt, TP)) + geom_line() + scale_x_date('Month') + ylab('Total phosphorus level') + xlab("")
ggplot(c3.in, aes(dt, TP)) + geom_point(color = "navyblue") + facet_wrap( ~ TP) + scale_x_date('Month') +
  ylab('Total phosphorus level') + xlab("")
count_TSObject = ts(c3.in[,c('TP')])
c3.in$clean_count = tsclean(count_TSObject)
ggplot() + geom_line(data = c3.in, aes(x = dt, y = clean_count)) + ylab('Cleaned Count')
c3.in$cnt_ma = ma(c3.in$clean_count, order = 7)
c3.in$cnt_ma30 = ma(c3.in$clean_count, order = 30)
ggplot() + geom_line(data = c3.in, aes(x = dt, y = clean_count, colour = "Counts")) +
geom_line(data = c3.in, aes(x = dt, y = cnt_ma, colour = "Weekly Moving Average")) +
geom_line(data = c3.in, aes(x = dt, y = cnt_ma30, colour = "Monthly Moving Average")) +
  ylab('TP Mg/L count')
count_ma = ts(na.omit(c3.in$cnt_ma), frequency = 30)
decomp = stl(count_ma, s.window = "periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)
adf.test(count_ma, alternative = "stationary")
Acf(count_ma, main = '')
Pacf(count_ma, main = '')
count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")
Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differnced Series')
auto.arima(deseasonal_cnt, seasonal = FALSE)
fit<-auto.arima(deseasonal_cnt, seasonal = FALSE)
tsdisplay(residuals(fit), lag.max = 45, main = '(1,1,1) Model Residuals')
fit2 = arima(deseasonal_cnt, order = c(1,1,7))
tsdisplay(residuals(fit2), lag.max = 20, main = 'Seasonal Model Residuals')
par(mfrow = c(1,1))
fcast <- forecast(fit2, h = 30)
plot(fcast)
hold <- window(ts(deseasonal_cnt), start = 53)
fit_no_holdout = arima(ts(deseasonal_cnt[-c(53:87)]), order = c(1,1,7))
fcast_no_holdout <- forecast(fit_no_holdout,h=35)
plot(fcast_no_holdout, main= " ")
lines(ts(deseasonal_cnt))
fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
seas_fcast <- forecast(fit_w_seasonality, h=0)
plot(seas_fcast)
lines(ts(count_ma))
lines(ts(deseasonal_cnt))
tsdisplay(residuals(fit_w_seasonality), lag.max = 15, main = 'Seasonal Model Residuals')
fit3 = auto.arima(deseasonal_cnt, seasonal = FALSE)
tsdisplay(residuals(fit3), lag.max = 15, main = 'Seasonal Model Residuals')
fit4 = arima(deseasonal_cnt, order = c(1,1,7))
tsdisplay(residuals(fit4), lag.max = 15, main = 'Seasonal Model Residuals')
fit5 = arima(deseasonal_cnt, order = c(1,1,1))
tsdisplay(residuals(fit5), lag.max = 15, main = 'Seasonal Model Residuals')
par(mfrow=c(2,2))
fcast <- forecast(fit_w_seasonality, h = 30)
plot(fcast)
fcast2 <- forecast(fit3, h = 30)
plot(fcast2)
fcast3 <- forecast(fit4, h = 30)
plot(fcast3)
fcast4 <- forecast(fit5)
plot(fcast4)




library(ggplot2)
ggplot(c3.in, aes(x=Date , y=TP)) + geom_line() +
  labs(x="Date" , y="TP") + ggtitle("Time series for STA1E cell 3 inflow.") +
  theme(plot.title = element_text(hjust=0.5))

