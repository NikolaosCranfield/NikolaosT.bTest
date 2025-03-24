salinity_data <- read.csv("C:/Users/georg/OneDrive/Desktop/salinity_data.csv", stringsAsFactors=TRUE)
View(salinity_data)


head("C:/Users/georg/OneDrive/Desktop/salinity_data.csv")


salinity_data$dt <- seq(as.Date("01/28/2013", format = "%m/%d/%Y"), length.out = nrow(salinity_data), by = "1 month")



min<-min(salinity_data$dt)
max <- max(salinity_data$dt)
new.df <- data.frame(date = seq(min, max, by = "1 month"))

new.df <- merge(new.df, data.frame(date = salinity_data$dt,  Sal = salinity_data$Salinity), by = "date", all.x = T)

new.df$Sal <- na.approx(new.df$Sal
                       #, max.gap = 5
)
View(new.df)


my.w = analyze.wavelet(
  new.df,
  "Sal",
  
  loess.span = 0,
  
  dt = 1,
  
  dj = 1 / 50,
  
  lowerPeriod = 2,
  
  upperPeriod = 160,
  
  make.pval = F,
  
  n.sim = 1000
  
)


wt.image(my.w, main = "Wavelet Power Spectrum of Salinity", legend.params = list(lab = "wavelet power levels"),
                       
                       periodlab = "Period (Months)", show.date = TRUE,
                       date.format = "%Y", timelab = "")

#wt.image(my.w, main = "Wavelet Power Spectrum", legend.params = list(lab = "wavelet power levels"), 
         periodlab = "Period (Months)", timelab = "Time (Years)", date.format = "%Y", spec.time.axis = 
           list(at = seq(2013, 2022, by = 1), labels = seq(2013, 2022, by = 1)))



plot(salinity_data$dt, salinity_data$Salinity, xlab = "Time (Years)", ylab = "Salinity (ppt)", type = "l")

acf(new.df$Sal, main = "Salinity")
pacf(new.df$Sal, main = "Salinity", lag.max = 12, plot = TRUE)
lag.plot(new.df$Sal, main = "Salinity", lags = 12, do.lines = FALSE)

         

#convert this to a date format

new.df$dt <- as.POSIXct(new.df$dt, format = "%Y-%m")


view(new.df$dt)

#dataframe passed to the wavelet transform, date column needs to be called 'date'

df <- data.frame(date = new.df$dt, Sal = new.df$Sal)


df$salinity_interpolate <- na.approx(new.df$Sal
                               #, max.gap = 5
)

min(df$salinity_interpolate)
max(df$salinity_interpolate)

df$salinity_data$dt <- runif(121, min = 0, max = 121) # fake data

#data <- #your data

window <- 16
max.lag <- window -1 

data.v <- salinity_data$Salinity

pcf.results <- matrix(0, nrow = length(data.v) - window, ncol = max.lag)

start.inds <- 1:(length(data.v) - window)

for (i in start.inds) {
  
  start <- i
  end <- start + (window - 1)
  
  subset <- data.v[start:end]
  
  pcf.obj <- pacf(subset, lag.max= max.lag, plot = F)
  pcf.v <- as.vector(pcf.obj$acf)
  
  pcf.results[i, ] <- pcf.v
  
  print(i)
  
}

image(pcf.results)

image(pcf.results, main = "Rolling Autocorrelation of Salinity", xlab = "Dates", ylab = "Lag(Months)") + 
  legend(x= 0, y = 0.75, abbreviate(levels(pcf.results), minlength=1), pch= as.numeric(pcf.results), 
         col= "yellow", yjust=0) 

library(reshape)

df <- melt(pcf.results)
view(df)
library(ggplot2)
figa<-ggplot(df, aes(x= Var1, y= Var2, fill = value)) + 
  geom_tile()+
  ggtitle("Rolling Autocorrelation of Salinity ") +
  xlab("Date (Months)")+ 
  ylab("Lag (Months)")+
  scale_fill_gradient(low="yellow", high="red")+
  theme_bw()+
  font("title", size = 20, face = "bold.italic", color = "black")+
  font("xylab", size = 15, face = "bold", color = "black")+
  font("xy.text", size = 10, face = "bold", color = "black")+
  font("legend.text", size = 10)+
  font("legend.title", face = "bold", size = 10)
ggsave("figa.png",width=30, height=20)
plot(figa)

      

