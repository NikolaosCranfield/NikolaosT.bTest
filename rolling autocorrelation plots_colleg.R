library(zoo)
library(WaveletComp)


R_sta_6 <- read.csv("R_sta_6.csv", header = T)



#get month into two-digit format

R_sta_6$month <- sprintf("%02d", R_sta_6$month)



#create new date column as test. 1st of month added to create complete date

R_sta_6$Date_txt <- paste0("01-", R_sta_6$month, "-", R_sta_6$year)



#convert this to a date format

R_sta_6$Date_dt <- as.POSIXct(R_sta_6$Date_txt, format = "%d-%m-%Y")




#dataframe passed to the wavelet transform, date column needs to be called 'date'

df <- data.frame(date = R_sta_6$Date_dt, in_tp_c = R_sta_6$in_tp_c)


df$tp_interpolate <- na.approx(df$in_tp_c
                               #, max.gap = 5
)

min(df$tp_interpolate)
max(df$tp_interpolate)

df$tp_interpolate <- runif(180, min = 0, max = 180) # fake data

#data <- #your data
max.lag <- 24
window <- 30

pcf.results <- matrix(0, nrow = length(df$tp_interpolate) - window, ncol = max.lag)

start.inds <- 1:(length(df$tp_interpolate) - window)

for (i in start.inds) {
  
  start <- i
  end <- start + (window - 1)
  
  subset <- df$tp_interpolate[start:end]
  
  pcf.obj <- pacf(subset, lag.max= max.lag, plot = F)
  pcf.v <- as.vector(pcf.obj$acf)
  
  pcf.results[i, ] <- pcf.v
  
  print(i)
  
}

image(pcf.results, main = "Rolling Autocorrelation STA_6 Inflow", xlab = "Date(Days)", ylab = "Lag(Days)") + 
  legend(x= 0, y = 0.75, abbreviate(levels(pcf.results), minlength=1), pch= as.numeric(pcf.results), 
         col= "yellow", yjust=0) 

library(reshape)

df <- melt(pcf.results)

library(ggplot2)
figa<-ggplot(df, aes(x = Var1, y = Var2, fill = value)) + 
  labs(fill= "Correlation")+
   geom_tile()+
   ggtitle("Rolling Autocorrelation STA_6 Inflow ") +
   xlab("Date(Months)")+ 
   ylab("Lag(Months)")+
   scale_fill_gradient(low="yellow", high="red")+
   theme_bw()+
   font("title", size = 20, face = "bold.italic", color = "black")+
   font("xylab", size = 15, face = "bold", color = "black")+
   font("xy.text", size = 10, face = "bold", color = "black")+
   font("legend.title", size = 10)+
   font("legend.title", face = "bold", size = 10)
ggsave("figa.png",width=30, height=20)
plot(figa)
#######################  

figa<-ggplot(df, aes(x = Var1, y = Var2, fill = value)) + 
  labs(fill= "Correlation")+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab") + 
  ggtitle("Rolling Autocorrelation STA_6 Inflow ") +
  xlab("Date(Months)")+ 
  ylab("Lag(Months)")+
  theme_bw()+
  font("title", size = 20, face = "bold.italic", color = "black")+
  font("xylab", size = 15, face = "bold", color = "black")+
  font("xy.text", size = 10, face = "bold", color = "black")+
  font("legend.title", size = 10)+
  font("legend.title", face = "bold", size = 10)
ggsave("figa.png",width=30, height=20)
plot(figa) 
  




 
 
 
 
 
 
 
 
 
 
 
 
 
  
  
  
  
  


 






9,.
data <- runif(100, min = 0, max = 100) # fake data

#data <- #your data


max.lag <- 20
window <- 30

pcf.results <- matrix(0, nrow = length(data) - window, ncol = max.lag)

start.inds <- 1:(length(data) - window)

for (i in start.inds) {
  
  start <- i
  end <- start + (window - 1)
  
  subset <- data[start:end]
  
  pcf.obj <- pacf(subset, lag.max= max.lag, plot = F)
  pcf.v <- as.vector(pcf.obj$acf)
  
  pcf.results[i, ] <- pcf.v
  
  print(i)
  
}

image(pcf.results, main = "inflow")

