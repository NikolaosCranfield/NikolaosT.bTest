#CODE 29.11.2023 CRANFIELD UNIVERSITY

library(readxl)
library(dplyr)
library(plyr)
library(readr)
library(data.table)
library(zoo)

# filenames <- list.files(path = "C:/Users/georg/OneDrive/Desktop/E", pattern = "*.csv", full.names = TRUE)
# print(filenames)
# 
# fullpath = file.path("C:/Users/georg/OneDrive/Desktop/E", filenames)
# print(filenames)
# 
# tp_interpolate <- lapply(filenames, fread, sep = ",")
# data <- rbindlist(tp_interpolate)
# 
# write.csv(data, file = "combined1", rownames = FALSE)
# 
# dataset <- do.call("rbind", lapply(filenames, FUN = function(files){read.csv(files)}))
# lapply(read.csv)
# 
# 
# Combined <- read.csv("Combined.csv", header = T)

files_path <- "C:/Users/wrust/OneDrive - Cranfield University/postdoc/papers/Nikolaos STAs"
setwd(files_path)

sta_1e <- read.csv('R_sta_1E.csv', header = T, stringsAsFactors = F)
head(sta_1e)

sta_1e$dt <-  paste0("01/", sta_1e$month, "/", sta_1e$year) %>% as.Date(., format = "%d/%m/%Y")

n <- nrow(sta_1e)
start_dt <- paste0("01/", sta_1e$month[1], "/", sta_1e$year[1]) %>% as.Date(., format = "%d/%m/%Y")
end_dt <- paste0("01/", sta_1e$month[n], "/", sta_1e$year[n]) %>% as.Date(., format = "%d/%m/%Y")

dt_series <- seq(start_dt, end_dt, by = "1 month")
sta_1e_df <- data.frame(dt = dt_series)

sta_1e_df <- left_join(sta_1e_df, data.frame(dt = sta_1e$dt, inflow = sta_1e$in_tp_c, outflow = sta_1e$out_tp_c))
sta_1e_df$inflow_int <- na.approx(sta_1e_df$inflow)
sta_1e_df$outflow_int <- na.approx(sta_1e_df$outflow)

#import 34
sta_34 <- read.csv('R_sta_34.csv', header = T, stringsAsFactors = F)
head(sta_34)

sta_34$dt <-  paste0("01/", sta_34$month, "/", sta_34$year) %>% as.Date(., format = "%d/%m/%Y")

n <- nrow(sta_34)
start_dt <- paste0("01/", sta_34$month[1], "/", sta_34$year[1]) %>% as.Date(., format = "%d/%m/%Y")
end_dt <- paste0("01/", sta_34$month[n], "/", sta_34$year[n]) %>% as.Date(., format = "%d/%m/%Y")

dt_series <- seq(start_dt, end_dt, by = "1 month")
sta_34_df <- data.frame(dt = dt_series)

sta_34_df <- left_join(sta_34_df, data.frame(dt = sta_34$dt, inflow = sta_34$in_tp_c, outflow = sta_34$out_tp_c))
sta_34_df$inflow_int <- na.approx(sta_34_df$inflow)
sta_34_df$outflow_int <- na.approx(sta_34_df$outflow)
head(sta_34_df)

#PLOT LAG MATRICES#####################
library(cowplot)
lag_mat <- function(vector, x_lab, limits) {
  
  sta_1e_inflow <- data.frame(lag0 = vector,
                              lag1 = lag(vector, 1),
                              lag2 = lag(vector, 2),
                              lag3 = lag(vector, 3),
                              lag4 = lag(vector, 4),
                              lag5 = lag(vector, 5),
                              lag6 = lag(vector, 6),
                              lag7 = lag(vector, 7),
                              lag8 = lag(vector, 8),
                              lag9 = lag(vector, 9),
                              lag10 = lag(vector, 10),
                              lag11 = lag(vector, 11),
                              lag12 = lag(vector, 12))
  library(reshape2)
  library(ggplot2)
  sta_1e_melt <- melt(sta_1e_inflow, id = "lag0")
  library(plyr)
  
  sta_1e_melt$variable <- factor(sta_1e_melt$variable, paste0("lag", 1:12), labels = paste0('lag n-', 1:12))
  
  cor_na <- function(x, y) {
    
    df <- data.frame(x = x, y = y) %>% na.omit()
    return(round(cor(df$x, df$y), 2))
    
  }
  
  cors <- ddply(sta_1e_melt, c("variable"), summarise, cor = round(cor_na(lag0, value), 2))
  head(cors)
  
  lag_cor <- ggplot(data = sta_1e_melt, aes(x = lag0, y = value)) + 
    geom_point(alpha = 0.4) + theme_bw() +
    geom_abline(intercept = 0, slope = 1) +
    facet_wrap(. ~ variable) + 
    geom_text(data=cors, aes(label=paste("r=", cor, sep="")), x=0, y=x_lab, hjust = 0) + 
    theme(panel.spacing=unit(1,"lines")) + 
    scale_x_continuous(limits = limits, expand = c(0,0)) +
    scale_y_continuous(limits = limits, expand = c(0,0))
  
  return(lag_cor)
}

#1e inflow
lag_mat_1e_in <- lag_mat(sta_1e_df$inflow_int, x_lab = 0.47, limits = c(0, 0.5))
lag_mat_1e_in <- lag_mat_1e_in + 
          xlab('Inflow TP. (mg/L)') + 
          ylab('Lagged Inflow TP. (mg/L)')

ggsave2(file="1e_inflow_lag_matrix.png", 
        lag_mat_1e_in, width = 200, height = 150, units = c("mm"))

#1e outflow
lag_mat_1e_out <- lag_mat(sta_1e_df$outflow_int, x_lab = 0.15, limits = c(0, 0.16))
lag_mat_1e_out <- lag_mat_1e_out + 
  xlab('Outflow TP. (mg/L)') + 
  ylab('Lagged Outflow TP. (mg/L)')

ggsave2(file="1e_outflow_lag_matrix.png", 
        lag_mat_1e_out, width = 200, height = 150, units = c("mm"))

#34 inflow
lag_mat_34_in <- lag_mat(sta_34_df$inflow_int, x_lab = 0.18, limits = c(0, 0.2))
lag_mat_34_in <- lag_mat_34_in + 
  xlab('Inflow TP. (mg/L)') + 
  ylab('Lagged Inflow TP. (mg/L)')

ggsave2(file="34_inflow_lag_matrix.png", 
        lag_mat_34_in, width = 200, height = 150, units = c("mm"))

#34 outflow
lag_mat_34_out <- lag_mat(sta_34_df$outflow_int, x_lab = 0.075, limits = c(0, 0.08))
lag_mat_34_out <- lag_mat_34_out + 
  xlab('Outflow TP. (mg/L)') + 
  ylab('Lagged Outflow TP. (mg/L)')

ggsave2(file="34_outflow_lag_matrix.png", 
        lag_mat_34_out, width = 200, height = 150, units = c("mm"))






#############
# plot(sta_1e_df$inflow_int, type = "l")
# 
# library(astsa)
# 
# astsa::lag.plot(combined, 12)
# 
# 
# 
# 
# library(Hmisc)
# 
# cor.test(Combined$tp_interpolate.inflow, Combined$tp_interpolate.outflow, method = "kendall", exact = FALSE)
# 
# set1 <- c("tp_interpolate.inflow", "tp_interpolate.outflow")
# mynewmatrix<-Combined[set1]
# matrix2<-rcorr(as.matrix(mynewmatrix))
# print(matrix2)
# 
# library(PerformanceAnalytics)
# chart.Correlation(mynewmatrix, main = "Correlations Combined")
# 
# ######################
# 
# library(zoo)
# library(WaveletComp)
# R_sta_34 <- read.csv("R_sta_34.csv", header = T)
# R_sta_34$month <- sprintf("%02d", R_sta_34$month)
# R_sta_34$Date_txt <- paste0("01-", R_sta_34$month, "-", R_sta_34$year)
# R_sta_34$Date_dt <- as.POSIXct(R_sta_34$Date_txt, format = "%d-%m-%Y")
# df <- data.frame(date = R_sta_34$Date_dt, in_tp_l = R_sta_34$in_tp_l)
# df$tp_interpolate <- na.approx(df$in_tp_l
#                                #, max.gap = 5
# )
# write.table(df, file = "sta6new.csv", sep = ",", row.names = FALSE)
# 
# 
# filenames <- list.files(path = "C:/Users/georg/OneDrive/Desktop/D", pattern = "*.csv", full.names = TRUE)
# print(filenames)
# 
# fullpath = file.path("C:/Users/georg/OneDrive/Desktop/D", filenames)
# print(filenames)
# 
# tp_interpolate <- lapply(filenames, fread, sep = ",")
# data <- rbindlist(tp_interpolate)
# 
# 
# dataset <- do.call("rbind", lapply(filenames, FUN = function(files){read.csv(files)}))
# lapply(read.csv)
# 
# 
# ############
# 
# Combined <- read.csv("combinedsta34.csv", header = T)
# Combined$x1<- lag(Combined$tp_interpolate.outflow.Mg.L., k =12, na.rm= TRUE)
# na.omit(Combined)
# Combined$x1[is.na(Combined$x1)] <- 0
# 
# library(Hmisc)
# library(ggpubr)
# 
# cor.test(Combined$tp_interpolate.inflow.Mg.L., Combined$tp_interpolate.outflow.Mg.L., method = "kendall", exact = FALSE)
# 
# 
# formula1 <- (y ~ x)
# Combined$tp_interpolate.inflow.Mg.L.<- as.numeric(Combined$tp_interpolate.inflow.Mg.L.)
# x1<-as.numeric(x1)
# 
# A<- ggplot(data= Combined, aes(x = x1, y = tp_interpolate.inflow.Mg.L.))+ 
#   xlab("Lag Outflow [TP]-Mg/L")+ 
#   ylab("Inflow [TP]-Mg/L")+ 
#   ggtitle("[TP]-Mg/L Inflow-Outflow of STA34") +
#   geom_point(size=2, color="black") +
#   geom_smooth(method = "lm", se=TRUE, formula = formula1, color="black")+
#   stat_poly_eq(formula = formula1,
#                aes(label = paste(..eq.label.., ..rr.label..,sep = "*\", \"*")),
#                label.x = 0.045, 
#                label.y = 0.125,
#                output.type = "expression",
#                geom = "text",
#                color = "black",
#                fontface="bold",
#                size = 6,
#                position = "identity",
#                na.rm = FALSE,
#                show.legend = NA,
#                inherit.aes = TRUE,
#                parse = TRUE)+
#   stat_cor(method = "pearson",
#            aes(label = paste(..r.label.., ..rr.label.., ..p.label.., sep = "*\", \"*")),
#            label.x = 0.03, 
#            label.y = 0.135,
#            output.type = "expression",
#            geom = "text",
#            color = "black",
#            fontface="bold",
#            size = 6,
#            position = "identity",
#            na.rm = FALSE,
#            show.legend = NA,
#            inherit.aes = TRUE,
#            parse = TRUE) +
#   font("title", size = 15, face = "bold.italic", color = "black") +
#   font("xylab", size = 15, face = "bold", color = "black") +
#   font("xy.text", size = 10, face = "bold", color = "black") +
#   font("legend.text", size = 10) +
#   font("legend.title", face = "bold", size = 15)
# plot(A)
# 
# B<- ggplot(data= Combined, aes(x = tp_interpolate.outflow.Mg.L., y = tp_interpolate.inflow.Mg.L.))+ 
#   xlab("Outflow [TP]-Mg/L")+ 
#   ylab("Inflow [TP]-Mg/L")+ 
#   ggtitle("[TP]-Mg/L Inflow-Outflow of STA34") +
#   geom_point(size=2, color="black") +
#   geom_smooth(method = "lm", se=TRUE, formula = formula1, color="black")+
#   stat_poly_eq(formula = formula1,
#                aes(label = paste(..eq.label.., ..rr.label..,sep = "*\", \"*")),
#                label.x = 0.045, 
#                label.y = 0.125,
#                output.type = "expression",
#                geom = "text",
#                color = "black",
#                fontface="bold",
#                size = 6,
#                position = "identity",
#                na.rm = FALSE,
#                show.legend = NA,
#                inherit.aes = TRUE,
#                parse = TRUE)+
#   stat_cor(method = "kendall",
#            aes(label = paste(..r.label.., ..rr.label.., ..p.label.., sep = "*\", \"*")),
#            label.x = 0.03, 
#            label.y = 0.135,
#            output.type = "expression",
#            geom = "text",
#            color = "black",
#            fontface="bold",
#            size = 6,
#            position = "identity",
#            na.rm = FALSE,
#            show.legend = NA,
#            inherit.aes = TRUE,
#            parse = TRUE) +
#   font("title", size = 15, face = "bold.italic", color = "black") +
#   font("xylab", size = 15, face = "bold", color = "black") +
#   font("xy.text", size = 10, face = "bold", color = "black") +
#   font("legend.text", size = 10) +
#   font("legend.title", face = "bold", size = 15)
# plot(B)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# write.table(Combined, file = "sta1enew.csv", sep = ",", row.names = FALSE)
# 
# 
# set1 <- c("tp_interpolate.inflow.Mg.L.", "x")
# 
# mynewmatrix<-Combined[set1]
# 
# matrix2<-rcorr(as.matrix(mynewmatrix))
# print(matrix2)
# 
# 
# lag.plot(mynewmatrix, main = "TP[Mg/L] Inflow-Outflow of STA_1E", lags = 6, do.lines = FALSE)
# 
# 
# 
# ############
# 
# library(sjPlot)
# library(GGally)
# 
# GGally::ggpairs(Combined, columns = 2:3, ggplot2::aes(colour = "tp_interpolate.outflow"))
# 
# 
# 
# #######################
# 
# 
# 
# library(Hmisc)
# 
# cor.test(sta_1Einflow$tp_interpolate, sta_1Eoutflow$tp_interpolate, method = "kendall", exact = FALSE)
# 
# set1 <- c("", "", "rainfall_sum")
# mynewmatrix<-R_sta_6[set1]
# matrix2<-rcorr(as.matrix(mynewmatrix))
# print(matrix2)
# 
# par(mfrow = c(2,1))
# plot(in_tp_c, rainfall_sum)
# abline(lm(in_tp_c~rainfall_sum))
# plot(out_tp_c, rainfall_sum)
# abline(lm(out_tp_c ~ rainfall_sum))
# 
# library(car)
# 
# scatterplot(in_tp_c, rainfall_sum, main = "Inflow STA_6")
# scatterplot(out_tp_c, rainfall_sum, main = "Outflow STA_6")
# library(PerformanceAnalytics)
# chart.Correlation(mynewmatrix, main = "Correlations STA_6")
# 
# 
# #######
# 
# library(zoo)
# library(WaveletComp)
# 
# 
# R_sta_6 <- read.csv("R_sta_6.csv", header = T)
# 
# 
# 
# #get month into two-digit format
# 
# R_sta_6$month <- sprintf("%02d", R_sta_6$month)
# 
# 
# 
# #create new date column as test. 1st of month added to create complete date
# 
# R_sta_6$Date_txt <- paste0("01-", R_sta_6$month, "-", R_sta_6$year)
# 
# 
# 
# #convert this to a date format
# 
# R_sta_6$Date_dt <- as.POSIXct(R_sta_6$Date_txt, format = "%d-%m-%Y")
# 
# 
# 
# 
# #dataframe passed to the wavelet transform, date column needs to be called 'date'
# 
# df <- data.frame(date = R_sta_6$Date_dt, in_tp_c = R_sta_6$in_tp_c)
# 
# 
# df$tp_interpolate <- na.approx(df$in_tp_c
#                                #, max.gap = 5
# )
# 
# min(df$tp_interpolate)
# max(df$tp_interpolate)
# 
# df$tp_interpolate <- runif(180, min = 0, max = 180) # fake data
# 



#ROLLING CORRELATION MATRIX############

#data <- #your data
max.lag <- 24
window <- 30

roll_cor_plot <- function(dates, vector, max.lag, window) {
  
  pcf.results <- matrix(NA, nrow = length(vector), ncol = max.lag)
  start.inds <- 1:(length(vector) - window)
  
  for (i in start.inds) {
    
    start <- i
    end <- start + (window - 1)
    
    subset <- vector[start:end]
    
    pcf.obj <- pacf(subset, lag.max = max.lag, plot = F)
    pcf.v <- as.vector(pcf.obj$acf)
    
    pcf.results[end + 1, ] <- pcf.v
    
    print(i)
    
  }
  
  pcf_melt <- melt(pcf.results)
  pcf_melt$dt <- dates[pcf_melt$Var1]
  
  plot <- ggplot(pcf_melt)  + 
    geom_tile(aes(x = dt, y = Var2, fill = value, colour = value)) + 
    scale_fill_distiller(palette = "Spectral", limits = c(-1, 1), breaks = round(seq(-1, 1, by = 0.2), 1)) + 
    scale_colour_distiller(palette = "Spectral", limits = c(-1, 1),  breaks = round(seq(-1, 1, by = 0.2), 1)) +
    scale_x_date(expand = c(0,0), breaks = "2 year", date_labels = "%Y") + scale_y_continuous(expand = c(0,0), breaks = 1:24) + 
    guides(fill=guide_legend(title="Pearson's r"), colour=guide_legend(title="Pearson's r")) + 
    ylab('Lag') + 
    xlab('Date') + theme_bw()
  
  return(plot)
  
}

sta1e_in_roll <- roll_cor_plot(dates =sta_1e_df$dt, vector = sta_1e_df$inflow_int, max.lag = max.lag, window = window)
sta1e_out_roll <- roll_cor_plot(dates =sta_1e_df$dt, vector = sta_1e_df$outflow_int, max.lag = max.lag, window = window)

sta34_in_roll <- roll_cor_plot(dates =sta_34_df$dt, vector = sta_34_df$inflow_int, max.lag = max.lag, window = window)
sta34_out_roll <- roll_cor_plot(dates =sta_34_df$dt, vector = sta_34_df$outflow_int, max.lag = max.lag, window = window)

ggsave2(file="sta1e_in_roll.png", sta1e_in_roll, width = 200, height = 100, units = c("mm"))
ggsave2(file="sta1e_out_roll.png", sta1e_out_roll, width = 200, height = 100, units = c("mm"))
ggsave2(file="sta34_in_roll.png", sta34_in_roll, width = 200, height = 100, units = c("mm"))
ggsave2(file="sta34_out_roll.png", sta34_out_roll, width = 200, height = 100, units = c("mm"))

#WAVELET TRANSFORM######

plot(sta_1e_df$dt, sta_1e_df$inflow_int, type = "l")

library(WaveletComp)

df <- data.frame(date = sta_34_df$dt, tp = sta_34_df$outflow_int)
plot(df$tp, type = "l")
TP.in.wav = analyze.wavelet(
  df,
  "tp",
  method = "AR",
  params = list(AR = list(p = 1)),
  loess.span = 0,
  dt = 1,
  dj = 1 / 50,
  lowerPeriod = 2,
  upperPeriod = 160,
  make.pval = T,
  n.sim = 1000
)

wt.image(TP.in.wav, main = "STA_3/4 Inflow Wavelet Power Spectrum", legend.params = list(lab = "Wavelet Power Levels", label.digits= 2),
         periodlab = "Period (Months)", show.date = TRUE, date.format = "%Y-%m-%d", timelab = "Time (Years)", n.levels = 20, color.key = "q", plot.ridge = F)



max(WT.melt$Var2)
nrow(sta_34_df)




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




