library(readxl)
library(dplyr)
library(plyr)
library(readr)
library(data.table)


filenames <- list.files(path = "C:/Users/georg/OneDrive/Desktop/E", pattern = "*.csv", full.names = TRUE)
print(filenames)

fullpath = file.path("C:/Users/georg/OneDrive/Desktop/E", filenames)
print(filenames)

tp_interpolate <- lapply(filenames, fread, sep = ",")
data <- rbindlist(tp_interpolate)

write.csv(data, file = "combined1", rownames = FALSE)


dataset <- do.call("rbind", lapply(filenames, FUN = function(files){read.csv(files)}))
lapply(read.csv)


Combined <- read.csv("Combined.csv", header = T)


lag.plot(mynewmatrix, main = "Correlations Inflow-Outflow of STA_1E", lags = 12, do.lines = FALSE)


library(astsa)

astsa::lag.plot(combined, 12)




library(Hmisc)

cor.test(Combined$tp_interpolate.inflow, Combined$tp_interpolate.outflow, method = "kendall", exact = FALSE)

set1 <- c("tp_interpolate.inflow", "tp_interpolate.outflow")
mynewmatrix<-Combined[set1]
matrix2<-rcorr(as.matrix(mynewmatrix))
print(matrix2)

library(PerformanceAnalytics)
chart.Correlation(mynewmatrix, main = "Correlations Combined")

######################

library(zoo)
library(WaveletComp)
R_sta_34 <- read.csv("R_sta_34.csv", header = T)
R_sta_34$month <- sprintf("%02d", R_sta_34$month)
R_sta_34$Date_txt <- paste0("01-", R_sta_34$month, "-", R_sta_34$year)
R_sta_34$Date_dt <- as.POSIXct(R_sta_34$Date_txt, format = "%d-%m-%Y")
df <- data.frame(date = R_sta_34$Date_dt, in_tp_l = R_sta_34$in_tp_l)
df$tp_interpolate <- na.approx(df$in_tp_l
                               #, max.gap = 5
)
write.table(df, file = "sta6new.csv", sep = ",", row.names = FALSE)


filenames <- list.files(path = "C:/Users/georg/OneDrive/Desktop/D", pattern = "*.csv", full.names = TRUE)
print(filenames)

fullpath = file.path("C:/Users/georg/OneDrive/Desktop/D", filenames)
print(filenames)

tp_interpolate <- lapply(filenames, fread, sep = ",")
data <- rbindlist(tp_interpolate)


dataset <- do.call("rbind", lapply(filenames, FUN = function(files){read.csv(files)}))
lapply(read.csv)


############

Combined <- read.csv("combinedsta34.csv", header = T)
Combined$x1<- lag(Combined$tp_interpolate.outflow.Mg.L., k =12, na.rm= TRUE)
na.omit(Combined)
Combined$x1[is.na(Combined$x1)] <- 0

library(Hmisc)
library(ggpubr)

cor.test(Combined$tp_interpolate.inflow.Mg.L., Combined$tp_interpolate.outflow.Mg.L., method = "kendall", exact = FALSE)


formula1 <- (y ~ x)
Combined$tp_interpolate.inflow.Mg.L.<- as.numeric(Combined$tp_interpolate.inflow.Mg.L.)
x1<-as.numeric(x1)

A<- ggplot(data= Combined, aes(x = x1, y = tp_interpolate.inflow.Mg.L.))+ 
  xlab("Lag Outflow [TP]-Mg/L")+ 
  ylab("Inflow [TP]-Mg/L")+ 
  ggtitle("[TP]-Mg/L Inflow-Outflow of STA34") +
  geom_point(size=2, color="black") +
  geom_smooth(method = "lm", se=TRUE, formula = formula1, color="black")+
  stat_poly_eq(formula = formula1,
               aes(label = paste(..eq.label.., ..rr.label..,sep = "*\", \"*")),
               label.x = 0.045, 
               label.y = 0.125,
               output.type = "expression",
               geom = "text",
               color = "black",
               fontface="bold",
               size = 6,
               position = "identity",
               na.rm = FALSE,
               show.legend = NA,
               inherit.aes = TRUE,
               parse = TRUE)+
  stat_cor(method = "pearson",
           aes(label = paste(..r.label.., ..rr.label.., ..p.label.., sep = "*\", \"*")),
           label.x = 0.03, 
           label.y = 0.135,
           output.type = "expression",
           geom = "text",
           color = "black",
           fontface="bold",
           size = 6,
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           parse = TRUE) +
  font("title", size = 15, face = "bold.italic", color = "black") +
  font("xylab", size = 15, face = "bold", color = "black") +
  font("xy.text", size = 10, face = "bold", color = "black") +
  font("legend.text", size = 10) +
  font("legend.title", face = "bold", size = 15)
plot(A)

B<- ggplot(data= Combined, aes(x = tp_interpolate.outflow.Mg.L., y = tp_interpolate.inflow.Mg.L.))+ 
  xlab("Outflow [TP]-Mg/L")+ 
  ylab("Inflow [TP]-Mg/L")+ 
  ggtitle("[TP]-Mg/L Inflow-Outflow of STA34") +
  geom_point(size=2, color="black") +
  geom_smooth(method = "lm", se=TRUE, formula = formula1, color="black")+
  stat_poly_eq(formula = formula1,
               aes(label = paste(..eq.label.., ..rr.label..,sep = "*\", \"*")),
               label.x = 0.045, 
               label.y = 0.125,
               output.type = "expression",
               geom = "text",
               color = "black",
               fontface="bold",
               size = 6,
               position = "identity",
               na.rm = FALSE,
               show.legend = NA,
               inherit.aes = TRUE,
               parse = TRUE)+
  stat_cor(method = "kendall",
           aes(label = paste(..r.label.., ..rr.label.., ..p.label.., sep = "*\", \"*")),
           label.x = 0.03, 
           label.y = 0.135,
           output.type = "expression",
           geom = "text",
           color = "black",
           fontface="bold",
           size = 6,
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           parse = TRUE) +
  font("title", size = 15, face = "bold.italic", color = "black") +
  font("xylab", size = 15, face = "bold", color = "black") +
  font("xy.text", size = 10, face = "bold", color = "black") +
  font("legend.text", size = 10) +
  font("legend.title", face = "bold", size = 15)
plot(B)















write.table(Combined, file = "sta1enew.csv", sep = ",", row.names = FALSE)


set1 <- c("tp_interpolate.inflow.Mg.L.", "x")

mynewmatrix<-Combined[set1]

matrix2<-rcorr(as.matrix(mynewmatrix))
print(matrix2)


lag.plot(mynewmatrix, main = "TP[Mg/L] Inflow-Outflow of STA_1E", lags = 6, do.lines = FALSE)



############

library(sjPlot)
library(GGally)

GGally::ggpairs(Combined, columns = 2:3, ggplot2::aes(colour = "tp_interpolate.outflow"))



#######################



library(Hmisc)

cor.test(sta_1Einflow$tp_interpolate, sta_1Eoutflow$tp_interpolate, method = "kendall", exact = FALSE)

set1 <- c("", "", "rainfall_sum")
mynewmatrix<-R_sta_6[set1]
matrix2<-rcorr(as.matrix(mynewmatrix))
print(matrix2)

par(mfrow = c(2,1))
plot(in_tp_c, rainfall_sum)
abline(lm(in_tp_c~rainfall_sum))
plot(out_tp_c, rainfall_sum)
abline(lm(out_tp_c ~ rainfall_sum))

library(car)

scatterplot(in_tp_c, rainfall_sum, main = "Inflow STA_6")
scatterplot(out_tp_c, rainfall_sum, main = "Outflow STA_6")
library(PerformanceAnalytics)
chart.Correlation(mynewmatrix, main = "Correlations STA_6")