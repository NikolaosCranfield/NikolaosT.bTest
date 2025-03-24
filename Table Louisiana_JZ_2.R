#set working directory to the folder with the data
setwd("C:/Users/c6356/OneDrive - Cranfield University/Students/Nikolaos/Pivot")

#read in the dataset
salinity_data <- read.csv("Sample Data_CRMS.csv", header=T, comment.char="#", stringsAsFactors=TRUE)

################################
#create a vector with station names
stations<-as.vector(unique(salinity_data$StationID))
#create a vector with column mames
columns<-as.vector(colnames(salinity_data))
#convert the Date field to date format
salinity_data$Date<-as.Date(salinity_data$Date, format = "%m/%d/%y")

#create a dataframe to store results
# this table will contain unique station IDs in the first columnt
results<-data.frame(stations)

#start a loop that generates information on the no data values in all variables for each station

for (i in 1:length(stations)){
  for(k in 3:length(columns)){

  name<-stations[i]
  subset<-salinity_data[which(salinity_data[1]== name),]
  
  min_date<-min(subset$Date)
  max_date<-max(subset$Date)
  
  duration<-as.numeric(max_date-min_date)
  tot_obs<-nrow(subset)
  results[i,2]<-min_date
  results[i,3]<-max_date
  results[i,4]<-duration
  results[i,5]<-tot_obs
  
  colnames(results)[2]<-"Min_Date"
  colnames(results)[3]<-"Max_Date"
  colnames(results)[4]<-"Duration_days"
  colnames(results)[5]<-"Total_observations"

  
  variable<-columns[k]
  nas<-sum(is.na(subset[k]))
  
  results[i,k+3]<-nas
  colnames(results)[k+3]<-paste0(variable,"_","NAs")
  
}}

#save the result in a csv format
write.csv(results, "no_data_numbers.csv",row.names = F)
