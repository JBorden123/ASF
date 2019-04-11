
if (.Platform$OS.type == "windows") { 
  
  root <- paste("C:\\Users\\", Sys.info()["user"], sep= "")
  
} else if (Sys.info()["sysname"] == "Darwin") {
  
  root <- paste("/Users/", Sys.info()["user"], sep= "") 
  
} else if (.Platform$OS.type == "unix") { 
  
  root <- paste("/usr/", Sys.info()["user"], sep= "")
  
} else {
  
  stop("Unknown OS")
  
}
setwd(root)
basedir = "~/Dropbox/R/Loggers"
setwd(basedir)

library(stringr)
library(reshape2)
library(tidyr)
library(dplyr)


cl <- read.csv("clean_logger_data.csv")
logger3 <- read.csv("logger_3_july11_20136768.csv")
logger4 <- read.csv("logger_4_july11_20127464.csv", skip=1)
logger6 <- read.csv("logger_6_july11_20127456.csv", skip=1)
logger7 <- read.csv("logger_7_july11_20136761.csv", skip=1)
logger10 <- read.csv("logger_10_july11_20136770.csv", skip=1)
logger11 <- read.csv("logger_11_july11.csv", skip=1)
logger16 <- read.csv("logger_16_July11_20136766.csv", skip=1)
logger17 <- read.csv("logger_17_july11_20127455.csv", skip=1)
logger18 <- read.csv("logger_18_july11.csv", skip=1)
logger19 <- read.csv("logger_19_july11_20127467.csv", skip=1)
loggerdates <- read.csv("LogSiteData.csv")
datevars <- c("logger_num", "date_placed", "time_placed", "date_removed", "time_removed")
loggerdates <- loggerdates[datevars]
loggerdates <- rename(loggerdates, LogNumber = logger_num) 

names(logger4) <- c("i", "date_time", "temp", "max_temp", "min_temp", "ave_temp_day","ave_temp_hr", "coupler_attached", "host_connected", "coupler_detached", "stopped", "end_file") 
names(logger6) <- c("i", "date_time", "temp", "max_temp", "min_temp", "ave_temp_day","ave_temp_hr", "coupler_attached", "host_connected", "stopped", "end_file") 
names(logger7) <- c("i", "date_time", "temp", "max_temp", "min_temp", "ave_temp_day","ave_temp_hr", "coupler_attached", "host_connected", "stopped", "end_file") 
names(logger10) <- c("i", "date_time", "temp", "max_temp", "min_temp", "ave_temp_day","ave_temp_hr", "coupler_attached", "host_connected", "stopped", "end_file") 
names(logger11) <- c("i","date_time", "temp", "intensity_lux", "coupler_attached", "host_connected","stopped","end_file") 
names(logger16) <- c("i", "date_time", "temp",  "coupler_attached", "host_connected", "stopped", "end_file") 
names(logger17) <- c("i", "date_time",  "temp", "max_temp", "min_temp", "ave_temp_day","ave_temp_hr", "coupler_attached", "host_connected", "stopped", "end_file") 
names(logger18) <- c("i", "date_time",  "temp", "coupler_attached", "host_connected", "stopped") 
names(logger19) <- c("i", "date_time", "temp", "max_temp", "min_temp", "ave_temp_day","ave_temp_hr", "coupler_attached", "host_connected", "stopped", "end_file") 

#Retaining only the relevant variables, and adding the LogNumber column
vars <- c("i", "date_time", "temp")
logger4 <- logger4[vars]
logger4$LogNumber <- 4
logger6 <- logger6[vars]
logger6$LogNumber <- 6
logger7 <- logger7[vars]
logger7$LogNumber <- 7
logger10 <- logger10[vars]
logger10$LogNumber <- 10
logger11 <- logger11[vars]
logger11$LogNumber <- 11
logger16 <- logger16[vars]
logger16$LogNumber <- 16
logger17 <- logger17[vars]
logger17$LogNumber <- 17
logger18 <- logger18[vars]
logger18$LogNumber <- 18
logger19 <- logger19[vars]
logger19$LogNumber <- 19
#Combining into one dataset
loggers <- rbind(logger4, logger6, logger7, logger10, logger11, logger16, logger17, logger18, logger19)
#Adding the date added/removed vars
loggers <- inner_join(loggers, loggerdates)

loggers[order(loggers$date_time),]	

#Converting the date/time stamps so we can drop observations outside specified dates
loggers$date_time <- as.POSIXct(strptime(loggers$date_time,"%m/%d/%y %I:%M:%S %p",tz=""))
#Added/removed stamps
loggers$date_time_placed <- paste(loggers$date_placed, loggers$time_placed)
loggers$date_time_placed <- as.POSIXct(strptime(loggers$date_time_placed,"%m/%d/%y %H:%M",tz=""))
loggers$date_time_removed <- paste(loggers$date_removed, loggers$time_removed)
loggers$date_time_removed <- as.POSIXct(strptime(loggers$date_time_removed,"%m/%d/%y %H:%M",tz=""))
#Checking they all converted well
class(loggers$date_time)
class(loggers$date_time_placed)
class(loggers$date_time_removed)

#Restricting to obs after the sensor was added
loggersrestricted <- subset(loggers, date_time >= date_time_placed)
#Restricting to obs before the sensor was removed
loggersrestricted <- subset(loggersrestricted, date_time <= date_time_removed)
