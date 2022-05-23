#merge and clean up logger data

#libraries
library(stringr)
library(reshape2)
library(tidyr)
library(dplyr)

#bring in data
#climb logger data
climbLogs <- read.csv("clean_data/clean_climb_logger_data.csv")

#Fixed Logger Data
loggerdates <- read.csv("raw_data/Loggers/LogSiteData.csv")
unique(loggerdates$logger_num)
logger3 <- read.csv("raw_data/Loggers/logger_10_july11_20136770.csv", skip = 1)
logger4 <- read.csv("raw_data/Loggers/logger_4_july11_20127464.csv", skip=1)
logger6 <- read.csv("raw_data/Loggers/logger_6_july11_20127456.csv", skip=1)
logger7 <- read.csv("raw_data/Loggers/logger_7_july11_20136761.csv", skip=1)
logger10 <- read.csv("raw_data/Loggers/logger_10_july11_20136770.csv", skip=1)
logger11 <- read.csv("raw_data/Loggers/logger_11_july11.csv", skip=1)
logger16 <- read.csv("raw_data/Loggers/logger_16_July11_20136766.csv", skip=1)
logger17 <- read.csv("raw_data/Loggers/logger_17_july11_20127455.csv", skip=1)
logger18 <- read.csv("raw_data/Loggers/logger_18_july11.csv", skip=1)
logger19 <- read.csv("raw_data/Loggers/logger_19_july11_20127467.csv", skip=1)

#datevars <- c("logger_num", "date_placed", "time_placed", "date_removed", "time_removed")
#loggerdates <- loggerdates[datevars]
loggerdates <- loggerdates %>% 
  mutate(LoggerNumber = logger_num) %>% 
  select(-logger_num)

#clean names of columns for all data loggers
names(logger4) <- c("i", "date_time", "temp", "max_temp", "min_temp", "ave_temp_day","ave_temp_hr", "coupler_attached", "host_connected", "coupler_detached", "stopped", "end_file") 
names(logger6) <- c("i", "date_time", "temp", "max_temp", "min_temp", "ave_temp_day","ave_temp_hr", "coupler_attached", "host_connected", "stopped", "end_file") 
names(logger7) <- c("i", "date_time", "temp", "max_temp", "min_temp", "ave_temp_day","ave_temp_hr", "coupler_attached", "host_connected", "stopped", "end_file") 
names(logger10) <- c("i", "date_time", "temp", "max_temp", "min_temp", "ave_temp_day","ave_temp_hr", "coupler_attached", "host_connected", "stopped", "end_file") 
names(logger11) <- c("i","date_time", "temp", "intensity_lux", "coupler_attached", "host_connected","stopped","end_file") 
names(logger16) <- c("i", "date_time", "temp",  "coupler_attached", "host_connected", "stopped", "end_file") 
names(logger17) <- c("i", "date_time",  "temp", "max_temp", "min_temp", "ave_temp_day","ave_temp_hr", "coupler_attached", "host_connected", "stopped", "end_file") 
names(logger18) <- c("i", "date_time",  "temp", "coupler_attached", "host_connected", "stopped") 
names(logger19) <- c("i", "date_time", "temp", "max_temp", "min_temp", "ave_temp_day","ave_temp_hr", "coupler_attached", "host_connected", "stopped", "end_file") 

#Retaining only the relevant variables, and adding the LoggerNumber column
vars <- c("i", "date_time", "temp")
logger4 <- logger4[vars]
logger4$LoggerNumber <- 4
logger6 <- logger6[vars]
logger6$LoggerNumber <- 6
logger7 <- logger7[vars]
logger7$LoggerNumber <- 7
logger10 <- logger10[vars]
logger10$LoggerNumber <- 10
logger11 <- logger11[vars]
logger11$LoggerNumber <- 11
logger16 <- logger16[vars]
logger16$LoggerNumber <- 16
logger17 <- logger17[vars]
logger17$LoggerNumber <- 17
logger18 <- logger18[vars]
logger18$LoggerNumber <- 18
logger19 <- logger19[vars]
logger19$LoggerNumber <- 19

#Combining into one dataset
FixedLoggers <- rbind(logger4, logger6, logger7, logger10, logger11, logger16, logger17, logger18, logger19)

#Adding the date added/removed vars
FixedLoggers <- inner_join(FixedLoggers, loggerdates)

FixedLoggers[order(FixedLoggers$date_time),]	

#Converting the date/time stamps so we can drop observations outside specified dates
FixedLoggers$date_time <- as.POSIXct(strptime(FixedLoggers$date_time,"%m/%d/%y %I:%M:%S %p",tz=""))

#Added/removed stamps
FixedLoggers$date_time_placed <- paste(FixedLoggers$date_placed, FixedLoggers$time_placed)
FixedLoggers$date_time_placed <- as.POSIXct(strptime(FixedLoggers$date_time_placed,"%m/%d/%y %H:%M",tz=""))
FixedLoggers$date_time_removed <- paste(FixedLoggers$date_removed, FixedLoggers$time_removed)
FixedLoggers$date_time_removed <- as.POSIXct(strptime(FixedLoggers$date_time_removed,"%m/%d/%y %H:%M",tz=""))
#Checking they all converted well
class(FixedLoggers$date_time)
class(FixedLoggers$date_time_placed)
class(FixedLoggers$date_time_removed)



#remove records from before and after logger was placed and removed at site!

#Restricting to obs after the sensor was added
FixedLoggers <- subset(FixedLoggers, date_time >= date_time_placed)
#Restricting to obs before the sensor was removed
FixedLoggers <- subset(FixedLoggers, date_time <= date_time_removed)

head(FixedLoggers)

FixedLoggerData <- FixedLoggers %>% 
  select(DateTime = date_time,TempC = temp, LoggerNumber, Tree_ID, Strata = strata, HabType = hab_type, DateTimePlaced = date_time_placed, DateTimeRemoved = date_time_removed)
unique(FixedLoggerData$Tree_ID)
#Save it!
write_csv(FixedLoggerData, "clean_data/FixedLogData.csv")

