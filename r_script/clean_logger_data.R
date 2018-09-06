# Date: Sept 6, 2018
# Purpose: Clean files for Jesse.

#install.packages("tidyverse")
#install.packages("lubridate")

# libraries
library(tidyverse)
library(lubridate)

# get data
RawSurveys <- read.csv("data/surveys.csv", stringsAsFactors = FALSE)
RawClimberLoggerData <- read.csv("data/climb_logger_data.csv", stringsAsFactors = FALSE)

# clean survey data
CleanSurveys <- RawSurveys %>% 
  # only keeping the data we need
  select(Date = date,
         StartTime = start_time,
         LoggerNumber = logger_num,
         TreeID = Tree_ID) %>% 
  filter(!(is.na(StartTime))) %>% 
  filter(!(is.na(LoggerNumber))) %>% 
  # standardize dates
  mutate(DateStartTime = as.POSIXct(paste(Date, StartTime), format="%d/%m/%Y %H:%M"))

# clean logger data
CleanClimberLoggerData <- RawClimberLoggerData %>% 
  select(LoggerNumber = logger_num,
         DateTime = date_time,
         TempC = temp_C,
         RH = rh_.,
         DewPTC = dewpt_C) %>% 
  # standardize dates
  mutate(DateTime = as.POSIXct(DateTime, format = "%m/%d/%y %H:%M"))


# loop to do what jesse needs

TaggedClimberLoggerData <- tibble()

# for each date-time in surveys
for (i in 1:length(CleanSurveys$DateStartTime)){
  
  # first find time bounds -15 mins to + 1 hours
  before_datetime = CleanSurveys$DateStartTime[i] - 15*60
  after_datetime = CleanSurveys$DateStartTime[i] + 60*60
  
  # find macthed in logger data
  ReqdLoggerData <- CleanClimberLoggerData %>% 
    # only keep date/times between bounds
    filter(DateTime >= before_datetime & DateTime <= after_datetime) %>% 
    # the logger number is the same
    filter(LoggerNumber == CleanSurveys$LoggerNumber[i]) %>% 
    # add relevant tree ID
    mutate(TreeID = CleanSurveys$TreeID[i])
  
  # save it in the tagged climber logger data
  TaggedClimberLoggerData <- bind_rows(TaggedClimberLoggerData,
                                       ReqdLoggerData)
  
  
}

# write csv
write.csv(TaggedClimberLoggerData, "TaggedClimberLoggerData.csv")





