# Date: Sept 6, 2018
# Purpose: Clean files for Jesse.

#install.packages("tidyverse")
#install.packages("lubridate")

# libraries
library(tidyverse)
library(lubridate)
library(psych)

# get data
RawSurveys <- read.csv("raw_data/surveys.csv", stringsAsFactors = FALSE)
RawClimberLoggerData <- read.csv("raw_data/climb_logger_data.csv", stringsAsFactors = FALSE)

# clean survey data
CleanSurveys <- RawSurveys %>% 
  # only keeping the data we need
  select(Date = date, #rename columns within select
         StartTime = start_time, #snake case is with underscores between words
         LoggerNumber = logger_num, #camel case is LikeThis... used for variables
         Tree_ID,
         MidTime = mid_time,
         CanopyTime = canopy_time,
         EndTime = end_time,
         DayNight = day_night) %>% 
  filter(!(is.na(StartTime))) %>% 
  filter(!(is.na(MidTime))) %>%
  filter(!(is.na(CanopyTime))) %>%
  filter(!(is.na(EndTime))) %>%
  filter(!(is.na(LoggerNumber))) %>% 
  # standardize dates
  mutate(DateStartTime = as.POSIXct(paste(Date, StartTime), format="%d/%m/%Y %H:%M")) %>%
  mutate(MidStartTime = as.POSIXct(paste(Date, MidTime), format="%d/%m/%Y %H:%M")) %>%
  mutate(CanopyStartTime = as.POSIXct(paste(Date, CanopyTime), format="%d/%m/%Y %H:%M")) %>%
  mutate(EndStartTime = as.POSIXct(paste(Date, EndTime), format="%d/%m/%Y %H:%M"))

# clean logger data

# correct column name

colnames(RawClimberLoggerData)[1] <- "logger_num"

CleanClimberLoggerData <- RawClimberLoggerData %>% 
  select(LoggerNumber = logger_num,
         DateTime = date_time,
         TempC = temp_C,
         RH = rh_.,
         DewPTC = dewpt_C) %>% 
  # standardize dates
  mutate(DateTime = as.POSIXct(DateTime, format = "%m/%d/%y %H:%M"))


# loop to do what jesse needs

TaggedClimberLoggerData <- tibble() #neater form of a dataframe...blank

# for each date-time in surveys
for (i in 1:length(CleanSurveys$DateStartTime)){
  # first find time bounds -15 mins to + 1 hours
  before_datetime = CleanSurveys$DateStartTime[i] - 15*60 #times 60 to make minutes from seconds
  after_datetime = CleanSurveys$DateStartTime[i] + 60*60 #same as above
  mid_start = CleanSurveys$MidStartTime[i]
  mid_end = CleanSurveys$MidStartTime[i] + 10*60
  canopy_start = CleanSurveys$CanopyStartTime[i]
  canopy_end = CleanSurveys$CanopyStartTime[i] + 10*60
  
  # find macthed in logger data
  ReqdLoggerData <- CleanClimberLoggerData %>% 
    # only keep date/times between bounds
    filter(DateTime >= before_datetime & DateTime <= after_datetime) %>% 
    # the logger number is the same
    filter(LoggerNumber == CleanSurveys$LoggerNumber[i]) %>% 
    # add relevant tree ID
    mutate(Tree_ID = CleanSurveys$Tree_ID[i]) %>% #mutate adds new columns
    mutate(DayNight = CleanSurveys$DayNight[i]) %>%
    # add strata timings
    mutate(Strata = if_else((DateTime >= before_datetime) & (DateTime <= before_datetime + 35*60), 
                            "Understory", 
                              if_else((DateTime >= mid_start) & (DateTime <= mid_end), 
                                    "Mid",
                                    if_else((DateTime >= canopy_start) & (DateTime <= canopy_end),
                                    "Canopy", "Unclassified"))))
  
  # save it in the tagged climber logger data
  TaggedClimberLoggerData <- bind_rows(TaggedClimberLoggerData,
                                       ReqdLoggerData)
  
  
}

# write csv
write.csv(TaggedClimberLoggerData, "clean_data/clean_logger_data.csv")

#create summary table with mean temp, RH, DewPt for each tree/strata day and Night
climb_logger_data <- TaggedClimberLoggerData %>%
  group_by(Tree_ID,DayNight,Strata) %>%
  summarize(mean_temp = mean(TempC), mean_RH = mean(RH), mean_DewPtC = mean(DewPTC))
