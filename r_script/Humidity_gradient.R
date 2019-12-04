#### EXPLORE humidity DATA

###charge_packages###
library(tidyverse)
library(ggplot2)
library(vegan)
library(dplyr)
library(forcats)
library(questionr)
library(reshape)
library(reshape2)
library(xlsx)
library(readxl)
library(readr)
library(gtools)

#### CHARGE DATA
metadata <- read.csv("clean_data/MetaAll.csv", header = TRUE)
climber_humidity_data <- read.csv("clean_data/clean_logger_data.csv", header = TRUE)





###### create column dy night

#using a Humidityorary data frame because problem with datetime format when mutate
a <- climber_humidity_data
#date time format
a$DateTime <- strptime(a$DateTime, "%m/%d/%y %H:%M")
# extract hour
a$hour <- as.numeric(format(a$DateTime, "%H"))
#same colums for merging
colnames(a)[1] <- "Id"
colnames(climber_humidity_data)[1] <- "Id"
#merge
climber_humidity_data <- merge(x = climber_humidity_data,y = a[, c("Id", "hour")], by=c("Id","Id"), all =TRUE)
# column day night
climber_humidity_data <- mutate(climber_humidity_data, DayNight = ifelse(hour > 18,"Night", "Day"))

# add column time of the day
a <- climber_humidity_data
#date time format
a$DateTime <- strptime(a$DateTime, "%m/%d/%y %H:%M")
# extract time
a$time <- strftime(a$DateTime, format="%H:%M")
# put in time
a$time  <- as.POSIXct(a$time,format="%H:%M")

#merge
climber_humidity_data <- merge(x = climber_humidity_data,y = a[, c("Id", "time")], by=c("Id","Id"), all =TRUE)





### summarise humidity by tree by undersotry, mid, canopy

climber_humidity_data_summarised <- climber_humidity_data %>% group_by(Tree_ID, DayNight, Strata) %>% summarise(mean_Temperature = mean(TempC), mean_RH = mean(RH), mean_DewPTC = mean(DewPTC))

### link metadata
climber_humidity_data_summarised <- merge(climber_humidity_data_summarised, metadata, by=c("Tree_ID","Tree_ID"), all.x = TRUE, all.y = FALSE )




####### GRAPHS

#humidity boxplot from climber loggers day_night
ggplot(data = filter(climber_humidity_data_summarised, Strata != "Unclassified")) + geom_boxplot(mapping = aes(x = Strata, y = mean_RH, color = DayNight)) + geom_point(mapping = aes(x = Strata, y = mean_RH, color = DayNight), alpha = 0.3, position = position_dodge(width = 0.6))  + facet_grid(.~forest_type) + scale_fill_manual(labels = c("Day", "Night"), values = c("yellow3", "midnightblue")) + theme_bw(base_size = 13) + labs(title = "humidity from loggers on climbers", x = "Strata", y = "Relative Humidity (%)")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "figures/humidity_Climber.png")

#by edge category
ggplot(data = filter(climber_humidity_data_summarised, Strata != "Unclassified", edge_category_m != "-10", edge_category_m != "NA")) + geom_boxplot(mapping = aes(x = Strata, y = mean_RH, color = DayNight)) + geom_point(mapping = aes(x = Strata, y = mean_RH, color = DayNight), alpha = 0.3, position = position_dodge(width = 0.6))  + facet_grid(.~edge_category_m) + scale_fill_manual(labels = c("Day", "Night"), values = c("yellow3", "midnightblue")) + theme_bw(base_size = 13) + labs(title = "humidity from loggers on climbers", x = "Strata", y = "Relative Humidity (%)")


# humidty acrooss time from climber loggers 
### link metadata
climber_humidity_data <- merge(climber_humidity_data, metadata, by=c("Tree_ID","Tree_ID"), all.x = TRUE, all.y = FALSE )
#plot
ggplot(data = filter(climber_humidity_data, Strata != "Unclassified", Strata != "NA")) + geom_point(mapping = aes(x = time, y = RH, color = Strata)) + geom_smooth(mapping = aes(x = time, y = RH, color = Strata), method = "auto") + facet_grid(.~forest_type) + labs(title="Relative humidity across day time (climber logger)", x = "Time of the day", y = "Relative humidity (%)") + theme_bw(base_size = 13)
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "figures/humidity_across_timeClimber.png")


# by edge category
ggplot(data = filter(climber_humidity_data, Strata != "Unclassified", Strata != "NA", edge_category_m != "-10", edge_category_m != "NA")) + geom_point(mapping = aes(x = time, y = RH, color = Strata)) + geom_smooth(mapping = aes(x = time, y = RH, color = Strata), method = "auto") + facet_grid(.~edge_category_m) + labs(title="Relative humidity across day time (climber logger)", x = "Time of the day", y = "Relative humidity (%)") + theme_bw(base_size = 13)










######################## individuals and data from climber logger at the same time


######### link climber loggers and herp data to see the temperature and humidity where each animal where found (for understory, mid and canopy only)

# get summarise temperature and humidity for the 3 strata
Herps_temperature_humidity <- filter(climber_temperature_data_summarised, Strata != "Unclassified")

######## prepare herp data for merging using tree, day/night, strata
herps <- animals
## recode strata to select canopy , mid and understory
herps$survey_strata <- fct_recode(herps$survey_strata,
                                  "Canopy" = "C",
                                  "Mid" = "M",
                                  "Understory" = "U")
# select these 3 strata
herps <- herps %>% filter(survey_strata == "Canopy" | survey_strata == "Mid" | survey_strata == "Understory")
#harmonise day night
herps$day_night <- fct_recode(herps$day_night,
                              "Day" = "D",
                              "Night" = "N")

# harmonise column names
names(herps)[names(herps) == "day_night"] <- "DayNight"
names(herps)[names(herps) == "survey_strata"] <- "Strata"

#merge climber logger data and herps
Herps_temperature_humidity <- merge(herps, Herps_temperature_humidity, by = c("Tree_ID", "DayNight", "Strata"))




### GRAPHS individuals and data from climber loggers

# humidity in strata where individuals found, by species
data <- Herps_temperature_humidity
a <- data %>% group_by(binomial) %>% summarise(Median_RH = median(mean_RH, na.rm = TRUE))
data <- merge(data, a, by = "binomial")
ggplot(data = filter(data, binomial != "NA")) + geom_boxplot(mapping = aes(x = reorder(binomial, - Median_RH), y = mean_RH)) + geom_jitter(mapping = aes(x = reorder(binomial, - Median_RH), y = mean_RH), color = "red", alpha = 0.3) + labs(title = "Mean relative humidity of the strata where the animals where found (climber surveys and loggers)", x = "Species", y = "Relative humidity (%)")




