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
climber_temperature_data <- read.csv("clean_data/clean_logger_data.csv", header = TRUE)

fixed_logger <- read.csv("clean_data/FixedLogData.csv", header = TRUE)
fixed_logger <- merge(fixed_logger, metadata, by=c("Tree_ID","Tree_ID"), all.x = TRUE, all.y = FALSE )


#################CLIMBER LOGGERS


###### create column dy night

#using a temporary data frame because problem with datetime format when mutate
a <- climber_temperature_data
#date time format
a$DateTime <- strptime(a$DateTime, "%m/%d/%y %H:%M")
# extract hour
a$hour <- as.numeric(format(a$DateTime, "%H"))
#same colums for merging
colnames(a)[1] <- "Id"
colnames(climber_temperature_data)[1] <- "Id"
#merge
climber_temperature_data <- merge(x = climber_temperature_data,y = a[, c("Id", "hour")], by=c("Id","Id"), all =TRUE)
# column day night
climber_temperature_data <- mutate(climber_temperature_data, DayNight = ifelse(hour > 18,"Night", "Day"))



### summarise temperature by tree by undersotry, mid, canopy

climber_temperature_data_summarised <- climber_temperature_data %>% group_by(Tree_ID, DayNight, Strata) %>% summarise(mean_temperature_c = mean(TempC), mean_RH = mean(RH), mean_DewPTC = mean(DewPTC))

### link metadata
climber_temperature_data_summarised <- merge(climber_temperature_data_summarised, metadata, by=c("Tree_ID","Tree_ID"), all.x = TRUE, all.y = FALSE )




####### GRAPHS

#temperature from cliber surveys
ggplot(data = filter(climber_temperature_data_summarised, Strata != "Unclassified")) + geom_boxplot(mapping = aes(x = Strata, y = mean_temperature_c, color = DayNight)) + geom_jitter(mapping = aes(x = Strata, y = mean_temperature_c, color = DayNight), alpha = 0.3, width = 0.1, height = 0.1) + facet_grid(.~forest_type) + scale_fill_manual(labels = c("Day", "Night"), values = c("yellow3", "midnightblue")) + theme_bw(base_size = 13) + labs(title = "Temperatures from loggers on climbers", x = "Strata", y = "Temperature (°C)")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "figures/Temperatures_Climber.png")





####################### FIXED LOGGERS 

# format day time
fixed_logger$DateTime <- strptime(fixed_logger$DateTime, "%m/%d/%y %H:%M")
fixed_logger$DateTime <- as.POSIXct(as.POSIXlt(fixed_logger$DateTime))



###### GRAPHS

# temperature VS time
ggplot(data=filter(fixed_logger, Strata != "NA")) + geom_line(mapping = aes(x = DateTime, y = TempC, color = Strata), size = 0.1) + facet_grid(Tree_ID~.) + theme_bw(base_size = 13) + labs(title = "Temperature variations by strata", x = "Time", y = "Temperature (°C)")




                                                                                            