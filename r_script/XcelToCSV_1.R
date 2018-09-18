###CREATE_CSV_FROM_EXCEL###
#install.packages("readxl")
library(readxl)

herpdata <- read_excel("raw_data/arabuko_sokoke_11.xlsx", sheet = "Herps")
write.csv(herpdata, "raw_data/herpdata.csv") 

surveys <- read_excel("raw_data/arabuko_sokoke_11.xlsx", sheet = "Surveys")
write.csv(surveys, "raw_data/surveys.csv") 

loggers <- read_excel("raw_data/arabuko_sokoke_11.xlsx", sheet = "Loggers")
write.csv(loggers, "raw_data/logger_record.csv") 

key <- read_excel("raw_data/arabuko_sokoke_11.xlsx", sheet = "Key")
write.csv(key, "raw_data/key.csv")

habitat <- read_excel("raw_data/arabuko_sokoke_11.xlsx", sheet = "Habitat")
write.csv(habitat, "raw_data/habitat.csv")

