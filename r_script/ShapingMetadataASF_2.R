#Shaping 
#arabuko metadata

library(dplyr)
library(ggplot2)
library(vegan)
library(psych)

#data
herpdata <- read.csv("raw_data/herpdata.csv", header = TRUE)
sites <- read.csv("raw_data/sites.csv", header = TRUE)
sites <- sites[,-13]#remove notes column
biodiv_data <- read.csv("clean_data/biodiv_data.csv", header = TRUE)
metadata <- merge(biodiv_data,sites, by = "Tree_ID", all = TRUE)
HabSummary <- read.csv("clean_data/HabSummary.csv", header = TRUE)
  
#change NA to 0 in biodiv cols
biodiv_cols <- names(metadata[,2:29]) #selecting Biodiv columns
metadata[,biodiv_cols][is.na(metadata[,biodiv_cols])] <- 0 #change NA in Biodiv columns to 0
  
#adding mean height by tree (probably need to add day vs night in case this 
#might change height distribution)
med_hght <- herpdata%>%
  filter(survey_type != "G")%>%
  group_by(Tree_ID)%>%
  summarize(med_hght = median(height_found_m))

med_hght <- herpdata%>%
  filter(survey_type != "G")%>%
  group_by(Tree_ID)%>%
  summarize(med_hght = median(height_found_m))


#merge hght and metadata
metadata <- merge(metadata, med_hght, by = "Tree_ID", all = TRUE)


#summary exploring
forest_grouped <- metadata %>%
  filter(extra_ground != "Y")%>%
  group_by(forest_type, edge_category_m)%>%
  summarise(count = n())

metadata <- merge(metadata, HabSummary, all = TRUE)

#save it
write.csv(metadata, file = "clean_data/metadata.csv")


########################
#metadata by forest type
########################
#Brachystegia
BR_metadata <- metadata %>%
  filter(forest_type == "BR")
write.csv(BR_metadata, file = "clean_data/BR_metadata.csv")

#Mixed
M_metadata <- metadata %>%
  filter(forest_type == "M")
write.csv(M_metadata, file = "clean_data/M_metadata.csv")

#Cynometera
CY_metadata <- metadata %>%
  filter(forest_type == "CY")
write.csv(CY_metadata, file = "clean_data/CY_metadata.csv")

head(metadata)


