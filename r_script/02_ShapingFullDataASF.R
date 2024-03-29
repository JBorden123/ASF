#Shaping 
#arabuko metadata into metadata with all survey details, this divided by forest type,


library(dplyr)
library(ggplot2)
library(vegan)
library(psych)
summarize <- dplyr::summarize

#data
herpdata <- read.csv("raw_data/herpdata.csv", header = TRUE)
sites <- read.csv("raw_data/sites.csv", header = TRUE)
sites <- sites[,-11]#remove notes column
biodiv_data <- read.csv("clean_data/biodiv_data.csv", header = TRUE)
metadata <- merge(biodiv_data,sites, by = "Tree_ID", all = TRUE)
HabSummary <- read.csv("clean_data/HabSummary.csv", header = TRUE)
TreeGPS <- read.csv("raw_data/TreeGPS.csv")

Surveys <- read_csv("raw_data/surveys.csv")
  
Surveys2 <- Surveys %>% 
  filter(!grepl("MAT", Tree_ID)) %>% 
  filter(!grepl(".100", Tree_ID))

Surveys2 <- left_join(Surveys2, sites, by = "Tree_ID")

NumberSurvsPerForest <- Surveys2 %>% 
  group_by(forest_type) %>% 
  summarize(count = n())

#adding median height by tree FOR ONLY arboreal species (probably need to add day vs night in case this 
#might change height distribution)
med_hght <- herpdata %>%
  filter(survey_type != "G", ArbVsTerr == "arboreal")%>% #remove ground surveys AND non-arboreal species. To detect shift in perch heights
  group_by(Tree_ID)%>%
  dplyr::summarize(med_hght = median(height_found_m))

MedHghtPerc <- merge(herpdata, sites, by = "Tree_ID", all = TRUE)

MedHghtPerc <- MedHghtPerc %>%
  filter(survey_type != "G", ArbVsTerr == "arboreal" ) %>%
  group_by(Tree_ID) %>%
  summarize(MedHghtPerc = median((height_found_m/HOT_m)), 
            HeightRange_m = (max(height_found_m)-min(height_found_m)))

#merge hght and metadata
metadata <- merge(metadata, med_hght, by = "Tree_ID", all = TRUE)
metadata <- merge(metadata, MedHghtPerc, by = "Tree_ID", all = TRUE)

#summary exploring


MetaAll <- merge(metadata, HabSummary, by = "Tree_ID", all = TRUE) #merge with habitat data

MetaAll <- merge(MetaAll, TreeGPS, by = "Tree_ID", all = TRUE)#add GPS 

forest_grouped <- MetaAll %>%
  filter(extra_ground != "Y")%>%
  group_by(forest_type, edge_category_m)%>%
  summarise(count = n())


#save it
write.csv(MetaAll, file = "clean_data/MetaAll.csv", row.names = FALSE)


########################
#metadata by forest type
########################
#Brachystegia
BR_metadata <- MetaAll %>%
  filter(forest_type == "BR")
write.csv(BR_metadata, file = "clean_data/MetaAllBR.csv", row.names = FALSE)

#Mixed
M_metadata <- MetaAll %>%
  filter(forest_type == "M")
write.csv(M_metadata, file = "clean_data/MetaAllM.csv", row.names = FALSE)

#Cynometera
CY_metadata <- MetaAll %>%
  filter(forest_type == "CY")
write.csv(CY_metadata, file = "clean_data/MetaAllCY.csv", row.names = FALSE)



