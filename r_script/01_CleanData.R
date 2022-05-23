###CREATE_CSV_FROM_EXCEL###

#libraries
#install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(vegan)
#install.packages("reshape")
library(reshape)
library(tidyr)
select <- dplyr::select

#write CSV files from Excel Raw Data
herpdata <- read_excel("raw_data/arabuko_sokoke_11.xlsx", sheet = "Herps")
write.csv(herpdata, "raw_data/herpdata.csv", row.names = FALSE) 

surveys <- read_excel("raw_data/arabuko_sokoke_11.xlsx", sheet = "Surveys")
write.csv(surveys, "raw_data/surveys.csv", row.names = FALSE) 

loggers <- read_excel("raw_data/arabuko_sokoke_11.xlsx", sheet = "Loggers")
write.csv(loggers, "raw_data/logger_record.csv", row.names = FALSE) 

key <- read_excel("raw_data/arabuko_sokoke_11.xlsx", sheet = "Key")
write.csv(key, "raw_data/key.csv", row.names = FALSE)

habitat <- read_excel("raw_data/arabuko_sokoke_11.xlsx", sheet = "Habitat")
write.csv(habitat, "raw_data/habitat.csv", row.names = FALSE)

sites <- read_excel("raw_data/arabuko_sokoke_11.xlsx", sheet = "Sites")
sites$edge_category_m <- as.numeric(sites$edge_category_m)
sites$category <- ifelse(sites$edge_category_m > 99, "core", "edge") 
write.csv(sites, "raw_data/sites.csv", row.names = FALSE)

#no longer need to make this subset becaus I manually added to it in the file 
#dividing by arb vs ground species
#ArbVsGround <- herpdata %>% 
# group_by(genus, species, binomial) %>% 
#summarise(count = n(),MinHght = min(height_found_m, na.rm = TRUE), 
#         MeahHght = mean(height_found_m, na.rm = TRUE), 
#        MaxHght = max(height_found_m, na.rm = TRUE), MedHght = median(height_found_m, na.rm = TRUE))

#export this species list with arboreal vs ground details
#write_csv(ArbVsGround, "raw_data/SpecListArbVsGround.csv")

SpecData <- read.csv("clean_data/SpecListStrataNoctDia.csv")
SpecData <- SpecData %>% 
  mutate(binomial = paste(genus, species, sep = "_")) %>% 
  select(binomial, ArbVsTerr, NoctVsDiurn)

########################
#Cleaning up data
########################

#HABITAT DATA

habitat <- read.csv("raw_data/habitat.csv", header = TRUE)

HabSummary <- habitat %>% #select summary columns
  select(Tree_ID, TotalAvgCan = total_avg_can_cov, CanMinus10 = can_minus_10, CanMinus5 = can_minus_5,
         Can0 = can_0, CanPlus5 = can_plus_5, CanPlus10 = can_plus_10,
         AvgHerbCover = avg_herb_cover..., AvgLeafLayer = avg_leaf_layer,
         StemLess8cm = stem_less_8cm, StemMore8cm = stem_more_8cm, 
         BasalArea = basal_area, Debris = debris)%>%
  mutate(TotalAvgCan = (100 - TotalAvgCan))%>%
  mutate(CanMinus10 = (100 - CanMinus10))%>%
  mutate(CanMinus5 = (100 - CanMinus5))%>%
  mutate(Can0 = (100 - Can0))%>%
  mutate(CanPlus5 = (100 - CanPlus5))%>%
  mutate(CanPlus10 = (100 - CanPlus10))

#mutate adds column in which we 
#transform these columns from densiometer readings (which gives
# measure of sky, not canopy), to now be % canopy


HabSummary <- HabSummary %>%
  distinct(Tree_ID, .keep_all = TRUE) #remove duplicates

write_csv(HabSummary, "clean_data/HabSummary.csv") #write the new dataset as a csv


#BIODIVERSITY DATA

#code for generating diversity, richness, abundance, eveness and species accum curves
#data
herpdata <- read.csv("raw_data/herpdata.csv", header = T)
herpdata <- herpdata %>% 
  mutate(binomial = paste(genus, species, sep = "_"))


herpdata <- left_join(herpdata, SpecData, by = "binomial")
#re-save herpdata
write.csv(herpdata, "raw_data/herpdata.csv", row.names = FALSE) 

####################
#For Entire Forest
####################
#generating abundances for all species by sites
herpdata2<-herpdata %>%
  group_by(Tree_ID,species_code) %>%
  tally()
herpdata2<- cast(herpdata2, Tree_ID ~ species_code, value='n')

herpdata2 <- merge(herpdata2, sites, all = TRUE) #bring in sites that don't have herps

herpdata2 <- herpdata2[,c(1:25)]

herpdata2[is.na(herpdata2)]<-0

spec_names <- names(herpdata2[,2:24]) #check this to be sure this 
#captures correct names of species columns

########################
#Add diversity metrics
spec_counts <- herpdata2[,spec_names]
row.names(spec_counts) <- herpdata2$Tree_ID

 #shannon diversity index
herpdata2$diversity_shannon<-diversity(spec_counts, index = "shannon")
herpdata2$EffectSpecNumShan <- exp(herpdata2$diversity_shannon)
#simpsons
herpdata2$diversity_simpson <- diversity(spec_counts, index = "simpson")
herpdata2$EffectSpecNumbSimps <- 1/(1-herpdata2$diversity_simpson)


#fisher's Alpha
#herpdata2$fisher_alpha <- fisher.alpha(spec_counts)

#abundance
herpdata2$abund <-rowSums(herpdata2 [,spec_names])

#richness
herpdata2$rich <- rowSums(herpdata2 [,spec_names] > 0)

#save them
#1)
write.csv(herpdata2, file = "clean_data/biodiv_data.csv", row.names = FALSE)

