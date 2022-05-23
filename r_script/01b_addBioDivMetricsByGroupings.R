# generate biodiv metrics for arboreal, terrestrial, nocturnal and diurnal groups
library(tidyverse)
library(vegan)
 select <- dplyr::select

#BIODIVERSITY DATA

#data
herpdata <- read.csv("raw_data/herpdata.csv", header = T) #set this up

sites <- read.csv("raw_data/sites.csv")

biodiv_data <- read_csv("clean_data/biodiv_data.csv")

##########################
#Arboreal Species
#######################

herpdataArb <- herpdata %>% 
 filter(ArbVsTerr == "arboreal")


####################
#for arboreal
####################
#generating abundances for all species by sites
herpdata2<-herpdataArb %>%
  group_by(Tree_ID,species_code) %>%
  tally()
herpdata2<- cast(herpdata2, Tree_ID ~ species_code, value='n')

herpdata2 <- merge(herpdata2, sites, all = TRUE) #bring in sites that don't have herps

herpdata2 <- herpdata2[,c(1:15)]

herpdata2[is.na(herpdata2)]<-0

spec_names <- names(herpdata2[,2:14]) #check this to be sure this 
#captures correct names of species columns

########################
#Add diversity metrics
spec_counts <- herpdata2[,spec_names]
row.names(spec_counts) <- herpdata2$Tree_ID

#shannon diversity index
herpdata2$diversity_shannon_Arb<-diversity(spec_counts, index = "shannon")
herpdata2$EffectSpecNumShan_Arb <- exp(herpdata2$diversity_shannon)

#simpsons
herpdata2$diversity_simpson_Arb <- diversity(spec_counts, index = "simpson")
herpdata2$EffectSpecNumbSimps_Arb <- 1/(1-herpdata2$diversity_simpson)

#abundance
herpdata2$abund_Arb <-rowSums(herpdata2 [,spec_names])

#richness
herpdata2$rich_Arb <- rowSums(herpdata2 [,spec_names] > 0)

ArborealBiodiv <- herpdata2 %>% 
  select(Tree_ID, diversity_shannon_Arb, diversity_simpson_Arb, EffectSpecNumShan_Arb, EffectSpecNumbSimps_Arb, abund_Arb, rich_Arb)

biodiv_data <- left_join(biodiv_data, ArborealBiodiv, by = "Tree_ID")



##########################
#Terrestrial Species
#######################

herpdataTerr <- herpdata %>% 
  filter(ArbVsTerr == "terrestrial")


####################
#for terrestrial species
####################
#generating abundances for all species by sites
herpdata2<-herpdataTerr %>%
  group_by(Tree_ID,species_code) %>%
  tally()
herpdata2<- cast(herpdata2, Tree_ID ~ species_code, value='n')

herpdata2 <- merge(herpdata2, sites, all = TRUE) #bring in sites that don't have herps

herpdata2 <- herpdata2[,c(1:12)]

herpdata2[is.na(herpdata2)]<-0

spec_names <- names(herpdata2[,2:11]) #check this to be sure this 
#captures correct names of species columns

########################
#Add diversity metrics
spec_counts <- herpdata2[,spec_names]
row.names(spec_counts) <- herpdata2$Tree_ID

#shannon diversity index
herpdata2$diversity_shannon_Ter<-diversity(spec_counts, index = "shannon")
herpdata2$EffectSpecNumShan_Ter <- exp(herpdata2$diversity_shannon)

#simpsons
herpdata2$diversity_simpson_Ter <- diversity(spec_counts, index = "simpson")
herpdata2$EffectSpecNumbSimps_Ter <- 1/(1-herpdata2$diversity_simpson)

#abundance
herpdata2$abund_Ter <-rowSums(herpdata2 [,spec_names])

#richness
herpdata2$rich_Ter <- rowSums(herpdata2 [,spec_names] > 0)

TerrestrialBiodiv <- herpdata2 %>% 
  select(Tree_ID, diversity_shannon_Ter, diversity_simpson_Ter, EffectSpecNumShan_Ter, EffectSpecNumbSimps_Ter, abund_Ter, rich_Ter)

biodiv_data <- left_join(biodiv_data, TerrestrialBiodiv, by = "Tree_ID")



##########################
#nocturnal Species
#######################

herpdataNoct <- herpdata %>% 
  filter(NoctVsDiurn == "nocternal")


####################
#for nocternal species
####################
#generating abundances for all species by sites
herpdata2<-herpdataNoct %>%
  group_by(Tree_ID,species_code) %>%
  tally()
herpdata2<- cast(herpdata2, Tree_ID ~ species_code, value='n')

herpdata2 <- merge(herpdata2, sites, all = TRUE) #bring in sites that don't have herps

herpdata2 <- herpdata2[,c(1:7)]

herpdata2[is.na(herpdata2)]<-0

spec_names <- names(herpdata2[,2:7]) #check this to be sure this 
#captures correct names of species columns

########################
#Add diversity metrics
spec_counts <- herpdata2[,spec_names]
row.names(spec_counts) <- herpdata2$Tree_ID

#shannon diversity index
herpdata2$diversity_shannon_Noct<-diversity(spec_counts, index = "shannon")
herpdata2$EffectSpecNumShan_Noct <- exp(herpdata2$diversity_shannon)

#simpsons
herpdata2$diversity_simpson_Noct <- diversity(spec_counts, index = "simpson")
herpdata2$EffectSpecNumbSimps_Noct <- 1/(1-herpdata2$diversity_simpson)

#abundance
herpdata2$abund_Noct <-rowSums(herpdata2 [,spec_names])

#richness
herpdata2$rich_Noct <- rowSums(herpdata2 [,spec_names] > 0)

NocturnalBiodiv <- herpdata2 %>% 
  select(Tree_ID, diversity_shannon_Noct, diversity_simpson_Noct, EffectSpecNumShan_Noct, EffectSpecNumbSimps_Noct, abund_Noct, rich_Noct)

biodiv_data <- left_join(biodiv_data, NocturnalBiodiv, by = "Tree_ID")


##########################
#Dirunal Species
#######################

herpdataDiur <- herpdata %>% 
  filter(NoctVsDiurn == "diurnal")


####################
#for Diurnal species
####################
#generating abundances for all species by sites
herpdata2<-herpdataDiur %>%
  group_by(Tree_ID,species_code) %>%
  tally()
herpdata2<- cast(herpdata2, Tree_ID ~ species_code, value='n')

herpdata2 <- merge(herpdata2, sites, all = TRUE) #bring in sites that don't have herps

herpdata2 <- herpdata2[,c(1:16)]

herpdata2[is.na(herpdata2)]<-0

spec_names <- names(herpdata2[,2:15]) #check this to be sure this 
#captures correct names of species columns

########################
#Add diversity metrics
spec_counts <- herpdata2[,spec_names]
row.names(spec_counts) <- herpdata2$Tree_ID

#shannon diversity index
herpdata2$diversity_shannon_Diur<-diversity(spec_counts, index = "shannon")
herpdata2$EffectSpecNumShan_Diur <- exp(herpdata2$diversity_shannon)

#simpsons
herpdata2$diversity_simpson_Diur <- diversity(spec_counts, index = "simpson")
herpdata2$EffectSpecNumbSimps_Diur <- 1/(1-herpdata2$diversity_simpson)

#abundance
herpdata2$abund_Diur <-rowSums(herpdata2 [,spec_names])

#richness
herpdata2$rich_Diur <- rowSums(herpdata2 [,spec_names] > 0)

DiurnalBiodiv <- herpdata2 %>% 
  select(Tree_ID, diversity_shannon_Diur, diversity_simpson_Diur, EffectSpecNumShan_Diur, EffectSpecNumbSimps_Diur, abund_Diur, rich_Diur)

biodiv_data <- left_join(biodiv_data, DiurnalBiodiv, by = "Tree_ID")


#############################################################3
#SAVE IT!
write.csv(biodiv_data, file = "clean_data/biodiv_data.csv", row.names = FALSE)

