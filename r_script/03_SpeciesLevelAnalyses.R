####### species level analysis

library(tidyverse)
library(cowplot)
library(randomForest)
library(lme4)
library(car) # for VIF testing
library(glmulti)# for automated model selection
library(MuMIn)#to dredge model
library(glmmTMB)
library(geomtextpath)

#data exploring
#THE DATA
herpdata <- read_csv("raw_data/herpdata.csv")
metadata <- read_csv("clean_data/MetaAll.csv")
sites <- read_csv("raw_data/sites.csv")
herpdata <- left_join(herpdata, sites, by = "Tree_ID")

#total abundances for arboreal species
herpList <- herpdata %>% 
 filter(ArbVsTerr == "arboreal") %>% 
  group_by(binomial) %>% 
  dplyr::summarize(Abund = n())

#most abundant arboreal species with min of 9 individuals but some much more abundant
AbundantArbSpecs <- herpdata %>% 
  filter(species_code == "CHDI" | species_code == "LYMO" | species_code == "HEPL" 
         |species_code == "HEMA" | species_code == "HEMR" | species_code == "TRPL")


Plot1 <- ggplot(AbundantArbSpecs, aes(edge_category_m, color = species_code))+
  geom_textdensity()

Plot1

ggplot(metadata, aes(edge_category_m, CHDI)) +
  geom_jitter(alpha = .5, height = .05)



