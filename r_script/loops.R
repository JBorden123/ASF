#species level analysis
#goal, generate lm and plots of all species height by edge dist and abundance
#by edge dist

library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
select <- dplyr::select


#data
herpd <- read.csv("raw_data/herpdata.csv")
sites <- read.csv("raw_data/sites.csv")
MetaAll <- read.csv("clean_data/MetaAll.csv")

#merge and clean Herp data
herpd <- merge(herpd, sites, by = "Tree_ID", all = TRUE)

herpd <- herpd %>%
  select(-c(notes.x, notes.y))

HghtHerpD <- herpd %>%
  filter(category != "matrix") %>%
  filter(survey_type == "C") %>%
  filter(forest_type == "BR")

#height by edge distance by species
ggplot(data = HghtHerpD, aes(x = edge_category_m, 
                         y = height_found_m_rec, color = binomial))+
  geom_point()+
  geom_smooth()+
  geom_jitter()+
  ylim(0,20)
               

#######################
#GLM for all species heights by distance

#number of species
spec_counts <- MetaAll %>%
  select(ARST:VAAL)

NumSpecies <- ncol(spec_counts)

variables <- MetaAll%>%
  select(edge_category_m,edge_dist_m,category)

# create an empty dataframe to populate with info from glm
reg_abund <- list(NA)
PsuedoRSquared <- tibble()

for(i in 1:NumSpecies){
  newdata <- cbind(abund = spec_counts[,i], variables)
  lm_abund <- lm(abund~
                   scale(edge_category_m),
                 data=newdata)
  
  PsuedoRSquared[[i]] <- 1 - (lm_abund$deviance/lm_abund$null.deviance) #mcfaddens pseudo R squared
  
  lm_mod <- summary(lm_abund)
  reg_abund[[i]] <- rbind(lm_mod$coefficients)
  
}
names(reg_abund) <- colnames(spec_counts)
PsuedoRSquared
