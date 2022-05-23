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
MetaAllBR <- read.csv("clean_data/MetaAllBR.csv")
MetaAllM <- read.csv("clean_data/MetaAllM.csv")

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
  geom_jitter()+
  geom_smooth(method = lm, se = FALSE)+
  ylim(0,20)
               

#height found by species:
ggplot(data = herpd, aes(x = common_name, y = height_found_m))+
  geom_boxplot(aes(color = common_name))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))


#######################
#GLM for all species Abundance by distance

#number of species
spec_counts <- MetaAll %>%
  select(ARST:VAAL)

NumSpecies <- ncol(spec_counts)

variables <- MetaAll%>%
  select(edge_category_m,edge_dist_m,category)

# create an empty dataframe to populate with info from glm
reg_abund <- list(NA)
RSquared <- list(NA)

for(i in 1:NumSpecies){
  newdata <- cbind(abund = spec_counts[,i], variables)
  lm_abund <- lm(abund~
                   scale(edge_category_m),
                 data=newdata)
  
  lm_mod <- summary(lm_abund)
  
  reg_abund[[i]] <- rbind(lm_mod$coefficients)
  RSquared[[i]] <- rbind(lm_mod$r.squared)
}
names(reg_abund) <- colnames(spec_counts)
reg_abund
names(RSquared) <- colnames(spec_counts)
RSquared


#Individual models (LM maybe not best technique for this) (as this data is COUNT data, appropriate distribution in Poisson)
summary(glm(LYMO ~ scale(edge_category_m), data = MetaAll, family = poisson))
summary(glm(HEPL ~ scale(edge_category_m), data = MetaAll, family = poisson))
summary(glm(HEBA ~ scale(edge_category_m), data = MetaAll, family = poisson))
summary(glm(HEMA ~ scale(edge_category_m), data = MetaAll, family = poisson))
summary(glm(HEMA ~ scale(edge_category_m), data = MetaAll, family = poisson))
summary(glm(HEMI ~ scale(edge_category_m), data = MetaAll, family = poisson))
summary(glm(TRMA ~ scale(edge_category_m), data = MetaAll, family = poisson))
summary(glm(NUBO ~ scale(edge_category_m), data = MetaAll, family = poisson))
summary(glm(CHDI ~ scale(edge_category_m), data = MetaAll, family = poisson))

hist(MetaAll$edge_category_m)

