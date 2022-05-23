#packages 
library(randomForest)
library(tidyverse)
library(lme4)
library(reshape2)
library(cowplot)
library(car) # for VIF testing
library(glmulti)# for automated model selection
library(MuMIn)#to dredge model
library(vegan)

select <- dplyr::select

#Data
MetaAll <- read_csv("clean_data/MetaAll.csv")
names(MetaAll)

BioDivData <- MetaAll %>% 
  filter(extra_ground != "Y") %>% 
  filter(edge_category_m != "NA", edge_category_m != -10) %>%  #remove extra surveys
  select(ARST:VAAL ,edge_category_m) %>% 
  group_by(edge_category_m) %>% 
  summarize_all(sum) %>% 
  column_to_rownames(var = "edge_category_m")

MetaAll$edge_category_m

DistanceBray <- vegdist(BioDivData, method = "bray") #bray curtis distance- 1 is dissimilar & 0 is identical

nmds_biodiv <- metaMDS(DistanceBray, k = 2, trace = T)
plot(nmds_biodiv)

