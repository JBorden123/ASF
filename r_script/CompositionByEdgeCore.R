#species compositions

library(dplyr)
library(tidyverse)
library(ggplot2)

#data
Herps <- read.csv("raw_data/herpdata.csv", header = TRUE)
Sites <- read.csv("raw_data/sites.csv", header = TRUE)

Herps <- merge(Herps, Sites, by = "Tree_ID", all = TRUE)


GroupedByEdge <- Herps %>%
  group_by(edge_category_m, binomial) %>%
  summarise(abund = n())

Edge <- GroupedByEdge %>%
  filter(edge_category_m < 50 & edge_category_m != -10 & !is.na(binomial))%>%
  group_by(binomial)%>%
  summarise(abund = sum(abund))
  
Core <- GroupedByEdge %>%
  filter(edge_category_m > 50 & !is.na(binomial))%>%
  group_by(binomial)%>%
  summarise(abund = sum(abund))

Matrix <- GroupedByEdge %>%
  filter(!is.na(binomial) & edge_category_m == -10)%>%
  group_by(binomial)%>%
  summarise(abund = sum(abund))
  

#Plot matrix , edge and core compositions
ggplot(Edge, aes(binomial, abund))+
  geom_bar(stat = "identity", width = .75)+
  coord_flip()+
  ggtitle("Species Abundances at Edge (0-30m)")+
  theme_classic()

ggplot(Core, aes(binomial, abund))+
  geom_bar(stat = "identity", width = .75)+
  coord_flip()+
  ggtitle("Species Abundances at Core (100-500m)")+
  theme_classic()

ggplot(Matrix, aes(binomial, abund))+
  geom_bar(stat = "identity", width = .75)+
  coord_flip()+
  ggtitle("Species Abundances in Matrix (-10m)")+
  theme_classic()
