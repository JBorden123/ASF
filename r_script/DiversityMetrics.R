
library(ggplot2)
library(dplyr)
library(tidyverse)
#install.packages("gvlma")
library(gvlma)
library(gridExtra)
#install.packages("ggpubr")
library(ggpubr)

#data
MetaAll <- read.csv("clean_data/MetaAll.csv", header = TRUE)
MetaAllBR <- read.csv("clean_data/MetaAllBR.csv")
MetaBRpres <- MetaAllBR %>%
  filter(abund > 0)
MetaAllM <- read.csv("clean_data/MetaAllM.csv")
MetaMPres <- MetaAllM %>%
  filter(abund > 0)
MetaAllCY <- read.csv("clean_data/MetaAllCY.csv")
MetaCYPres <- MetaAllCY %>%
  filter(abund > 0)
#All sites for COre and Edge
MetaEdge <- MetaAll %>%
  filter(edge_category_m == 0 | edge_category_m == 30)
MetaCore <- MetaAll %>%
  filter(edge_category_m > 99)

#all sites for Core and edge that have at least one animal
#present
MetaEdgePres <- AllEdge %>%
  filter(abund > 0)
MetaCorePres <- MetaCore %>%
  filter(abund > 0)

###############
#ABUNDANCE & RICHNESS & DIVERSITY
###############

#Abundance & RICH by edge... all data
#abundance
AbundByEdgeAll <- ggplot(MetaAll, aes(edge_category_m, abund)) +
  geom_point()+
  geom_jitter(height = .1, width = 4)+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance (m)", y = "Abundance", 
       title = "All Sites (***)")+
  theme_classic()
AbundByEdgeAll

summary(glm(abund~
              edge_category_m,
            data = MetaAll, family = poisson))

#richness
MetaAllPres <- MetaAll%>%
  filter(abund > 0)

RichByEdgeAll <- ggplot(MetaAllPres, aes(edge_category_m, rich)) +
  geom_point()+
  geom_jitter(height = .1, width = 4)+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance (m)", y = "Richness", 
       title = "All Sites ()")+
  theme_classic()
RichByEdgeAll

summary(glm(rich~
              edge_category_m,
            data = MetaAllPres, family = poisson))

#diversity
DivByEdgeAll <- ggplot(MetaAllPres, aes(edge_category_m, diversity_shannon)) +
  geom_point()+
  geom_jitter(height = .1, width = 4)+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance (m)", y = "Shannon's Diversity",
       title = "All Sites ()")+
  theme_classic()
DivByEdgeAll

summary(glm(diversity_shannon~
              edge_category_m,
            data = MetaAllPres, family = poisson))

#Abundance by edge Brachystegia
AbunByEdgeBR <- ggplot(MetaAllBR, aes(edge_category_m, abund)) +
  geom_point()+
  geom_jitter(height = .1, width = 4)+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance", y = "Abundance", 
       title = "Brachystegia Sites (**)")+
  theme_classic()
AbunByEdgeBR
summary(glm(abund~
              edge_category_m,
            data = MetaAllBR, family = poisson))
#Richness
RichByEdgeBR <- ggplot(MetaBRpres, aes(edge_category_m, rich)) +
  geom_point()+
  geom_jitter(height = .1, width = 4)+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance", y = "Richness", 
       title = "Brachystegia Sites ()")+
  theme_classic()
RichByEdgeBR
summary(glm(rich~
              edge_category_m,
            data = MetaBRpres, family = poisson))

#diversity Brachystegia
DivByEdgeBR <- ggplot(MetaBRpres, aes(edge_category_m, diversity_shannon)) +
  geom_point()+
  geom_jitter(height = .1, width = 4)+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance (m)", y = "Shannon's Diversity",
       title = "Brachystegia Sites ()")+
  theme_classic()
DivByEdgeBR

summary(glm(diversity_shannon~
              edge_category_m,
            data = MetaBRpres, family = poisson))

#abund by edge Mixed
AbundByEdgeM <- ggplot(data = MetaAllM, aes(edge_category_m, abund)) +
  geom_point()+
  geom_jitter(height = .1, width = 4)+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance", y = "Abundance",
       title = "Mixed Sites (*)")+
  theme_classic()
AbundByEdgeM

summary(glm(abund~
              edge_category_m,
            data = MetaAllM, family = poisson))
#richness
RichByEdgeM <- ggplot(data = MetaMPres, aes(edge_category_m, rich)) +
  geom_point()+
  geom_jitter(height = .1, width = 4)+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance", y = "Richness", 
       title = "Mixed Sites ()")+
  theme_classic()
RichByEdgeM

summary(glm(rich~
              edge_category_m,
            data = MetaMPres, family = poisson))

#diversity Mixed
DivByEdgeM <- ggplot(MetaMPres, aes(edge_category_m, diversity_shannon)) +
  geom_point()+
  geom_jitter(height = .1, width = 4)+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance (m)", y = "Shannon's Diversity",
       title = "Mixed Sites ()")+
  theme_classic()
DivByEdgeM

summary(glm(diversity_shannon~
              edge_category_m,
            data = MetaMPres, family = poisson))


#abun by edge Cynometera
AbunByEdgeCY <- ggplot(data = MetaAllCY, aes(edge_category_m, abund)) +
  geom_point()+
  geom_jitter(height = .1, width = 4)+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance", y = "Abundance", 
       title = "Cynometera Sites (**)")+
  theme_classic()
AbunByEdgeCY

summary(glm(abund~
              edge_category_m,
            data = MetaAllCY, family = poisson))

#rich by edge Cynometera
RichByEdgeCY <- ggplot(data = MetaCYPres, aes(edge_category_m, rich)) +
  geom_point()+
  geom_jitter(height = .1, width = 4)+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance", y = "Richness", 
       title = "Cynometera Sites (.)")+
  theme_classic()

RichByEdgeCY

summary(glm(rich~
              edge_category_m,
            data = MetaCYPres, family = poisson))


#diversity Cynometera
DivByEdgeCY <- ggplot(MetaCYPres, aes(edge_category_m, diversity_shannon)) +
  geom_point()+
  geom_jitter(height = .1, width = 4)+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance (m)", y = "Shannon's Diversity",
       title = "Cynometera Sites ()")+
  theme_classic()
DivByEdgeCY

summary(glm(diversity_shannon~
              edge_category_m,
            data = MetaCYPres, family = poisson))



#plot together
ggarrange(HghtByEdgeAll, HghtByEdgeBR, HghtByEdgeM, ncol = 3, nrow = 1)

ggarrange(PercHghtByEdgeAll, PercHghtByEdgeBR, PercHghtByEdgeM, ncol = 3, nrow = 1)

ggarrange(AbundByEdgeAll, RichByEdgeAll, DivByEdgeAll, ncol = 3, nrow = 1)

ggarrange(AbunByEdgeBR, RichByEdgeBR, DivByEdgeBR, AbundByEdgeM,
          RichByEdgeM, DivByEdgeM, AbunByEdgeCY, RichByEdgeCY, DivByEdgeCY,
          nrow = 3, ncol = 3)
