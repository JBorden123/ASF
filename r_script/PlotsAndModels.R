######
#Data exploring
######

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
MetaAllM <- read.csv("clean_data/MetaAllM.csv")
MetaAllCY <- read.csv("clean_data/MetaAllCY.csv")


#select climb surveys
MetaClimbs <- MetaAll %>%
  filter(extra_ground == "N")%>%
  filter(edge_category_m != -10)

MetaClimbs$Log1MedHght <- log(MetaClimbs$med_hght + 1)


#par(mfrow=c(2,2))
mod2 <- lm(Log1MedHght ~ edge_category_m, data= MetaClimbs)
plot(mod2)
gvlma(mod2) #tests assumptions of linear model


#subset forest types from climbing data
MetaClimbsBR <- MetaClimbs %>%
  filter(forest_type == "BR")
MetaClimbsM <- MetaClimbs %>%
  filter(forest_type == "M")
MetaClimbsCY <- MetaClimbs %>%
  filter(forest_type == "CY")


#################
#HEIGHT BY EDGE 
#################

#height by end all data
HghtByEdgeAll <- ggplot(MetaClimbs, aes(edge_category_m, med_hght)) +
  geom_point()+
  geom_jitter()+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance", y = "Median Perch Height", title = "All Sites (.)")
HghtByEdgeAll

summary(lm(Log1MedHght~edge_category_m, data = MetaClimbs))

hist(MetaClimbs$edge_category_m)

#height by edge Brachystegia
HghtByEdgeBR <- ggplot(data = filter(MetaClimbs, forest_type == "BR"), aes(edge_category_m, med_hght)) +
  geom_point()+
  geom_jitter()+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance", y = "Median Perch Height", title = "Brachystegia Sites")
HghtByEdgeBR

summary(lm(Log1MedHght~edge_category_m, data = MetaClimbsBR))

#height by edge Mixed
HghtByEdgeM <- ggplot(data = filter(MetaClimbs, forest_type == "M"), aes(edge_category_m, med_hght)) +
  geom_point()+
  geom_jitter()+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance", y = "Median Perch Height", title = "Mixed Sites (.)")
HghtByEdgeM

summary(lm(Log1MedHght~edge_category_m, data = MetaClimbsM))

#height by edge Cynometera
HghtByEdgeCY <- ggplot(data = filter(MetaClimbs, forest_type == "CY"), aes(edge_category_m, med_hght)) +
  geom_point()+
  geom_jitter()+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance", y = "Median Perch Height", title = "Cynometera Sites")
HghtByEdgeCY

summary(lm(Log1MedHght~edge_category_m, data = MetaClimbsCY))


###############
#ABUNDANCE & RICHNESS
###############

#Abundance & RICH by edge... all data
#abundance
AbundByEdgeAll <- ggplot(MetaAll, aes(edge_category_m, abund)) +
  geom_point()+
  geom_jitter()+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance (m)", y = "Abundance", title = "All Sites (***)")
AbundByEdgeAll

summary(glm(abund~
              edge_category_m,
            data = MetaAll, family = poisson))

#richness
RichByEdgeAll <- ggplot(MetaAll, aes(edge_category_m, rich)) +
  geom_point()+
  geom_jitter()+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance (m)", y = "Richness", title = "All Sites (*)")
RichByEdgeAll

summary(glm(rich~
              edge_category_m,
            data = MetaAll, family = poisson))

#Abundance by edge Brachystegia
AbunByEdgeBR <- ggplot(MetaAllBR, aes(edge_category_m, abund)) +
  geom_point()+
  geom_jitter()+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance", y = "Abundance", title = "Brachystegia Sites (**)")
AbunByEdgeBR
summary(glm(abund~
              edge_category_m,
            data = MetaAllBR, family = poisson))
#Richness
RichByEdgeBR <- ggplot(MetaAllBR, aes(edge_category_m, rich)) +
  geom_point()+
  geom_jitter()+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance", y = "Richness", title = "Brachystegia Sites (.)")
RichByEdgeBR
summary(glm(rich~
              edge_category_m,
            data = MetaAllBR, family = poisson))

#abund by edge Mixed
AbundByEdgeM <- ggplot(data = MetaAllM, aes(edge_category_m, abund)) +
  geom_point()+
  geom_jitter()+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance", y = "Abundance", title = "Mixed Sites (*)")
AbundByEdgeM

summary(glm(abund~
              edge_category_m,
            data = MetaAllM, family = poisson))
#richness
RichByEdgeM <- ggplot(data = MetaAllM, aes(edge_category_m, rich)) +
  geom_point()+
  geom_jitter()+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance", y = "Richness", title = "Mixed Sites ()")
RichByEdgeM

summary(glm(rich~
              edge_category_m,
            data = MetaAllM, family = poisson))

#abun by edge Cynometera
AbunByEdgeCY <- ggplot(data = MetaAllCY, aes(edge_category_m, abund)) +
  geom_point()+
  geom_jitter()+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance", y = "Abundance", title = "Cynometera Sites (**)")
AbunByEdgeCY

summary(glm(abund~
              edge_category_m,
            data = MetaAllCY, family = poisson))

#rich by edge Cynometera
RichByEdgeCY <- ggplot(data = MetaAllCY, aes(edge_category_m, rich)) +
  geom_point()+
  geom_jitter()+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance", y = "Richness", title = "Cynometera Sites (.)")

RichByEdgeCY

summary(glm(rich~
              edge_category_m,
            data = MetaAllCY, family = poisson))

#plot together
ggarrange(HghtByEdgeAll, HghtByEdgeBR, HghtByEdgeM, ncol = 1, nrow = 3)

ggarrange(AbundByEdgeAll, RichByEdgeAll, ncol = 2, nrow = 1)

ggarrange(AbunByEdgeBR, RichByEdgeBR, AbundByEdgeM, RichByEdgeM, AbunByEdgeCY, RichByEdgeCY,
          nrow = 3, ncol = 2)
