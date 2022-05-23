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

MetaClimbs$AsinSqrtPercHght <- (asin(sqrt(MetaClimbs$MedHghtPerc)))

summary(MetaClimbs$forest_type)

#par(mfrow=c(2,2))
mod2 <- glm(Log1MedHght ~ edge_category_m, data= MetaClimbs)
summary(mod2)


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

#height by edge all data
HghtByEdgeAll <- ggplot(MetaClimbs, aes(edge_category_m, med_hght)) +
  geom_point()+
  geom_jitter()+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance", y = "Median Perch Height", title = "All Sites (.)")
HghtByEdgeAll

summary(lm(Log1MedHght~edge_category_m, data = MetaClimbs))

hist(MetaClimbs$edge_category_m)

#Percentage height by HOT by edge all data 
PercHghtByEdgeAll <- ggplot(MetaClimbs, aes(edge_category_m,AsinSqrtPercHght)) +
  geom_point()+
  geom_jitter()+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance", y = "proportion of height to HOT", title = "All Sites ()")
PercHghtByEdgeAll

summary(lm(AsinSqrtPercHght~edge_category_m, data = MetaClimbs))


#height by edge Brachystegia
HghtByEdgeBR <- ggplot(data = filter(MetaClimbs, forest_type == "BR"), aes(edge_category_m, med_hght)) +
  geom_point()+
  geom_jitter()+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance", y = "Median Perch Height", title = "Brachystegia Sites")
HghtByEdgeBR

summary(lm(Log1MedHght~edge_category_m, data = MetaClimbsBR))

#Perc height by edge
PercHghtByEdgeBR <- ggplot(data = filter(MetaClimbs, forest_type == "BR"), aes(edge_category_m, AsinSqrtPercHght)) +
  geom_point()+
  geom_jitter()+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance", y = "proportion of median height to HOT", title = "Brachystegia Sites")
PercHghtByEdgeBR

summary(lm(AsinSqrtPercHght~edge_category_m, data = MetaClimbsBR))


#height by edge Mixed
HghtByEdgeM <- ggplot(data = filter(MetaClimbs, forest_type == "M"), aes(edge_category_m, med_hght)) +
  geom_point()+
  geom_jitter()+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance", y = "Median Perch Height", title = "Mixed Sites (.)")
HghtByEdgeM

summary(lm(Log1MedHght~edge_category_m, data = MetaClimbsM))

#Perc height by edge Mixed
PercHghtByEdgeM <- ggplot(data = filter(MetaClimbs, forest_type == "M"), aes(edge_category_m, AsinSqrtPercHght)) +
  geom_point()+
  geom_jitter()+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance", y = "proportion of median height to HOT", title = "Mixed Sites")
PercHghtByEdgeM

summary(lm(AsinSqrtPercHght~edge_category_m, data = MetaClimbsM))


#height by edge Cynometera
HghtByEdgeCY <- ggplot(data = filter(MetaClimbs, forest_type == "CY"), aes(edge_category_m, med_hght)) +
  geom_point()+
  geom_jitter()+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance", y = "Median Perch Height", title = "Cynometera Sites")
HghtByEdgeCY

summary(lm(Log1MedHght~edge_category_m, data = MetaClimbsCY))


#Perc height by edge Cynometera
PercHghtByEdgeCY <- ggplot(data = filter(MetaClimbs, forest_type == "CY"), aes(edge_category_m, AsinSqrtPercHght)) +
  geom_point()+
  geom_jitter()+
  geom_smooth(method = lm)+
  labs(x = "Rough Edge Distance", y = "proportion of median height to HOT", title = "Cynometera Sites")
PercHghtByEdgeCY

summary(lm(AsinSqrtPercHght~edge_category_m, data = MetaClimbsCY))



