###########################
#DO distance decay analysis of the ASF sites
#to test if distance decay is faster the deeper into the forest you go
#does distance decay change between edge bands.


#install.packages("betapart")
#install.packages("mobsim")
#library(mobsim)
library(betapart)

#install.packages("geosphere")
library ("geosphere")
#install.packages("gdata")
library ("gdata")
library("reshape2")
library("vegan")
library("ade4")
library("ggplot2")
library("grid")
library("gridExtra")
require("cowplot")
library(dplyr)
library(tidyverse)
select <- dplyr::select

#data
MetaAll <- read.csv("clean_data/MetaAll.csv")
LatLon <- read.csv("raw_data/TreeGPS.csv", header = TRUE)
LatLon <- LatLon[!duplicated(LatLon$Tree_ID),] #remove a weird duplicate for BR8
LatLon <- LatLon[,c("Tree_ID","lon","lat")] #switch lat lon order for function later on

??dplyr::distinct
################
#CLEAN ALL DATA AND MAKE SUBSETS
################
#All
CleanMetaAll <- MetaAll %>%
  filter(extra_ground != "Y") %>%
  filter(edge_category_m > -5) #remove matrix and ground surveys without canopy climbs
#this is to ensure that all the data has the same sampling effort (1 climb and 1 ground)

CleanMetaAll <- CleanMetaAll[!duplicated(CleanMetaAll$Tree_ID),]

row.names(CleanMetaAll) <- CleanMetaAll$Tree_ID #create row names from column

#CleanMetaAll <- CleanMetaAll %>% #remove site column which is now row names
  select(-Tree_ID)

summary(CleanMetaAll$forest_type)


#####################
#Creating species matrices for all the different distance decay 
#analyses, subsetted by forest type, and edge difference and the whole thing


#species across all sites:
SpecAll <- CleanMetaAll %>%
  select(ARST:VAAL, forest_type, edge_category_m, Tree_ID)

#subset by forest type
SpecBR <- SpecAll %>%
  filter(forest_type == "BR")%>%
  select(-forest_type, -edge_category_m)
  #column_to_rownames(var = "Tree_ID")

SpecM <- SpecAll %>%
  filter(forest_type == "M")%>%
  select(-forest_type, -edge_category_m)
  #column_to_rownames(var = "Tree_ID")

SpecCY <- SpecAll %>%
  filter(forest_type == "CY")%>%
  select(-forest_type, -edge_category_m)%>%
  column_to_rownames(var = "Tree_ID")

#subset by edge distance
Spec0 <- SpecAll %>%
  filter(edge_category_m == "0") %>%
  select(-forest_type, -edge_category_m)%>%
  column_to_rownames(var = "Tree_ID")

Spec30 <- SpecAll %>%
  filter(edge_category_m == "30") %>%
  select(-forest_type, -edge_category_m)%>%
  column_to_rownames(var = "Tree_ID")

Spec100 <- SpecAll %>%
  filter(edge_category_m == "100") %>%
  select(-forest_type, -edge_category_m)%>%
  column_to_rownames(var = "Tree_ID")

Spec250 <- SpecAll %>%
  filter(edge_category_m == "250") %>%
  select(-forest_type, -edge_category_m)%>%
  column_to_rownames(var = "Tree_ID")

Spec500 <- SpecAll %>%
  filter(edge_category_m == "500") %>%
  select(-forest_type, -edge_category_m) %>%
  column_to_rownames(var = "Tree_ID")

#edge and core
SpecEdge <- SpecAll %>%
  filter(edge_category_m == "0" | edge_category_m == "30") %>%
  select(-forest_type, -edge_category_m) %>%
  column_to_rownames(var = "Tree_ID")
SpecCore <- SpecAll %>%
  filter(edge_category_m == "100" | edge_category_m == "250" | edge_category_m == "500") %>%
  select(-forest_type, -edge_category_m) %>%
  column_to_rownames(var = "Tree_ID")

#remove unwanted columns from full species matrix
SpecAll <- SpecAll%>%
  rownames_to_column()%>%
  select(-forest_type, -edge_category_m)%>%
  column_to_rownames(var = "Tree_ID")%>%
  select(-rowname)



#####FUNCTION TO REMOVE columns with no species occurances and 
#sites with no species

WeedSpecDataMatrix <- function(d){
  require(dplyr)
  d <- d %>%
    select(which(!colSums(d) == 0))#remove columns with no records
  #remove rows with no species
  d$RowSum <- rowSums(d)
  d <- d %>% #remove column with rowsums again
    rownames_to_column()%>%
    filter(RowSum > 0) %>%
    column_to_rownames(var = "rowname")%>%
    select(-RowSum)
  return(d)
  }

#use this funciton on all the data sets
#all
SpecAll <- WeedSpecDataMatrix(SpecAll)
#for each Forest Type
SpecBR <- WeedSpecDataMatrix(SpecBR)
SpecM <- WeedSpecDataMatrix(SpecM)
SpecCY <- WeedSpecDataMatrix(SpecCY)
#for each edge depth
Spec0 <- WeedSpecDataMatrix(Spec0)
Spec30 <- WeedSpecDataMatrix(Spec30)
Spec100 <- WeedSpecDataMatrix(Spec100)
Spec250 <- WeedSpecDataMatrix(Spec250)
Spec500 <- WeedSpecDataMatrix(Spec500)
#edge and core
SpecEdge <- WeedSpecDataMatrix(SpecEdge)
SpecCore <- WeedSpecDataMatrix(SpecCore)
#########################
#DISTANCE DECAY FUNCTION
#########################
SpecDistanceDecay <- function(spec,latlon){
  require(betapart)
  require(tidyverse)
  require(dplyr)
  require(base)
  require(geosphere)
  require(gdata)
  spec[spec>1]=1 #make the species matrix pres/abs data, if using SORENSEN BELLOW
  Betad <- beta.pair(spec, index.family = "sorensen") #similarity matrix
  d2 <- Betad$beta.sim #similarities/disimilarities
  #make sure to use the right matrix above depending 
  sites <- row.names(spec) #grab relevant row names
  coordinates <- latlon %>% #grab lat long coordinates which 
    #the rownames from above in the Tree_ID column
    filter(Tree_ID %in% c(sites))%>%
    column_to_rownames(var = "Tree_ID")
  distance <- distm(coordinates, coordinates) #make a physical distance matrix
  distance[upper.tri(distance, diag = TRUE)] <- NA #Remove upper triangle of distance matrix
  distancelist <- melt(distance, varnames = c("row", "col"),na.rm = TRUE)
  BetaList <- melt(d2, varnames = c("row", "col"),na.rm = TRUE)
  DistBeta <- cbind (BetaList, distancelist)
  colnames(DistBeta) <- c("Beta","Row","Column","Distance")
  #DistBeta[,1] = 1- DistBeta[,1] #change it so 1 and 0 mean opposites
  return(DistBeta)
  #return(distance)
}

###############################
#Use the above function to make data frames for all subsets of the data
##############################
#all data
BetaDistAll <- SpecDistanceDecay(SpecAll,LatLon)
#by hab type
BetaDistBR <- SpecDistanceDecay(SpecBR,LatLon)
BetaDistM <- SpecDistanceDecay(SpecM,LatLon)
BetaDistCY <- SpecDistanceDecay(SpecCY,LatLon)
#by edge dist
BetaDist0 <- SpecDistanceDecay(Spec0,LatLon)
BetaDist30 <- SpecDistanceDecay(Spec30,LatLon)
BetaDist100 <- SpecDistanceDecay(Spec100,LatLon)
BetaDist250 <- SpecDistanceDecay(Spec250,LatLon)
BetaDist500 <- SpecDistanceDecay(Spec500,LatLon)
#edge and Core
BetaDistEdge <- SpecDistanceDecay(SpecEdge,LatLon)
BetaDistCore <- SpecDistanceDecay(SpecCore,LatLon)

#plot and model these distance decays!
#all
ggplot(data=BetaDistAll,
       mapping = aes(x=Distance ,y=Beta)) +
  geom_point(size = 1.5, alpha=0.5, color = "green") +
  geom_smooth(method="lm")+
  ggtitle("All ASF Distance Decay")

#BR
ggplot(data=BetaDistBR,
       mapping = aes(x=Distance ,y=Beta)) +
  geom_point(size = 1.5, alpha=0.5, color = "green") +
  geom_smooth(method="lm")+
  ggtitle("Brachystegia Forest Distance Decay")

#M
ggplot(data=BetaDistM,
       mapping = aes(x=Distance ,y=Beta)) +
  geom_point(size = 1.5, alpha=0.5, color = "green") +
  geom_smooth(method="lm")+
  ggtitle("Mixed Forest Distance Decay")

#CY
ggplot(data=BetaDistCY,
       mapping = aes(x=Distance ,y=Beta)) +
  geom_point(size = 1.5, alpha=0.5, color = "green") +
  geom_smooth(method="lm")+
  ggtitle("Cynometera Forest Distance Decay")

#
#By Edge Dist
#

# at edge 0
ggplot(data=BetaDist0,
       mapping = aes(x=Distance ,y=Beta)) +
  geom_point(size = 1.5, alpha=0.5, color = "green") +
  geom_smooth(method="lm")+
  ggtitle("Edge = 0 Distance Decay")

# at edge 30
ggplot(data=BetaDist30,
       mapping = aes(x=Distance ,y=Beta)) +
  geom_point(size = 1.5, alpha=0.5, color = "green") +
  geom_smooth(method="lm")+
  ggtitle("Edge = 30 Distance Decay")

# at edge 100
ggplot(data=BetaDist100,
       mapping = aes(x=Distance ,y=Beta)) +
  geom_point(size = 1.5, alpha=0.5, color = "green") +
  geom_smooth(method="lm")+
  ggtitle("Edge = 100m Distance Decay")

# at edge 250
ggplot(data=BetaDist250,
       mapping = aes(x=Distance ,y=Beta)) +
  geom_point(size = 1.5, alpha=0.5, color = "green") +
  geom_smooth(method="lm")+
  ggtitle("Edge = 250m Distance Decay")


# at edge 500
ggplot(data=BetaDist500,
       mapping = aes(x=Distance ,y=Beta)) +
  geom_point(size = 1.5, alpha=0.5, color = "green") +
  geom_smooth(method="lm")+
  ggtitle("Edge = 500m Distance Decay")

# at edge 0 and 30m
ggplot(data=BetaDistEdge,
       mapping = aes(x=Distance ,y=Beta)) +
  geom_point(size = 1.5, alpha=0.5, color = "green") +
  geom_smooth(method="lm")+
  ggtitle("Edge (0-30m) Distance Decay")
# at edge 0 and 30m
ggplot(data=BetaDistCore,
       mapping = aes(x=Distance ,y=Beta)) +
  geom_point(size = 1.5, alpha=0.5, color = "green") +
  geom_smooth(method="lm")+
  ggtitle("Core (100-500m) Distance Decay")

#
#MODELS
#
#all
modAll <- lm(Beta~Distance,BetaDistAll)
summary(modAll)
#forest types
modBR <- lm(Beta~Distance,BetaDistBR)
summary(modBR)
modM <- lm(Beta~Distance,BetaDistM)
summary(modM)
modCY <- lm(Beta~Distance,BetaDistCY)
summary(modCY)
#edge distance
mod0 <- lm(Beta~Distance,BetaDist0)
summary(mod0)
mod30<- lm(Beta~Distance,BetaDist30)
summary(mod30)
mod100 <- lm(Beta~Distance,BetaDist100)
summary(mod100)
mod250 <- lm(Beta~Distance,BetaDist250)
summary(mod250)
mod500 <- lm(Beta~Distance,BetaDist500)
summary(mod500)
modEdge <- lm(Beta~Distance,BetaDistEdge)
summary(modEdge)
modCore <- lm(Beta~Distance,BetaDistCore)
summary(modEdge)

#
#
#
#
#DO NOT NEED THE BELLOW, IT IS THE 
#EXCESSIVE CODE I USED BEFORE WRITING THE FUNCTION
#
#
#
#


#remove species with no records
#for all
SpecAll <- SpecAll %>%
  select(which(!colSums(SpecAll) %in% 0)) #remove specie with no records in this dataset
#by forest type
SpecBR <- SpecBR %>%
  select(which(!colSums(SpecBR) %in% 0))
SpecM <- SpecM %>%
  select(which(!colSums(SpecM) %in% 0))
SpecCY <- SpecCY %>%
  select(which(!colSums(SpecCY) %in% 0))
#by edge distance
Spec0 <- Spec0 %>%
  select(which(!colSums(Spec0) %in% 0))
Spec30 <- Spec30 %>%
  select(which(!colSums(Spec30) %in% 0))
Spec100 <- Spec100 %>%
  select(which(!colSums(Spec100) %in% 0))
Spec250 <- Spec250 %>%
  select(which(!colSums(Spec250) %in% 0))
Spec500 <- Spec500 %>%
  select(which(!colSums(Spec500) %in% 0))


#remove sites with no species
SpecAll$RowSum <- rowSums(SpecAll)

SpecAll <- SpecAll %>% #remove column with rowsums again
  filter(RowSum > 0) %>%
  select(-RowSum)



