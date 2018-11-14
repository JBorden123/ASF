#This script cleans my site data and subsets it into all data by habitat type,
#environmental data by habitat type and species data by habitat type
#then I will go from there into multivariate analysis

library(raster)
library(vegan)
#install.packages("ca")
library(ca)
library(dplyr)
library(ggplot2)
select <- dplyr::select

#You will also be using a function from a Biostats package developed
#by Kevin McGarigal. Save this package (i.e., R script) to your 
#working directory and use the source function to call it in:
source("biostats.r")

#data
MetaAll <- read.csv("clean_data/MetaAll.csv")



################
#CLEAN ALL DATA AND MAKE SUBSETS
################
#All
CleanMetaAll <- MetaAll %>%
  filter(extra_ground != "Y") %>%
  filter(edge_category_m > -5) 

#Brachy  
CleanMetaBR <- CleanMetaAll %>%
  filter(forest_type == "BR")
  
#Mixed
CleanMetaM <- CleanMetaAll %>%
  filter(forest_type == "M")

#Cynometra
CleanMetaCY <- CleanMetaAll %>%
  filter(forest_type == "CY")

summary(CleanMetaAll$Tree_ID)
summary(CleanMetaAll$forest_type)


#####################
#Creating species matrix and predictor variable matrices
SpecAll <- CleanMetaAll %>%
  select(ARST:VAAL) 
row.names(SpecAll) <- CleanMetaAll$Tree_ID
#remove sites with no species from both species and env data sets
SpecAll2 <- SpecAll
#remove sites with no species
SpecAll2$RowSum <- rowSums(SpecAll)

SpecAll2 <- SpecAll2 %>%
  filter(RowSum > 0) %>%
  select(-RowSum)
#remove sites with no species from env data
EnvAll$RowSum <- rowSums(SpecAll) 
EnvAll <- EnvAll %>%
  filter(RowSum >0) %>%
  select(-RowSum)


SpecAll <- SpecAll2




SpecAll <- SpecAll %>%
  select(which(!colSums(SpecAll) %in% 0)) #remove specie with no records in this dataset
  
SpecM <- CleanMetaM %>%
  select(ARST:VAAL)

SpecBR <- CleanMetaBR %>%
  select(ARST:VAAL)

SpecCY <- CleanMetaCY %>%
  select(ARST:VAAL)

#Einvironmental variables
EnvAll <- CleanMetaAll %>% #herbaceous cover??
  select(edge_category_m, AvgLeafLayer, 
         TotalAvgCan, BasalArea, Debris, StemLess8cm, StemMore8cm, forest_type)

EnvM <- EnvAll %>%
  filter(forest_type == "M") %>%
  select(edge_category_m, AvgLeafLayer, 
         TotalAvgCan, BasalArea, Debris, StemLess8cm, StemMore8cm)

EnvBR <- CleanMetaAll %>%
  filter(forest_type == "BR") %>%
  select(edge_category_m, AvgLeafLayer, 
         TotalAvgCan, BasalArea, Debris, StemLess8cm, StemMore8cm)
  
EnvCY <- CleanMetaAll %>%
  filter(forest_type == "CY") %>%
  select(edge_category_m, AvgLeafLayer, 
         TotalAvgCan, BasalArea, Debris, StemLess8cm, StemMore8cm)

#this loop replaces NA values with the column average
#all
for(i in 1:ncol(EnvAll)){ 
  EnvAll[is.na(EnvAll[,i]), i] <- mean(EnvAll[,i], na.rm = TRUE)
}
#mixed forest
for(i in 1:ncol(EnvM)){
  EnvM[is.na(EnvM[,i]), i] <- mean(EnvM[,i], na.rm = TRUE)
}
#brachy
for(i in 1:ncol(EnvBR)){
  EnvBR[is.na(EnvBR[,i]), i] <- mean(EnvBR[,i], na.rm = TRUE)
}
#cynometera
for(i in 1:ncol(EnvCY)){
  EnvCY[is.na(EnvCY[,i]), i] <- mean(EnvCY[,i], na.rm = TRUE)
}



##############################
#Distance based RDA
####### db-RDA pages(pages 249-251 in Brocard et al.)
##########
#All data and habitat types

#data
spe <- SpecAll
env <- EnvAll

str(env)


#name each environmental varaible
EdgeDist <- env$edge_category_m
LeafLayer <- env$AvgLeafLayer
Canopy <- env$TotalAvgCan
Basal <- env$BasalArea
Debris <- env$Debris
StemSmall <- env$StemLess8cm
StemLarge <- env$StemMore8cm
Hab <- env$forest_type

#Use the "capscale" function in Vegan to run db-rda.Note that the "distance" 
#argument turns the site by species matrix into a distance matrix. You can use 
#any distance measure in vegan (i.e., vegdist function)

db.rda <- capscale(spe ~ EdgeDist + LeafLayer + Basal + Debris + StemSmall +
                     StemLarge + Hab, distance = "bray", add=TRUE)
summary(db.rda)


#R2 and adjusted R2
R2 <- RsquareAdj(db.rda)$r.squared
R2adj <- RsquareAdj(db.rda)$adj.r.squared


#Plot using the F-scores:
par(mfrow=c(1,1))
plot(db.rda, scaling=1, 
     main="Triplot db-rda F scores", xlim = c(-4,7))
spe.sc <- scores(db.rda, choices=1:2, scaling=2, display="sp")
arrows(0, 0, spe.sc[, 1], spe.sc[, 2], length=0, lty=1, col="red")

#Plot using the Z-scores:
plot(db.rda, scaling=1, display=c("sp", "lc", "cn"), 
     main="Triplot db-rda  Z scores")
arrows(0, 0, spe.sc[, 1], spe.sc[, 2], length=0, lty=1, col="red")


#Conduct a permutation test using anova function in vegan to test the significance of the model, individual axes, and varaibles:

#Global test of the RDA result
anova(db.rda, step=1000)

#Tests of all canonical axes:
anova(db.rda, by="axis", step=1000)

#Tests of all variables:
anova(db.rda, by="margin", step=1000)

#partial RDA
#Use the "capscale" function in Vegan to run partial db-rda

db.rda <- capscale(spe ~ alt + Condition(oxy + dbo + dur), distance = "bray", add=TRUE)
summary(db.rda)

R2 <- RsquareAdj(db.rda)$r.squared
R2adj <- RsquareAdj(db.rda)$adj.r.squared


#Here we partition the variance for the model we constructed trough forward selection above: 
#first we have to create our distance matrix as the response matrix
resp<-vegdist(spe, method="bray")

#then we run the varpart function from vegan
spe.part <- varpart(resp,~ alt,~ oxy,~ dur ,~dbo ,data=env2)
plot(spe.part, digits=2)


