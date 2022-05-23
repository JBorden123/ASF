#This script cleans my site data and subsets it into all data by habitat type,
#environmental data by habitat type and species data by habitat type
#then I will go from there into multivariate analysis conducting dbRDA for 
#all sites, for each forest type on its own. And finally I will run a CART model
#to try to characterize if there are any significant differences from edge
#to core.

library(raster)
library(vegan)
#install.packages("ca")
library(ca)
#install.packages("ggvegan")
#library(ggvegan)
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
  filter(edge_category_m > -5) #remove matrix and ground surveys without canopy climbs
#this is to ensure that all the data has the same sampling effort (1 climb and 1 ground)

CleanMetaAll <- CleanMetaAll[!duplicated(CleanMetaAll$Tree_ID),]

row.names(CleanMetaAll) <- CleanMetaAll$Tree_ID #create row names from column

CleanMetaAll <- CleanMetaAll %>% #remove site column which is now row names
  select(-Tree_ID)

summary(CleanMetaAll$forest_type)


#####################
#Creating species matrix and predictor variable matrices

#species across all sites:
SpecAll <- CleanMetaAll %>%
  select(ARST:VAAL) 

SpecAll <- SpecAll %>%
  select(which(!colSums(SpecAll) %in% 0)) #remove specie with no records in this dataset

#remove sites with no species from both species and env data sets
SpecAll2 <- SpecAll
#remove sites with no species
SpecAll2$RowSum <- rowSums(SpecAll)

#add column of forest type back in
SpecAll2$forest_type <- CleanMetaAll$forest_type

SpecAll2 <- SpecAll2 %>% #remove column with rowsums again
  filter(RowSum > 0) %>%
  select(-RowSum)

#Einvironmental variables EnvAll will be used in db.rda and EnvAll2 
#will be used in the CART
EnvAll2 <- CleanMetaAll %>% #herbaceous cover??
  select(edge_category_m, AvgLeafLayer, 
         TotalAvgCan, BasalArea, Debris, StemLess8cm, 
         StemMore8cm, forest_type, AvgHerbCover)

#remove sites with no species from env data
EnvAll2$RowSum <- rowSums(SpecAll) 
EnvAll <- EnvAll2 %>%
  filter(RowSum >0) %>%
  select(-RowSum)

EnvAll2 <- EnvAll2 %>% #this data for CART model
  select(-RowSum) #remove rowsum column

EnvM <- EnvAll %>% #create environmental data for Mixed forest
  filter(forest_type == "M") %>%
  select(edge_category_m, AvgLeafLayer, 
         TotalAvgCan, BasalArea, Debris, StemLess8cm, StemMore8cm, AvgHerbCover)

EnvBR <- EnvAll %>% #create environmental data for Brachy forest
  filter(forest_type == "BR") %>%
  select(edge_category_m, AvgLeafLayer, 
         TotalAvgCan, BasalArea, Debris, StemLess8cm, StemMore8cm, AvgHerbCover)
  
EnvCY <- EnvAll %>% #create environmental data for Cyno forest
  filter(forest_type == "CY") %>%
  select(edge_category_m, AvgLeafLayer, 
         TotalAvgCan, BasalArea, Debris, StemLess8cm, StemMore8cm, AvgHerbCover)

#Set species data for each habitat type
SpecAll <- SpecAll2 %>%
  select(ARST:VAAL)

SpecM <- SpecAll2 %>%
  filter(forest_type == "M") %>%
 select(ARST:VAAL)

SpecBR <- SpecAll2 %>%
   filter(forest_type == "BR")%>%
   select(ARST:VAAL)

SpecCY <- SpecAll2 %>%
  filter(forest_type == "CY")%>%
  select(ARST:VAAL)
 
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


summary(EnvAll$forest_type)

##############################
#Distance based RDA
####### db-RDA pages(pages 249-251 in Brocard et al.)
##########


#All data and habitat types
#data
spe <- SpecAll
env <- EnvAll

#don't need to transform and standardize data for distance based RDA

#transform species data
#Log_spe <- log1p(spe)

#standardize species data
#csum <- colSums(Log_spe)

#Stand_spec <- sweep(Log_spe, 2, csum, "/") #final transformed and standardized spec matrix

#Stand_spec2 <- Stand_spec #create second dataset to add forest type back in
#Stand_spec2$forest_type <- SpecAll2$forest_type

#Stand_specM <- Stand_spec2 %>% 
#  filter(forest_type == "M") %>%
#  select(-forest_type)

#Stand_specBR <- Stand_spec2 %>% 
#  filter(forest_type == "BR") %>%
#  select(-forest_type)

#Stand_specCY <- Stand_spec2 %>% 
#  filter(forest_type == "CY") %>%
#  select(-forest_type)


#environmental data
str(env)

env2 <- env %>%
  select(-forest_type)

env2 <- as.data.frame(scale(env2))

env2$forest_type <- env$forest_type

#look at correlations between enviromental variables
round(as.dist(cor(env2)), 2)

#name each environmental varaible for all data
EdgeDist <- env2$edge_category_m
LeafLayer <- env2$AvgLeafLayer
Canopy <- env2$TotalAvgCan
Basal <- env2$BasalArea
Debris <- env2$Debris
StemSmall <- env2$StemLess8cm
StemLarge <- env2$StemMore8cm
HerbCover <- env2$AvgHerbCover
Hab <- env2$forest_type

#for mixed
EdgeDistM <- EnvM$edge_category_m
LeafLayerM <- EnvM$AvgLeafLayer
CanopyM <- EnvM$TotalAvgCan
BasalM <- EnvM$BasalArea
DebrisM <- EnvM$Debris
StemSmallM <- EnvM$StemLess8cm
StemLargeM <- EnvM$StemMore8cm
HerbCoverM <- EnvM$AvgHerbCover

#fore BR
EdgeDistBR <- EnvBR$edge_category_m
LeafLayerBR <- EnvBR$AvgLeafLayer
CanopyBR <- EnvBR$TotalAvgCan
BasalBR <- EnvBR$BasalArea
DebrisBR <- EnvBR$Debris
StemSmallBR <- EnvBR$StemLess8cm
StemLargeBR <- EnvBR$StemMore8cm
HerbCoverBR <- EnvBR$AvgHerbCover

#Use the "capscale" function in Vegan to run db-rda.Note that the "distance" 
#argument turns the site by species matrix into a distance matrix. You can use 
#any distance measure in vegan (i.e., vegdist function)

??capscale

#Consider adding an interaction term between habitat
#do variance partitioning to 

#all
db.rda <- capscale(spe ~ EdgeDist +
                     Basal +
                     Canopy +
                     LeafLayer + 
                     StemSmall+ 
                     StemLarge + 
                     Debris +
                     HerbCover+
                     Hab, distance = "bray", add=TRUE)
summary(db.rda)

#Mixed
db.rdaM <- capscale(SpecM ~ EdgeDistM +
                      LeafLayerM + 
                      CanopyM +
                      BasalM + 
                      StemSmallM + 
                      StemLargeM+
                      HerbCoverM+
                      DebrisM, 
                   distance = "bray", add=TRUE)
summary(db.rdaM)

#Brachystegia
db.rdaBR <- capscale(SpecBR ~ EdgeDistBR +
                       LeafLayerBR + 
                       CanopyBR +
                       BasalBR +
                       StemSmallBR +
                       StemLargeBR+
                       HerbCoverBR+
                       DebrisBR,
                     distance = "bray", add=TRUE)
summary(db.rdaBR)


#R2 and adjusted R2
R2 <- RsquareAdj(db.rda)$r.squared
R2adj <- RsquareAdj(db.rda)$adj.r.squared

R2M <- RsquareAdj(db.rdaM)$r.squared
R2adjM <- RsquareAdj(db.rdaM)$adj.r.squared

R2BR <- RsquareAdj(db.rdaBR)$r.squared
R2adjBR <- RsquareAdj(db.rdaBR)$adj.r.squared

#Plot using the F-scores:
par(mfrow=c(1,1))
#All 
plot(db.rda, scaling=2, 
     main="Triplot ASF db-rda F scores", xlim = c(-8,8))
spe.sc <- scores(db.rda, choices=1:2, scaling=2, display="sp")
arrows(0, 0, spe.sc[, 1], spe.sc[, 2], length=0, lty=1, col="red")

#Plot using the Z-scores:
plot(db.rda, scaling=2, display=c("sp", "lc", "cn"), 
     main="Triplot ASF db-rda  Z scores", xlim = c(-8,8))
arrows(0, 0, spe.sc[, 1], spe.sc[, 2], length=0, lty=1, col="red")

#Mixed
plot(db.rdaM, scaling=1, 
     main="Triplot Mixed Forest db-rda F scores", xlim = c(-8,8))
spe.sc <- scores(db.rdaM, choices=1:2, scaling=2, display="sp")
arrows(0, 0, spe.sc[, 1], spe.sc[, 2], length=0, lty=1, col="red")

#Plot using the Z-scores:
plot(db.rdaM, scaling=2, display=c("sp", "lc", "cn"), 
     main="Triplot Mixed Forest db-rda  Z scores", xlim = c(-8,8))
arrows(0, 0, spe.sc[, 1], spe.sc[, 2], length=0, lty=1, col="red")

#Brachystegia
plot(db.rdaBR, scaling=2, 
     main="Triplot Brachystegia Forest db-rda F scores", xlim = c(-8,8))
spe.sc <- scores(db.rdaBR, choices=1:2, scaling=2, display="sp")
arrows(0, 0, spe.sc[, 1], spe.sc[, 2], length=0, lty=1, col="red")

#Plot using the Z-scores:
plot(db.rdaBR, scaling=2, display=c("sp", "lc", "cn"), 
     main="Triplot Brachystegia Forest db-rda  Z scores", xlim = c(-8,8))
arrows(0, 0, spe.sc[, 1], spe.sc[, 2], length=0, lty=1, col="red")



#Conduct a permutation test using anova function in vegan to test 
#the significance of the model, individual axes, and varaibles:
#ultimately its explaining very little but still something. 
#Use a global RDA with habitat type and... 


#Global test of the RDA result
anova(db.rda, step=1000)
anova(db.rdaM, step = 1000)
anova(db.rdaBR , step = 1000)

#Tests of all canonical axes:
anova(db.rda, by="axis", step=1000)
anova(db.rdaM, by = "axis", step = 1000)
anova(db.rdaBR, by = "axis", step = 1000)


#Tests of all variables:
anova(db.rda, by="margin", step=1000)
anova(db.rdaM, by = "margin", step = 1000)
anova(db.rdaBR, by = "margin", step = 1000)

#Here we partition the variance for the model we constructed trough forward selection above: 
#first we have to create our distance matrix as the response matrix
resp<-vegdist(spe, method="bray")

#then we run the varpart function from vegan
spe.part <- varpart(resp, ~edge_category_m, ~StemLess8cm, 
                    ~forest_type, ~BasalArea, data = env2)
plot(spe.part, digits=2)

summary(spe.part)

##########
#Questions for Ben

#HOw and if I can use percentage data in the distance RDA?

#do I do the same steps for regular RDA, log transforming and scaling before conducing 
#an distance based RDA?

#do i need to remove sites with no species? 









#################################################
#Classification and Regression Trees (CART)
################################################

# CART allows you to determine what 
#variables are important in separating groups. It differs 
#from DA in that it does not have the assumptions of a 
#parametric test and thus provides a more flexible approach 
#to discrimination.
par(mfrow = c(1,1))
#libraries
library(MASS)
library(rpart)
library(ade4)
library(vegan)

################
#CART (CLASSIFICATION AND REGRESSION TREES)

#the data
EnvAll2 #my all data

EnvAll2M <- EnvAll2 %>% #CART data with only M forest
  filter(forest_type == "M")

EnvAll2BR <- EnvAll2 %>% #CART data with only BR forest
  filter(forest_type == "BR")


#Setting the training and testing data
#Like you did for DA, you are going to split our data set into 
#“training” data and “testing” data. This will allow you to 
#test the predictions of the CART model on a “new” data set.
#Randomly select 75 samples from the iris data set. 
#Use “set.seed” so we are all working with the same training set:
set.seed(10)
train <- sample(1:73, 35) #train for all data
train2 <- sample(1:30, 15) #train for M and BR data

#Check the frequency of each species in the training data, 
#to make sure they are relatively proportional to the frequency 
#in the complete data set.
summary(as.factor(EnvAll2$edge_category_m))
summary(EnvAll2$forest_type)

freq <- table(EnvAll2$edge_category_m[train])
freq

#Specifying the model
#Next specify the CART model. In the iris data set, 
#Species is the categorical response variables and the four 
#measures of flower morphology are the explanatory variables:

model <- edge_category_m ~ .
#“edge_category_m ~ .” is the model and the “dot” stands for all 
#of the variables (so you don’t have to type them all)

#Running the CART algorithm
#You will use the rpart function in the rpart package to 
#develop CART models.
`?`(rpart)

ASF_rpart <- rpart(edge_category_m ~ ., data = EnvAll2[train, ], 
                    method = "class", 
                    control = rpart.control(minsplit = 10))

ASF_rpartM <- rpart(edge_category_m ~ ., data = EnvAll2M[train2, ], 
                   method = "class", 
                   control = rpart.control(minsplit = 10))

ASF_rpartBR <- rpart(edge_category_m ~ ., data = EnvAll2BR[train2, ], 
                   method = "class", 
                   control = rpart.control(minsplit = 10))

#Look up what rpart.control and it’s parameters do and play
#with them!
summary(ASF_rpart) 
summary(ASF_rpartM)
summary(ASF_rpartBR)

#Plotting Cart Tree and viewing summary
#You will plot your tree using the function post.rpart:
`?`(post.rpart)
post(ASF_rpart, file = "", title = "ASF Habitat Classification Tree")
post(ASF_rpartM, file = "", title = "ASF Mixed Forest Habitat Classification Tree")
post(ASF_rpartBR, file = "", title = "ASF BR Habitat Classification Tree")

#Now, look at a node by node summary of the tree and the variable importance:
summary(ASF_rpart)
#Now let’s look at the cross-validation results:
printcp(ASF_rpart)
plotcp(ASF_rpart)

cp <- printcp(ASF_rpart)[4, 1]
cp

#Now, get out the pruning shears:
ASF_prune <- prune(ASF_rpart, cp = cp)

print(ASF_prune)
summary(ASF_prune)

#Plot pruned tree:
post(ASF_prune, file = "", title = "ASF Pruned Tree")

sum(residuals(ASF_prune)^2)

#Predict with both trees:
#what node/group are they assigned (groups named by mean ozone value)
pruned <- predict(ASF_prune, EnvAll2[-train, ], type = "vector")
full <- predict(ASF_rpart, EnvAll2[-train, ], type = "vector")


EnvAll2[-train,1]-full #diference between actual values and mean of its group



###########
#exploring
############
ggplot(data = EnvM, aes(edge_category_m, AvgLeafLayer))+
  geom_point()+
  geom_smooth()+
  geom_jitter()

ggplot(data = EnvAll2, aes(edge_category_m, AvgLeafLayer))+
  geom_point()+
  geom_smooth()+
  geom_jitter()

ggplot(data = EnvBR, aes(edge_category_m, AvgLeafLayer ))+
  geom_point()+
  geom_smooth()+
  geom_jitter()

ggplot(data = EnvM, aes(edge_category_m, TotalAvgCan))+
  geom_point()+
  geom_smooth(method = lm)+
  geom_jitter()

ggplot(data = EnvAll2, aes(edge_category_m, TotalAvgCan))+
  geom_point()+
  geom_smooth(method = lm)+
  geom_jitter()

ggplot(data = EnvBR, aes(edge_category_m, TotalAvgCan))+
  geom_point()+
  geom_smooth(method = lm)+
  geom_jitter()


summary(EnvAll2$AvgLeafLayer)
summary(EnvBR$AvgLeafLayer)
summary(EnvM$AvgLeafLayer)
