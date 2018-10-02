
#libraries
library(vegan)
library(ca)
library(ggplot2)
library(psych)
library(dplyr)
library(raster)
library(cluster)
library(pvclust)
select <- dplyr::select

#data
biodiv_data <- read.csv("clean_data/biodiv_data.csv", row.names = "Tree_ID")
MetaAll <- read.csv("clean_data/MetaAll.csv", header = TRUE)

MetaAll <- MetaAll %>%
  select(Tree_ID:VAAL, diversity_shannon:rich, DBH_cm:StemMore8cm)%>%
  select(-edge_dist_m)%>%
  select(-forest_type)

#select columns with continuous variables to run NMDS
#alter the data
BiodivData <- biodiv_data %>%
  select(ARST:VAAL)#keeping only species data

#look at species data
describe(BiodivData)

#######################
#PCoA
#dissimilarity matrix
bray_biodiv <- vegdist(biodiv_data, "bray")
#PCoA
cmd <- cmdscale(bray_biodiv, k = 10, eig = TRUE)

cmd$points 
#this is the scaled eigenvectors, which become the coordinates in PCoA space

#Let’s make a PCoA table to look at the eigenvalues, and the 
#proportional and cumulative variance:
eigenvalues <- cmd$eig[1:10]
propVar <- eigenvalues/sum(eigenvalues)
cumVar <- cumsum(propVar)
PCoA_Table <- cbind(eigenvalues, propVar, cumVar)
PCoA_Table

#Scree plot:
plot(eigenvalues)
lines(lowess(eigenvalues))

ordiplot(scores(cmd)[, c(1, 2)], type = "t", cex = 1, main = "ASF Herp PCoA")


################
#NMDS
#dissimilarity matrix
DistBiodiv <- vegdist(biodiv_data, "bray")

nmds_biodiv <- metaMDS(DistBiodiv, k = 2, trace = T)

ordiplot(nmds_biodiv, type = "t", main = "NMDS Arabuko Herps")

#habitat, edge and species composition
NMDSData <- MetaAll %>%
  filter(edge_category_m != -10) %>%
  filter(DBH_cm != "NA") %>%
  select(Tree_ID:rich, edge_category_m, TotalAvgCan, AvgHerbCover:StemMore8cm)

NMDSData2 <- NMDSData[,-1]

dist <- vegdist(NMDSData2, "bray")


######################
#K Means Clustering
HabSummary <- read.csv("clean_data/HabSummary.csv", row.names = "Tree_ID")
HabSummaryCluster <- HabSummary %>%
  select(-c(TotalAvgCan:CanPlus10))

par(mfrow = c(1,1))
apply(HabSummaryCluster, 2, hist)

hist((HabSummaryCluster$AvgHerbCover))
hist((HabSummaryCluster$AvgLeafLayer))
hist((HabSummaryCluster$StemLess8cm))
hist((HabSummaryCluster$StemMore8cm))

#test if we need to scale
HabSum.tot <- apply(HabSummaryCluster, 2, sum)
cv(HabSum.tot) #RULE OF THUMB: scale if cv is more than 50. Otherwise not needed.

HabSummaryCluster <- scale(HabSummaryCluster)

#Set a vector for the loop to fill. 
wss <- rep(0, 10)

#Run a loop for 1 to 8 clusters:
for (i in 1:10) # sets the number of times the loop will be run i.e., the number of clusters in this case)
  
  wss[i] <- sum(kmeans(HabSummaryCluster, centers = i,nstart=25)$withinss) # run the kmeans function for each number of clusters (i) and extract the within sum of squares for each.

#Check out you vector of within group sum of squares for one to eight groups:
wss 

#Make the scree plot:
plot(1:10, wss, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares")

#How many clusters are present in the best K-means solution? I would say 2

#Another indicator of how many cluster to use is looking at the average 
#silhouette width.
`?`(silhouette)
#We can run a similar loop to look at the average silhouette width:
sil <- rep(0, 10)
for (i in 2:8) sil[i] <- summary(silhouette(kmeans(HabSummaryCluster, centers = i, iter.max = 100, 
                                                   nstart = 25)$cluster, dist(HabSummaryCluster)))$avg.width
plot(2:10, sil[2:10], type = "b", xlab = "Number of groups", ylab = "average silhouette width ")






##########
#Clusters
###########
HabSum.kop <- kmeans(HabSummaryCluster, centers = 8, iter.max = 10, nstart = 25)

#Plot a scatter plot showing cluster designations:
pairs(HabSummaryCluster, panel = function(x, y, z) text(x, y, HabSum.kop$cluster))



#Calculating the distance/dissimilarity matrix
#Select the (an) appropriate dissimilarity metric for binary 
#data and calculate the dissimilarity/distance matrix.
rowSums(BiodivData) 

BiodivData <- BiodivData[which(rowSums(BiodivData) > 0),]

DistBiodiv <- vegdist(BiodivData, "bray")

#######################################################
#Polythetic Agglomerative Hierarchical Clustering (PAHC)
#######################################################

#You will use the hclust function in the stats package to 
#conduct PAHC. This hclust function contains the six fusion 
#methods we discussed in lecture. We will use hclust to cluster 
#the Caribbean bird data and construct dendrograms.
`?`(hclust)

#Clustering algorithms
singleTree <- hclust(DistBiodiv, method = "single")
completeTree <- hclust(DistBiodiv, method = "complete")
centroidTree <- hclust(DistBiodiv, method = "centroid")
medianTree <- hclust(DistBiodiv, method = "median")
averageTree <- hclust(DistBiodiv, method = "average")
wardTree <- hclust(DistBiodiv, method = "ward.D2")


par(mfrow = c(1, 1))
plot(averageTree)

#Agglomerative coefficient (HOW CLUSTERY IS THE DATA)
#First, calculate the agglomerative coefficient for each fusion method:
ag1 <- coef.hclust(singleTree)
ag2 <- coef.hclust(completeTree)
ag3 <- NA
ag4 <- NA
ag5 <- coef.hclust(averageTree)
ag6 <- coef.hclust(wardTree)


#Cophenetic correlation coefficient (WHICH IS THE BEST FIT / METHOD)
cc1 <- cor(DistBiodiv, cophenetic(singleTree))
cc2 <- cor(DistBiodiv, cophenetic(completeTree))
cc3 <- cor(DistBiodiv, cophenetic(centroidTree))
cc4 <- cor(DistBiodiv, cophenetic(medianTree))
cc5 <- cor(DistBiodiv, cophenetic(averageTree))
cc6 <- cor(DistBiodiv, cophenetic(wardTree))
cophCor <- round(c(cc1, cc2, cc3, cc4, cc5, cc6), 2)

#Let’s put this all in a table:
methods <- c("single", "complete", "centroid", "median", "average", "ward")
dendrogramTable <- data.frame(methods, cophCor, agc)

dendrogramTable #shows that average is the most important


##################################################
#Polythetic Divisive Hierarchical Clustering (PDHC)
#You will use the diana function in the cluster package to conduct PDHC. 
#We will use diana to cluster the Caribbean bird data and construct dendrograms.
`?`(diana)

#Clustering algorithm
diSpecies <- diana(DistBiodiv)

par(mfrow = c(1,1))
# Next, plot the dendrogram:
plot(diSpecies, which.plots = 2)

# calculate the divisive coefficient
diTree$dc
# and calculate the cophenetic correlation coefficient:
d.coph <- cor(distBirds, cophenetic(diTree))

