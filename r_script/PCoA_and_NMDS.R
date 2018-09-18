#data
biodiv_data <- read.csv("clean_data/biodiv_data.csv", row.names = "Tree_ID")

#libraries
library(vegan)
library(ca)
library(ggplot2)
library(psych)
library(dplyr)

metadata <- read.csv("clean_data/metadata.csv")

#alter the data
BiodivData <- biodiv_data[,-c(1,25:29)] #keeping only species data


#look at species data
describe(BiodivData)

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



#NMDS
#dissimilarity matrix
bray_biodiv <- vegdist(biodiv_data, "bray")

nmds_biodiv <- metaMDS(bray_biodiv, k = 2, trace = T)

ordiplot(nmds_biodiv, type = "t", main = "NMDS Arabuko Herps")

