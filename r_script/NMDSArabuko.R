
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
  select(Tree_ID:VAAL, diversity_shannon:rich, DBH_cm:StemMore8cm)

#select columns with continuous variables to run NMDS
#alter the data
BiodivData <- biodiv_data %>%
select(ARST:VAAL)#keeping only species data

#look at species data
describe(BiodivData)


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
  select(-HEAN)%>%
  select(Tree_ID:rich, edge_category_m, TotalAvgCan, AvgHerbCover:StemMore8cm)%>%
  filter(StemLess8cm > 0)

NMDSData2 <- NMDSData[,-1]

#NMDSData2 <- NMDSData2%>%
# add_rownames(var = NMDSData$Tree_ID)

describe(NMDSData2)

#NMDSData2 <- scale(NMDSData2)

dist <- vegdist(NMDSData2, "bray")

NMDSArabuko <- metaMDS(dist, k =2, trace = T)

NMDSdf <- as.data.frame(scores(NMDSArabuko))
NMDSdf$Tree_ID <- NMDSData$Tree_ID
NMDSdf <- merge(NMDSdf, MetaAll, by = "Tree_ID", all = FALSE)

ggplot() + 
  geom_point(data=NMDSdf, aes(x = NMDS1, y = NMDS2, shape = forest_type, colour = edge_category_m),size=2) + # add the point markers
  #geom_text(data=NMDSdf, aes(x = NMDS1, y = NMDS2, label = Tree_ID),alpha=0.8) +  
  #ylim(-.25,.25)+
  #xlim(-.5,.5)+
  theme_classic()
