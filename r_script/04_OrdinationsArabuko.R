
#libraries
library(vegan)
library(ca)
library(ggplot2)
library(psych)
library(dplyr)
library(raster)
library(cluster)
library(pvclust)
library(MVA)
library(Hmisc)
library(StatMatch)
library(MASS)
select <- dplyr::select

#data
biodiv_data <- read.csv("clean_data/biodiv_data.csv")
MetaAll <- read.csv("clean_data/MetaAll.csv", header = TRUE)
HabSummary <- read.csv("clean_data/HabSummary.csv")
biodiv_data <- merge(biodiv_data, HabSummary, by = "Tree_ID", 
                     all = FALSE)
Sites <- read.csv("raw_data/sites.csv")


#select columns with continuous variables to run NMDS
#alter the data
BiodivData <- biodiv_data %>%
select(ARST:VAAL)#keeping only species data

rowSums(BiodivData)
BiodivData$RowSums <- rowSums(BiodivData)
BiodivData <- BiodivData %>% 
  filter(RowSums > 0) %>% 
  select(-RowSums)

#look at species data
describe(BiodivData)


#######
#NMDS of biodiv data across all sites
########
biodiv_data2 <- biodiv_data %>%
  select(ARST:VAAL)

#biodiv_data2 <- scale(biodiv_data2)

#dissimilarity matrix
DistBiodiv <- vegdist(BiodivData, "bray")

nmds_biodiv <- metaMDS(DistBiodiv, k = 2, trace = T)

biodivNMDSdf <- as.data.frame(scores(nmds_biodiv))
biodivNMDSdf$Tree_ID <- biodiv_data$Tree_ID
biodivNMDSdf <- merge(biodivNMDSdf, MetaAll, by = "Tree_ID", all = FALSE)

ggplot()+
geom_point(data = biodivNMDSdf, aes(x = NMDS1, y = NMDS2,
                                    shape = forest_type, color = edge_category_m))+
  labs(title = "NMDS of Community Composition")+
  theme_classic()

################
#NMDS using habitat variables
habdata <- HabSummary %>%
  select(AvgHerbCover:BasalArea)

habdata[is.na(habdata)] <- 73.06

habdata <- scale(habdata)

distHab <- vegdist(habdata, "bray")

HabNMDS <- metaMDS(distHab, k =2, trace = T)

HabNMDSdf <- as.data.frame(scores(HabNMDS))
HabNMDSdf$Tree_ID <- HabSummary$Tree_ID
HabNMDSdf <- merge(HabNMDSdf, Sites, by = "Tree_ID", all = FALSE)

ggplot() + 
  geom_point(data=HabNMDSdf, aes(x = NMDS1, y = NMDS2, shape = forest_type, colour = edge_category_m),size=2) + # add the point markers
  #geom_text(data=NMDSdf, aes(x = NMDS1, y = NMDS2, label = Tree_ID),alpha=0.8) +  
  #ylim(-.25,.25)+
  #xlim(-.5,.5)+
  labs(title = "NMDS of Habitat variables")+
  theme_classic()

################
#NMDS using ALL variables
MetaAll <- MetaAll %>%
  select(Tree_ID:VAAL, diversity_shannon:rich, DBH_cm:StemMore8cm)


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

NMDSData2 <- scale(NMDSData2)

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
  labs(title = "NMDS of ASF all variables")+
  theme_classic()
  

##########################
#Testing for groups
##########################
group <- NMDSdf$forest_type
#use the anosim function in the vegan package. To test if there
#is any significant groupings by forest type when all variables are included
`?`(anosim)
set.seed(11)

ASFAnosim <- anosim(dist, group, permutations = 1000)

#Explore the output table and then plot the permuted F-ratios:
ASFAnosim$statistic # r statistic. -1 = out of group similarity, 
#0 = random groupings, 1 = good grouping

ASFAnosim$signif #p value
ASFAnosim$statistic

hist(ASFAnosim$perm, main = "Histogram of R statistics for ASF")
points(ASFAnosim$statistic, 0, pch = 19, col = "red", bg = "red", cex = 2)


