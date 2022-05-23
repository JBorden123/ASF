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

CleanMetaAll$edge_category_m <- as.factor(CleanMetaAll$edge_category_m)

MetaBR <- CleanMetaAll %>%
  filter(forest_type == "BR")

MetaM <- CleanMetaAll %>%
  filter(forest_type == "M")

MetaCY <- CleanMetaAll %>%
  filter(forest_type == "CY")


#all Habitat Types together
#small stem by edge
ggplot(CleanMetaAll, aes(x = edge_category_m, y = StemLess8cm,
                         color = forest_type, fill = forest_type, alpha = .1))+
  geom_boxplot(width = .4)+
  ggtitle("Small Woody Stem Count (<8cm), by Edge by Forest Type")
  #geom_jitter(height = 0, width = .2, aes(alpha = .2))

  # Large stem by edge
  ggplot(CleanMetaAll, aes(x = edge_category_m, y = StemMore8cm,
                           color = forest_type, fill = forest_type, alpha = .1))+
    stat_summary(geom = "point", fun.y = "mean", size = 2, position = position_dodge(0.3)) +
    stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.1, position = position_dodge(0.3))+
    ggtitle("Large Woody Stem Count (>8cm), by Edge by Forest Type")+
    stat_summary(geom = "line", fun.y = "mean")
    
      #geom_jitter(height = 0, width = .2, aes(alpha = .2, fill = forest_type))  

#basal area
  ggplot(CleanMetaAll, aes(x = edge_category_m, y = BasalArea,
                           color = forest_type, fill = forest_type, alpha = .1))+
    geom_boxplot(width = .4)+
    ggtitle("Basal Area by Edge by Forest Type")

#Debris
ggplot(CleanMetaAll, aes(x = edge_category_m, y = Debris,
                           color = forest_type, fill = forest_type, alpha = .1))+
    geom_boxplot(width = .4)+
    ggtitle("Corse Woody Debris by Edge by Forest Type")
  
#Avg Leaf layers
ggplot(CleanMetaAll, aes(x = edge_category_m, y = AvgLeafLayer,
                         color = forest_type, fill = forest_type, alpha = .1))+
  geom_boxplot(width = .4)+
  ggtitle("Leaf Layer Estimate by Edge by Forest Type")

#Herbaceous Cover
ggplot(CleanMetaAll, aes(x = edge_category_m, y = AvgHerbCover,
                         color = forest_type, fill = forest_type, alpha = .1))+
  geom_boxplot(width = .4)+
  ggtitle("Herbaceous Cover by Edge by Forest Type")

#Canopy Density
ggplot(CleanMetaAll, aes(x = edge_category_m, y = TotalAvgCan,
                         color = forest_type, fill = forest_type, alpha = .1))+
  geom_boxplot(width = .4)+
  ggtitle("Canopy Density by Edge by Forest Type")

  
#
#habitat types separately
#

# ASF all by small woody stem
ggplot(CleanMetaAll, aes(x = edge_category_m, y = StemLess8cm))+
    #geom_jitter(color = "gray", height = 0, width = .05)+
  geom_boxplot(width = .4)+
  geom_boxplot(data = MetaBR, aes(x= edge_category_m, y = StemLess8cm),
               color = "blue", width = .3)+
  geom_boxplot(data = MetaM, aes(x= edge_category_m, y = StemLess8cm),
               color = "green", width = .2)
  ggtitle("Small Stem by Edge ASF")

 #BR
ggplot(MetaBR, aes(x = edge_category_m, y = StemLess8cm))+
  geom_boxplot()+
  geom_jitter(color = "gray", height = 0, width = .05)+
  ggtitle("Small Stem by Edge BR")

#M
ggplot(MetaM, aes(x = edge_category_m, y = StemLess8cm))+
  geom_boxplot()+
  geom_jitter(color = "gray", height = 0, width = .05)+
  ggtitle("Small Stem by Edge M")

#CY
ggplot(MetaCY, aes(x = edge_category_m, y = StemLess8cm))+
  geom_boxplot()+
  geom_jitter(color = "gray", height = 0, width = .05)+
  ggtitle("Small Stem by Edge CY")

#basal Area
#all
ggplot(CleanMetaAll, aes(x = edge_category_m, y = BasalArea))+
  geom_boxplot()+
  geom_jitter(color = "gray", height = 0, width = .05)+
  ggtitle("Basal Area by Edge ASF")
  
#BR
ggplot(MetaBR, aes(x = edge_category_m, y = BasalArea))+
  geom_boxplot()+
  geom_jitter(color = "gray", height = 0, width = .05)+
  ggtitle("Basal Area by Edge BR")
  
#M
ggplot(MetaM, aes(x = edge_category_m, y = BasalArea))+
  geom_boxplot(aes(x = edge_category_m, y = BasalArea))+
  geom_jitter(color = "gray", height = 0, width = .05)+
  ggtitle("Basal Area by Edge M")

#CY
ggplot(MetaCY, aes(x = edge_category_m, y = BasalArea))+
  geom_boxplot()+
  geom_jitter(color = "gray", height = 0, width = .05)+
  ggtitle("Basal Area By Edge CY")

#large stems
#All
ggplot(CleanMetaAll, aes(x = edge_category_m, y = StemMore8cm))+
  geom_boxplot()+
  geom_smooth()

#BR
ggplot(MetaBR, aes(x = edge_category_m, y = StemMore8cm))+
  geom_point()+
  geom_smooth()

#M
ggplot(MetaM, aes(x = edge_category_m, y = StemMore8cm))+
  geom_boxplot()+
  geom_smooth()

#CY
ggplot(MetaCY, aes(x = edge_category_m, y = StemMore8cm))+
  geom_boxplot()+
  geom_smooth()
