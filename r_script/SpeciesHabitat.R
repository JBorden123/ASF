
#### habitat preferences of abundant species


###charge_packages###
library(tidyverse)
library(ggplot2)
library(vegan)
library(dplyr)
library(forcats)
library(questionr)
library(reshape)
library(reshape2)
library(xlsx)
library(readxl)
library(readr)



#### CHARGE DATA
data <- read.csv("clean_data/MetaAll.csv", header = TRUE)
animals <- read.csv("raw_data/herpdata.csv", header = TRUE)

# put metadata in animals
animals <- merge(animals, data,intersect = c("Tree_ID", "Tree_ID"), all.x = TRUE, all.y = FALSE)




######################## GRAPHS 



######## habitat description VS abundance of a few species

# HEPL
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = DBH_cm, y = HEPL))  + geom_smooth(mapping = aes(x = DBH_cm, y = HEPL), method = "lm") + labs(title = "HEPL")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = HOT_m, y = HEPL))+ geom_smooth(mapping = aes(x = HOT_m, y = HEPL), method = "lm") + labs(title = "HEPL")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = edge_category_m, y = HEPL))+ geom_smooth(mapping = aes(x = edge_category_m, y = HEPL), method = "lm") + labs(title = "HEPL")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = TotalAvgCan, y = HEPL)) + geom_smooth(mapping = aes(x = TotalAvgCan, y = HEPL), method = "lm")+ labs(title = "HEPL")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = AvgHerbCover, y = HEPL)) + geom_smooth(mapping = aes(x = AvgHerbCover, y = HEPL), method = "lm")+ labs(title = "HEPL")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = AvgLeafLayer, y = HEPL)) + geom_smooth(mapping = aes(x = AvgLeafLayer, y = HEPL), method = "lm")+ labs(title = "HEPL")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = StemLess8cm, y = HEPL)) + geom_smooth(mapping = aes(x = StemLess8cm, y = HEPL), method = "lm")+ labs(title = "HEPL")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = StemMore8cm, y = HEPL)) + geom_smooth(mapping = aes(x = StemMore8cm, y = HEPL), method = "lm")+ labs(title = "HEPL")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = Debris, y = HEPL)) + geom_smooth(mapping = aes(x = Debris, y = HEPL), method = "lm")+ labs(title = "HEPL")

# HEMA
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = DBH_cm, y = HEMA))  + geom_smooth(mapping = aes(x = DBH_cm, y = HEMA), method = "lm") + labs(title = "HEMA")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = HOT_m, y = HEMA))+ geom_smooth(mapping = aes(x = HOT_m, y = HEMA), method = "lm") + labs(title = "HEMA")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = edge_category_m, y = HEMA))+ geom_smooth(mapping = aes(x = edge_category_m, y = HEMA), method = "lm") + labs(title = "HEMA")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = TotalAvgCan, y = HEMA)) + geom_smooth(mapping = aes(x = TotalAvgCan, y = HEMA), method = "lm")+ labs(title = "HEMA")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = AvgHerbCover, y = HEMA)) + geom_smooth(mapping = aes(x = AvgHerbCover, y = HEMA), method = "lm")+ labs(title = "HEMA")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = AvgLeafLayer, y = HEMA)) + geom_smooth(mapping = aes(x = AvgLeafLayer, y = HEMA), method = "lm")+ labs(title = "HEMA")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = StemLess8cm, y = HEMA)) + geom_smooth(mapping = aes(x = StemLess8cm, y = HEMA), method = "lm")+ labs(title = "HEMA")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = StemMore8cm, y = HEMA)) + geom_smooth(mapping = aes(x = StemMore8cm, y = HEMA), method = "lm")+ labs(title = "HEMA")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = Debris, y = HEMA)) + geom_smooth(mapping = aes(x = Debris, y = HEMA), method = "lm")+ labs(title = "HEMA")

# LYMO
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = DBH_cm, y = LYMO))  + geom_smooth(mapping = aes(x = DBH_cm, y = LYMO), method = "lm") + labs(title = "LYMO")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = HOT_m, y = LYMO))+ geom_smooth(mapping = aes(x = HOT_m, y = LYMO), method = "lm") + labs(title = "LYMO")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = edge_category_m, y = LYMO))+ geom_smooth(mapping = aes(x = edge_category_m, y = LYMO), method = "lm") + labs(title = "LYMO")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = TotalAvgCan, y = LYMO)) + geom_smooth(mapping = aes(x = TotalAvgCan, y = LYMO), method = "lm")+ labs(title = "LYMO")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = AvgHerbCover, y = LYMO)) + geom_smooth(mapping = aes(x = AvgHerbCover, y = LYMO), method = "lm")+ labs(title = "LYMO")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = AvgLeafLayer, y = LYMO)) + geom_smooth(mapping = aes(x = AvgLeafLayer, y = LYMO), method = "lm")+ labs(title = "LYMO")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = StemLess8cm, y = LYMO)) + geom_smooth(mapping = aes(x = StemLess8cm, y = LYMO), method = "lm")+ labs(title = "LYMO")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = StemMore8cm, y = LYMO)) + geom_smooth(mapping = aes(x = StemMore8cm, y = LYMO), method = "lm")+ labs(title = "LYMO")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = Debris, y = LYMO)) + geom_smooth(mapping = aes(x = Debris, y = LYMO), method = "lm")+ labs(title = "LYMO")

# CHDI
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = DBH_cm, y = CHDI))  + geom_smooth(mapping = aes(x = DBH_cm, y = CHDI), method = "lm") + labs(title = "CHDI")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = HOT_m, y = CHDI))+ geom_smooth(mapping = aes(x = HOT_m, y = CHDI), method = "lm") + labs(title = "CHDI")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = edge_category_m, y = CHDI))+ geom_smooth(mapping = aes(x = edge_category_m, y = CHDI), method = "lm") + labs(title = "CHDI")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = TotalAvgCan, y = CHDI)) + geom_smooth(mapping = aes(x = TotalAvgCan, y = CHDI), method = "lm")+ labs(title = "CHDI")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = AvgHerbCover, y = CHDI)) + geom_smooth(mapping = aes(x = AvgHerbCover, y = CHDI), method = "lm")+ labs(title = "CHDI")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = AvgLeafLayer, y = CHDI)) + geom_smooth(mapping = aes(x = AvgLeafLayer, y = CHDI), method = "lm")+ labs(title = "CHDI")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = StemLess8cm, y = CHDI)) + geom_smooth(mapping = aes(x = StemLess8cm, y = CHDI), method = "lm")+ labs(title = "CHDI")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = StemMore8cm, y = CHDI)) + geom_smooth(mapping = aes(x = StemMore8cm, y = CHDI), method = "lm")+ labs(title = "CHDI")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = Debris, y = CHDI)) + geom_smooth(mapping = aes(x = Debris, y = CHDI), method = "lm")+ labs(title = "CHDI")

# NUBO
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = DBH_cm, y = NUBO))  + geom_smooth(mapping = aes(x = DBH_cm, y = NUBO), method = "lm") + labs(title = "NUBO")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = HOT_m, y = NUBO))+ geom_smooth(mapping = aes(x = HOT_m, y = NUBO), method = "lm") + labs(title = "NUBO")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = edge_category_m, y = NUBO))+ geom_smooth(mapping = aes(x = edge_category_m, y = NUBO), method = "lm") + labs(title = "NUBO")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = TotalAvgCan, y = NUBO)) + geom_smooth(mapping = aes(x = TotalAvgCan, y = NUBO), method = "lm")+ labs(title = "NUBO")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = AvgHerbCover, y = NUBO)) + geom_smooth(mapping = aes(x = AvgHerbCover, y = NUBO), method = "lm")+ labs(title = "NUBO")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = AvgLeafLayer, y = NUBO)) + geom_smooth(mapping = aes(x = AvgLeafLayer, y = NUBO), method = "lm")+ labs(title = "NUBO")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = StemLess8cm, y = NUBO)) + geom_smooth(mapping = aes(x = StemLess8cm, y = NUBO), method = "lm")+ labs(title = "NUBO")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = StemMore8cm, y = NUBO)) + geom_smooth(mapping = aes(x = StemMore8cm, y = NUBO), method = "lm")+ labs(title = "NUBO")
ggplot(data = filter(data, extra_ground != "Y")) + geom_point(mapping = aes(x = Debris, y = NUBO)) + geom_smooth(mapping = aes(x = Debris, y = NUBO), method = "lm")+ labs(title = "NUBO")



############# species abundance by forest type

ggplot(data = filter(animals, survey_type == "C" | survey_type == "G"))  + geom_histogram(mapping = aes(x = binomial, fill = forest_type), stat = "count", position = "dodge") + theme_bw(base_size = 13) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 



############# abundance of each species by edge category

# edge category in factor
animals$edge_category_rec <- as.character(as.numeric(animals$edge_category_m))
## reorder animals$edge_category_rec
animals$edge_category_rec <- factor(animals$edge_category_rec, levels=c("-10", "0", "30", "100", "250", "500"))
ggplot(data = filter(animals, survey_type == "C" | survey_type == "G"))  + geom_histogram(mapping = aes(x = binomial, fill = edge_category_rec), stat = "count", position = "dodge") + theme_bw(base_size = 13) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# and by forest type
ggplot(data = filter(animals, survey_type == "C" | survey_type == "G"))  + geom_histogram(mapping = aes(x = binomial, fill = edge_category_rec), stat = "count", position = "dodge") + theme_bw(base_size = 13) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_grid(forest_type~.)






