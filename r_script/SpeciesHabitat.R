
#### habitat preferences of abundant species

# !! all with C no extra surveyx nor matrix only to have same sampling effort

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
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = DBH_cm, y = HEPL))  + geom_smooth(mapping = aes(x = DBH_cm, y = HEPL), method = "lm") + labs(title = "HEPL")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = HOT_m, y = HEPL))+ geom_smooth(mapping = aes(x = HOT_m, y = HEPL), method = "lm") + labs(title = "HEPL")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = edge_category_m, y = HEPL))+ geom_smooth(mapping = aes(x = edge_category_m, y = HEPL), method = "lm") + labs(title = "HEPL")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = TotalAvgCan, y = HEPL)) + geom_smooth(mapping = aes(x = TotalAvgCan, y = HEPL), method = "lm")+ labs(title = "HEPL")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = AvgHerbCover, y = HEPL)) + geom_smooth(mapping = aes(x = AvgHerbCover, y = HEPL), method = "lm")+ labs(title = "HEPL")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = AvgLeafLayer, y = HEPL)) + geom_smooth(mapping = aes(x = AvgLeafLayer, y = HEPL), method = "lm")+ labs(title = "HEPL")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = StemLess8cm, y = HEPL)) + geom_smooth(mapping = aes(x = StemLess8cm, y = HEPL), method = "lm")+ labs(title = "HEPL")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = StemMore8cm, y = HEPL)) + geom_smooth(mapping = aes(x = StemMore8cm, y = HEPL), method = "lm")+ labs(title = "HEPL")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = Debris, y = HEPL)) + geom_smooth(mapping = aes(x = Debris, y = HEPL), method = "lm")+ labs(title = "HEPL")

# HEMA
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = DBH_cm, y = HEMA))  + geom_smooth(mapping = aes(x = DBH_cm, y = HEMA), method = "lm") + labs(title = "HEMA")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = HOT_m, y = HEMA))+ geom_smooth(mapping = aes(x = HOT_m, y = HEMA), method = "lm") + labs(title = "HEMA")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = edge_category_m, y = HEMA))+ geom_smooth(mapping = aes(x = edge_category_m, y = HEMA), method = "lm") + labs(title = "HEMA")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = TotalAvgCan, y = HEMA)) + geom_smooth(mapping = aes(x = TotalAvgCan, y = HEMA), method = "lm")+ labs(title = "HEMA")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = AvgHerbCover, y = HEMA)) + geom_smooth(mapping = aes(x = AvgHerbCover, y = HEMA), method = "lm")+ labs(title = "HEMA")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = AvgLeafLayer, y = HEMA)) + geom_smooth(mapping = aes(x = AvgLeafLayer, y = HEMA), method = "lm")+ labs(title = "HEMA")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = StemLess8cm, y = HEMA)) + geom_smooth(mapping = aes(x = StemLess8cm, y = HEMA), method = "lm")+ labs(title = "HEMA")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = StemMore8cm, y = HEMA)) + geom_smooth(mapping = aes(x = StemMore8cm, y = HEMA), method = "lm")+ labs(title = "HEMA")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = Debris, y = HEMA)) + geom_smooth(mapping = aes(x = Debris, y = HEMA), method = "lm")+ labs(title = "HEMA")

# LYMO
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = DBH_cm, y = LYMO))  + geom_smooth(mapping = aes(x = DBH_cm, y = LYMO), method = "lm") + labs(title = "LYMO")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = HOT_m, y = LYMO))+ geom_smooth(mapping = aes(x = HOT_m, y = LYMO), method = "lm") + labs(title = "LYMO")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = edge_category_m, y = LYMO))+ geom_smooth(mapping = aes(x = edge_category_m, y = LYMO), method = "lm") + labs(title = "LYMO")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = TotalAvgCan, y = LYMO)) + geom_smooth(mapping = aes(x = TotalAvgCan, y = LYMO), method = "lm")+ labs(title = "LYMO")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = AvgHerbCover, y = LYMO)) + geom_smooth(mapping = aes(x = AvgHerbCover, y = LYMO), method = "lm")+ labs(title = "LYMO")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = AvgLeafLayer, y = LYMO)) + geom_smooth(mapping = aes(x = AvgLeafLayer, y = LYMO), method = "lm")+ labs(title = "LYMO")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = StemLess8cm, y = LYMO)) + geom_smooth(mapping = aes(x = StemLess8cm, y = LYMO), method = "lm")+ labs(title = "LYMO")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = StemMore8cm, y = LYMO)) + geom_smooth(mapping = aes(x = StemMore8cm, y = LYMO), method = "lm")+ labs(title = "LYMO")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = Debris, y = LYMO)) + geom_smooth(mapping = aes(x = Debris, y = LYMO), method = "lm")+ labs(title = "LYMO")

# CHDI
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = DBH_cm, y = CHDI))  + geom_smooth(mapping = aes(x = DBH_cm, y = CHDI), method = "lm") + labs(title = "CHDI")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = HOT_m, y = CHDI))+ geom_smooth(mapping = aes(x = HOT_m, y = CHDI), method = "lm") + labs(title = "CHDI")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = edge_category_m, y = CHDI))+ geom_smooth(mapping = aes(x = edge_category_m, y = CHDI), method = "lm") + labs(title = "CHDI")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = TotalAvgCan, y = CHDI)) + geom_smooth(mapping = aes(x = TotalAvgCan, y = CHDI), method = "lm")+ labs(title = "CHDI")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = AvgHerbCover, y = CHDI)) + geom_smooth(mapping = aes(x = AvgHerbCover, y = CHDI), method = "lm")+ labs(title = "CHDI")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = AvgLeafLayer, y = CHDI)) + geom_smooth(mapping = aes(x = AvgLeafLayer, y = CHDI), method = "lm")+ labs(title = "CHDI")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = StemLess8cm, y = CHDI)) + geom_smooth(mapping = aes(x = StemLess8cm, y = CHDI), method = "lm")+ labs(title = "CHDI")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = StemMore8cm, y = CHDI)) + geom_smooth(mapping = aes(x = StemMore8cm, y = CHDI), method = "lm")+ labs(title = "CHDI")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = Debris, y = CHDI)) + geom_smooth(mapping = aes(x = Debris, y = CHDI), method = "lm")+ labs(title = "CHDI")

# NUBO
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = DBH_cm, y = NUBO))  + geom_smooth(mapping = aes(x = DBH_cm, y = NUBO), method = "lm") + labs(title = "NUBO")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = HOT_m, y = NUBO))+ geom_smooth(mapping = aes(x = HOT_m, y = NUBO), method = "lm") + labs(title = "NUBO")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = edge_category_m, y = NUBO))+ geom_smooth(mapping = aes(x = edge_category_m, y = NUBO), method = "lm") + labs(title = "NUBO")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = TotalAvgCan, y = NUBO)) + geom_smooth(mapping = aes(x = TotalAvgCan, y = NUBO), method = "lm")+ labs(title = "NUBO")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = AvgHerbCover, y = NUBO)) + geom_smooth(mapping = aes(x = AvgHerbCover, y = NUBO), method = "lm")+ labs(title = "NUBO")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = AvgLeafLayer, y = NUBO)) + geom_smooth(mapping = aes(x = AvgLeafLayer, y = NUBO), method = "lm")+ labs(title = "NUBO")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = StemLess8cm, y = NUBO)) + geom_smooth(mapping = aes(x = StemLess8cm, y = NUBO), method = "lm")+ labs(title = "NUBO")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = StemMore8cm, y = NUBO)) + geom_smooth(mapping = aes(x = StemMore8cm, y = NUBO), method = "lm")+ labs(title = "NUBO")
ggplot(data = filter(data, extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = Debris, y = NUBO)) + geom_smooth(mapping = aes(x = Debris, y = NUBO), method = "lm")+ labs(title = "NUBO")




############# species abundance by forest type

ggplot(data = filter(animals, extra_ground != "Y", category != "matrix"))  + geom_histogram(mapping = aes(x = binomial, fill = forest_type), stat = "count", position = "dodge", width = 0.5) + theme_bw(base_size = 13) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Abundance of each species by forest type (no matrix, no extra ground)", x = "Species", y = "Abundance")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "figures/Spec_Abun_by_Forest_Type.png")


############# abundance of each species by edge category

# edge category in factor
animals$edge_category_rec <- as.character(as.numeric(animals$edge_category_m))
## reorder animals$edge_category_rec
animals$edge_category_rec <- factor(animals$edge_category_rec, levels=c("-10", "0", "30", "100", "250", "500"))
ggplot(data = filter(animals, survey_type == "C" | survey_type == "G"))  + geom_histogram(mapping = aes(x = binomial, fill = edge_category_rec), stat = "count", position = "dodge", width = 0.5) + theme_bw(base_size = 13) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# and by forest type
a <- animals %>% filter(extra_ground != "Y", category != "matrix") %>% group_by(forest_type, binomial, edge_category_rec) %>% tally
ggplot(data = a) + geom_col(mapping = aes(x = binomial, y = n, fill = edge_category_rec), position = position_dodge2(width = 0.9, preserve = "single")) + theme_bw(base_size = 13) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_grid(forest_type~., scales = "free_y") + labs(title = "Abundance of each species by edge category by forest type (no matrix, no extra ground)", x = "Species", y = "Abundance")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "figures/Spec_Abun_by_edge_category_by_Forest_Type.png")



############ SVL VS height for a few species

# hemidactylus
ggplot(data = filter(animals, extra_ground != "Y", category != "matrix", genus == "Hemidactylus")) + geom_point(mapping = aes(x = SVL_cm, y = height_found_m)) + geom_smooth(mapping = aes(x = SVL_cm, y = height_found_m), method = "lm") + labs(title = "Hemidactylus, SVL VS height (C and G surveys, no matrix)", x = "SVL (cm)", y = "Height found (m)") + theme_bw(base_size = 13)
ggplot(data = filter(animals, survey_type == "C", extra_ground != "Y", category != "matrix", genus == "Hemidactylus")) + geom_point(mapping = aes(x = SVL_cm, y = height_found_m)) + geom_smooth(mapping = aes(x = SVL_cm, y = height_found_m), method = "lm") + labs(title = "Hemidactylus, SVL VS height (C surveys only)", x = "SVL (cm)", y = "Height found (m)") + theme_bw(base_size = 13)

# lygodactylus
ggplot(data = filter(animals, extra_ground != "Y", category != "matrix", genus == "Lygodactylus")) + geom_point(mapping = aes(x = SVL_cm, y = height_found_m)) + geom_smooth(mapping = aes(x = SVL_cm, y = height_found_m), method = "lm") + labs(title = "Lygodactylus, SVL VS height (C and G surveys, no matrix)", x = "SVL (cm)", y = "Height found (m)") + theme_bw(base_size = 13)
ggplot(data = filter(animals, survey_type == "C", extra_ground != "Y", category != "matrix", genus == "Lygodactylus")) + geom_point(mapping = aes(x = SVL_cm, y = height_found_m)) + geom_smooth(mapping = aes(x = SVL_cm, y = height_found_m), method = "lm") + labs(title = "Lygodactylus, SVL VS height (C surveys only)", x = "SVL (cm)", y = "Height found (m)") + theme_bw(base_size = 13)


#HEPL
ggplot(data = filter(animals, extra_ground != "Y", category != "matrix", species_code == "HEPL")) + geom_point(mapping = aes(x = SVL_cm, y = height_found_m)) + geom_smooth(mapping = aes(x = SVL_cm, y = height_found_m), method = "lm") + labs(title = "HEPL, SVL VS height (C and G surveys, no matrix)", x = "SVL (cm)", y = "Height found (m)") + theme_bw(base_size = 13)
ggplot(data = filter(animals, survey_type == "C", extra_ground != "Y", category != "matrix", species_code == "HEPL")) + geom_point(mapping = aes(x = SVL_cm, y = height_found_m)) + geom_smooth(mapping = aes(x = SVL_cm, y = height_found_m), method = "lm") + labs(title = "HEPL, SVL VS height (C surveys only)", x = "SVL (cm)", y = "Height found (m)") + theme_bw(base_size = 13)

#LYMO
ggplot(data = filter(animals, extra_ground != "Y", category != "matrix", species_code == "LYMO")) + geom_point(mapping = aes(x = SVL_cm, y = height_found_m)) + geom_smooth(mapping = aes(x = SVL_cm, y = height_found_m), method = "lm") + labs(title = "LYMO, SVL VS height (C and G surveys, no matrix)", x = "SVL (cm)", y = "Height found (m)") + theme_bw(base_size = 13)
ggplot(data = filter(animals, survey_type == "C", extra_ground != "Y", category != "matrix", species_code == "LYMO")) + geom_point(mapping = aes(x = SVL_cm, y = height_found_m)) + geom_smooth(mapping = aes(x = SVL_cm, y = height_found_m), method = "lm") + labs(title = "LYMO, SVL VS height (C surveys only)", x = "SVL (cm)", y = "Height found (m)") + theme_bw(base_size = 13)

#CHDI
ggplot(data = filter(animals, extra_ground != "Y", category != "matrix", species_code == "CHDI")) + geom_point(mapping = aes(x = SVL_cm, y = height_found_m)) + geom_smooth(mapping = aes(x = SVL_cm, y = height_found_m), method = "lm") + labs(title = "CHDI, SVL VS height (C and G surveys, no matrix)", x = "SVL (cm)", y = "Height found (m)") + theme_bw(base_size = 13)
ggplot(data = filter(animals, survey_type == "C", extra_ground != "Y", category != "matrix", species_code == "CHDI")) + geom_point(mapping = aes(x = SVL_cm, y = height_found_m)) + geom_smooth(mapping = aes(x = SVL_cm, y = height_found_m), method = "lm") + labs(title = "CHDI, SVL VS height (C surveys only)", x = "SVL (cm)", y = "Height found (m)") + theme_bw(base_size = 13)

#HEMA
ggplot(data = filter(animals, extra_ground != "Y", category != "matrix", species_code == "HEMA")) + geom_point(mapping = aes(x = SVL_cm, y = height_found_m)) + geom_smooth(mapping = aes(x = SVL_cm, y = height_found_m), method = "lm") + labs(title = "HEMA, SVL VS height (C and G surveys, no matrix)", x = "SVL (cm)", y = "Height found (m)") + theme_bw(base_size = 13)
ggplot(data = filter(animals, survey_type == "C", extra_ground != "Y", category != "matrix", species_code == "HEMA")) + geom_point(mapping = aes(x = SVL_cm, y = height_found_m)) + geom_smooth(mapping = aes(x = SVL_cm, y = height_found_m), method = "lm") + labs(title = "HEMA, SVL VS height (C surveys only)", x = "SVL (cm)", y = "Height found (m)") + theme_bw(base_size = 13)


