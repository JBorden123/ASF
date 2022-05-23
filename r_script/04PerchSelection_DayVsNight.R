
##### behavior : variation day and night

# !! check in same number of matrix and extra ground during day an dnight, to add them or not in the graphs

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


### substrate recoding

animals$substrate_rec <- fct_recode(animals$substrate,
                                    "Branch" = "branch",
                                    "Branch 1" = "branch1",
                                    "Branch 1" = "Branch1 / dead",
                                    "Branch 1" = "Branch1 / hole with water",
                                    "Branch 1" = "Branch1 / under bark",
                                    "Branch 6+" = "branch10",
                                    "Branch 6+" = "branch10+",
                                    "Branch 2" = "branch2",
                                    "Branch 3" = "branch3",
                                    "Branch 4" = "branch4",
                                    "Branch 5" = "branch5",
                                    "Branch 6+" = "branch5+",
                                    "Branch 6+" = "branch6",
                                    "Branch 6+" = "branch7",
                                    "Branch 6+" = "branch8",
                                    "Dirt" = "dirt",
                                    "Grass" = "grass",
                                    "Trunk" = "Hole / trunk",
                                    "Leaf" = "leaf",
                                    "Litter" = "litter",
                                    "Log" = "log",
                                    "Pole" = "pole",
                                    "Road" = "road",
                                    "Sand" = "sand",
                                    "Sand" = "Sand / bush",
                                    "Soil" = "Sand / soil",
                                    "Branch" = "small branch",
                                    "Soil" = "soil",
                                    "Stump" = "stump",
                                    "Trunk" = "trunk",
                                    "Trunk" = "Trunk / tree hole",
                                    "Trunk" = "Trunk / under bark",
                                    "Under debris" = "und_debris",
                                    "Vine" = "vine")



#################### GRAPHS

##### height

# graph height during day or night, c and G surveys, by species
data <- filter(animals, binomial != "NA", survey_type == "C" | survey_type == "G", extra_ground != "Y", category != "matrix")
a <- data %>% group_by(binomial) %>% summarise(Median_height = median(height_found_m, na.rm = TRUE))
data <- merge(data, a, by = "binomial", all = TRUE)
ggplot(data = data) + geom_boxplot(mapping = aes(x = reorder(binomial, -Median_height), y = height_found_m, color = day_night)) + geom_jitter(mapping = aes(x = reorder(binomial, -Median_height), y = height_found_m, color = day_night), alpha = 0.3, width = 0.1, height = 0.1) + theme_bw(base_size = 13) + labs(title = "Height by species (reptiles, G and C surveys, no matrix)", x = "Species", y = "Height (m)")  + scale_color_manual(labels = c("Day", "Night"), values = c("yellow3", "midnightblue")) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "figures/HeightSpeciesDayNight.png")


# boxplot of day / night height by forest type
ggplot(data = filter(animals, extra_ground != "Y", category != "matrix", survey_type == "C" | survey_type == "G")) + geom_boxplot(mapping = aes(x = forest_type, y = height_found_m, color = day_night)) + geom_jitter(mapping = aes(x = forest_type, y = height_found_m, color = day_night), alpha = 0.3, width = 0.1, height = 0.1) + labs(x = "Forest type", y = "Height found (m)", title = "Height found during day or night by forest type") + theme_bw(base_size = 13) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_color_manual(labels = c("Day", "Night"), values = c("yellow3", "midnightblue"))


##### abundance

# graph abundance day or night
ggplot(data = filter(animals, extra_ground != "Y", category != "matrix", survey_type == "C" | survey_type == "G")) + geom_histogram(mapping = aes(x = binomial, fill = day_night), stat = "count", position = "dodge") + labs(x = "Species", y = "Number of individuals", title = "Number of individuals found by species during day or night") + theme_bw(base_size = 13) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_manual(labels = c("Day", "Night"), values = c("yellow3", "midnightblue"))
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "figures/AbundanceDayNight.png")


######### habitat

#graph habitat by day or night, species with good sample size
animals$substrate_rec <- factor(animals$substrate_rec, levels=c("Under debris", "Road", "Soil", "Sand", "Dirt", "Litter", "Grass", "Log", "Stump", "Pole", "Trunk", "Branch", "Branch 1", "Branch 2", "Branch 3", "Branch 4", "Branch 5", "Branch 6+", "Vine", "Leaf", "NA"))
ggplot(data = filter(animals,extra_ground != "Y", category != "matrix", survey_type == "C" | survey_type == "G", species_code == "CHDI" | species_code == "HEPL" | species_code == "LYMO" | species_code == "HEMA")) + geom_histogram(mapping = aes(x = substrate_rec, fill = day_night), stat = "count", position = "dodge") + theme_bw(base_size = 13)  + scale_fill_manual(labels = c("Day", "Night"), values = c("yellow3", "midnightblue")) + facet_grid(species_code~., scales = "free_y") + labs(title = "Habitat use by day and night, a few species, C and G surveys", x = "Substrate", y = "Number of individuals") + theme_bw(base_size = 13) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "figures/HabitatUseDayNightFewSpecies.png")


#graph habitat by day or night, all species
animals$substrate_rec <- factor(animals$substrate_rec, levels=c("Under debris", "Road", "Soil", "Sand", "Dirt", "Litter", "Grass", "Log", "Stump", "Pole",  "Trunk", "Branch", "Branch 1", "Branch 2", "Branch 3", "Branch 4", "Branch 5", "Branch 6+", "Vine","Leaf",  "NA"))
ggplot(data = filter(animals,extra_ground != "Y", category != "matrix", survey_type == "C" | survey_type == "G")) + geom_histogram(mapping = aes(x = substrate_rec, fill = day_night), stat = "count", position = "dodge") + theme_bw(base_size = 13) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_manual(labels = c("Day", "Night"), values = c("yellow3", "midnightblue")) + labs(title = "Habitat use by day and night, all species, C and G surveys", x = "Substrate", y = "Number of individuals") 
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "figures/HabitatUseDayNightAllSpecies.png")



animals$substrate_rec <- factor(animals$substrate_rec, levels=c("Under debris", "Road", "Soil", "Sand", "Dirt", "Litter", "Grass", "Log", "Stump", "Pole",  "Trunk", "Branch", "Branch 1", "Branch 2", "Branch 3", "Branch 4", "Branch 5", "Branch 6+", "Vine","Leaf",  "NA"))
ggplot(data = filter(animals,extra_ground != "Y", category != "matrix", survey_type == "C" | survey_type == "G")) + geom_boxplot(mapping = aes(x = substrate_rec, y = SVL_cm, fill = day_night), position = "dodge") + geom_jitter(mapping = aes(x = substrate_rec, y = SVL_cm, colour = day_night), alpha = 0.4, height = 0.1, width = 0.3) + theme_bw(base_size = 13) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_manual(labels = c("Day", "Night"), values = c("yellow3", "midnightblue")) + scale_colour_manual(labels = c("Day", "Night"), values = c("yellow3", "midnightblue")) + labs(title = "Size by habitat by day and night, all species, C and G surveys (zoom < 20 cm)", x = "Substrate", y = "SVL (cm)") + scale_y_continuous(lim = c(0, 20))


animals$substrate_rec <- factor(animals$substrate_rec, levels=c("Under debris", "Road", "Soil", "Sand", "Dirt", "Litter", "Grass", "Log", "Stump", "Pole",  "Trunk", "Branch", "Branch 1", "Branch 2", "Branch 3", "Branch 4", "Branch 5", "Branch 6+", "Vine","Leaf",  "NA"))
ggplot(data = filter(animals,extra_ground != "Y", category != "matrix", survey_type == "C" | survey_type == "G", species_code == "CHDI" | species_code == "HEPL" | species_code == "LYMO" | species_code == "HEMA")) + geom_boxplot(mapping = aes(x = substrate_rec, y = SVL_cm, fill = day_night), position = "dodge")  + geom_jitter(mapping = aes(x = substrate_rec, y = SVL_cm, colour = day_night), alpha = 0.4, height = 0.1, width = 0.3) + theme_bw(base_size = 13) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_manual(labels = c("Day", "Night"), values = c("yellow3", "midnightblue"))+ scale_colour_manual(labels = c("Day", "Night"), values = c("yellow3", "midnightblue")) + labs(title = "Size by habitat by day and night, a few species, C and G surveys", x = "Substrate", y = "SVL (cm)") + facet_grid(species_code~., scales = "free_y")






