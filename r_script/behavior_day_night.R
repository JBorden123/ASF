
##### behavior : variation day and night

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
animals <- read.csv("raw_data/herpdata.csv", header = TRUE)


############# GRAPHS

##### height

# graph height during day or night, c and G surveys, by species
ggplot(data = filter(animals, survey_type == "C" | survey_type == "G")) + geom_boxplot(mapping = aes(x = binomial, y = height_found_m, color = day_night)) + geom_jitter(mapping = aes(x = binomial, y = height_found_m, color = day_night), alpha = 0.3, width = 0.1, height = 0.1) + labs(x = "Species", y = "Height found (m)", title = "Height found by species during day or night") + theme_bw(base_size = 13) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_color_manual(labels = c("Day", "Night"), values = c("yellow3", "midnightblue"))

# graph abundance day or night
ggplot(data = filter(animals, survey_type == "C" | survey_type == "G")) + geom_histogram(mapping = aes(x = binomial, fill = day_night), stat = "count", position = "dodge") + labs(x = "Species", y = "Number of individuals", title = "Number of individuals found by species during day or night") + theme_bw(base_size = 13) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_manual(labels = c("Day", "Night"), values = c("yellow3", "midnightblue"))


##### habitat
## !! need to sort substrate

#graph habitat by day or night, species with good sample size
ggplot(data = filter(animals,survey_type == "C" | survey_type == "G", species_code == "CHDI" | species_code == "HEPL" | species_code == "LYMO" | species_code == "HEMA")) + geom_histogram(mapping = aes(x = substrate, fill = day_night), stat = "count", position = "dodge") + theme_bw(base_size = 13) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_manual(labels = c("Day", "Night"), values = c("yellow3", "midnightblue")) + facet_grid(species_code~., scales = "free_y") + labs(title = "Habitat use by day and night, a few species, C and G surveys", x = "Substrate", y = "Number of individuals") + theme_bw(base_size = 13)

#graph habitat by day or night, all species
ggplot(data = filter(animals,survey_type == "C" | survey_type == "G")) + geom_histogram(mapping = aes(x = substrate, fill = day_night), stat = "count", position = "dodge") + theme_bw(base_size = 13) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_manual(labels = c("Day", "Night"), values = c("yellow3", "midnightblue")) + labs(title = "Habitat use by day and night, all species, C and G surveys", x = "Substrate", y = "Number of individuals") 

