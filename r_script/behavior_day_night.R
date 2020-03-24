
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



######### height



## graph height during day or night, c and G surveys only, by species
# (G and C surveys only, because need same number of ground and canopy surveys between day and night, because for example more ground surveys during day will bring lower meah height durnig day because canopy less explored) )


# keep only G and C
data <- filter(animals, binomial != "NA", survey_type == "C" | survey_type == "G", extra_ground != "Y", category != "matrix")
# create median height to then order the boxplot by descending median
a <- data %>% group_by(binomial) %>% summarise(Median_height = median(height_found_m, na.rm = TRUE))
data <- merge(data, a, by = "binomial", all = TRUE)
#remove ground species
data <- data %>% filter(Median_height > 0)
#plot
ggplot(data = data, mapping = aes(x = reorder(binomial, -Median_height), y = height_found_m, color = day_night))  + theme_bw(base_size = 23)  + geom_boxplot(position = position_dodge(width=0.9)) + geom_point (position = position_jitterdodge(dodge.width=0.9), alpha = 0.2) + labs(title = "Height by species (reptiles, G and C surveys, no matrix)", x = "Species", y = "Height (m)")  + scale_color_manual(labels = c("Day", "Night"), values = c("yellow3", "midnightblue")) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_flip()
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "figures/HeightSpeciesDayNight.png")




#### plot Height VS day/night, few species with good sample size and ID, plus p-value



## Recodage de data$day_night
data$day_night <- fct_recode(data$day_night,
               "Day" = "D",
               "Night" = "N")



# HEMI

df <- data %>% filter(species_code == "HEMI")

ggplot(data = df) + geom_density(mapping = aes(x = height_found_m))
shapiro.test(df$height_found_m)

compare_means(height_found_m ~ day_night,  data = df)


my_comparisons <- list("Day", "Night")

grapha <- ggboxplot(data = df , x = "day_night", y = "height_found_m") + geom_jitter(alpha = 0.2, width = 0.1, height = 0.1) + theme_bw(base_size = 23)+ labs(title = "Hemidactylus mrimaensis", x = "", y = "Height (m)") + ylim(0, 17) +
  stat_compare_means(label.y = 15, size = 6)     # Add global p-value

plot(grapha)





#HEMA

df <- data %>% filter(species_code == "HEMA")

ggplot(data = df) + geom_density(mapping = aes(x = height_found_m))
shapiro.test(df$height_found_m)

compare_means(height_found_m ~ day_night,  data = df)


my_comparisons <- list("Day", "Night")

graphb <- ggboxplot(data = df , x = "day_night", y = "height_found_m") + geom_jitter(alpha = 0.2, width = 0.1, height = 0.1) + theme_bw(base_size = 23)+ labs(title = "Hemidactylus mabouia", x = "", y = "") + ylim(0, 17) +
  stat_compare_means(label.y = 15, size = 6)     # Add global p-value

plot(graphb)





#HEPL

df <- data %>% filter(species_code == "HEPL")

ggplot(data = df) + geom_density(mapping = aes(x = height_found_m))
shapiro.test(df$height_found_m)

compare_means(height_found_m ~ day_night,  data = df)


my_comparisons <- list("Day", "Night")

graphc <- ggboxplot(data = df , x = "day_night", y = "height_found_m") + geom_jitter(alpha = 0.2, width = 0.1, height = 0.1) + theme_bw(base_size = 23)+ labs(title = "Hemidactylus platycephalus", x = "", y = "Height (m)") + ylim(0, 17) +
  stat_compare_means(label.y = 15, size = 6)     # Add global p-value

plot(graphc)



#LYMO


df <- data %>% filter(species_code == "LYMO")

ggplot(data = df) + geom_density(mapping = aes(x = height_found_m))
shapiro.test(df$height_found_m)

compare_means(height_found_m ~ day_night,  data = df)


my_comparisons <- list("Day", "Night")

graphd <- ggboxplot(data = df , x = "day_night", y = "height_found_m") + geom_jitter(alpha = 0.2, width = 0.1, height = 0.1) + theme_bw(base_size = 23)+ labs(title = "Lygodactylus mombasicus", x = "", y = "") + ylim(0, 17) +
  stat_compare_means(label.y = 15, size = 6)     # Add global p-value

plot(graphd)


#CHDI

df <- data %>% filter(species_code == "CHDI")

ggplot(data = df) + geom_density(mapping = aes(x = height_found_m))
shapiro.test(df$height_found_m)

compare_means(height_found_m ~ day_night,  data = df)


my_comparisons <- list("Day", "Night")

graphe <- ggboxplot(data = df , x = "day_night", y = "height_found_m") + geom_jitter(alpha = 0.2, width = 0.1, height = 0.1) + theme_bw(base_size = 23)+ labs(title = "Chamaeleo dilepis", x = "", y = "Height (m)") + ylim(0, 17) +
  stat_compare_means(label.y = 15, size = 6)     # Add global p-value

plot(graphe)




#TRPL

df <- data %>% filter(species_code == "TRPL")

ggplot(data = df) + geom_density(mapping = aes(x = height_found_m))
shapiro.test(df$height_found_m)

compare_means(height_found_m ~ day_night,  data = df)


my_comparisons <- list("Day", "Night")

graphf <- ggboxplot(data = df , x = "day_night", y = "height_found_m") + geom_jitter(alpha = 0.2, width = 0.1, height = 0.1) + theme_bw(base_size = 23)+ labs(title = "Trachylepis planifrons", x = "", y = "") + ylim(0, 17) +
  stat_compare_means(label.y = 15, size = 6)     # Add global p-value

plot(graphf)


#PLOT TOGETHER

png("figures/height_VS_day_night_few_species.png", width = 1000, height = 1200)
plot_grid(grapha, graphb, graphc, graphd, graphe, graphf, ncol=2, labels=c("A", "B", "C", "D", "E", "F"), label_size = 23)
dev.off()








#### boxplot of day / night height by forest type

# keep G and C data
data <- filter(animals, survey_type == "C" | survey_type == "G", extra_ground != "Y", category != "matrix")
## Recodage de data$day_night
data$day_night <- fct_recode(data$day_night,
                             "Day" = "D",
                             "Night" = "N")

# G and C surveys
grapha <- ggplot(data = data, mapping = aes(x = day_night, y = height_found_m)) + geom_boxplot() + geom_jitter(alpha = 0.2, width = 0.05, height = 0.05)  + labs(x = "", y = "Height found (m)", title = "Height VS day/night by forest type, G and C only") + theme_bw(base_size = 23) + scale_color_manual(labels = c("Day", "Night"), values = c("yellow3", "midnightblue")) + facet_grid(.~forest_type)
plot(grapha)

# all
ggplot(data = animals, mapping = aes(x = forest_type, y = height_found_m, color = day_night)) + geom_boxplot(position = position_dodge(width=0.9)) + geom_point (position = position_jitterdodge(dodge.width=0.9), alpha = 0.2) + labs(x = "Forest type", y = "Height found (m)", title = "Height VS day/night by forest type, all surveys") + theme_bw(base_size = 23) + scale_color_manual(labels = c("Day", "Night"), values = c("yellow3", "midnightblue"))








#####number of individuals by day/night by forest type
# (??? nearly no nocturnal species in CY forest ??)




#restricted to G and C surveys
graphb <- ggplot(data = data, mapping = aes(x = day_night)) + geom_bar() + facet_grid(.~forest_type) + labs(x = "", y = "Number of individuals", title = "Abundance VS day/night by forest type, G and C only") + theme_bw(base_size = 23)
plot(graphb)

# all surveys (useless because no same number of surveys between day/night for matrix and extra ground)
ggplot(data = animals, mapping = aes(x = day_night)) + geom_bar() + facet_grid(.~forest_type)


#PLOT TOGETHER

png("figures/height_and_abundance_VS_day_night_forest_type.png", width = 800, height = 700)
plot_grid(grapha, graphb, ncol=1, labels=c("A", "B"), label_size = 23)
dev.off()








#number of individuals by survey type by forest type
ggplot(data = animals, mapping = aes(x = survey_type)) + geom_bar() + facet_grid(.~forest_type)







##### abundance by species day / night

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








data <- filter(animals, binomial != "NA")
# create median height to then order the boxplot by descending median
a <- data %>% group_by(binomial) %>% summarise(Median_height = median(height_found_m, na.rm = TRUE))
data <- merge(data, a, by = "binomial", all = TRUE)
#remove ground species
data <- data %>% filter(Median_height > 0)
#plot
ggplot(data = data, mapping = aes(x = reorder(binomial, -Median_height), y = height_found_m, color = day_night))  + theme_bw(base_size = 13)  + geom_boxplot(position = position_dodge(width=0.9)) + geom_point (position = position_jitterdodge(dodge.width=0.9), alpha = 0.2) + labs(title = "Height by species (reptiles, G and C surveys, no matrix)", x = "Species", y = "Height (m)")  + scale_color_manual(labels = c("Day", "Night"), values = c("yellow3", "midnightblue")) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_flip()










