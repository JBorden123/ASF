#### SCRIPT to count number of each species by tree (climber survey), and by day and night
#### to detect species sharing habitat or not

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


######### create summary data frame

# !! canopy surveys only  to only get individuals in trees very close to each other, sharing habitat

#### total number of each species by tree
aggregation_by_tree <- animals %>% filter(survey_type == "C") %>% group_by(Tree_ID, binomial) %>% tally()

### add number total of individuals by tree
a <- animals %>% filter(survey_type == "C") %>% group_by(Tree_ID) %>% tally()
names(a)[names(a) == "n"] <- "n_total"
aggregation_by_tree <- merge(a, aggregation_by_tree, intersect = c("Tree_ID", "Tree_ID"), all = TRUE)
names(aggregation_by_tree)[names(aggregation_by_tree) == "n"] <- "n_species_total"

## add total of each species by day
a <- animals %>% filter(survey_type == "C", day_night == "D") %>% group_by(Tree_ID, binomial) %>% tally()
names(a)[names(a) == "n"] <- "n_species_day"

# add total of each species by night
b <- animals %>% filter(survey_type == "C", day_night == "N") %>% group_by(Tree_ID,  binomial) %>% tally()
names(b)[names(b) == "n"] <- "n_species_night"
#merge
a <- merge(a, b, intersect = c("Tree_ID", "Tree_ID"), all = TRUE)
#NA in 0
a[is.na(a)] <- 0
aggregation_by_tree <- merge(a, aggregation_by_tree, intersect = c("Tree_ID", "Tree_ID"), all = TRUE)

# add total number of individuals by day
a <- animals %>% filter(survey_type == "C", day_night == "D") %>% group_by(Tree_ID) %>% tally()
names(a)[names(a) == "n"] <- "n_total_day"
aggregation_by_tree <- merge(a, aggregation_by_tree, intersect = c("Tree_ID", "Tree_ID"), all = TRUE)

# add total number of individuals by night
a <- animals %>% filter(survey_type == "C", day_night == "N") %>% group_by(Tree_ID) %>% tally()
names(a)[names(a) == "n"] <- "n_total_night"
aggregation_by_tree <- merge(a, aggregation_by_tree, intersect = c("Tree_ID", "Tree_ID"), all = TRUE)
aggregation_by_tree[is.na(aggregation_by_tree)] <- 0

### reorder columns
aggregation_by_tree = aggregation_by_tree[,c("Tree_ID","n_total","n_total_day","n_total_night", "binomial", "n_species_total", "n_species_day", "n_species_night")]





### SUMMARY restricted to tree where at least one species with more than 1 individal
#get max number of individuals from same species
a <- aggregation_by_tree %>% group_by(binomial) %>% summarise(n_max_species = max(n_species_total))
# remove the ones with <= 1
a <- a %>% filter(n_max_species > 1)
# use this selection to get rest of the data by merging
aggregation_by_tree_restricted <- merge(a, aggregation_by_tree, intersect = c("Tree_ID", "Tree_ID"), all.x = TRUE, all.y = FALSE)
### reorder columns
aggregation_by_tree_restricted = aggregation_by_tree_restricted[,c("Tree_ID","n_total","n_total_day","n_total_night", "binomial", "n_species_total", "n_species_day", "n_species_night")]





################ GRAPHS


### graph density distribution of number of individuals by species in same tree area, only showing species with more than 1 individdual per tree area ever
# removing unidentified species
ggplot(data = filter(aggregation_by_tree_restricted, binomial != "NA", binomial != "Lygodactylus_sp.")) + geom_density(mapping = aes(x = n_species_total, color = binomial)) + labs(x = "Number of individuals from same species in same tree area", y = "Density", title = "Distribution of aggregation patterns by species with individuals in same tree area") + guides(color=guide_legend("Species")) + scale_x_continuous(limits=c(0, 8), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8)) + theme_bw(base_size = 10)
ggplot(data = filter(aggregation_by_tree_restricted, binomial != "NA", binomial != "Lygodactylus_sp.")) + geom_histogram(mapping = aes(x = n_species_total, fill = binomial), position = "dodge", binwidth = 1) + labs(x = "Number of individuals from same species in same tree area", y = "Number of tree areas", title = "Distribution of aggregation patterns by species with individuals in same tree area") + guides(color=guide_legend("Species")) + scale_y_continuous(limits=c(0, 10), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))+ scale_x_continuous(limits=c(0, 8), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8)) + theme_bw(base_size = 10)





