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



###CREATE_CSV_FROM_EXCEL###

animals <- read_excel("~/TRAVAIL/Kenya/DATA/arabuko_sokoke_10.xlsx", sheet = "Herps")
write.csv(animals, "D:/Documents/TRAVAIL/Kenya/DATA/animals.csv") 


surveys <- read_excel("~/TRAVAIL/Kenya/DATA/arabuko_sokoke_10.xlsx", sheet = "Surveys")
write.csv(surveys, "D:/Documents/TRAVAIL/Kenya/DATA/surveys.csv") 

loggers <- read_excel("~/TRAVAIL/Kenya/DATA/arabuko_sokoke_10.xlsx", sheet = "Loggers")
write.csv(loggers, "D:/Documents/TRAVAIL/Kenya/DATA/loggers.csv") 

key <- read_excel("~/TRAVAIL/Kenya/DATA/arabuko_sokoke_10.xlsx", sheet = "Key")
write.csv(key, "D:/Documents/TRAVAIL/Kenya/DATA/key.csv")



###CHARGE_DATA###

###charge_animals_data###
animals <- read_excel("~/TRAVAIL/Kenya/DATA/arabuko_sokoke_10.xlsx", sheet = "Herps")
View(animals)

##charge_sites_data###
sites <- read_excel("~/TRAVAIL/Kenya/DATA/arabuko_sokoke_10.xlsx", sheet = "Sites")
View(sites)

###charge_habitat_data###
habitat <- read_excel("~/TRAVAIL/Kenya/DATA/arabuko_sokoke_10.xlsx", sheet = "Habitat")




###RECODE_DATA###


###recoding_avg_leaf_int_in_numeric###
habitat$avg_leaf_int <- as.numeric(as.character(habitat$avg_leaf_int))

###recoding_stem_less_8cm_in_numeric###
habitat$stem_less_8cm <- as.numeric(as.character(habitat$stem_less_8cm))

###recoding_stem_more_8cm_in_numeric###
habitat$stem_more_8cm <- as.numeric(as.character(habitat$stem_more_8cm))

###recoding_avg_canopy_cover_in_numeric###
habitat$avg_canopy_cover <- as.numeric(as.character(habitat$avg_canopy_cover))

###recoding_wedge_prism_in_numeric###
habitat$wedge_prism <- as.numeric(as.character(habitat$wedge_prism))

###recoding_debris_in_numeric###
habitat$debris <- as.numeric(as.character(habitat$debris))

###recoding_avg_leaf_int_in_numeric###
habitat$avg_leaf_int <- as.numeric(as.character(habitat$avg_leaf_int))

###recoding_height_found_in_numeric###
animals$height_found_m <- as.numeric(as.character(animals$height_found_m))

###recoding_dist_from_center_in_numeric###
animals$dist_frm_center_m <- as.numeric(as.character(animals$dist_frm_center_m))

###recoding_DBH_cm_in_numeric###
sites$DBH_cm <- as.numeric(as.character(sites$DBH_cm))

###recoding_HOT_m_in_numeric###
sites$HOT_m <- as.numeric(as.character(sites$HOT_m))

###recoding_MHC_m_in_numeric###
sites$MHC_m <- as.numeric(as.character(sites$MHC_m))

###recoding_SVL_cm_in_numeric###
animals$SVL_cm <- as.numeric(as.character(animals$SVL_cm))

###recoding_tail_cm_in_numeric###
animals$tail_cm <- as.numeric(as.character(animals$tail_cm))

###recoding_mass_g_in_numeric###
animals$mass_g <- as.numeric(as.character(animals$mass_g))

###recode_edge_category_m_as_factor###
sites$edge_category_m <- as.character(as.numeric(sites$edge_category_m))

### keep edge_category_m_as_numeric###
sites$edge_category_m_numeric <- as.numeric(as.character(sites$edge_category_m))


###restrict_habitat_to_summarized_data###
sum_habitat <- select(habitat, Tree_ID, wedge_prism, debris, surveyor, avg_canopy_cover, avg_leaf_int, stem_less_8cm, stem_more_8cm)
sum_habitat <- filter(sum_habitat, stem_less_8cm != "NA")
View(sum_habitat)
write.csv(sum_habitat, "D:/Documents/TRAVAIL/Kenya/DATA/habitat_summarized.csv")

###add_colum_total_stem###
sum_habitat <- mutate(sum_habitat, stem_total = stem_less_8cm + stem_more_8cm)






###CREATE_DATA_FRAMES###

###create_number_of_each_species_by_site
Species_by_site <- (animals %>% group_by(Tree_ID, binomial) %>% tally %>% cast(Tree_ID~binomial, fill = 0))
View(Species_by_site)

###Link_Sites_and_Species_by_site###
Species_by_described_site <- merge(Species_by_site, sites, by = intersect("Tree_ID", "Tree_ID"), all = TRUE)
###and_change_NA_in_0_for number or each species###
Species_by_described_site[,2:27][is.na(Species_by_described_site[,2:27])]<-0

###Link_species_by_described_site_with_sum_habitat###
Species_by_described_site <- merge(Species_by_described_site, sum_habitat, by = intersect("Tree_ID", "Tree_ID"), all = TRUE)
View(Species_by_described_site)



###Link_animals_and sites###
animals_by_described_site <- merge(animals, sites, by = intersect("Tree_ID", "Tree_ID"), all = TRUE)
###Link it with sum habitat###
animals_by_described_site <- merge(animals_by_described_site, sum_habitat, by = intersect("Tree_ID", "Tree_ID"), all = TRUE)
###remove rows with no animals###
animals_by_described_site <- filter(animals_by_described_site, ind_numb != "NA")
###show it###


### create height found in % of HOT###
animals_by_described_site <- mutate(animals_by_described_site, height_found_percentage = height_found_m/HOT_m)
animals_by_described_site$height_found_percentage_breaks <- cut(animals_by_described_site$height_found_percentage, breaks = c(0, 0.05, 0.2, 0.4, 0.6, 0.8, 1),include.lowest = TRUE)
View(animals_by_described_site)

### create number of each species by height in %###
Species_by_height_found_percentage_breaks <- (animals_by_described_site %>% group_by(height_found_percentage_breaks, binomial) %>% tally %>% cast(height_found_percentage_breaks~binomial, fill = 0))
View(Species_by_height_found_percentage_breaks)



### recode substrate ###
animals_by_described_site$substrate_rec <- fct_recode(animals_by_described_site$substrate,
                                                      "Litter" = "litter",
                                                      "Branch_1_hole" = "Branch1 / hole with water",
                                                      "Branch_3" = "branch3",
                                                      "Sand" = "sand",
                                                      "Trunk_under_bark" = "Trunk / under bark",
                                                      "Trunk" = "trunk",
                                                      "Branch_1" = "branch1",
                                                      "Trunk_hole" = "Trunk / tree hole",
                                                      "Vine" = "vine",
                                                      "Branch" = "branch",
                                                      "Sand" = "Sand / bush",
                                                      "Branch_2" = "branch2",
                                                      "Branch_4" = "branch4",
                                                      "Branch_5+" = "branch8",
                                                      "Branch_5+" = "branch10",
                                                      "Branch_5+" = "branch7",
                                                      "Log" = "log",
                                                      "Leaf" = "leaf",
                                                      "Dirt" = "dirt",
                                                      "Pole" = "pole",
                                                      "Branch_5" = "branch5",
                                                      "Branch1_under_bark" = "Branch1 / under bark",
                                                      "Branch_1" = "Branch1 / dead",
                                                      "Trunk_hole" = "Hole / trunk",
                                                      "Branch_5+" = "branch5+",
                                                      "Under_debris" = "und_debris",
                                                      "Branch_5+" = "branch10+",
                                                      "Soil" = "soil",
                                                      "Road" = "road",
                                                      "Stump" = "stump",
                                                      "Branch_5+" = "branch6",
                                                      "Branch" = "small branch",
                                                      "Sand" = "Sand / soil",
                                                      "Grass" = "grass")

## Réordonnancement de animals_by_described_site$substrate_rec
animals_by_described_site$substrate_rec <- factor(animals_by_described_site$substrate_rec, levels=c("Under_debris", "Soil", "Dirt", "Sand", "Road", "Litter", "Grass", "Log", "Stump", "Pole", "Trunk", "Trunk_hole", "Trunk_under_bark", "Vine", "Branch", "Branch_1", "Branch_1_hole", "Branch1_under_bark", "Branch_2", "Branch_3", "Branch_4", "Branch_5", "Branch_5+", "Leaf", "NA"))




###EXPORT_DATA_FRAME(for_maping)###

###create_CSV_species_by_site###
write.csv(Species_by_site, "D:/Documents/TRAVAIL/Kenya/DATA/Species_by_site.csv") 

###create_CSV_habitat###
write.csv(habitat, "D:/Documents/TRAVAIL/Kenya/DATA/habitat.csv")

###create_CSV_sitest###
write.csv(sites, "D:/Documents/TRAVAIL/Kenya/DATA/sites.csv")

###create_CSV_animals###
write.csv(animals, "D:/Documents/TRAVAIL/Kenya/DATA/animals.csv")

###create_CSV_sum_habitat###
write.csv(sum_habitat, "D:/Documents/TRAVAIL/Kenya/DATA/habitat_summarized.csv")


######BIODIVERSISTY_INDEX#########

#generating abundances for all species by sites

animals2_biodivesity_index <-animals %>%
  group_by(Tree_ID,species_code) %>%
  tally()
animals2_biodivesity_index<- cast(animals2_biodivesity_index, Tree_ID ~ species_code, value='n')
animals2_biodivesity_index[is.na(animals2_biodivesity_index)]<-0


spec_names <- names(animals2_biodivesity_index[,2:25]) #check this to be sure this 
#captures correct names of species columns


########################
#Add diversity metrics
spec_counts <- animals2_biodivesity_index[,spec_names] 
View(spec_counts)

#shannon diversity index
animals2_biodivesity_index$diversity_shannon<-diversity(spec_counts, index = "shannon")

#simpsons
animals2_biodivesity_index$diversity_simpson <- diversity(spec_counts, index = "simpson")

#fisher's Alpha #####DO NOT FUNCTION####
animals2_biodivesity_index$fisher_alpha <- fisher.alpha(spec_counts)

#abundance
animals2_biodivesity_index$abund <-rowSums(animals2_biodivesity_index [,spec_names])

#richness
animals2_biodivesity_index$rich <- rowSums(animals2_biodivesity_index [,spec_names] > 0)

#species acummulation curve
spec_accum <- specaccum(spec_counts)
plot(spec_accum)

View(animals2_biodivesity_index)

###link it with description of habitats and sites###
animals_bio_index_by_described_site <- merge(animals2_biodivesity_index, sites, by = intersect("Tree_ID", "Tree_ID"), all = TRUE)
animals_bio_index_by_described_site <- merge(animals_bio_index_by_described_site, sum_habitat, by = intersect("Tree_ID", "Tree_ID"), all = TRUE)
###and change NA for 0###
animals_bio_index_by_described_site[,2:29][is.na(animals_bio_index_by_described_site[,2:29])]<-0

View(animals_bio_index_by_described_site)

#save it
write.csv(animals_bio_index_by_described_site, "D:/Documents/TRAVAIL/Kenya/DATA/animals_bio_index_by_described_site.csv")



#####BIODIVERSITY INDEX BY HEIGHT IN % OF HOT#####

spec_names_2 <- names(Species_by_height_found_percentage_breaks[,2:27]) #check this to be sure this 
#captures correct names of species columns

########################
#Add diversity metrics
spec_counts_2 <- Species_by_height_found_percentage_breaks[,spec_names_2] 
View(spec_counts_2)

#shannon diversity index
Species_by_height_found_percentage_breaks$diversity_shannon<-diversity(spec_counts_2, index = "shannon")

#simpsons
Species_by_height_found_percentage_breaks$diversity_simpson <- diversity(spec_counts_2, index = "simpson")

#fisher's Alpha #####DO NOT FUNCTION####
Species_by_height_found_percentage_breaks$fisher_alpha <- fisher.alpha(spec_counts_2)

#abundance
Species_by_height_found_percentage_breaks$abund <-rowSums(Species_by_height_found_percentage_breaks [,spec_names_2])

#richness
Species_by_height_found_percentage_breaks$rich <- rowSums(Species_by_height_found_percentage_breaks [,spec_names_2] > 0)


View(Species_by_height_found_percentage_breaks)

###Save it####
write.csv(Species_by_height_found_percentage_breaks, "D:/Documents/TRAVAIL/Kenya/DATA/Species_by_height_found_percentage_breaks.csv")





############# NEW SUMMARY DATA FRAMES #####

### height by species, min mean max, SVL min mean max, mass min mean max, plus n

## !! mean height with canopy surveys only to not lower mean height
## !! remove amphibians and unknown species

#sumarise
height_SVL_by_species <- animals %>% filter(amph_rept == "R", species != "sp.", binomial != "NA") %>% group_by(binomial) %>% summarise(Min_height_m = min(height_found_m, na.rm = TRUE), Max_height_m = max(height_found_m, na.rm = TRUE), Min_SVL_cm = min(SVL_cm, na.rm = TRUE), Max_SVL_cm = max(SVL_cm, na.rm = TRUE), Min_mass_g = min(mass_g, na.rm = TRUE), Max_mass_g = max(mass_g, na.rm = TRUE))
# calculate mean height with just Canopy surveys
a <- animals %>% filter(survey_type == "C", amph_rept == "R", species != "sp.", binomial != "NA") %>% group_by(binomial)  %>% summarise(Mean_height_m = mean(height_found_m, na.rm = TRUE))
height_SVL_by_species <- merge(a, height_SVL_by_species, by=c("binomial","binomial"), all= TRUE)
#add column number of individuals for mean height (just canopy surveys)
a <- animals %>% filter(survey_type == "C", amph_rept == "R", species != "sp.", binomial != "NA") %>% group_by(binomial) %>% tally()
height_SVL_by_species <- merge(a, height_SVL_by_species, by=c("binomial","binomial"), all = TRUE)
#rename columns
names(height_SVL_by_species)[names(height_SVL_by_species) == "n"] <- "n1"
#add column number of individuals
a <- animals %>% filter(amph_rept == "R", species != "sp.", binomial != "NA") %>% group_by(binomial) %>% tally()
height_SVL_by_species <- merge(a, height_SVL_by_species, by=c("binomial","binomial"), all = TRUE)

#number of decimals in each column
height_SVL_by_species$Min_height_m <- format(round(height_SVL_by_species$Min_height_m, 1), nsmall = 2)
height_SVL_by_species$Max_height_m <- format(round(height_SVL_by_species$Max_height_m, 1), nsmall = 2)
height_SVL_by_species$Mean_height_m <- format(round(height_SVL_by_species$Mean_height_m, 1), nsmall = 2)
height_SVL_by_species$Min_SVL_cm <- format(round(height_SVL_by_species$Min_SVL_cm, 1), nsmall = 1)
height_SVL_by_species$Max_SVL_cm <- format(round(height_SVL_by_species$Max_SVL_cm, 1), nsmall = 1)
height_SVL_by_species$Min_mass_g <- format(round(height_SVL_by_species$Min_mass_g, 2), nsmall = 2)
height_SVL_by_species$Max_mass_g <- format(round(height_SVL_by_species$Max_mass_g, 2), nsmall = 2)

# create column mean height with n for mean calculation
height_SVL_by_species <- height_SVL_by_species %>% mutate(Mean_height_m_2 = paste(Mean_height_m, " (", n1, ")", sep = ""))
# remove 2 columns used to build it
height_SVL_by_species$n1 = NULL
height_SVL_by_species$Mean_height_m = NULL
##rename columns
names(height_SVL_by_species)[names(height_SVL_by_species) == "Mean_height_m_2"] <- "Mean_height_m"


## add column family
height_SVL_by_species <- mutate(height_SVL_by_species, family = binomial)
## Recodage de height_SVL_by_species$Family en height_SVL_by_species$Family_rec
height_SVL_by_species$family <- fct_recode(height_SVL_by_species$family,
                                           "Chamaeleonidae" = "Chamaeleo_dilepis",
                                           "Cordylidae" = "Cordylus_tropidosternum",
                                           "Colubridae" = "Dispholidus_ typus",
                                           "Lacertidae" = "Gastropholis_prasina",
                                           "Gekkonidae" = "Hemidactylus_ barbouri",
                                           "Gekkonidae" = "Hemidactylus_angulatus",
                                           "Gekkonidae" = "Hemidactylus_mabouia",
                                           "Gekkonidae" = "Hemidactylus_mrimaensis",
                                           "Gekkonidae" = "Hemidactylus_platycephalus",
                                           "Lacertidae" = "Latastia_longicaudata",
                                           "Gekkonidae" = "Lygodactylus_mombasicus",
                                           "Scincidae" = "Mochlus_afer",
                                           "Scincidae" = "Mochlus_sundevalli",
                                           "Elapidae" = "Naja_melanoleuca",
                                           "Lacertidae" = "Nucras_boulengeri",
                                           "Lamprophiidae" = "Psammophis_ orientalis",
                                           "Lamprophiidae" = "Psammophis_ punctulatus",
                                           "Lamprophiidae" = "Rhamphiophis_rostratus",
                                           "Scincidae" = "Trachylepis_ maculilabris",
                                           "Scincidae" = "Trachylepis_planifrons",
                                           "Scincidae" = "Trachylepis_varia",
                                           "Varanidae" = "Varanus_albigularis")
# recode family as character
height_SVL_by_species$family <- as.character(as.factor(height_SVL_by_species$family))


## order columns
height_SVL_by_species = height_SVL_by_species[,c("family","binomial","n","Min_height_m", "Mean_height_m", "Max_height_m", "Min_SVL_cm", "Max_SVL_cm", "Min_mass_g", "Max_mass_g" )]
## Recodage de height_SVL_by_species$Mean_height_m en height_SVL_by_species$Mean_height_m_rec
height_SVL_by_species$Mean_height_m <- fct_recode(height_SVL_by_species$Mean_height_m,
                                                  NULL = "  NA (NA)")
## Recodage de height_SVL_by_species$Min_SVL_cm en height_SVL_by_species$Min_SVL_cm_rec
height_SVL_by_species$Min_SVL_cm <- fct_recode(height_SVL_by_species$Min_SVL_cm,
                                               NULL = "  Inf")
## Recodage de height_SVL_by_species$Max_SVL_cm en height_SVL_by_species$Max_SVL_cm_rec
height_SVL_by_species$Max_SVL_cm <- fct_recode(height_SVL_by_species$Max_SVL_cm,
                                               NULL = " -Inf")

## Recodage de height_SVL_by_species$Min_mass_g en height_SVL_by_species$Min_mass_g_rec
height_SVL_by_species$Min_mass_g <- fct_recode(height_SVL_by_species$Min_mass_g,
                                               NULL = "   Inf")
## Recodage de height_SVL_by_species$Max_mass_g en height_SVL_by_species$Max_mass_g_rec
height_SVL_by_species$Max_mass_g <- fct_recode(height_SVL_by_species$Max_mass_g,
                                               NULL = "  -Inf")

## order the data frame alphabetically by family
height_SVL_by_species <- height_SVL_by_species[order(height_SVL_by_species$binomial),]
height_SVL_by_species <- height_SVL_by_species[order(height_SVL_by_species$family),]

#view
View(height_SVL_by_species)

#export it

write.xlsx(height_SVL_by_species, file="D:/Documents/TRAVAIL/Kenya/DATA/Summary_height_svl_mass.xlsx", sheetName="Summary", row.names = FALSE)
# load it back
wb <- loadWorkbook("D:/Documents/TRAVAIL/Kenya/DATA/Summary_height_svl_mass.xlsx")
sheets <- getSheets(wb)
# autosize column widths
autoSizeColumn(sheets[[1]], colIndex=1:ncol(height_SVL_by_species))
saveWorkbook(wb,"D:/Documents/TRAVAIL/Kenya/DATA/Summary_height_svl_mass.xlsx")







#####################  GRAPH_EXPLORATION  ######



############ sample size description #####

### abundance by species ###
ggplot(data = animals_by_described_site) + geom_bar(mapping = aes( x = species_code)) + labs(title = "abundance by species", x = "Species code", y = "Number of individuals")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/abundance by species.png")

### number of trees surveyed ###
## Réordonnancement de sites$edge_category_m
sites$edge_category_m <- factor(sites$edge_category_m, levels=c("-10", "0", "30", "100", "250", "500"))
ggplot(data = filter(sites, extra_ground != "Y", category != "matrix")) + geom_bar(mapping = aes (x = edge_category_m)) + facet_grid(.~forest_type) + scale_y_continuous(breaks = seq(0, 9, by = 1)) + labs(title = "Number of sites with complete surveys (normal climber and ground) done, by forest type", x = "Distance form the edge (m)", y = "Number of sites with standard surveys done")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Sites with standard surveys done.png")






############reptiles_location_in_trees#########

####### distance from center

###boxplot_dist_from_center_geckos###
ggplot(data = filter(animals, genus == "Hemidactylus" | genus == "Lygodactylus")) + geom_boxplot(mapping = aes (x = genus, y = dist_frm_center_m), show.legend=FALSE) + facet_grid(.~day_night) + labs (title = "Geckos distance from center")

### distance_from_center_geckos_only_C_surveys_density_graph###
ggplot(data = filter(animals, genus == "Hemidactylus" | genus == "Lygodactylus", survey_type == "C")) + geom_density(mapping = aes (dist_frm_center_m, color = genus)) + facet_grid(.~day_night)+ labs (title = "Geckos distance from center in climber surveys only")

### distance_from_center_different_speices_only_C_surveys###
ggplot(data = filter(animals, binomial == "Chamaeleo_dilepis" | binomial =="Hemidactylus_mabouia"| binomial =="Hemidactylus_platycephalus" | binomial == "Lygodactylus_mombasicus", survey_type =="C")) + geom_boxplot(mapping = aes (x = species_code, y = dist_frm_center_m))+ geom_jitter(aes(x = species_code, y = dist_frm_center_m), color = "red", alpha = 0.2) + labs (title = "Distance from center by species for climber surveys only") + coord_polar(theta ="x", direction=1 )

### distance from center by species by size for climber surveys only###
ggplot(data = filter(animals, binomial == "Chamaeleo_dilepis" | binomial =="Hemidactylus_mabouia"| binomial == "Hemidactylus_platycephalus" | binomial == "Lygodactylus_sp." | binomial == "Hemidactylus_ sp." | binomial == "Lygodactylus_mombasicus", survey_type =="C" )) + geom_jitter(mapping = aes (x = SVL_cm, y = dist_frm_center_m)) + facet_grid(.~binomial) + geom_smooth(aes (x = SVL_cm, y = dist_frm_center_m), method = "lm") + labs (title = "Distance from center by species by size for climber surveys only")

### distace from center by size, lizards, all species together,climber surveys only### 
ggplot(data = filter(animals, survey_type =="C", SVL_cm < 25)) + geom_jitter(mapping = aes (x = SVL_cm, y = dist_frm_center_m)) + geom_smooth(aes (x = SVL_cm, y = dist_frm_center_m), method = "lm") + labs (title = "Distance from center by size, lizards, climber surveys only")

### distance from center by sex by species, climber surveys only###
ggplot(data = filter(animals, survey_type =="C", species_code != "NA")) + geom_boxplot(mapping = aes (x = sex, y = dist_frm_center_m)) + facet_grid(.~species_code) + labs (title = "Distance from center by sexe by species, climber surveys only")

### distance from center by mass by species, climber surveys only###
ggplot(data = filter(animals, survey_type =="C", species_code != "NA")) + geom_point(mapping = aes (x = mass_g, y = dist_frm_center_m)) + facet_grid(.~species_code) + geom_smooth(aes (x = mass_g, y = dist_frm_center_m), method = "lm") + labs (title = "Distance from center by mass by species, climber surveys only")

### distance from center by mass, all species, climber surveys only###
ggplot(data = filter(animals, survey_type =="C")) + geom_point(mapping = aes (x = mass_g, y = dist_frm_center_m)) + geom_smooth(aes (x = mass_g, y = dist_frm_center_m), method = "lm") + labs (title = "Distance from center by mass, all species, climber surveys only")

###Distance from center, HEPL, C surveys, day / night###
ggplot(data= filter(animals_by_described_site, survey_type == "C", height_found_percentage != "NA", species_code == "HEPL")) + geom_density(mapping = aes(x = dist_frm_center_m, color = day_night)) + labs(title = "Distance from center,HEPL, C surveys only", y = "Disrtibution of individuals", x = "Distance from center (m)")



######## height found

###boxplot_height_found_geckos_only_C_surveys###
ggplot(data = filter(animals, genus == "Hemidactylus" | genus == "Lygodactylus", survey_type == "C")) + geom_boxplot(mapping = aes (x = genus, y = height_found_m), show.legend=FALSE) + geom_jitter(aes(x = genus, y = height_found_m), color = "red", alpha = 0.2) + facet_grid(.~day_night)+ labs (title = "Geckos height found in climber surveys only")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Geckos height found in climber surveys only.png")

### density_height_found_geckos_only_C_surveys###
ggplot(data = filter(animals, genus == "Hemidactylus" | genus == "Lygodactylus", survey_type == "C")) + geom_density(mapping = aes (height_found_m, color = genus)) + facet_grid(.~day_night)+ labs (title = "Geckos height found in climber surveys only")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Geckos height found in climber surveys only, distribution.png")


###height_found_all_hemidactylus###
ggplot(data = filter(animals, genus == "Hemidactylus")) + geom_boxplot(mapping = aes (x = binomial, y = height_found_m), show.legend=FALSE) + geom_jitter(mapping = aes (x = binomial, y = height_found_m), color = "red", alpha = 0.2) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+ labs (title = "Height found for all Hemydactylus")

###height found by species, a few species###
ggplot(data = filter(animals, binomial == "Chamaeleo_dilepis" | binomial =="Hemidactylus_mabouia" | binomial =="Hemidactylus_mrimaensis" | binomial =="Hemidactylus_platycephalus" | binomial == "Lygodactylus_mombasicus")) + geom_boxplot(mapping = aes (x = binomial, y = height_found_m))+ geom_jitter(aes(x = binomial, y = height_found_m), color = "red", alpha = 0.2) + labs (title = "Height found by species")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Heoght found few species.png")


### height found by species, all species###
ggplot(data = animals) + geom_boxplot(mapping = aes (x = binomial, y = height_found_m))+ geom_jitter(aes(x = binomial, y = height_found_m), color = "red", alpha = 0.2) + labs (title = "Height found by species, all species")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Heoght found all species.png")


### height found related to SVL, all species, SVL <25 cm to avoid snakes###
ggplot(data = filter(animals_by_described_site, SVL_cm < 25)) + geom_jitter(mapping = aes(x = SVL_cm, y = height_found_m)) + labs(title = "Height found related to SVL, all species, SVL < 25cm") + geom_smooth(aes (x = SVL_cm, y = height_found_m), method = "lm")

### height found related to SVL, by species, SVL < 25 cm for more visibility###
ggplot(data = filter(animals_by_described_site, species_code != "NA", SVL_cm < 25)) + geom_jitter(mapping = aes(x = SVL_cm, y = height_found_m)) + labs(title = "Height found related to SVL, by species, SVL < 25 cm for more visibility") + facet_grid(.~species_code)

###height found related to SVL, Hemidactylus, all###
ggplot(data = filter(animals_by_described_site, genus == "Hemidactylus")) + geom_jitter(mapping = aes (x = SVL_cm, y = height_found_m))+ labs (title = "Height found related to SVL, Hemidactylus") + geom_smooth(aes (x = SVL_cm, y = height_found_m), method = "lm")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Heoght found vs svl for hemidactylus.png")


###height found related to SVL, Lydodactylus, all###
ggplot(data = filter(animals_by_described_site, genus == "Lygodactylus")) + geom_jitter(mapping = aes (x = SVL_cm, y = height_found_m))+ labs (title = "Height found related to SVL, Lygodactylus") + geom_smooth(aes (x = SVL_cm, y = height_found_m), method = "lm")

### height found by sexe by species###
ggplot(data = filter(animals, species_code != "NA")) + geom_boxplot(mapping = aes (x = sex, y = height_found_m)) + facet_grid(.~species_code) + labs (title = "Heigh found by sexe by species") + geom_jitter(mapping = aes (x = sex, y = height_found_m), color = "red", alpha = 0.2)
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Heoght found by sexe by species.png")


### height found by mass, all species, climber surveys only###
ggplot(data = filter(animals, survey_type =="C")) + geom_point(mapping = aes (x = mass_g, y = height_found_m)) + geom_smooth(aes (x = mass_g, y = height_found_m), method = "lm") + labs (title = "Height found by mass, all species, climber surveys only")

### height found by mass, by species, climber surveys only###
ggplot(data = filter(animals, survey_type =="C", species_code != "NA")) + geom_point(mapping = aes (x = mass_g, y = height_found_m)) + facet_grid(.~species_code) + geom_smooth(aes (x = mass_g, y = height_found_m), method = "lm") + labs (title = "height found by mass by species, climber surveys only")

### height found by mass by species, mass < 50 g for more visibility
ggplot(data = filter(animals, species_code != "NA", mass_g < 50)) + geom_point(mapping = aes (x = mass_g, y = height_found_m)) + facet_grid(.~species_code) + labs (title = "height found by mass by species, mass < 50 g") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

###height found related to mass, Hemidactulus, all###
ggplot(data = filter(animals_by_described_site, genus == "Hemidactylus")) + geom_jitter(mapping = aes (x = mass_g, y = height_found_m))+ labs (title = "Height found related to mass, Hemidactylus") + geom_smooth(aes (x = mass_g, y = height_found_m), method = "lm")

###height found related to mass, Hemidactulus, climber surveys only###
ggplot(data = filter(animals_by_described_site, genus == "Hemidactylus", survey_type == "C")) + geom_jitter(mapping = aes (x = mass_g, y = height_found_m))+ labs (title = "Height found related to mass, Hemidactylus, climber surveys only") + geom_smooth(aes (x = mass_g, y = height_found_m), method = "lm")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Heoght found by mass, hemidactylus, climber surveys.png")


###Height found related to mass, Lygodactylus, all###
ggplot(data = filter(animals_by_described_site, genus == "Lygodactylus")) + geom_jitter(mapping = aes (x = mass_g, y = height_found_m))+ labs (title = "Mass found related to SVL, Lygodactylus") + geom_smooth(aes (x = mass_g, y = height_found_m), method = "lm")





################ REPTILES AND EDGE CATEGORY ##########


###### abundance / edge category###

## Réordonnancement de animals_bio_index_by_described_site$edge_category_m
animals_bio_index_by_described_site$edge_category_m <- factor(animals_bio_index_by_described_site$edge_category_m, levels=c("-10", "0", "30", "100", "250", "500"))

### abundance by species by edge category by forest type, few species ###
ggplot(data = filter(animals_by_described_site, edge_category_m != "NA")) + geom_bar(mapping = aes( x = species_code)) + facet_grid(forest_type~edge_category_m) + labs(title = "abundance by species", x = "Species code", y = "Number of individuals") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/abundance by species.png")


### Number_of_HEPL_by_edge_category_by_forest_type###
Species_by_described_site$edge_category_m <- factor(Species_by_described_site$edge_category_m, levels=c("-10", "0", "30", "100", "250", "500"))
ggplot(data = Species_by_described_site) + geom_jitter(mapping = aes (x = edge_category_m, y = Hemidactylus_platycephalus, color = forest_type),show.legend=TRUE)+ labs (title = "Number of HEPL by edge category by forest type")

### Number_of_HEPL_by_edge_category_boxplot_by_forest_type###
ggplot(data = filter(Species_by_described_site, edge_category_m != "NA")) + geom_boxplot(mapping = aes(x = edge_category_m, y = Hemidactylus_platycephalus)) + facet_grid(.~forest_type) + geom_jitter(mapping = aes(x = edge_category_m, y = Hemidactylus_platycephalus), alpha = 0.2, width = 0.07, height= 0.07, color = "red") + labs(title = "Abundance of Hemidactylus platycephalus by edge category by forest type", x = "Distance from the edge (m)", y = "Number of HEPL") + scale_y_continuous(breaks=seq(0, 10, by = 1))
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Number of hepl by edge category by forest type.png")

### Number_of_HEPL_by_edge_category_bargraph###
ggplot(data = Species_by_described_site) + geom_bar(mapping = aes (x = edge_category_m, y = Hemidactylus_platycephalus, fill = forest_type),show.legend=TRUE, stat = "identity")+ labs (title = "Number of HEPL by edge category by forest type")

### Number_of_LYMO_by_edge_category_bargraph###
ggplot(data = Species_by_described_site) + geom_bar(mapping = aes (x = edge_category_m, y = Lygodactylus_mombasicus, fill = forest_type),show.legend=TRUE, stat = "identity")+ labs (title = "Number of LYMO by edge category by forest type")

### Number_of_LYMO_by_edge_category_boxplot_by_forest_type###
ggplot(data = filter(Species_by_described_site, edge_category_m != "NA")) + geom_boxplot(mapping = aes(x = edge_category_m, y = Lygodactylus_mombasicus)) + facet_grid(.~forest_type) + geom_jitter(mapping = aes(x = edge_category_m, y = Lygodactylus_mombasicus), alpha = 0.2, width = 0.07, height= 0.07, color = "red") + labs(title = "Abundance of Lygodactylus mombasicus by edge category by forest type", x = "Distance from the edge (m)", y = "Number of LYMO") + scale_y_continuous(breaks=seq(0, 10, by = 1))
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Number of LYMO by edge category by forest type.png")

### Number_of_NUBO_by_edge_category_boxplot_by_forest_type###
ggplot(data = filter(Species_by_described_site, edge_category_m != "NA")) + geom_boxplot(mapping = aes(x = edge_category_m, y = Nucras_boulengeri)) + facet_grid(.~forest_type) + geom_jitter(mapping = aes(x = edge_category_m, y = Nucras_boulengeri), alpha = 0.2, width = 0.07, height= 0.07, color = "red") + labs(title = "Abundance of Nucras_boulengeri by edge category by forest type", x = "Distance from the edge (m)", y = "Number of NUBO") + scale_y_continuous(breaks=seq(0, 20, by = 1))
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Number of NUBO by edge category by forest type.png")

### Number_of_HEMA_by_edge_category_boxplot_by_forest_type###
ggplot(data = filter(Species_by_described_site, edge_category_m != "NA")) + geom_boxplot(mapping = aes(x = edge_category_m, y = Hemidactylus_mabouia)) + facet_grid(.~forest_type) + geom_jitter(mapping = aes(x = edge_category_m, y = Hemidactylus_mabouia), alpha = 0.2, width = 0.07, height= 0.07, color = "red") + labs(title = "Abundance of Hemidactylus_mabouia by edge category by forest type", x = "Distance from the edge (m)", y = "Number of HEMA") + scale_y_continuous(breaks=seq(0, 10, by = 1))
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Number of HEMA by edge category by forest type.png")

### Number_of_CHDI_by_edge_category_boxplot_by_forest_type###
ggplot(data = filter(Species_by_described_site, edge_category_m != "NA")) + geom_boxplot(mapping = aes(x = edge_category_m, y = Chamaeleo_dilepis)) + facet_grid(.~forest_type) + geom_jitter(mapping = aes(x = edge_category_m, y = Chamaeleo_dilepis), alpha = 0.2, width = 0.07, height= 0.07, color = "red") + labs(title = "Abundance of Chamaeleo_dilepis by edge category by forest type", x = "Distance from the edge (m)", y = "Number of CHDI") + scale_y_continuous(breaks=seq(0, 10, by = 1))
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Number of CHDI by edge category by forest type.png")

### Number_of_HEMI_by_edge_category_boxplot_by_forest_type###
ggplot(data = filter(Species_by_described_site, edge_category_m != "NA")) + geom_boxplot(mapping = aes(x = edge_category_m, y = Hemidactylus_mrimaensis)) + facet_grid(.~forest_type) + geom_jitter(mapping = aes(x = edge_category_m, y = Hemidactylus_mrimaensis), alpha = 0.2, width = 0.07, height= 0.07, color = "red") + labs(title = "Abundance of Hemidactylus_mrimaensis by edge category by forest type", x = "Distance from the edge (m)", y = "Number of HEMI") + scale_y_continuous(breaks=seq(0, 10, by = 1))
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Number of HEMI by edge category by forest type.png")

### Number_of_TRPL_by_edge_category_boxplot_by_forest_type###
ggplot(data = filter(Species_by_described_site, edge_category_m != "NA")) + geom_boxplot(mapping = aes(x = edge_category_m, y = Trachylepis_planifrons)) + facet_grid(.~forest_type) + geom_jitter(mapping = aes(x = edge_category_m, y = Trachylepis_planifrons), alpha = 0.2, width = 0.07, height= 0.07, color = "red") + labs(title = "Abundance of Trachylepis_planifrons by edge category by forest type", x = "Distance from the edge (m)", y = "Number of TRPL") + scale_y_continuous(breaks=seq(0, 10, by = 1))
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Number of TRPL by edge category by forest type.png")



## Réordonnancement de animals_bio_index_by_described_site$edge_category_m
animals_bio_index_by_described_site$edge_category_m <- factor(animals_bio_index_by_described_site$edge_category_m, levels=c("-10", "0", "30", "100", "250", "500"))
###abundance related to edge category by forest type###
ggplot(data = animals_bio_index_by_described_site) + geom_boxplot(mapping = aes (x = edge_category_m, y = abund)) + geom_jitter(mapping = aes (x = edge_category_m, y = abund), color = "red", alpha = 0.2, width = 0.07, height= 0.07) + facet_grid(.~forest_type) + labs (title = "abundance related to edge category by forest type, all surveys", x = "Distance from the edge (m)", y = "Number of individuals")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Abundance related to edge category by forest type all surveys.png")


###abundance related to edge category by forest type, standard surveys only###
ggplot(data = filter(animals_bio_index_by_described_site, extra_ground != "Y", category != "matrix" )) + geom_boxplot(mapping = aes (x = edge_category_m, y = abund))  + facet_grid(.~forest_type)  + geom_jitter(mapping = aes (x = edge_category_m, y = abund), color = "red", alpha = 0.2, width = 0.07, height= 0.07) + labs (title = "abundance related to edge category by forest type, standard surveys only", x = "Distance form the edge", y = "Number of individuals found") + scale_y_continuous(breaks = seq(0, 15, by = 1))
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Abundance related to edge category by forest type standard surveys only.png")



###### SVL / edge category

### SVL by edge category, all species###
animals_by_described_site$edge_category_m <- factor(animals_by_described_site$edge_category_m, levels=c("-10", "0", "30", "100", "250", "500"))
ggplot(data = filter(animals_by_described_site, edge_category_m != "NA")) + geom_point(mapping = aes (x = edge_category_m_numeric, y = SVL_cm)) + geom_smooth(aes (x = edge_category_m_numeric, y = SVL_cm), method = "lm") + labs (title = "SVL by edge category, all species")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/SVL by edge category.png")

### SVL by edge category, species < 10 cm###
ggplot(data = filter(animals_by_described_site, edge_category_m != "NA", SVL_cm < 10 )) + geom_point(mapping = aes (x = edge_category_m_numeric, y = SVL_cm)) + geom_smooth(aes (x = edge_category_m_numeric, y = SVL_cm), method = "lm") + labs (title = "SVL by edge category, individuals < 10 cm")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/SVL by edge category for svl inferior 10 cm.png")



### SVL by edge category, genus Hemidactylus###
ggplot(data = filter(animals_by_described_site, edge_category_m != "NA", genus == "Hemidactylus")) + geom_point(mapping = aes (x = edge_category_m_numeric, y = SVL_cm)) + labs (title = "SVL by edge category, genus Hemidactylus") + geom_smooth(mapping = aes (x = edge_category_m_numeric, y = SVL_cm), method = "lm") + labs (title = "SVL by edge category, genus Hemidactylus")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/SVL by edge category hemidactylus.png")

### SVL by edge category, Hemidactylus platycephalus###
ggplot(data = filter(animals_by_described_site, edge_category_m != "NA", species_code == "HEPL")) + geom_point(mapping = aes (x = edge_category_m_numeric, y = SVL_cm)) + labs (title = "SVL by edge category, HEPL") + geom_smooth(mapping = aes (x = edge_category_m_numeric, y = SVL_cm), method = "lm") + labs (title = "SVL by edge category, HEPL")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/SVL by edge category hemidactylus platycephalus.png")


### SVL by edge category, genus Lygodactylus###
ggplot(data = filter(animals_by_described_site, edge_category_m != "NA", genus == "Lygodactylus")) + geom_point(mapping = aes (x = edge_category_m_numeric, y = SVL_cm)) + labs (title = "SVL by edge category, genus Lygodactylus") + geom_smooth(mapping = aes (x = edge_category_m_numeric, y = SVL_cm), method = "lm") + labs (title = "SVL by edge category, genus Lygodactylus")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/SVL by edge category Lygodactylus.png")



### SVL by edge category, Lygodactylus mombasicus###
ggplot(data = filter(animals_by_described_site, edge_category_m != "NA", species_code == "LYMO")) + geom_point(mapping = aes (x = edge_category_m_numeric, y = SVL_cm)) + labs (title = "SVL by edge category, Lygodactylus mombasicus") + geom_smooth(mapping = aes (x = edge_category_m_numeric, y = SVL_cm), method = "lm") + labs (title = "SVL by edge category, LYMO")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/SVL by edge category Lygodactylus mombasicus.png")

### SVL by edge category, Chamaeleo dilepis###
ggplot(data = filter(animals_by_described_site, edge_category_m != "NA", species_code == "CHDI")) + geom_point(mapping = aes (x = edge_category_m_numeric, y = SVL_cm)) + labs (title = "SVL by edge category, Chamaeleo dilepis") + geom_smooth(mapping = aes (x = edge_category_m_numeric, y = SVL_cm), method = "lm") + labs (title = "SVL by edge category, CHDI")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/SVL by edge category Chamaeleo dilepis.png")


### SVL by edge category, Hemidactylus mabouia###
ggplot(data = filter(animals_by_described_site, edge_category_m != "NA", species_code == "HEMA")) + geom_point(mapping = aes (x = edge_category_m_numeric, y = SVL_cm)) + labs (title = "SVL by edge category, Hemidactylus mabouia") + geom_smooth(mapping = aes (x = edge_category_m_numeric, y = SVL_cm), method = "lm") + labs (title = "SVL by edge category, HEMA")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/SVL by edge category Hemidactylus mabouia.png")


### SVL by edge category, Hemidactylus mrimaensis###
ggplot(data = filter(animals_by_described_site, edge_category_m != "NA", species_code == "HEMI")) + geom_point(mapping = aes (x = edge_category_m_numeric, y = SVL_cm)) + labs (title = "SVL by edge category, Hemidactylus mrimaensis") + geom_smooth(mapping = aes (x = edge_category_m_numeric, y = SVL_cm), method = "lm") + labs (title = "SVL by edge category, HEMI")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/SVL by edge category Hemidactylus mrimaensis.png")


######## MAss / edge category

### mass  by edge category, all species###
ggplot(data = filter(animals_by_described_site, edge_category_m != "NA")) + geom_point(mapping = aes (x = edge_category_m_numeric, y = mass_g)) + geom_smooth(aes (x = edge_category_m_numeric, y = mass_g), method = "lm") + labs (title = "mass by edge category, all species")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/mass by edge category.png")

### mass by edge category, species < 100 g###
ggplot(data = filter(animals_by_described_site, edge_category_m != "NA", mass_g < 100)) + geom_point(mapping = aes (x = edge_category_m_numeric, y = mass_g)) + geom_smooth(aes (x = edge_category_m_numeric, y = mass_g), method = "lm") + labs (title = "mass by edge category, species < 100 g")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/mass by edge category, species inferior 100 g.png")



####### SEX / edge category

### Sex by edge category, Lygodactylus###
ggplot(data = filter(animals_by_described_site, genus == "Lygodactylus")) + geom_bar(mapping = aes(x = edge_category_m, fill = sex), position = "dodge") + labs(title = "Number of individuals of each sex by edge category, Lygodactylus", x = "edge category (m)", y = "Number of individuals")

###Sex by edge category, LYMO###
ggplot(data = filter(animals_by_described_site, species_code == "LYMO")) + geom_bar(mapping = aes(x = edge_category_m, fill = sex), position = "dodge") + labs(title = "Number of individuals of each sex by edge category, Lygodactylus mombasicus", x = "edge category (m)", y = "Number of individuals")

###Sex by edge caegory, Hemidactylus###
ggplot(data = filter(animals_by_described_site, genus == "Hemidactylus")) + geom_bar(mapping = aes(x = edge_category_m, fill = sex), position = "dodge") + labs(title = "Number of individuals of each sex by edge category, Hemidactylus", x = "edge category (m)", y = "Number of individuals")

###Sex by edge category, HEPL###
ggplot(data = filter(animals_by_described_site, species_code == "HEPL")) + geom_bar(mapping = aes(x = edge_category_m, fill = sex), position = "dodge") + labs(title = "Number of individuals of each sex by edge category, Hemidactylus platycephalus", x = "edge category (m)", y = "Number of individuals")

###Sex by edge category, HEMA###
ggplot(data = filter(animals_by_described_site, species_code == "HEMA")) + geom_bar(mapping = aes(x = edge_category_m, fill = sex), position = "dodge") + labs(title = "Number of individuals of each sex by edge category, Hemidactylus mabouia", x = "edge category (m)", y = "Number of individuals")




############ Height / edge category


###height found by edge category by forest type, all species###
ggplot(data= filter(animals_by_described_site, edge_category_m != "NA")) + geom_point(mapping = aes(x = edge_category_m_numeric, y = height_found_m)) + geom_smooth(aes(x = edge_category_m_numeric, y = height_found_m), method = "lm") + facet_grid(.~forest_type) + labs(title = "Height found by edge category by forest type, all species")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Height found by edge category by forest type all species.png")


###height found by edge category by forest type, all species, standard surveys only###
ggplot(data= filter(animals_by_described_site, edge_category_m != "NA", extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = edge_category_m_numeric, y = height_found_m)) + geom_smooth(aes(x = edge_category_m_numeric, y = height_found_m), method ="lm") + facet_grid(.~forest_type) + labs(title = "Height found by edge category by forest type, all species, standard surveys only")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Height found by edge category by forest type all species standard surveys only.png")


###height found by edge category by forest type, all species, day, night, standard surveys only###
ggplot(data= filter(animals_by_described_site, edge_category_m != "NA", extra_ground != "Y", category != "matrix")) + geom_point(mapping = aes(x = edge_category_m_numeric, y = height_found_m)) + geom_smooth(aes(x = edge_category_m_numeric, y = height_found_m), method ="lm") + facet_grid(day_night~forest_type) + labs(title = "Height found by edge category by forest type, all species, standard surveys only")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Height found by edge category by forest type all species standard surveys only day night.png")




###height found by edge category by forest type, Hemidactylus and Lygodactylus###
ggplot(data= filter(animals_by_described_site, genus == "Hemidactylus" | genus == "Lygodactylus", edge_category_m != "NA")) + geom_boxplot(mapping = aes(x = edge_category_m, y = height_found_m, fill = day_night)) + geom_jitter(aes(x = edge_category_m, y = height_found_m), color = "red", alpha = 0.2) + facet_grid(.~forest_type) + labs(title = "Height found by edge category by forest type, Hemidactylus and Lygodactylus")

###height found by edge category by forest type, Hemidactylus###
ggplot(data= filter(animals_by_described_site, genus == "Hemidactylus", edge_category_m != "NA")) + geom_boxplot(mapping = aes(x = edge_category_m, y = height_found_m, fill = day_night)) + geom_jitter(aes(x = edge_category_m, y = height_found_m), color = "red", alpha = 0.2) + facet_grid(.~forest_type) + labs(title = "Height found by edge category by forest type, genus Hemidactylus")

###height found by edge category, Hemidactylus###
ggplot(data= filter(animals_by_described_site, survey_type != "NA", height_found_percentage != "NA", edge_category_m != "NA", genus == "Hemidactylus")) + geom_bar(mapping = aes(x = height_found_percentage_breaks)) + facet_grid(.~edge_category_m) + labs(title = "Height location of individuals by edge category, Hemidactylus, G & C surveys only", y = "Number of individuals", x = "Height in % of HOT") + coord_flip()
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Height found by edge category hemidactylus.png")


###height found by edge category by forest type, HEPL###
ggplot(data= filter(animals_by_described_site, species_code == "HEPL", edge_category_m != "NA")) + geom_boxplot(mapping = aes(x = edge_category_m, y = height_found_m, fill = day_night)) + geom_jitter(aes(x = edge_category_m, y = height_found_m), color = "red", alpha = 0.2) + facet_grid(.~forest_type) + labs(title = "Height found by edge category by forest type, Hemidactylus platycephalus")

###height found by edge category by forest type, HEMA###
ggplot(data= filter(animals_by_described_site, species_code == "HEMA", edge_category_m != "NA")) + geom_boxplot(mapping = aes(x = edge_category_m, y = height_found_m, fill = day_night)) + geom_jitter(aes(x = edge_category_m, y = height_found_m), color = "red", alpha = 0.2) + facet_grid(.~forest_type) + labs(title = "Height found by edge category by forest type, Hemidactylus mabouia")

###height found by edge category by forest type, Lygodactylus###
ggplot(data= filter(animals_by_described_site, genus == "Lygodactylus", edge_category_m != "NA")) + geom_boxplot(mapping = aes(x = edge_category_m, y = height_found_m, fill = day_night)) + geom_jitter(aes(x = edge_category_m, y = height_found_m), color = "red", alpha = 0.2) + facet_grid(.~forest_type) + labs(title = "Height found by edge category by forest type, genus Lygodactylus")

###height found by edge category by forest type, LYMO###
ggplot(data= filter(animals_by_described_site, species_code == "LYMO", edge_category_m != "NA")) + geom_boxplot(mapping = aes(x = edge_category_m, y = height_found_m, fill = day_night)) + geom_jitter(aes(x = edge_category_m, y = height_found_m), color = "red", alpha = 0.2) + facet_grid(.~forest_type) + labs(title = "Height found by edge category by forest type, Lygodactylus mombasicus")

###height found by edge category by forest type, CHDI###
ggplot(data= filter(animals_by_described_site, species_code == "CHDI", edge_category_m != "NA")) + geom_boxplot(mapping = aes(x = edge_category_m, y = height_found_m, fill = day_night)) + geom_jitter(aes(x = edge_category_m, y = height_found_m), color = "red", alpha = 0.2) + facet_grid(.~forest_type) + labs(title = "Height found by edge category by forest type, Chamaeleo dilepis")




###### Distance from center / edge category

### Distance from center by edge category, all species, climber surveys only###
ggplot(data = filter(animals_by_described_site, survey_type == "C")) + geom_boxplot(mapping = aes (x = edge_category_m, y = dist_frm_center_m)) + geom_jitter(aes(x = edge_category_m, y = dist_frm_center_m), color = "red", alpha = 0.2) + labs (title = "Distance from center by edge categroy, all species, climber surveys only")

### Distance from center by edge category, all species, climber surveys only, day, night###
ggplot(data = filter(animals_by_described_site, survey_type == "C")) + geom_boxplot(mapping = aes (x = edge_category_m, y = dist_frm_center_m, fill = day_night)) + geom_jitter(aes(x = edge_category_m, y = dist_frm_center_m), color = "red", alpha = 0.2) + labs (title = "Distance from center by edge categroy, all species, climber surveys only, day, night")

### Distance from center by edge category by forest type, all species, climber surveys only, day, night###
ggplot(data = filter(animals_by_described_site, survey_type == "C")) + geom_boxplot(mapping = aes (x = edge_category_m, y = dist_frm_center_m, fill = day_night)) + geom_jitter(aes(x = edge_category_m, y = dist_frm_center_m), color = "red", alpha = 0.2) + facet_grid(.~forest_type) + labs (title = "Distance from center by edge categroy by forest type, all species, climber surveys only, day, night")


### Distance from center by edge category by forest type, Lygodactylus, climber surveys only###
ggplot(data = filter(animals_by_described_site, survey_type == "C", genus == "Lygodactylus")) + geom_boxplot(mapping = aes (x = edge_category_m, y = dist_frm_center_m)) + geom_jitter(aes(x = edge_category_m, y = dist_frm_center_m), color = "red", alpha = 0.2) + facet_grid(.~forest_type) + labs (title = "Distance from center by edge categroy, Lygodactylus, climber surveys only")

### Distance from center by edge category by forest type, LYMO, climber surveys only###
ggplot(data = filter(animals_by_described_site, survey_type == "C", species_code == "LYMO")) + geom_boxplot(mapping = aes (x = edge_category_m, y = dist_frm_center_m)) + geom_jitter(aes(x = edge_category_m, y = dist_frm_center_m), color = "red", alpha = 0.2) + facet_grid(.~forest_type) + labs (title = "Distance from center by edge categroy, LYMO, climber surveys only")

### Distance from center by edge category by forest type, Hemidactylus, climber surveys only###
ggplot(data = filter(animals_by_described_site, survey_type == "C", genus == "Hemidactylus")) + geom_boxplot(mapping = aes (x = edge_category_m, y = dist_frm_center_m)) + geom_jitter(aes(x = edge_category_m, y = dist_frm_center_m), color = "red", alpha = 0.2) + facet_grid(.~forest_type) + labs (title = "Distance from center by edge categroy, Hemidactylus, climber surveys only")

### Distance from center by edge category by forest type, HEPL, climber surveys only###
ggplot(data = filter(animals_by_described_site, survey_type == "C", species_code == "HEPL")) + geom_boxplot(mapping = aes (x = edge_category_m, y = dist_frm_center_m)) + geom_jitter(aes(x = edge_category_m, y = dist_frm_center_m), color = "red", alpha = 0.2) + facet_grid(.~forest_type) + labs (title = "Distance from center by edge categroy, HEPL, climber surveys only")



###### diversity / edge category

###diversity shannon related to edge category by forest type###
animals_bio_index_by_described_site$edge_category_m <- factor(animals_bio_index_by_described_site$edge_category_m, levels=c("-10", "0", "30", "100", "250", "500"))
ggplot(data = filter(animals_bio_index_by_described_site, edge_category_m != "NA")) + geom_boxplot(mapping = aes (x = edge_category_m, y = diversity_shannon), show.legend=FALSE) + geom_jitter(mapping = aes (x = edge_category_m, y = diversity_shannon), color = "red", alpha = 0.2) + facet_grid(.~forest_type) + labs (title = "diversity shannon related to edge category by forest type")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Shannon diversity by edge category by forest type.png")


###diversity simpson related to edge category by forest type###
ggplot(data = animals_bio_index_by_described_site) + geom_boxplot(mapping = aes (x = edge_category_m, y = diversity_simpson), show.legend=FALSE) + geom_jitter(mapping = aes (x = edge_category_m, y = diversity_simpson), color = "red", alpha = 0.2) + facet_grid(.~forest_type) + labs (title = "diversity simpson related to edge category by forest type")


###richness related to edge category by forest type###
ggplot(data = animals_bio_index_by_described_site) + geom_boxplot(mapping = aes (x = edge_category_m, y = rich)) + geom_jitter(mapping = aes (x = edge_category_m, y = rich), color = "red", alpha = 0.2) + facet_grid(.~forest_type) + labs (title = "richness related to edge category by forest type")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Richness related to edge category by forest type.png")





################ reptiles and forest_type ###########

###### abundance / forest type

###Number of each species by site, by forest tye###
ggplot(data = filter(animals_by_described_site, forest_type != "CY")) + geom_bar(mapping = aes (x = binomial)) + facet_grid(.~forest_type) + labs (title = "Number of each species by forest type", x = "Species", y = "Number of individuals") + theme(axis.text.x = element_text(angle = 65, hjust = 1))

###Number of HEPL by site, by forest type###
ggplot(data = Species_by_described_site) + geom_boxplot(mapping = aes (x = forest_type, y = Hemidactylus_platycephalus)) + geom_jitter(mapping = aes (x = forest_type, y = Hemidactylus_platycephalus, color = "red", alpha = 0.2), show.legend = FALSE) + labs (title = "Number of HEPL by forest type", x = "Species", y = "Number of individuals")

###Number of HEMA by site, by forest type###
ggplot(data = Species_by_described_site) + geom_boxplot(mapping = aes (x = forest_type, y = Hemidactylus_mabouia)) + geom_jitter(mapping = aes (x = forest_type, y = Hemidactylus_mabouia, color = "red", alpha = 0.2), show.legend = FALSE) + labs (title = "Number of HEMA by forest type", x = "Species", y = "Number of individuals") 

###Number of HEMR by site, by forest type###
ggplot(data = Species_by_described_site) + geom_boxplot(mapping = aes (x = forest_type, y = Hemidactylus_mrimaensis)) + geom_jitter(mapping = aes (x = forest_type, y = Hemidactylus_mrimaensis, color = "red", alpha = 0.2), show.legend = FALSE) + labs (title = "Number of HEMR by forest type", x = "Species", y = "Number of individuals") 

###Number of LYMO by site, by forest type###
ggplot(data = Species_by_described_site) + geom_boxplot(mapping = aes (x = forest_type, y = Lygodactylus_mombasicus)) + geom_jitter(mapping = aes (x = forest_type, y = Lygodactylus_mombasicus, color = "red", alpha = 0.2), show.legend = FALSE) + labs (title = "Number of LYMO by forest type", x = "Species", y = "Number of individuals") 

###Number of CHDI by site, by forest type###
ggplot(data = Species_by_described_site) + geom_boxplot(mapping = aes (x = forest_type, y = Chamaeleo_dilepis)) + geom_jitter(mapping = aes (x = forest_type, y = Chamaeleo_dilepis, color = "red", alpha = 0.2), show.legend = FALSE) + labs (title = "Number of CHDI by forest type", x = "Species", y = "Number of individuals")


#### Sex / forest type

###Number of each sex by forest type, Lygodactylus###
ggplot(data = filter(animals_by_described_site, genus == "Lygodactylus")) + geom_bar(mapping = aes(x = forest_type, fill = sex), position = "dodge") + labs(title = "Number of individuals of each sex by forest type, Lygodactylus", x = "forest type", y = "Number of individuals")

###Number of each sex by forest type, LYMO###
ggplot(data = filter(animals_by_described_site, species_code == "LYMO")) + geom_bar(mapping = aes(x = forest_type, fill = sex), position = "dodge") + labs(title = "Number of individuals of each sex by forest type, LYMO", x = "forest type", y = "Number of individuals")

###Number of each sex by forest type, Hemdidactylus###
ggplot(data = filter(animals_by_described_site, genus == "Hemidactylus")) + geom_bar(mapping = aes(x = forest_type, fill = sex), position = "dodge") + labs(title = "Number of individuals of each sex by forest type, Hemidactylus", x = "forest type", y = "Number of individuals")

###Number of each sex by forest type, HEPL###
ggplot(data = filter(animals_by_described_site, species_code == "HEPL")) + geom_bar(mapping = aes(x = forest_type, fill = sex), position = "dodge") + labs(title = "Number of individuals of each sex by forest type, HEPL", x = "forest type", y = "Number of individuals")

###Number of each sex by forest type, HEMA###
ggplot(data = filter(animals_by_described_site, species_code == "HEMA")) + geom_bar(mapping = aes(x = forest_type, fill = sex), position = "dodge") + labs(title = "Number of individuals of each sex by forest type, HEMA", x = "forest type", y = "Number of individuals")

###Number of each sex by forest type, HEMR###
ggplot(data = filter(animals_by_described_site, species_code == "HEMI")) + geom_bar(mapping = aes(x = forest_type, fill = sex), position = "dodge") + labs(title = "Number of individuals of each sex by forest type, HEMR", x = "forest type", y = "Number of individuals")


#### SVL / forest type

###SVL by forest type, all species, SVL <25 cm###
ggplot(data = filter(animals_by_described_site)) + geom_boxplot(mapping = aes (x = forest_type, y = SVL_cm)) + geom_jitter(aes (x = forest_type, y = SVL_cm), color = "red", alpha = 0.2) + labs (title = "SVL by forest type, all species, SVL < 25") + coord_cartesian(ylim = c(0, 25))

###SVL by forest type, Hemidactylus###
ggplot(data = filter(animals_by_described_site, genus == "Hemidactylus")) + geom_boxplot(mapping = aes (x = forest_type, y = SVL_cm)) + geom_jitter(aes (x = forest_type, y = SVL_cm), color = "red", alpha = 0.2) + labs (title = "SVL by forest_type, genus Hemidactylus", x = "Forest type", y = "SVL (cm)")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/SVL by forest type hemidactylus.png")


###SVL by forest type, HEPL###
ggplot(data = filter(animals_by_described_site, species_code == "HEPL")) + geom_boxplot(mapping = aes (x = forest_type, y = SVL_cm)) + geom_jitter(aes (x = forest_type, y = SVL_cm), color = "red", alpha = 0.2) + labs (title = "SVL by forest_type, HEPL", x = "Forest type", y = "SVL (cm)")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/SVL by forest type hemidactylus platycephalus.png")


###SVL by forest type, HEMA###
ggplot(data = filter(animals_by_described_site, species_code == "HEMA")) + geom_boxplot(mapping = aes (x = forest_type, y = SVL_cm)) + geom_jitter(aes (x = forest_type, y = SVL_cm), color = "red", alpha = 0.2) + labs (title = "SVL by forest_type, HEMA", x = "Forest type", y = "SVL (cm)")

###SVL by forest type, HEMR###
ggplot(data = filter(animals_by_described_site, species_code == "HEMI")) + geom_boxplot(mapping = aes (x = forest_type, y = SVL_cm)) + geom_jitter(aes (x = forest_type, y = SVL_cm), color = "red", alpha = 0.2) + labs (title = "SVL by forest_type, HEMR", x = "Forest type", y = "SVL (cm)")

###SVL by forest type, Lygodactylus###
ggplot(data = filter(animals_by_described_site, genus == "Lygodactylus")) + geom_boxplot(mapping = aes (x = forest_type, y = SVL_cm)) + geom_jitter(aes (x = forest_type, y = SVL_cm), color = "red", alpha = 0.2) + labs (title = "SVL by forest_type, genus Lygodactylus", x = "Forest type", y = "SVL (cm)")

###SVL by forest type, LYMO###
ggplot(data = filter(animals_by_described_site, species_code == "LYMO")) + geom_boxplot(mapping = aes (x = forest_type, y = SVL_cm)) + geom_jitter(aes (x = forest_type, y = SVL_cm), color = "red", alpha = 0.2) + labs (title = "SVL by forest_type, LYMO", x = "Forest type", y = "SVL (cm)")



#### height / forest type

###Height found by forest type, in m, all species###
ggplot(data= filter(animals_by_described_site)) + geom_boxplot(mapping = aes(x = forest_type, y = height_found_m)) + geom_jitter(aes(x = forest_type, y = height_found_m), color = "red", alpha = 0.2) + labs(title = "Height found by forest type, all species")

###Height found by forest type, in % of HOT, all species###
ggplot(data= filter(animals_by_described_site)) + geom_boxplot(mapping = aes(x = forest_type, y = height_found_percentage)) + geom_jitter(aes(x = forest_type, y = height_found_percentage), color = "red", alpha = 0.2) + labs(title = "Height found by forest type, all species")

###Height found by forest type, in % of HOT, density, all species###
ggplot(data= filter(animals_by_described_site)) + geom_density(mapping = aes(x = height_found_percentage)) + facet_grid(.~forest_type) + labs(title = "Height found by forest type, all species", y = "Distribution of individuals", x = "Height found in % of HOT") + coord_flip()

###Height location of individuals by forest type, all species, G and C surveys only###
ggplot(data= filter(animals_by_described_site, survey_type != "NA", height_found_percentage != "NA")) + geom_bar(mapping = aes(x = height_found_percentage_breaks)) + facet_grid(.~forest_type) + labs(title = "Height location of individuals by forest type, all species, G & C surveys only", y = "Number of individuals", x = "Height in % of HOT") + coord_flip()

###Height location of individuals by forest type, all species, G and C surveys only###
ggplot(data= filter(animals_by_described_site, height_found_percentage != "NA", survey_type != "NA")) + geom_density(mapping = aes(x = height_found_percentage)) + facet_grid(.~forest_type) + labs(title = "Height found by forest type, all species, G & C surveys only", y = "Density", x = "Height found in % of HOT") + coord_flip()

###Height location of individuals by forest type, Hemidactylus and Lygodactylus, G and C surveys only###
ggplot(data= filter(animals_by_described_site, survey_type != "NA", height_found_percentage != "NA", genus == "Hemidactylus" | genus == "Lygodactylus")) + geom_bar(mapping = aes(x = height_found_percentage_breaks, fill = genus), position = "dodge") + facet_grid(.~forest_type) + labs(title = "Height location of individuals by forest type, Hemidactylus and Lygodactylus, G & C surveys only", y = "Number of individuals", x = "Height in % of HOT") + coord_flip()

###Height location of individuals by forest type, Hemidactylus and Lygodactylus, G and C surveys only###
ggplot(data= filter(animals_by_described_site, genus == "Hemidactylus" | genus == "Lygodactylus", survey_type != "NA")) + geom_density(mapping = aes(x = height_found_percentage, color = genus)) + facet_grid(.~forest_type) + labs(title = "Height found by forest type, Hemidactylus and Lygodactylus, G and C surveys only", y = "Distribution of individuals", x = "Height found in % of HOT") + coord_flip()

###Height location of individuals by forest type, Hemidactylus species, G and C surveys only###
ggplot(data= filter(animals_by_described_site, survey_type != "NA", height_found_percentage != "NA", genus == "Hemidactylus", binomial != "Hemidactylus_ sp.")) + geom_bar(mapping = aes(x = height_found_percentage_breaks, fill = binomial), position = "dodge") + facet_grid(.~forest_type) + labs(title = "Height location of individuals by forest type, Hemidactylus, G & C surveys only", y = "Number of individuals", x = "Height in % of HOT") + coord_flip()

###Height location of individuals by forest type, CHDI, G and C surveys only###
ggplot(data= filter(animals_by_described_site, survey_type != "NA", height_found_percentage != "NA", species_code == "CHDI")) + geom_bar(mapping = aes(x = height_found_percentage_breaks)) + facet_grid(.~forest_type) + labs(title = "Height location of individuals by forest type, Chamaeleo dilepis, G & C surveys only", y = "Number of individuals", x = "Height in % of HOT") + coord_flip()



#### Distance from center / forest type

###Distance from center by forest type, all species###
ggplot(data= filter(animals_by_described_site, survey_type == "C", height_found_percentage != "NA")) + geom_density(mapping = aes(x = dist_frm_center_m)) + facet_grid(.~forest_type) + labs(title = "Distance from center by forest type,all species, C surveys only", y = "Disrtibution of individuals", x = "Distance from center (m)")

###Distance from center by forest type, all species, day / night###
ggplot(data= filter(animals_by_described_site, survey_type == "C", height_found_percentage != "NA")) + geom_density(mapping = aes(x = dist_frm_center_m, color = day_night)) + facet_grid(.~forest_type) + labs(title = "Distance from center by forest type,all species, C surveys only", y = "Disrtibution of individuals", x = "Distance from center (m)")

###Distance from center by forest type, Hemidactylus and Lygodactylus###
ggplot(data= filter(animals_by_described_site, survey_type == "C", height_found_percentage != "NA", genus == "Hemidactylus" | genus == "Lygodactylus")) + geom_density(mapping = aes(x = dist_frm_center_m, color = genus)) + facet_grid(.~forest_type) + labs(title = "Distance from center by forest type,Hemidactylus and Lygodactylus, C surveys only", y = "Disrtibution of individuals", x = "Distance from center (m)")

###Distance from center by forest type, Hemidactylus, day / night###
ggplot(data= filter(animals_by_described_site, survey_type == "C", height_found_percentage != "NA", genus == "Hemidactylus")) + geom_density(mapping = aes(x = dist_frm_center_m, color = day_night)) + facet_grid(.~forest_type) + labs(title = "Distance from center by forest type,Hemidactylus, C surveys only", y = "Disrtibution of individuals", x = "Distance from center (m)")

###Distance from center by forest type, HEPL###
ggplot(data= filter(animals_by_described_site, survey_type == "C", height_found_percentage != "NA", species_code == "HEPL")) + geom_density(mapping = aes(x = dist_frm_center_m)) + facet_grid(.~forest_type) + labs(title = "Distance from center by forest type,HEPL, C surveys only", y = "Disrtibution of individuals", x = "Distance from center (m)")



#### Diversity / forest type

###Shannon diversity by forest type###
ggplot(data = animals_bio_index_by_described_site) + geom_boxplot(mapping = aes (x = forest_type, y = diversity_shannon), show.legend=FALSE) + geom_jitter(mapping = aes (x = forest_type, y = diversity_shannon), color = "red", alpha = 0.2) + labs (title = "diversity shannon by forest type")

###Simpson diversity by forest type###
ggplot(data = animals_bio_index_by_described_site) + geom_boxplot(mapping = aes (x = forest_type, y = diversity_simpson)) + geom_jitter(mapping = aes (x = forest_type, y = diversity_simpson), color = "red", alpha = 0.2) + labs (title = "diversity simpson by forest type")

###abundance by forest type###
ggplot(data = animals_bio_index_by_described_site) + geom_boxplot(mapping = aes (x = forest_type, y = abund)) + geom_jitter(mapping = aes (x = forest_type, y = abund), color = "red", alpha = 0.2) + labs (title = "abundance by forest type")

###richness by forest type###
ggplot(data = animals_bio_index_by_described_site) + geom_boxplot(mapping = aes (x = forest_type, y = rich)) + geom_jitter(mapping = aes (x = forest_type, y = rich), color = "red", alpha = 0.2) + labs (title = "richness by forest type")





############reptiles_and_forest_characteristics#######


#### abundance / forest characteristics

###Number_of_HEPL_related_to_number_total_of_stems###
ggplot(data = Species_by_described_site) + geom_jitter(mapping = aes (x = stem_total, y = Hemidactylus_platycephalus),show.legend=TRUE)+ geom_smooth(aes (x = stem_total, y = Hemidactylus_platycephalus), method = "auto") + labs (title = "Number of HEPL related to number of stems")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Number of hepl by number of stems.png")


###Number_of_LYMO_related_to_number_total_of_stems###
ggplot(data = Species_by_described_site) + geom_jitter(mapping = aes (x = stem_total, y = Lygodactylus_mombasicus),show.legend=TRUE)+ geom_smooth(aes (x = stem_total, y = Lygodactylus_mombasicus), method = "auto") + labs (title = "Number of LYMO related to number of stems")

###Number_of_HEPL_related_to_number_total_of_stems_by_forest###
ggplot(data = Species_by_described_site) + geom_jitter(mapping = aes (x = stem_total, y = Hemidactylus_platycephalus),show.legend=TRUE)+ geom_smooth(aes (x = stem_total, y = Hemidactylus_platycephalus), method = "lm")+ facet_grid(.~forest_type) + labs (title = "Number of HEPL related to number of stems by forest type")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Number of hepl by number of stems by forest type.png")


###Number_of_LYMO_related_to_number_total_of_stems_by_forest###
ggplot(data = Species_by_described_site) + geom_jitter(mapping = aes (x = stem_total, y = Lygodactylus_mombasicus),show.legend=TRUE)+ geom_smooth(aes (x = stem_total, y = Lygodactylus_mombasicus), method = "auto")+ facet_grid(.~forest_type) + labs (title = "Number of LYMO related to number of stems by forest type")

###Number of HEPL related to wedge_prism###
ggplot(data = Species_by_described_site) + geom_jitter(mapping = aes (x = wedge_prism, y = Hemidactylus_platycephalus),show.legend=TRUE)+ geom_smooth(aes (x = wedge_prism, y = Hemidactylus_platycephalus), method = "auto") + labs (title = "Number of HEPL related to wedge_prism")

###Number of HEPL related to wedge_prism by forest type###
ggplot(data = Species_by_described_site) + geom_jitter(mapping = aes (x = wedge_prism, y = Hemidactylus_platycephalus),show.legend=TRUE)+ geom_smooth(aes (x = wedge_prism, y = Hemidactylus_platycephalus), method = "auto") + facet_grid(.~forest_type) + labs (title = "Number of HEPL related to wedge_prism by forest type")

###Number of LYMO related to wedge_prism###
ggplot(data = Species_by_described_site) + geom_jitter(mapping = aes (x = wedge_prism, y = Lygodactylus_mombasicus),show.legend=TRUE)+ geom_smooth(aes (x = wedge_prism, y = Lygodactylus_mombasicus), method = "lm") + labs (title = "Number of LYMO related to wedge_prism by forest type")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Number of LYMO by wedge prism.png")


###Number of LYMO related to wedge_prism by forest type###
ggplot(data = Species_by_described_site) + geom_jitter(mapping = aes (x = wedge_prism, y = Lygodactylus_mombasicus),show.legend=TRUE)+ geom_smooth(aes (x = wedge_prism, y = Lygodactylus_mombasicus), method = "lm") + facet_grid(.~forest_type) + labs (title = "Number of LYMO related to wedge_prism by forest type")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Number of LYMO by wedge prism by forest type.png")


###Number of HEPL related to DBH by forest type###
ggplot(data = Species_by_described_site) + geom_jitter(mapping = aes (x = DBH_cm, y = Hemidactylus_platycephalus), color = "red", alpha = 0.2)+ labs (title = "Number of HEPL related to DBH") + facet_grid(.~forest_type) + geom_smooth(aes(x = DBH_cm, y = Hemidactylus_platycephalus), method = "lm")

###Number of LYMO related to DBH by forest type###
ggplot(data = Species_by_described_site) + geom_jitter(mapping = aes (x = DBH_cm, y = Lygodactylus_mombasicus), color = "red", alpha = 0.2)+ labs (title = "Number of LYMO related to DBH") + facet_grid(.~forest_type) + geom_smooth(aes(x = DBH_cm, y = Lygodactylus_mombasicus), method = "lm")

###Number of HEPL related to HOT by forest type###
ggplot(data = Species_by_described_site) + geom_jitter(mapping = aes (x = HOT_m, y = Hemidactylus_platycephalus), color = "red", alpha = 0.2)+ labs (title = "Number of HEPL related to HOT") + facet_grid(.~forest_type) + geom_smooth(aes(x = HOT_m, y = Hemidactylus_platycephalus), method = "lm")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Number of hepl reloated to hot.png")


###Number of LYMO related to HOT by forest type###
ggplot(data = Species_by_described_site) + geom_jitter(mapping = aes (x = HOT_m, y = Lygodactylus_mombasicus), color = "red", alpha = 0.2)+ labs (title = "Number of LYMO related to HOT") + geom_smooth(aes(x = HOT_m, y = Lygodactylus_mombasicus), method = "lm") + facet_grid(.~forest_type)




##########forest_structure_exploration########

###Number_of_stems_by_leaf_layer_intercept###
ggplot(Species_by_described_site) + geom_jitter(aes (x = avg_leaf_int, y = stem_total, color = forest_type)) + geom_smooth(aes (x = avg_leaf_int, y = stem_total), method = "auto") + labs (title = "Number of stems by leaf layer intercept")

###M_forest_Number_of_stems_by_leaf_layer_intercept###
ggplot(data = filter(Species_by_described_site, forest_type == "M")) + geom_jitter(aes (x = avg_leaf_int, y = stem_total)) + geom_smooth(aes (x = avg_leaf_int, y = stem_total), method = "auto")+ labs (title = "Number of stems by leaf layer intercept in M forest")

###CY_forest_Number_of_stems_by_leaf_layer_intercept###
ggplot(data = filter(Species_by_described_site, forest_type == "CY")) + geom_jitter(aes (x = avg_leaf_int, y = stem_total)) + geom_smooth(aes (x = avg_leaf_int, y = stem_total), method = "auto") + labs (title = "Number of stems by leaf layer intercept in CY forest")

###BR_forest_Number_of_stems_by_leaf_layer_intercept###
ggplot(data = filter(Species_by_described_site, forest_type == "BR")) + geom_jitter(aes (x = avg_leaf_int, y = stem_total)) + geom_smooth(aes (x = avg_leaf_int, y = stem_total), method = "auto")+ labs (title = "Number of stems by leaf layer intercept in BR forest")

###Number_of_stem_by_forest_type_boxplot###
ggplot(data = Species_by_described_site) + geom_boxplot(aes (x = forest_type, y = stem_total))+ labs (title = "Number of stems by forest type")

###HOT by DBH by forest type_point
ggplot(data = Species_by_described_site) + geom_jitter(mapping = aes (x = DBH_cm, y = HOT_m), color = "red", alpha = 0.2)+ labs (title = "HOT by DBH by forest type") + facet_grid(.~forest_type) + geom_smooth(aes(x = DBH_cm, y = HOT_m), method = "lm")



########forest caracteristics by edge category#######

###Number_of_stem_depending_on_edge_category_by_forest_type_boxplot###
ggplot(data = Species_by_described_site) + geom_boxplot(mapping = aes (x = edge_category_m, y = stem_total))+ facet_grid(.~forest_type) + labs (title = "Number of stems by edge category by forest type")

###DBH by edge category by forest type_boxplot###
Species_by_described_site$edge_category_m <- factor(Species_by_described_site$edge_category_m, levels=c("-10", "0", "30", "100", "250", "500"))
ggplot(data = Species_by_described_site) + geom_boxplot(aes (x = edge_category_m, y = DBH_cm)) + geom_jitter(mapping = aes (x = edge_category_m, y = DBH_cm), color = "red", alpha = 0.2)+ labs (title = "DBH by edge category by forest type") + facet_grid(.~forest_type)

###HOT by edge category by forest type_boxplot###
Species_by_described_site$edge_category_m <- factor(Species_by_described_site$edge_category_m, levels=c("-10", "0", "30", "100", "250", "500"))
ggplot(data = Species_by_described_site) + geom_boxplot(aes (x = edge_category_m, y = HOT_m)) + geom_jitter(mapping = aes (x = edge_category_m, y = HOT_m), color = "red", alpha = 0.2)+ labs (title = "HOT by edge category by forest type") + facet_grid(.~forest_type)





######ANALYSIS RELATED TO HEIGHT IN TREE######

###shannon diversity index related to height in tree###
ggplot(data = filter(Species_by_height_found_percentage_breaks, height_found_percentage_breaks != "NA")) + geom_bar(mapping = aes(y =diversity_shannon, x =  height_found_percentage_breaks), stat = "identity") + coord_flip() + labs(title = "Shannon diversity index related to height in tree", y = "Shannon diversty index", x = "Height in the tree (percentage of HOT)")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Shannon diversity by height in tree.png")


###simpson diversity index related to height in tree###
ggplot(data = filter(Species_by_height_found_percentage_breaks, height_found_percentage_breaks != "NA")) + geom_bar(mapping = aes(y =diversity_simpson, x =  height_found_percentage_breaks), stat = "identity") + coord_flip() + labs(title = "simpson diversity index related to height in tree", y = "simpson diversty index", x = "Height in the tree (percentage of HOT)")

###abundance related to height in tree###
ggplot(data = filter(Species_by_height_found_percentage_breaks, height_found_percentage_breaks != "NA")) + geom_bar(mapping = aes(y =abund, x =  height_found_percentage_breaks), stat = "identity") + coord_flip() + labs(title = "abundance related to height in tree", y = "abundance", x = "Height in the tree (percentage of HOT)")

###richness related to height in tree###
ggplot(data = filter(Species_by_height_found_percentage_breaks, height_found_percentage_breaks != "NA")) + geom_bar(mapping = aes(y =rich, x =  height_found_percentage_breaks), stat = "identity") + coord_flip() + labs(title = "richness related to height in tree", y = "richness", x = "Height in the tree (percentage of HOT)")




###### day and night species study #####


### height of lygodactylus related to presence of hemidactylus in the same tree### 
ggplot(data=filter(animals_by_described_site, genus == "Lygodactylus", Genus_Lygo_and_Hemi_in_same_tree_climber_surveys != "Hemidactylus only", Genus_Lygo_and_Hemi_in_same_tree_climber_surveys != "NA", Genus_Lygo_and_Hemi_in_same_tree_climber_surveys != "NONE")) + geom_boxplot(mapping=aes(x = Genus_Lygo_and_Hemi_in_same_tree_climber_surveys, y = height_found_m)) + geom_jitter(mapping=aes(x = Genus_Lygo_and_Hemi_in_same_tree_climber_surveys, y = height_found_m), color = "red", alpha = 0.2) + facet_grid(.~day_night) + labs(title = "Height of genus Lygodactylus related to presence of Hemidactylus in same tree", x ="Presence / absence of genus Hemidactylus and Lygodactylus in the same tree", y = "Height of individuals (m)")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Height_Lygodactylus_by_presence_of_Hemidactylus_in_same_tree.png")

### height of hemidactylus related to presence of lygodactylus in the same tree### 
ggplot(data=filter(animals_by_described_site, genus == "Hemidactylus", Genus_Lygo_and_Hemi_in_same_tree_climber_surveys != "Lygodactylus only", Genus_Lygo_and_Hemi_in_same_tree_climber_surveys != "NA", Genus_Lygo_and_Hemi_in_same_tree_climber_surveys != "NONE")) + geom_boxplot(mapping=aes(x = Genus_Lygo_and_Hemi_in_same_tree_climber_surveys, y = height_found_m)) + geom_jitter(mapping=aes(x = Genus_Lygo_and_Hemi_in_same_tree_climber_surveys, y = height_found_m), color = "red", alpha = 0.2) + facet_grid(.~day_night) + labs(title = "Height of genus Hemidactylus related to presence of Lygodactylus in same tree", x ="Presence / absence of genus Hemidactylus and Lygodactylus in the same tree", y = "Height of individuals (m)")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Height_hemidactylus_by_presence_of_lygodactylus_in_same_tree.png")

###height of lygodactylus related to presence of hemidactylus in the same ground area###
ggplot(data=filter(animals_by_described_site, genus == "Lygodactylus", Genus_Lygo_and_Hemi_in_same_ground_area_ground_surveys != "Hemidactylus only", Genus_Lygo_and_Hemi_in_same_ground_area_ground_surveys != "NA", Genus_Lygo_and_Hemi_in_same_ground_area_ground_surveys != "NONE")) + geom_boxplot(mapping=aes(x = Genus_Lygo_and_Hemi_in_same_ground_area_ground_surveys, y = height_found_m)) + geom_jitter(mapping=aes(x = Genus_Lygo_and_Hemi_in_same_ground_area_ground_surveys, y = height_found_m), color = "red", alpha = 0.2) + facet_grid(.~day_night) + labs(title = "Height of genus Lygodactylus related to presence of Hemidactylus in same ground area", x ="Presence / absence of genus Hemidactylus and Lygodactylus in the same ground area", y = "Height of individuals (m)")


###height of hemidactylus related to presence of lygodactylus in the same ground area###
ggplot(data=filter(animals_by_described_site, genus == "Hemidactylus", Genus_Lygo_and_Hemi_in_same_ground_area_ground_surveys != "Lygodactylus only", Genus_Lygo_and_Hemi_in_same_ground_area_ground_surveys != "NA", Genus_Lygo_and_Hemi_in_same_ground_area_ground_surveys != "NONE")) + geom_boxplot(mapping=aes(x = Genus_Lygo_and_Hemi_in_same_ground_area_ground_surveys, y = height_found_m)) + geom_jitter(mapping=aes(x = Genus_Lygo_and_Hemi_in_same_ground_area_ground_surveys, y = height_found_m), color = "red", alpha = 0.2) + facet_grid(.~day_night) + labs(title = "Height of genus Hemidactylus related to presence of Lygodactylus in same ground area", x ="Presence / absence of genus Lygodactylus and Hemidactylus in the same ground area", y = "Height of individuals (m)")



### height of hemidactylus and lygodactylus, day, night ###
ggplot(data=filter(animals_by_described_site, genus == "Hemidactylus"| genus == "Lygodactylus", survey_day_and_night == "Y")) + geom_boxplot(mapping=aes(x = genus, y = height_found_m)) + geom_jitter(mapping=aes(x = genus, y = height_found_m), color = "red", alpha = 0.2) + facet_grid(.~day_night) + labs(title = "Height of genus Hemidactylus and Lygodactylus, only suverys with ground and climb", x ="Genus", y = "Height of individuals (m)")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/Height_hemidactylus_and_lygodactylus_day_night.png")

### number of hemidactylus and hemidactylus in sites surveyed day and night###
ggplot(data=filter(animals_by_described_site, genus == "Hemidactylus"| genus == "Lygodactylus", survey_day_and_night == "Y")) + geom_histogram(mapping=aes(x = genus), stat = "count") + facet_grid(.~day_night) + labs(title = "Number of Hemidactylus and Lygodactylus for sites surveyed day and night", x ="Genus", y = "Number of individuals")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/number_of_hemidactylus_and_lygodactylus_day_night.png")

### substrate of hemidactylus, day night###
ggplot(data=filter(animals_by_described_site, genus == "Hemidactylus", survey_day_and_night == "Y")) + geom_bar(mapping=aes(x = substrate_rec )) + facet_grid(.~day_night) + labs(title = "Substrate of Hemidactylus by substrate for sites surveyed day and night", x ="Substrate", y = "Number of individuals from genus Hemidactylus") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/substrate_of_hemidactulus_day_night.png")


### substrate of lygodactylus, day night###
ggplot(data=filter(animals_by_described_site, genus == "Lygodactylus", survey_day_and_night == "Y")) + geom_bar(mapping=aes(x = substrate_rec )) + facet_grid(.~day_night) + labs(title = "Substrate of Lygodactylus by substrate for sites surveyed day and night", x ="Substrate", y = "Number of individuals from genus Lygodactylus") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/substrate_of_lygodactulus_day_night.png")

### height of tree by presence of hemidactylus and lygodactylus in same tree###
ggplot(data=filter(animals_by_described_site, Genus_Lygo_and_Hemi_in_same_tree_climber_surveys != "NA")) + geom_boxplot(mapping=aes(x = Genus_Lygo_and_Hemi_in_same_tree_climber_surveys, y = HOT_m)) + geom_jitter(mapping=aes(x = Genus_Lygo_and_Hemi_in_same_tree_climber_surveys, y = HOT_m), color = "red", alpha = 0.2) + labs(title = "Height of tree by presence of genus Hemidactylus and Lygodactylus in same tree", x ="Presence / absence of genus Hemidactylus and Lygodactylus in the same tree", y = "Height of the tree (m)")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/height_of_tree_by_presence_of_hemidactylus_and_lygodactylus_in_same_tree.png")

### height of tree by presence of hemidactylus or lygodactylus###
ggplot(data=filter(animals_by_described_site, genus == "Hemidactylus" | genus == "Lygodactylus")) + geom_boxplot(mapping=aes(x = genus, y = HOT_m)) + geom_jitter(mapping=aes(x = genus, y = HOT_m), color = "red", alpha = 0.2) + labs(title = "Height of tree by presence of genus Hemidactylus or Lygodactylus", x ="Presence  of genus Hemidactylus and Lygodactylus", y = "Height of the tree (m)")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "D:/Documents/TRAVAIL//KENYA/DATA/DATA_Analysis/Analysis/Graphs/height_of_tree_by_presence_of_hemidactylus_and_lygodactylus.png")
