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
animals$height_found_m_rec <- as.numeric(as.character(animals$height_found_m))

###recoding_dist_from_center_in_numeric###
animals$dist_frm_center_m_rec <- as.numeric(as.character(animals$dist_frm_center_m))

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
habitat$edge_category_m <- as.character(as.numeric(habitat$edge_category_m))

###restrict_habitat_to_summarized_data###
sum_habitat <- select(habitat, Tree_ID, wedge_prism, debris, surveyor, avg_canopy_cover, avg_leaf_int, stem_less_8cm, stem_more_8cm)
sum_habitat <- filter(sum_habitat, stem_less_8cm != "NA")
View(sum_habitat)
write.csv(sum_habitat, "D:/Documents/TRAVAIL/Kenya/DATA/habitat_summarized.csv")



###CREATE_DATA_FRAMES###

###create_number_of_each_species_by_site
Species_by_site <- (animals %>% group_by(Tree_ID, binomial) %>% tally %>% cast(Tree_ID~binomial))
View(Species_by_site)


###Link_Sites_and_Species_by_site###
Species_by_described_site <- merge(Species_by_site, sites, by = intersect("Tree_ID", "Tree_ID"), all = TRUE)


###Link_species_by_described_site_with_sum_habitat###
Species_by_described_site <- merge(Species_by_described_site, sum_habitat, by = intersect("Tree_ID", "Tree_ID"), all = TRUE)
View(Species_by_described_site)



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


###GRAPH_EXPLORATION###


###boxplot_dist_from_center_geckos###
ggplot(data = filter(animals, genus == "Hemidactylus" | genus == "Lygodactylus")) + geom_boxplot(mapping = aes (x = genus, y = dist_frm_center_m_rec), show.legend=FALSE) + facet_grid(.~day_night)

###boxplot_height_found_geckos_only_C_surveys###
ggplot(data = filter(animals, genus == "Hemidactylus" | genus == "Lygodactylus", survey_type == "C")) + geom_boxplot(mapping = aes (x = genus, y = height_found_m_rec), show.legend=FALSE) + facet_grid(.~day_night)

### density_height_found_geckos_only_C_surveys###
ggplot(data = filter(animals, genus == "Hemidactylus" | genus == "Lygodactylus", survey_type == "C")) + geom_density(mapping = aes (height_found_m_rec, color = genus)) + facet_grid(.~day_night)

### distance_from_center_geckos_only_C_surveys###
ggplot(data = filter(animals, genus == "Hemidactylus" | genus == "Lygodactylus", survey_type == "C")) + geom_density(mapping = aes (dist_frm_center_m_rec, color = genus)) + facet_grid(.~day_night)


### Number_of_HEPL_by_edge_category_by_forest_type###
ggplot(data = Species_by_described_site) + geom_jitter(mapping = aes (x = edge_category_m, y = Hemidactylus_platycephalus, color = forest_type),show.legend=TRUE)

### Number_of_HEPL_by_edge_category_boxplot###
Species_by_described_site$edge_category_m <- factor(Species_by_described_site$edge_category_m, levels=c("-10", "0", "30", "100", "250", "500"))
ggplot(data = Species_by_described_site) + geom_boxplot(aes (x = edge_category_m, y = Hemidactylus_platycephalus))