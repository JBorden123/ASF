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
library(gtools)

#### CHARGE DATA
data <- read.csv("clean_data/MetaAll.csv", header = TRUE)
animals <- read.csv("raw_data/herpdata.csv", header = TRUE)

# put metadata in animals
animals <- merge(animals, data,intersect = c("Tree_ID", "Tree_ID"), all.x = TRUE, all.y = FALSE)



############# NEW SUMMARY DATA FRAMES #####

### height by species, min mean max, SVL min  max, mass min max, plus n

## !! mean height with canopy surveys only to not lower mean height
## !! remove amphibians and unknown species

#sumarise
height_SVL_by_species <- animals %>% filter(amph_rept == "R", species != "sp.", binomial != "NA") %>% group_by(binomial) %>% summarise(Min_height_m = min(height_found_m, na.rm = TRUE), Max_height_m = max(height_found_m, na.rm = TRUE), Mean_SVL_cm = mean(SVL_cm, na.rm = TRUE),  Max_SVL_cm = max(SVL_cm, na.rm = TRUE), Min_mass_g = min(mass_g, na.rm = TRUE), Max_mass_g = max(mass_g, na.rm = TRUE))

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
height_SVL_by_species$Mean_SVL_cm <- format(round(height_SVL_by_species$Mean_SVL_cm, 1), nsmall = 1)
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







##### ADD max number per same area and per same tree

######### create summary data frame

# !! canopy surveys only  to only get individuals in trees very close to each other, sharing habitat

#### max number of each species by tree
aggregation_per_tree <- animals %>% filter(survey_type == "C", amph_rept == "R", species != "sp.") %>% group_by(Tree_ID, binomial) %>% tally()
aggregation <- aggregation_per_tree %>% group_by(binomial) %>% summarise(abund_max_per_tree = max(n))

#### max number of each species per same area (about circle with 60 m diameter)
aggregation_per_area <- animals %>% filter(amph_rept == "R", species != "sp.") %>% group_by(Tree_ID, binomial) %>% tally()
a <- aggregation_per_area %>% group_by(binomial) %>% summarise(abund_max_per_area = max(n))


### merge together
aggregation <- merge(aggregation, a, intersect = c("binomial", "binomial"), all = TRUE)

#remove NA
aggregation <- aggregation %>% filter(binomial != "NA")






#### merge with summary height SVL mass
summary_species <- merge(height_SVL_by_species, aggregation, intersect = c("binomial", "binomial"), all = TRUE)

#remove mass to make place for others values columns
summary_species$Min_mass_g = NULL
summary_species$Max_mass_g = NULL



## add column Family
summary_species <- mutate(summary_species, Family = binomial)

## Recodage de height_SVL_by_species$Family en height_SVL_by_species$Family_rec
summary_species$Family <- fct_recode(summary_species$Family,
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
                                           "Lamprophiidae" = "Psammophi_ orientalis",
                                           "Lamprophiidae" = "Psammophi_ punctulatus",
                                           "Lamprophiidae" = "Rhamphiophis_rostratus",
                                           "Scincidae" = "Trachylepi_ maculilabris",
                                           "Scincidae" = "Trachylepis_planifrons",
                                           "Scincidae" = "Trachylepis_varia",
                                           "Varanidae" = "Varanus_albigularis")

# recode Family as character
summary_species$Family <- as.character(as.factor(summary_species$Family))


## order columns
summary_species = summary_species[,c("Family","binomial","n","Min_height_m", "Mean_height_m", "Max_height_m", "Mean_SVL_cm", "Max_SVL_cm","abund_max_per_tree", "abund_max_per_area" )]

## Recodage de summary_species$Mean_height_m en summary_species$Mean_height_m_rec
summary_species$Mean_height_m <- fct_recode(summary_species$Mean_height_m,
                                                  NULL = "  NA (NA)")

## Recodage de summary_species$Mean_SVL_cm en summary_species$Mean_SVL_cm_rec
summary_species$Mean_SVL_cm <- fct_recode(summary_species$Mean_SVL_cm,
                                               NULL = "  Inf")

## Recodage de summary_species$Max_SVL_cm en summary_species$Max_SVL_cm_rec
summary_species$Max_SVL_cm <- fct_recode(summary_species$Max_SVL_cm,
                                               NULL = " -Inf")

## order the data frame alphabetically by Family
summary_species <- summary_species[order(summary_species$binomial),]
summary_species <- summary_species[order(summary_species$Family),]

# rename columns
names(summary_species)[names(summary_species) == "binomial"] <- "Species"
names(summary_species)[names(summary_species) == "Min_height_m"] <- "Min height (m)"
names(summary_species)[names(summary_species) == "Mean_height_m"] <- "Mean height (m)"
names(summary_species)[names(summary_species) == "Mean_height_m"] <- "Max height (m)"
names(summary_species)[names(summary_species) == "Mean_SVL_cm"] <- "Mean SVL (cm)"
names(summary_species)[names(summary_species) == "Max_SVL_cm"] <- "Max SVL (cm)"
names(summary_species)[names(summary_species) == "abund_max_per_tree"] <- "Max abundance per tree"
names(summary_species)[names(summary_species) == "abund_max_per_area"] <- "Max abundance per area"


# write species name correctly
## Recodage de summary_species$Species en summary_species$Species_rec
summary_species$Species <- fct_recode(summary_species$Species,
               "Arthroleptis stenodactylus" = "Arthroleptis_stenodactylus",
               "Chamaeleo dilepis" = "Chamaeleo_dilepis",
               "Cordylus tropidosternum" = "Cordylus_tropidosternum",
               "Dispholidus typus" = "Dispholidus_ typus",
               "Gastropholis prasina" = "Gastropholis_prasina",
               "Hemidactylus barbouri" = "Hemidactylus_ barbouri",
               "Hemidactylus angulatus" = "Hemidactylus_angulatus",
               "Hemidactylus mabouia" = "Hemidactylus_mabouia",
               "Hemidactylus mrimaensis" = "Hemidactylus_mrimaensis",
               "Hemidactylus platycephalus" = "Hemidactylus_platycephalus",
               "Latastia longicaudata" = "Latastia_longicaudata",
               "Lygodactylus mombasicus" = "Lygodactylus_mombasicus",
               "Lygodactylus sp." = "Lygodactylus_sp.",
               "Mochlus afer" = "Mochlus_afer",
               "Mochlus sundevalli" = "Mochlus_sundevalli",
               "Naja melanoleuca" = "Naja_melanoleuca",
               "Nucras boulengeri" = "Nucras_boulengeri",
               "Psammophis orientalis" = "Psammophi_ orientalis",
               "Psammophis punctulatus" = "Psammophi_ punctulatus",
               "Rhamphiophis rostratus" = "Rhamphiophis_rostratus",
               "Trachylepis maculilabris" = "Trachylepi_ maculilabris",
               "Trachylepis planifrons" = "Trachylepis_planifrons",
               "Trachylepis varia" = "Trachylepis_varia",
               "Varanus albigularis" = "Varanus_albigularis")




#view
View(summary_species)

#export it

write.xlsx(summary_species, file="output/SummarySpecies.xlsx", sheetName="Summary", row.names = FALSE)
# load it back
wb <- loadWorkbook("output/SummarySpecies.xlsx")
sheets <- getSheets(wb)
# autosize column widths
autoSizeColumn(sheets[[1]], colIndex=1:ncol(summary_species))
saveWorkbook(wb,"output/SummarySpecies.xlsx")






############# number of species able to access the vertical gradient
# !!!! careful, must be done a bit differently, this is counting on the ground species that havent' been seen on the ground

a <- animals



#get max height by species
a <- a %>% group_by(binomial, forest_type) %>% summarise(min_height_m = min(height_found_m, na.rm = TRUE), max_height_m = max(height_found_m, na.rm = TRUE))

a <- a %>% filter(binomial != "NA")

a <- a %>% mutate(zero_to_one = ifelse(min_height_m < 1, 1, 0))
a <- a %>% mutate(one_to_two = ifelse(min_height_m <2 & max_height_m >= 1, 1, 0))
a <- a %>% mutate(two_to_three = ifelse(min_height_m < 3 & max_height_m >= 2, 1, 0))
a <- a %>% mutate(three_to_four = ifelse(min_height_m < 4 & max_height_m >= 3, 1, 0))
a <- a %>% mutate(four_to_five = ifelse(min_height_m < 5 & max_height_m >= 4, 1, 0))
a <- a %>% mutate(five_to_six = ifelse(min_height_m < 6 & max_height_m >= 5, 1, 0))
a <- a %>% mutate(six_to_seven = ifelse(min_height_m < 7 & max_height_m >= 6, 1, 0))
a <- a %>% mutate(seven_to_height = ifelse(min_height_m < 8 & max_height_m >= 7, 1, 0))
a <- a %>% mutate(height_to_nine = ifelse(min_height_m < 9 & max_height_m >= 8, 1, 0))
a <- a %>% mutate(nine_to_ten = ifelse(min_height_m < 10 & max_height_m >= 9, 1, 0))
a <- a %>% mutate(ten_to_eleven = ifelse(min_height_m < 11 & max_height_m >= 10, 1, 0))
a <- a %>% mutate(eleven_to_twelve = ifelse(min_height_m < 12 & max_height_m >= 11, 1, 0))
a <- a %>% mutate(twelve_to_thirteen = ifelse(min_height_m < 13 & max_height_m >= 12, 1, 0))
a <- a %>% mutate(thirteen_to_fourteen = ifelse(min_height_m < 14 & max_height_m >= 13, 1, 0))
a <- a %>% mutate(fourteen_to_fifteen = ifelse(min_height_m < 15 & max_height_m >= 14, 1, 0))
a <- a %>% mutate(fifteen_to_sixteen = ifelse(min_height_m < 16 & max_height_m >= 15, 1, 0))
a <- a %>% mutate(sixteen_to_seventeen = ifelse(min_height_m < 17 & max_height_m >= 16, 1, 0))


a <- a %>% group_by(forest_type) %>% summarise(zero_to_one = sum(zero_to_one), one_to_two = sum(one_to_two), two_to_three = sum(two_to_three), three_to_four = sum(three_to_four), four_to_five = sum(four_to_five), five_to_six = sum(five_to_six), six_to_seven = sum(six_to_seven), seven_to_height = sum(seven_to_height), height_to_nine = sum(height_to_nine), nine_to_ten = sum(nine_to_ten), ten_to_eleven = sum(ten_to_eleven), eleven_to_twelve = sum(eleven_to_twelve), twelve_to_thirteen = sum(twelve_to_thirteen), thirteen_to_fourteen = sum(thirteen_to_fourteen), fourteen_to_fifteen = sum(fourteen_to_fifteen), fifteen_to_sixteen = sum(fifteen_to_sixteen), sixteen_to_seventeen = sum(sixteen_to_seventeen))


names(a)[names(a) == "zero_to_one"] <- "[0, 1)"
names(a)[names(a) == "one_to_two"] <- "[1, 2)"
names(a)[names(a) == "two_to_three"] <- "[2, 3)"
names(a)[names(a) == "three_to_four"] <- "[3, 4)"
names(a)[names(a) == "four_to_five"] <- "[4, 5)"
names(a)[names(a) == "five_to_six"] <- "[5, 6)"
names(a)[names(a) == "six_to_seven"] <- "[6, 7)"
names(a)[names(a) == "seven_to_height"] <- "[7, 8)"
names(a)[names(a) == "height_to_nine"] <- "[8, 9)"
names(a)[names(a) == "nine_to_ten"] <- "[9, 10)"
names(a)[names(a) == "ten_to_eleven"] <- "[10, 11)"
names(a)[names(a) == "eleven_to_twelve"] <- "[11, 12)"
names(a)[names(a) == "twelve_to_thirteen"] <- "[12, 13)"
names(a)[names(a) == "thirteen_to_fourteen"] <- "[13, 14)"
names(a)[names(a) == "fourteen_to_fifteen"] <- "[14, 15)"
names(a)[names(a) == "fifteen_to_sixteen"] <- "[15, 16)"
names(a)[names(a) == "sixteen_to_seventeen"] <- "[16, 17)"




a <- melt(a, id= "forest_type")

names(a)[names(a) == "variable"] <- "height"
names(a)[names(a) == "value"] <- "species_number"

#reorder
a <- a %>% arrange(desc(species_number))

#plot
ggplot(data = a, mapping = aes(x = height, y = species_number)) + geom_bar(stat = "identity") + coord_flip() + theme_bw(base_size = 23) + facet_grid(.~forest_type) + labs(title = "Species richness by height", y = "Number of species accessing the vertical strata", x = "Height (m)")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "figures/NumberSpeciesUsingVerticalGradient.png")







######### SUMMARY DATA FRAME WITH NUMERIC COLUMNS FOR GRAPHS

#sumarise
summary_species_numeric <- animals %>% filter(amph_rept == "R", species != "sp.", binomial != "NA") %>% group_by(binomial) %>% summarise(Mean_height_m = mean(height_found_m, na.rm = TRUE), Min_height_m = min(height_found_m, na.rm = TRUE), Max_height_m = max(height_found_m, na.rm = TRUE), Mean_SVL_cm = mean(SVL_cm, na.rm = TRUE),  Max_SVL_cm = max(SVL_cm, na.rm = TRUE), Min_mass_g = min(mass_g, na.rm = TRUE), Max_mass_g = max(mass_g, na.rm = TRUE), sd_height_m=sd(height_found_m, na.rm = TRUE))

#add height range
summary_species_numeric <- mutate(summary_species_numeric, Height_range_m = Max_height_m - Min_height_m)

#add column number of individuals
a <- animals %>% filter(amph_rept == "R", species != "sp.", binomial != "NA") %>% group_by(binomial) %>% tally()
summary_species_numeric <- merge(a, summary_species_numeric, by=c("binomial","binomial"), all = TRUE)





############ GRAPHS


### standard deviation of height VS height, mean by species
a <- ggplot(data = filter(summary_species_numeric, n > 1)) + geom_point(mapping = aes(x = Mean_height_m, y = sd_height_m)) + geom_smooth(mapping = aes(x = Mean_height_m, y = sd_height_m), method = "lm") + labs(title = "Height sd VS mean by species (all surveys, n > 1)", x = "Mean height by species (m)", y = "Standard deviation of height by species (m)") + theme_bw(base_size = 13)
data_frame <- filter(summary_species_numeric, n > 1)
lm_eqn <- function(data_frame){
  m <- lm(sd_height_m ~ Mean_height_m, data_frame);
  eq <- substitute(italic(r)~"="~rvalue*","~italic(p)~"="~pvalue, list(rvalue = sprintf("%.2f",sign(coef(m)[2])*sqrt(summary(m)$r.squared)), pvalue = format(summary(m)$coefficients[2,4], digits = 2)))
  as.character(as.expression(eq));                 
}
a <- a + geom_text(x = 1, y = 3, label = lm_eqn(data_frame), parse = TRUE)
a  <- a +  geom_text(
  label=data_frame$binomial, 
  nudge_x = 100, nudge_y = 100, x = data_frame$Mean_height_m, y = data_frame$sd_height_m, check_overlap = TRUE
 
)
a
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "figures/Sd_height_VS_Mean_height_by_species.png")





