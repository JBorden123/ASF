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
a <- animals



#get mex height by species
a <- a %>% group_by(binomial) %>% summarise(max_height_m = max(height_found_m, na.rm = TRUE))
## Recodage de a$height_found_m en a$height_found_m_rec
a$max_height_m_rec <- cut(a$max_height_m, include.lowest=FALSE,  right=FALSE, left = TRUE,
                            breaks=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))
#count number of species for each max depth rank
a <- a %>% group_by(max_height_m_rec) %>% tally()
#add row with 0 species for this max height
b<-data.frame("[2,3)","0")
names(b)<-c("max_height_m_rec","n")
c<-data.frame("[5,6)","0")
names(c)<-c("max_height_m_rec","n")
d<-data.frame("[9,10)","0")
names(d)<-c("max_height_m_rec","n")
e<-data.frame("[10,11)","0")
names(e)<-c("max_height_m_rec","n")
f<-data.frame("[4,5)","0")
names(f)<-c("max_height_m_rec","n")
g<-data.frame("[13,14)","0")
names(g)<-c("max_height_m_rec","n")
#merge
a <- rbind(a, b, c, d, e, g ,f)
## RecodE to start at 0
## Recodage de a$max_height_m_rec en a$max_height_m_rec_rec
a$max_height_m_rec <- fct_recode(a$max_height_m_rec,
               "[0,2)" = "[1,2)",
               "[0,3)" = "[2,3)",
               "[0,4)" = "[3,4)",
               "[0,5)" = "[4,5)",
               "[0,6)" = "[5,6)",
               "[0,7)" = "[6,7)",
               "[0,8)" = "[7,8)",
               "[0,9)" = "[8,9)",
               "[0,10)" = "[9,10)",
               "[0,11)" = "[10,11)",
               "[0,12)" = "[11,12)",
               "[0,13)" = "[12,13)",
               "[0,14)" = "[13,14)",
               "[0,15)" = "[14,15)",
               "[0,16)" = "[15,16)",
               "[0,17)" = "[16,17)",
               "[0,3)" = "[2,3)",
               "[0,6)" = "[5,6)",
               "[0,10)" = "[9,10)",
               "[0,11)" = "[10,11)")

#reorder
a <- a %>% arrange(desc(max_height_m_rec))
#cumulative sum
a <- a %>% mutate(n_species = cumsum(n))
#plot
ggplot(data = a) + geom_bar(mapping = aes(x = max_height_m_rec, y = n_species), stat = "identity") + coord_flip() + theme_bw(base_size = 13) + labs(title = "Number of species able to use the vertical gradient", y = "Number of species accessing the vertical strata", x = "Height in the tree (m)")
ggsave(width = 14, height = 8, device = "png", plot = last_plot(), filename = "figures/NumberSpeciesUsingVerticalGradient.png")












