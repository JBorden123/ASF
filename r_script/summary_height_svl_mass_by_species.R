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


############# NEW SUMMARY DATA FRAMES #####

### height by species, min mean max, SVL min  max, mass min max, plus n

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
                                           "Lamprophiidae" = "Psammophi_ orientalis",
                                           "Lamprophiidae" = "Psammophi_ punctulatus",
                                           "Lamprophiidae" = "Rhamphiophis_rostratus",
                                           "Scincidae" = "Trachylepi_ maculilabris",
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

write.xlsx(height_SVL_by_species, file="output/SummaryHeightSVLMass.xlsx", sheetName="Summary", row.names = FALSE)
# load it back
wb <- loadWorkbook("output/SummaryHeightSVLMass.xlsx")
sheets <- getSheets(wb)
# autosize column widths
autoSizeColumn(sheets[[1]], colIndex=1:ncol(height_SVL_by_species))
saveWorkbook(wb,"output/SummaryHeightSVLMass.xlsx")



