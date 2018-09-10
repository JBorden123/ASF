library(dplyr)
library(ggplot2)
library(vegan)
library(reshape)
library(tidyr)

#data
metadata <- read.csv("clean_data/metadata.csv")
BR_metadata <- read.csv("clean_data/BR_metadata.csv")
M_metadata <- read.csv("clean_data/M_metadata.csv")
CY_metadata <- read.csv("clean_data/CY_metadata.csv")

###########
#species accumulation curves

par(mfrow = c(2,2))
##########
#All data
###########
spec_names <- names(metadata[,3:25]) #check this to be sure this 
#captures correct names of species columns

spec_counts <- metadata[,spec_names]

#species acummulation curve
spec_accum <- specaccum(spec_counts)
plot(spec_accum, main = "All", ylim = c(0,25))

#CHAO 1 Estimate for each site 
estimateR(spec_counts)

#Rarefy..... need to remove rows with NO individuals
min.N <- min(rowSums(spec_counts)) #sum all individuals for each plot and find the minimum of that

#Rarefying your plot is if you have slightly different sampling techniques or efforts
#helps reduce the screw up from different sampling efforts
rarefy(spec_counts, sample = min.N, se = TRUE)

##########
#BRACHYSTEGIA DATA
###########

BR_spec_counts <- BR_metadata[,spec_names]

#species acummulation curve
BR_spec_accum <- specaccum(BR_spec_counts)
plot(BR_spec_accum, main = "Brachystegia", ylim = c(0,25))

#CHAO 1 Estimate for each site 
estimateR(BR_spec_counts)


##########
#Mixed Forest data
###########
M_spec_counts <- M_metadata[,spec_names]

#species acummulation curve
M_spec_accum <- specaccum(M_spec_counts)
plot(M_spec_accum, main = "Mixed", ylim = c(0,25))

#CHAO 1 Estimate for each site 
estimateR(M_spec_counts)


##########
#Cynometera Forest data
###########
CY_spec_counts <- CY_metadata[,spec_names]

#species acummulation curve
CY_spec_accum <- specaccum(CY_spec_counts)
plot(CY_spec_accum, main = "Cynometera", ylim = c(0,25))

#CHAO 1 Estimate for each site 
estimateR(CY_spec_counts)


