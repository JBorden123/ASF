###########
#code for generating diversity, richness, abundance, eveness and species accum curves
###########

library(dplyr)
library(ggplot2)
library(vegan)
library(reshape)
library(tidyr)

#data
herpdata <- read.csv("raw_data/herpdata.csv", header = T) #set this up

####################
#For Entire Forest
####################
#generating abundances for all species by sites
herpdata2<-herpdata %>%
  group_by(Tree_ID,species_code) %>%
  tally()
herpdata2<- cast(herpdata2, Tree_ID ~ species_code, value='n')
herpdata2[is.na(herpdata2)]<-0

spec_names <- names(herpdata2[,2:24]) #check this to be sure this 
#captures correct names of species columns

########################
#Add diversity metrics
spec_counts <- herpdata2[,spec_names]
row.names(spec_counts) <- herpdata2$Tree_ID

#shannon diversity index
herpdata2$diversity_shannon<-diversity(spec_counts, index = "shannon")

#simpsons
herpdata2$diversity_simpson <- diversity(spec_counts, index = "simpson")

#fisher's Alpha
#herpdata2$fisher_alpha <- fisher.alpha(spec_counts)

#abundance
herpdata2$abund <-rowSums(herpdata2 [,spec_names])

#richness
herpdata2$rich <- rowSums(herpdata2 [,spec_names] > 0)


#save it
write.csv(herpdata2, file = "clean_data/biodiv_data.csv")

