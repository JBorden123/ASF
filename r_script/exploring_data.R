library(ggplot2)
library(dplyr)
library(cowplot)

#data exploring
#THE DATA
herpdata <- read.csv("raw_data/herpdata.csv", header = TRUE)
metadata <- read.csv("clean_data/metadata.csv", header = TRUE)
sites <- read.csv("raw_data/sites.csv", header = TRUE)


#abund by coarse edge distance
ggplot(metadata, aes(edge_category_m, abund))+
  geom_point()+
  geom_jitter()

#rich by coarse edge distance
ggplot(metadata, aes(edge_category_m, rich))+
  geom_point()+
  geom_jitter()

#shannon diversity by coarse edge distance
ggplot(metadata, aes(edge_category_m, diversity_shannon))+
  geom_point()+
  geom_jitter()

#abund by tree species
ggplot(metadata, aes(Tree_species, abund))+
  geom_point()+
  geom_jitter()

#platycephalus abundance by coarse edge dist
ggplot(metadata, aes(edge_category_m, HEPL))+
  geom_point()+
  geom_jitter()

#L. mombasicus abundance by coarse edge dist
ggplot(metadata, aes(edge_category_m, LYMO))+
  geom_point()+
  geom_jitter()

#C. dilepis abundance by coarse edge dist
ggplot(metadata, aes(edge_category_m, CHDI))+
  geom_point()+
  geom_jitter()


#selecting species by habitat type
HerpByHab <- merge(herpdata, sites, by = "Tree_ID", all = TRUE)
HerpByHabMat <- HerpByHab

HerpByHab <- HerpByHab %>%
  filter(edge_category_m != -10)%>%
  filter(binomial != "NA")%>%
  group_by(forest_type, binomial) %>%
  summarise(SpecAbun = n())

HerpByHabMat <- HerpByHabMat %>%
  filter(edge_category_m == -10)%>%
  filter(binomial != "NA")%>%
  group_by(forest_type, binomial) %>%
  summarise(SpecAbun = n())


HerpHabPlot1 <- ggplot(HerpByHab, aes(binomial, SpecAbun, color = forest_type))+
  geom_col(aes(fill = forest_type))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  labs(title = "Species by Habitat Type")

HerpHabPlot2 <- ggplot(HerpByHabMat, aes(binomial, SpecAbun, color = forest_type))+
  geom_col(aes(fill = forest_type))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  labs(title = "Species by Habitat Type Matrix")

plot_grid(HerpHabPlot1, HerpHabPlot2, labels = c("Forest", "Matrix"), ncol = 2, nrow = 1)

plot_grid(inv_abun_urb2, nat_abun_urb2, inv_rich_urb2, nat_rich_urb2, 
          labels = c("        Invasives", "         Natives"), ncol = 2, nrow = 2)

HerpHabMat <- merge(herpdata, sites, by = "Tree_ID", all = TRUE)

HerpHabMat <- HerpHabMat %>%
  group_by()

