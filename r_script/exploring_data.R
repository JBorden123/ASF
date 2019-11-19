library(ggplot2)
library(dplyr)
library(cowplot)

#data exploring
#THE DATA
herpdata <- read.csv("raw_data/herpdata.csv", header = TRUE)
metadata <- read.csv("clean_data/metadata.csv", header = TRUE)
sites <- read.csv("raw_data/sites.csv", header = TRUE)

#historgram of heights of all species colored by species
ggplot(herpdata, aes(height_found_m_rec, fill = species_code))+
  geom_histogram()


summary(herpdata$height_found_m)

ggplot(metadata, aes(StemLess8cm, abund))+
  geom_point()+
  geom_smooth(method = lm)

ggplot(metadata, aes(StemLess8cm, rich))+
  geom_point()+
  geom_smooth(method = lm)

ggplot(metadata, aes(StemLess8cm, diversity_shannon))+
  geom_point()+
  geom_smooth(method = lm)

#abund by coarse edge distance
ggplot(metadata, aes(edge_category_m, abund))+
  geom_bar()+
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
HerpByEdge <- HerpByHab

#make an category column with edge, core and matrix, based on the edge category column
HerpByEdge$category <- case_when(
  HerpByEdge$edge_category_m > 50 ~ "Core",
  HerpByEdge$edge_category_m < 50 & HerpByEdge$edge_category_m > -1 ~ "Edge",
  HerpByEdge$edge_category_m < 0 ~ "Matrix",
  TRUE ~ as.character(HerpByEdge$category)
  )


#make habitat type 
HerpByHab <- HerpByHab %>%
  filter(edge_category_m != -10)%>%
  filter(species_code != "NA")%>%
  group_by(forest_type, species_code) %>%
  summarise(SpecAbun = n())


HerpByHabMat <- HerpByHabMat %>%
  filter(edge_category_m == -10)%>%
  filter(species_code != "NA")%>%
  group_by(forest_type, species_code) %>%
  summarise(SpecAbun = n())

HerpByEdge <- HerpByEdge %>%
  filter(species_code != "NA" & category != "NA")%>%
  group_by(category, species_code) %>%
  summarise(SpecAbun = n())

#PLOT SPECIES ABUNDANCES
#all ASF
ggplot(HerpByHab, aes(species_code,SpecAbun, width = .75))+
  geom_bar(position = "dodge",stat = "identity", 
           aes(fill = forest_type), colour = "#27223C")+
  coord_flip() +
  scale_fill_manual(values=c("#DBB165", "#D3DDDC", "#2E604A"))+
  labs(title = "Species by Habitat Type")+
  scale_x_discrete(
    limits=c("NAME","GAPR","TRVA","VAAL","ARST","COTR","DITY",
             "LALO","MOAF","MOSU","PSOR","HEBA","HEMI",
             "NUBO","TRMA","CHDI","HEMA","LYMO","HEPL"))
#all ASF by EDGE
ggplot(HerpByEdge, aes(species_code,SpecAbun, width = .75))+
  geom_bar(position = "dodge",stat = "identity", 
           aes(fill = category), colour = "#27223C")+
  coord_flip() +
  scale_fill_manual(values=c("#2E604A", "#DBB165", "#D3DDDC"))+
  labs(title = "Species by Edge, Core, Matrix")+
  scale_x_discrete(
    limits=c("NAME","GAPR","TRVA","VAAL","ARST","COTR","DITY",
             "LALO","MOAF","MOSU","PSOR","HEBA","HEMI",
             "NUBO","TRMA","CHDI","HEMA","LYMO","HEPL"))
#matrix
ggplot(HerpByHabMat, aes(species_code,SpecAbun, width = .75))+
  geom_bar(position = "dodge",stat = "identity", 
           aes(fill = forest_type), colour = "#27223C")+
  coord_flip() +
  scale_fill_manual(values=c("#DBB165", "#D3DDDC", "#2E604A"))+
  labs(title = "Species by Habitat Type Matrix")+
  scale_x_discrete(
    limits=c("NAME","GAPR","TRVA","VAAL","ARST","COTR","DITY",
             "LALO","MOAF","MOSU","PSOR","HEBA","HEMI",
             "NUBO","TRMA","CHDI","HEMA","LYMO","HEPL"))
  


plot_grid(HerpHabPlot1, HerpHabPlot2, labels = c("Forest", "Matrix"), ncol = 2, nrow = 1)

plot_grid(inv_abun_urb2, nat_abun_urb2, inv_rich_urb2, nat_rich_urb2, 
          labels = c("        Invasives", "         Natives"), ncol = 2, nrow = 2)

#merge Herp and Hab data to plot some plots
HerpHabMat <- merge(herpdata, sites, by = "Tree_ID", all = TRUE)

HerpHabBR <- HerpHabMat %>%
  filter(forest_type == "BR")

HerpHabM <- HerpHabMat %>%
  filter(forest_type == "M")

HerpHabCY <- HerpHabMat %>%
  filter(forest_type == "CY")


#Abundance by edge dist by species
ggplot(HerpHabMat, aes(edge_category_m, fill = binomial))+
  geom_histogram()+
  ggtitle("All ASF")

ggplot(HerpHabBR, aes(edge_category_m, fill = binomial))+
  geom_histogram()+
  ggtitle("Brachystegia Forest")

ggplot(HerpHabM, aes(edge_category_m, fill = binomial))+
  geom_histogram()+
  ggtitle("Mixed Forest")

ggplot(HerpHabCY, aes(edge_category_m, fill = binomial))+
  geom_histogram()+
  ggtitle("Cynometera Forest")





##### VERTICAL DISTRIBUTIONS GRAHPS

#vertical height distribution by edge category by forest type, same sampling effort

ggplot(data = filter(HerpHabMat, survey_type == "C" | survey_type == "G", extra_ground != "Y", category != "matrix")) + geom_density(mapping = aes(x= height_found_m), alpha = 1, linetype = 1) + facet_grid(forest_type~edge_category_m, scales = "free_x") +
  theme(axis.title.y = element_text(size = rel(1.5),face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(1.5),face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(color = "black", face = "bold",  size = rel(1.3)),
        axis.text.y = element_text(color = "black", face = "bold",  size = rel(1.3)),
        strip.text.x = element_text(colour = "black", face = "italic"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white"), strip.background = element_rect(colour = "black", fill = "white")) +  coord_flip(xlim = c(0,20))+  guides(fill = guide_legend(override.aes = list(alpha = 1))) + theme(axis.text.x=element_blank(), axis.line=element_blank(), axis.ticks=element_blank()) + labs(title = "Vertical distribution by edge category by forest type (G and C surveys, no matrix)", y = "Relative density", x = "Vertical height (m)")


#vertical height distribution by forest type, same sampling effort

ggplot(data = filter(HerpHabMat, survey_type == "C" | survey_type == "G", extra_ground != "Y", category != "matrix")) + geom_density(mapping = aes(x= height_found_m), alpha = 1, linetype = 1) + facet_grid(.~forest_type, scales = "free_x") +
  theme(axis.title.y = element_text(size = rel(1.5),face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(1.5),face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(color = "black", face = "bold",  size = rel(1.3)),
        axis.text.y = element_text(color = "black", face = "bold",  size = rel(1.3)),
        strip.text.x = element_text(colour = "black", face = "italic"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white"), strip.background = element_rect(colour = "black", fill = "white")) +  coord_flip(xlim = c(0,20))+  guides(fill = guide_legend(override.aes = list(alpha = 1))) + theme(axis.text.x=element_blank(), axis.line=element_blank(), axis.ticks=element_blank()) + labs(title = "Vertical distribution by forest type (G and C surveys, no matrix)", y = "Relative density", x = "Vertical height (m)")


#vertical height distribution by edge category, same sampling effort

ggplot(data = filter(HerpHabMat, survey_type == "C" | survey_type == "G", extra_ground != "Y", category != "matrix")) + geom_density(mapping = aes(x= height_found_m), alpha = 1, linetype = 1) + facet_grid(.~edge_category_m, scales = "free_x") +
  theme(axis.title.y = element_text(size = rel(1.5),face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(1.5),face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(color = "black", face = "bold",  size = rel(1.3)),
        axis.text.y = element_text(color = "black", face = "bold",  size = rel(1.3)),
        strip.text.x = element_text(colour = "black", face = "italic"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white"), strip.background = element_rect(colour = "black", fill = "white")) +  coord_flip(xlim = c(0,20))+  guides(fill = guide_legend(override.aes = list(alpha = 1))) + theme(axis.text.x=element_blank(), axis.line=element_blank(), axis.ticks=element_blank()) + labs(title = "Vertical distribution by edge category (G and C surveys, no matrix)", y = "Relative density", x = "Vertical height (m)")



#vertical height distribution by edge category by forest type, Climber surveys only

ggplot(data = filter(HerpHabMat, survey_type == "C")) + geom_density(mapping = aes(x= height_found_m), alpha = 1, linetype = 1) + facet_grid(forest_type~edge_category_m, scales = "free_x") +
  theme(axis.title.y = element_text(size = rel(1.5),face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(1.5),face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(color = "black", face = "bold",  size = rel(1.3)),
        axis.text.y = element_text(color = "black", face = "bold",  size = rel(1.3)),
        strip.text.x = element_text(colour = "black", face = "italic"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white"), strip.background = element_rect(colour = "black", fill = "white")) +  coord_flip(xlim = c(0,20))+  guides(fill = guide_legend(override.aes = list(alpha = 1))) + theme(axis.text.x=element_blank(), axis.line=element_blank(), axis.ticks=element_blank()) + labs(title = "Vertical distribution by edge category be forest type (C surveys only)", y = "Relative density", x = "Vertical height (m)")



# HEPL
ggplot(data = filter(HerpHabMat, species_code == "HEPL", survey_type == "C" | survey_type == "G", extra_ground != "Y", category != "matrix")) + geom_density(mapping = aes(x= height_found_m), alpha = 1, linetype = 1) + facet_grid(forest_type~edge_category_m, scales = "free_x") +
  theme(axis.title.y = element_text(size = rel(1.5),face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(1.5),face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(color = "black", face = "bold",  size = rel(1.3)),
        axis.text.y = element_text(color = "black", face = "bold",  size = rel(1.3)),
        strip.text.x = element_text(colour = "black", face = "italic"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white"), strip.background = element_rect(colour = "black", fill = "white")) +  coord_flip(xlim = c(0,20))+  guides(fill = guide_legend(override.aes = list(alpha = 1))) + theme(axis.text.x=element_blank(), axis.line=element_blank(), axis.ticks=element_blank()) + labs(title = "HEPL vertical distribution by edge category by forest type (G and C surveys, no matrix)", y = "Relative density", x = "Vertical height (m)")



# HEMA
ggplot(data = filter(HerpHabMat, species_code == "HEMA", survey_type == "C" | survey_type == "G", extra_ground != "Y", category != "matrix")) + geom_density(mapping = aes(x= height_found_m), alpha = 1, linetype = 1) + facet_grid(forest_type~edge_category_m, scales = "free_x") +
  theme(axis.title.y = element_text(size = rel(1.5),face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(1.5),face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(color = "black", face = "bold",  size = rel(1.3)),
        axis.text.y = element_text(color = "black", face = "bold",  size = rel(1.3)),
        strip.text.x = element_text(colour = "black", face = "italic"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white"), strip.background = element_rect(colour = "black", fill = "white")) +  coord_flip(xlim = c(0,20))+  guides(fill = guide_legend(override.aes = list(alpha = 1))) + theme(axis.text.x=element_blank(), axis.line=element_blank(), axis.ticks=element_blank()) + labs(title = "HEMA vertical distribution by edge category by forest type (G and C surveys, no matrix)", y = "Relative density", x = "Vertical height (m)")


# LYMO
ggplot(data = filter(HerpHabMat, species_code == "LYMO", survey_type == "C" | survey_type == "G", extra_ground != "Y", category != "matrix")) + geom_density(mapping = aes(x= height_found_m), alpha = 1, linetype = 1) + facet_grid(forest_type~edge_category_m, scales = "free_x") +
  theme(axis.title.y = element_text(size = rel(1.5),face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(1.5),face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(color = "black", face = "bold",  size = rel(1.3)),
        axis.text.y = element_text(color = "black", face = "bold",  size = rel(1.3)),
        strip.text.x = element_text(colour = "black", face = "italic"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white"), strip.background = element_rect(colour = "black", fill = "white")) +  coord_flip(xlim = c(0,20))+  guides(fill = guide_legend(override.aes = list(alpha = 1))) + theme(axis.text.x=element_blank(), axis.line=element_blank(), axis.ticks=element_blank()) + labs(title = "LYMO vertical distribution by edge category by forest type (G and C surveys, no matrix)", y = "Relative density", x = "Vertical height (m)")


# CHDI
ggplot(data = filter(HerpHabMat, species_code == "CHDI", survey_type == "C" | survey_type == "G", extra_ground != "Y", category != "matrix")) + geom_density(mapping = aes(x= height_found_m), alpha = 1, linetype = 1) + facet_grid(forest_type~edge_category_m, scales = "free_x") +
  theme(axis.title.y = element_text(size = rel(1.5),face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(1.5),face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(color = "black", face = "bold",  size = rel(1.3)),
        axis.text.y = element_text(color = "black", face = "bold",  size = rel(1.3)),
        strip.text.x = element_text(colour = "black", face = "italic"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white"), strip.background = element_rect(colour = "black", fill = "white")) +  coord_flip(xlim = c(0,20))+  guides(fill = guide_legend(override.aes = list(alpha = 1))) + theme(axis.text.x=element_blank(), axis.line=element_blank(), axis.ticks=element_blank()) + labs(title = "CHDI vertical distribution by edge category by forest type (G and C surveys, no matrix)", y = "Relative density", x = "Vertical height (m)")









