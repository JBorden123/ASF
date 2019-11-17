#species by forest type plots

#libraries
library(dplyr)
library(ggplot2)
#install.packages("wesanderson")
library(wesanderson)

#the data
MetaBR <- read.csv("clean_data/MetaAllBR.csv", header = TRUE)
MetaM <- read.csv("clean_data/MetaAllM.csv", header = TRUE)
MetaCY <- read.csv("clean_data/MetaAllCY.csv", header = TRUE)

#matrix surveys
MetaBRMat <- MetaBR %>% 
  filter(edge_category_m == -10)
MetaMMat <- MetaM %>%
  filter(edge_category_m == -10)
MetaCYMat <- MetaCY %>%
  filter(edge_category_m == -10)

#remove matrix surveys
MetaBR <- MetaBR %>%
  filter(edge_category_m != -10)
MetaM <- MetaM %>%
  filter(edge_category_m != -10)
MetaCY <- MetaCY %>%
  filter(edge_category_m != -10)

#make species matrices from each forest type
BRspec <- MetaBR %>%
  select(ARST:VAAL)
Mspec <- MetaM %>%
  select(ARST:VAAL)
CYspec <- MetaCY %>%
  select(ARST:VAAL)
#and for matrix surveys
BRspecMat <- MetaBRMat %>%
  select(ARST:VAAL)
MspecMat <- MetaMMat %>%
  select(ARST:VAAL)
CYspecMat <- MetaCYMat %>%
  select(ARST:VAAL)


#Brachystegia
AbunBR <- as.data.frame(colSums(BRspec))
species <- rownames(AbunBR)
rownames(AbunBR) = NULL
AbunBR <- cbind(AbunBR, species)
names(AbunBR) <- c("abundance", "species")

ggplot(AbunBR, aes(species, abundance, color = "green"))+
  geom_col(fill = "#DBB165", colour = "#27223C")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))+
  ylim(0,55)+
  labs(title = "Brachystegia Community")

#Brachystegia Matrix
AbunBRMat <- as.data.frame(colSums(BRspecMat))
species <- rownames(AbunBRMat)
rownames(AbunBRMat) = NULL
AbunBRMat <- cbind(AbunBRMat, species)
names(AbunBRMat) <- c("abundance", "species")

ggplot(AbunBRMat, aes(species, abundance, color = "green"))+
  geom_col(fill = "#DBB165", colour = "#27223C")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))+
  ylim(0,55)+
  labs(title = "Brachystegia Matrix Community")

#Mixed Forest
AbunM <- as.data.frame(colSums(Mspec))
species <- rownames(AbunM)
rownames(AbunM) = NULL
AbunM <- cbind(AbunM, species)
names(AbunM) <- c("abundance", "species")

ggplot(AbunM, aes(species, abundance))+
  geom_col(fill = "#2E604A", colour = "#27223C")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))+
  ylim(0,55)+
  labs(title = "Mixed Forest Community")

#Mixed Forest Matrix
AbunMMat <- as.data.frame(colSums(MspecMat))
species <- rownames(AbunMMat)
rownames(AbunMMat) = NULL
AbunMMat <- cbind(AbunMMat, species)
names(AbunMMat) <- c("abundance", "species")

ggplot(AbunMMat, aes(species, abundance))+
  geom_col(fill = "#2E604A", colour = "#27223C")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))+
  ylim(0,55)+
  labs(title = "Mixed Forest Matrix Community")


#Cynometera Forest
AbunCY <- as.data.frame(colSums(CYspec))
species <- rownames(AbunCY)
rownames(AbunCY) = NULL
AbunCY <- cbind(AbunCY, species)
names(AbunCY) <- c("abundance", "species")


ggplot(AbunCY, aes(species, abundance))+
  geom_col(fill = "#D3DDDC", colour = "#27223C")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))+
  ylim(0,55)+
  labs(title = "Cynometera Forest Community")

#Cynometera Forest Matrix
AbunCYMat <- as.data.frame(colSums(CYspecMat))
species <- rownames(AbunCYMat)
rownames(AbunCYMat) = NULL
AbunCYMat <- cbind(AbunCYMat, species)
names(AbunCYMat) <- c("abundance", "species")


ggplot(AbunCYMat, aes(species, abundance))+
  geom_col(fill = "#D3DDDC", colour = "#27223C")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))+
  ylim(0,55)+
  labs(title = "Cynometera Forest Community")

