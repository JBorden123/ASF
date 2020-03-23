

## script to see if height differences depending individuals characteristices : size, sex, etc.





###charge_packages###
library(randomcoloR)
library(tidyverse)
library(ggplot2)
library(vegan)
library(dplyr)
library(forcats)
library(questionr)
library(reshape)
library(reshape2)
library(xls)
library(xlsx)
library(readxl)
library(readr)
library(cowplot)
library(gridExtra)
library(chron)
library(FactoMineR)
library(factoextra)
library(missMDA)
library(corrplot)
library(data.table)
library(ggpubr)
library(tidyr)
library(rstatix)
library(coin)
library(multcomp)
library(colorspace)




#### CHARGE DATA
data <- read.csv("clean_data/MetaAll.csv", header = TRUE)
animals <- read.csv("raw_data/herpdata.csv", header = TRUE)

# put metadata in animals
animals <- merge(animals, data,intersect = c("Tree_ID", "Tree_ID"), all.x = TRUE, all.y = FALSE)





### SELECT CLEAN/USEFUL DATA

#keep only reptiles
lizards <- animals %>% filter(amph_rept == "R")

#remove snakes
lizards <- lizards %>% filter(SVL_cm < 17)

#remove Varanus (too large, not comparable to others)
lizards <- lizards %>% filter(genus != "Varanus")


##### keep known geuns/species

# remove unknown genus
lizards_ID <- lizards %>% filter(genus != "Gecko")
lizards_ID <- lizards_ID %>% filter(genus != "Lizard")
lizards_ID <- lizards_ID %>% filter(genus != "skink sp.")


# keep genus with good sample size
lizards_Id_good_sample_genus <- lizards_ID %>% filter(genus != "Gastropholis", genus != "Latastia", genus != "Mochlus")

# remove low sample size at species level
a <- lizards_Id_good_sample_genus %>% group_by(species_code) %>% tally()

lizards_Id_good_sample_species <- lizards_Id_good_sample_genus %>% filter(species_code != "HEAN", species_code != "HEBA", species_code != "TRMA", species_code != "TRPL", species_code != "TRVA", species_code != "NA" )

#remove NUBO (because only on ground)
lizards_Id_good_sample_species <- lizards_Id_good_sample_species %>% filter(species_code != "NUBO")








######################## GRAPHS


# CHDI
a <- ggplot(data = filter(lizards_Id_good_sample_species, species_code == "CHDI")) + geom_point(mapping = aes(x = SVL_cm, y = height_found_m))  + geom_smooth(mapping = aes(x = SVL_cm, y = height_found_m), method = "lm") + labs(title = "Height VS SVL, CHDI", x = "SVL (cm)", y = "Height (m)") + theme_bw(base_size = 23)
data_frame <- filter(lizards_Id_good_sample_species, species_code == "CHDI")
lm_eqn <- function(data_frame){
  m <- lm(height_found_m ~ SVL_cm, data_frame);
  eq <- substitute(italic(r)~"="~rvalue*","~italic(p)~"="~pvalue, list(rvalue = sprintf("%.2f",sign(coef(m)[2])*sqrt(summary(m)$r.squared)), pvalue = format(summary(m)$coefficients[2,4], digits = 2)))
  as.character(as.expression(eq));                 
}
a <- a + geom_text(x = 10, y = 10, label = lm_eqn(data_frame), parse = TRUE, size = 8)
a
graph1 <- a

# HEMA
a <- ggplot(data = filter(lizards_Id_good_sample_species, species_code == "HEMA")) + geom_point(mapping = aes(x = SVL_cm, y = height_found_m))  + geom_smooth(mapping = aes(x = SVL_cm, y = height_found_m), method = "lm") + labs(title = "Height VS SVL, HEMA", x = "SVL (cm)", y = "Height (m)") + theme_bw(base_size = 23)
data_frame <- filter(lizards_Id_good_sample_species, species_code == "HEMA")
lm_eqn <- function(data_frame){
  m <- lm(height_found_m ~ SVL_cm, data_frame);
  eq <- substitute(italic(r)~"="~rvalue*","~italic(p)~"="~pvalue, list(rvalue = sprintf("%.2f",sign(coef(m)[2])*sqrt(summary(m)$r.squared)), pvalue = format(summary(m)$coefficients[2,4], digits = 2)))
  as.character(as.expression(eq));                 
}
a <- a + geom_text(x = 4, y = 5, label = lm_eqn(data_frame), parse = TRUE, size = 8)
a
graph2 <- a

# HEMI
a <- ggplot(data = filter(lizards_Id_good_sample_species, species_code == "HEMI")) + geom_point(mapping = aes(x = SVL_cm, y = height_found_m))  + geom_smooth(mapping = aes(x = SVL_cm, y = height_found_m), method = "lm") + labs(title = "Height VS SVL, HEMI", x = "SVL (cm)", y = "Height (m)") + theme_bw(base_size = 23)
data_frame <- filter(lizards_Id_good_sample_species, species_code == "HEMI")
lm_eqn <- function(data_frame){
  m <- lm(height_found_m ~ SVL_cm, data_frame);
  eq <- substitute(italic(r)~"="~rvalue*","~italic(p)~"="~pvalue, list(rvalue = sprintf("%.2f",sign(coef(m)[2])*sqrt(summary(m)$r.squared)), pvalue = format(summary(m)$coefficients[2,4], digits = 2)))
  as.character(as.expression(eq));                 
}
a <- a + geom_text(x = 5, y = 5, label = lm_eqn(data_frame), parse = TRUE, size = 8)
a
graph3 <- a

# HEPL
a <- ggplot(data = filter(lizards_Id_good_sample_species, species_code == "HEPL")) + geom_point(mapping = aes(x = SVL_cm, y = height_found_m))  + geom_smooth(mapping = aes(x = SVL_cm, y = height_found_m), method = "lm") + labs(title = "Height VS SVL, HEPL", x = "SVL (cm)", y = "Height (m)") + theme_bw(base_size = 23)
data_frame <- filter(lizards_Id_good_sample_species, species_code == "HEPL")
lm_eqn <- function(data_frame){
  m <- lm(height_found_m ~ SVL_cm, data_frame);
  eq <- substitute(italic(r)~"="~rvalue*","~italic(p)~"="~pvalue, list(rvalue = sprintf("%.2f",sign(coef(m)[2])*sqrt(summary(m)$r.squared)), pvalue = format(summary(m)$coefficients[2,4], digits = 2)))
  as.character(as.expression(eq));                 
}
a <- a + geom_text(x = 5, y = 12.5, label = lm_eqn(data_frame), parse = TRUE, size = 8)
a
graph4 <- a

# LYMO
a <- ggplot(data = filter(lizards_Id_good_sample_species, species_code == "LYMO")) + geom_point(mapping = aes(x = SVL_cm, y = height_found_m))  + geom_smooth(mapping = aes(x = SVL_cm, y = height_found_m), method = "lm") + labs(title = "Height VS SVL, LYMO", x = "SVL (cm)", y = "Height (m)") + theme_bw(base_size = 23)
data_frame <- filter(lizards_Id_good_sample_species, species_code == "LYMO")
lm_eqn <- function(data_frame){
  m <- lm(height_found_m ~ SVL_cm, data_frame);
  eq <- substitute(italic(r)~"="~rvalue*","~italic(p)~"="~pvalue, list(rvalue = sprintf("%.2f",sign(coef(m)[2])*sqrt(summary(m)$r.squared)), pvalue = format(summary(m)$coefficients[2,4], digits = 2)))
  as.character(as.expression(eq));                 
}
a <- a + geom_text(x = 3, y = 8, label = lm_eqn(data_frame), parse = TRUE, size = 8)
a
graph5 <- a


# all animals < 20 cm
a <- ggplot(data = lizards, mapping = aes(x = SVL_cm, y = height_found_m)) + geom_point() + geom_smooth(method = "lm") + labs(title = "Height VS SVL, lizards/geckos", x = "SVL (cm)", y = "Height (m)") + theme_bw(base_size = 23)
data_frame <- lizards
lm_eqn <- function(data_frame){
  m <- lm(height_found_m ~ SVL_cm, data_frame);
  eq <- substitute(italic(r)~"="~rvalue*","~italic(p)~"="~pvalue, list(rvalue = sprintf("%.2f",sign(coef(m)[2])*sqrt(summary(m)$r.squared)), pvalue = format(summary(m)$coefficients[2,4], digits = 2)))
  as.character(as.expression(eq));                 
}
a <- a + geom_text(x = 10, y = 12.5, label = lm_eqn(data_frame), parse = TRUE, size = 8)
a
graph0 <- a



png("figures/height_VS_SLV.png", width = 1000, height = 1200)
plot_grid(graph0, graph1, graph2, graph3, graph4, graph5, ncol=2, labels=c("A", "B", "C", "D", "E", "F"), label_size = 23)
dev.off()



      


