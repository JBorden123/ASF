# Height Analysis

library(tidyverse)
library(performance)#for checking VIF of glmmTMB models
library(cowplot)
library(randomForest)
library(lme4)
library(car) # for VIF testing
library(glmulti)# for automated model selection
library(MuMIn)#to dredge model
library(glmmTMB)
library(lmerTest)
library(DHARMa)
library(ggeffects)
library(piecewiseSEM) #for structural equation modeling (SEM)
library(sjPlot) #for ploting SEMs
library(effects) #to plot effects of GLMS
library(car) #for VIF


#data exploring
#THE DATA
herpdata <- read_csv("raw_data/herpdata.csv")
metadata <- read_csv("clean_data/MetaAll.csv")
sites <- read_csv("raw_data/sites.csv")
habData <- read_csv("clean_data/HabSummary.csv")
BioDivData <- read_csv("clean_data/biodiv_data.csv")
BioDivData <- BioDivData %>% 
  dplyr::select(Tree_ID, EffectSpecNumShan, abund, rich)
names(BioDivData)

#historgram of heights of all species colored by species
ggplot(herpdata, aes(height_found_m_rec, fill = species_code))+
  geom_histogram()


metadata2 <- metadata %>% 
  filter(edge_category_m != "NA", edge_category_m != -10, extra_ground != "Y", med_hght != "NA") %>% 
  mutate(forest_type = as.factor(forest_type)) %>% 
  filter(HOT_m != "NA")

#height by edge dist
HghtPlot1 <- ggplot(metadata2, aes(edge_category_m, med_hght))+
  geom_smooth(method = lm, color = "black", se = FALSE)+
  geom_jitter(alpha = .5)+
  theme_classic()+
  labs(y = "Median community perch height (m)", x = "Approx. distance from forest edge")+
  theme(axis.text=element_text(size=17), #change font size of axis text
        axis.title=element_text(size=19),
        title = element_text(size = 15))

HghtPlot1  


#height by percentage of tree by edge dist
HghgtPlot2 <- ggplot(metadata2, aes(edge_category_m, MedHghtPerc*100))+
  geom_smooth(method = lm, color = "black", se = FALSE)+
  geom_jitter(alpha = .5)+
  theme_classic()+
  labs(y = "Perch height (% of tree hght.)", x = "Approx. distance from forest edge")+
  theme(axis.text=element_text(size=17), #change font size of axis text
        axis.title=element_text(size=19),
        title = element_text(size = 15))


hist((metadata$med_hght))
summary(metadata2)


####################################################
#arboreal species data
ArbSpec <- herpdata %>% 
  filter(ArbVsTerr == "arboreal")
ArbSpec <- left_join(ArbSpec,sites, by = "Tree_ID") %>% 
  mutate(HeightFracTree = (height_found_m/HOT_m))
ArbSpec<- left_join(ArbSpec, habData, by = "Tree_ID") %>% 
  mutate(forest_type = as.factor(forest_type), Tree_ID = as.factor(Tree_ID)) %>% 
  filter(HeightFracTree != "NA")

ArbSpec <- left_join(ArbSpec, BioDivData)

ArbSpec$category <- factor(ArbSpec$category, levels = c("edge", "core"))
# a transformation function to get rid of 0s and 1s in fraction/proportional data
# so that we can use a beta distribution GLMM
transform01 <- function(x) {
  (x * (length(x) - 1) + 0.5) / (length(x))
}
#and the reverse in case you need it for plotting:
backtransform01 <- function(x) {
  (x * length(x) - 0.5) / (length(x) - 1)
}  

#transform the fraction of tree metric to have NO 0s or 1s 
#to be able use in beta distributed model (which is appropriate for proportional data)
ArbSpec$HeightFracTree <- transform01((ArbSpec$height_found_m/ArbSpec$HOT_m))



#PLOT arboreal height shifts as community and again at species level
ArbSpec2 <- ArbSpec %>% 
  filter(species != "sp.")

ggplot(data = ArbSpec, aes(edge_category_m, HeightFracTree*100, color = binomial)) +
  geom_jitter(height = .1, width = 6, alpha = 0.5) +
  geom_smooth(method = lm, alpha = .1) +
  theme_classic() +
  #facet_wrap(~NoctVsDiurn)+
  lims(y = c(0,100))+
  labs(x = "Approx. distance from forest edge (m)", y = "Perch height (% of tree)", color = "Species")+
  theme(axis.text=element_text(size=17), #change font size of axis text
        axis.title=element_text(size=19),
        title = element_text(size = 15),
        strip.text.x = element_text(size = 19))

ggplot(data = ArbSpec2, aes(abund, height_found_m, color = binomial)) +
         geom_point(alpha = 0.5) +
         geom_smooth(method = lm, alpha = .1) +
  theme_classic() +
  #facet_wrap(~NoctVsDiurn)+
  lims(y = c(0,10))



#mixed effect models for arboreal community height

#dividing by nocturnal vs diurnal (Charge one object- run code- then charge the other and rerun)
#ArbData <- ArbSpec %>% 
#  filter(NoctVsDiurn == "diurnal")
#ArbData <- ArbSpec %>% 
#  filter(NoctVsDiurn == "nocturnal")

#height GLMM for Individual animals using fraction of tree height.
HghtGLMM<- glmmTMB(HeightFracTree ~
                    scale(edge_category_m) +
                     scale(TotalAvgCan) + #not really related to vertical structureing
                    scale(StemMore8cm) + #not any different than basal area 
                    scale(StemLess8cm) + #understory veg structure
                    scale(AvgLeafLayer) + #vertical structure
                    scale(BasalArea) + #vertical structure
                    scale(Debris) +#low microhabitat
                    #scale(abund) +
                    # scale(EffectSpecNumShan) +
                    # (1 | Tree_ID)+ #so few individuals for tree that it seems justified to leave this random effect out
                     (1 | binomial)+
                   (1 | forest_type), 
                   data = ArbSpec, family = "beta_family")
summary(HghtGLMM)
check_collinearity(HghtGLMM)

#model diagnostics using DHARMa

testDispersion(HghtGLMM)
simulationOutput <- simulateResiduals(fittedModel = HghtGLMM, plot = F)
plot(simulationOutput)


HghtModPlot <- plot_model(HghtGLMM, type = "est", show.values = TRUE)

HghtModPlot2 <- HghtModPlot+
  theme_classic()+
  theme(axis.text=element_text(size=17), #change font size of axis text
        axis.title=element_text(size=19),
        title = element_text(size = 15),
        axis.text.y = element_text(angle = 45))+
  lims(y = c(0.5,1.5))+
  scale_x_discrete(labels = c("debris", "basal area", "canopy leaf layer", "small woody stems", 
                              "large woody stems", "% canopy cover", "dist. to edge"))+
  geom_hline(yintercept = 1, linetype = "dashed", alpha = .5)+
  labs(title = "", y = "")
                                    

HghtModPlot2

Fig4 <- cowplot::plot_grid(HghgtPlot2, HghtModPlot2, ncol = 2, nrow = 1, 
                   rel_widths = c(2,2), labels = c("a)", "b)"))

ggsave("figures/finalFigures/4HhgtbyEdge&HghtGLMER.pdf", width = 250, height = 120, units = "mm")
  

################################################

#                  NICHE BREADTH

#################################################
ArbSpec3 <- ArbSpec %>% 
  filter(category != "NA") %>% 
  filter(species != "sp.") %>% 
  filter(genus != "Cordylus", genus != "Dispholidus", genus != "Gastropholis",
         genus != "Varanus", species != "maculilabris")

NicheBreadth <- ArbSpec %>% 
  filter(category != "NA", binomial != "NA", species != "sp.") %>%
  filter( genus != "Cordylus", genus != "Dispholidus", genus != "Gastropholis",
          genus != "Varanus", species != "maculilabris") %>% 
  group_by( genus, binomial, category, edge_category_m) %>% 
  summarise(HghtRange = (max(height_found_m)- min(height_found_m))) 
  
  #filter(edge_category_m != 100)

  
EdgeCoreHghts <- ggplot(ArbSpec3, aes(category, HeightFracTree*100, fill = binomial, color = binomial,
                    na.action(na.omit)))+
  geom_jitter(alpha = .8, width = .1, height = 0)+
  geom_boxplot(alpha = .5)+
  #geom_smooth(method = lm, se = FALSE)+
  facet_wrap(~species_code, ncol = 6, nrow = 1)+
  lims(y = c(0,100))+
  theme_classic()+
  labs(y = "Perch height (% of tree)", x = "")+
  theme(axis.text=element_text(size=13), #change font size of axis text
        axis.title=element_text(size=15),
        title = element_text(size = 13),
        strip.text.x = element_text(size = 12),
        legend.position = "none",
        axis.text.x = element_blank())  
  

EdgeCoreHghts
ArbSpec <- ArbSpec %>% 
  filter(category != "NA")

EdgeCoreHghtsAll <- ggplot(ArbSpec, aes(category, HeightFracTree*100,
                                         na.action(na.omit)))+
  geom_jitter(alpha = .5, width = .1, height = 0)+
  geom_boxplot(alpha = .5, fill = "grey")+
  #geom_smooth(method = lm, se = FALSE)+
  #facet_wrap(~species_code, ncol = 6, nrow = 1)+
  lims(y = c(0,100))+
  theme_classic()+
  labs(y = "", x = "", title = "All")+
  theme(axis.text=element_text(size=17), #change font size of axis text
        axis.title=element_text(size=15),
        title = element_text(size = 13),
        strip.text.x = element_text(size = 12),
        legend.position = "none",
        axis.text.x = element_blank())  
EdgeCoreHghtsAll

EdgeCoreRanges<- ggplot(NicheBreadth, aes(category,HghtRange, fill = binomial, color = binomial))+
  geom_jitter(alpha = .8)+
  geom_boxplot(alpha = .5)+
  #geom_smooth(method = lm, size = 1, alpha = .5, se = FALSE)+
  theme_classic()+
 facet_wrap(~binomial, ncol = 6, nrow = 1)+
  lims(y = c(0,20))+
  labs(y = "Vertical niche breadth (m)", x = "")+
  theme(axis.text=element_text(size=13), #change font size of axis text
        axis.title=element_text(size=14),
        title = element_text(size = 13),
        strip.text.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom")

EdgeCoreRanges

EdgeCoreRangesAll <- ggplot(NicheBreadth, aes(category,HghtRange))+
  geom_jitter(alpha = .5, width = .1, height = 0)+
  geom_boxplot(alpha = .5, fill = "grey")+
  #geom_smooth(method = lm, se = FALSE)+
  #facet_wrap(~species_code, ncol = 6, nrow = 1)+
  lims(y = c(0,20))+
  theme_classic()+
  labs(y = "", x = "")+
  theme(axis.text=element_text(size=13), #change font size of axis text
        axis.title=element_text(size=15),
        title = element_text(size = 13),
        strip.text.x = element_text(size = 12),
        legend.position = "none",
        axis.text.x = element_blank())  

EdgeCoreRangesAll

EdgeCoreHghtsBoth <- cowplot::plot_grid(EdgeCoreHghts, EdgeCoreHghtsAll, ncol = 2, nrow = 1,
                   rel_widths = c(4,1.5))

EdgeCoreRangesBoth<- cowplot::plot_grid(EdgeCoreRanges, EdgeCoreRangesAll, ncol = 2, nrow = 1, 
                           rel_widths = c(4,1.5))

ggsave("figures/finalFigures/6aEdgeCoreHghts.pdf", EdgeCoreHghts, height = 100, width = 200, units = "mm")
ggsave("figures/finalFigures/6bEdgeCoreHghts.pdf", EdgeCoreHghtsAll, height = 150, width = 100, units = "mm")

ggsave("figures/finalFigures/6cEdgeCoreHghts.pdf", EdgeCoreRanges, height = 100, width = 200, units = "mm")
ggsave("figures/finalFigures/6dEdgeCoreHghts.pdf", EdgeCoreRangesAll, height = 150, width = 100, units = "mm")



ggsave("figures/finalFigures/6EdgeCoreHghts&Ranges.pdf",
       height = 300, width = 320, units = "mm")

summary(lm(HghtRange~
  edge_category_m,
  #  (1|binomial),
     data = NicheBreadth))

hist((NicheBreadth$HghtRange))


################################################

#                  Structural Equation Model!

#################################################
str(ArbSpec)


#SEM for height with edge dist -> abundance -> perch height (and one direct effect from edge distance to perch height)
ModelList <- psem(glmer(abund ~ scale(edge_category_m) +
                        (1 | forest_type),
                      family = poisson, data = ArbSpec),
                  
                  glmmTMB(HeightFracTree ~
                            scale(edge_category_m)+
                            scale(abund)+
                          (1 | binomial)+
                            (1 | forest_type), 
                          data = ArbSpec, family = "beta_family"))


summary(ModelList)








#################################################################

#################################################################

#################################################################
# NO LONGER NEEDED

#################################################################

#mixed effect models for arboreal community height
metadata2$MedHghtPerc <- transform01(metadata2$MedHghtPerc)
#mixed model of fraction of tree height
str(metadata2)

MedHghtPercGLMM<- glmmTMB(MedHghtPerc ~
                            scale(edge_category_m) +
                            #scale(TotalAvgCan) + #not really related to vertical structureing
                            #scale(StemMore8cm) +
                            scale(StemLess8cm) + #understory veg structure
                            scale(AvgLeafLayer) + #vertical structure
                            scale(BasalArea) + #vertical structure
                            scale(Debris) + #low microhabitat
                            #(1 | Tree_ID), #so few individuals for tree that it seems justified to leave this random effect out
                            #(1 | binomial) +
                            (1 | forest_type), , 
                          data = metadata2, family = beta_family)

summary(MedHghtPercGLMM)
vif(MedHghtPercGLMM) #good

#Dredge and model averaging
DredgeMedHghtPercGLMM <- dredge(MedHghtPercGLMM)
summary(model.avg(DredgeMedHghtPercGLMM, subset = delta <= 2)) # This computes model average coefficients for the
#top models using Delta AIC <=2! 





