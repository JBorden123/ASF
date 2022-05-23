#alanyze biodiv metrics by edge by group types

#packages 
library(randomForest)
library(tidyverse)
library(lme4)
library(reshape2)
library(cowplot)
library(car) # for VIF testing
library(glmulti)# for automated model selection
library(MuMIn)#to dredge model
library(pscl) #for zero inflated models 
library(sjPlot) #for ploting models
library(DHARMa)

select <- dplyr::select

#Data
MetaAll <- read_csv("clean_data/MetaAll.csv")
names(MetaAll)
HerpData <- read_csv("raw_data/herpdata.csv")
SiteData <- read_csv("raw_data/sites.csv")

BioDivData <- MetaAll %>% 
  filter(extra_ground != "Y") %>% 
  select(Tree_ID, rich, abund, EffectSpecNumShan, EffectSpecNumbSimps, rich_Arb, abund_Arb,
         EffectSpecNumShan_Arb, EffectSpecNumbSimps_Arb, rich_Ter, abund_Ter, EffectSpecNumShan_Ter,
         EffectSpecNumbSimps_Ter, rich_Noct, abund_Noct, EffectSpecNumShan_Noct, EffectSpecNumbSimps_Noct,
         rich_Diur, abund_Diur, EffectSpecNumShan_Diur, EffectSpecNumbSimps_Diur, edge_category_m, edge_dist_m, forest_type,
         TotalAvgCan, AvgLeafLayer, StemLess8cm, StemMore8cm, BasalArea, Debris, forest_type, HOT_m) %>% 
  filter(edge_category_m != "NA", edge_category_m != -10) %>%  #remove extra surveys
  mutate(forest_type = as.factor(forest_type)) #change to factor for modeling


#look at the shape of our response variables and decide appropriate transformations
#or modelling distrubutions!
hist(BioDivData$abund) #count data = poisson distribution
hist(BioDivData$rich) #count data = poisson
hist(BioDivData$EffectSpecNumShan) # poisson
hist(BioDivData$abund_Arb) #poisson
hist(BioDivData$abund_Ter) #poisson
hist(BioDivData$abund_Diur) #poisson
hist(BioDivData$abund_Noct) #poisson
hist(BioDivData$rich_Arb) #poisson
hist(BioDivData$rich_Diur) #poisson
hist(BioDivData$rich_Noct) #poisson
hist(BioDivData$rich_Ter) #poisson
hist(BioDivData$EffectSpecNumShan_Arb) #poisson
hist(BioDivData$EffectSpecNumShan_Ter) #poisson
hist(BioDivData$EffectSpecNumShan_Noct)#poisson
hist(BioDivData$EffectSpecNumShan_Diur)#poisson

summary(BioDivData)
names(BioDivData)
str(BioDivData)


##########################################################################

###                    BUILD SOME MODELS

#########################################################################


##########################################################
#         Abundances
##########################################################

AbundGLMM<- glmer(abund ~
        scale(edge_category_m) +
       scale(TotalAvgCan) +
      scale(StemMore8cm)+
        scale(StemLess8cm)+
        scale(AvgLeafLayer)+
        scale(BasalArea)+
        scale(HOT_m)+
        scale(Debris)+
        (1 | forest_type), 
      data = BioDivData, family = poisson, 
      na.action = na.roughfix) # this na.action uses the median value to fill NAs.From the randomForest Package

summary(AbundGLMM)
vif(AbundGLMM) #ALL GOOD
testDispersion(AbundGLMM)
simulationOutput <- simulateResiduals(fittedModel = AbundGLMM, plot = F)
plot(simulationOutput)

plot_model(AbundGLMM, type = "est", show.values = TRUE)+
  theme_classic()+
  geom_hline(yintercept = 1, linetype = "dashed", alpha = .5)
           
#Dredge and model averaging
DredgeAbundGLMM <- dredge(AbundGLMM)
Mod1<- summary(model.avg(DredgeAbundGLMM, subset = delta <= 2)) # This compurtes model average coefficients for the
#top models using Delta AIC <=2! 

##############################################################

        ## PLOTS
##############################################################

Abund <- ggplot(BioDivData, aes(edge_category_m, abund))+
  geom_jitter(alpha = .4, height = .05, width = 6)+
  geom_smooth(method = lm, se = FALSE, color = "#7B5ABF",
              alpha = .8)+
  theme_classic()+
  labs(y = "Abundance", x = "")+
  theme(axis.text=element_text(size=17), #change font size of axis text
        axis.title=element_text(size=19))
Abund 

Rich <- ggplot(BioDivData, aes(edge_category_m, rich))+
  geom_jitter(alpha = .4, height = .05, width = 6)+
  geom_smooth(method = lm, se = FALSE, color = "#7B5ABF",
              alpha = .8)+
  theme_classic()+
  labs(y = "Richness", x = "")+
  theme(axis.text=element_text(size=17), #change font size of axis text
        axis.title=element_text(size=19))
Rich

Div <- ggplot(BioDivData, aes(edge_category_m, EffectSpecNumShan))+
  geom_jitter(alpha = .4, height = .05, width = 6)+
  geom_smooth(method = lm, se = FALSE, color = "#7B5ABF",
              alpha = .8)+
  theme_classic()+
  labs(y = "Diversity (ENS)", x = "")+
  theme(axis.text=element_text(size=17), #change font size of axis text
        axis.title=element_text(size=19))
Div

Fig3 <- cowplot::plot_grid(Abund,Rich, Div, ncol = 3, nrow = 1)
ggsave("figures/finalFigures/3Abund&DivByEdge.pdf", height = 100, width = 300, units = "mm")

#fig 4 = niche packing
SiteData$category <- if_else(SiteData$edge_category_m < 101, "Edge", "Core" )

HerpData$genus <- if_else(grepl("skink",HerpData$genus), "Trachylepis", HerpData$genus)

HerpData2 <- left_join(HerpData, SiteData, by = "Tree_ID")

HerpData3<- HerpData2 %>% 
  filter(category != "NA", edge_category_m != -10, genus != "NA", edge_category_m != 100) %>% 
  filter(genus != "Lizard")

Test <- HerpData3 %>%  #this helps me see how many trees are in each category at this point. we have 32 edge and 27 core. 
  group_by(category, Tree_ID) %>% 
  dplyr::summarise(total = n())

Edge <- HerpData3 %>% 
  filter(category == "Edge")
Core <- HerpData3 %>% 
  filter(category == "Core")

PackingPlot <- ggplot(HerpData3, aes((height_found_m/HOT_m)*100, fill = genus))+
  geom_histogram(alpha = .8, binwidth = 4, col=("white"))+
  facet_wrap(~category, ncol = 2)+
  theme_classic()+
  labs(x = "Vertical strata (% of tree height)", y = "Total abundance")+
  theme(axis.text=element_text(size=17), #change font size of axis text
        axis.title=element_text(size=19),
        title = element_text(size = 15),
        strip.text.x = element_text(size = 19))+
  coord_flip()+
 scale_y_reverse()
  
PackingPlot
ggsave("figures/finalFigures/4PackingPlot2.png",PackingPlot, height = 150, width = 180, 
       units = "mm")


 EdgePack <- ggplot(Edge, aes((height_found_m/HOT_m)*100, fill = genus))+
  geom_histogram(alpha = .8, binwidth = 4, col=("white"))+
  #facet_wrap(~category, ncol = 2)+
  theme_classic()+
  labs(x = "Vertical strata (% of tree height)", y = "Total abundance")+
  theme(axis.text=element_text(size=17), #change font size of axis text
        axis.title=element_text(size=19),
        title = element_text(size = 15),
        strip.text.x = element_text(size = 19))+
  coord_flip()+
  lims(x = c(0,100))+
  scale_y_reverse()+
  theme(legend.position = "none")

EdgePack

CorePack <- ggplot(Core, aes((height_found_m/HOT_m)*100, fill = genus))+
  geom_histogram(alpha = .8, binwidth = 4, col=("white"))+
  #facet_wrap(~category, ncol = 2)+
  theme_classic()+
  labs(x = "Vertical strata (% of tree height)", y = "Total abundance")+
  theme(axis.text=element_text(size=17), #change font size of axis text
        axis.title=element_text(size=19),
        title = element_text(size = 15),
        strip.text.x = element_text(size = 19))+
  lims(x = c(0,100))+
  theme(legend.position = "none")+
  coord_flip()

CorePack

ggsave("figures/finalFigures/4aEdgePack.pdf", EdgePack, height = 200, width = 125, units = "mm")
ggsave("figures/finalFigures/4bCorePack.pdf", CorePack, height = 200, width = 125, units = "mm")


#arboreal abundance
ArbAbundGLMM<- glmer(abund_Arb ~
                    scale(edge_category_m) +
                    scale(TotalAvgCan) +
                    scale(StemMore8cm)+
                    scale(StemLess8cm)+
                    scale(AvgLeafLayer)+
                    scale(BasalArea)+
                    scale(HOT_m)+
                      scale(Debris)+
                    (1 | forest_type), 
                  data = BioDivData, family = poisson, 
                  na.action = na.roughfix) # this na.action uses the median value to fill NAs.From the randomForest Package)

summary(ArbAbundGLMM)
vif(ArbAbundGLMM) #all good

#Dredge and model averaging
DredgeArbAbundGLMM <- dredge(ArbAbundGLMM)
summary(model.avg(DredgeArbAbundGLMM, subset = delta <= 2)) # This compurtes model average coefficients for the
#top models using Delta AIC <=2! 

plot_model

#terrestrial abundance
TerAbundGLMM<- glmer(abund_Ter ~
                    scale(edge_category_m) +
                    scale(TotalAvgCan) +
                    scale(StemMore8cm)+
                    scale(StemLess8cm)+
                    scale(AvgLeafLayer)+
                    scale(BasalArea)+
                    scale(HOT_m)+
                      scale(Debris)+
                    (1 | forest_type), 
                  data = BioDivData, family = poisson, 
                  na.action = na.roughfix) # this na.action uses the median value to fill NAs.From the randomForest Package)

summary(TerAbundGLMM)
vif(TerAbundGLMM) #all good < 3

#Dredge and model averaging
DredgeTerAbundGLMM <- dredge(TerAbundGLMM)
summary(model.avg(DredgeTerAbundGLMM, subset = delta <= 2)) # This compurtes model average coefficients for the
#top models using Delta AIC <=2! 


#nocturnal abundance
NoctAbundGLMM<- glmer(abund_Noct ~
                    scale(edge_category_m) +
                    scale(TotalAvgCan) +
                    scale(StemMore8cm)+
                    scale(StemLess8cm)+
                    scale(AvgLeafLayer)+
                    scale(BasalArea)+
                    scale(HOT_m)+
                      scale(Debris)+
                    (1 | forest_type), 
                  data = BioDivData, family = poisson, 
                  na.action = na.roughfix) # this na.action uses the median value to fill NAs.From the randomForest Package)

summary(NoctAbundGLMM)
vif(NoctAbundGLMM) #all good <2

#Dredge and model averaging
DredgeNoctAbundGLMM <- dredge(NoctAbundGLMM)
summary(model.avg(DredgeNoctAbundGLMM, subset = delta <= 2)) # This compurtes model average coefficients for the
#top models using Delta AIC <=2! 

#diurnal abundance
DiurAbundGLMM<- glmer(abund_Diur ~
                    scale(edge_category_m) +
                    scale(TotalAvgCan) +
                    scale(StemMore8cm)+
                    scale(StemLess8cm)+
                    scale(AvgLeafLayer)+
                    scale(BasalArea)+
                    scale(HOT_m)+
                      scale(Debris)+
                    (1 | forest_type), 
                  data = BioDivData, family = poisson, 
                  na.action = na.roughfix) # this na.action uses the median value to fill NAs.From the randomForest Package)

summary(DiurAbundGLMM)
vif(DiurAbundGLMM) # all good <2

#Dredge and model averaging
DredgeDiurAbundGLMM <- dredge(DiurAbundGLMM)
summary(model.avg(DredgeDiurAbundGLMM, subset = delta <= 2)) # This compurtes model average coefficients for the
#top models using Delta AIC <=2! 


##########################################################
#               RICHNESS
##########################################################

#all richness
RichGLMM<- glmer(rich ~
                    scale(edge_category_m) +
                    scale(TotalAvgCan) +
                    scale(StemMore8cm)+
                    scale(StemLess8cm)+
                    scale(AvgLeafLayer)+
                    scale(BasalArea)+
                    scale(HOT_m)+
                   scale(Debris)+
                    (1 | forest_type), 
                  data = BioDivData, family = poisson, 
                 na.action = na.roughfix) # this na.action uses the median value to fill NAs.From the randomForest Package)

summary(RichGLMM)
vif(RichGLMM) #good
testDispersion(RichGLMM)
simulationOutput <- simulateResiduals(fittedModel = RichGLMM, plot = F)

plot_model(RichGLMM, type = "est", show.values = TRUE)+
  theme_classic()+
  geom_hline(yintercept = 1, linetype = "dashed", alpha = .5)

#Dredge and model averaging
DredgeRichGLMM <- dredge(RichGLMM)
summary(model.avg(DredgeRichGLMM, subset = delta <= 2)) # This compurtes model average coefficients for the
#top models using Delta AIC <=2! 

#arboreal richness
ArbRichGLMM<- glmer(rich_Arb ~
                   scale(edge_category_m) +
                   scale(TotalAvgCan) +
                   scale(StemMore8cm)+
                   scale(StemLess8cm)+
                   scale(AvgLeafLayer)+
                   scale(BasalArea)+
                   scale(HOT_m)+
                     scale(Debris)+
                   (1 | forest_type), 
                 data = BioDivData, family = poisson, 
                 na.action = na.roughfix) # this na.action uses the median value to fill NAs.From the randomForest Package)

summary(ArbRichGLMM)
vif(ArbRichGLMM)#good

#Dredge and model averaging
DredgeArbRichGLMM <- dredge(ArbRichGLMM)
summary(model.avg(DredgeArbRichGLMM, subset = delta <= 2)) # This compurtes model average coefficients for the
#top models using Delta AIC <=2! 

#Terrestrial richness
TerRichGLMM<- glmer(rich_Ter ~
                      scale(edge_category_m) +
                      scale(TotalAvgCan) +
                      scale(StemMore8cm)+
                      scale(StemLess8cm)+
                      scale(AvgLeafLayer)+
                      scale(BasalArea)+
                      scale(HOT_m)+
                      scale(Debris)+
                      (1 | forest_type), 
                    data = BioDivData, family = poisson, 
                    na.action = na.roughfix) # this na.action uses the median value to fill NAs.From the randomForest Package)

summary(TerRichGLMM)
vif(TerRichGLMM) #Good

#Dredge and model averaging
DredgeTerRichGLMM <- dredge(TerRichGLMM)
summary(model.avg(DredgeTerRichGLMM, subset = delta <= 2)) # This compurtes model average coefficients for the
#top models using Delta AIC <=2! 


#Nocturnal richness
NoctRichGLMM<- glmer(rich_Noct ~
                      scale(edge_category_m) +
                      scale(TotalAvgCan) +
                      scale(StemMore8cm)+
                      scale(StemLess8cm)+
                      scale(AvgLeafLayer)+
                      scale(BasalArea)+
                      scale(HOT_m)+
                      scale(Debris)+
                      (1 | forest_type), 
                    data = BioDivData, family = poisson, 
                    na.action = na.roughfix) # this na.action uses the median value to fill NAs.From the randomForest Package)

summary(NoctRichGLMM)
vif(NoctRichGLMM) #good

#Dredge and model averaging
DredgeNoctRichGLMM <- dredge(NoctRichGLMM)
summary(model.avg(DredgeNoctRichGLMM, subset = delta <= 2)) # This compurtes model average coefficients for the
#top models using Delta AIC <=2! 

#Diurnal richness
DiurRichGLMM<- glmer(rich_Diur ~
                      scale(edge_category_m) +
                      scale(TotalAvgCan) +
                      scale(StemMore8cm)+
                      scale(StemLess8cm)+
                      scale(AvgLeafLayer)+
                      scale(BasalArea)+
                      scale(HOT_m)+
                      scale(Debris)+
                      (1 | forest_type), 
                    data = BioDivData, family = poisson, 
                    na.action = na.roughfix) # this na.action uses the median value to fill NAs.From the randomForest Package)

summary(DiurRichGLMM)
vif(DiurRichGLMM) #good

#Dredge and model averaging
DredgeDiurRichGLMM <- dredge(DiurRichGLMM)
summary(model.avg(DredgeDiurRichGLMM, subset = delta <= 2)) # This compurtes model average coefficients for the
#top models using Delta AIC <=2! 



##########################################################
#           EFFECTIVE SPECIES NUMBER
##########################################################

#all ESN ([E]ffective [S]pecies [N]umber)
ESN_GLMM<- glmer(EffectSpecNumShan ~
                   scale(edge_category_m) +
                   #scale(TotalAvgCan) +
                   scale(StemMore8cm)+
                   scale(StemLess8cm)+
                   scale(AvgLeafLayer)+
                   scale(BasalArea)+
                   scale(HOT_m)+
                   scale(Debris)+
                   (1 | forest_type), 
                 data = BioDivData, family = Gamma(link = "identity"), 
                 na.action = na.roughfix) # this na.action uses the median value to fill NAs.From the randomForest Package)

summary(ESN_GLMM)
vif(ESN_GLMM)
plot_model(ESN_GLMM, type = "pred")

#Dredge and model averaging
DredgeESN_GLMM <- dredge(ESN_GLMM)
summary(model.avg(DredgeESN_GLMM, subset = delta <= 2)) # This compurtes model average coefficients for the
#top models using Delta AIC <=2! 

testDispersion(ESN_GLMM)
simulationOutput <- simulateResiduals(fittedModel = ESN_GLMM, plot = F)
plot(simulationOutput)


#Arb ESN ([E]ffective [S]pecies [N]umber)
ArbESN_GLMM<- glmer(EffectSpecNumShan_Arb ~
                   scale(edge_category_m) +
                   scale(TotalAvgCan) +
                   scale(StemMore8cm)+
                   scale(StemLess8cm)+
                   scale(AvgLeafLayer)+
                   scale(BasalArea)+
                   scale(HOT_m)+
                   scale(Debris)+
                   (1 | forest_type), 
                 data = BioDivData, family = Gamma(link = "identity"), 
                 na.action = na.roughfix) # this na.action uses the median value to fill NAs.From the randomForest Package)

summary(ArbESN_GLMM)
vif(ArbESN_GLMM) #good

#Dredge and model averaging
DredgeArbESN_GLMM <- dredge(ArbESN_GLMM)
summary(model.avg(DredgeArbESN_GLMM, subset = delta <= 2)) # This computes model average coefficients for the

#top models using Delta AIC <=2! 
testDispersion(ArbESN_GLMM)
simulationOutput <- simulateResiduals(fittedModel = ArbESN_GLMM, plot = F)
plot(simulationOutput)

#Terrestrial ESN ([E]ffective [S]pecies [N]umber)
TerESN_GLMM<- glmer(EffectSpecNumShan_Ter ~
                      scale(edge_category_m) +
                      scale(TotalAvgCan) +
                      scale(StemMore8cm)+
                      scale(StemLess8cm)+
                      scale(AvgLeafLayer)+
                      scale(BasalArea)+
                      scale(HOT_m)+
                      scale(Debris)+
                      (1 | forest_type), 
                    data = BioDivData, family = Gamma(link = "identity"), 
                    na.action = na.roughfix) # this na.action uses the median value to fill NAs.From the randomForest Package)

summary(TerESN_GLMM)
vif(TerESN_GLMM) #good

#Dredge and model averaging
DredgeTerESN_GLMM <- dredge(TerESN_GLMM)
summary(model.avg(DredgeTerESN_GLMM, subset = delta <= 2)) # This computes model average coefficients for the
#top models using Delta AIC <=2! 


#Nocternal ESN ([E]ffective [S]pecies [N]umber)
NoctESN_GLMM<- glmer(EffectSpecNumShan_Noct ~
                      scale(edge_category_m) +
                      scale(TotalAvgCan) +
                      scale(StemMore8cm)+
                      scale(StemLess8cm)+
                      scale(AvgLeafLayer)+
                      scale(BasalArea)+
                      scale(HOT_m)+
                      scale(Debris)+
                      (1 | forest_type), 
                    data = BioDivData, family = Gamma(link = "identity"), 
                    na.action = na.roughfix) # this na.action uses the median value to fill NAs.From the randomForest Package)

summary(NoctESN_GLMM)
vif(NoctESN_GLMM) #good

#Dredge and model averaging
DredgeNoctESN_GLMM <- dredge(NoctESN_GLMM)
summary(model.avg(DredgeNoctESN_GLMM, subset = delta <= 2)) # This computes model average coefficients for the
#top models using Delta AIC <=2!

#Diurnal ESN ([E]ffective [S]pecies [N]umber)
DiurESN_GLMM<- glmer(EffectSpecNumShan_Diur ~
                      scale(edge_category_m) +
                      scale(TotalAvgCan) +
                      scale(StemMore8cm)+
                      scale(StemLess8cm)+
                      scale(AvgLeafLayer)+
                      scale(BasalArea)+
                      scale(HOT_m)+
                      scale(Debris)+
                      (1 | forest_type), 
                    data = BioDivData, family = Gamma(link = "identity"), 
                    na.action = na.roughfix) # this na.action uses the median value to fill NAs.From the randomForest Package)

summary(DiurESN_GLMM)
vif(DiurESN_GLMM) #good

#Dredge and model averaging
DredgeDiurESN_GLMM <- dredge(DiurESN_GLMM)
summary(model.avg(DredgeDiurESN_GLMM, subset = delta <= 2)) # This computes model average coefficients for the
#top models using Delta AIC <=2! 