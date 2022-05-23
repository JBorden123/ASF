# Clean summerize and create data from logger data with summary for strata and times and 
# comparison between climber and fixed loggers


#library
library(tidyverse)
library(lubridate)
library(MuMIn)
library(lme4)
library(randomForest)
library(car)
library(gam)
library(lmerTest)
library(sjPlot)
library(cowplot)
library(geomtextpath)

??plot_model
 
select <- dplyr::select


#### CHARGE DATA
ClimbLoggerData <- read.csv("clean_data/clean_climb_logger_data.csv", header = TRUE)
FixedLoggerData <- read.csv("clean_data/FixedLogData.csv", header = TRUE)
SiteData <- read_csv("raw_data/sites.csv")
HabData <- read_csv("clean_data/HabSummary.csv")

#format dates
ClimbLoggerData$DateTime <- mdy_hm(ClimbLoggerData$DateTime)
FixedLoggerData$DateTime <- ymd_hms(FixedLoggerData$DateTime)

#select only needed site variables
SiteData <- SiteData %>% 
  dplyr::select(Tree_ID, category, EdgeCategory = edge_category_m, forest_type)

#merge site data with climb and fixed logger data
ClimbLoggerData <- left_join(ClimbLoggerData, SiteData, by = "Tree_ID")
FixedLoggerData <- left_join(FixedLoggerData, SiteData, by = "Tree_ID")

#add category for matrix
FixedLoggerData <- FixedLoggerData %>% 
  mutate(category = ifelse(is.na(category),"matrix", category)) %>% 
  select(-DateTimePlaced, -DateTimeRemoved) %>% 
  mutate(HabCatStrat = paste(HabType, category, Strata, sep= "_"))

#FixedLoggerData$HabStrata <- paste(FixedLoggerData$Tree_ID, FixedLoggerData$Strata, sep = "_")
unique(FixedLoggerData$HabCatStrat)
head(FixedLoggerData)
head(ClimbLoggerData)

FixedLogSum <- FixedLoggerData %>% 
  group_by(DateTime, HabCatStrat) %>% 
  summarise(MeanTempByCat = mean(TempC))

#use fixed loggers to calculate the differences between canopy and under for each
#habitat type and for canopy and matrix & understory and Matrix for each hab type
CoreMatDiff <- FixedLogSum %>% 
  spread(key = HabCatStrat, value = MeanTempByCat) %>% 
  dplyr::mutate(CanSubUndBR = BR_core_Canopy - BR_core_Understory, CanSubUndM = M_core_Canopy - M_core_Understory,
         CanSubMatBR = BR_core_Canopy - MAT_matrix_NA, UndSubMatBR = BR_core_Understory - MAT_matrix_NA,
         CanSubMatM = M_core_Canopy - MAT_matrix_NA, UndSubMatM = M_core_Understory - MAT_matrix_NA) %>% 
  select(DateTime, CanSubUndBR, CanSubUndM, CanSubMatBR, UndSubMatBR, CanSubMatM, UndSubMatM) %>% 
  gather(key = DiffCategory, value = TempDiff_C, -DateTime)

#Plot it! Difference between habitat types and strata and matrix vs core
# for FIXED LOGGERS
#### THIS PLOT NEEDS DAY NIGHT ADDED!!!!
ggplot(CoreMatDiff, aes(DiffCategory, TempDiff_C, color = DiffCategory))+
  geom_boxplot(alpha = .03)+
  geom_point(alpha = .03)+
  geom_hline(yintercept = 0, color = "blue", alpha = .5)+
  theme_bw()

# Take home = 
#1-canopy is warmer than understory in both habitat types
#2- interior canopy is cooler than matrix both habs, but more in BR
#3 - interior Understory is cooler than matrix in both habs, but more in BR

FixedLogSumWide <- FixedLogSum %>% 
  spread(key = HabCatStrat, value = MeanTempByCat) %>% 
  mutate(RoundedTime = DateTime)

#round times to nearest 10 mins, to join with fixed loggers
ClimbLoggerData <- ClimbLoggerData %>% 
  mutate(RoundedTime = round_date(DateTime, "10 mins")) %>% 
  mutate(EdgeCatStratHab = paste(EdgeCategory, Strata, forest_type, sep = "_")) %>% 
  mutate(EdgeCatHab = paste(EdgeCategory, forest_type, sep = "_")) %>% 
  mutate(EdgeCatStrat = paste(EdgeCategory, Strata, sep = "_")) %>% 
  filter(EdgeCategory > -5, Strata != "NA", Strata != "Unclassified") %>% 
  mutate(DayNight = if_else(hour(DateTime) < 17, "Day", "Night"), HourOfDay = hour(DateTime))

hist(ClimbLoggerData$HourOfDay)

#JOIN climbing and fixed logger readings to compare each climb with fixed references
ClimbLoggerData <- left_join(ClimbLoggerData, FixedLogSumWide, by = "RoundedTime")
ClimbLoggerData <- ClimbLoggerData %>% 
  mutate(DiffFromMatrix = TempC - MAT_matrix_NA)

head(ClimbLoggerData)


###################################################################
############################################################################################################
#are there edge effects on microclimate (temp and RH)
#################################################################################################################################################################################################################################################

hist(ClimbLoggerData$TempC)
hist(ClimbLoggerData$RH)

#day
LoggerData<- ClimbLoggerData %>% 
  filter(DayNight == "Day")
#night
LoggerData <- ClimbLoggerData %>% 
  filter(DayNight == "Night")

#MODELS!
TempMod<-(lmer(TempC~
       Strata +
          Strata : scale(EdgeCategory) +
          (1 | HourOfDay) +
      (1|forest_type), data = ClimbLoggerData))
      
summary(TempMod)


RHMod<- (lmer(RH ~
                    #Strata +
                    Strata * scale(EdgeCategory) +
                    (1 | HourOfDay) +
                        (1|forest_type), data = ClimbLoggerData))


summary(RHMod)

#Plots
TempPlot <- plot_model(TempMod, type = "est", show.values = TRUE) 
  
TempPlot <- TempPlot + 
  theme_bw() +
  lims(y = c(-.5, .5))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = .7)+
  scale_x_discrete(labels = c("Und. x edge", "Mid. x edge", 
                              "Can. x edge", "Understory", "Midstory"))+
  labs(y = "")

TempPlot 

 RHplot <- plot_model(RHMod, type = "est", show.values = TRUE, colors = "blue")

RHplot <- RHplot+ 
  theme_bw() +
  lims(y = c(-.5, 7))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = .7)+
  scale_x_discrete(labels = c("Und. x edge", "Mid. x edge", "Can. x edge", "Understory", "Midstory"))

RHplot

###################################################################
###################################################################

###################################################################
##############      ANOVA          #####################################################
###################################################################
AovData <- ClimbLoggerData %>% 
  filter(EdgeCategory == "0" | EdgeCategory == 500) %>% 
  mutate(EdgeStrata = paste(EdgeCategory, Strata, sep = "_"))

Mod1 <- (aov(RH ~ EdgeStrata, data = AovData))
base::summary(Mod1)
TukeyHSD(Mod1)


ggplot(AovData, aes(EdgeStrata, RH, fill = Strata, color = EdgeCategory))+
  geom_jitter(width = .1, alpha = .4)+
  geom_boxplot(alpha = .5)

# COmparing each tree to itself by average temps by strata. Forget the difference to 

TreeSumTemps <- ClimbLoggerData %>% 
  group_by(forest_type, DayNight, category, EdgeCategory, Tree_ID, Strata) %>% 
  summarise(MeanTempC = mean(TempC), MeanRH = mean(RH))

#Temp
TreeSumTempsWide <- TreeSumTemps %>% 
  select(Tree_ID, Strata, MeanTempC) %>% 
  spread(key = Strata, value = MeanTempC)
TreeSumTempsWide <- TreeSumTempsWide %>% 
  mutate(CanDifUndC = Canopy - Understory, MidDifUndC = Mid - Understory) 
 
#RH
TreeSumRHWide <- TreeSumTemps %>% 
  select(Tree_ID, Strata, MeanRH) %>% 
  spread(key = Strata, value = MeanRH)

TreeSumRHWide <- TreeSumRHWide %>% 
  mutate(CanDifUndRH = Canopy - Understory, MidDifUndRH = Mid - Understory) %>% 
  select(Tree_ID, CanDifUndRH,MidDifUndRH, CanopyRH = Canopy, MidRH = Mid, UnderstoryRH = Understory) %>% 
  select(-EdgeCategory, -forest_type, -DayNight, -category)

names(TreeSumRHWide)
names(TreeSumTempsWide)

TreeSumTempsWide <- left_join(TreeSumTempsWide, HabData, by = "Tree_ID")

TreeSumRHWide <- left_join(TreeSumRHWide, HabData, by = "Tree_ID")


# PLOT comparing canopy to ground at different edge distances and by forest type
CanToUnd <- ggplot(TreeSumTempsWide, aes((EdgeCategory), CanDifUndC, color = DayNight))+
  geom_smooth(method = lm, alpha = .2, se = FALSE)+
  geom_jitter(height = 0, width = 5, alpha = .3)+
  geom_hline(yintercept = 0, color = "blue", alpha = .4)+
  #facet_grid(cols = vars(DayNight), rows = vars(forest_type))+
  #facet_wrap(~forest_type)+
  theme_classic()+
  labs(title = "Within tree temperature difference (C)",
       y = "Temperature difference (C) (canopy - understory)",
       x = "Approx. distance from forest edge (m)")

CanToUndRH <- ggplot(TreeSumRHWide, aes((EdgeCategory), CanDifUndRH, color = DayNight))+
  geom_smooth(method = lm, alpha = .2, se = FALSE)+
  geom_jitter(height = 0, width = 5, alpha = .3)+
  geom_hline(yintercept = 0, color = "blue", alpha = .4)+
  #facet_grid(cols = vars(DayNight), rows = vars(forest_type))+
  #facet_wrap(~forest_type)+
  theme_classic()+
  labs(title = "Within tree difference for humidity (RH)",
       y = "Difference in RH (canopy - understory)",
       x = "Approx. distance from forest edge (m)")


#plot raw temps
RawTemps <- ggplot(ClimbLoggerData, aes((EdgeCategory), TempC, color = Strata))+
  geom_smooth(method = lm, se = FALSE, alpha = .2)+
  geom_jitter(height = 0, width = 5, alpha = .5)+
  #facet_grid(cols = vars(DayNight), rows = vars(forest_type))+
  facet_wrap(~DayNight)+
  theme_classic()+
  #geom_texthline(label = "core canopy temp", yintercept = 26, linetype = "dashed",
   #              alpha = .7, size = 5.5, vjust = .4)+
  lims(y = c(22,30))+
  labs(x = "", y = "Temperature (C)"  ,title = "Temperature by strata and edge distance (C)")+
  theme(legend.position = "none")
RawTemps

RawRH <- ggplot(ClimbLoggerData, aes((EdgeCategory), RH, color = Strata))+
  geom_smooth(method = lm, se = FALSE, alpha = .1)+
  geom_jitter(height = 0, width = 5, alpha = .5)+
  #geom_hline(yintercept = 0, color = "blue", alpha = .4)+
  #facet_grid(cols = vars(DayNight), rows = vars(forest_type))+
  facet_wrap(~DayNight)+
  theme_classic()+
  theme(legend.position = "none")+
#geom_texthline(label = "core canopy RH",yintercept = 86.5, 
   #              linetype = "dashed", alpha = .7,  size = 5.5, vjust = .4)+
 # lims(y = c(22,30))+
  labs(x = "Distance from forest edge (m)", y = "Humidity (RH)"  ,title = "Relative humidity by strata and edge distance (C)")

RawRH

#build figures
TempsPlot<- cowplot::plot_grid(RawTemps, TempPlot, ncol = 2, nrow = 1, labels = c('(a)', "(b)"), rel_widths = c(3,1.75))
TempsPlot
RHPlots <- cowplot::plot_grid(RawRH, RHplot, ncol = 2, nrow = 1, labels = c('(c)', "(d)"), rel_widths = c(3,1.75))
RHPlots

Fig2a <- cowplot::plot_grid(TempsPlot, RHPlots, ncol = 1, nrow = 2)

Fig2a

ggsave("figures/finalFigures/2AThermalMicroStructure.pdf", Fig2a, width = 250, height = 250, units = "mm")

Fig2b <- cowplot::plot_grid(RawTemps, RawRH, ncol = 1, nrow = 2, labels = c('(a)', "(b)"), rel_heights = c(1,1.2))
Fig2b
ggsave("figures/finalFigures/2BTempAndRHByEdge.pdf", Fig2b, width = 250, height = 250, units = "mm")


#plot raw temps & RH but facet by strata!
TempsEdgePlot <- ggplot(ClimbLoggerData, aes((EdgeCategory), TempC, color = DayNight))+
  geom_smooth(method = lm, se = FALSE, alpha = .2)+
  geom_jitter(height = 0, width = 5, alpha = .5)+
  #facet_grid(cols = vars(DayNight), rows = vars(forest_type))+
  facet_wrap(~Strata)+
  theme_classic()+
  lims(y = c(22,30))+
  labs(x = "Approx. distance from forest edge (m)", y = "Temperature (C)"  ,
       title = "Edge effects on temperature across forest strata")


TempsEdgePlot

RHEdgePlot <- ggplot(ClimbLoggerData, aes((EdgeCategory), RH, color = DayNight))+
  geom_smooth(method = lm, se = FALSE, alpha = .2)+
  geom_jitter(height = 0, width = 5, alpha = .5)+
  #facet_grid(cols = vars(DayNight), rows = vars(forest_type))+
  facet_wrap(~Strata)+
  theme_classic()+
  labs(x = "Approx. distance from forest edge (m)", y = "Relative humidity (%)"  ,
       title = "Edge effects on temperature across forest strata")


RHEdgePlot

#build figures
TempsPlot<- cowplot::plot_grid(TempsEdgePlot, CanToUnd, ncol = 2, nrow = 1, rel_widths = c(2,1), labels = c('(a)', "(b)"))

RHPlot <- cowplot::plot_grid( RHEdgePlot, CanToUndRH, ncol = 2, nrow = 1, rel_widths = c(2,1), labels = c('(c)', "(d)"))

Fig2b <- cowplot::plot_grid(TempsPlot, RHPlot, ncol = 1, nrow = 2)
Fig2b

ggsave("figures/finalFigures/2BThermalMicroStructure.pdf", Fig2b, width = 300, height = 250, units = "mm")



#GLM Testing forest difference in temps
DayTempDif <- TreeSumTempsWide %>% 
  filter(DayNight == "Day") %>% 
  mutate(forest_type = as.factor(forest_type))
NightTempDif <- TreeSumTempsWide %>% 
  filter(DayNight == "Night")%>% 
  mutate(forest_type = as.factor(forest_type))

DayRH <- TreeSumRHWide %>% 
  filter(DayNight == "Day") %>% 
  mutate(forest_type = as.factor(forest_type))
NightRHDif <- TreeSumRHWide %>% 
  filter(DayNight == "Night")%>% 
  mutate(forest_type = as.factor(forest_type))

summary(DayRH$CanDifUndRH)


hist((DayTempDif$CanDifUnd))
hist(NightTempDif$CanDifUnd)
str(DayTempDif)

#does thermal structure increase towards core during the day? (YES!)
lmer1 <- (glm(CanDifUndC ~
                 scale(EdgeCategory) * forest_type+
                scale(BasalArea),
               data = DayTempDif))



summary(lmer1)

summary(gam1)
plot(gam1)
Dredge1 <- dredge(glm1)
summary(model.avg(Dredge1, subset = delta <= 2)) # This compurtes model average coefficients for the
#top models using Delta AIC <=2! 

#is canopy different than understory in daytime (YES!)
t.test(DayTempDif$Canopy, DayTempDif$Understory)

#does thermal structure increase towards core during the NIGHT? (NO)
glm2 <- (glm(CanDifUndC ~
               forest_type * EdgeCategory+
               #scale(StemMore8cm)+
               #scale(TotalAvgCan)+
               scale(BasalArea),
             data = NightTempDif, family = gaussian))

summary(glm2)
dredge(glm2)

#is canopy different than ground at day (YES!), and at night (NO)
t.test(DayTempDif$Canopy, DayTempDif$Understory)
t.test(NightTempDif$Canopy, NightTempDif$Understory)



BRLoggerData <- ClimbLoggerData %>% 
  filter(forest_type == "BR")

MLoggerData <- ClimbLoggerData %>% 
  filter(forest_type == "M")

#Add columns for difference by edge cat
BRLoggerData <- BRLoggerData %>% 
  mutate( DiffIntCan = TempC - BR_interior_Canopy,
          DiffIntUnd = TempC - BR_interior_Understory)

MLoggerData <- MLoggerData %>% 
  mutate(DiffIntCan = TempC - M_interior_Canopy,
         DiffIntUnd = TempC - M_interior_Understory)



#PLOT differences from fixed loggers
#Logger - matrix
Plot1 <- ggplot(BRLoggerData, aes(EdgeCategory, DiffFromMatrix, color = Strata))+
  geom_jitter(height = 0, width = 5, alpha = .5)+
  geom_smooth(method = lm, se = FALSE)+
  geom_hline(yintercept = 0, color = "blue")+
  facet_wrap(~DayNight)+
  theme_classic()+
  labs(title = " Brachystegia, difference from matrix")+
  lims(y = c(-10,8))

Plot2 <- ggplot(MLoggerData, aes(EdgeCategory, DiffFromMatrix, color = Strata))+
  geom_jitter(height = 0, width = 5, alpha = .5)+
  geom_smooth(method = lm, se = FALSE)+
  geom_hline(yintercept = 0, color = "blue")+
  facet_wrap(~DayNight)+
  theme_classic()+
  labs(title = "Mixed, difference from matrix")+
  lims(y = c(-10,8))

# difference from interior canopy
Plot3 <- ggplot(BRLoggerData, aes(EdgeCategory, DiffIntCan, color = Strata))+
  geom_jitter(height = 0, width = 5, alpha = .5)+
  geom_smooth(method = lm, se = FALSE)+
  geom_hline(yintercept = 0, color = "blue")+
  facet_wrap(~DayNight)+
  theme_classic()+
  labs(title = " Brachystegia, difference from int. canopy")+
  lims(y = c(-10,8))

Plot4 <- ggplot(MLoggerData, aes(EdgeCategory, DiffIntCan, color = Strata))+
  geom_jitter(height = 0, width = 5, alpha = .5)+
  geom_smooth(method = lm, se = FALSE)+
  geom_hline(yintercept = 0, color = "blue")+
  facet_wrap(~DayNight)+
  theme_classic()+
  labs(title = "Mixed, difference from int. canopy")+
  lims(y = c(-10,8))

# difference from interior understory
Plot5 <- ggplot(BRLoggerData, aes(EdgeCategory, DiffIntUnd, color = Strata))+
  geom_jitter(height = 0, width = 5, alpha = .5)+
  geom_smooth(method = lm, se = FALSE)+
  geom_hline(yintercept = 0, color = "blue")+
  facet_wrap(~DayNight)+
  theme_classic()+
  labs(title = " Brachystegia, difference from int. Understory")+
  lims(y = c(-10,8))

Plot6 <- ggplot(MLoggerData, aes(EdgeCategory, DiffIntUnd, color = Strata))+
  geom_jitter(height = 0, width = 5, alpha = .5)+
  geom_smooth(method = lm, se = FALSE)+
  geom_hline(yintercept = 0, color = "blue")+
  facet_wrap(~DayNight)+
  theme_classic()+
  labs(title = "Mixed, difference from int. Understory")+
  lims(y = c(-10,8))


plot_grid(Plot1, Plot3, Plot5, Plot2, Plot4, Plot6, ncol = 3, nrow = 2)

head(ClimbLoggerData)



