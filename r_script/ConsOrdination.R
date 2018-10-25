#Constrained ordination!


library(raster)
library(vegan)
#install.packages("ca")
library(ca)
library(dplyr)
library(ggplot2)
select <- dplyr::select

#You will also be using a function from a Biostats package developed
#by Kevin McGarigal. Save this package (i.e., R script) to your 
#working directory and use the source function to call it in:
source("biostats.r")

#data
MetaAll <- read.csv("clean_data/MetaAll.csv")
MetaAllM <- read.csv("clean_data/MetaAllM.csv")

################
#ALL DATA
################
CleanMetaAll <- MetaAll %>%
  filter(extra_ground != "Y") %>%
  filter(edge_category_m > -5)

#Brachy  
CleanMetaBR <- CleanMetaAll %>%
  filter(forest_type == "BR")
  
#Mixed
CleanMetaM <- CleanMetaAll %>%
  filter(forest_type == "M")

#Cynometra
CleanMetaCY <- CleanMetaAll %>%
  filter(forest_type == "CY")

summary(CleanMetaAll$Tree_ID)
summary(CleanMetaAll$forest_type)


#####################
#Creating species matrix and predictor variable matrices

SpecAll <- CleanMetaAll %>%
  select(ARST:VAAL) 

SpecAll <- SpecAll %>%
  select(which(!colSums(SpecAll) %in% 0)) #remove specie with no records in this dataset
  
A.B.flood.subset <- A.B.flood %>% select(which(!colSums(A.B.flood, na.rm=TRUE) %in% 0))
SpecM <- CleanMetaM %>%
  select(ARST:VAAL)

SpecBR <- CleanMetaBR %>%
  select(ARST:VAAL)

SpecCY <- CleanMetaCY %>%
  select(ARST:VAAL)

#Einvironmental variables
EnvAll <- CleanMetaAll %>% #herbaceous cover??
  select(edge_category_m, AvgLeafLayer, 
         TotalAvgCan, BasalArea, Debris, StemLess8cm, StemMore8cm)

#this loop replaces NA values with the column average
for(i in 1:ncol(EnvAll)){
  EnvAll[is.na(EnvAll[,i]), i] <- mean(EnvAll[,i], na.rm = TRUE)
}

#correlation matrix
round(as.dist(cor(EnvAll)), 2)

#Data selection, transformation and standardization
#Species within community data sets vary greatly in there occurrence, 
#abundance, and habitat specificity. Species that are common, widespread
#and extremely abundant can obscure patterns in the ordination. Species
#that are rare and have few occurrences in a data set may not be 
#accurately placed in ecological space. You must decide which species
#are “rare” and which are super abundant.

#Selecting Species
#To explore patterns of rarity and commonness, you will use the foa 
#function from the Biostats package. This function will give you a 
#whole series of plots that allow you to explore the occurrence and 
#abundance patterns of the species in your data. The second plot, 
#Empirical Distribution of Species Relative Occurrence, will be the 
#one we use to remove common and/or rare species. 
occur <- foa.plots(SpecAll)

rare <- which(occur[, 2] < 5)

common <- which(occur[, 2] > 95)

reduced <- SpecAll[, -c(rare, common)]

#Species transformations and standardizations
#First, check if species abundances are normally distributed across sites:
mapply(hist, as.data.frame(SpecAll[, 1:20]),
       main = colnames(SpecAll[, 1:20]), 
       xlab = "abundance")

#As you can see, most of the species distributions are right skewed. 
#Use the log transformation (logx+1) to transform the species distributions
#for both the full and reduced datasets:
log.full <- log1p(SpecAll)
log.red <- log1p(reduced)

#Next, check the row and column sum variability using the coefficient 
#of variation (cv) for both data sets:
#If either the row or column sums have cv >50, standardize by the total:

# Full data set:
rsum <- rowSums(log.full)
csum <- colSums(log.full)
hist(rsum)

hist(csum)

cv(rsum)
cv(csum)

# Reduced data set:
rsumRed <- rowSums(log.red)
csumRed <- colSums(log.red)
hist(rsumRed)

hist(csumRed)

cv(rsumRed)
cv(csumRed)

#If either the row or column sums have cv >50, standardize by the total:
#this is a way of standardizing the data in each column in the species data.
#It divides every value in a column by its column sum. Generating a
#proportion of its total abundance represented in that sample.
cSpec <- sweep(log.full, 2, csum, "/")
cSpecRed <- sweep(log.red, 2, csumRed, "/")


##Determine Response Model (RDA vs. CCA)
#Now that the species date are reduced, transformed and standardized, 
#you need to determine if species abundances show a linear (RDA) or a 
#unimodal (CCA) relationship with the underlying gradient.
#First, use Detrended Correspondence Analysis (DCA) to determine the 
#length of the canonical axes. You will use the decorana function in 
#the vegan Library. While DCA is a separate analysis with its own
#assumptions and multifaceted output, you will focus on axis length. 
#An axis length > 3 is evidence of a unimodal relationship. An axis length
#of <3 is evidence of a linear relationship. 
`?`(decorana)
decorana(cSpec)
decorana(cSpecRed)

#Next, plot out each species on the first canonical axis. You need to 
#set the environmental variables first (the next section will get into 
#the details of the explanatory variables). For, now just set them 
#and run the initial CCA to check for linearity.
#Set Explanatory Variables:
Vars <- varechem[, c(1, 2, 7)]
env <- as.data.frame(scale(Vars))
#Run CCA:
sp.CCA <- cca(cSpec ~ ., data = env)

#Function for plotting species abundances vs. CCA Axis 1:
f2 <- function(x) {
  plot(x ~ sp.CCA$CC$wa[, 1], xlab = "CCA AXIS 1", ylab = "Abundance ")
}

# Apply the function across all the species:
mapply(f2, varespec)

#Explanatory Variables
#Constrained ordination affords you the ability to include explanatory 
#variables in the ordination. You want to avoid mullitcolinearity among 
#explanatory variables and check if they are measured on the same scale.
#Based on a priori knowledge of this system, use the variables AL, P, 
#and N in the ordination.
#First look at all of the pairwise correlations between these variables: 
Vars <- varechem[, c(1, 2, 7)]
Vars
round(as.dist(cor(Vars)), 2)
#Do the variables AL, P, N look like they are measured on different scale?
#Check the cv to see if you need to z-standardize them:
cv(colSums(Vars))

#You need to make a data frame of the scaled variables to run the
#Constrained Ordination:
env <- as.data.frame(scale(Vars))

# Running the CCA
#You will run the constrained ordination using the cca in the vegan library.

`?`(cca)

## Unconstrained Ordination (CA) Before running the constrained model, 
#run an unconstrained ordination (i.e. a regular Correspondence Analysis 
#(CA; See Lab 4). CA will give you a measure of the amount of variation 
#in the site by species matrix that you will try to explain with the 
#explanatory variables (i.e. constraints). 

#this ordination ordinates the sites by species composition and then infuses
#the environmental variables on them. SO... the environmental data in a triplot
#IS NOT factor loadings. They are correlations. Species on the plot
#show "factor loadings" if they are close to eachother they are following
#similar distributional patterns.

# Full Data
#CA
ca <- cca(cSpec)
plot(ca)

summary(ca)
print(ca)

#CCA
cca <- cca(cSpec ~ ., data = env)
plot(cca)

summary(cca)

# Reduced Data
#CA
caR <- cca(cSpecRed)
plot(caR)

summary(caR)

#CCA
ccaR <- cca(cSpecRed ~ ., data = env)
plot(ccaR)

summary(ccaR)

#The first thing you should focus on in the summary is the proportion of 
#“inertia” (i.e. variance) explained by the Constrained Ordination. 
#Notice that the total amount of inertia is the same as the Unconstrained
#Ordination you just ran. Now look at the eigenvalue and proportion and
#cumulative amount of variation.
#
#Monte Carlo testing of the significance of the constrained axis.
#The permutation allows you to test if you constrained axes explain 
#more variation than would be expected randomly. You will use the 
#anova.cca function in vegan to conduct the permutation. It is 
#“anova-like” but not an anova. Global Test (i.e. all variables together):
anova(cca) #full
anova(ccaR) #reduced
#Axes Tests (i.e. each axis individually):
anova(cca, by = "axis") #full
anova(ccaR, by = "axis") #reduced


#Variable Tests (i.e. each variable individually):
anova(cca, by = "terms") #full
anova(ccaR, by = "terms") #reduced


#Observed (F matrix) and Predicted (Z Matrix) Site Scores
#Now look back at you cca summary again:
summary(cca)
#The matrix labeled “Site scores (weighted averages of species scores)” is
#the F matrix and the matrix labeled “Site constraints (linear 
#combinations of constraining variables)”is the Z matrix. Look at these 
#two sets of site scores projected in ordination space:
par(mfrow = c(1, 2))

plot(sp.CCA$CC$wa[, 1], sp.CCA$CC$wa[, 2], xlab = "CCA AXIS 1", ylab = "CCA AXIS 2")
plot(sp.CCA$CC$u[, 1], sp.CCA$CC$u[, 2], xlab = "CCA AXIS 1", ylab = "CCA AXIS 2")

#Look at the correlation between these two matrices. These correlations 
#can lend insight as to how well the predicted site locations match the 
#observed ones. However, they are not to be trusted as the only line of 
#evidence.
?spenvcor

#this funciton
spenvcor(cca)
spenvcor(ccaR)

#Intra-set correlations and biplot scores for the constraining variables.
#Correlations between the Z matrix (predicted site scores) and the 
#environmental variables provide information on which variables have 
#the largest influence on the constrained ordination. These also
#denote the placement of the environmental variables as vectors
#on the CCA tri-plot.
sp.CCA$CCA$biplot

##The Tri-Plot (using the site scores from the F matrix)
plot(sp.CCA, choices = c(1, 2), display = c("wa", "sp", "bp"), scaling = 2)
#and using the site scores from the Z matrix:
plot(sp.CCA, choices = c(1, 2), display = c("lc", "sp", "bp"), scaling = 2)



#**Now run the constrained ordination with the reduced data set. 
#Does excluding the common species improve the effectiveness of the 
#constrained ordination and or change your interpretation?
