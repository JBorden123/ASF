library(MVA)
library(psych)
library(Hmisc)
library(vegan)
library(StatMatch)
library(MASS)
library(raster)
library(cluster)
library(raster)

#data
metadata <- read.csv("clean_data/metadata.csv", header = TRUE)

#data screening
describeBy(data.frame(metadata))
describe(data.frame(metadata))

