#Spatial stuff
library(raster)

library(ggmap)
library(dplyr)
library(rstudioapi)
library(devtools)



register_google(key = "AIzaSyAaO50aTtpR4N75DlhLuT3yJA49SH1yJnU")

###############
#use lat long columns in a CSV to make a spatial object
###############
MetaAll<- read.csv("clean_data/MetaAll.csv")
#read in points in a CSV file and turn lat and lon columns into a spatial object

lat_lon <- MetaAll%>%
  dplyr::select(lat, lon)%>%
  filter(lat != "NA",lon != "NA")

#Map with stamen maps
summary(lat_lon)
bbox <- c(left = 39.75, bottom = -3.54, right = 40.05, top = -3.17)

map1 <- ggmap(get_stamenmap(bbox,zoom = 13, maptype = c("terrain",
                                             "terrain-background", "terrain-labels", "terrain-lines", "toner",
                                             "toner-2010", "toner-2011", "toner-background", "toner-hybrid",
                                             "toner-labels", "toner-lines", "toner-lite", "watercolor")))

map1+
  geom_point(data = lat_lon, aes(x = lon, y = lat),color="#BD2D13",pch=4)


#need to speficy projection associated with the csv (due to )
points_crs <- crs("+proj=longlat +datum=WGS84 +towgs84=0,0,0")
#this creates a projection specification
#this code is for any lat long data and specifies the standard 
#associated with this object

# arguments bellow are - columns with lat lon data, name of data frame,
#projection to be used

points_spat <- SpatialPointsDataFrame(lat_lon[c('lon', 'lat')],
                                      lat_lon,
                                      proj4string = points_crs)

plot(points_spat)#plot this new spatial object


#########
#GGMAP!!!!
#########
#ggmap doesn't like spatial data, prefers csv with lat lon

avg_long<-mean(lat_lon$lon) #ggmap wants to know mean of lat longs to get to right area
avg_lat<-mean(lat_lon$lat)
center = c(lon = -95.3632715, lat = 29.7632836)

map <- ggmap(get_googlemap(center = c(lon = 39.90041,lat = -3.384623), zoom = 10,
                     maptype = "satellite",
                           color = "color"))


#select map locatoin and type from google maps
register_google
ggmap(map)+ #plot this map with points added
  geom_point(data = lat_lon, aes(x = lon, y = lat),color="#BD2D13",pch=4)
?get_map

?register_google
###############
#raster stacks, stacking rasters on top of eachother
##############

#make a list of files
ndvi_files <- list.files("data/HARV_NDVI/", full.names = T, #where are the files
                         pattern = "HARV_NDVI.*.tif") #what is their naming pattern?
# '.*' astrix is a wild card and means "anything"-any string of any characters

ndvi_rasters <- stack(ndvi_files)

plot(ndvi_rasters) #plot all the rasters!

plot(ndvi_rasters, c(1,2,3))#plots the first 3 rasters in the stack

#calculating statistics on cell values in rasters, and multiple rasters
avg_ndvi <- cellStats(ndvi_rasters, mean) #what raster stack and what function on each

#make summary stats (mean values per raster by cells) into a data frame
avg_ndvi_df <- data.frame(samp_period= 1:length(avg_ndvi),ndvi=avg_ndvi)
#gives arguments are column name and what to fill it with

library(dplyr)

avg_ndvi_df <- tibble::rownames_to_column(avg_ndvi_df,var="name") 
#turns the row names into a column names
