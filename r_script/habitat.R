#Cleaning up habitat data

#Data
habitat <- read.csv("raw_data/habitat.csv", header = TRUE)
habitat <- habitat[,-1]
