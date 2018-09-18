#Cleaning up habitat data

#Data
habitat <- read.csv("raw_data/habitat.csv", header = TRUE)


HabSummary <- habitat %>% #select summary columns
  select(Tree_ID, TotalAvgCan = total_avg_can_cov, CanMinus10 = can_minus_10, CanMinus5 = can_minus_5,
         Can0 = can_0, CanPlus5 = can_plus_5, CanPlus10 = can_plus_10,
         AvgHerbCover = avg_herb_cover..., AvgLeafLayer = avg_leaf_layer,
         StemLess8cm = stem_less_8cm, StemMore8cm = stem_more_8cm)


HabSummary <- HabSummary %>%
  distinct(Tree_ID, .keep_all = TRUE) #remove duplicates

#transform these columns from densiometer readings (which gives
# measure of sky, not canopy), to now be % canopy
HabSummary$TotalAvgCan = 100 - HabSummary$TotalAvgCan
HabSummary$CanMinus10 = 100 - HabSummary$CanMinus10
HabSummary$CanMinus5 = 100 - HabSummary$CanMinus5
HabSummary$Can0 = 100 - HabSummary$Can0
HabSummary$CanPlus5 = 100 - HabSummary$CanPlus5
HabSummary$CanPlus10 = 100 - HabSummary$CanPlus10


write_csv(HabSummary, "clean_data/HabSummary.csv") #write the new dataset as a csv

