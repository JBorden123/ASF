#Cleaning up habitat data

#Data
habitat <- read.csv("raw_data/habitat.csv", header = TRUE)


HabSummary <- habitat %>% #select summary columns
  select(Tree_ID, TotalAvgCan = total_avg_can_cov, CanMinus10 = can_minus_10, CanMinus5 = can_minus_5,
         Can0 = can_0, CanPlus5 = can_plus_5, CanPlus10 = can_plus_10,
         AvgHerbCover = avg_herb_cover..., AvgLeafLayer = avg_leaf_layer,
         StemLess8cm = stem_less_8cm, StemMore8cm = stem_more_8cm)%>%
  mutate(TotalAvgCan = (100 - TotalAvgCan))%>%
  mutate(CanMinus10 = (100 - CanMinus10))%>%
  mutate(CanMinus5 = (100 - CanMinus5))%>%
  mutate(Can0 = (100 - Can0))%>%
  mutate(CanPlus5 = (100 - CanPlus5))%>%
  mutate(CanPlus10 = (100 - CanPlus10))
#mutate adds column in which we 
#transform these columns from densiometer readings (which gives
# measure of sky, not canopy), to now be % canopy


HabSummary <- HabSummary %>%
  distinct(Tree_ID, .keep_all = TRUE) #remove duplicates



write_csv(HabSummary, "clean_data/HabSummary.csv") #write the new dataset as a csv

