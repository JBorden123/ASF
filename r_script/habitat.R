#Cleaning up habitat data

#Data
habitat <- read.csv("raw_data/habitat.csv", header = TRUE)


HabSummary <- habitat %>%
  select(Tree_ID, TotalAvgCan = total_avg_can_cov, CanMinus10 = can_minus_10, CanMinus5 = can_minus_5,
         Can0 = can_0, CanPlus5 = can_plus_5, CanPlus10 = can_plus_10,
         AvgHerpCover = avg_herb_cover..., AvgLeafLayer = avg_leaf_layer,
         StemLess8cm = stem_less_8cm, StemMore8cm = stem_more_8cm)
