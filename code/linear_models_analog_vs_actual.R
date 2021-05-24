# linear models for mean_analog tree_cover ~ focal_pixel_tree_cover by Sigma
# running this file for landscape ecology abstract: 

#### Data: 
out_summary<-readRDS("./outputs/summary_mean_distance_bykm&sigma.rds")
out_summary<-filter(out_summary, focal_trees<=100)

### libraries: 
library(tidyverse)

# get 6 linear models of mean analog tree cover ~ actual tree cover by 6 levels
# of climate dissimilarity: 

head(out_summary)

# six models: 
models <- out_summary %>% 
  split(.$Sigma_bin1) %>% 
  map(function(df) lm(mean_analog ~focal_trees, data = df))

# model summaries: 
models %>% 
  map(summary) %>% 
  map_dbl(~.$r.squared)


models %>% 
  map(summary) %>% 
  map_dbl(~.$coefficients[2])
str(models)
