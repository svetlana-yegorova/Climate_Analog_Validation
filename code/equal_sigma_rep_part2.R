
### Take the distances to sigma level, and use them to randomly pull n number (whatever the sliced 
### number of analogs is) of pixels within the calculated distances in step 1 
### from the full, non-sliced dataset. In a sense, I will re-slice the full 
### dataset, only now by distance instead of sigma. 
###

## libraries: 
library(tidyverse)
library(dplyr)
library(future.apply)

## data
# distances by focal plot: 
summaries<-readRDS("./outputs/equal_sigma_distance_summary.rds")

# unsliced dataset: 
outputs<-readRDS("/home/svetlanayegorova/Documents/Climate_analog_calculations/outputs/1200_sample_full.rds")

# approximately 7 pixels do not have tree cover data. Remove them. 
outputs<-subset(outputs, !is.na(tree_diff)&!is.na(analog_trees))
#### The Work ##############

#1. Create an ouputs variable with climate bins: 

outputs<-mutate(outputs, Sigma_bin=case_when(Sigma<=0.1 ~0.1, 
                                   Sigma> 0.1 & Sigma<=0.5 ~0.5, 
                                   Sigma> 0.5 & Sigma<=1.0 ~ 1.0, 
                                   Sigma> 1.0 & Sigma<=1.5 ~1.5, 
                                   Sigma> 1.5 & Sigma<=2.0 ~ 2.0, 
                                   Sigma> 2.0 & Sigma<= 3.0 ~ 3.0, 
                                   Sigma>3.0 ~ 25))


#2. Write a function that for each focal plot, pulls n random analogs within a
# distance band associated with each sigma level. I would need to chunk up the 
# the "full" dataset to be able to run it. 

# divide outputs into four chunks: 
u_plots<-unique(outputs$focal_plot)

u_plots1<-u_plots[1:100]
ch1<-ch1 %>% filter(focal_plot %in% u_plots1)
summaries1<-summaries %>% filter(focal_plot %in% u_plots1)

u_plots2<-u_plots[301:600]
ch2<-outputs %>% filter(focal_plot %in% u_plots2)

u_plots3<-u_plots[601:900]
ch3<-outputs %>% filter(focal_plot %in% u_plots3)

u_plots4<-u_plots[901:1197]
ch4<-outputs %>% filter(focal_plot %in% u_plots4)



# pick focal plot:
i=1
plot<-summaries1$focal_plot[i]

# pick number of samples: 
N<-summaries1$plot_count[i]

d_low<-ifelse(summaries1$Sigma_bin1[i]==0.1, 0, summaries1$q_90_dist[i-1])
d_high<-summaries$q_90_dist[i]

mean_diff_km<-ch1%>%
  filter(focal_plot==plot, dist>= d_low & dist<d_high)%>%
  slice(n=N, replace=FALSE)%>%
  summarize(mean(abs(tree_diff)))%>%
  pull(.)


# put it into a function. The function takes summaries table row number as an 
# argument, and uses it to pull the rest of the pertinent information out of the
# summaries table. 

dist_resid<-function(i) {
  # pick focal plot:
  plot<-summaries1$focal_plot[i]
  
  # pick number of samples: 
  N<-summaries1$plot_count[i]
  
  # calculate distance boundaries:
  d_low<-ifelse(summaries1$Sigma_bin1[i]==0.1, 0, summaries1$q_90_dist[i-1])
  d_high<-summaries1$q_90_dist[i]
  
  
  # calculate mean tree cover difference for distance bin:
  mean_diff_km<-ch1%>%
    filter(focal_plot==plot, dist>= d_low & dist<d_high)%>%
    slice(n=N, replace=FALSE)%>%        # take n samples from the distance bin
    summarize(mean(abs(tree_diff)))%>%
    pull(.)
  
  return(mean_diff_km)
  
}

plan(multisession)

distance_model<-future_lapply(X=1:nrow(summaries1), FUN=dist_resid)
