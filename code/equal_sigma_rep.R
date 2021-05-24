# Why of this file. I am comparing distance-only model of predicting tree cover
# to the climate dissimilarity model. The intention is to compare difference 
# between focal and analog tree cover for each model. 
# 

# I am loading a sub-sampled dataset, that has ~ 1200 focal plots, where each 
# focal plot has an equal number of analogs in each sigma bin (sliced dataset).

# I calculate mean tree cover difference  for each sigma level. 

### Want to compare tree cover difference from the sigma model to tree cover 
### difference from the geographical distance model. For that to happen... 
### Need to do: calculate mean (or 90th percentile) distance to analog based
### on the sliced (by Sigma) dataset for each plot. 
### Take distances, and use them to randomly pull n number (whatever the sliced 
### number of analogs is) of pixels within the calculated distances in step 1 
### from the full, non-sliced dataset. In a sense, I will re-slice the full 
### dataset, only now by distance instead of sigma. 
###



# combine the sliced chunks, where each focal plot has an equal number
# of analogs in each sigma bin. 
rm(AGC_hexes, biomass_plots, data_out, output, tree_hexes, trees_fia, tt, output)

##########
library(tidyverse)
library(ggplot2)
library(future.apply)


# load half the data: 
output<-vector("list", length=6)
for (i in 1:6){
  output[[i]]<-readRDS(paste0("./outputs/sliced_output", i, "_of12.rds"))
  output[[i]]<-do.call('rbind', output[[i]])
  print(i)
}


out<-do.call('rbind', output)

saveRDS(out, './outputs/sliced_to_equal_sigma_rep.rds')

#### load the second half of the data: 

output1<-vector("list", length=6)
for (i in 7:12){
  output[[i]]<-readRDS(paste0("./outputs/sliced_output", i, "_of12.rds"))
  output[[i]]<-do.call('rbind', output[[i]])
  print(i)
}

out1<-do.call('rbind', output1)

test_out<-aggregate(analog_plot~Sigma_bin1, data=out, FUN=length)

# save the second half of the data: 
saveRDS(out, './outputs/sliced_to_equal_sigma_rep1.rds')

out_f<-rbind(out, out1)

#rm(output, output1, out, out1)

# make sure that analog to self is removed from the dataset: 
out_self<-filter(out_f, focal_plot==analog_plot)
out<-filter(out_f, focal_plot!=analog_plot)

# intention: filter out the focal plot not fully represented in the out: 
# head(out)
# sub_test1<-out%>% filter(Sigma_bin1==0.1)
# nrow(sub_test1)
# sub_test2<-out%>% filter(Sigma_bin1==0.5)
# nrow(sub_test2)

head(out_f)


## what is the distance distribution for different sigmas: 
distributions<-ggplot(data=subset(out))+
  geom_histogram(aes(dist))+
  ggtitle("Distance-to-analog distribution by Sigma")+
  xlab("Distance")+
  theme(axis.text.x = element_text( size=20),
        axis.text.y = element_text( size = 20), 
        axis.title = element_text(size = 20), 
        plot.title = element_text(size = 20, face = "bold"), 
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15), 
        strip.text = element_text(size = 20))+
  facet_wrap(vars(Sigma_bin1))

png("./outputs/distance_distribution_by_sigma.png", width=1200 , height=1000)
distributions
dev.off()

# observation about the graph above: mean distance grows from 0.1 to ~ 1.5, then stagnate
class(out)
str(out)
out<-ungroup(out)
head(out)


# calculate mean and 90th percentile distance to each sigma bin for each 
# focal plot (as well as mean tree difference per sigma bin): 
out_f$dummy<-1

out_smry1<-out_f %>%
  group_by(focal_plot, Sigma_bin1) %>%
  dplyr::summarize(mean_difference=mean(abs(tree_diff)), q_90_dist=quantile(dist, .90), mean_dist=mean(dist), plot_count=sum(dummy))

head(out_smry1)
range(out_smry1$plot_count)
hist(out_smry1$plot_count, breaks=100)
?hist()
# export the out_smry1 table: 
saveRDS(out_smry1, './outputs/equal_sigma_distance_summary.rds')

# remove unnecessary files: 


# are there any focal plots with incomplete records? Doesn't look that way. 
head(out_smry1)
tail(out_smry1)

# need to add tree cover difference by physical distance: 

# do it for one plot: 
i=1
plot<-out_smry$focal_plot[i]
d_low<-ifelse(out_smry$Sigma_bin1[i]==0.1, 0, out_smry$q_90_dist[i-1])
d_high<-out_smry$q_90_dist[i]
mean_diff_km<-out%>%
  filter(focal_plot==plot, dist>= d_low & dist<d_high)%>%
  summarize(mean(abs(tree_diff)))%>%
  pull(.)

#### turning above steps into a function that takes row number of table
# out_smry as an argument: 

distance_error<-function(row_n){
  plot<-out_smry1$focal_plot[row_n]
  d_low<-ifelse(out_smry1$Sigma_bin1[row_n]==0.1, 0, out_smry1$q_90_dist[row_n-1])
  d_high<-out_smry1$q_90_dist[row_n]
  mean_diff_km<-out%>%
    filter(focal_plot==plot, dist>= d_low & dist<d_high)%>%
    summarize(mean(abs(tree_diff)))%>%
    pull(.)
  print(row_n)
  return(mean_diff_km)
}

# apply function :
rm(distance_model)
distance_model1<-lapply(X=1:nrow(out_smry1), FUN=distance_error)

length(distance_model1)
nrow(out_smry1)


# how many NA' in distance_model? NA's are computed for larger
# values of sigma, when 90th percentile distance to 
sum(is.na(distance_model1))

# add distance model to the out_smry table: 
distance_model_c1<-do.call('rbind', distance_model1)

out_smry1<-cbind(out_smry1, distance_model_c1[, 1])
head(out_smry)

colnames(out_smry1)[6]<-c("km_error")

# 
full_summary<-rbind(out_smry, out_smry1)
nrow(full_summary)
#out_smry<-select(out_smry, c(1:5, 8))
head(out_smry)
# save the out_smry: 
saveRDS(out_smry, "./outputs/cover_diff_by_sig&dist_1_600.rds")
saveRDS(full_summary, "./outputs/cover_diff_by_sig&dist_full_1200.rds")

out_smry$c_vs_d<-out_smry$km_error-out_smry$mean_difference
head(out_smry)

# plot the difference in cover for the climate and the distance model: 
ggplot(data=full_summary)+
  geom_boxplot(aes(x=Sigma_bin1, y=c_vs_d))+
  ggtitle("Distance vs Climate model")

# remove na's
full_summary<-filter(full_summary, !is.na(c_vs_d))

full_summary_global<-full_summary%>%
  group_by(Sigma_bin1)%>%
  dplyr::summarise(mean_c_error=mean(mean_difference), c_sd=sd(mean_difference), 
                   mean_km_error=mean(km_error), km_sd=sd(km_error))

# add columns for effor bar info: 
full_summary_global<-full_summary_global%>%
  mutate(c_lower=mean_c_error-c_sd, c_upper=mean_c_error+c_sd)

full_summary_global<-full_summary_global%>%
  mutate(d_lower=mean_km_error-km_sd, d_upper=mean_km_error+km_sd)
  
mean_improvement<-(full_summary)%>%
  group_by(Sigma_bin1)%>%
  dplyr::summarize(mean_dabove_c=mean(c_vs_d),  plot_number=length(c_vs_d))


### How does error from climate model compare to error from distance model: 
cnd_models_by_plot<-ggplot(data=full_summary)+
  geom_point(aes(x=mean_difference, y=km_error, colour=Sigma_bin1), cex=4)+
  geom_abline(intercept = 0, slope=1, col='blue')+
  ggtitle("Tree Cover Difference From the Distance and Climate Models")+
  ylab("Distance Model Difference from Focal")+
  xlab("Climate Model Difference from Focal")+
  theme_bw()+
  theme(axis.text.x = element_text( size=20),
        axis.text.y = element_text( size = 20), 
        axis.title = element_text(size = 20), 
        plot.title = element_text(size = 20, face = "bold"), 
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15), 
        strip.text = element_text(size = 20))

png("./outputs/mean_error_by_model&plot.png", height=1000, width=1000)
cnd_models_by_plot
dev.off()

### Mean error by model: 
mean_diff_by_model<-ggplot(data=full_summary_global)+
  geom_errorbar(aes(x=mean_c_error, ymin=d_lower, ymax=d_upper, colour=Sigma_bin1), width=0.5, size=0.5, color="blue") +
  geom_errorbar(aes(y=mean_km_error, xmin=c_lower, xmax=d_upper, colour=Sigma_bin1), width=0.5, size=0.5, color="blue") + 
  geom_point(aes(x=mean_c_error, y=mean_km_error, colour=Sigma_bin1), cex=4.5)+
  geom_abline(intercept = 0, slope=1)+
  coord_fixed(ratio=1)+
  ylim(0, 49)+
  xlim(0, 49)+
  ggtitle("Mean Tree Cover Difference From the Distance and Climate Models")+
  xlab("Climate Model Tree Cover Difference from Focal")+
  ylab("Distance Model Tree Cover Difference from Focal")+
  theme(axis.text.x = element_text( size=20),
        axis.text.y = element_text( size = 20), 
        axis.title = element_text(size = 20), 
        plot.title = element_text(size = 20, face = "bold"), 
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15), 
        strip.text = element_text(size = 20))

png("./outputs/mean_error_by_model.png", height=1000, width=1000)
mean_diff_by_model
dev.off()


# what is the overall distribution of distances? 
head(out)
overall_dist<-ggplot(data=out)+
  geom_histogram(aes(x=dist))+
  theme_bw()+
  ggtitle("distance frequency in the dataset balanced by sigma levels")+
  xlab("Distance (km)")+
  theme(axis.text.x = element_text( size=20),
        axis.text.y = element_text( size = 20), 
        axis.title = element_text(size = 20), 
        plot.title = element_text(size = 20, face = "bold"), 
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15), 
        strip.text = element_text(size = 20))
# visualize t mean distances to analog: 

head(out_smry)
mean_dist<-ggplot(data=out_smry)+
  geom_boxplot(aes(x=Sigma_bin1, y=mean_dist))+
  ggtitle("Distance-to-analog distribution by Sigma")+
  xlab("Sigma Bin")+
  ylab("Mean Distance to Analog")+
  theme(axis.text.x = element_text( size=20),
        axis.text.y = element_text( size = 20), 
        axis.title = element_text(size = 20), 
        plot.title = element_text(size = 20, face = "bold"), 
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15), 
        strip.text = element_text(size = 20))
png("./outputs/mean_distance2analog.png", width=800, height=700)
mean_dist
dev.off()

# plot 90th percentile of distance to analog: 
dist2analog_90pctl<-ggplot(data=out_smry)+
  geom_boxplot(aes(x=Sigma_bin1, y=q_90_dist))+
  ggtitle("Distance-to-analog distribution by Sigma")+
  xlab("Sigma Bin")+
  ylab("90th Percentile Distance to Analog")+
  theme(axis.text.x = element_text( size=20),
        axis.text.y = element_text( size = 20), 
        axis.title = element_text(size = 20), 
        plot.title = element_text(size = 20, face = "bold"), 
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15), 
        strip.text = element_text(size = 20))

png("./outputs/ninetieth_distance2analog.png", width=800, height=700)
dist2analog_90pctl
dev.off()
