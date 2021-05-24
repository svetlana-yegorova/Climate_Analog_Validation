# This file calculates mean distance to analog by sigma level, and the mean  
# tree cover difference between focal and analog plots by sigma level
# (with the ideas to visualize analog model performance by sigma and to 
# compare distance only movel
# to climate dissimilarity model),\.
# graphs the mean analog tree cover by focal tree cover with 1200 focal plots. 
rm(tt, trees_fia, tree_hexes, data_out,  biomass_plots, AGC_hexes)
##### Libraries #############
library(ggplot2)
library(tidyverse)
#install.packages("modelr")
library(modelr)
library(stats)
library(future.apply)
library(plyr)
##### Data ##################
outputs<-readRDS("./data/1200_pixels.rds")
outputs<-filter(outputs, !is.na(tree_diff) & !is.na(analog_trees))


##### Plots ################
# head(outputs)
# 
# outputs<-mutate(outputs, case_when(Sigma<=0.1 ~0.1, 
#                                    Sigma> 0.1 & Sigma<=0.5 ~0.5, 
#                                    Sigma> 0.5 & Sigma<=1.0 ~ 1.0, 
#                                    Sigma> 1.0 & Sigma<=1.5 ~1.5, 
#                                    Sigma> 1.5 & Sigma<=2.0 ~ 2.0, 
#                                    Sigma> 2.0 & Sigma<= 3.0 ~ 3.0, 
#                                    Sigma>3.0 ~ 25))
# 
# colnames(outputs)[12]<-"Sigma_bin1"
# outputs<-filter(outputs, !is.na(analog_trees))
# outputs$Sigma_bin1<-as.factor(outputs$Sigma_bin1)
# getwd()
#saveRDS(outputs, "./data/1200_pixels.rds")

##### calculate mean analog  tree cover value per sigma bin####
# get a table with focal pixel tree cover: 

focal_sub<-filter(outputs, focal_plot==analog_plot)
nrow(focal_sub)


# and a table that removes climate distance to self: 
output_nf<-filter(outputs, focal_plot!=analog_plot)
# nrow(output_nf)-nrow(outputs)
# head(output_nf)
# rm(outputs)
# are there any more NA's left? 
# sum(is.na(output_nf))
# no, should be all good. 


# Variation that groups all records by focal plot number: 
# out_summary<-output_nf%>% 
#   group_by(focal_plot, Sigma_bin1)%>%
#   summarize(mean_analog=mean(analog_trees), mean_km=mean(dist))
# head(out_summary$focal_plot)


# We want to even out the number of analogs per sigma bin per plot, 
# to give each sigma bin equal representation.  
# This variation that samples x analogs per sigma_bin1 per plot, where x is
# the smallest number of analogs in a given bin (probably Sigma of 0.1)
# plan of work: 

# head(output_nf)
# perform calculations for one focal plot, plot 20774: 
test<-subset(output_nf, focal_plot==20774)
test$focal_plot<-as.factor(test$focal_plot)

# # number of analogs per sigma bin: 
num_analogs<-aggregate(analog_plot~focal_plot+Sigma_bin1, data=test, FUN=length)
# 
# # choose the minimum value: 
 min_num<-min(num_analogs$analog_plot)
# 
# # slice the test dataset to contain min_num of analogs for each sigma bin: 
# 
test_sliced<-test%>%
  group_by(Sigma_bin1)%>%
  slice_sample(n=min_num, replace=FALSE)
# 
test_num_analogs<-aggregate(analog_plot~focal_plot+Sigma_bin1, data=test_sliced, FUN=length)

##### ----Write a sub-sampling function-------------------------- #####


# make sure that focal_plot and Sigma_bin1 are factors, so that aggregate function 
# can handle them as such. 

output_nf$focal_plot<-as.factor(output_nf$focal_plot)
output_nf$Sigma_bin1<-as.factor(output_nf$Sigma_bin1)

sub_sample_min<-function(plot){
  data<-subset(output_nf, focal_plot==plot)
  data$focal_plot<-as.factor(data$focal_plot)
  num_analogs<-aggregate(analog_plot~focal_plot+Sigma_bin1, data=data, FUN=length)
  min_num<-min(num_analogs$analog_plot)
  
  sliced<-data%>%
    group_by(Sigma_bin1)%>%
    slice_sample(n=min_num, replace=FALSE)
  return(sliced)
}
# 
# test1<-sub_sample_min(20774)
# test_num_analogs<-aggregate(analog_plot~focal_plot+Sigma_bin1, data=test1, FUN=length)
# 

# # test the function: 
# nrow(sub_samle_min(20774))/7
# 
# # test the function with lapply:
# focal<-unique(focal_sub$focal_plot)
# str(focal)
# focal_test<-focal[1:2]
# slice_test<-lapply(X=focal_test, FUN=sub_sample_min)
# str(slice_test)

# divide output_nf into chunks: 
plan(multisession)

# divide data into 12 chunks, 100 focal plots each

for (i in 1:12){
  
  m<-(i-1)*100+1 # lower boundary for focal plot
  n<-i*100      # upper boundary for focal plot  
  output_nf<-filter(outputs, focal_plot  %in% focal[m:n])
  saveRDS(output_nf, paste0("./outputs/output_chunk_", i, "_of_12.rds"))
  

}

# test that the function is doing what it's supposed to do: 
test_data<-readRDS("./outputs/sliced_output2_of12.rds")
test_data1<-do.call('rbind', test_data)
class(test_data1)
head(test_data1)

test_num_analogs<-aggregate(analog_plot~Sigma_bin1, data=test_data1, FUN=length)


# next steps: 
# load sliced_outputs, rbind them, and concantenate them into a single dataframe
# calculate average distance, make the graph.
str(s_out)
test<-unique(s_out$focal_plot)
nrow(s_out)



#### add focal pixel tree cover to outputs dataframe  ####
add_focal_t<-function(f_plot){
  focal_trees=filter(focal_sub, analog_plot==f_plot)%>%
    select(analog_trees)%>%
    pull(.)
  return(c(focal_trees, f_plot))
}

test_fcover<-lapply(X=out_summary$focal_plot, FUN=add_focal_t)

# same length, good. 

fcover<-do.call('rbind', test_fcover)

### add focal tree cover to the out_summary
out_summary<-cbind(out_summary, fcover[, 1], fcover[, 2] )
head(out_summary)
colnames(out_summary)[5]<-("focal_trees")
colnames(out_summary)[6]<-("plot_check")
colnames(out_summary)[4]<-("mean_km")
colnames(out_summary)

# were focal plots matched correctly? 
test<-filter(out_summary, focal_plot!=plot_check)

# save the  summary table: 
saveRDS(out_summary, "./outputs/summary_mean_distance_bykm&sigma.rds")
out_summary<-readRDS("./outputs/summary_mean_distance_bykm&sigma.rds")
# now, plot. 
head(out_summary)
head(focal_sub)
hist(focal_sub$analog_trees)
out_summary<-filter(out_summary, focal_trees<=100)


### subset out_summary to certain sigma bins: 
out_summary_v<-out_summary %>%
  filter(Sigma_bin1 %in% c(0.1, 0.5, 1, 2, 3, 25))
head(out_summary_v)

analogbyf<-ggplot(data=out_summary_v , aes(x=focal_trees, y=mean_analog))+
  geom_point(aes(colour=log(mean_km)))+
  geom_abline(intercept = 0, slope=1)+
  geom_smooth(method="lm", color='yellow', cex=2)+
  xlim(0, 100)+
  facet_wrap(vars(Sigma_bin1), nrow=1)+
  theme_bw()+
  ggtitle("1200 focal plots, actual vs mean predicted tree cover by Sigma bin")+
  xlab("Focal Tree Cover")+
  ylab("Mean Analog Tree Cover")+
  coord_fixed()+
  theme(axis.text.x = element_text( size=20,  angle=70),
        axis.text.y = element_text( size = 20), 
        axis.title = element_text(size = 20), 
        plot.title = element_text(size = 20, face = "bold"), 
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15), 
        strip.text = element_text(size = 20)) # this line sets the size for facet label 
#png( "./outputs/focal_by_mean_analog_tree_cover_1200.png", width=1500, height=700)

png("./outputs/grad_con_viz.png", width=1500, height=700)
analogbyf
dev.off()



####### Check if RMSE for linear #############
s01<-lm(mean_analog~focal_trees, data=subset(out_summary, Sigma_bin1==0.1))
summary(s01)

rmse(s01)

##### ---------Calculate Mean Analog Difference 
#####----------for different levels of sigma: ----------- #####
outputs_nona<-filter(output_nf, !is.na(tree_diff))
(nrow(outputs)-nrow(outputs_nona))/nrow(outputs)
output_nf$dummy<-1

m_difference_fplot<-output_nf%>% 
  group_by(focal_plot, Sigma_bin1)%>%
  summarize(mean_dif=mean(tree_diff), plot_count=sum(dummy))
head(m_difference_fplot)

# subsample each sigma level to an equal amount of analog plots: 

m_difference_subsample<-output_nf%>% 
  group_by(focal_plot, Sigma_bin1)%>%
  slice_sample(n=15, replace=FALSE)%>%
  summarize(mean_dif=mean(tree_diff), plot_count=sum(dummy))


# plot mean % cover difference between analog and focal by Sigma: 

mean_diff_box<-ggplot(data=m_difference_fplot, aes(x=Sigma_bin1, y=mean_dif))+
  geom_boxplot()+
  theme_bw()+
  ggtitle("Mean Analog Estimate Difference from Focal Cover", subtitle = "1200 focal plots")+
  xlab("Sigma Bins")+
  ylab("Mean % Tree Cover Difference")+
  theme(axis.text.x = element_text( size=20,  angle=70),
        axis.text.y = element_text( size = 20), 
        axis.title = element_text(size = 20), 
        plot.title = element_text(size = 20, face = "bold"), 
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15), 
        strip.text = element_text(size = 20)) 

png("./outputs/mean_analog_estimate.png", width=1000, height=1000)
mean_diff_box
dev.off()


ggplot(data=m_difference_subsample, aes(x=Sigma_bin1, y=mean_dif))+
  geom_boxplot()
?geom_boxplot

### global mean difference by sigma: 
rm(outputs)
global_diff_sub<-output_nf%>%
  group_by(Sigma_bin1)%>%
  slice_sample(n=100, replace=FALSE)%>%
  summarize(global_mean=mean(tree_diff), analog_count=sum(dummy))

head(global_diff)


mean_analog_diff<-ggplot(data=global_diff_sub, aes(x=Sigma_bin1, y=global_mean))+
  geom_point(pch=8, cex=3)+
  #geom_boxplot()+
  geom_abline(intercept = 0, slope=0)+
  theme_bw()+
  ggtitle("Mean Analog Estimate Difference from Focal Cover", subtitle = "100 subsamples per Sigma")+
  xlab("Sigma Bins")+
  ylab("Mean % Tree Cover Difference")+
  theme(axis.text.x = element_text( size=20,  angle=70),
  axis.text.y = element_text( size = 20), 
  axis.title = element_text(size = 20), 
  plot.title = element_text(size = 20, face = "bold"), 
  legend.title=element_text(size=20), 
  legend.text=element_text(size=15), 
  strip.text = element_text(size = 20)) 

# plot mean % cover difference between analog and focal by Sigma + add
# global mean points: 

mean_diff_box<-ggplot(data=m_difference_fplot, aes(x=Sigma_bin1, y=mean_dif))+
  geom_boxplot()+
  geom_point(data=global_diff_sub, aes(x=Sigma_bin1, y=global_mean), color='red')+
  theme_bw()+
  ggtitle("Mean Analog Estimate Difference from Focal Cover", subtitle = "1200 focal plots")+
  xlab("Sigma Bins")+
  ylab("Mean % Tree Cover Difference")+
  theme(axis.text.x = element_text( size=20,  angle=70),
        axis.text.y = element_text( size = 20), 
        axis.title = element_text(size = 20), 
        plot.title = element_text(size = 20, face = "bold"), 
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15), 
        strip.text = element_text(size = 20)) 

png("./outputs/mean_analog_estimate.png", width=1000, height=1000)
mean_diff_box
dev.off()