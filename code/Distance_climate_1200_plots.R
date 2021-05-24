# This file looks at the effects of climate distance and physical distance on the quality of tree cover prediction. 
# The main differences from "distance_climate_hypotheses.R" are: 1) use larger dataset (1200 focal plots); 
# 2. focus on a few graphs, less exploration


##### -------Plan of Work-------------------- #####
# new sigma bins 0-0.1, 0.1-0.5, 0.5-1, 1-1.5, 1.5-2, 2-3. 

# need to account for sample intensity per sigma level... sample sigma per plot?
# Sample plots per sigma bin, I think, the latter. 

# plot residuals (difference between focal and analog) by distance and sigma

# convert Sean's graph to distance to 1:1 line 



##### Libraries #############
library(plyr)
library(ggformula)
library(ggplot2)
library(tidyverse)
library(car)
library(dplyr)
library(tidyverse)


##### Data ##################
outputs<-readRDS("/home/svetlanayegorova/Documents/Climate_analog_calculations/outputs/1200_sample_full.rds")

# approximately 7 pixels do not have tree cover data. Remove them. 
outputs<-subset(outputs, !is.na(tree_diff)&!is.na(analog_trees))
#### The Work ##############

#1. Create a variable with climate bins: 

outputs<-mutate(outputs, case_when(Sigma<=0.1 ~0.1, 
                                   Sigma> 0.1 & Sigma<=0.5 ~0.5, 
                                   Sigma> 0.5 & Sigma<=1.0 ~ 1.0, 
                                   Sigma> 1.0 & Sigma<=1.5 ~1.5, 
                                   Sigma> 1.5 & Sigma<=2.0 ~ 2.0, 
                                   Sigma> 2.0 & Sigma<= 3.0 ~ 3.0, 
                                   Sigma>3.0 ~ 25))

colnames(outputs)[11]<-"Sigma_bin1"

#1a. Add distance bins, so that we can stratify b

#2. Subsample outputs per sigma bin:
#head(sig_bin)
sig_bin<-outputs %>% 
  filter(Sigma<=3) %>%
  group_by(focal_plot, Sigma_bin1)%>%
  slice_sample(n=15, replace=FALSE)

sig_bin$Sigma_bin1<-as.factor(sig_bin$Sigma_bin1)
# str(sig_bin)
# nrow(sig_bin)

# per sigma bin, how many samples? 
sig_bin$dummy<-1
test<-ddply(sig_bin, .(Sigma_bin1), summarise, sum=sum(dummy))


#2.1 Plot absolute difference by kilometers and sigma (Sigma<=3): 
plot<-ggplot(data=sig_bin, aes(x=dist, y=abs(tree_diff)))+
         geom_point(aes(x=dist, y=abs(tree_diff)), color='grey')+
         geom_smooth(method=loess, se=FALSE, aes(colour= Sigma_bin1), cex=3)+
         ggtitle("Absolute Residuals by Distance to Analog", subtitle = " ~17000 samples per Sigma_bin")+
         ylab("Abs Difference")+xlab("Distance (km)")+
         theme_bw()+
         coord_cartesian(ylim=c(0, 35))+
        theme(axis.text.x = element_text( size=20,  angle=70), 
              axis.title = element_text(size=20),
        axis.text.y = element_text(size=20),
        plot.title = element_text(size = 30, face = "bold"), 
        plot.subtitle = element_text(size=20))+
  theme(legend.text = element_text(size=20), 
        legend.title = element_text(size=20))

png("./Documents/Climate_Analog_Validation/outputs/2000_samplepersigma_under25.png",  width=1500, height=800)
plot
dev.off()

# 2.2 Plot absolute difference by kilometers and sigma (all sigmas): 
sig_bin1<-outputs %>% 
  #filter(Sigma<=3) %>%
  group_by(focal_plot, Sigma_bin1)%>%
  slice_sample(n=15, replace=FALSE)

sig_bin1$Sigma_bin1<-as.factor(sig_bin1$Sigma_bin1)


plot1<-ggplot(data=sig_bin1, aes(x=dist, y=abs(tree_diff)))+
  geom_point(aes(x=dist, y=abs(tree_diff)), colour='grey')+
  geom_smooth(method=loess, se=FALSE, aes(colour= Sigma_bin1), cex=3)+
  ggtitle("Absolute Residuals by Distance to Analog, all Sigmas", subtitle = " ~17000 samples per Sigma_bin")+
  ylab("Abs Difference")+xlab("Distance (km)")+
  #ylim(0, 45)+
  theme_bw()+
  theme(axis.text.x = element_text( size=20,  angle=70), 
        axis.text.y = element_text(size=20), 
        plot.title = element_text(size = 30, face = "bold"), 
        plot.subtitle = element_text(size=20))+
        theme(legend.text = element_text(size=20), 
        legend.title = element_text(size=20))
png("./Documents/Climate_Analog_Validation/outputs/2000_samplepersigma_all.png",  width=1500, height=800)
plot1
dev.off()

##### ------Implement Sean's Idea------------------------ #####
# This one accounts for distance and sample size change with distance 
# better than the curves in the above plots. 

# plan of work: 
# 1. Calculate average distance to different levels of Sigma and associated residual size. Do it per plot (a) and for all
# plots (b) (global average distance and average residual per sigma) 
# 2. Calculate average residual per distance bin associated with levels of sigma. 
# 3. Compare the difference between 1 and 2. 


### 1a. Average (per plot) distance to sigma and associated residual: 
head(outputs)
dist_per_plot<-outputs %>%
  group_by(focal_plot, Sigma_bin1)%>%
  summarize(mean_dist=mean(dist), sd_d=sd(dist),  mean_resid=mean(abs(tree_diff)))
head(dist_per_plot)

dist_per_plot$Sigma_bin1<-as.factor(dist_per_plot$Sigma_bin1)
nrow(dist_per_plot)


# do a quick visualization: 
dist_by_plot<-ggplot(data=dist_per_plot, aes(x=mean_dist, y=mean_resid))+
  geom_point(aes(colour=Sigma_bin1))+
  ggtitle("mean absolute residual by distance and sigma", subtitle = "(each point represents a single focal plot)")+
  theme_bw()+ 
  theme(axis.text.x = element_text( size=20,  angle=70), 
        axis.title = element_text(size=20),
                    axis.text.y = element_text(size=20), 
                    plot.title = element_text(size = 30, face = "bold"), 
                    plot.subtitle = element_text(size=20))+
  theme(legend.text = element_text(size=20), 
        legend.title = element_text(size=20))

png("./Documents/Climate_Analog_Validation/outputs/mean_resid_by_dist&sigma_perplot.png", width=800, height=800)
dist_by_plot
dev.off()
### 1b. Global average distance to analog per sigma and associated residual: 
global_dis<-outputs %>%
  group_by(Sigma_bin1)%>%
  summarize(mean_dist=mean(dist), sd_d=sd(dist), mean_resid=mean(abs(tree_diff)))
head(global_dis)
global_dis$Sigma_bin1<-as.factor(global_dis$Sigma_bin1)

dist_by_sig_global<-ggplot(data=global_dis, aes(x=mean_dist, y=mean_resid))+
  geom_point(aes(colour=Sigma_bin1), cex=5)+
  ggtitle("mean absolute residual by distance and sigma")+
  ylim(0, 39)+xlim(0, 1000)+
  theme_bw()+ 
  theme(axis.text.x = element_text( size=20,  angle=70), 
        axis.title = element_text(size=20),
        axis.text.y = element_text(size=20), 
        plot.title = element_text(size = 30, face = "bold"), 
        plot.subtitle = element_text(size=20))+
  theme(legend.text = element_text(size=20), 
        legend.title = element_text(size=20))
dist_by_sig_global


png("./Documents/Climate_Analog_Validation/outputs/mean_resid_by_dist&sigma_global.png", width=800, height=800)
dist_by_sig_global
dev.off()

# 2a. Calculate residuals associated with mean distances calculated in 1a and 1b. 

# write a function that calculates mean residual for a given distance: 
# global first
# the process: filter residuals that are located between distances of mean(dist)-sd(dist) and mean+sd(dist), 
# calculate the mean absolute residual for that distance bin. 
head(outputs)

i=1
resid.function<-function(i){
  mean_resid_km<-outputs%>%
    filter(between(dist, (global_dis$mean_dist[i]-global_dis$sd_d[i]), (global_dis$mean_dist[i]+global_dis$sd_d[i])))%>%
    summarize(mean_resid_km=mean(abs(tree_diff))) %>%
    pull(.)
  return(mean_resid_km)
}

global_dis_res<-lapply(X=c(1:nrow(global_dis)), FUN=resid.function)
global_dis_res<-do.call("rbind", global_dis_res)

global_dis$mean_by_dist<-global_dis_res
head(global_dis)
global_dis$Sigma_bin1<-as.factor(global_dis$Sigma_bin1)

# compare global mean residuals by distance and sigma: 
global_resid_kmsig<-ggplot(data=global_dis, aes(x=mean_resid, y=mean_by_dist))+
  geom_point(aes(colour=Sigma_bin1), cex=5)+
  geom_abline(intercept = 0, slope=1)+
  coord_cartesian(xlim=c(0, 40), ylim=c(0, 37))+
  ggtitle("Mean Residuals by Kilometer Bin vs Climate Distance Bin")+
  theme_bw()+
  theme(axis.text.x = element_text( size=20,  angle=70), 
        axis.title = element_text(size=20),
        axis.text.y = element_text(size=20), 
        plot.title = element_text(size = 30, face = "bold"), 
        plot.subtitle = element_text(size=20))+
  theme(legend.text = element_text(size=20), 
        legend.title = element_text(size=20))

png("./Documents/Climate_Analog_Validation/outputs/mean_resid_by_dist&sigma_global.png", width=800, height=800)
global_resid_kmsig
dev.off()


## Now mean residuals per plot: 
head(dist_per_plot)

# function should go through every focal plot and calculate residual for a given distance buffer for that plot: 
# subset chunk to the focal plot
head(outputs)

dist_per_plot %>% 
  add_column(km_mean_resid=NA)

dist_per_plot<-dist_per_plot[2001:8372, ]
dist_per_plot<-as.data.frame(dist_per_plot)

for(i in 2001:8372) {
  mean_resid<-outputs %>%
    filter(focal_plot==dist_per_plot$focal_plot[i] & between(dist, dist_per_plot$mean_dist[i]-dist_per_plot$sd_d[1], dist_per_plot$mean_dist[i]+dist_per_plot$sd_d[i]))%>%
    summarize(mean_resid_km=mean(abs(tree_diff))) %>%
    pull(.)
  dist_per_plot$km_mean_resid[i]<-mean_resid
  if(i%%500==0){
    print(i)
    saveRDS(dist_per_plot, paste0("./Documents/Climate_Analog_Validation/outputs/dist_per_plot", i, ".rds"))
  }
}

i
dist_per_plot<-subset(dist_per_plot, !is.na(km_mean_resid))

dist_f<-function(i){
  mean_resid<-outputs %>%
    filter(focal_plot==dist_per_plot$focal_plot[i] & between(dist, dist_per_plot$mean_dist[i]-dist_per_plot$sd_d[1], dist_per_plot$mean_dist[i]+dist_per_plot$sd_d[i]))%>%
    summarize(mean_resid_km=mean(abs(tree_diff))) %>%
    pull(.)
  return(mean_resid)
  
}
n=nrow(dist_per_plot)
f_distances<-mclapply(X=1:n, FUN=dist_f, mc.cores = 8)


# remove NA values for a few plots that do not have analogs: 
dist_per_plot$Sigma_bin1<-as.factor(dist_per_plot$Sigma_bin1)
range(dist_per_plot$km_mean_resid)
head(dist_per_plot)

ggplot(data=dist_per_plot, aes(x=mean_resid, y=km_mean_resid))+
  geom_point(aes(colour=Sigma_bin1))+
  geom_abline(intercept=0, slope=1)
