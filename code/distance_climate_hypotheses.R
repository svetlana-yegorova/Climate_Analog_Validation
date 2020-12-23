# This file examines tree cover difference between analog and focal plots by physical distance and 
# Sigma. It plots residuals for anlogs of each focal plot by distance, and examines whether distance &
# residual relationship is modulated by sigma.

############### Data ######################
data_out<-readRDS("./data/fia_tree_cover4km.rds")
colnames(data_out)[24]<-("analog_trees")
trees_fia<-subset(data_out, !(is.na(analog_trees)))
trees_fia$cover_d<-trees_fia$focal_trees-trees_fia$analog_trees
trees_fia$tree_ratio<-trees_fia$cover_d/trees_fia$focal_trees

############## Libraries #####################
library(plyr)
library(ggformula)
library(ggplot2)
library(tidyverse)
library(car)
library(dplyr)
library(tidyverse)

##################################################
# unique focal plot numbers: 
focal<-unique(trees_fia$from_plot)

##### What is the distribution of the focal tree cover?  #####
hist(unique(trees_fia$focal_trees), breaks=60)

sub1<-subset(trees_sub1000, focal_trees>60)
sub2<-subset(trees_sub1000, focal_trees>30&focal_trees<=60)
sub3<-subset(trees_sub1000, focal_trees<=30)
length(unique(sub1$from_plot))
length(unique(sub2$from_plot))

length(unique(trees_fia$from_plot))
head(trees_fia)

########### Plots:  #########################
# for each focal plot get 1000 random analogs
# 1. check how many analogs under sigma 4 are there per plot? 
# Some plots have fewer than 1000 analogs within Sigma 4. 

trees_fia$dummy<-1
check<-ddply(subset(trees_fia, Sigma<25), .(from_plot), summarise, n_plots=sum(dummy))

trees_sub1000 <-trees_fia %>% group_by(from_plot) %>% slice_sample(n=1000, replace=FALSE)
test<-ddply(trees_sub1000, .(from_plot), summarize, test=sum(dummy))

# do a case study of plots with focal cover more than 60%: 
diff_by_km_case<-ggplot(data=subset(trees_sub1000, trees_focal>60), aes(x=km, y=cover_d))+
 # geom_point(data=subset(trees_sub1000), aes(x=km, y=cover_d))+
  geom_smooth(method=loess, se=FALSE)+
  ggtitle("Tree Cover Residual by Distance to Analog, Focal Cover")+
  ylab("Tree Cover Difference")+xlab("Distance (km)")+
  facet_wrap(vars(from_plot), nrow=3)+
  theme_bw()

diff_by_km_case
png("./outputs/cover_difference_60pcnt.png")
diff_by_km_case
dev.off()

##### --------Focal Tree Cover Sub 30%---------------------- #####

# do a case study of plots with focal cover less than 30%: 
diff_by_km_case_sub3<-ggplot(data= sub3, aes(x=km, y=abs(cover_d)))+
  geom_point()+
  geom_smooth(method=loess, se=FALSE, cex=3)+
  ggtitle("Absolute Residuals by Distance to Analog, Focal Cover <=30%")+
  ylab("Tree Cover Difference")+xlab("Distance (km)")+
  theme_bw()+
  theme(axis.text.x = element_text( size=10,  angle=70), 
        plot.title = element_text(size = 20, face = "bold"))+
  facet_wrap(vars(from_plot), nrow=3)

png("./outputs/cover_difference_under_30pcnt.png", width=1500, height=1000)
diff_by_km_case_sub3
dev.off()


# do a case study of plots with focal cover less than 60%, but greater than 30%: 
diff_by_km_case_sub2<-ggplot(data= sub2, aes(x=km, y=abs(cover_d)))+
  geom_point(data= sub2, aes(x=km, y=abs(cover_d)))+
  geom_smooth(method=loess, se=FALSE, cex=3)+
  ggtitle("Absolute Residuals by Distance to Analog, 30% <Focal Cover< 60%")+
  ylab("Tree Cover Difference")+xlab("Distance (km)")+
  theme_bw()+
  theme(axis.text.x = element_text( size=10,  angle=70), 
        plot.title = element_text(size = 20, face = "bold"))+
  facet_wrap(vars(from_plot), nrow=3)

png("./outputs/cover_difference_under_sub2.png", width=1500, height=1000)
diff_by_km_case_sub2
dev.off()

##### --------Focal Tree Cover Over 60%---------------------- #####

# do a case study of plots with focal cover less than 30%: 
diff_by_km_case_sub1<-ggplot(data= sub1, aes(x=km, y=abs(cover_d)))+
  geom_point()+
  geom_smooth(method=loess, se=FALSE, cex=3)+
  ggtitle("Absolute Residual by Distance to Analog, Focal Cover >60%")+
  ylab("Tree Cover Difference")+xlab("Distance (km)")+
  theme_bw()+
  theme(axis.text.x = element_text( size=10,  angle=70), 
        plot.title = element_text(size = 20, face = "bold"))+
  facet_wrap(vars(from_plot), nrow=3)

png("./outputs/cover_difference_over_60pcnt.png", width=1500, height=1000)
diff_by_km_case_sub1
dev.off()

##### -------Investigate the effect of Sigma on how residuals change
#####--------with distance  #####
# 1. subset the 1000 analog plots from the Sigma<25 
# 2. Plot by fine sigma gradation
# 3. Plot by coarse 

#1. Select plots that have at least 1000 Sigma<25 analogs: 
trees_fia$dummy<-1

good_analogs<-
  trees_fia %>%
  filter(Sigma<25) %>%
  group_by(from_plot) %>%
  summarise( ct=sum(dummy)) %>%
  filter(ct>=1000)
nrow(good_analogs)                                                               # 85 plot have over 1000 analogs under sigma
                                                                                 # 25
#1.a.subset trees_fia by the plots in "good_analogs"
sub0<-trees_fia %>%
  filter(Sigma<25 & from_plot %in% good_analogs$from_plot) %>%                   # select plots that have sufficient number of "good"
  group_by(from_plot) %>%                                                        # analogs, then take 1000 random samples out of the 
  slice_sample(n=1000, replace=FALSE)                                            # "good" (< 25 sigma) analogs


#2. Plot by sigma categories: 
# what are my sigma category options? 
str(sub0)

sub0$Sig_ceiling<-as.factor(sub0$Sig_ceiling)

# Tree cover 30 to 60 %
mid<-ggplot(data=subset(sub0, focal_trees>30&focal_trees<60), aes(x=km, y=abs(cover_d)))+
  geom_point(colour="grey")+
  geom_smooth(method=loess, se=FALSE, aes(colour= Sig_ceiling), cex=3)+
  # geom_spline( aes(colour= Sig_ceiling), nknots=5, cex=2)+
  ggtitle("Absolute Residual by Distance to Analog & Sigma, 30%<Focal Cover< 60%")+
  ylab("Tree Cover Difference")+xlab("Distance (km)")+
  ylim(-10, 100)+
  theme_bw()+
  theme(axis.text.x = element_text( size=13,  angle=70), 
        plot.title = element_text(size = 20, face = "bold"), 
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15))+
  facet_wrap(vars(from_plot), nrow=3)


png("./outputs/c_dif_by_sigma_mid.png", width=1500, height=1000)
mid
dev.off()


# Tree cover over 60%
high<-ggplot(data=subset(sub0, focal_trees>=60), aes(x=km, y=abs(cover_d)))+
  geom_point(colour="grey")+
  geom_smooth(method=loess, se=FALSE, aes(colour= Sig_ceiling), cex=3)+
  # geom_spline( aes(colour= Sig_ceiling), nknots=5, cex=2)+
  ggtitle("Absolute Residual by Distance to Analog & Sigma, Focal Cover > 60%")+
  ylab("Tree Cover Difference")+xlab("Distance (km)")+
  ylim(-10, 100)+
  theme_bw()+
  theme(axis.text.x = element_text( size=13,  angle=70), 
        plot.title = element_text(size = 20, face = "bold"), 
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15))+
  facet_wrap(vars(from_plot), nrow=3)


png("./outputs/c_dif_by_sigma_high.png", width=1500, height=1000)
high
dev.off()



# Tree cover under 30% 
low<-ggplot(data=subset(sub0, focal_trees<=30), aes(x=km, y=abs(cover_d)))+
  geom_point(colour="grey")+
  geom_smooth(method=loess, se=FALSE, aes(colour= Sig_ceiling), cex=3)+
  # geom_spline( aes(colour= Sig_ceiling), nknots=5, cex=2)+
  ggtitle("Absolute Residual by Distance to Analog & Sigma, Focal Cover < 30%")+
  ylab("Tree Cover Difference")+xlab("Distance (km)")+
  ylim(-10, 100)+
  theme_bw()+
  theme(axis.text.x = element_text( size=13,  angle=70), 
        plot.title = element_text(size = 20, face = "bold"), 
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15))+
  facet_wrap(vars(from_plot), nrow=3)


png("./outputs/c_dif_by_sigma_low.png", width=1500, height=1000)
low
dev.off()

##### ---------If we only look at large sigmas (>4),
##### does the residual-distance relationship hold?----- #####
# 1. Subset trees_fia to only have analogs over 4 sigma, and then subsample them
# to 1000 random samples: 

sub25<-trees_fia %>%
  filter(Sigma>4)%>%                   # select plots that have sufficient number of "good"
  group_by(from_plot) %>%                                                        # analogs, then take 1000 random samples out of the 
  slice_sample(n=1000, replace=FALSE)  

#1.a subset trees_fia with sigmas <4, do residuals behave better when we group
# analogs by coarser gradation of sigma? 

subu25<-trees_fia %>%
  filter(Sigma<4)%>%                   # select plots that have sufficient number of "good"
  group_by(from_plot) %>%                                                        # analogs, then take 1000 random samples out of the 
  slice_sample(n=1000, replace=FALSE)  

#1.b create three coarse sigma filters: sigma under 4, sigma over 4
sub_all<-trees_fia%>%
  mutate(Sig_coarse=as.factor(case_when(Sigma<4 ~4, 
                              Sigma>4 ~25))) %>%
  mutate(Sig_coarse1=as.factor(case_when(Sigma<=2 ~2, 
                               Sigma >2 & Sigma <=3 ~3, 
                               Sigma>3 ~ 25))) %>%
  group_by(from_plot) %>%                                                        # analogs, then take 1000 random samples out of the 
  slice_sample(n=1000, replace=FALSE)


head(sub25)
# Tree cover over 60%, sigma =25
high_25<-ggplot(data=subset(sub25, focal_trees>=60), aes(x=km, y=abs(cover_d)))+
  geom_point(colour="grey")+
  geom_smooth(method=loess, se=FALSE, cex=3)+
  # geom_spline( aes(colour= Sig_ceiling), nknots=5, cex=2)+
  ggtitle("Absolute Residual by Distance to poor analogs \n (sigma>3.5), Focal Cover > 60%\n")+
  ylab("Tree Cover Difference")+xlab("Distance (km)")+
  ylim(-10, 100)+
  theme_bw()+
  theme(axis.text.x = element_text( size=13,  angle=70), 
        plot.title = element_text(size = 20, face = "bold"), 
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15))+
  facet_wrap(vars(from_plot), nrow=3)

high_25


##### -------Use two coarse bins of sigmas: 
#####--------Sigmas <4 and Sigmas above that, do better analogs make for a better
##### tree cover prediction? Yes, they do! --------- #####
# Tree cover over 60%

head(sub_all)
high_all<-ggplot(data=subset(sub_all, focal_trees>=60), aes(x=km, y=abs(cover_d)))+
  geom_point(colour="grey")+
  geom_smooth(method=loess, se=FALSE, cex=3, aes(colour=Sig_coarse))+
  # geom_spline( aes(colour= Sig_ceiling), nknots=5, cex=2)+
  ggtitle("Absolute Residual by Distance to coarsely pooled analog, Focal Cover > 60%\n")+
  ylab("Tree Cover Difference")+xlab("Distance (km)")+
  ylim(-10, 100)+
  theme_bw()+
  theme(axis.text.x = element_text( size=13,  angle=70), 
        plot.title = element_text(size = 20, face = "bold"), 
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15))+
  facet_wrap(vars(from_plot), nrow=3)

png("./outputs/residuals_by_distance&coarse_sigma.png", width=1500, height=1000)
high_all
dev.off()

# medium cover: 
med_all<-ggplot(data=subset(sub_all, focal_trees<60& focal_trees>=30), aes(x=km, y=abs(cover_d)))+
  geom_point(colour="grey")+
  geom_smooth(method=loess, se=FALSE, cex=3, aes(colour=Sig_coarse))+
  # geom_spline( aes(colour= Sig_ceiling), nknots=5, cex=2)+
  ggtitle("Absolute Residual by Distance to coarsely pooled analog, 30%<=Focal Cover<60%\n")+
  ylab("Tree Cover Difference")+xlab("Distance (km)")+
  ylim(-10, 100)+
  theme_bw()+
  theme(axis.text.x = element_text( size=13,  angle=70), 
        plot.title = element_text(size = 20, face = "bold"), 
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15))+
  facet_wrap(vars(from_plot), nrow=3)

png("./outputs/med_residuals_by_distance&coarse_sigma.png", width=1500, height=1000)
med_all
dev.off()

?geom_smooth
# low cover: 
low_all<-ggplot(data=subset(sub_all, focal_trees<30), aes(x=km, y=abs(cover_d)))+
  geom_point(colour="grey")+
  geom_smooth(method=loess, se=FALSE, cex=3, aes(colour=Sig_coarse))+
  # geom_spline( aes(colour= Sig_ceiling), nknots=5, cex=2)+
  ggtitle("Absolute Residual by Distance to coarsely pooled analog, Focal Cover<30%\n")+
  ylab("Tree Cover Difference")+xlab("Distance (km)")+
  ylim(-10, 100)+
  theme_bw()+
  theme(axis.text.x = element_text( size=13,  angle=70), 
        plot.title = element_text(size = 20, face = "bold"), 
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15))+
  facet_wrap(vars(from_plot), nrow=3)

png("./outputs/low_residuals_by_distance&coarse_sigma.png", width=2000, height=1000)
low_all
dev.off()

##### ---------If we combine all data together? --------------------- #####
sub_all$Sig_ceiling<-as.factor(sub_all$Sig_ceiling)

all<-ggplot(data=sub_all, aes(x=km, y=abs(cover_d)))+
  geom_point(colour="grey")+
  geom_smooth(method=loess, se=FALSE, cex=3, aes(colour=Sig_ceiling))+
  # geom_spline( aes(colour= Sig_ceiling), nknots=5, cex=2)+
  ggtitle("Absolute Residuals by Distance to Analog & Sigma \n 100 FIA focal plots \n")+
  ylab("Absolute Tree Cover Difference")+xlab("Distance (km)")+
  ylim(-1, 100)+
  theme_bw()+
  theme(axis.text.x = element_text( size=13,  angle=70), 
        plot.title = element_text(size = 20, face = "bold"), 
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15))#+
  #facet_wrap(vars(from_plot), nrow=3)
setwd("/home/svetlanayegorova/Documents/Climate_Analog_Validation")

png("./outputs/sigma_vs_km_all_plots.png", height=800, width = 800)
all
dev.off()


##### -------Sean's Suggestion----------------------- #####                                        # implement Sean's suggestion: 
# calculate mean distance to analogs of different sigma values.
# take all analogs within that distance, and calculate the mean residuals
# for the two groups. 

#1. find out mean distance for analogs at different sigma levels:

sig_dist<-trees_fia %>%
  group_by(from_plot, Sig_ceiling) %>%
  summarize(mean_dist=mean(km), sd_dist=sd(km), mean_resid=mean(abs(cover_d)))
print(sig_dist, nrow=10)

#2. For each plot and each distance category, calculate the average residual: 

# can do it with a for loop:  
# add a new empty column: 
sig_dist %>% 
  add_column(km_mean_resid=NA)
head(sig_dist)
# trees_fia%>%
#   filter(between(km, 0, 500 ))

for(i in 1:nrow(sig_dist)) {
mean_resid<-trees_fia %>%
  filter(from_plot==sig_dist$from_plot[i] & between(km, sig_dist$mean_dist[i]-sig_dist$sd_dist[1], sig_dist$mean_dist[i]+sig_dist$sd_dist[i]))%>%
  summarize(mean_resid_km=mean(abs(cover_d))) %>%
  pull(.)
sig_dist$km_mean_resid[i]<-mean_resid
}


# takes a minute to run the loop, think of a way to do it with a function.   
# str(mean_resid)
sig_dist$Sig_ceiling<-as.factor(sig_dist$Sig_ceiling)
seans_way<-ggplot(data=sig_dist)+
  geom_point(aes(x=km_mean_resid,  y=mean_resid, col=Sig_ceiling), cex=3)+
  geom_abline(intercept = 0, slope=1)+
  labs(title="Physical Dist vs Climate Distance Residuals")+
  xlab("Residuals from Distance Only model")+
  ylab("Residuals from Climate Only model")+
  theme_bw()+
  theme(axis.text.x = element_text( size=13,  angle=70), 
        plot.title = element_text(size = 20, face = "bold"), 
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15))

png("./outputs/Sean_dist_vs_climate.png", height=800, width=800)
seans_way
dev.off()



##### -----What is the average distance for each sigma? ----------- #####
str(sig_dist)
km2sig<-sig_dist %>% 
  group_by(Sig_ceiling) %>%
  summarize(mean_dist=mean(mean_dist))


km_to_sigma<-ggplot(data=sig_dist, aes(x=Sig_ceiling, y=mean_dist))+
  geom_point(aes(col=from_plot), cex=3)+
  geom_point(data=km2sig, aes(x=Sig_ceiling, y=mean_dist), pch=17, cex=5, col="yellow")+
  labs(title="Mean Distance to Sigma per Plot")



########################### What is going on with the focal tree cover? 
# test0<-ddply(trees_sub1000, .(from_plot), summarize, mean=mean(focal_trees))
# head(trees_fia)
# test1<-unique(trees_fia$focal_trees)
# length(test1)
# length(unique(test0$mean))
# 
# diff_by_km_case
# png("./outputs/cover_difference_60pcnt.png")
# diff_by_km_case
# dev.off()
# 
# 
# ###### separate the dataset by focal tree cover and plot tree cover difference by focal cover###############333
# diff_by_km_1<-ggplot(data= subset(trees_fia, focal_trees<=30), aes(x=km, y=cover_d))+
#   geom_smooth(method=loess, se=FALSE, aes(colour=Sig_c_f))+
#   ggtitle("Tree Cover Residual by Distance to Analog & Sigma")+
#   ylab("Tree Cover Difference")+xlab("Distance (km)")+
#   facet_wrap(vars(from_plot), nrow=5)+
#   theme_bw()
# 
# diff_by_km_1
# 
# 
# 
# # absolute majority of analogs for plot 841 are at Sigma 25: 
# test1<-ddply(subset(trees_sub1000, from_plot==841), .(Sig_c_f), summarize, test=sum(dummy))
# 
# 
# 
# # case study of difference by sigma: 
# diff_by_km_case<-ggplot(data= subset(trees_sub1000, from_plot==6610 & Sigma<25), aes(x=Sigma, y=cover_d))+
#   # geom_point(data=subset(trees_sub1000), aes(x=km, y=cover_d))+
#   geom_smooth(method=loess, se=FALSE)+
#   ggtitle("Tree Cover Residual by Distance to Analog & Sigma")+
#   ylab("Tree Cover Difference")+xlab("Sigma")+
#   #facet_wrap(vars(from_plot), nrow=3)+
#   theme_bw()
# 
# diff_by_km_case
#        
