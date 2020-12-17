# This file examines tree cover difference between analog and focal plots by physical distance and 
# Sigma. 

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
