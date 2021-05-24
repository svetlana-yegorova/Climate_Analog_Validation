# partial regression with a small dataset: 

############# Data #####################
 rm(AGC_hexes, biomass_plots, data_out, tree_hexes, trees_fia, tt)
# data<-readRDS("./data/1200_pixels.rds")
# data<-subset(data, !is.na(tree_diff))

########### Libraries ###################
library(ggplot2)
library(tidyverse)
library(plyr)
#install.packages("patchwork")
library(patchwork)

### load a smaller dataset to work with
sm_data<-readRDS("./data/fia_tree_cover4km.rds")
# remove nas
test<-sum(is.na(sm_data$trees_fia))
sm_data1<-sm_data%>%
  filter(!is.na(trees_fia))

nrow(sm_data1)
sm_data<-sm_data1

########### Explore and massage the data ################3

# add tree_diff variable: 

sm_data$tree_diff<-sm_data$focal_trees-sm_data$trees_fia

# # 1 regress tree_diff on distance: 
# range(sm_data$Sigma)
d_model<-lm(abs(tree_diff)~km, data=sm_data)
summary(d_model)

# # visualize tree_diff ~ distance relationship: 
# ggplot(data=sm_data, aes(x=km, y=abs(tree_diff)))+
#   geom_smooth(method="lm")+
#   ylim(0, 70)
# #  geom_smooth(method = "loess", se=FALSE)
# 
# 
# # 2 regress MD on distance: 
cd_model<-lm(MD~ km, data=sm_data)
summary(cd_model)
# 
# md_km<-ggplot(data=sm_data)+
#   geom_smooth(aes(x=km, y=MD), method="lm", color="green")+
#   ylim(0, 40)+
#   ggtitle("Climate~ geographic distance")
# 
# cover_km<-ggplot(data=sm_data, aes(x=km, y=abs(tree_diff)))+
#   geom_smooth(method="lm")+
#   ylim(0, 50)+
#   ggtitle("Tree cover ~ geogrpahic distance")
#  
# md_km+cover_km
# # geom_smooth(aes(x=km, y=abs(tree_diff)), method="lm", color="yellow")
# #+
# #geom_smooth(method="loess", se=FALSE)
# 
# 
# # get residuals from both regressions for the partial regression: 
cover_resids<-d_model$residuals
md_resids<-cd_model$residuals
# length(d_model$residuals)
# length(cd_model$residuals)
# nrow(sm_data)

resids<-data.frame(cbind(cover_resids, md_resids))
head(resids)
p_reg<-lm(cover_resids~md_resids, data=resids)
summary(p_reg)
# 
# p_reg<-ggplot(data=resids, aes(x=md_resids, y=cover_resids))+
#   geom_smooth(method="lm", color="yellow")+
#   ggtitle("partial regression")
# 
# p_reg_plot<-md_km+cover_km+p_reg
# 
# png("./outputs/partial_regression_plot.png", width=1600, height=600)
# p_reg_plot
# dev.off()
# 
# 
# 
# ##### ------------------------------ #####
# 
# ### redo partial regression limiting Sigma to < 4: 
sm_data1<-subset(sm_data, Sigma<4)
# 
# # 1 regress tree_diff on distance: 
# range(sm_data$Sigma)
d_model<-lm(abs(tree_diff)~km, data=sm_data1)
summary(d_model)

# # visualize tree_diff ~ distance relationship: 
# lim_dist<-ggplot(data=sm_data1, aes(x=km, y=abs(tree_diff)))+
#   geom_smooth(method="lm")+
#   ylim(0, 50)+
#   ggtitle("distance<1500")
# 
# cover_km+lim_dist
# 
# # 2 regress MD on distance: 
cd_model<-lm(MD~ km, data=sm_data1)
summary(cd_model)
# 
# md_km1<-ggplot(data=sm_data1)+
#   geom_smooth(aes(x=km, y=MD), method="lm", color="green")+
#   ylim(0, 40)+
#   ggtitle("km<1500")
# 
# #compare the two plots
# md_km+md_km1
# 
# 
# cover_km<-ggplot(data=sm_data, aes(x=km, y=abs(tree_diff)))+
#   geom_smooth(method="lm")+
#   ylim(0, 70)+
#   ggtitle("Tree cover ~ geogrpahic distance")
# 
# md_km1+lim_dist
# # geom_smooth(aes(x=km, y=abs(tree_diff)), method="lm", color="yellow")
# #+
# #geom_smooth(method="loess", se=FALSE)
# 
# 
# # get residuals from both regressions for the partial regression: 
cover_resids<-d_model$residuals
md_resids<-cd_model$residuals
# length(d_model$residuals)
# length(cd_model$residuals)
# nrow(sm_data)

resids<-data.frame(cbind(cover_resids, md_resids))
head(resids)
p_reg1<-lm(cover_resids~md_resids, data=resids)
summary(p_reg1)

test_reg<-lm(tree_diff~MD+km, data=sm_data)
summary(test_reg)

# summary(p_reg1)
# 
# 
# p_reg1_plot<-ggplot(data=resids1500, aes(x=md_resids, y=cover_resids))+
#   stat_binhex(aes(fill=log(..count..)))+
#   geom_smooth(method="lm", color="yellow")+
#   ggtitle("partial regression")
# 
# md_km1+lim_dist+p_reg1_plot

# next steps: 
# subset the data, either randomly or by sigma bin and run analyses
# visualize data with loess


# subset data for each plot by sigma level (100 focal plots * 8 sigma levels * 50 ~ 40,000 obs) 
head(sm_data)
unique(sm_data1$Sig_ceiling)
data_sub1<-sm_data%>% 
  mutate(sigma_bin=case_when(Sigma<=0.2~ 0.2,
                             Sigma<=0.5&Sigma>0.2~0.5, 
                             Sigma<=1&Sigma>0.5 ~ 1, 
                             Sigma>1&Sigma<=1.5 ~1.5, 
                             Sigma>1.5&Sigma <=2 ~ 2, 
                             Sigma>2 &Sigma<=3 ~3, 
                             Sigma>3&Sigma<4 ~4, 
                             Sigma> 4 ~5))%>%
  group_by(from_plot, sigma_bin)%>%
  slice_sample(n=50, replace=FALSE)

unique(data_sub1$sigma_bin)


# plot with loess: 

# cover by km: 
cover_km_loess<-ggplot(data=data_sub1, aes(x=km, y=abs(tree_diff)))+
  stat_binhex(aes(fill=log(..count..)))+
  geom_smooth(method="lm", color="red")+
  geom_smooth(method = "loess", se=FALSE, color="yellow")+
  ylim(0, 70)+
  ggtitle("Tree cover ~ geogrpahic distance")+
  theme_classic()+
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(size=20), 
        plot.title = element_text(color = "black", size = 25, face = "bold", hjust = 0.5))

png("./outputs/cover_km_loess_subsample.png", height =800, width =1000)
cover_km_loess
dev.off()


# climate by geographic distance:  
climate_km_loess<-ggplot(data=data_sub1, aes(x=km, y=MD))+
  stat_binhex(aes(fill=log(..count..)))+
  geom_smooth(method="lm", color="red")+
  geom_smooth(method = "loess", se=FALSE, color="yellow")+
  #ylim(0,)+
  ggtitle("MD ~ geogrpahic distance")+
  theme_classic()+
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(size=20), 
        plot.title = element_text(color = "black", size = 25, face = "bold", hjust = 0.5))

png("./outputs/climate_km_loess_subsample.png", height =800, width =1000)
climate_km_loess
dev.off()


# run a partial regression with the small dataset: 

#1 tree difference by distance: 
dist_model<-lm(abs(tree_diff)~km, data=data_sub1)
dist_res<-dist_model$residuals

summary(dist_model)

# md by distance: 
clim_km<-lm(MD~km, data=data_sub1)
clim_res<-clim_km$residuals
summary(clim_km)
range(sm_data1$MD)

p_reg<-lm(dist_res~clim_res)
summary(p_reg)


# put residuals into a dataframe to plot with ggplot: 
resids<-data.frame(cbind(clim_res, dist_res))
head(resids)

# plot partial regression from a subset of data: 
p_reg<-ggplot(data=resids, aes(x=clim_res, y=dist_res))+
  stat_binhex(aes(fill=log(..count..)))+
  geom_smooth(method="loess", se=FALSE, color="yellow")+
  geom_smooth(method="lm", color="red")+
  ggtitle("Partial Regression Plot")+
  theme_classic()+
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(size=20), 
        plot.title = element_text(color = "black", size = 25, face = "bold", hjust = 0.5))

png("./outputs/p_regression_subsample.png", height = 700, width=900)
p_reg
dev.off()