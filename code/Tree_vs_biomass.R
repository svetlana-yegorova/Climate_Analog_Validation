### This file compares slopes of lines fit to biomass and tree cover data and checks the normality assumption for 

### Data #####################
data_out<-readRDS("./data/fia_tree_cover4km.rds")
ncol(data_out)
colnames(data_out)[24]<-("analog_trees")
trees_fia<-subset(data_out, !(is.na(analog_trees)))

### Libraries ################
library(tidyverse)
library(ggplot2)
library(car)
library(dplyr)

### check the normality assumption of the tree cover data 
### 1. tree cover data are not normally distributed, but follow more of a uniform distribution) 


hist(trees_fia$focal_trees[trees_fia$focal_trees!=0], breaks=100)
hist(log(trees_fia$focal_trees), breaks=100)
hist(trees_fia$analog_trees[trees_fia$analog_trees!=0], breaks=100)
hist(log(trees_fia$analog_trees[trees_fia$analog_trees!=0]), breaks=100)


### 2.Fit a linear model and examine normality of the residuals (residuals look normal! :) 
slope_25<-lm(analog_trees~focal_trees, data=subset(trees_fia, Sigma<=0.25))
summary(slope_25)
str(slope_25)
hist(slope_25$residuals)

# set up an empty dataframe to store slopes and intercepts from the analog ~ focal linear models: 

sig_coef<-data.frame(sigma=as.numeric(), slope=as.numeric(), intcpt=as.numeric() )

# get unique values of sigmas for which I want regressions done: 
sigmas<-unique(as.numeric(levels(trees_fia$Sig_c_f)))
rm(sig_coef)

# coefficients for the tree cover models: 
for (i in sigmas) {
  model<-lm(analog_trees~focal_trees, data=subset(trees_fia, Sig_c_f== i))
  srt <-cbind(i, as.numeric(model$coefficients[1]), model$coefficients[2])
  sig_coef<-rbind(sig_coef, srt)
}

# coefficients for the biomass model: 
sig_coef_c<-data.frame(sigma=as.numeric(), slope=as.numeric(), intcpt=as.numeric() )
#rm(srt)
for (i in sigmas){
  model<-lm(lc~lfc, data=subset(trees_fia, Sig_c_f== i))
  srt <-cbind(i, as.numeric(model$coefficients[1]), model$coefficients[2])
  sig_coef_c<-rbind(sig_coef_c, srt)
}

names(sig_coef_c)<-c("sigma", "c_int", "carbon_slope")
names(sig_coef)<-c("sigma", "intercept", "tree_slope")

sig_coef<-cbind(sig_coef, sig_coef_c[, -1])
# colnames(sig_coef)[4]<-"c_int"
# colnames(sig_coef)[5]<-"c_slope"

# 3a. Pivot the table to put coefficients in the same columns. 

sig_pivot<-pivot_longer(sig_coef, c(carbon_slope, tree_slope), names_to=c("type"), values_to="slope_value")

# 3b. Plot slopes from the tree cover regression and the carbon regression: 
# 

slopes<-ggplot(data=sig_pivot, aes(x=sigma, y=slope_value, col=type))+
  geom_point(cex=3)+ 
  ylim(-0.08, 0.95)+ 
  theme_bw()+
  ggtitle("Linear Model Slopes by Sigma")

# png("tree&carbon_slopes.png", width =700, height=700)
slopes
# dev.off()

# 3c. zoom into the 0 to 4 region on the x-axis, add ticks in the 0 to 1 stretch 
slopes_zoom<-ggplot(data=sig_pivot, aes(x=sigma, y=slope_value, col=type))+
  geom_point(cex=3)+ 
  ylim(-0.08, 0.95)+ xlim(0, 4.3)+
  theme_bw()+
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1, 2, 3, 4), limits=c(0, 4.5)) +
  theme(axis.text.x = element_text(face="bold", size=12,  angle=45))+ # rotate x-axis text for easier viewing. 
  ggtitle("Linear Model Slopes by Sigma")+
  theme(plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "gray41", size = 14, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "gray65", face = "italic"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

slopes_zoom

# png("./outputs/slopes_zoomed.png", width=700, height=700)

slopes_zoom
# dev.off()


####### re-create MSE by sigma and distance plot to examine the effect of physical distance and climate #########
#### Plan of work: 
#### 1. regress analog tree cover on focal tree cover data, get the residuals (for a reasonable sigma, say within 2 sigmas)
#### get the residuals from that regression. the residuals. Doing this should neutralize the effect of focal tree cover on 
#### the error of the analog tree cover. 
### 2. create an hexbin plot calculating MSE for each bin, with climate distance on horizontal axis, physical distance on the 
### vertical axis and hexbins colored by the mean residual size. 

#1. the regression
head(trees_fia)

# if i use all data, then the slope is flat
tr_tr<-lm(analog_trees~focal_trees, data=trees_fia)
summary(tr_tr)


plot (analog_trees~focal_trees, data=trees_fia)
abline(a=tr_tr$coefficients[1], b=tr_tr$coefficients[2], col='red')

# try sigma<=3.# slope of 0.5 for Sigma<=3. 

trees_fia_s3<-subset(trees_fia, Sigma<=3)
tr_tr3<-lm(analog_trees~focal_trees, data=trees_fia_s3)
summary(tr_tr3)

# gather the residuals: 
nrow(trees_fia_s3)
trees_fia_s3$resid<-tr_tr3$residuals

#2. Create a hexbin plot of residuals by climate and physical distance. 
head(trees_fia_s3)

residual_hexbin<-ggplot(data=trees_fia_s3, aes(x=km, y=MD, z=abs(tree_resid)))+
stat_summary_hex(fun="mean", bins=50)+
viridis::scale_fill_viridis(limits  = c(0, 30),na.value='red',name='mean \nabs(residual)\n') +
ggtitle("Residuals from analog ~ focal tree cover mapped by climatic \nand physical distance. (Sigma<=3)\n")+
theme_bw()+
theme(plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "gray41", size = 14, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "gray65", face = "italic"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

png("./outputs/residuals_byMD&km.png")
residual_hexbin
dev.off()

# for a given distance, residuals are the lowest at the lowest climate distance, but do not vary in the expected way 
# for a given climate distance. 

#3. Map the difference between focal and analog pixel divided by focal tree cover instead of pure residual: 
colnames(trees_fia_s3)[25]<-"tree_resid"
trees_fia_s3$tree_ratio<-abs(trees_fia_s3$focal_trees-trees_fia_s3$analog_trees)/(trees_fia_s3$focal_trees+0.01) # add a small value to avoid dividing by zero

#3a. Look at whether residuals vary with focal tree cover: 
ggplot(data=trees_fia_s3, aes(x=km, y=tree_ratio))+
stat_binhex(aes(fill=log10(..count..)), bins=100)+
  viridis::scale_fill_viridis(limits  = c(0, 4),na.value='yellow',name='log10 \nabs(cover difference ratio)\n') +
  ylim(0, 5)
# ratio values weakly deteriorate with distance


# 3b. hexbin plot by climate and physical distance with tree cover ratio instead of residuals. Get a similar result. 
# for a given physical distance, lowest ratio is found at lowest climate distance. 
# for all climate distances, lowest ratio is found at the lowest physical distance (<100 km away)

ratio_hexbin<-ggplot(data=trees_fia_s3, aes(x=km, y=MD, z=tree_ratio))+
  stat_summary_hex(fun="mean", bins=100)+
  viridis::scale_fill_viridis(limits  = c(0, 5),na.value='yellow',name='mean \nratio\n') +
  ggtitle("tree cover difference ratio mapped by climatic \nand physical distance. (Sigma<=3)\n")+
  theme_bw()+
  theme(plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "gray41", size = 14, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "gray65", face = "italic"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))


ratio_hexbin

# 3c. also look at the density of observations. Looks like the residual and the 
# ratio pattern follows the observation density - better residuals/lower error when fewer observations are available


data_density<-ggplot(data=trees_fia_s3, aes(x=km, y=MD))+
  stat_binhex(aes(fill=log10(..count..)),bins=50) +
  viridis::scale_fill_viridis(limits  = c(0, 3.5),na.value='red',name='log10 \n(# observations)\n') +
  ggtitle("Density of observations. (Sigma<=3)")+
  theme_bw()+
  theme(plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "gray41", size = 14, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "gray65", face = "italic"),
        axis.title.x = element_text(size = 15),-
        axis.title.y = element_text(size = 15))
data_density
png("./outputs/observation_density.png")
data_density
dev.off()


#3c. Look at the variability of each hexbin (vs mean estimate that we looked at above)

variability<-ggplot(data=subset(trees_fia_s3, focal_trees>=1), aes(x=km, y=MD, z=tree_ratio))+
  stat_summary_hex(fun=function(z) sd(z), bins=50)+
  viridis::scale_fill_viridis(limits  = c(0, 10),na.value='red',name='sd(ratio)') +
  ggtitle("ratio variability by climatic and physical distance. (Sigma<=3)", "focal pixels with <1% cover excluded")+
  theme_bw()+
  theme(plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "gray41", size = 14, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "gray65", face = "italic"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))
png("./outputs/ratio_variability.png")
variability
dev.off()

#3d. Look at the variability of residuals in each hexbin (vs ratio looked at above)

resid_variability<-ggplot(data=subset(trees_fia_s3, focal_trees>=1), aes(x=km, y=MD, z=abs(tree_resid)))+
  stat_summary_hex(fun=function(z) sd(z), bins=50)+
  viridis::scale_fill_viridis(limits  = c(0, 20),na.value='red',name='sd(residual)') +
  ggtitle("residual variability by climatic and physical distance. (Sigma<=3)")+
  theme_bw()+
  theme(plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "gray41", size = 14, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "gray65", face = "italic"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))
png("./outputs/residual_variability.png")
resid_variability
dev.off()


# plot residuals by MD: 

ggplot(data=trees_fia_s3, aes(x=MD, y=abs(tree_resid)))+
  stat_binhex(aes(fill=log10(..count..)),bins=50)+
  geom_abline(intercept=res_md$coefficients[1], slope=res_md$coefficients[2])

res_md<-lm(abs(tree_resid)~MD+km, data=trees_fia_s3)
summary(res_md)

res_km_md<-lm(abs(tree_resid)~km +MD +km*MD, data=trees_fia_s3)
summary(res_km_md)

res_km<-lm(abs(tree_resid)~km, data=trees_fia_s3)
summary(res_km)
vif(res_km_md)


#################### can I subsample my data based on analog distance? 

head(trees_fia_s3)

# what are the values of km_bin? 
unique(trees_fia_s3$km_bin)

# how many observations per bin? 
hist(trees_fia_s3$km_bin)
abline(a=1000,b=0, col='red')


# subsample to 853 observations: the number of observations in the last kilometer bin
nrow(subset(trees_fia_s3, km_bin==1500))

trees_sub853 <- subset(trees_fia_s3, km_bin<=1450) %>% group_by(km_bin) %>% sample_n(853)
head(trees_sub853)

hist(trees_sub853$km_bin)

# subsample again, but 0.25 sigma level: 
trees_sub853$sigma_level<-floor(trees_sub853$Sigma/0.25)
trees_sub<-trees_sub853%>%group_by(sigma_level)%>%sample_n(1000)


#look at data density now: 

data_density<-ggplot(data=trees_sub, aes(x=km, y=MD))+
  stat_binhex(aes(fill=log10(..count..)),bins=50) +
  viridis::scale_fill_viridis(limits  = c(0, 2),na.value='red',name='log10 \n(# observations)\n') +
  ggtitle("Density of observations. (Sigma<=3)")+
  theme_bw()+
  theme(plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "gray41", size = 14, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "gray65", face = "italic"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))
data_density

png("./outputs/subsampled_density.png")
data_density
dev.off()

# now re-plot residual variability with the subset: 

resid_variability<-ggplot(data=subset(trees_sub, focal_trees>=1),  aes(x=km, y=MD, z=abs(tree_resid)))+
  stat_summary_hex(fun=function(z) sd(z), bins=30)+
  viridis::scale_fill_viridis(limits  = c(0, 25),na.value='red',name='sd(residual)') +
  ggtitle("residual variability by climatic and physical distance. (Sigma<=3)")+
  theme_bw()+
  theme(plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "gray41", size = 14, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "gray65", face = "italic"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

png("./outputs/sub_residual_variability.png")
resid_variability
dev.off()


# now re-plot ratio variability with the subset: 

ratio_variability<-ggplot(data=subset(trees_sub, focal_trees>=1),  aes(x=km, y=MD, z=tree_ratio))+
  stat_summary_hex(fun=function(z) sd(z), bins=30)+
  viridis::scale_fill_viridis(limits  = c(0, 5),na.value='grey',name='sd(residual)') +
  ggtitle("ratio variability by climatic and physical distance. (Sigma<=3)")+
  theme_bw()+
  theme(plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "gray41", size = 14, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "gray65", face = "italic"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

png("./outputs/sub_ratio_variability.png")
ratio_variability
dev.off()

# mean ratios: 
ratio_hexbin<-ggplot(data=subset(trees_sub, focal_trees>=1), aes(x=km, y=MD, z=tree_ratio))+
  stat_summary_hex(fun="mean", bins=30)+
  viridis::scale_fill_viridis(limits  = c(0, 3),na.value='grey',name='mean \nratio\n') +
  ggtitle("difference ratio mapped by climatic \nand physical distance. (Sigma<=3)\n", "focal pixels with tree cover < 1% excluded")+
  theme_bw()+
  theme(plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "gray41", size = 14, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "gray65", face = "italic"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))
png("./outputs/sub_mean_ratio.png")
ratio_hexbin
dev.off()


# mean residuals: 

resid_hexbin<-ggplot(data=trees_sub, aes(x=km, y=MD, z=abs(tree_resid)))+
  stat_summary_hex(fun=function(z) mean(abs(z)), bins=30)+
  viridis::scale_fill_viridis(limits  = c(0, 35),na.value='grey',name='mean \nresidual\n') +
  ggtitle("sub-sampled residuals mapped by climatic \nand physical distance. (Sigma<=3)\n")+
  theme_bw()+
  theme(plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "gray41", size = 14, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "gray65", face = "italic"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

png("./outputs/sub_residuals_byMD&km.png")
resid_hexbin
dev.off()


ggplot(data=trees_sub, aes(x=MD, y=tree_resid))+
  geom_point()


ggplot(data=trees_sub, aes(x=focal_trees, y=analog_trees))+
  geom_point()


#### also try partial regression. 1. regress analog cover ratio on sigma. 
# 2. regress sigma on distance.. 3. regress residuals from 1 on residuals from 2? 

### do partial regression with analog~focal residuals, use data for Sigma<=3, because those are the data that
### we are interested in. 

#. Make explanatory variables range from 0 to 1. 
km_min<- min(trees_fia_s3$km)
km_range<-max(trees_fia_s3$km - km_min)
trees_fia_s3$st_km<-(trees_fia_s3$km - km_min)/km_range

md_range<-max(trees_fia_s3$MD)-min(trees_fia_s3$MD)
trees_fia_s3$st_md<-(trees_fia_s3$MD-min(trees_fia_s3$MD))/md_range


head(trees_fia_s3)
# calculate tree cover difference ratio for all plots/all sigma levels: 
trees_fia$tree_ratio<-abs(trees_fia$focal_trees-trees_fia$analog_trees)/(trees_fia$focal_trees+0.01)

# plot tree cover difference ratio by sigma level: 
ggplot(data=subset(trees_fia_s3, focal_trees>=1), aes(x=st_md,  y=tree_ratio))+
  stat_binhex(aes(fill=log10(..count..)),bins=100)+
  #geom_vline(xintercept = 4, col="yellow")+
  xlim(0, 1)

# 1. regress cover ratio on MD
ratio_lm<-lm(tree_ratio~st_km, data=subset(trees_fia_s3, focal_trees>=1))  
summary(ratio_lm)  

#2. regress MD on distance: 
md_lm<-lm(st_md~st_km, data=subset(trees_fia_s3, focal_trees>=1))
summary(md_lm)

ggplot(data=subset(trees_fia_s3, focal_trees>=1), aes(x=st_km, y=st_md))+
  stat_binhex(aes(fill=log10(..count..)),bins=100)+
  geom_abline(intercept=md_lm$coefficients[1], slope=md_lm$coefficients[2])

?geom_abline  

str(trees_fia_s3)

# 3. regress residuals from 1 on residuals on 2. 
res1<-ratio_lm$residuals # ratio by MD residuals
res2<-md_lm$residuals # md by km residuals. 
hist(res1, breaks=100)
p_reg<-data.frame(cbind(res1, res2))
str(p_reg)

range(p_reg$res1)
p_regr<-lm(abs(res1)~abs(res2), data=p_reg)
summary(p_regr)

ggplot(data=p_reg, aes(x=abs(res2), y=abs(res1)))+
  stat_binhex(aes(fill=log10(..count..)),bins=100)+
  ylab("tree cover ratio not explained by distance") + xlab("climate variability not explained by distance")+
  ggtitle("Partial regression ")+
  ylim(-2.5, 10)+
  geom_abline(intercept=p_regr$coefficients[1], slope=p_regr$coefficients[2], col="yellow")


############# analog - focal residual partial regression
### subsample by distance bin and then run regression? 
head(trees_fia_s3)
hist((trees_fia_s3$tree_resid))

#1a. residuals regressed on km: 
r1<-lm(analog_trees~km+focal_trees, data=trees_fia_s3)
summary(r1)
hist(r1$residuals)

r_test<-lm(analog_trees~focal_trees+MD+km, data=trees_fia_s3)
summary(r_test)
vif(r_test)
#1b. residuals regressed on km, subsampled dataset: 
r1b<-lm(analog_trees~km+focal_trees, data=trees_sub)
summary(r1b)
hist(r1b$residuals)


#2a. md regressed on km: 
r2<-lm(MD~km+focal_trees, data=trees_fia_s3)
summary(r2)

#2b. as in 2a, but with a subsample dataset: 
r2b<-lm(MD~km+focal_trees, data=trees_sub)
summary(r2b)


cover_res<-data.frame(cbind(r1$residuals, r2$residuals))
sub_res<-data.frame(cbind(r1a$residuals, r2b$residuals))
#3a. plot and model: 
head(sub_res)
ggplot(data=cover_res, aes(x=abs(X2), y=abs(X1)))+
  stat_binhex(aes(fill=log10(..count..)),bins=100)
  
  