### This file compares slopes of lines fit to biomass and tree cover data and checks the normality assumption for 

### Data #####################
data_out<-readRDS("./data/fia_tree_cover4km.rds")
ncol(data_out)
colnames(data_out)[24]<-("analog_trees")
trees_fia<-subset(data_out, !(is.na(analog_trees)))

### Libraries ################
library(tidyverse)
library(ggplot2)


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

ggplot(data=trees_fia_s3, aes(x=km, y=MD, z=abs(resid)))+
stat_summary_hex(fun="mean", bins=50)+
viridis::scale_fill_viridis(limits  = c(0, 50),na.value='yellow',name='mean \nabs(residual)\n') +
ggtitle("Residuals from Analog ~ focal tree cover mapped by climatic and physical distance")+
theme_bw()
