### This file compares slopes of lines fit to biomass and tree cover data and checks the normality assumption for 

### Data #####################
data_out<-readRDS("./data/fia_tree_cover4km.rds")
trees_fia<-subset(data_out, !(is.na(analog_trees)))


### Libraries ################
library(tidyverse)



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

# set up an empty dataframe where to store slope and intercept from the analog ~ focal linear models: 

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

png("tree&carbon_slopes.png", width =700, height=700)
slopes
dev.off()

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

png("./outputs/slopes_zoomed.png", width=700, height=700)

slopes_zoom
dev.off()

?scale_x_continuous

plot(slope~sigma, data=sig_coef)
