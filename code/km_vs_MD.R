# Date: 12/7/2020
# epxloring relationship between distance and MD

############### Data ######################
data_out<-readRDS("./data/fia_tree_cover4km.rds")
ncol(data_out)
colnames(data_out)[24]<-("analog_trees")
trees_fia<-subset(data_out, !(is.na(analog_trees)))
trees_fia$cover_d<-trees_fia$focal_trees-trees_fia$analog_trees
trees_fia$tree_ratio<-trees_fia$cover_d/trees_fia$focal_trees

# calculate MD ceiling
trees_fia$MD_c<-ceiling(trees_fia$MD)
head(trees_fia)

############## Libraries #####################
library(plyr)
library(ggformula)
### 1. What is the relationship between MD and km at different MD levels (a: all Sigmas, b: Sigma<=2)
### 2. What is the relationship between km and the quality of biomass/tree cover prediction? 
### 3. Why do we see a Sigma signal in the slopes/splines, but not residuals? Maybe instead of residuals, look at pure difference
### between predicted and actual. So, look at error. At higher sigma values the quality of the linear fit decreases. So residuals do 
### not necessarily represent the quality of the fit. 

####################################1. relationship between MD and km ######################################################
# a. for all sigma levels: 
km_vs_md<-ggplot(data=trees_fia, aes(x=km, y=MD))+
  stat_binhex(aes(fill=log10(..count..)), bins=100)+
  geom_smooth(method='lm', colour='yellow')+
  ggtitle("Relationship between MD and KM, all MD's")

png("./outputs/km_vs_md.png")  
km_vs_md
dev.off()

#b. for sigmas<=2: 
trees_fia$sigma_break<-ifelse(trees_fia$Sigma<=2, 0, 1)
  
  
  
  
km_vs_md2<-ggplot(data=trees_fia, aes(x=km, y=MD, z=sigma_break))+
  #stat_binhex(aes(fill=log10(..count..)), bins=100)+
  geom_smooth(method='lm', colour="yellow")+
  geom_smooth(data=subset(trees_fia, Sigma<=2), method="lm", colour="pink")+
  ggtitle("Relationship between MD and KM, Sigma<=2 (pink); all sigmas (yellow)")+
  theme_bw()

png("./outputs/km_vs_md_different_sigmas.png")  
km_vs_md2
dev.off()
####################2. relationship between km and quality of biomass prediction  ##############################


# for all sigmas (weak, positive relationship)
coverbykm<-lm(tree_ratio~km, data=subset(trees_fia, focal_trees>=1))
summary(coverbykm)

coverbymd<-lm(tree_ratio~MD, data=subset(trees_fia, focal_trees>=1))
summary(coverbymd)

coverbysigkm<-lm(tree_ratio~MD+km, data=subset(trees_fia, focal_trees>=1))
summary(coverbysigkm)


# cover ratio by distance: 
ggplot(data=subset(trees_fia, focal_trees>=1), aes(x=km, y=tree_ratio))+
  stat_binhex(aes(fill=log10(..count..)), bins=100)+
  geom_smooth(method='lm', colour='yellow')
  
# cover ratio by MD: 
ggplot(data=subset(trees_fia, focal_trees>=1), aes(x=MD, y=tree_ratio))+
  stat_binhex(aes(fill=log10(..count..)), bins=100)+
  geom_smooth(method='lm', colour='yellow')+
  geom_spline(colour="green", nknots=20, cex=2)
  


# cover ratio by distance for sigma<=2: 
coverbykm_s2<-lm(tree_ratio~km, data=subset(trees_fia, focal_trees>=1&Sigma<=2))
summary(coverbykm_s2)

# cover ratio by distance: 
ratio_by_km<-ggplot(data=subset(trees_fia, focal_trees>=1&Sigma<=2), aes(x=km, y=tree_ratio))+
  stat_binhex(aes(fill=log10(..count..)), bins=100)+
  geom_smooth(method='lm', colour='yellow')+
  geom_spline(colour="green", nknots=20, cex=2)+
  ggtitle("tree cover ratio by km, Sigma<=2")
  #ylim(0, 10)

png("./outputs/ratio_by_kms2.png")
ratio_by_km
dev.off()

# cover ratio by md for sigma<=2: 
coverbymd_s2<-lm(tree_ratio~MD, data=subset(trees_fia, focal_trees>=1&Sigma<=2))
summary(coverbykm_s2)

# cover ratio by md and km: 
coverby_mdkm<-lm(tree_ratio~MD+km+MD*km, data=subset(trees_fia, focal_trees>=1&Sigma<=2))
summary(coverby_mdkm)


coverby_mdkm_all<-lm(tree_ratio~MD+km+MD*km, data=subset(trees_fia, focal_trees>=1))
summary(coverby_mdkm_all)


# cover ratio by sigma plot: 
ggplot(data=subset(trees_fia, focal_trees>=1&Sigma<=2), aes(x=MD, y=tree_ratio))+
  stat_binhex(aes(fill=log10(..count..)), bins=100)+
  geom_smooth(method='lm', colour='yellow')+
  geom_spline(colour="green", nknots=20, cex=2)+
  ggtitle("tree cover ratio by MD, Sigma<=2")

# collect slopes into a dataframe: 
#rm(slopes)
slopes<-data.frame(rbind(coverbykm$coefficients[2], coverbykm_s2$coefficients[2], coverbymd$coefficients[2], coverbymd_s2$coefficients[2]))
slopes$model<-cbind(rbind('km', 'km_sigma2','md', 'md_sigma2'))
names(slopes)<-c("slope", "model")
slopes$sigma<-c("all", "<=2", "all", "<=2")
slopes$model<-c("km", "km", "md", "md")
slopes$variable<-c("km", "km", "md", "md")

#rm(both)
both<-data.frame(rbind(coverby_mdkm$coefficients[2], coverby_mdkm$coefficients[3], coverby_mdkm$coefficients[4]))
both$model<-rbind("md+km", "md+km", "md+km")
both$sigma<-rbind("<=2", "<=2", "<=2")
both$variable<-rbind("md", "km", "md*km")
colnames(both)[1]<-"slope"

slopes<-rbind(slopes, both)
slopes
gg_slope<-ggplot(data=slopes, aes(x=model, y=slope, z=variable))+
  geom_point(aes(color=variable, shape=sigma), cex=3)+
  ggtitle("linear regression slopes")+
  theme_bw()

png("./outputs/slopes_km_md.png")
gg_slope
dev.off()

### measure prediction quality as pure difference between 
### standardize MD and km, to be able to compare their relative influence: 
sigma2_trees<-subset(trees_fia, Sigma<=2)
sigma2_trees$standard_md<-(sigma2_trees$MD-min(sigma2_trees$MD))/(max(sigma2_trees$MD))
sigma2_trees$standard_km<-(sigma2_trees$km-min(sigma2_trees$km))/(max(sigma2_trees$km))


# influence of distance: 
km_st<-lm(tree_ratio~standard_km, data=subset(sigma2_trees, focal_trees>=1))
summary(km_st)

# influence of md: 
md_st<-lm(tree_ratio~standard_md, data=subset(sigma2_trees, focal_trees>=1))
summary(md_st)

#md and km together: 
md_km_st<-lm(tree_ratio~standard_md*standard_km, data=subset(sigma2_trees, focal_trees>=1))
summary(md_km_st)

ggplot(data=subset(trees_fia, focal_trees>=1&Sigma<=2), aes(x=MD, y=tree_ratio))+
  stat_binhex(aes(fill=log10(..count..)), bins=100)+
  geom_smooth(method='lm', colour='yellow')+
  geom_spline(colour="green", nknots=20, cex=2)+
  ggtitle("tree cover ratio by MD, Sigma<=2")





# finally, rerun the analyses with un-weighed cover difference. Rationale for using pure cover difference: 
# for focal plots with low tree cover, ratio will always be high, for focal plots with high tree cover ratio will always be low. 
# that is not informative. I.e, there is an iverse relationship between focal tree cover and difference ratio. Include focal 
# tree cover in the model, to account for that influence: 
head(sigma2_trees)
sigma2_trees$cover_d<-sigma2_trees$focal_trees-sigma2_trees$analog_trees


# visualize the relationship of response variables (ratio or difference) with focal tree cover: 
ratio<-ggplot(data=subset(sigma2_trees, focal_trees>=1), aes(y=tree_ratio, x=focal_trees))+
  stat_binhex(aes(fill=log10(..count..)), bins=100)+
  geom_smooth(method='lm', colour='yellow')+
  geom_spline(colour="green", nknots=20, cex=2)+
  ggtitle("difference ratio as a function of focal tree cover")
png("./outputs/ratio_by_focal_cover.png")
ratio
dev.off()

diff<-ggplot(data=trees_fia, aes(y=cover_d, x=focal_trees))+
  stat_binhex(aes(fill=log10(..count..)), bins=100)+
  geom_smooth(method='lm', colour='yellow')+
  geom_spline(colour="green", nknots=20, cex=2)+
  ggtitle("Cover difference as a function of focal tree cover")+
  labs(x="Focal tree cover", y="Focal-analog tree cover")+
  theme_bw()

png("./outputs/cover_differece_by_focal_cover.png")
diff
dev.off()
max(trees_fia$focal_trees)

# take residuals from difference~ focal cover regression: 
cov_resid<-lm(cover_d~focal_trees, trees_fia)
summary(cov_resid)


trees_fia$nf_resid<-cov_resid$residuals
par(mfrow=c(2, 2))
hist(trees_fia$cover_d, main = "cover difference distribution")
hist(trees_fia$focal_trees, main="focal tree cover dist")
hist(trees_fia$nf_resid, main="cover difference residual distribution")
hist(log(trees_fia$tree_ratio))


# influence of climate distance independent of physical distance: 

diff1<-ggplot(data=trees_fia, aes(y=nf_resid, x=focal_trees))+
  stat_binhex(aes(fill=log10(..count..)), bins=100)+
  geom_smooth(method='lm', formula="y~x", colour='yellow')+
  geom_spline(colour="green", nknots=20, cex=2)+
  ggtitle("Residuals as a function of focal tree cover")+
  labs(x="Focal tree cover", y="lm(difference~focal cover) residuals")+
  theme_bw()

png("./outputs/cover_residuals_by_focal_cover.png")
diff1
dev.off()



diff2<-ggplot(data=sigma2_trees, aes(y=nf_resid, x=focal_trees))+
  stat_binhex(aes(fill=log10(..count..)), bins=100)+
  geom_smooth(method='lm', formula="y~x", colour='yellow')+
  geom_spline(colour="green", nknots=20, cex=2)+
  ggtitle("Residuals as a function of focal tree cover")+
  labs(x="Focal tree cover", y="lm(difference~focal cover) residuals")+
  theme_bw()


########## Repeat the MD by km plot, using nf_resid and nf_resid (nf for no focal influence) variability: 


# residuals by md and km (no strong pattern)
residual_hexbin<-ggplot(data=trees_fia, aes(x=km, y=MD, z=abs(nf_resid)))+
  stat_summary_hex(fun="mean", bins=50)+
  viridis::scale_fill_viridis(limits  = c(0, 40),na.value='red',name='mean \nabs(residual)\n') +
  ggtitle("Residuals from analog difference ~ focal tree cover \n mapped by climatic and physical distance. \n")+
  ylim(0, 5)+
  theme_bw()+
  theme(plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "gray41", size = 14, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "gray65", face = "italic"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))
png("./outputs/mean_no_focal_influence_residuals_by_mdkm_zoomed.png")
residual_hexbin
dev.off()

# residual variability by md and km: 
residual_variability<-ggplot(data=trees_fia, aes(x=km, y=MD, z=nf_resid))+
  stat_summary_hex(fun=function(z) sd(z), bins=50)+
  viridis::scale_fill_viridis(limits  = c(0, 40),na.value='red', name='SD \n(residuals)\n') +
  ggtitle("Variability of residuals ", subtitle = "")+
  ylim(0, 5)+
  theme_bw()+
  theme(plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "gray41", size = 14, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "gray65", face = "italic"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

png("./outputs/residual_variability_zoomed.png")
residual_variability
dev.off()

##########2. finally partial regression of ratio on md, and then on km#############
# regress ratio on 
test<-lm(nf_resid~standard_km, data=sigma2_trees)
summary(test)

test1<-lm(standard_md~standard_km, data=sigma2_trees)
summary(test1)

p_reg<-data.frame(cbind(test$residuals, test1$residuals))

p_reg_plot<-ggplot(data=p_reg, aes(x=test1$residuals, y=test$residuals))+
  stat_binhex(aes(fill=log10(..count..)), bins=100)+
  #geom_spline(colour="green", nknots=20, cex=2)+
  theme_bw()+
  labs(x="climate distance not explained by physical distance", y="cover residuals not explained by physical distance")+
  ggtitle("Partial Regression Plot. Infulence of climate distance after accounting for physical distance.")

png("./outputs/partial_regression_plot.png")  
p_reg_plot
dev.off()


### partial regression for the independ influence of distance: 

test2<-lm(nf_resid~standard_md, data=sigma2_trees)
summary(test2)

test3<-lm(standard_km~standard_md, data=sigma2_trees)
summary(test3)

p_reg1<-data.frame(cbind(test2$residuals, test3$residuals))


p_reg_plot1<-ggplot(data=p_reg, aes(x=test3$residuals, y=test2$residuals))+
  stat_binhex(aes(fill=log10(..count..)), bins=100)+
  #geom_spline(colour="green", nknots=20, cex=2)+
  theme_bw()+
  labs(x="physical distance not explained by climate distance", y="cover residuals not explained by climate distance")+
  ggtitle("Partial Regression Plot",  subtitle = "Infulence of physical distance after accounting for climate distance.")


png("./outputs/partial_regression_km.png")
p_reg_plot1
dev.off()

############# look at individual influence of km and md on mean residuals and residual variability:
head(trees_fia)
error_by_MD<-ggplot(data=trees_fia, aes(x=MD, y=nf_resid))+
  stat_binhex(aes(fill=log10(..count..)), bins=50)+
  labs(x="MD ", y="residuals")+
  ggtitle("Tree cover residuals by Mahalanobis distance")

png("./outputs/prediction_error_by_MD.png")
error_by_MD
dev.off()

error_by_km<-ggplot(data=trees_fia, aes(x=km, y=nf_resid))+
  stat_binhex(aes(fill=log10(..count..)), bins=50)+
  labs(x="km ", y="residuals")+
  ggtitle("Tree cover residuals by physical distance")


png("./outputs/prediction_error_by_km.png")
error_by_km
dev.off()
error_by_km

### variability of residuals by MD and by km: 
head(trees_fia)

# residuals do get larger, eventually(the boxplot shows the median, not the mean). 
boxplot_by_MD<-ggplot(data=trees_fia, aes(x=as.factor(MD_c), y=nf_resid))+
  geom_boxplot()+
  labs(x="MD", y="residual boxplot")+
  ggtitle("Tree cover residual variabiity by Mahalanobis distance")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))
png("./outputs/residuals_by_MD.png")
boxplot_by_MD
dev.off()

# residuals by distance: 
boxplot_by_km<-ggplot(data=trees_fia, aes(x=as.factor(km_bin), y=nf_resid))+
  geom_boxplot()+
  labs(x="km bin", y="residual boxplot")+
  ggtitle("Tree cover residual variabiity by physical distance")+
  theme(axis.text.x = element_text(angle = 90))

png("./outputs/residual_by_km.png")
boxplot_by_km
dev.off()

# variance by climate distance: 
sd_by_sig<-ddply(trees_fia, .(Sig_c_f), summarize, resid_sd=sd(nf_resid), mean_residual=mean(nf_resid))
# plot(resid_var~Sig_c_f, data=var_by_sig)
# plot(mean_residual~Sig_c_f, data=var_by_sig)


ggplot(data=sd_by_sig, aes(x=Sig_c_f, y=resid_sd))+
  geom_point(aes(x=Sig_c_f, y=resid_sd))+
  geom_point(aes(x=Sig_c_f, y=mean_residual), col='red')


sd_by_md<-ddply(trees_fia, .(MD_c), summarize, resid_sd=sd(nf_resid), mean_residual=mean(nf_resid))
# plot(resid_var~MD_c, data=var_by_md)

res_by_md<-ggplot(data=sd_by_md, aes(x=MD_c, y=resid_sd))+
  geom_point(aes(x=MD_c, y=resid_sd))+
  geom_point(aes(x=MD_c, y=mean_residual), col='red')+
  labs(title="mean (red) and sd (black) of cover residuals by Mahalanobis distance", x="MD", y="mean/sd of the residuals")

png("./outputs/res_by_md.png")
res_by_md
dev.off()


# variance by physical distance: 
sd_by_km<-ddply(trees_fia, .(km_bin), summarize, resid_sd=sd(nf_resid), mean_residual=mean(nf_resid))

res_by_km<-ggplot(data=sd_by_km, aes(x=km_bin, y=resid_sd))+
  geom_point(aes(x=km_bin, y=resid_sd))+
  geom_point(aes(x=km_bin, y=mean_residual), col='red')+
  labs(title="mean (red) and sd (black) of cover residuals by km", x="km", y="mean/sd of the residuals")
png("./outputs/res_by_km.png")
res_by_km
dev.off()

# plot(resid_sd~km_bin, data=sd_by_km, main="Residual SD by Distance Bin", ylab="Residual SD", xlab="kilometers")
# 
# sd_by_md<-ddply(trees_fia, .(MD_c), summarize, resid_sd=sd(nf_resid))
# plot(resid_sd~MD_c, data=sd_by_md)
