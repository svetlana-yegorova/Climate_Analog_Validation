# more on multicollinearity here: https://datascienceplus.com/multicollinearity-in-r/
# partial regression - which is really just a method to get the 
# coefficients in multiple regression 

# partial residual plot is a way to visualize the effect of individual 
# independent (or explanatory) variables.  
# to do a partial residual plot, plot residuals from the 

# partial residual vs partial regression plot 
# partial regression plot: 
# 1) take residuals from regressing the dependent variable
# on all other independent variables except for independent variable i
# 2) regress indep var i on all other indep vars in the full multivar regression
# 3) plot and regress redials from #1 against residuals in #2. 
# shows the correct strength of relationship between
# independent variable and dep var, but hard to interpret otherwise, because
# the horizontal axis represents the residualized variable. The slope of the regression
# in #3 should be equal to i's coefficient in the full multivar regression - need
# to check that. 

# partial residual plot: 
# on the y axis: residuals from the full model + biXi,  
# on the x axis - Xi. Makes the relationship between Xi and the
# response variable more understandable. 

# partial residual plot will have deflated variance if there is multi-collinearity
# in the model. Examine whether there is evidence of multi-collinearity. 
# plot a partial residual plot
# work with a small dataset to speed up processing. 
############# Data #####################
rm(AGC_hexes, biomass_plots, data_out, tree_hexes, trees_fia, tt, tabLabeller)
# 1200 focal plots data:
# data<-readRDS("./data/1200_pixels.rds")
# data<-subset(data, !is.na(tree_diff))


### load a smaller dataset (100 focal plots) to work with
data<-readRDS("./data/fia_tree_cover4km.rds")

# calculate tree difference for this dataset
data$tree_diff<-data$focal_trees-data$trees_fia


########### Libraries ###################
library(ggplot2)
library(tidyverse)
library(plyr)
library(car)

# library for Farrar-Glauber collinearity test: 
# #install.packages("mctest")
# library(mctest)
# #install.packages("ppcor")
# library(ppcor)
############ Examine dataset for evidence of collinearity ###########
# data<-subset(data, !is.na(data$tree_diff) & km<=1500)

data<-subset(data, !is.na(data$tree_diff))

# pairwise correlation (data limited to analogs within 1500 km to remove
# undue influence of non-analogs): 
# tree_dist<-cor(abs(data$tree_diff), data$km)
# 
# tree_md<-cor(abs(data$tree_diff), data$MD)
# 
# dist_md<-cor(data$km, data$MD)


# do not do matrix of scatterplots - t crashes R session, unless plot a subset of 
# data. 

# examine VIF, run multiple regression:
# standardize distance and MD to make coef interp easy.

# select the three columns for ease of handling
data1<-data%>%dplyr::select("MD", "Sigma", "km", "tree_diff")
# head(data1)



# standardize variables: 
# need to figure out how standaardize funciton works. 
# data1<-data1%>%mutate(md_st=scale(MD)[, 1] , km_st=scale(km)[, 1]) 
# data1
# mean(data1$md_st)
# sd(data1$md_st)
# test<-scale(data1$MD)
# str(test)
# head(test[ ,1])

# regression with unscaled variables: 
# limit data to MD ~ 13 (what is the associated sigma value? - 25)
# test<-filter(data, Sigma>3 & Sigma< 25)
m_reg15<-lm(abs(tree_diff)~km+MD, data=subset(data1, MD<=13))


# m_reg<-lm(abs(tree_diff)~km+MD, data=data1)
# summary(m_reg)
# summary(m_reg15)

# # regression with standardized/scaled variables: 
# sm_reg15<-lm(abs(tree_diff)~km_st+md_st, data=subset(data1, km<=1500))
# sm_reg<- lm(abs(tree_diff)~km_st+md_st, data=data1)
# summary(sm_reg)

# both distance and climate difference increase tree cover error. 
# each sd in distance increases error by 5%, each sd in md increases
# error by a little over 3%. 
# standard error for both coefficients is low... so doesn't suggest
# collinearity. 
# But! Rsquared is super low (0.06)


# looking at individual effects of distance and climate: 
test_lm<-lm(abs(tree_diff)~md_st, data=data1)
summary(test_lm)

test_lm1<-lm(abs(tree_diff)~km_st, data=data1)
summary(test_lm1)

# Calculate VIF (vif turns out to be ~ 1.12)
car::vif(m_reg)
vif(sm_reg)
vif(m_reg)
vif(m_reg15)
# low VIFs

#  other methods for examining collinearity: Farrar-Glaubert Test
head(data1)
X<-data1[, 3:5]
X$tree_diff<-abs(X$tree_diff)
head(X) 

omcdiag(sm_reg)
# ok, overall multicolinarity test detects
# fishy Farrar Chi-Square

imcdiag(sm_reg)

pcor(X, method="pearson")
# even though VIF is low, the pcor and Farrar-Glaubert
# tests indicate multicollinearity. I am still not sure it is 
# an issue... since my sd's are low. something to consult 
# a statistician about. 

#  assumptions of multiple regression: 
# using plot(sm_reg) takes forever to plot. Too many residuals. Have to 
# take a subset of them. 
# 1. errors/residuals are normally distributed, with a mean of 0
# 2. Residuals are constant across prdicted varables (not quite, residuals
# decrease slightly as fitted values increase
# 3. Covariance among errors - maybe plot errors by distance see what they look
# like. 

# so, take  10 or 20% of the dataset and plot the three plots mentioned above. 
# 
# # get the residuals from the model: 
# data1$ns_res<-m_reg$residuals
# # gonna need to make a different data_frame with MD<13:
data2<-filter(data1, MD<=13)

m_reg15<-lm(abs(tree_diff)~km+MD, data=data2)

reg15_km<-lm(abs(tree_diff)~km, data=data2)
reg15_clim<-lm(abs(tree_diff)~MD, data=data2)

summary(reg15_km)
summary(reg15_clim)
summary(m_reg15)
data2$ns_res13<-m_reg15$residuals


# get biXi values/md's contribution: 
# data1$md_co<-m_reg$coefficients[3]*data1$MD #calculates biXi
# data1$p_res<-data1$md_co+data1$ns_res # adds biXi and residuals: 

data2$md_co13<-m_reg15$coefficients[3]*data2$MD
data2$p_res13<-data2$md_co13+data2$ns_res13


## randomly take 5% of the data to examine residuals: 
# n=nrow(data1)*0.005
# samp<-sample_n(data1, n , replace=FALSE)

m=nrow(data2)*0.005
samp1<-sample_n(data2, m, replace=FALSE)

# plot residuals by distance: 
residuals_by_distance<-ggplot(data=samp1, aes(x=km, y=ns_res13))+
  geom_point(color='lightblue')+
  geom_smooth(method="lm")+
  geom_smooth(method="loess")+
  ylab("residuals")+
  ggtitle("tree cover delta ~ km + md residuals by distance")+
  theme_classic()+
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(size=20),
        plot.title = element_text(color = "black", size = 25, face = "bold", hjust = 0.5))


png("./outputs/residuals_by_distance.png", width=900, height=750)
residuals_by_distance
dev.off()

# do a partial residual plot with the sampled dataset. 
# plot residuals+the modeled effect of MD on tree cover against MD values: 


# add ticks or marks for Sigma 1 and 2 equivalencies: 

p_resid_plot<-ggplot(data=samp, aes(x=MD, y=p_res))+
  geom_point(color="lightblue")+
  geom_smooth(method="lm")+
  geom_smooth(method="loess", color='yellow')+
  geom_vline(xintercept=2.17, col="blue", lty=2)+
  geom_vline(xintercept = 3.12, col="blue", lty=2)+
  theme_classic()+
  ggtitle("Partial Residual Plot. Effect of Climate Similarity \n Independent of Distance", 
          subtitle="Slope of ~ 0.6%")+
  ylab("Partial residuals \n change in tree cover difference due to MD only")+
  coord_cartesian(xlim=c(0,12), ylim=c(-10, 20))+
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(size=20),
        plot.title = element_text(color = "black", size = 25, face = "bold", hjust = 0.5))

  
png("./outputs/partial_residual_plot_zoomed_in.png", width=750, height = 500)
p_resid_plot
dev.off()




# run regression on this "tiny" dataset: 
# regress on distance only: 
s_reg<-lm(abs(tree_diff)~ km_st, data=samp)
summary(s_reg)

# multiple regression: 
tiny_reg<-lm(abs(tree_diff)~km_st+md_st, data=samp)
summary(tiny_reg)

par(mfrow=c(2, 2))
plot(tiny_reg)

par(mfrow=c(1, 1))
plot(tiny_reg, 4)

# remove data instances 402, 1752, 9305 and rerun the regression: 
samp1<-samp[-c(402, 1752, 9305), ]
tiny_reg1<-lm(abs(tree_diff)~km_st+md_st, data=samp1)
summary(tiny_reg1)
summary(tiny_reg)


png("./outputs/m_reg_assumptions_check.png", height=1000, width=1000)
par(mfrow=c(2, 2))
plot(tiny_reg1) 
dev.off()

##### -------Do Regression Coefficients change
####---------If I remove data that is more than 1500 km away? 
####---------If I remove data that are more than 3.5 sigma away?
### The point of this exploration is to see if the relationship between 
### distance, climate and tree cover is driven by the faraway plots. 
### After sub-sampling, it does not appear to be.... when I limit data
### by physical distance, the coeffeicients change only slightly. 
### When I limit by Sigma (<4), they change more drastically, but not as 
### expected (I expected coeffificents to drop in value). Instead, 
### MD coefficient increases, km coefficient decrases and overall Rsquared
### also drops. 

### Km limit multiple regression; filter "large" dataset by distance, then 
### draw a "small" random sample: 
## randomly take 5% of the data to examine residuals: 

n=nrow(data1)*0.005
samp<-data1 %>% filter(km<=1500)%>%
      sample_n(n , replace=FALSE)

tiny_reg_km<-lm(abs(tree_diff)~km_st+md_st, data=samp)
summary(tiny_reg_km)
summary(tiny_reg)
# when limited by distance, regression coefficients for both climate and 
# distance get smaller. 


### limit by sigma (under 3.5): 
### R squared drops from ~ 0.07 to 0.01 and 
### coefficient size for MD grows from ~ 3 to 9. Why does limiting by 
### sigma has this effect? 
### Go with the "full" tiny dataset, do not limt by distance or sigma. 

n=nrow(data1)*0.005
samp<-data1 %>% filter(Sigma<4)%>%
  sample_n(n , replace=FALSE)

tiny_reg_s<-lm(abs(tree_diff)~km_st+md_st, data=samp)
summary(tiny_reg_s)
vif(tiny_reg_s)

durbinWatsonTest(tiny_reg1)
### what does scale-location test tell me? 
### equal variance assumption


# plot residual distribution: 
res_hist<-ggplot(data=samp, aes(x=res))+
  geom_histogram()+
  theme_classic()+
  ggtitle("residual distribution")

# residuals by MD and km (two plots). what md corresponds to sigma of 1 and 2?

head(data)
s1<-subset(data, Sigma>0.9 & Sigma<=1) # MD ~ 2.17
s2<-subset(data, Sigma>1.9 & Sigma<=2) # MD ~ 3.12

# By MD: 
res_md<-ggplot(data=samp, aes(x=MD, y=res))+
  geom_point()+
  geom_vline(xintercept=2.17, col="blue")+
  geom_vline(xintercept = 3.12, col="green")+
  theme_classic()+
  xlim(0, 10)+
  ggtitle("residuals by MD")

# By km: 
res_km<-ggplot(data=samp, aes(x=km, y=res))+
  geom_point()+
  # geom_vline(xintercept=2.17, col="blue")+
  # geom_vline(xintercept = 3.12, col="green")+
  theme_classic()+
  # xlim(0, 10)+
  ggtitle("residuals by km")

# autocorrelation (Durbin Watson test)
durbinWatsonTest(sm_reg)

############ partial residual plot ####################
# on the y axis: residuals from the full model + biXi,  
# on the x axis - Xi. Makes the relationship between Xi and the
# response variable more understandable. 

# model: 
sm_reg

head(data1)
# make the partial residual plot: 

## need to change the ticks on the bottom axis to match unstandardized
## md values
# calculate tick values: 
ticks<-c(0, 2.5, 5, 7.5)
us_ticks<-round(ticks*sd(data1$MD)+mean(data1$MD), digits=1)

# vertical lines at MDs equvalent to Sigma 1 (MD 2.17) and Sigma 2 (MD 3.12)
line1<-round((2.17-mean(data1$MD))/sd(data1$MD), digits=1)
line2<-round((3.12-mean(data1$MD))/sd(data1$MD), digits=1)

p_res<-ggplot(data=data1, aes(x=md_st, y=p_res))+
  stat_binhex(aes(fill=log(..count..)))+
  geom_smooth(method="lm", color="yellow")+
  geom_vline(xintercept = line1, lty=2)+
  geom_vline(xintercept = line2, lty=2, col='red')+

  ggtitle("Contribution of climate after accounting for \n\ the effect of distance")+
  theme_classic()+
  
 # scale_x_continuous(name="climate dissimilarity (MD)", breaks=ticks, labels=us_ticks, limits=c(-0.5, 5))+
  scale_y_continuous(name="Tree cover difference from focal cover \n\
  after accounting for distance (km)")+
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(size=20),
        plot.title = element_text(color = "black", size = 25, face = "bold", hjust = 0.5))
  
p_res 

# save the graph: 

png("./outputs/partial_residual_plot.png", height=700, width=1000)
p_res

dev.off()


# limit to 2 md sds: 

p_res1<-ggplot(data=data1, aes(x=md_st, y=p_res))+
  stat_binhex(aes(fill=log(..count..)))+
  geom_smooth(method="lm", color="yellow")+
  geom_vline(xintercept = line1, lty=2)+
  geom_vline(xintercept = line2, lty=2, col='red')+
  
  ggtitle("Contribution of climate after accounting for \n\ the effect of distance")+
  theme_classic()+
  xlim(-2, 2.5)+
  # scale_x_continuous(name="climate dissimilarity (MD)", breaks=ticks, labels=us_ticks, limits=c(-0.5, 5))+
  scale_y_continuous(name="Tree cover difference from focal cover \n\
  after accounting for distance (km)")+
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(size=20),
        plot.title = element_text(color = "black", size = 25, face = "bold", hjust = 0.5))

p_res1 


############ partial regression ####################
#1 regress tree cover difference on distance
#2 regress climate distance on geographical distance
#3 plot residuals from #1 against residuals in #2

head(data)

# plot a subset of tree_diff by physical distance: 
# subsamble by sigma level: 
data_sub1<-data%>% 
  group_by(focal_plot, Sigma_bin1)%>%
  slice_sample(n=50, replace=FALSE)
head(data_sub)
nrow(data_sub)

test_plot<-ggplot(data=subset(data_sub, Sigma<25), aes(x=dist, y=abs(tree_diff)))+
  stat_binhex(aes(fill=log(..count..)))+
  geom_smooth(method="lm", color="pink")+
  #geom_smooth(method="loess", se=FALSE, color="yellow")+
  labs(title="Tree cover ~ distance relationship", x="Distance", y="Absolute cover difference")+
  theme_classic()+
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(size=20),
        plot.title = element_text(color = "black", size = 25, face = "bold", hjust = 0.5))


png("./outputs/tree_cover_by_km.png", height=700, width=1000)
test_plot
dev.off()

#### plot climate distance by geographic distance: 
climate_plot<-ggplot(data=subset(data_sub, Sigma<25), aes(x=dist, y=Sigma))+
  stat_binhex(aes(fill=log(..count..)))+
  geom_smooth(method="lm", color="pink")+
  geom_smooth(method="loess", se=FALSE, color="yellow")+
  labs(title="Sigma ~ distance relationship", x="sqrt(Distance)", y="Sigma")+
  theme_classic()+
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(size=20), 
        plot.title = element_text(color = "black", size = 25, face = "bold", hjust = 0.5))#+
 # scale_x_continuous(breaks=c(0, 200, 500, 1000, 1500, 2000))

png("./outputs/sigma_by_km.png", height=700, width=1000)
climate_plot
dev.off()


#### not partial regression
#1 regress tree cover difference on distance: 
tree_lm<-lm(abs(tree_diff)~dist, data=subset(data_sub1, dist<1500))

#2 regress sigma on distance (or square root of dist):
climate_lm<-lm(MD ~ sqrt(dist), data=subset(data_sub1, dist<1500))


part_reg<-cbind(tree_lm$residuals, climate_lm$residuals)
part_reg<-data.frame(part_reg)
head(part_reg)
colnames(part_reg)<-c("trees", "climate")
str(part_reg)


partial_reg<-lm(tree_lm$residuals~climate_lm$residuals)
summary(partial_reg)



# plot the relationship between residuals: 
ggplot(data=part_reg, aes(x=climate, y=trees))+
  stat_binhex(aes(fill=log(..count..)))+
  #geom_smooth(method="loess", se=FALSE)
  geom_smooth(method="lm", color="yellow")+
  theme_classic()
  # 




summary(partial_reg)

##### test out using MD instead of Sigma ###########
#1 regress tree cover difference on distance: 
tree_lm<-lm(abs(tree_diff)~dist, data=subset(data_sub1, Sigma<25))

#2 regress sigma on distance (or square root of dist):
#2.1 but visualize first: 
# to speed up visualization calculate summaries per 10 km bin: 
head(data_sub1)
data_sub2<-data_sub1%>%
  filter(Sigma<25)%>%
  mutate(km_bin=ceiling(dist/10))

# variation with all sigmas: 
data_sub3<-data_sub1%>%
  #filter(Sigma<25)%>%
  mutate(km_bin=ceiling(dist/10))

head(data_sub2)
max(data_sub2$Sigma)

data3<-ddply(data_sub2, .(focal_plot, km_bin), summarize, mean_diff=mean(abs(tree_diff)))
head(data3)


dist_model<-ggplot(data=data3, aes(x=km_bin, y=mean_diff))+
  stat_binhex(aes(fill=log(..count..)))+
  geom_smooth(method="lm")+
  geom_smooth(method="loess", se=FALSE, color="yellow")+
  theme_classic()+
  ggtitle("Sigma<4")+
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(size=20), 
        plot.title = element_text(color = "black", size = 25, face = "bold", hjust = 0.5))

png("./outputs/dist_model_sigma4.png", height=700, width=900)
dist_model
dev.off()

### tree difference vs distance for all sigmas: 

data4<-ddply(data_sub3, .(focal_plot, km_bin), summarize, mean_diff=mean(abs(tree_diff)))
head(data4)

dist_model_all<-ggplot(data=data4, aes(x=km_bin, y=mean_diff))+
  stat_binhex(aes(fill=log(..count..)))+
  geom_smooth(method="lm")+
  geom_smooth(method="loess", se=FALSE, color="yellow")+
  theme_classic()+
  ggtitle("All Sigmas")+
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(size=20), 
        plot.title = element_text(color = "black", size = 25, face = "bold", hjust = 0.5))

png("./outputs/dist_model_sigma_all.png", height=700, width=900)
dist_model_all
dev.off()

### variation of relationship between distance and tree difference, this time
### limited by distance (instead of Sigma). Doing this to see if 
### the lack of distance-tree difference relationship is a result of removing 
### plots with sigma >4, or by limiting far distances: 

data_sub3.1<-data_sub1%>%
  filter(dist<1600)%>%
  mutate(km_bin=ceiling(dist/10))

dist_model1<-ggplot(data=data_sub3.1, aes(x=km_bin, y=abs(tree_diff)))+
  stat_binhex(aes(fill=log(..count..)))+
  geom_smooth(method="lm")+
  geom_smooth(method="loess", se=FALSE, color="yellow")+
  theme_classic()+
  ggtitle("Distance < 1600, all Sigmas")+
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(size=20), 
        plot.title = element_text(color = "black", size = 25, face = "bold", hjust = 0.5))

png("./outputs/dist_model_1600.png", height=800, width=1000)
dist_model1
dev.off()


#### Look at the linear models of tree_diff ~ distance
#### from Sigma<4 vs dist <= 1500

lm_sigma4<-lm(abs(tree_diff)~dist, data=subset(data, Sigma<4))
summary(lm_sigma4)

lm_km1500<-lm(abs(tree_diff)~dist, data=subset(data, dist<1500))
summary(lm_km1500)

# look at the relationship between tree_diff and sigma: 
head(data_sub3)
tree_by_sigma<-ggplot(data=subset(data_sub3, Sigma<4), aes(x=Sigma, y=abs(tree_diff)))+
  stat_binhex(aes(fill=log(..count..)))+
  geom_smooth(method="lm")+
  geom_smooth(method="loess", se=FALSE)
  

# 
# md_dist<-ggplot(data=subset(data_sub1, Sigma<4), aes(x=dist, y=MD))+
#   stat_binhex(aes(fill=log(..count..)))+
#   ylim(0, 5)+
#   #geom_smooth(method="loess", se=FALSE)+
#   geom_smooth(method="lm")
# 
# md_sqrt_dist<-ggplot(data=subset(data_sub1, Sigma<4), aes(x=sqrt(dist), y=MD))+
#   stat_binhex(aes(fill=log(..count..)))+
#   ylim(0, 4)+
#   geom_smooth(method="loess", se=FALSE, color="yellow")+
#   geom_smooth(method="lm")
# 
# 
# md_lm1<-lm(MD ~ dist, data=subset(data_sub1, Sigma<25))
# md_lm1<-lm(MD ~ sqrt(dist), data=subset(data_sub1, Sigma<25))
# 
