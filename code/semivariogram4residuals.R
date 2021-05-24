# this file performs a semivariogram on residuals from multiple regression
# tree cover difference ~ km + md
# and another semivariogram on residuals of tree diff ~ km
# to see if modelng distance explicitly reduces/removes spatial patterns in 
# the data. 

##### --------Libraries---------------------- #####

library(sp)
library(gstat)

##### ---------Data--------------------- #####

data<-readRDS("./data/1200_pixels.rds")
data<-subset(data, !is.na(tree_diff))
focal<-subset(data, focal_plot==analog_plot)
data_test<-subset(data, focal_plot==17327)
rm(data)
head(data_test)

#### regress tree cover difference on MD and km: 
range(data_test$tree_diff)
lm_test<-lm(abs(tree_diff)~dist+MD, data=data_test)
summary(lm_test)

# R squared comparable to the large dataset regression 
# coefficients: similar for km, greater by a factor of 5 for MD in 
# the single plot regression. 

# get residuals: 
data_test$res<-lm_test$residuals

# the semivariogram: 
coordinates(data_test)<- ~analog_lon+analog_lat
proj4string(data_test) = "+proj=longlat"

# variogram for tree cover: 
# what does the cutoff argument specify? 
vgrm<-variogram(analog_trees~analog_lon+analog_lat, data_test, cutoff=1000)

# variogram for residuals: 
# still spatial structure remaining after multiple regression... 
vrgm_res<-variogram(res~analog_lon+analog_lat, data=data_test, cutoof=1000)
vrgm_res

par(mfrow=c(2,2))
png("./outputs/residual_varince_plot.png")
plot(vrgm_res, main="multivariate regression (cover diff ~ km + MD) residuals")
dev.off()

plot(vgrm, main="Tree cover variogram, unregressed")

# semivariagram for the "global data - all plots"

vgrm
plot(np~dist, data=vgrm)

png("./outputs/semivariance_global.png")
plot(vgrm, main="Full Dataset Semivariance", xlab="distance (km)")
dev.off()


# fit a variogram function: 

vgrm.fit<-fit.variogram(vgrm, model=vgm(1, "Exp", 900, 1))



