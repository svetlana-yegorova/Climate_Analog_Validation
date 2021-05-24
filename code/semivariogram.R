# Semivariance is the average squared difference in values between a focal locatoin 
# and all of its analog locations at distance h. 
# This file calculates the semivariogram of the overstory cover for the FIA analog plots.  
rm(AGC_hexes, biomass_plots, data_out, tree_hexes, trees_fia, tt)
##### --------Libraries---------------------- #####

library(sp)
library(gstat)

##### ---------Data--------------------- #####

data<-readRDS("./data/1200_pixels.rds")
data<-subset(data, !is.na(tree_diff))
focal<-subset(data, focal_plot==analog_plot)
data_test<-subset(data, focal_plot==17327)
rm(data)
##### ---------Meuse Example--------------------- #####
data(meuse)
head(meuse)
?meuse
coordinates(meuse) = ~x+y
tet<-variogram(log(zinc)~1, meuse)
plot(gamma~dist, data=tet)

tet1<-variogram(log(zinc)~x+y, meuse, alpha=c(0,45,90,135))
plot(gamma~dist, data=tet1)

tet2<-variogram(log(zinc)~1, meuse, width=90, cutoff=1300)
plot(gamma~dist, data=tet2)


v = variogram(log(zinc)~x+y, meuse)
v.fit = fit.variogram(v, vgm(1, "Sph", 700, 1))
v.fit
set = list(gls=1)
v
g = gstat(NULL, "log-zinc", log(zinc)~x+y, meuse, model=v.fit, set = set)
variogram(g)



##### ----------FIA example-------------------- #####
head(data_test)

# for ease of handling, get a dataset with plot #, coordinates and tree cover: 
data_clean<-data_test[, c(4, 7, 8, 9)]
head(data_clean)
rm(data, data_test, focal)

coordinates(data_clean)<- ~analog_lon+analog_lat
proj4string(data_clean) = "+proj=longlat"

vgrm<-variogram(analog_trees~analog_lon+analog_lat, data_clean, cutoff=1000)
# semivariagram for the "global data - all plots"

vgrm
plot(np~dist, data=vgrm)

png("./outputs/semivariance_global.png")
plot(vgrm, main="Full Dataset Semivariance", xlab="distance (km)")
dev.off()


# fit a variogram function: 

vgrm.fit<-fit.variogram(vgrm, model=vgm(1, "Exp", 900, 1))



