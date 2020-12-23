# This file calculates the semivariogram of the overstory cover for the FIA analog plots.  

##### --------Libraries---------------------- #####

library(sp)
library(gstat)

##### ---------Data--------------------- #####

data_out<-readRDS("./data/fia_tree_cover4km.rds")
colnames(data_out)[24]<-("analog_trees")
trees_fia<-subset(data_out, !(is.na(analog_trees)))
trees_fia$cover_d<-trees_fia$focal_trees-trees_fia$analog_trees
trees_fia$tree_ratio<-trees_fia$cover_d/trees_fia$focal_trees

##### ------------------------------ #####
