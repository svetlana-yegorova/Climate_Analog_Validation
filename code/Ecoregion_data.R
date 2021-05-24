### This file adds ecoregion information to the FIA pixels. The goal of this 
# addition is to be able to examine analog performance in predicting focal 
# location's tree cover by ecoregion. 

# plan of work: 
# 1) import the ecoregion boundaries & assess attributes. 
# 2) intersect boundaries with fia points and 
# 3) extract ecoregion information in R (read about how to do that)


## libraries
library(sf)

## data
# ecoregions: 
eco_reg<-st_read("./data/dataverse_files/tnc_terr_ecoregions.shp")

# pixel/plot coordinates: 
plots<-read.csv("./data/fia.c.1cond.csv")


### look at the attributes of eco_reg: 
# what kind of object is eco_reg? 
# it is an sf type object, due to the way we imported it. 
# it has both spatian and non-spatial (attribute data)
class(eco_reg)
str(eco_reg)
eco_reg$ECO_NAME

#plot(eco_reg)
