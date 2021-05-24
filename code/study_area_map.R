# this file produces a map of pixels used for climate analog validation 
# case study: 

#### Libraries ####
library(tidyverse)
library(ggplot2)
library(sf)
library(rgdal)
library(broom)


#### Data ####
data<-readRDS("./data/1200_pixels.rds")

# state boundaries
bndrs<-readOGR("./data/us_state_boundaries/cb_2018_us_state_5m.shp")



### "Clean" the data, only keep the coordinate, plot number, and tree cover
# columns: 

data_clean<-data%>% filter(focal_plot==20774)%>%
  select(analog_plot, analog_lat, analog_lon, analog_trees)

focal<-data%>% filter(focal_plot==analog_plot)%>%
  select(focal_plot, analog_lat, analog_lon, analog_trees)

# convert data_clean to a simple feature object
# meuse_sf = st_as_sf(meuse, coords = c("x", "y"), crs = 28992, agr = "constant")


data_sf<-st_as_sf(data_clean, coords=c("analog_lon", "analog_lat"), 
                  crs="+proj=longlat +datum=WGS84 +no_defs")
focal_sf<-st_as_sf(focal, coords=c("analog_lon", "analog_lat"), 
                   crs="+proj=longlat +datum=WGS84 +no_defs")

# looks nice enough: 
# but how do I do it with ggplot? 

# Perahps add state boundaries
# north arrow. 
plot(data_sf, cex=0.1)

map<-ggplot(data=data_sf, crs="+proj=longlat +datum=WGS84 +no_def")+
  geom_sf( color="blue")+
  geom_sf(data=focal_sf, color="yellow")+
  # ggtitle("Forest Cover Pixel Locations", subtitle = "30,000 potential analog pixels \n
  #         1200 focal pixels")+

  theme_void()
  # theme(plot.title = element_text(color = "darkgrey", size = 25, hjust = 0.5), 
  #       plot.subtitle = element_text(color="darkgrey", size=18), 
  #       plot.title.position = "plot",
  #       legend.title = element_text(color= "white"))

png("./outputs/study_area_map.png", width = 350, height = 350)
map
dev.off()


#### work with boundaries: 

# fortify shapefile into a dataframe format required by ggplot: 
str(bndrs)
bndrs
bndrs_fort<-tidy(bndrs, region = "NAME")

str(bndrs_fort)
ggplot()+
  geom_polygon(data=bndrs_fort, aes(x=long, y=lat, group=group))

# add state boundaries to teh existing sample pixel plots: 
mapstates = map_data("state")

map<-ggplot(data=data_sf, crs="+proj=longlat +datum=WGS84 +no_def")+
  geom_polygon(data=bndrs_fort, aes(x=long, y=lat, group=group), 
               fill="lightgrey")+
  geom_sf( color="blue")+
  geom_sf(data=focal_sf, color="yellow")+
  geom_polygon(data=bndrs_fort, aes(x=long, y=lat, group=group), color="white",
               fill=NA)+
  # geom_path(data = mapstates, color = "white", size = .75)+
  coord_sf(xlim = c(-125, -104),ylim = c(32, 50))+
  theme_void()

# explor the map: 
png("./outputs/study_area_map.png")
map
dev.off()