# this file attempts to do nested sampling of climate and distance: 
# divide the data into three distance categories: (50, 100)200, 500, 1000, 1500 km
# for each distance category: come up with the total number of analogs 
# per sigma bin, and come up with a mean estimate of tree cover difference: 

# data: 
### load a smaller dataset (100 focal plots) to work with
#data<-readRDS("./data/fia_tree_cover4km.rds")
#rm(data)

# calculate tree difference for this dataset
#data$tree_diff<-data$focal_trees-data$trees_fia


#load the large dataset: 
rm(AGC_hexes, biomass_plots, data_out, tree_hexes, trees_fia, tt, tabLabeller)

data<-readRDS("./data/1200_pixels.rds")
data<-subset(data, !is.na(tree_diff))

# libraries: 

library(tidyverse)
library(dplyr)
library(ggplot2)
library(plyr)

# explore brewer color palettes: 
# install.packages("RColorBrewer")
# install.packages("wesanderson")
library(RColorBrewer)
library(wesanderson)
##### beak  up data by distance category ########
head(data)
data<-data %>% mutate(dist_bin = as.factor(case_when( dist<=50 ~ 50, 
                                                      dist>50&dist<=100 ~ 100,
                                                      dist<=200 ~ 200, 
                                                      dist>=200 & dist<=500 ~ 500, 
                                                      dist>500&dist<=1000 ~ 1000, 
                                                      dist>1000 & dist<=1500 ~ 1500, 
                                                      dist>1500 & dist<=2000 ~ 2000,
                                                      TRUE ~ 3000)))

                  
                                            
# count the number of analogs per sigma bin: 
data$dummy<-1
# str(data)
# detach(package(plyr))
# str(data)
analog_count<-data %>%
  filter(Sigma_bin1!= 0.1 & Sigma_bin1 !=25 & dist_bin!=2000) %>%
  dplyr::group_by(dist_bin, Sigma_bin1) %>%
  dplyr::summarize(sum_d=sum(dummy))

head(analog_count)
print(analog_count, n= Inf)

# pick the lowest number of analogs, and draw that number out of each bin: 
n =min(analog_count$sum_d)
# 
# analog_sample<-data %>% 
#   filter(dist<=1500 & Sigma_bin1!= 0.1 & Sigma_bin1 !=25) %>%
#   dplyr::group_by(dist_bin, Sigma_bin1) %>%
#   sample_n(n, replace = FALSE)%>%
#   dplyr::summarize(mean_cover=mean(abs(tree_diff)), cover_sd=sd(abs(tree_diff)), check=sum(dummy))
# 
# 
# head(analog_sample)
# print(analog_sample, n=Inf)            
# 
# n
# 
# # plot mean difference in tree cover estimate: 
# 
# stratified_sample<-ggplot(data=analog_sample, aes(x=dist_bin, y=mean_cover, z=Sigma_bin1))+
#   geom_point(aes(color=Sigma_bin1), cex=6)+
#   #geom_point(aes(x=dist_bin, y=cover_sd))+
#   coord_cartesian(ylim=c(0, 30))+
#   theme_classic()+
#   ggtitle("Mean tree cover difference per Sigma bin and distance bin", 
#           subtitle = "1803 samples per bin")+
#   ylab("mean tree cover difference from focal plot")+
#   scale_x_discrete(name="distance bin (km)", breaks=c(200, 500, 1000, 1500), 
#                  labels=c("0-200", "200-500", "500-1000", "1000-1500"))+
#   theme(axis.title = element_text(size = 20), 
#         axis.text = element_text(size=20),
#         plot.title = element_text(color = "black", size = 25, face = "bold", hjust = 0.5))
# 
# png("./outputs/stratified_sample_distance_climate.png", width=750, height=500)
# stratified_sample
# dev.off()
# 
##### -------Plot the deviation of the difference------------------ #####

# 
# stratified_sample_sd<-ggplot(data=analog_sample, aes(x=dist_bin, y=mean_cover, z=Sigma_bin1))+
#   #geom_point(aes(color=Sigma_bin1))+
#   geom_point(aes(x=dist_bin, y=cover_sd, color=Sigma_bin1),pch=6)+
#   coord_cartesian(ylim=c(0, 30))+
#   theme_classic()+
#   ggtitle("Mean tree cover difference per Sigma bin and distance bin", 
#           subtitle = "1803 samples per bin")+
#   ylab("mean tree cover difference from focal plot")+
#   scale_x_discrete(name="distance bin", breaks=c(200, 500, 1000, 1500), 
#                    labels=c("0-200", "200-500", "500-1000", "1000-1500"))+
#   theme(axis.title = element_text(size = 20), 
#         axis.text = element_text(size=20),
#         plot.title = element_text(color = "black", size = 25, face = "bold", hjust = 0.5))
# stratified_sample_sd
# 


### Think of a different response variable. 
### What if we average climate analog predictions per sigma level per bin? 


# resample the data to include Sigma = 25 and summarize it to get mean tree 
# cover difference and sd: 
analog_sample1<-data %>% 
  filter(dist_bin!= 2000 & dist_bin!= 3000 & Sigma_bin1!= 0.1) %>%
  dplyr::group_by(dist_bin, Sigma_bin1) %>%
  sample_n(n, replace = FALSE)%>%
  dplyr::summarize(mean_cover=mean(abs(tree_diff)), cover_sd=sd(tree_diff), check=sum(dummy))
print(analog_sample1, n=Inf)


saveRDS(analog_sample1, "./outputs/analog_sample1__for_Marie.RDS")
### Graphing absolute mean tree cover difference: 

### Include non-analog plots
### add standard error bars (sd - measures dispersion within the sample vs se - measures ? )
# there are no analogs below 1.5 sigma beyond 1500 km (at least in our dataset)
dodge <- position_dodge(width=0.7) 
colnames(analog_sample1)[2]<-"Sigma"

# rename sigma 25 to "> 3"
analog_sample1$Sigma<-plyr::revalue(analog_sample1$Sigma, c("25"=">3"))

# set the bluegreen palette boundaries: 
cust_bg<-brewer.pal(n=8, "BuGn")[2:8]
#brewer.pal(n = 9, "Oranges")[3:9] #there are 9, I exluded the two lighter hues

stratified_sample1<-ggplot(data=analog_sample1, aes(x=dist_bin, y=abs(mean_cover), z=Sigma))+
  geom_point(aes(color=Sigma),  position=dodge, cex=7)+ # position=dodge offsets points so that we can see them
  geom_errorbar(aes(ymin=mean_cover-cover_sd, ymax=mean_cover+cover_sd, 
                    color=Sigma), position= dodge, width=0.2)+ # position=dodge offsets error bars, so that we can see them easily
  geom_hline(yintercept = 0, lty=2)+
  coord_cartesian(ylim=c(-10, 60))+
  theme_classic()+
  ggtitle("Changes in analog prediction quality  \n with distance & climate similarity", 
          subtitle = "1803 samples per point")+
  ylab("tree cover difference from focal")+
  scale_colour_manual(values = rev(cust_bg)) +
  # rev() function reverses the order in which colors are assigned to the points. 
  scale_x_discrete(name="distance category (km)", breaks=c(50, 100, 200, 500, 1000, 1500), 
                   labels=c("0-50", "50-100", "100-200", "200-500", "500-1000", "1000-1500"))+
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size=20),
        plot.title = element_text(color = "darkgrey", size = 25, face = "bold", hjust = 0.5))

png("./outputs/stratified_sample_distance_climate_with_non_analog.png", width=1000, height=500)
stratified_sample1
dev.off()



########################## Variations on the main theme #######################


# variation of the summary function, instead of sd get 90th percentile bar: 
analog_sample2<-data %>% 
  filter(dist_bin!= 2000 & dist_bin!= 3000 & Sigma_bin1!= 0.1) %>%
  dplyr::group_by(dist_bin, Sigma_bin1) %>%
  sample_n(n, replace = FALSE)%>%
  dplyr::summarize(mean_cover=mean(abs(tree_diff)), 
                   n10=quantile(abs(tree_diff), 0.1), 
                   n90=quantile(abs(tree_diff), 0.9), 
                   check=sum(dummy))

print(analog_sample2, n=Inf)

# rename Sigma_bin1 variable for ease of plotting: 
colnames(analog_sample2)[2]<-"Sigma"
head(analog_sample2)
# plotting: 

stratified_sample2<-ggplot(data=analog_sample2, aes(x=dist_bin, y=abs(mean_cover), z=Sigma))+
  geom_point(aes(color=Sigma),  position=dodge, cex=7)+ # position=dodge offsets points so that we can see them
  geom_errorbar(aes(ymin=n10, ymax=n90, 
                    color=Sigma), position= dodge, width=0.2)+ # position=dodge offsets error bars, so that we can see them easily
  geom_hline(yintercept = 0, lty=2)+
  coord_cartesian(ylim=c(0, 50))+
  theme_classic()+
  ggtitle("Changes in analog prediction quality with distance & climate similarity", 
          subtitle = "1803 samples per point. Bars represent 10th and 90th percentile of data distribution")+
  ylab("absolute tree cover difference from focal")+
  scale_colour_manual(values = rev(cust_bg)) +
  # rev() function reverses the order in which colors are assigned to the points. 
  scale_x_discrete(name="distance category (km)", breaks=c(50, 100, 200, 500, 1000, 1500), 
                   labels=c("0-50", "50-100", "100-200", "200-500", "500-1000", "1000-1500"))+
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size=20),
        plot.title = element_text(color = "darkgrey", size = 25, face = "bold", hjust = 0.5))
# 
png("./outputs/stratified_sample_distance_climate_with_non_analog_percentile_bars.png", width=1000, height=500)
stratified_sample2
dev.off()
# 
# 
# display.brewer.all()
# 
# display.brewer.pal(n = 6, name = 'BuGn')
