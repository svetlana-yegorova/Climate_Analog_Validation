
### data
analog_sample1<-readRDS("./outputs/analog_sample1__for_Marie.RDS") # change the
# path to match where you store the sample file

### libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plyr)
library(RColorBrewer)


### some data massaging to make the plot: 


dodge <- position_dodge(width=0.7) # offsets the points
colnames(analog_sample1)[2]<-"Sigma"

# rename sigma 25 to "> 3"
analog_sample1$Sigma<-plyr::revalue(analog_sample1$Sigma, c("25"=">3"))

# set the brewer bluegreen palette boundaries: 
cust_bg<-brewer.pal(n=8, "BuGn")[2:8]
#brewer.pal(n = 9, "Oranges")[3:9] #there are 9, I exluded the two lighter hues


# the plot itself: 

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

#png("./outputs/stratified_sample_distance_climate_with_non_analog.png", width=1000, height=500)
stratified_sample1
#dev.off()
