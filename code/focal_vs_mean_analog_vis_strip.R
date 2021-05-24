# plots of analog tree cover vs focal tree cover by sigma level: 
# loads data summarized from ~ 1200 focal plots in the file
# 1200_focal_visualizations.R
# Why are non-analog means less dispersed? 

# data: 
out_summary<-readRDS("./outputs/summary_mean_distance_bykm&sigma.rds")

# libraries: 
library(ggplot2)
library(plyr)
library(RColorBrewer)

# now, plot. 
head(out_summary)
out_summary<-filter(out_summary, focal_trees<=100)

# Change Sigma_bin1 to Sigma for more readability
colnames(out_summary)[2]<-"Sigma"

# change sigma level 25 to >3 to make the graphs look nice. 
# this needs to be done upstream of out_summary file: 
# subsample non-analog sigma dataset to the same number of analogs
# as sigma of 3 before plotting
out_summary$Sigma<-plyr::revalue(out_summary$Sigma, c("25"=">3"))

### subset out_summary to certain sigma bins: 
out_summary_v<-out_summary %>%
  filter(Sigma %in% c("0.1", "0.5", "1", "2", "3", ">3"))

# set up the sigma color palette, skip the palest colors: 
cust_bg<-brewer.pal(n=8, "BuGn")[2:8]


# plot predicted vs mean analog: 
analogbyf<-ggplot(data=out_summary_v , aes(x=focal_trees, y=mean_analog))+
  geom_point(aes(colour= Sigma))+
  geom_abline(intercept = 0, slope=1)+
  geom_smooth(method="lm", color='yellow', cex=2)+
  xlim(0, 100)+
  facet_wrap(vars(Sigma), nrow=1)+
  scale_colour_manual(values = rev(cust_bg)) +
  theme_classic()+
  ggtitle("Predicted vs Observed Tree Cover")+
  xlab("Focal Tree Cover")+
  ylab("Mean Analog Tree Cover")+
  coord_fixed()+
  guides(colour = guide_legend(override.aes = list(size=5))) +
theme(axis.text.x = element_text( size=20,  angle=70),
      axis.text.y = element_text( size = 20),
      axis.title = element_text(size = 20),
      plot.title = element_text(size = 35, hjust=0.5, vjust = 1.5),
      legend.title=element_text(size=20),
      legend.text=element_text(size=15),
      strip.text = element_text(size = 20)) # this line sets the size for facet label
png( "./outputs/focal_by_mean_analog_tree_cover_1200.png", width=1500, height=700)
analogbyf
dev.off()