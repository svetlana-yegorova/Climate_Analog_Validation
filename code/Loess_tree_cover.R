# this file plots tree cover data and fits a spline curve to it. 
############# Date ########################
# 12/2/20 - cleaning up the file and backing up on github


############## Libraries #################3
# install.packages("ggformula")
library(ggformula)
library(ggplot2)
library(plyr)

############# 
data_out<-readRDS("./data/fia_tree_cover4km.rds")

# get the focal plot numbers: 
# focal<-unique(data_out$from_plot)
# write.csv(focal, "focal_plot_numbers.csv")
# focal

##### rename the trees_fia column into analog_trees for clarity: 
colnames(data_out)[24]<-c('analog_trees')

trees_fia=data_out


### 1 Tree Cover Distribution ###

par(mar=c(1,2,1,1))
par(mfrow=c(2, 2))
str(trees_fia)
# overall tree cover distribution:
hist(trees_fia$focal_trees, breaks=100)
hist(log(trees_fia$focal_trees), breaks=100)
hist(trees_fia$analog_trees, breaks=100)
hist(log(trees_fia$analog_trees), breaks=100)
# not a normal distribution :(


### 2 plot focal tree cover by analog tree cover for different sigmas ####

tt<-ggplot(data=subset(trees_fia, Sigma<=0.25), aes(x=focal_trees, y=analog_trees))+
  geom_point(aes(colour=Sigma))+
  geom_abline(intercept=0, slope=1, col='red')+
  geom_smooth(method='lm')

tt
### 3 Remove NA values from the tree cover data: 

tree_na<-subset(trees_fia, is.na(analog_trees))
tree_naf<-subset(trees_fia, is.na(focal_trees))

#get the list of plots that have na tree cover: 
#subset(trees_fia, is.na(analog_trees))%>%
#  ddply()
str(tree_na)
tree_na_plots<-ddply(tree_na, .(plot), summarize, sum=sum(dummy))
tree_na_plots

# there are 97 analog plots that do not have tree cover data: 


################ check focal plots for NA's 
tree_na_focal<-ddply(tree_naf, .(from_plot), summarize, sum=sum(dummy))
# no focal plots with NAs

# exclude the NA plots: 
trees_fia<-subset(trees_fia, !is.na(analog_trees))
# nrow(trees_fia)

tt<-ggplot(data=subset(trees_fia, Sigma<=0.25), aes(x=focal_trees, y=analog_trees))+
  geom_point(aes(colour=Sigma))+
  geom_abline( intercept=0, slope=1, col='red')+
  geom_smooth(method='lm')
tt


############# plot hexbins to show the density of points and instead of straight line fit a spline function #####

gg_spline<-ggplot(data=subset(trees_fia, Sigma<=0.25), aes(x=focal_trees, y=analog_trees))+
  geom_point(aes(colour=Sigma))+
  geom_abline( intercept=0, slope=1, col='red')+
  geom_spline(col='yellow', nknots=10) # how to make it smoother? #

# nknots gives the number of x values that serve as anchors, I think



gg_spline

############ hexbins with facets and smooth spline instead of line fit to the data: 
tree_hexes<-ggplot(data=trees_fia, aes(x=focal_trees, y=analog_trees))+
  stat_binhex(aes(fill=log10(..count..)), bins=30)+
  geom_abline( intercept=0, slope=1, col='lightblue', linetype="dashed", cex=1)+ # add linetype= "dashed". 
  geom_spline(colour="yellow", nknots=10, cex=2)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x="Focal Pixel Tree Cover", y= "Analog Pixel Tree Cover", color="sigma")+
  ggtitle("Model Performance with Increasing Climate Dissimilarity")+
  theme(plot.title = element_text(hjust=0.5), # centers the main title 
        plot.caption = element_text()) + coord_fixed()+
  facet_wrap(vars(Sig_c_f), nrow=1)

png("./outputs/tree_cover_splines.png", height=700, width=1600)
tree_hexes
dev.off()
######### biomass hexbins with smooth spline fit to the data: 
head(trees_fia)

AGC_hexes<-ggplot(data=trees_fia, aes(x=lfc, y=lc))+
  stat_binhex(aes(fill=log10(..count..)), bins=30)+
  geom_abline( intercept=0, slope=1, col='lightblue', linetype="dashed", cex=1)+ # add linetype= "dashed". 
  geom_spline(colour="yellow", nknots=10, cex=2)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x="Focal Carbon", y= "Analog Carbon", color="sigma")+
  ggtitle("Model Performance with Increasing Climate Dissimilarity")+
  theme(plot.title = element_text(hjust=0.5), # centers the main title 
        plot.caption = element_text()) + coord_fixed(ratio=1, ylim=c(-6, 6), xlim=c(-6, 6))+
  facet_wrap(vars(Sig_c_f), nrow=1)

png("./outputs/carbon_splines.png",  height=700, width=1600)
AGC_hexes
dev.off()



