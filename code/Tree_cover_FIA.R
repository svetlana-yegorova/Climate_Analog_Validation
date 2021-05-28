# This file examines tree cover data at focal and analog pixels assoicated with FIA
# plot locations. 

### Analysis plan: 
### 1. look at the tree cover data distribution (histogram), transform if necessary
### 2. do the focal~ analog plot and linear regression with tree cover, save residuals. 
### 3. plot residuals against MD or Sigma or both. 

################### data ##########################3
data_out<-readRDS("./data/fia_tree_cover4km.rds")
################## libraries #################################

library(ggplot2)
library(plyr)


head(data_out)

# get the focal plot numbers: 
# focal<-unique(data_out$from_plot)
# write.csv(focal, "./outputs/focal_plot_numbers.csv")
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

### 2 plot focal tree cover by analog tree cover for different sigmas ####

tt<-ggplot(data=subset(trees_fia, Sigma<=0.25), aes(x=focal_trees, y=analog_trees))+
  geom_point(aes(colour=Sigma))+
  geom_abline(intercept=0, slope=1, col='red')+
  geom_smooth(method='lm')

tt
### looks like there are some NA values in the tree cover data: 
library(plyr)

tree_na<-subset(trees_fia, is.na(analog_trees))
tree_naf<-subset(trees_fia, is.na(focal_trees))

#get the list of plots that have na tree cover: 
#subset(trees_fia, is.na(analog_trees))%>%
#  ddply()
str(tree_na)
tree_na_plots<-ddply(tree_na, .(plot), summarize, sum=sum(dummy))
tree_na_plots
# there are 97 analog plots that do not have tree cover data: 


#################33 check focal plots for NA's 
tree_na_focal<-ddply(tree_naf, .(from_plot), summarize, sum=sum(dummy))
# no focal plots with NAs

# exclude the NA plots: 
trees_fia<-subset(trees_fia, !is.na(analog_trees))
nrow(trees_fia)

tt<-ggplot(data=subset(trees_fia, Sigma<=0.25), aes(x=focal_trees, y=analog_trees))+
  geom_point(aes(colour=Sigma))+
  geom_abline( intercept=0, slope=1, col='red')+
  geom_smooth(method='lm')
tt
summary(tt)
par(mfrow=c(2, 1))
hist(log(data_out$focal_tree))
hist(log(data_out$analog_tree))

#### get the slope of the line and check lines with more relaxed sigmas:  
slope_25<-lm(analog_trees~focal_trees, data=subset(trees_fia, Sigma<=0.25))
summary(slope_25)

slope_100<-lm(analog_trees~focal_trees, data=subset(trees_fia, Sigma<=1.0))
summary(slope_100)



slope_200<-lm(analog_trees~focal_trees, data=subset(trees_fia, Sigma<=2.0))
summary(slope_200)

slope_all<-lm(analog_trees~focal_trees, data=subset(trees_fia, Sigma<=4.00))
summary(slope_all)

##################### Finess the plots: 
# 1. Remove grey background
# 2. make 1:1 line dashed
# 3. make nice axes titles and add main title
# 4. put multiple plots in a row
# 5. keep individual plots square (x and y axis on the same scale)

#1. Remove grey background and grids: 

ggplot(data=subset(trees_fia, Sigma<=0.25), aes(x=focal_trees, y=analog_trees))+
  geom_point(aes(colour=Sigma))+
  geom_abline( intercept=0, slope=1, col='red')+
  geom_smooth(method='lm')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


# 2. 1:1 line dashed: 

ggplot(data=subset(trees_fia, Sigma<=0.25), aes(x=focal_trees, y=analog_trees))+
  geom_point(aes(colour=Sigma))+
  geom_abline( intercept=0, slope=1, col='red', linetype="dashed", cex=1)+ # add linetype= "dashed". 
  geom_smooth(method='lm')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# 3. axes and main titles: 
ggplot(data=subset(trees_fia, Sigma<=0.25), aes(x=focal_trees, y=analog_trees))+
  geom_point(aes(colour=Sigma))+
  geom_abline( intercept=0, slope=1, col='red', linetype="dashed", cex=1)+ # add linetype= "dashed". 
  geom_smooth(method='lm')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x="Focal Pixel Tree Cover", y= "Analog Pixel Tree Cover")+
  ggtitle("Sigma<= 0.25")+
  theme(plot.title = element_text(hjust=0.5), # centers the main title 
    plot.caption = element_text())

# 5. Square the plot: 
ggplot(data=subset(trees_fia, Sigma<=0.25), aes(x=focal_trees, y=analog_trees))+
  geom_point(aes(colour=Sigma))+
  geom_abline( intercept=0, slope=1, col='red', linetype="dashed", cex=1)+ # add linetype= "dashed". 
  geom_smooth(method='lm')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x="Focal Pixel Tree Cover", y= "Analog Pixel Tree Cover", color="sigma")+
  ggtitle("Sigma<= 0.25")+
  theme(plot.title = element_text(hjust=0.5), # centers the main title 
        plot.caption = element_text()) + coord_fixed()


# 6. Add facet by sigma value: 
head(trees_fia)
str(trees_fia)
# trees_fia$Sig_c_f
# custom labelling function: 
tabLabeller <- function(variable, value) {
  return(paste("Sigma:", as.character(value)))
}


tree_cover_plots<-ggplot(data=subset(trees_fia, Sigma<=4), aes(x=focal_trees, y=analog_trees))+
  geom_point(aes(colour=Sigma))+
  geom_abline( intercept=0, slope=1, col='lightblue', linetype="dashed", cex=1)+ # add linetype= "dashed". 
  geom_smooth(colour="yellow", method='lm')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x="Focal Pixel Tree Cover", y= "Analog Pixel Tree Cover", color="climate \ndistance")+
  #ggtitle("Model Performance with Increasing Climate Dissimilarity")+
  theme(plot.title = element_text(hjust=0.5), # centers the main title 
        plot.caption = element_text()) + coord_fixed()+
  facet_wrap(vars(Sig_c_f), nrow=2, labeller = tabLabeller)+
  theme(plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "gray41", size = 14, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "gray65", face = "italic"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) 

# png("./outputs/tree_cover_plots_2rows.png", height=700, width=1600)
# tree_cover_plots
# dev.off()  


# add hexbins: 
 
ggplot(data=subset(trees_fia, Sigma<=1), aes(x=focal_trees, y=analog_trees))+
  stat_binhex(aes(fill=log10(..count..)), bins=50)+
  geom_abline( intercept=0, slope=1, col='red', linetype="dashed", cex=1)+ # add linetype= "dashed". 
  geom_smooth(method='lm')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x="Focal Pixel Tree Cover", y= "Analog Pixel Tree Cover", color="sigma")+
  ggtitle("Sigma<= 0.25")+
  theme(plot.title = element_text(hjust=0.5), # centers the main title 
        plot.caption = element_text()) + coord_fixed()

# hexbins with facets: 
tree_hexes<-ggplot(data=trees_fia, aes(x=focal_trees, y=analog_trees))+
  stat_binhex(aes(fill=log10(..count..)), bins=30)+
  geom_abline( intercept=0, slope=1, col='lightblue', linetype="dashed", cex=1)+ # add linetype= "dashed". 
  geom_smooth(colour="yellow", method='lm')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x="Focal Pixel Tree Cover", y= "Analog Pixel Tree Cover", color="sigma")+
  ggtitle("Model Performance with Increasing Climate Dissimilarity")+
  theme(plot.title = element_text(hjust=0.5), # centers the main title 
        plot.caption = element_text()) + coord_fixed()+
  facet_wrap(vars(Sig_c_f), nrow=1, labeller = tabLabeller) +
  theme(plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "gray41", size = 14, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "gray65", face = "italic"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) 


png("./outputs/tree_cover_plots_with_hexbins_all_sigs.png", height=700, width=1600)
tree_hexes
dev.off()

##### --------Plot Mean Analog Tree Cover for each level of sigma---------------------- #####
#  calculate mean analog for different levels of sigma
head(trees_fia)
str(trees_fia$Sig_c_f)

# add one more level of Sigma: 
trees_fia$Sig_c_f1<-ifelse(trees_fia$Sigma<=0.1, 0.1, as.numeric(as.character(trees_fia$Sig_c_f)))
mean_analog<-ddply(trees_fia, .(from_plot, focal_trees, Sig_c_f1), summarize, mean_cover=mean(analog_trees), mean_km=mean(km))
head(mean_analog)


mean_an<-ggplot(data=subset(mean_analog, Sig_c_f1<25), aes(x=focal_trees, y=mean_cover))+
  #stat_binhex(aes(fill=log10(..count..)), bins=30)+
  geom_point(aes(colour=log(mean_km)), cex=2)+
  geom_abline( intercept=0, slope=1, col='lightblue', linetype="dashed", cex=1)+ # add linetype= "dashed". 
  geom_smooth(colour="yellow", method='lm', se=FALSE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x="Focal Pixel Tree Cover", y= " Mean Analog Pixel Tree Cover", color="mean log(km)\n to analog\n")+
  ggtitle("Model Performance with Increasing Climate Dissimilarity")+
  theme(plot.title = element_text(hjust=0.5), # centers the main title 
        plot.caption = element_text()) + coord_fixed()+
  facet_wrap(vars(Sig_c_f1), nrow=1, labeller = tabLabeller) +
  theme(plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "gray41", size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "gray65", face = "italic"),
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17)) 

png("./outputs/mean_analog.png", height=700, width=1600)
mean_an
dev.off()


tree_hexes<-ggplot(data=subset(trees_fia, Sigma<25), aes(x=focal_trees, y=analog_trees))+
  stat_binhex(aes(fill=log10(..count..)), bins=30)+
  geom_abline( intercept=0, slope=1, col='lightblue', linetype="dashed", cex=1)+ # add linetype= "dashed". 
  geom_smooth(colour="yellow", method='lm')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x="Focal Pixel Tree Cover", y= "Analog Pixel Tree Cover", color="sigma")+
  ggtitle("Model Performance with Increasing Climate Dissimilarity")+
  theme(plot.title = element_text(hjust=0.5), # centers the main title 
        plot.caption = element_text()) + coord_fixed()+
  facet_wrap(vars(Sig_c_f1), nrow=1, labeller = tabLabeller) +
  theme(plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "gray41", size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "gray65", face = "italic"),
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17)) 


# slope coefficients for the mean analog model: 
sigmas<-unique(trees_fia$Sig_c_f1)
rm(sig_coef_c)
sig_coef_c<-data.frame(sigma=as.numeric(), slope=as.numeric(), intcpt=as.numeric() )
head(mean_analog)

#rm(srt)
for (i in sigmas){
  model<-lm(mean_cover~focal_trees, data=subset(mean_analog, Sig_c_f1== i))
  srt <-cbind(i, as.numeric(model$coefficients[1]), model$coefficients[2])
  sig_coef_c<-rbind(sig_coef_c, srt)
}

names(sig_coef_c)<-c("sigma", "intercept", "slope")
sig_coef_c


# slopes for analog~focal relationship: 
#rm(sig_coef)
sig_coef<-data.frame(sigma=as.numeric(), slope=as.numeric(), intcpt=as.numeric() )

# coefficients for the tree cover models: 
for (i in sigmas) {
  model<-lm(analog_trees~focal_trees, data=subset(trees_fia, Sig_c_f1== i))
  srt <-cbind(i, as.numeric(model$coefficients[1]), model$coefficients[2])
  sig_coef<-rbind(sig_coef, srt)
}
names(sig_coef)<-c("sigma", "intercept", "slope")

sig_coef$model<-"individual"
sig_coef_c$model<-"mean"

coef<-rbind(sig_coef, sig_coef_c)
colnames(coef)[4]<-"model"

#### plot and compare the plots: 

slps<-ggplot(data=coef, aes(x=sigma, y=slope))+
  geom_point(aes(colour=model))+
  ggtitle("Mean analog tree cover by sigma level")+
  theme_bw()+
  ylim(0, 1)+
  scale_x_continuous(breaks = c(0,0.1, 0.25, 0.5, 0.75, 1, 2, 3, 4), limits=c(0, 4.5)) +
  theme(axis.text.x = element_text(face="bold", size=12,  angle=70))+ # rotate x-axis text for easier viewing. 
  ggtitle("Average and Individual Models by Sigma")+
  theme(plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "gray41", size = 14, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "gray65", face = "italic"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))
  

png("./outputs/mean_vs_individual_analog_slopes.png")
slps
dev.off()
