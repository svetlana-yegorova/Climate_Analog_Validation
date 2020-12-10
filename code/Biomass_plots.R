# This file plots biomass prediction at different levels of sigma/climate
# similarity. 

###############################################################
############## Data ###########################################
trees_fia<-readRDS("./data/fia_tree_cover4km.rds")

############# Libraries #####################################
library(ggplot2)

### give columns readable names and remove NA's
colnames(trees_fia)[24]<-c('analog_trees')
trees_fia<-subset(trees_fia, !is.na(analog_trees))

# set up the basic plot for biomass: 
ggplot(data=subset(trees_fia, Sigma<=0.25), aes(x=lfc, y=lc))+
  geom_point(aes(colour=Sigma))+
  geom_abline( intercept=0, slope=1, col='lightblue', linetype="dashed", cex=1)+ # add linetype= "dashed". 
  geom_smooth(colour='yellow', method='lm')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x="Focal Plot Biomass (Mg/ha)", y= "Analog Plot Biomass (Mg/ha)", color="sigma")+
  ylim(-3, 6)+xlim(-3, 6)+
  #scale_x_discrete(breaks=c("-2","2","6"),
                  # labels=c("0.13", "7", "403" ))+
  #ggtitle("Sigma<= 0.25")+
  theme(plot.title = element_text(hjust=0.5), # centers the main title 
        plot.caption = element_text()) + coord_fixed()

# facet plots by sigma levels: 
biomass_plots<-ggplot(data=trees_fia, aes(x=lfc, y=lc))+
  geom_point(aes(colour=Sigma))+
  geom_abline( intercept=0, slope=1, col='lightblue', linetype="dashed", cex=1)+ # add linetype= "dashed". 
  geom_smooth(colour='yellow', method='lm')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x="Focal Plot Biomass", y= "Analog Plot Biomass", color="sigma")+
  ylim(-3, 6)+xlim(-3, 6)+
  ggtitle("Biomass Model Performance with Increasing Climate Dissimilarity")+
  theme(plot.title = element_text(hjust=0.5), # centers the main title 
        plot.caption = element_text()) +
  theme(plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(color = "gray41", size = 14, face = "bold", hjust = 0.5),
      plot.caption = element_text(color = "gray65", face = "italic"),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15)) + coord_fixed()+
  facet_wrap(vars(Sig_c_f), nrow=1)


png("./outputs/biomass_plots.png", height=700, width=1600)
biomass_plots
dev.off()  



