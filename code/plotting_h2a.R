# libraries
library(ggplot2)

# load sliced outputs (sliced to equal sigma representation for each focal plot)

# set up the input structure
out_vector<-vector("list", length=11)

for (i in 1:11){
  out<-readRDS(paste0("./outputs/sliced_output", i, "_of12.rds"))
  out_vector[[i]]<-do.call('rbind', out)
}

# unpack the vector into a dataframe: 

output<-do.call('rbind', out_vector)
head(output)

# plot error by distance and sigma bin: 
str(output)
output_sub<-subset(output, Sigma<4)

all<-ggplot(data=output_sub, aes(x=dist, y=abs(tree_diff)))+
  geom_point(colour="grey")+
  geom_smooth(method=loess, se=FALSE, cex=3, aes(colour=Sigma_bin1))+
  # geom_spline( aes(colour= Sig_ceiling), nknots=5, cex=2)+
  ggtitle("Absolute Residuals by Distance to Analog & Sigma \n equal Sigma representation per focal plot \n")+
  ylab("Absolute Tree Cover Difference")+xlab("Distance (km)")+
  ylim(-1, 100)+
  theme_bw()+
  theme(axis.text.x = element_text( size=13,  angle=70), 
        plot.title = element_text(size = 20, face = "bold"), 
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15))

png("./outputs/equal_sample_tree_diff_by_km&sigma_4.png", width=900, height=700)
all
dev.off()
