#1) divide outputs dataframe/tibble into 4 chunks, 300 focal plots each. 
#2) for each chunk, get a list of unque focal plots. 
#3) write a function that takes focal plot number as its argument: 
  # subset the chunk to the data for the focal plot and the dist_per_plot to focal plot
  # iterate through distance group in the dist_per_plot
  # get a tibble with focal_plot, distance group and associated residual. 


#1) divide outputs tibble: 
nrow(outputs)
all_focals<-unique(outputs$focal_plot)
length(all_focals)
focals1<-all_focals[1:400]
focals2<-all_focals[401:800]
focals3<-all_focals[801:length(all_focals)]

rm(all_focals)


# Operations for one plot: 
out_chnk1<-filter(outputs, focal_plot %in% focals1)

# function actions: 


distance.fun<-function(i){

  out1<-data.frame(plot=integer(), Sigma_bin=double(), mean_resid_km=double())
  plot<-focals1[i]
  output_data<-filter(out_chnk1, focal_plot==plot)
  dist_table<-filter(dist_per_plot, focal_plot==plot)
  
        for (n in 1:nrow(dist_table)){
          mean_resid_km<-output_data%>%
            filter(between(dist, (dist_table$mean_dist[n]-dist_table$sd_d[n]), (dist_table$mean_dist[n]+dist_table$sd_d[n])))%>%
            summarize(mean_resid_km=mean(abs(tree_diff))) %>%
            pull(.)
          out<-data.frame(cbind(plot, Sigma_bin=dist_table$Sigma_bin1[n], mean_resid_km))
          out1<-plyr::rbind.fill(out1, out)
          return(out1)
        }
 
return(out1)
}

# turn on multicore settings
# library(future.apply)
# plan(multisession)
# 
# str(focals1)
# 
# test<-future_lapply(X=focals1[1:2], FUN=distance.fun)
# 
# test
# out
# out1

out1<-data.frame(plot=integer(), Sigma_bin=double(), mean_resid_km=double())

for (i in 1:length(focals1)){
  plot<-focals1[i]
  output_data<-filter(out_chnk1, focal_plot==plot)
  dist_table<-filter(dist_per_plot, focal_plot==plot)
  
  for (n in 1:nrow(dist_table)){
    mean_resid_km<-output_data%>%
      filter(between(dist, (dist_table$mean_dist[n]-dist_table$sd_d[n]), (dist_table$mean_dist[n]+dist_table$sd_d[n])))%>%
      summarize(mean_resid_km=mean(abs(tree_diff))) %>%
      pull(.)
    out<-data.frame(cbind(plot, Sigma_bin=dist_table$Sigma_bin1[n], mean_resid_km))
    out1<-plyr::rbind.fill(out1, out)
    
  }
  if(i%%20==0){print(i)}
}

nrow(out1)

saveRDS(out1, "./Documents/Climate_Analog_Validation/outputs/mean_residual_by_distance_1.rds")

out_chnk2=filter(outputs, focal_plot %in% focals2  )
out2<-data.frame(plot=integer(), Sigma_bin=double(), mean_resid_km=double())

for (i in 1:length(focals2)){
  plot<-focals2[i]
  output_data<-filter(out_chnk2, focal_plot==plot)
  dist_table<-filter(dist_per_plot, focal_plot==plot)
  
  for (n in 1:nrow(dist_table)){
    mean_resid_km<-output_data%>%
      filter(between(dist, (dist_table$mean_dist[n]-dist_table$sd_d[n]), (dist_table$mean_dist[n]+dist_table$sd_d[n])))%>%
      summarize(mean_resid_km=mean(abs(tree_diff))) %>%
      pull(.)
    out<-data.frame(cbind(plot, Sigma_bin=dist_table$Sigma_bin1[n], mean_resid_km))
    out2<-plyr::rbind.fill(out2, out)
    
  }
  if(i%%20==0){print(i)}
}

nrow(out1)

saveRDS(out2, "./Documents/Climate_Analog_Validation/outputs/mean_residual_by_distance_2.rds")



##### -------Chunk 3----------------------- #####


out_chnk3=filter(outputs, focal_plot %in% focals3  )
out3<-data.frame(plot=integer(), Sigma_bin=double(), mean_resid_km=double())

for (i in 1:length(focals3)){
  plot<-focals3[i]
  output_data<-filter(out_chnk3, focal_plot==plot)
  dist_table<-filter(dist_per_plot, focal_plot==plot)
  
  for (n in 1:nrow(dist_table)){
    mean_resid_km<-output_data%>%
      filter(between(dist, (dist_table$mean_dist[n]-dist_table$sd_d[n]), (dist_table$mean_dist[n]+dist_table$sd_d[n])))%>%
      summarize(mean_resid_km=mean(abs(tree_diff))) %>%
      pull(.)
    out<-data.frame(cbind(plot, Sigma_bin=dist_table$Sigma_bin1[n], mean_resid_km))
    out3<-plyr::rbind.fill(out3, out)
    
  }
  if(i%%20==0){print(i)}
}


nrow(out3)
all_distances<-rbind(out1, out2, out3)
saveRDS(all_distances, "./Documents/Climate_Analog_Validation/outputs/mean_residual_by_distance_all.rds")
head(all_distances)

# now I need to join all_distances with dist_table
# by focal plot and Sigma_bin


colnames(all_distances)<-c("focal_plot", "Sigma_bin1", "mean_resid_km")

dist_per_plot<-as.data.frame(dist_per_plot)
getwd()

outputs<-readRDS("./outputs/mean_residual_by_distance_all.rds")
head(outputs)

dist_per_plot1<-join(dist_per_plot, outputs, by=c("focal_plot", "Sigma_bin1"))
head(dist_per_plot1)
dist_per_plot1$Sigma_bin1<-as.factor(dist_per_plot1$Sigma_bin1)

blah<-ggplot(data=dist_per_plot1, aes(x=mean_resid, y=mean_resid_km))+
  geom_point(aes(color=Sigma_bin1))+
  geom_abline(slope=1, intercept=0)+
  ylim(0, 80)+
  theme_bw()+
  labs(x="Sigma Mean Residuals", y="Distance Mean Residuals")+
  ggtitle("Variance of Prediction by Sigma and Distance")+
  theme(axis.text.x = element_text( size=20,  angle=70), 
        axis.title = element_text(size=20),
        axis.text.y = element_text(size=20),
        plot.title = element_text(size = 30, face = "bold"), 
        plot.subtitle = element_text(size=20))+
  theme(legend.text = element_text(size=20), 
        legend.title = element_text(size=20))

png( "./Documents/Climate_Analog_Validation/outputs/distance_residuals_vs_climate_residuals_per_plot.png", width=800, height=800)
blah
dev.off()
