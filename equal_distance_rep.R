######## data #############
# subsampled by sigma level
sigma_equal<-readRDS("./outputs/cover_diff_by_sig&dist_full_1200.rds")           
head(sigma_equal)

# full dataset: 
outputs<-readRDS("./data/1200_pixels.rds")
outputs<-filter(outputs, !is.na(tree_diff))
outputs<-filter(outputs, focal_plot!=analog_plot)

# function to subsample outputs by distance bins: 
# number of analogs per sigma bin: 
num_analogs<-aggregate(analog_plot~focal_plot+Sigma_bin1, data=outputs, FUN=length)


## to subsample distance bins, add distance bin category to the output file: 

# get the distance bins for the given plot: 
length(unique(outputs$focal_plot))

# set up output the length of the number of focal plots. I will put distance categories
# in the lists, and then add them to the outputs dataframe. 
out_dist<-vector("list", length(unique(outputs$focal_plot)))


data<-subset(output, focal_plot==plot)
dist_ref<-subset(sigma_equal, focal_plot==plot)
head(dist_ref)

# for each row in the dataframe, comapre distance to the distances in dist_ref
# table and assign category. 
?case_when
for (i in nrow(data)){
  dist_category<-case_when( data$dist[i]%>% between(dist_ref$q_90_dist[1]) )
  
  
}
  
}


# function to subsample min number of analogs from distance bins, as an argument
# takes focal plot as an argument 

sub_sample_min<-function(plot){
  data<-subset(output, focal_plot==plot)
  data$focal_plot<-as.factor(data$focal_plot)
  # do the following to figure out the # of analogs to draw for each plot
  num_analogs<-aggregate(analog_plot~focal_plot+Sigma_bin1, data=data, FUN=length)
  min_num<-min(num_analogs$analog_plot)
  

  # but need to group by distance bin: 
  sliced<-data%>%
    group_by(Sigma_bin1)%>%
    slice_sample(n=min_num, replace=FALSE)
  return(sliced)
}
