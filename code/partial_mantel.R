# partial mantel correlation with small data-set (100 focal plots).
# Partial mantel correlation test is testing for the 
# correlation between two distance matrices (e.g. tree cover distance matrix
# and climate distance matrix, after accounting for the effect of a third matrix, 
# e.g., physical distance matrix). 
# the purpose is to account for spatial structure in the data when looking for the 
# effect of climate that is not related to spatial structure only. 



##### Data ##################
rm(AGC_hexes, biomass_plots, data_out, tree_hexes, trees_fia, tt, tabLabeller)
# 
# ### load a smaller dataset (100 focal plots) to work with
# # data<-readRDS("./data/fia_tree_cover4km.rds")
# 
# data<-readRDS("./data/fia_tree_cover4km.rds")

# the 1000 plot dataset: 
rm(data)
data<-readRDS("./data/1200_pixels.rds")
data<-subset(data, !is.na(tree_diff))


##### Libraries #############
library(vegan) # for mantel.partial test - asks for square matrices, which 
# I don't have. Try running partial mantel test for a square matrix of 100 plots. 
library(tidyverse) 
library(dplyr)

# install.packages("ecodist") 
#library(ecodist)

### work flow ###########
# 1. Go throuhg the mantel test example and figure out what form the data have 
# to be in. 



## vegan package: 
# ## Is vegetation related to environment?
# data(varespec)
# data(varechem)
# str(varechem)
# head(varechem)
# 
# # calculate the distance matrix: 
# veg.dist <- vegdist(varespec) # Bray-Curtis
# env.dist <- vegdist(scale(varechem), "euclid")
# mantel(veg.dist, env.dist)
# mantel(veg.dist, env.dist, method="spear")
# 
# 
#### work flow ###########
# put distance, climate and tree cover difference data into matrices
# use one of the pivot functions to do that? 

# limit the full dataset to the 1200 plots for both focal and analog plots. 
head(data)
# data$tree_diff<-abs(data$trees_fia-data$focal_trees)
rm(data1)

# get a list of focal plots: 
focal<-unique(data$focal_plot)
length(focal)

data_sq<-data%>%filter(analog_plot %in% focal)
head(data_sq)

# now there are 1197 focal plots, how many unique analog plots are there: 
test<-unique(data_sq$analog_plot)
length(test) # 1197 should be good

# tree matrix: 
# dist.temp = dist(temp, method = "euclidean")
# tree_m<-dist()

tree_s<-data_sq%>%dplyr::select(analog_plot, focal_plot,  tree_diff)
head(tree_s)
tree_s$tree_diff<-abs(tree_s$tree_diff)


clim_s<-data_sq%>%dplyr::select(analog_plot, focal_plot, MD)
# what is the range of climate distances represented here: 
range(clim_s$MD)
hist(clim_s$MD)
dist_s<-data_sq%>%dplyr::select(analog_plot, focal_plot, dist)

# form distance matrices for the three variables
# # analog plots as column names, focal plots as row names: 
# why are there 101 columns, but only 100 rows? Remove the first column with plot numbers: 
tree_p<-(pivot_wider(tree_s, id_cols=c(focal_plot), names_from=analog_plot, values_from=tree_diff))[, -1]
clim_p<-pivot_wider(clim_s, id_cols=c(focal_plot), names_from=analog_plot, values_from=MD)[, -1]
dist_p<-pivot_wider(dist_s, id_cols=c(focal_plot), names_from=analog_plot, values_from=dist)[, -1]


# now... partial mantel.
# the order in which matrices are listed matters: 
# the third matrix is the one whose effect we are not interested in (want to partial

# try straight-up mantel test of tree and climate correlation (~ 0.3 correlation at 0.001 significance):
mantel_clim<-vegan::mantel(tree_p, clim_p, method = "spearman", permutations = 999, na.rm = TRUE, 
                  parallel= 6)
mantel_clim

# straight-up mantel test of tree cover and distance correlation (~ 0.32 correlation between 
# distance and tree cover, at 0.001 significance).   
mantel_dist<-vegan::mantel(tree_p, dist_p, method = "spearman", permutations = 999, na.rm = TRUE)
mantel_dist


# partial mantel test : 
mantel_try<-mantel.partial(tree_p, clim_p, dist_p, method = "pearson", permutations = 999,
               strata = NULL, na.rm = TRUE)

# so what just happened? 
# There is a mantel statistic of 0.12, indicating weak-ish (but statistically 
# significant) correlation between tree cover
# and climate similarity after accounting for physical distance. 



# examine the value ranges for all three matrices: 
head(tree_s)
range(tree_s$tree_diff)
par(mfrow=c(1, 3))
hist(tree_s$tree_diff, main="Cover difference")
hist(clim_s$MD, main="Climate Similarity")
hist(dist_s$dist, main="Km Distance")
#### explore ecodist package's example of mantel test: 
data(graze)
head(graze)

# take two subsets of sites with different dominant grass abundances
# this produces a non-square matrix
dom1 <- subset(graze, POPR > 45 & DAGL < 20) # 13 sites
dom2 <- subset(graze, POPR < 45 & DAGL > 20) # 8 sites
# first two columns are site info
dom.xd <- xdistance(dom1[, -c(1,2)], dom2[, -c(1,2)], "bray")

# environmental and spatial distances; preserve rownames
forest.xd <- xdistance(dom1[, "forestpct", drop=FALSE],
                       dom2[, "forestpct", drop=FALSE])

sitelocation.xd <- xdistance(dom1[, "sitelocation", drop=FALSE],
                             dom2[, "sitelocation", drop=FALSE])
# permutes rows and columns of full nonsymmetric matrix
xmantel(dom.xd ~ forest.xd, dims=c(13, 8))
xmantel(dom.xd ~ forest.xd + sitelocation.xd, dims=c(13, 8))


# try on my data: 
# remove first row and column from the datasets (names of sites)
tree_p1<-tree_p[-1, -1]
clim_p1<-clim_p[-1, -1]
dist_p1<-dist_p[-1, -1]
nrow(tree_p1)
ncol(tree_p1)

head(tree_p1)
tree_m<-as.matrix(tree_p1)
clim_m<-as.matrix(clim_p1)
dist_m<-as.matrix(dist_p1)

# says there is an issue with dimentions... don't see it, but the code won't run. 

mantel_try<-xmantel(tree_m ~ clim_m+ dist_m, dims=c(nrow(tree_m), ncol(tree_m)))

                    