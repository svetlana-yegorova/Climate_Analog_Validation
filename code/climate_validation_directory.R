# This is the master or directory scrip that organizes and explains
# "sub"-scripts in this R project. 

# The project is aimed at validating the biological meaning of climate analogs
# by testing whether contemporary climate analogs can reproduce the focal pixel's 
# tree cover and whether limiting distance to analog improves analog's precision
# and accuracy.
# I use remotely sensed Landsat tree cover data and climate
# analogs calculated at the  4 km pixel resolution to validate the biological 
# meaning of climate analogs. 

# these scripts should stand alone in terms of data requirements. They do not
# have to be run sequentially. 

# code to visualize and print graphs of  tree cover prediction by sigma value: 
source("./code/Tree_cover_FIA.R")

# check that linear fit is appropriate for the predicted vs actual tree cover by
# examining loess curve fits to poredicted vs actul data: 
source("./Loess_tree_cover.R")

# code to visualize FIA plot biomass by sigma value (mostly here to compare
# with the tree cover plots): 
source("./code/Biomass_plots.R")

# examine the relationship between climate and physical distance. mostly exploratory 
# plots: 
source("./code/km_vs_MD.R")





# print the graph of analog prediction quality by distance bin: 
source("./code/nested_sampling_for_km_sig.R")

# just testing what git is doingn test... 
