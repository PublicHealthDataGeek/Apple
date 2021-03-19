##############
#  Code to explore PCT in order to get Borough level kilometers cycled
#
# Created 19/3/20
#
##############


# https://itsleeds.github.io/pct/articles/pct_training.html

#load packages
library(tidyverse)
library(pct)
library(sf)



#Get road network for preselected regin
rnet = pct::get_pct_rnet(region = "london")


> names(rnet)
# [1] "local_id"       "bicycle"        "govtarget_slc" 
# [4] "govnearmkt_slc" "gendereq_slc"   "dutch_slc"     
# [7] "ebike_slc"      "geometry" 

#Calculate road length
rnet$segment_length = as.numeric(sf::st_length(rnet))
#Calculate daily km's cycled
rnet$m_cycled_per_working_day = rnet$segment_length * rnet$bicycle