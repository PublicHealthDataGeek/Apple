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

# import May 2020 ONS LA boundary data
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

# Get route network for London
lon_rnet = pct::get_pct_rnet(region = "london")

# Change CRS to ONS CRS
st_crs(lon_rnet) # QGS 84, ePSG 4326
lon_rnet = st_transform(lon_rnet, crs=27700) 
# st_crs(rnet) # PROJCRS["OSGB 1936 / British National Grid",

names(lon_rnet)
# [1] "local_id"       "bicycle"        "govtarget_slc" 
# [4] "govnearmkt_slc" "gendereq_slc"   "dutch_slc"     
# [7] "ebike_slc"      "geometry" 

#Calculate road length
lon_rnet$segment_length = as.numeric(sf::st_length(lon_rnet))

# summary(lon_rnet$segment_length)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.145    44.753   103.081   170.911   207.936 11316.022 

# summary(lon_rnet$bicycle)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    1.00    6.00   53.33   25.00 4113.00 

#Calculate daily km's cycled
# calculate daily metres cycled per day (multiple segment length by number of users)
lon_rnet$m_cycled_per_working_day = lon_rnet$segment_length * lon_rnet$bicycle
lon_rnet_centroid = st_centroid(lon_rnet)  # check works - see if any difference with interseciton

# Convert route network to centroids
#AGGREGATE USING STCENTROID ????
# convert the route network to centroids using the function st_centroid() before aggregating to avoid double counting
# as each point can only be in one region unlike linestrings which can be in multiple
# this allocates the whole length of route to that zone the centroid is in.  
# WILL NEED TO BE CLEAR WHAT THE DENOMINATOR IS SHOWING - IS IT THE PEOPLE WHO CYCLE IN THAT BOROUGH OR THE NUMBER OF KM CYCLED TRHOUGH THAT BOROUGH?



# Aggregate cycling to Borough level
cycled_m_per_Borough = aggregate(lon_rnet["m_cycled_per_working_day"], lon_lad_2020, FUN = sum)

# OBtain million kms cycled for commuting per year (200 = number of working days)
lon_lad_2020$millionkm_cycled_for_commuting_per_year_estimated = cycled_m_per_Borough$m_cycled_per_working_day * 
  2 * 200 / # estimate of trips days per year, morning and afternoon
  1e9 # to get million km

# Create dataset of Boroughs and estimated cycling
Borough_commuting = lon_lad_2020 %>%
  st_drop_geometry() %>%
  select(c(BOROUGH, millionkm_cycled_for_commuting_per_year_estimated))

# Save RDS
# saveRDS(Borough_commuting, file = "/home/bananafan/Documents/PhD/Paper1/data/Borough_commuting")