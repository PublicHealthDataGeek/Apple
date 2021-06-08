##################################################################################
#  Borough level analysis using cleansed data                                    #
#                                                                                #
# This script using the cleansed CID data where the Boroughs have been corrected #  
# to develop datasets for Borough level analysis                                 #
#                                                                                #
# This code was rerun on 8th June because the number of Crossings was altered    #
# as the multicrossing observations all became single crossing observations      #
# (see data_clean_CID_crossings.R)                                               #
##################################################################################

# install packages
library(tidyverse)
library(sf)
library(mapview)
library(units)
# library(leaflet)
# library(leafem)
# # library(leafsync)
# # library(summarytools)
# library(forcats)

#library(tmap)
#library(CycleInfraLnd)

# Load datasets - these datasets were downloaded from TFL 25th February 2021
c_asl = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_asl")
c_crossings = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_crossings")
c_cyclelanetrack = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_cycle_lane_track")
c_Rroutes = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_restricted_route")
c_Rpoints = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_restrictedpoints")
c_parking = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_parking")
c_signage = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_signage")
c_signals = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_signals")
c_trafficcalming = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_trafficcalming")
# NB crossings, cycle lanes/tracks, restricted routes and signage need Borough factoring but now c_datasets are correct

########################################################
# Create dataset of Borough level counts by asset type #
########################################################

# 1) Create Borough counts by asset type
# ## a) ASL
asl_borough_count = c_asl %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(ASL = n()) # 33 boroughs no NAs

# ## b) Crossings
crossings_borough_count = c_crossings %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Crossings = n()) # 33 boroughs no NAs

# ##  c) Signals
signal_borough_count = c_signals %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Signals = n()) # 23 boroughs no NAs

# ## d) Traffic calming
traffic_calming_borough_count= c_trafficcalming %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(TrafficCalming = n()) # 27 boroughs no NAs

# ## e) Cycle lanes and tracks
cycle_lane_track_borough_count = c_cyclelanetrack %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(CycleLanesAndTracks = n()) # 33 boroughs no NAs

# ## f) Restricted routes
restricted_route_borough_count = c_Rroutes %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(RestrictedRoutes = n()) # 33 boroughs no NAs

# ## g) Cycle Parking sites
cycle_parking_sites_borough_count = c_parking %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(CycleParkingSites = n()) # 33 boroughs no NAs

# ## h) Cycle Parking spaces
cycle_parking_space_borough_count = c_parking %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(CycleParkingSpaces = sum(PRK_CPT))

# ## i) Signage
signage_borough_count = c_signage %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Signage = n()) # 33 boroughs no NAs

# ## j) Restricted points
restricted_points_borough_count = c_Rpoints %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Restricted_Points = n()) # 27 boroughs no NAs

# 2) Create count dataset
CID_count_borough = list(asl_borough_count, crossings_borough_count, 
                         signal_borough_count,
                         traffic_calming_borough_count,
                         cycle_lane_track_borough_count, 
                         restricted_route_borough_count,
                         cycle_parking_sites_borough_count,
                         cycle_parking_space_borough_count,
                         signage_borough_count,
                         restricted_points_borough_count) %>%
  reduce(left_join, by = "BOROUGH")

# 3) Replace NA in count with 0

#sum(is.na(CID_count_borough)) # = 16 (10 from Signals and 6 from restricted points)
CID_count_borough = CID_count_borough %>% 
  replace(is.na(.), 0)
# sum(is.na(CID_count_borough)) # checks that no NAs now exist (= 0)


# 4) Save dataset
saveRDS(CID_count_borough, file = "/home/bananafan/Documents/PhD/Paper1/data/CID_count_by_borough")



#########################################################
# Create dataset of Borough level lengths by asset type #
#########################################################

# 1) Calculate lengths of each asset

# Restricted routes
c_Rroutes$length_m = st_length(c_Rroutes$geometry)
c_Rroutes$length_km = set_units(st_length(c_Rroutes$geometry), km)

# Cycle Lanes and Tracks - already done in the dataset
## c_cyclelanetrack$b_length_m = st_length(c_cyclelanetrack$geometry)
## c_cyclelanetrack$length_km = set_units(st_length(c_cyclelanetrack$geometry), km)
 
# 2) Calculate total length of assets by Borough
restricted_route_borough_length = c_Rroutes %>%
  group_by(BOROUGH) %>%
  summarise(across(c(length_m, length_km), sum))
# drop geometry cant be done within the pipe as it causes weirdness in the measurements (see issue 1)
# it needs to be dropped to make dataset manageable
restricted_route_borough_length = st_drop_geometry(restricted_route_borough_length) 


cycle_lane_track_borough_length = c_cyclelanetrack %>%
   group_by(BOROUGH) %>%
   summarise(across(c(length_m, length_km), sum))
 # drop geometry cant be done within the pipe as it causes weirdness in the measurements (see issue 1)
 # it needs to be dropped to make dataset manageable
cycle_lane_track_borough_length = st_drop_geometry(cycle_lane_track_borough_length) 
 

#3) Create dataset for lengths
length_CID_by_borough = list(cycle_lane_track_borough_length,
                             restricted_route_borough_length) %>%
  reduce(left_join, by = "BOROUGH") %>%
  mutate(across(2:5, round, 1)) %>%
  rename(c(CycleLaneTrack_km = length_km.x, RestrictedRoute_km = length_km.y,
           CycleLaneTrack_m = length_m.x, RestrictedRoute_m = length_m.y))

# 4) Save dataset
saveRDS(length_CID_by_borough, file = "/home/bananafan/Documents/PhD/Paper1/data/CID_length_by_borough")

