##################################################################################
#  Borough level analysis using cleansed data                                    #
#                                                                                #
# This script using the cleansed CID data where the Boroughs have been corrected #  
# to develop datasets for Borough level analysis                                 #
#                                                                                #
##################################################################################

# install packages
library(tidyverse)
library(sf)
library(mapview)
# library(leaflet)
# library(leafem)
# # library(leafsync)
# # library(summarytools)
# library(forcats)
# library(units)
#library(tmap)
#library(CycleInfraLnd)

# Load datasets
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

lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")



dataset_names = c("c_asl", "c_crossings", "c_cyclelanetrack", "c_Rroutes", 
                  "c_Rpoints", "c_parking", "c_signage", "c_signals", 
                  "c_trafficcalming") #list of datasets to loop through
borough_names = as.character(pull(lon_lad_2020, BOROUGH)) # get list of borough names
borough_count = data.frame(Borough = vector(mode = "character", length = length(borough_names))) # create output df for loop


# Create dataset of Borough level counts by asset type

# 1) Create Borough counts by asset type
# ## a) Cycle lanes and tracks
cycle_lane_track_borough_count = c_cyclelanetrack %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(CycleLanesAndTracks = n()) # 33 boroughs no NAs


######## NB use right summarise terms so that visualising borough counts still work!!!!!





##########################################
# OLD CODE BELOW - probably still useful - needs checking and correcting though

####### Count of assets by Borough
CID_by_borough = list(asl_borough_count, crossings_borough_count, 
                      cycle_lane_track_borough_count, 
                      restricted_route_borough_count,
                      cycle_parking_sites_borough_count,
                      cycle_parking_space_borough_count,
                      signal_borough_count,
                      traffic_calming_borough_count,
                      signage_borough_count,
                      restricted_points_borough_count) %>%
  reduce(left_join, by = "BOROUGH")

# need to replace NA in Borough with 'Borough not stated' and NAs in other columns with 0
sum(is.na(CID_by_borough)) # = 21

# Replace 'Borough NA' with text and count NAs with 0
CID_by_borough = CID_by_borough %>% 
  replace_na(list(BOROUGH = "No Borough stated in CID")) %>%
  replace(is.na(.), 0)
sum(is.na(CID_by_borough)) # checks that no NAs now exist (= 0)

saveRDS(CID_by_borough, file = "/home/bananafan/Documents/PhD/Paper1/output/CID_count_by_borough")

###### Length of line assets by Borough

# 
# # Length of cycle lanes and tracks by Borough
# f_cycle_lane_track$length_m = st_length(f_cycle_lane_track$geometry)
# f_cycle_lane_track$length_km = set_units(st_length(f_cycle_lane_track$geometry), km)
# 
# cycle_lane_track_borough_length = f_cycle_lane_track %>%
#   group_by(BOROUGH) %>%
#   summarise(across(c(length_m, length_km), sum))
# # drop geometry cant be done within the pipe as it causes weirdness in the measurements (see issue 1)
# # it needs to be dropped to make dataset manageable
# cycle_lane_track_borough_length = st_drop_geometry(cycle_lane_track_borough_length) 
# 
# # Save these datasets for combination




length_CID_by_borough = list(cycle_lane_track_borough_length,
                             restricted_route_borough_length) %>%
  reduce(left_join, by = "BOROUGH") %>%
  mutate(across(2:5, round, 1)) %>%
  rename(c(CycleLaneTrack_km = length_km.x, RestrictedRoute_km = length_km.y,
           CycleLaneTrack_m = length_m.x, RestrictedRoute_m = length_m.y)) %>%
  replace_na(list(BOROUGH = "No Borough stated in CID")) # Correct NA in Borough column

saveRDS(length_CID_by_borough, file = "/home/bananafan/Documents/PhD/Paper1/output/CID_length_by_borough")

