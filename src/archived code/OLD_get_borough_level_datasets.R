#############################################
# Create Borough level comparison data sets #
#
# Created 13/1/21
#
# This code created datasets of borough level asset data for counts and length
# that can be used to compare boroughs
#
###########


#

# install packages
library(tidyverse)
library(CycleInfraLnd)
library(sf)
#library(tmap)
library(mapview)
library(leaflet)
library(leafem)
#library(leafsync)
library(summarytools)
library(forcats)
library(units)


#######################
# Advanced Stop Lines #
#######################

# download CID data and transform CRS
advanced_stop_line = get_cid_lines(type = "advanced_stop_line")

# Count number of ASLs by Borough
asl_borough_count= advanced_stop_line %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(ASL = n()) # 33 boroughs plus 1 asl with no borough assigned

#############
# Crossings #
#############

# download crossings data
crossings = get_cid_lines(type = "crossing")

# Count number of Crossings by Borough
crossings_borough_count = crossings %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Crossings = n()) # 33 boroughs plus 28 crossings with no borough


##########################
# Cycle Lanes and Tracks #
##########################

# download CID data using the Cycle Infra Lnd package
cycle_lane_track = get_cid_lines(type = "cycle_lane_track") # n = 24976
cycle_lane_track = st_transform(cycle_lane_track, crs=27700) 

# Count number of cycle lanes by Borough
cycle_lane_track_borough_count = cycle_lane_track %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(CycleLanesAndTracks = n()) # 33 boroughs plus 354 cycle lanes/tracks with no borough

# Measure Length of cycle lanes and tracks by Borough
cycle_lane_track$length_m = st_length(cycle_lane_track$geometry)
cycle_lane_track$length_km = set_units(st_length(cycle_lane_track$geometry), km)

cycle_lane_track_borough_length = cycle_lane_track %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(across(c(length_m, length_km), sum))
# drop geometry cant be done within the pipe as it causes weirdness in the measurements (see issue 1)
# it needs to be dropped to make dataset manageable
cycle_lane_track_borough_length = st_drop_geometry(cycle_lane_track_borough_length) 

#####################
# Restricted Routes #
#####################

# download CID data 
restricted_route = get_cid_lines(type = "restricted_route")
restricted_route = st_transform(restricted_route, crs=27700) 

# Count number of restricted routes by Borough
restricted_route_borough_count = restricted_route %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(RestrictedRoutes = n()) # 33 boroughs plus 18 restricted routes with no borough

# Length of restricted routes by Borough
restricted_route$length_m = st_length(restricted_route$geometry)
restricted_route$length_km = set_units(st_length(restricted_route$geometry), km)

restricted_route_borough_length = restricted_route %>%
  group_by(BOROUGH) %>%
  summarise(across(c(length_m, length_km), sum))
# drop geometry cant be done within the pipe as it causes weirdness in the measurements (see issue 1)
# it needs to be dropped to make dataset manageable
restricted_route_borough_length = st_drop_geometry(restricted_route_borough_length) 

#####################
# Cycle Parking     #
#####################

# download data
cycle_parking = get_cid_points(type = "cycle_parking")

# Count cycle parking locations/sites
cycle_parking_sites_borough_count = cycle_parking %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(CycleParkingSites = n())

# Count Number of cycling parking spaces (capacity)
# PRK_PROVIS (number of stands etc) and PRK_CPT (number of bikes that can be 
# parked) have NAs.  These need correcting before can calculate no of 
# bikes that can be parked per borough
# sum(is.na(cycle_parking$PRK_PROVIS)) # 2 NA
# sum(is.na(cycle_parking$PRK_CPT)) # 2 NAs
# CPcount_borough_NA = f_cycle_parking %>% # identify which observations are NA
#   st_drop_geometry() %>%
#   filter(is.na(PRK_CPT)) # 2 observations have NA - both for PROVIS & CPT

# Asset images reviewed - able to see how many stands and spaces each have
# RWG999580 4 stands (PROVIS) 8 spaces (CPT)
# RWG999458 3 stands, 6 spaces
# therefore can correct missing data
cycle_parking$PRK_PROVIS[cycle_parking$FEATURE_ID == "RWG999580"] = 4
cycle_parking$PRK_CPT[cycle_parking$FEATURE_ID == "RWG999580"] = 8
cycle_parking$PRK_PROVIS[cycle_parking$FEATURE_ID == "RWG999458"] = 3
cycle_parking$PRK_CPT[cycle_parking$FEATURE_ID == "RWG999458"] = 6

# check coded correctly
# x = f_cycle_parking %>%
#   filter(FEATURE_ID == "RWG999580" | FEATURE_ID == "RWG999458") # = yes coded correctly
# now calculate number of cycle parking spaces by borough
cycle_parking_spaces_borough_count = cycle_parking %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(CycleParkingSpaces = sum(PRK_CPT))

###########
# Signals #
###########

# download data
signal = get_cid_points(type = "signal") #n = 443

# Count number of signals by Borough 
signal_borough_count = signal %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Signals = n())


###################
# Traffic calming #
###################

# download data
traffic_calming = get_cid_points(type = "traffic_calming")

# Count number of traffic calming measures by Borough 
traffic_calming_borough_count = traffic_calming %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(TrafficCalming = n())

###########
# Signage #
###########

# download data
signage = get_cid_points(type = "signage")

# examine RWG999275 as this appears twice
# RWG_duplicate = signage %>%
#   filter(FEATURE_ID == "RWG999275") # some difference in variables
# mapview(RWG_duplicate) # located in different areas
# create new FEATURE_IDs so that observations can be differentiated
signage$FEATURE_ID[signage$BOROUGH == "Barnet"] = "RWG999275a"
signage$FEATURE_ID[signage$BOROUGH == "Haringey"] = "RWG999275b"

# Borough level analysis
signage_borough_count = signage %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Signage = n())

#####################
# Restricted points #
#####################

# download CID data using the Cycle Infra Lnd package
restricted_points = get_cid_points(type = "restricted_point")
restricted_points = st_transform(restricted_points, crs=27700) 

# Borough level analysis
restricted_points_borough_count = restricted_points %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(RestrictedPoints = n())

##################################################################################
# Create data frames for Borough level analysis of assets by type
#
# 
# 

####### Count of assets by Borough
CID_by_borough = list(asl_borough_count, 
                      crossings_borough_count, 
                      cycle_lane_track_borough_count, 
                      restricted_route_borough_count,
                      cycle_parking_sites_borough_count,
                      cycle_parking_spaces_borough_count,
                      signal_borough_count,
                      traffic_calming_borough_count,
                      signage_borough_count,
                      restricted_points_borough_count) %>%
  reduce(left_join, by = "BOROUGH")

# need to replace NA in Borough with 'Borough not stated' and NAs in other columns with 0
sum(is.na(CID_by_borough)) # = 22

# Replace 'Borough NA' with text and count NAs with 0
CID_by_borough = CID_by_borough %>% 
  replace_na(list(BOROUGH = "No Borough stated in CID")) %>%
  replace(is.na(.), 0)
sum(is.na(CID_by_borough)) # checks that no NAs now exist (= 0)

saveRDS(CID_by_borough, file = "/home/bananafan/Documents/PhD/Paper1/output/CID_count_by_borough")

###### Length of line assets by Borough
length_CID_by_borough = list(cycle_lane_track_borough_length,
                             restricted_route_borough_length) %>%
  reduce(left_join, by = "BOROUGH") %>%
  mutate(across(2:5, round, 1)) %>%
  rename(c(CycleLaneTrack_km = length_km.x, RestrictedRoute_km = length_km.y,
           CycleLaneTrack_m = length_m.x, RestrictedRoute_m = length_m.y)) %>%
  replace_na(list(BOROUGH = "No Borough stated in CID")) # Correct NA in Borough column

saveRDS(length_CID_by_borough, file = "/home/bananafan/Documents/PhD/Paper1/output/CID_length_by_borough")

