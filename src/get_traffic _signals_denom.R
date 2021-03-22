################################################################################
#  Get Traffic Signals denominators                                            #
#                                                                              #
# This code attempts to get Traffic signalsdenominators for ASL and Signals    #
# TFL official data is that they have 2649 junctions and 6416 traffic signals  #
# But what is the Borough distribution of these.  And how many traffic signals #
# does each junction have?                                                     #      
################################################################################

# ASLs can occur a signalised junctions
# Cycle Signals are recorded in the CID as a cycle signal if they occur at 
# signalsised junctions (if they are at a pedestrian crossing they go in the 
# crossing dataset)

library(tidyverse)
library(mapview)
library(sf)

# Set Mapview options to use data CRS rather than OSM projections
mapviewOptions(native.crs = TRUE)


#################################################################
# Initial data cleansing of raw TFL signal controlled junctions #
#################################################################

# FOI Request
# Please provide me with a list of all signal-controlled junctions maintained by Transport for London. 
# Please indicate the location of each junction, either with coordinates or by reference to road names. 
# https://www.whatdotheyknow.com/request/list_of_signal_controlled_juncti#incoming-1137262

# data = read_csv(file = "/home/bananafan/Downloads/Junction locations.csv.txt", col_names = TRUE)
# 
# # Correct missing Easting and Northing
# data$Easting = replace(data$Easting, 
#                        which(data$Easting == 0.0),
#                        values = 529861)
# 
# data$Northing = replace(data$Northing, 
#                        which(data$Northing == 0.0),
#                        values = 159201)
# 1 observation has incorrect geometry - located off cornwall  Location	MARLPIT LANE - A23 BYPASS ROAD OFF-SLIP
# long lat from googlemaps for this location: 51.31710567379319, -0.1379708953172271

# Convert longlat to easting northing
#Longitude: -0.137971 = Easting: 529861
#Latitude: 51.317106 = Northing: 159201


# # Convert to sf with OSGB 1936
# tfl_traffic_controlled_signals = data %>%
#   st_as_sf(coords = c("Easting", "Northing"), crs = 27700) # n = 2649
# 
# # Save tfl traffic controlled signals
# saveRDS(tfl_traffic_controlled_signals, file = "/home/bananafan/Documents/PhD/Paper1/data/tfl_traffic_controlled_signals")

# Currently there are 6,416 traffic signals in the London. TFL FOI
# https://tfl.gov.uk/corporate/transparency/freedom-of-information/foi-request-detail?referenceId=FOI-2098-1920


#  THEREFORE TFL ARE SAYING 2649 junctions and 6416 traffic signals

#####################################################################
# Data wrangling to get denominator of traffic signals at junctions #
#####################################################################


# Load OSM 2020 traffic signals dataset (n = 7050)
osm_traffic_signals_20 = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/osm_traffic_signals_20")

# Load TFL signal controlled traffic junctions (n = 2649)
tfl_traffic_controlled_signals = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/tfl_traffic_controlled_signals")

# Load CID ASL (n = 3775)
c_asl = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_asl")

# import May 2020 ONS LA boundary data
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

# Spatially join TFL dataset to ONS London Boroughs
tfl_signalled_junctions = st_join(tfl_traffic_controlled_signals, lon_lad_2020) # n = 2649

# Count number of TFL signalled junctions by Borough
tfl_signalled_junctions_borough_count = tfl_signalled_junctions %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(count = n())  



########################################
# EXPLORE USING CITY OF LONDON DATASET #
########################################
city_tfl_junctions = tfl_signalled_junctions %>%
  filter(BOROUGH == "City of London") # n = 57

city_osm_traffic_signals20 = osm_traffic_signals_20 %>%
  filter(BOROUGH == "City of London") # n = 195

city_asl = c_asl %>%
  filter(BOROUGH == "City of London") # n= 122


## Visualise
mapview(city_tfl_junctions$geometry, color = "red", cex = 2) + 
  mapview(city_osm_traffic_signals20$geometry, color = "yellow", cex = 2) +
  mapview(city_asl$geometry, color = "green")

# 4 x as many OSM points that TFL junction points - this is becase these represent actual traffic signals rather than junctions
# Over 2x as many ASLs as TFL junction points
# Most TFL junctions are in the OSM dataset but some arent.  
# Some of the TFL junctions that have ASL are not in the OSM dataset 
# Some of the ASL are some distance from the TFL junction point - some look to the 40m or so

m1 = mapview(city_tfl_junctions, color = "red", cex = 4)
m2 = mapview(city_osm_traffic_signals20, cex = 4)
m3 = mapview(city_asl)
leafsync::sync(m1,m2, m3)

# ?how to resolve???
# What is the question, what am I trying to show? 
# -> Want ot be able to compare boroughs by ASL provision
# 
# Approach 1: 
# numerator = borough count of ASLs
# denominator = borough count of tfl traffic signals - from some sort of cross of the datasets
# 
# Approach 2: what proportion of TFL junctions have ASLs by borough - match ASLs to junctions
# numerator = number of TFL junctions that have ASLs
# denominator = borough count of TFL junctions
#  - e.g by buffering around TFL junctions and then checking to see if there is one or more ASL in that buffer
#  - what happens if some roads feeding in dont have an ASL associated? 
#  - size of buffer as some asls look to be 40m from the tfl junction point.  


  








############
# OLD CODE #
############

# Are all ASL inside an OSM item?  
mapview(city_osm_traffic_signals20$geometry, color = "yellow", cex = 2) +
  mapview(city_asl$geometry, color = "green", cex = 2)


# What proportion of TFL junctions has an ASL in by Borough Count.

# Create 20m Buffer around each TFL junction
tfl_buffer_size = 20
city_tfl_buffer = st_buffer(city_signalled_junctions, dist = tfl_buffer_size)
city_tfl_buffer_union = st_union(city_tfl_buffer)


a_buffer = city_asl[city_tfl_buffer_union, ] # n = 70

a_buffer_variables = st_join(a_buffer, c_asl, join = st_nearest_feature)


mapview(city_signalled_junctions_buffer$geometry, color = "red") + 
  mapview(city_osm_junctions20_buffer)
  mapview(city_osm_traffic_signals20$geometry, color = "yellow", cex = 2) +
  mapview(city_asl$geometry, color = "green", cex = 2)

city_osm__buffer = st_buffer(city_osm_junctions20, dist = buffer_size)

all_cid_contraflows_buffer_union = st_union(all_cid_contraflows_buffer)   
mapview(city_signalled_junctions_buffer)

# Identify collisions within the contraflow buffer
a_buffer = a_sf[all_cid_contraflows_buffer_union, ] # n = 568


# Join the collision data that is within a 10m buffer of a contraflow section to TFL CID variables using the nearest CID feature
a_buffer_with_cid_variables = st_join(a_buffer, all_cid_contraflows_minimum_variables, join = st_nearest_feature)

