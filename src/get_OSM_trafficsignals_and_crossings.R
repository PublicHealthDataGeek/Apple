######################################
# Get traffic signals and crossings denominators
# 
# This code obtains the denominators for the following asset comparisons:
# - ASL - occur at signalised junctions
# - Signals - in CID only cycle signals at junctions are included as Signal assets
# - Crossings - in the CID cycle signals at crossings eg pedestrian crossings are in the Crossings dataset
# 
# It does this by combiningin......
# OSM - Jan 2019
# OS MM - March 2019


# Load packages
library(tidyverse)
library(sf)
library(osmextract)
library(mapview)

# Set Mapview options to use data CRS rather than OSM projections
mapviewOptions(native.crs = TRUE)



###############################################################################
#                        Import and manipulate OSM data                       #
###############################################################################
# Determining year of OSM historical data

# Review of survey dates shows that only 17+1 assets were surveyed in 2019.  
# Therefore ok to use the historical OSM data from 2/1/19 to start with. 
# NB may be worth using 2020 if things are missing as OSM could have been updated

# year  Total
# <dbl>  <dbl>
#   1  2017 177593
# 2  2018  56340
# 3  2019     17
# 4  6482      1

# Obtain historical OSM data
# Historical greater london OSM data is available at:
# http://download.geofabrik.de/europe/great-britain/england/greater-london.html#

# Load 2019 OSM dataset
gl_pbf19 = "/home/bananafan/Documents/PhD/Paper1/data/greater-london-190101.osm.pbf"

# import May 2020 ONS LA boundary data (required for NA management)
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")
                       
## Create spatial object for all 33 boroughs
london_union = st_union(lon_lad_2020) 

## Create object with just Borough names and geometry
lon_boroughs = lon_lad_2020 %>%
  select(c("BOROUGH", "geometry"))  

####################################
# Obtaining Traffic lights/signals #
####################################

# https://wiki.openstreetmap.org/wiki/Tag:highway%3Dtraffic_signals

# Import points layer
gl_osm_points = oe_read(gl_pbf19, quiet = FALSE, layer = "points") 
# Simple feature collection with 294419 features and 10 fields
# geographic CRS: WGS 84

# Change CRS to match ONS and CID
gl_osm_points = st_transform(gl_osm_points, crs=27700) # PROJCRS["OSGB 1936 / British National Grid",

names(gl_osm_points)
# [1] "osm_id"     "name"       "barrier"    "highway"    "ref"        "address"    "is_in"     
# [8] "place"      "man_made"   "other_tags" "geometry" 

str(gl_osm_points) # 3294419 obs. of  11 variables, all characters except geometry

unique(gl_osm_points$highway) # 33 unique names inc NA
# [1] "traffic_signals"          NA                         "crossing"                
# [4] "mini_roundabout"          "give_way"                 "speed_camera"            
# [7] "motorway_junction"        "turning_circle"           "junction"                
# [10] "elevator"                 "stop"                     "keep_clear"              
# [13] "bus_stop"                 "turning_loop"             "checkpoint"              
# [16] "street_lamp"              "yield"                    "access"                  
# [19] "passing_place"            "steps"                    "sign"                    
# [22] "services"                 "bus_stand"                "lift"                    
# [25] "spiral_staircase"         "service"                  "milestone"               
# [28] "footway"                  "path"                     "crossing;traffic_signals"
# [31] "escalator"                "noexit"                   "no_exit"                 
# [34] "ser"                

# Factor these values
gl_osm_points$highway = factor(gl_osm_points$highway)

############################
# Initial data exploration #
############################

# Look at junctions
junctions = gl_osm_points %>%
  filter(highway == "junction")  
mapview(junctions) # as only two unlikely to add value but hold onto for now

# Looks at observations coded "crossing;traffic_signals"
c_ts_oddities = gl_osm_points %>%
  filter(highway == "crossing;traffic_signals") # 1 observation
mapview(c_ts_oddities) # this appears to be a crossing at traffic signals so recode as crossing
# c_ts_oddities$osm_id = 2360742427

# Recode c_ts_oddities
gl_osm_points$highway = replace(gl_osm_points$highway, which(gl_osm_points$osm_id == "2360742427"), 
                               values = "crossing")


# Obtain traffic signals dataset
traffic_signals = gl_osm_points %>%
  filter(highway == "traffic_signals") # 5773 observations

# Obtain crossings dataset
crossings = gl_osm_points %>%
  filter(highway == "crossing") # 12933 observations (this includes the one above recoded)

# Examine to see if all are within London boundaries
## Create spatial object for all 33 boroughs
london_union = st_union(lon_lad_2020) 

## Visualise to see if any are outside london boundary
mapview(traffic_signals, cex = 1, zcol = "highway") + 
  mapview(london_union, alpha.regions = 0.1) # Yes some traffic signals are
mapview(crossings, cex = 1, zcol = "highway") + 
  mapview(london_union, alpha.regions = 0.1) # Yes some crossings are

# Limit to within London Boundary
traffic_signals = st_intersection(traffic_signals, london_union) 
crossings = st_intersection(crossings, london_union) 

# spatial join and add Borough label  
## Create object with just Borough names and geometry
lon_boroughs = lon_lad_2020 %>%
  select(c("BOROUGH", "geometry"))  

## Spatially join to London Borough names 
traffic_signals = st_join(traffic_signals, lon_boroughs)
crossings = st_join(crossings, lon_boroughs)




# Focus on Traffic signals
mapview(traffic_signals, cex = 3, zcol = "highway") + mapview(london_union, alpha.regions = 0.1)


#  Both traffic signals and crossings seem to have other tags that suggest that they are both
unique(crossings$other_tags)
unique(traffic_signals$other_tags)


# Examine geometrical proximity
any_touch = any(st_touches(crossings, traffic_signals, sparse = FALSE))
# outcome is FALSE

two_metres = st_is_within_distance(crossings, traffic_signals, dist = 2)
summary(lengths(two_metres)>0)
# Mode      FALSE    TRUE 
# logical   12844      56  # This means that 56 crossings are within 2 m of traffic signals

two_metres_reverse = st_is_within_distance(traffic_signals, crossings, dist = 2)
summary(lengths(two_metres_reverse)>0)
# Mode      FALSE    TRUE 
# logical    5698      57 # This means that 57 traffic signals are within 2 m of traffic crossings


################################################################################
#             Import and manipulate OS MasterMap Highways dataset              #
################################################################################

# Import London road_nodes dataset (n = 231304) Year March 2019
lon_os_road_node = readRDS(file = "/home/bananafan/Documents/PhD/Datasets/lon_os_road_node")

# Data wrangle node dataset
unique(lon_os_road_node$formOfRoadNode)  # identify unique types of RoadNodes
# [1] "roadEnd"             "junction"            "pseudoNode"          "roundabout"         
# [5] "enclosedTrafficArea"

# formofRoadNode codes: 
# Enclosed traffic area = The road node is situated inside and/or represents an enclosed traffic area.A traffic area isan area with no internal structure of legally defined driving directions. At least two roads are connected to the area.
# Junction = Three or more road links intersect at the road node.
# pseudo node = Exactly two road links connect to the road node.
# road end = Only one road link connects to the road node. It signifies the end of a road.
# roundabout = The road node represents or is a part of a roundabout.

# enclosed traffic areas are things like parking lots.  

# Create dataset of junctions
lon_os_road_nodes_junctions = lon_os_road_node %>%
  filter(formOfRoadNode == "junction") # n= 152015


###############################################################################
#            Manipulate and combine OSM and OS MM Highways data               #
###############################################################################

# Visualise

mapview(traffic_signals, cex = 3) + mapview(lon_os_road_nodes_junctions, cex = 3) +
  mapview(london_union, alpha.regions = 0.1)

# limit to one Borough to explore
city_osm_trafficsignals = traffic_signals %>%
  filter(BOROUGH == "City of London") # n = 152
city_os_junctions = lon_os_road_nodes_junctions %>%
  filter(BOROUGH == "City of London") # n = 739

mapview(city_ts$geometry, color = "red", cex = 2) + mapview(city_j$SHAPE, color = "yellow", cex = 2)

#can I match my traffic signals with junction nodes from OS Mastermap
c_asl = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_asl")
city_asl = c_asl %>%
  filter(BOROUGH == "City of London") # n= 122

mapview(city_osm_trafficsignals$geometry, color = "red", cex = 2) + 
  mapview(city_os_junctions$SHAPE, color = "yellow", cex = 2) +
  mapview(city_asl$geometry, color = "green", cex = 2)

mapview(city_osm_trafficsignals, color = "red", cex = 2)


########################################
# Obtaining Borough level summary data #


# Load datasets - these datasets were created 2_3_2021 from TFL datasets downloaded 25/2/21
CID_count = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/CID_count_by_borough")
CID_asl_signals = CID_count %>%
  select(c("BOROUGH", "ASL", "Signals"))

# Add column for inner/outer london
Inner = c("City of London", "Camden", "Greenwich", "Hackney", "Hammersmith & Fulham", 
          "Islington", "Kensington & Chelsea", "Lambeth", "Lewisham", "Southwark",  
          "Tower Hamlets", "Wandsworth", "Westminster") 

CID_asl_signals =  CID_asl_signals %>%
  mutate(London = ifelse(BOROUGH %in% Inner, "Inner London", "Outer London")) # 14 variables

borough_traffic_signals_count = traffic_signals %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Total_number_traffic_signals = n())

asl_signals_trafficlights = left_join(CID_asl_signals, borough_traffic_signals_count)
asl_signals_trafficlights_2019 = asl_signals_trafficlights %>%
  mutate(Prop_TrafficSignals_are_ASL = round(ASL/Total_number_traffic_signals*100)) %>%
  mutate(Prop_TrafficSignals_are_CyclistSignals = round(Signals/Total_number_traffic_signals*100)) 

########## Create 2019 dataset
# Obtain traffic signals dataset

traffic_signals19 = gl_osm_points %>%
  filter(highway == "traffic_signals") # 5773 observations

# Limit to within London Boundary
traffic_signals19 = st_intersection(traffic_signals19, london_union) 

# Spatially join to London Borough names 
traffic_signals19 = st_join(traffic_signals19, lon_boroughs)

# now create table with 19 data
borough_traffic_signals_count = traffic_signals19 %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Total_number_traffic_signals19 = n())
asl_signals_trafficlights = left_join(CID_asl_signals, borough_traffic_signals_count)
asl_signals_trafficlights = asl_signals_trafficlights %>%
  mutate(Prop_TrafficSignals_are_ASL19 = round(ASL/Total_number_traffic_signals19*100)) %>%
  mutate(Prop_TrafficSignals_are_CyclistSignals19 = round(Signals/Total_number_traffic_signals19*100)) 

####Try again with 2020 data
# Load 2020 OSM dataset
gl_pbf20 = "/home/bananafan/Documents/PhD/Paper1/data/greater-london-200101.osm.pbf"
# Import points layer
gl_osm_points20 = oe_read(gl_pbf20, quiet = FALSE, layer = "points") 
#Simple feature collection with 328851 features and 10 fields
# geographic CRS: WGS 84

# Change CRS to match ONS and CID
gl_osm_points20 = st_transform(gl_osm_points20, crs=27700) # PROJCRS["OSGB 1936 / British National Grid",

# Obtain traffic signals dataset
traffic_signals20= gl_osm_points20 %>%
  filter(highway == "traffic_signals") # 7109 observations

# Limit to within London Boundary # n = 7050
traffic_signals20 = st_intersection(traffic_signals20, london_union) 

## Spatially join to London Borough names 
traffic_signals20 = st_join(traffic_signals20, lon_boroughs)

borough_traffic_signals20_count = traffic_signals20 %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Total_number_traffic_signals_20 = n())

asl_signals_trafficlights20 = left_join(CID_asl_signals, borough_traffic_signals20_count)
asl_signals_trafficlights20 = asl_signals_trafficlights20 %>%
  mutate(Prop_20TrafficSignals_are_ASL = round(ASL/Total_number_traffic_signals_20*100)) %>%
  mutate(Prop_20TrafficSignals_are_CyclistSignals = round(Signals/Total_number_traffic_signals_20*100)) 

asl_signal_trafficlight1920 = left_join(asl_signals_trafficlights, asl_signals_trafficlights20)

asl_tl_1920 = asl_signal_trafficlight1920 %>%
  select(c("BOROUGH", "ASL", "London", "Total_number_traffic_signals19", "Prop_TrafficSignals_are_ASL19", 
           "Total_number_traffic_signals_20", "Prop_20TrafficSignals_are_ASL"))

signals_tl_1920 = asl_signal_trafficlight1920 %>%
  select(c("BOROUGH", "Signals", "London", "Total_number_traffic_signals19", "Prop_TrafficSignals_are_CyclistSignals19", 
           "Total_number_traffic_signals_20", "Prop_20TrafficSignals_are_CyclistSignals"))

#Does the data effect change is use rank????

asl_tl_1920_prop_rank = asl_tl_1920 %>%
  mutate(across(.cols = c("ASL", "Prop_TrafficSignals_are_ASL19", 
                          "Prop_20TrafficSignals_are_ASL"),
                .fns = ~round(rank(-.x)), 
                .names = "{.col}_rank")) 

signals_tl_1920_prop_rank = signals_tl_1920 %>%
  mutate(across(.cols = c("Signals", "Prop_TrafficSignals_are_CyclistSignals19", 
                          "Prop_20TrafficSignals_are_CyclistSignals"),
                .fns = ~round(rank(-.x)), 
                .names = "{.col}_rank")) 


# Save these datasets for discussion with Roger
saveRDS(asl_tl_1920_prop_rank, file = "/home/bananafan/Documents/PhD/Paper1/data/asl_tl_1910_prop_rank_comparison")
saveRDS(signals_tl_1920_prop_rank, file = "/home/bananafan/Documents/PhD/Paper1/data/signals_tl_1910_prop_rank_comparison")
saveRDS(traffic_signals20, file = "/home/bananafan/Documents/PhD/Paper1/data/osm_traffic_signals_20")


####################
# Crossings

# Load datasets - these datasets were created 2_3_2021 from TFL datasets downloaded 25/2/21
#CID_count = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/CID_count_by_borough")
CID_crossings = CID_count %>%
  select(c("BOROUGH", "Crossings"))

#crossings19 = crossings

#####  NB Will need to relable that crossing one as per data wrangling
# Limit to within London Boundary
crossings19 = st_intersection(crossings19, london_union) 

# Spatially join to London Borough names
crossings19 = st_join(crossings19, lon_boroughs)

# now create table with 19 data
borough_crossings_count = crossings19 %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Total_number_crossings19 = n())

crossings19_CIDjoined = left_join(CID_crossings, borough_crossings_count)

crossings19_CIDjoined = crossings19_CIDjoined %>%
  mutate(Prop_Crossings_are_CIDcrossings19 = round(Crossings/Total_number_crossings19*100))

#Look at 2020 OSM data
# Obtain traffic signals dataset
crossings20= gl_osm_points20 %>%   ###NB will need to add extra oddity obs when doing properly
  filter(highway == "crossing") # 16124 observations

# Limit to within London Boundary # n = 7050
crossings20 = st_intersection(crossings20, london_union) 

## Spatially join to London Borough names 
crossings20 = st_join(crossings20, lon_boroughs)

borough_crossings20_count = crossings20 %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Total_number_crossings_20 = n())

crossings20_CIDjoined = left_join(CID_crossings, borough_crossings20_count)

crossings20_CIDjoined = crossings20_CIDjoined %>%
  mutate(Prop_Crossings_are_CIDcrossings20 = round(Crossings/Total_number_crossings_20*100))

# Join 19 and 20 data
crossings1920 = left_join(crossings19_CIDjoined, crossings20_CIDjoined)

crossings_1920_prop_rank = crossings1920 %>%
  mutate(across(.cols = c("Crossings", "Prop_Crossings_are_CIDcrossings19", 
                          "Prop_Crossings_are_CIDcrossings20"),
                .fns = ~round(rank(-.x)), 
                .names = "{.col}_rank")) 

# Save these datasets for discussion with Roger
saveRDS(crossings_1920_prop_rank, file = "/home/bananafan/Documents/PhD/Paper1/data/crossings_1910_prop_rank_comparison")
