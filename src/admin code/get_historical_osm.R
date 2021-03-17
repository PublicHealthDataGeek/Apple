###############################
# Getting historical OSM data #
# Created 16/11/20            #
#                             #
###############################

# Step 1: obtain historical OSM data
# -----------------------------------

# Historical OSM data is available via geofabrik
# http://download.geofabrik.de/
#
# Technical info is here:
# https://download.geofabrik.de/technical.html
#
# Historical greater london OSM data is available at:
# http://download.geofabrik.de/europe/great-britain/england/greater-london.html#

# 

# Step 2: Load historical information into R
# ------------------------------------------

# load packages
library(osmextract)
library(sf)
library(mapview)
library(tidyverse)

# Set Mapview options to use data CRS rather than OSM projections
mapviewOptions(native.crs = TRUE)

# load OSM dataset
gl_pbf = "/home/bananafan/Documents/PhD/Paper1/data/greater-london-200101.osm.pbf"

# import May 2020 ONS LA boundary data (required for NA management)
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")


##############
# Traffic lights/signals
#
#####################

# https://wiki.openstreetmap.org/wiki/Tag:highway%3Dtraffic_signals

# Import points layer
gl_osm_points = oe_read(gl_pbf, quiet = FALSE, layer = "points") 
# 328851 features and 10 fields, point
# geographic CRS: WGS 84

# Change CRS to match ONS and CID
gl_osm_points = st_transform(gl_osm_points, crs=27700) # PROJCRS["OSGB 1936 / British National Grid",

names(gl_osm_points)
# [1] "osm_id"     "name"       "barrier"    "highway"    "ref"        "address"    "is_in"     
# [8] "place"      "man_made"   "other_tags" "geometry" 

str(gl_osm_points) # 328851 obs. of  11 variables, all characters except geometry

unique(gl_osm_points$highway) # 33 unique names inc NA
# [1] NA                         "traffic_signals"          "crossing"                
# [4] "mini_roundabout"          "give_way"                 "speed_camera"            
# [7] "motorway_junction"        "turning_circle"           "milestone"               
# [10] "junction"                 "elevator"                 "stop"                    
# [13] "keep_clear"               "bus_stop"                 "turning_loop"            
# [16] "checkpoint"               "street_lamp"              "yield"                   
# [19] "services"                 "access"                   "passing_place"           
# [22] "steps"                    "sign"                     "lift"                    
# [25] "footway"                  "spiral_staircase"         "bus_stand"               
# [28] "crossing;traffic_signals" "path"                     "traffic_signals;crossing"
# [31] "noexit"                   "no_exit"                  "residential" 

gl_osm_points$highway = factor(gl_osm_points$highway)
unique(gl_osm_points$highway)

traffic_signals_crossings = gl_osm_points %>%
  filter(highway == "traffic_signals" | highway == "crossing" | highway == "crossing;traffic_signals" | highway == "traffic_signals;crossing")



# visualise - some are outside london boundary
mapview(traffic_signals_crossings, cex = 1, zcol = "highway") + mapview(london_union, alpha.regions = 0.1)

# Limit traffic_signals_crossings to within London Boundary
## Create spatial object for all 33 boroughs
london_union = st_union(lon_lad_2020) 
## Limit tsc to within the Outer London Boundary
traffic_signals_crossings = st_intersection(traffic_signals_crossings, london_union)


# spatial join and add Borough label  
## Create object with just Borough names and geometry
lon_boroughs = lon_lad_2020 %>%
  select(c("BOROUGH", "geometry"))  

## Spatially join London road link data to London Borough names 
traffic_signals_crossings = st_join(traffic_signals_crossings, lon_boroughs)


# Focus on Traffic signals

traffic_signals = traffic_signals_crossings %>% 
  filter(highway == "traffic_signals" | highway == "traffic_signals;crossing")
mapview(traffic_signals, cex = 3, zcol = "highway") + mapview(london_union, alpha.regions = 0.1)

traffic_signals_count = traffic_signals %>%
  st_drop_geometry() %>%
  group_by(highway) %>%
  summarise(count = n())
highway                  count
* <fct>                    <int>
  1 traffic_signals           7050
2 traffic_signals;crossing     1

traffic_signals_crossings_count = traffic_signals_crossings %>%
  st_drop_geometry() %>%
  group_by(highway) %>%
  summarise(count = n())

# highway                  count
# * <fct>                    <int>
# 1 crossing                 16044
# 2 crossing;traffic_signals     2
# 3 traffic_signals           7050
# 4 traffic_signals;crossing     1

# Examine crossing;traffic_signals and traffic_signals;crossing
query_tsc = traffic_signals_crossings %>% 
  filter(highway == "crossing;traffic_signals" | highway == "traffic_signals;crossing")
mapview(query_tsc)
#1795462099 and = 5923168568 bicycle crossings at traffic signals
# 2592923410 ? not sure
mapview(gl_osm_points) + mapview(lon_lad_2020, alpha.regions = 0.1)

traffic_signals =  traffic_signals_crossings %>% 
  filter(highway == "traffic_signals")

crossings = traffic_signals_crossings %>% 
  filter(highway == "crossing")

#  Both traffic signals and crossings seem to have other tags that suggest that they are both
unique(crossings$other_tags)
unique(traffic_signals$other_tags)

# Try geom

any_touch = any(st_touches(crossings, traffic_signals, sparse = FALSE))
# outcome is FALSE

two_metres = st_is_within_distance(crossings, traffic_signals, dist = 2)
summary(lengths(two_metres)>0)
# Mode      FALSE    TRUE 
# logical   15937     107  # This means that 107 crossings are within 2 m of traffic signals

two_metres_reverse = st_is_within_distance(traffic_signals, crossings, dist = 2)
summary(lengths(two_metres_reverse)>0)
# Mode      FALSE    TRUE 
# logical    6940     110 # This means that 110 traffic signals are within 2 m of traffic crossings

# Join
two_m_join = st_join(crossings, traffic_signals, join = st_is_within_distance, dist =2)
unique(two_m_join$osm_id.x)

two_m_join_reverse = st_join(traffic_signals, crossings, join = st_is_within_distance, dist =2)
  
  
# Limit road links to within the Outer London Boundary
)
check = st_intersection(gl_osm_points, london_union)


#can I match my traffic signals with junction nodes from OS Mastermap














# Get different osm layers
gl_osm_lines = oe_read(gl_pbf, quiet = FALSE) # 355244 obs. of  11 variables, linestring, crs= WGS84
gl_osm_points = oe_read(gl_pbf, quiet = FALSE, layer = "points") # 328851 features and 10 fields, point, crs= WGS84 
gl_osm_mls = oe_read(gl_pbf, quiet = FALSE, layer = "multilinestrings") # 4413 features, 4 fields, multilinestring, crs= WGS84 
gl_osm_multipolygons = oe_read(gl_pbf, quiet = FALSE, layer = "multipolygons") # 546748 features, 25 fields, multipolygon, crs= WGS84 


# examine gl_osm_lines dataset
str(gl_osm_lines) # 355244 obs. of  11 variables, all characters except 'z_order'
unique(gl_osm_lines$highway) # 35 unique names inc NA
unique(gl_osm_lines$waterway) # 18 unique names inc NA - ? any relevance? 
unique(gl_osm_lines$aerialway) #  4 unique names inc NA - dont look to be relevant
unique(gl_osm_lines$barrier) # 36 unique names inc NA
unique(gl_osm_lines$man_made) # 45 unique names in NA - dont look to be relevant
unique(gl_osm_lines$maxspeed) # 46 unique names inc NA but some are above speed limit
unique(gl_osm_lines$z_order) # integer, ranges from -50 to +160
unique(gl_osm_lines$other_tags) # massive amount of data!!!!!!

# examine gl_osm_points dataset
str(gl_osm_points) # 328851 obs. of  11 variables, all characters
unique(gl_osm_points$name) # 46127 unique names inc NA
unique(gl_osm_points$barrier) # 68 unique names in NA
unique(gl_osm_points$highway) # 33 unique names inc NA
unique(gl_osm_points$ref) # 13343 unique names - some look like postcodes
unique(gl_osm_points$address) # 9 unique names in NA
unique(gl_osm_points$is_in) # 116 unique names - frequent boroughs but multiple spellings, sometimes towns
unique(gl_osm_points$place) # 14 plus NA, description of place eg village, city, hamlet etc
unique(gl_osm_points$man_made) # 57 unique - dont look to be relevant to cycling infrastructure

# examine gl_osm_mls dataset
str(gl_osm_mls) # 4413 obs, 5 variables, all characters
unique(gl_osm_mls$name) # 2667 unique names - refer to train lines, bus lines and LCN
unique(gl_osm_mls$type) # 1 'route'
# will need to use other to find cycling related MLS

# examine gl_osm_multipolygon dataset 
str(gl_osm_multipolygons) # 546748 obs. of  26 variables
# 
# $ osm_id     : chr  "152" "332" "406" "819" ...
# $ osm_way_id : chr  NA NA NA NA ...
# $ name       : chr  NA "Kingsmere" NA "The Lake" ...
# $ type       : chr  "multipolygon" "multipolygon" "multipolygon" "multipolygon" ...
# $ aeroway    : chr  NA NA NA NA ...
# $ amenity    : chr  NA NA NA NA ...
# $ admin_level: chr  NA NA NA NA ...
# $ barrier    : chr  NA NA NA NA ...
# $ boundary   : chr  NA NA NA NA ...
# $ building   : chr  NA NA NA NA ...
# $ craft      : chr  NA NA NA NA ...
# $ geological : chr  NA NA NA NA ...
# $ historic   : chr  NA NA NA NA ...
# $ land_area  : chr  NA NA NA NA ...
# $ landuse    : chr  NA NA NA NA ...
# $ leisure    : chr  NA NA NA NA ...
# $ man_made   : chr  NA NA NA NA ...
# $ military   : chr  NA NA NA NA ...
# $ natural    : chr  "water" "water" "water" "water" ...
# $ office     : chr  NA NA NA NA ...
# $ place      : chr  NA NA NA NA ...
# $ shop       : chr  NA NA NA NA ...
# $ sport      : chr  NA NA NA NA ...
# $ tourism    : chr  NA NA NA NA ...
# $ other_tags


##############
# Traffic lights/signals
#
#####################

# https://wiki.openstreetmap.org/wiki/Tag:highway%3Dtraffic_signals

unique(gl_osm_points$highway) # 33 unique names inc NA

[1] NA                         "traffic_signals"          "crossing"                
[4] "mini_roundabout"          "give_way"                 "speed_camera"            
[7] "motorway_junction"        "turning_circle"           "milestone"               
[10] "junction"                 "elevator"                 "stop"                    
[13] "keep_clear"               "bus_stop"                 "turning_loop"            
[16] "checkpoint"               "street_lamp"              "yield"                   
[19] "services"                 "access"                   "passing_place"           
[22] "steps"                    "sign"                     "lift"                    
[25] "footway"                  "spiral_staircase"         "bus_stand"               
[28] "crossing;traffic_signals" "path"                     "traffic_signals;crossing"
[31] "noexit"                   "no_exit"                  "residential" 



# import May 2020 ONS LA boundary data (required for NA management)
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

# Create spatial object for all 33 boroughs
london_union = st_union(lon_lad_2020) 

st_crs(gl_osm_points)
gl_osm_points = st_transform(gl_osm_points, crs=27700) 
st_crs(gl_osm_points) # PROJCRS["OSGB 1936 / British National Grid",

mapview(gl_osm_points) + mapview(lon_lad_2020, alpha.regions = 0.1)

# Limit road links to within the Outer London Boundary
check = st_intersection(gl_osm_points, london_union)






# CAn add specific tags (e.g. 'max speed') which adds it as an extra column


test = oe_read(gl_pbf, quiet = FALSE, extra_tags = c("maxspeed"))
