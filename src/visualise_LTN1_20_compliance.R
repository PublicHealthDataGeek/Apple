################################################################################
#  Visualise degree of compliance with LTN 1/20
# 
# This code takes 2019 osm speed limit data and examines whether the degree of 
# separation of cycling is appropriate for the speed based on LTN 1/20 guidance


# Background
# Order of Protection from motor traffic on highways (DFT guidance pg 33)
# Fully kerbed > stepped > light segregation > Mandatory/Advisory
# FK/S/LS suitable for most people at 20/30 mph only FK suitable for 40mph+
# M/A only suitable for most poepl on 20mph roads with motor vehicle flow of <5000

# Correspnd in CID to:
# CLT_SEGREG > CLT_STEPP > CLT_PARSEG > CLT_MANDAT > CLT_ADVIS
#  NB seems to be little difference in CID between SEGREG and STEPP - majority of 
# stepped are also labelled as segreg and only 5 are labelled as just stepped and they
# look very similar to those that are segreg in the photos


#Load packages
library(osmextract)
library(tidyverse)
library(mapview)
#library(tmap)
library(sf)
#library(tmaptools) # for palette explorer 
#library(viridis)
#library(ggpubr) # for text grobs
#library(ggspatial) # get north arrow and bar

# Package options
mapviewOptions(native.crs = TRUE)
tmap_design_mode(design.mode = FALSE)


################################################################################
#                   Import and data cleanse OSM speed limit data               #
################################################################################

# Load 2019 OSM dataset
gl_pbf19 = "/home/bananafan/Documents/PhD/Paper1/data/greater-london-190101.osm.pbf"
gl_osm_lines19 = oe_read(gl_pbf19, quiet = FALSE, extra_tags = "maxspeed") # Simple feature collection with 313409 features and 10 fields, CRS WGS84

# Change CRS to match ONS and CID
gl_osm_lines19 = st_transform(gl_osm_lines19, crs=27700) # PROJCRS["OSGB 1936 / British National Grid",

# Limit OS data to inside London Boundary
# import May 2020 ONS LA boundary data
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

# Create spatial object for all 33 boroughs
london_union = st_union(lon_lad_2020) 

# Limit road links to within the Outer London Boundary
gl_osm_lines19 = st_intersection(gl_osm_lines19, london_union) # n = ???



names(gl_osm_lines19)
# [1] "osm_id"     "name"       "highway"    "waterway"   "aerialway"  "barrier"   
# [7] "man_made"   "maxspeed"   "z_order"    "other_tags" "geometry"  

unique(gl_osm_lines19$maxspeed) # looks like all sorts of speeds for different types of infrastructure
# [1] "30 mph"   "40 mph"   NA         "20 mph"   "70 mph"   "60 mph"   "50 mph"   "85 mph"  
# [9] "80 mph"   "10 mph"   "15 mph"   "75 mph"   "5 mph"    "45 mph"   "110 mph"  "100 mph" 
# [17] "30 mpoh"  "25 mph"   "50"       "30"       "115 mph"  "230"      "105 mph"  "90 mph"  
# [25] "7"        "signals"  "35 mph"   "65 mph"   "40"       "55 mph"   "20"       "10mph"   
# [33] "12 mph"   "95 mph"   "national" "64"       "none"     "30 mpj"   "5"        "16.09"   
# [41] "walk"     "variable" "4 mph"    "15"       "10"       "125 mph"  "5mph"     "4mph"     

unique(gl_osm_lines19$highway)
# [1] "primary"         "residential"     "trunk"           "footway"         "service"        
# [6] "unclassified"    "tertiary"        "secondary"       "motorway_link"   "cycleway"       
# [11] NA                "motorway"        "tertiary_link"   "secondary_link"  "construction"   
# [16] "bridleway"       "trunk_link"      "pedestrian"      "primary_link"    "path"           
# [21] "living_street"   "steps"           "track"           "unsurfaced"      "raceway"        
# [26] "proposed"        "road"            "no"              "corridor"        "crossing"       
# [31] "escalator"       "elevator"        "stepping_stones" "disused"         "bus_stop"

# https://wiki.openstreetmap.org/wiki/Key:highway
# highway=* distinguishes roads by function and importance rather by their physical characteristic and legal classification.

### Make decisions about which values from highway to keep

# [1] "primary"      KEEP      "residential"   KEEP  "trunk"         KEEP  "trunk_link"     KEEP
# [5] "footway"      DROP      "service"       KEEP  "unclassified"  KEEP  "tertiary"       KEEP       
# [9] "secondary"    KEEP      "motorway_link" DROP  "cycleway"      DROP        NA         ??      
# [13] "motorway"    DROP      "tertiary_link" KEEP  "bridleway"     DROP  "secondary_link" KEEP
# [17] "pedestrian"  FALSE     "primary_link"  KEEP   "path"         DROP  "living_street"  KEEP
# [21] "steps"       DROP      "track"                "construction" DROP  "proposed"       DROP       
# [25] "raceway"     DROP      "road" ????            "no"                  "corridor"      DROP 
# [29] "escalator"   DROP      "elevator"     DROP        "cy"      DROP    "stepping_stones" DROP
# [33] "disused"     DROP      "crossing"     DROP        "access"  DROP

highways_to_keep = c("primary", "residential", "trunk", "trunk_link", "service", "unclassified", "tertiary",
                     "secondary", "tertiary_link", "secondary_link", "primary_link", "living_street")

os_relevent_highways = gl_osm_lines19 %>%
  filter(highway %in% highways_to_keep) # n = 157202
unique(os_relevent_highways$maxspeed) 
max_speed_count = os_relevent_highways %>%
  st_drop_geometry() %>%
  group_by(maxspeed) %>%
  summarise(count = n())

max_speed_count %>% print(n = Inf)
# maxspeed count
# <chr>    <int>
#  1 10           6
# 2 10 mph     381
# 3 10mph        6
# 4 12 mph      11
# 5 15           4
# 6 15 mph     126
# 7 16.09        1
# 8 20          13
# 9 20 mph   44152
# 10 25 mph       2
# 11 30          40
# 12 30 mph   22850
# 13 30 mpj       2
# 14 30 mpoh      1
# 15 4 mph        7
# 16 40           2
# 17 40 mph    2326
# 18 4mph         1
# 19 5            8
# 20 5 mph      644
# 21 50          10
# 22 50 mph    1113
# 23 5mph         2
# 24 60 mph     124
# 25 64           1
# 26 7            4
# 27 70 mph     160
# 28 national     1
# 29 signals     17
# 30 variable     1
# 31 NA       85186

# drop observations with no speed limit data
os_speed_limits = os_relevent_highways %>%
  filter(!is.na(maxspeed)) # n = 72016

# drop observations with signals or variable in the maxspeed
os_speed_limits = os_speed_limits %>%
  filter(maxspeed != "variable") %>%
  filter(maxspeed != "signals") %>%
  filter(maxspeed != "national") # n = 71997

# Remove mph and convert to integer 
os_speed_limits$maxspeed_num = sapply(str_split(os_speed_limits$maxspeed, " mph"), `[`,1) # works byt still have 10mph so run again
os_speed_limits$maxspeed_num = sapply(str_split(os_speed_limits$maxspeed, "mph"), `[`,1)
os_speed_limits$maxspeed_num = sapply(str_split(os_speed_limits$maxspeed_num, " mpj"), `[`,1)
os_speed_limits$maxspeed_num = sapply(str_split(os_speed_limits$maxspeed_num, " mpoh"), `[`,1)
os_speed_limits$maxspeed_num = as.integer(os_speed_limits$maxspeed_num)

# Create speed limit groups
os_speed_limits = os_speed_limits %>%
  mutate(speed_limit = cut(maxspeed_num,
                           breaks = seq(0, 70, by = 10),
                           labels = c("10mph", "20mph", "30mph", "40mph", "50mph", "60mph", "70mph"))) 
tidy_speed_limit = os_speed_limits %>%
  st_drop_geometry() %>%
  group_by(speed_limit) %>%
  summarise(count = n())

# speed_limit count
# 1 10mph        1059
# 2 20mph       44307
# 3 30mph       22895
# 4 40mph        2328
# 5 50mph        1123
# 6 60mph         124
# 7 70mph         161

mapview(os_speed_limits, zcol = "speed_limit")


############################################
# Join speed limit data to Cycle lane data #
############################################

#IMPORT cid DATA FROM VISUALISE CYCLELANES.r


# Select OSM variables to keep 
lon_os_speed_limits_tidy = lon_os_speed_limits %>%
  select(c("osm_id", "name", "highway", "maxspeed_num", "speed_limit", "geometry"))

#  Now test how to join the speed limit data to the CID data
# select small amount of data
barnet_cl = on_road_factor %>%
  filter(BOROUGH == "Barnet") # 93 obs
barnet_borough = lon_lad_2020 %>%
  filter(BOROUGH == "Barnet")
barnet_os_speeds_limits = st_intersection(lon_os_speed_limits_tidy, barnet_borough)

# View data
m1 =  mapview(barnet_cl, zcol = "Highest_separation")
m2 = mapview(barnet_os_speeds_limits, zcol = "speed_limit", color = brewer.pal(9, "YlOrRd"))
leafsync::sync(m1,m2)


# Try different spatial join approaches to examine impact
# 1) st_join with st_intersects
barnet_speed_intersects = st_join(barnet_cl, lon_os_speed_limits_tidy) # => 22 assets have speed limit data - this uses st_intersects

#2) st_join with nearest neighbour - ? query not granular enough
barnet_speed_nearest = st_join(barnet_cl, lon_os_speed_limits_tidy, join = st_nearest_feature) #= all assets have speed limit data

# 3) Create buffer around osm speed data observations then join based on within certain distance
barnet_sl_buffer = st_buffer(barnet_os_speeds_limits, dist = 3)
mapview(barnet_sl_buffer)
barnet_speed_buffer_within = st_join(barnet_cl, barnet_sl_buffer, join = st_within) # ie entire cycle lane is within the speed limit buffer
# 7 assets had speed limit data

barnet_speed_buffer_touches = st_join(barnet_cl, barnet_sl_buffer, join = st_touches) 
# 0 assets

# 4) try buffer around CID
barnet_cl_buffer = st_buffer(barnet_cl, dist = 2) # 2m buffer around cycle lanes
mapview(barnet_cl_buffer) + mapview(barnet_cl, zcol = "Highest_separation")

barnet_cl_buffer_touches = st_join(barnet_cl_buffer,barnet_os_speeds_limits, join = st_touches) # 0 assets
barnet_cl_buffer_intersects = st_join(barnet_cl_buffer,barnet_os_speeds_limits)  # now have 113 assets - some new ones created by the intersections, others dont have speed limit attached



# now write some logic that checks to see if degree of separation matches the road speed limit.  
# use max speed nuermic from os

#40+ can onlyl be fully seg
# 30+ can be seg, step or par
# 20mph can be any

# if speed limit = 40+ then higest seg must be segregate 
# if speed limit - 30 + then seg must be seg step or part seg_levels
# if speed limit =20 then seg can be any value.  

#column = is seg approp to speed limit - do want t/f

barnet_speed_near_test = barnet_speed_nearest %>%
  mutate(seg_appro_speed_limit = case_when((maxspeed_num == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (maxspeed_num == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (maxspeed_num == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (maxspeed_num == 30) & (Highest_separation == "Stepped") ~ "TRUE",
                                           (maxspeed_num == 30) & (Highest_separation == "Part-segregated") ~ "TRUE",
                                           (maxspeed_num <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (maxspeed_num <= 20) & (Highest_separation == "Stepped") ~ "TRUE",
                                           (maxspeed_num <= 20) & (Highest_separation == "Part-segregated") ~ "TRUE",
                                           (maxspeed_num <= 20) & (Highest_separation == "Mandatory cycle lane") ~ "TRUE",
                                           (maxspeed_num <= 20) & (Highest_separation == "Advisory cycle lane") ~ "TRUE",
                                           (maxspeed_num <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           TRUE ~ "FALSE"))
# THis seems to work for barnet - need to check on other datasets

test = st_join(on_road_factor, lon_os_speed_limits_tidy, join = st_nearest_feature)

# Count the number of highways by type and percent of total count
highway_count = gl_osm_lines20 %>%
  st_drop_geometry() %>%
  group_by(highway) %>%
  summarise(count = n()) %>%
  mutate(percent = round(count/sum(count)*100)) %>%
  arrange(desc(percent), desc(count))











