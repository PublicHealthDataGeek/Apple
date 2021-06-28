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
library(RColorBrewer)
library(leafem) # for adding labels to mapview
library(leafsync) # for syncing mapview maps
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
gl_osm_lines19 = oe_read(gl_pbf19, quiet = FALSE, 
                         extra_tags = c("maxspeed", "width", "maxwidth", "lanes")) # Simple feature collection with 313409 features and 10 fields, CRS WGS84

# Change CRS to match ONS and CID
gl_osm_lines19 = st_transform(gl_osm_lines19, crs=27700) # PROJCRS["OSGB 1936 / British National Grid", n = 313409

# Limit OS data to inside London Boundary
# import May 2020 ONS LA boundary data
lon_lad_2020_bfe = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

# Create spatial object for all 33 boroughs
london_union_bfe = st_union(lon_lad_2020_bfe) 

# Limit road links to within the Outer London Boundary
lon_osm_lines19 = st_intersection(gl_osm_lines19, london_union_bfe) # n = 310848


names(lon_osm_lines19)
# 1] "osm_id"     "name"       "highway"    "waterway"   "aerialway"  "barrier"    "man_made"  
# [8] "maxspeed"   "width"      "maxwidth"   "lanes"      "z_order"    "other_tags" "geometry"   

  
# Keep observations that are highway types that would be relevent to onroad cycle lanes
unique(lon_osm_lines19$highway)
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

osm_relevent_highways = lon_osm_lines19 %>%
  filter(highway %in% highways_to_keep) # n = 155920


# Examine maxspeed variable
max_speed_count = osm_relevent_highways %>%
  st_drop_geometry() %>%
  group_by(maxspeed) %>%
  summarise(count = n())

max_speed_count %>% print(n = Inf)
# maxspeed count
# <chr>    <int>
# 1 10           6
# 2 10 mph     368
# 3 10mph        6
# 4 12 mph      11
# 5 15           3
# 6 15 mph     115
# 7 16.09        1
# 8 20          13
# 9 20 mph   44134
# 10 25 mph       2
# 11 30          40
# 12 30 mph   22714
# 13 30 mpj       2
# 14 30 mpoh      1
# 15 4 mph        7
# 16 40           2
# 17 40 mph    2259
# 18 4mph         1
# 19 5            8
# 20 5 mph      635
# 21 50          10
# 22 50 mph    1097
# 23 5mph         2
# 24 60 mph      96
# 25 64           1
# 26 7            4
# 27 70 mph     122
# 28 national     1
# 29 signals     17
# 30 variable     1
# 31 NA       84241

# drop observations with no speed limit data
osm_speed_limits = osm_relevent_highways %>%
  filter(!is.na(maxspeed)) # n = 71679

# drop observations with signals or variable in the maxspeed
osm_speed_limits = osm_speed_limits %>%
  filter(maxspeed != "variable") %>%
  filter(maxspeed != "signals") %>%
  filter(maxspeed != "national") # n = 71660

# Remove mph and convert to integer 
osm_speed_limits$maxspeed_num = sapply(str_split(osm_speed_limits$maxspeed, " mph"), `[`,1) # works byt still have 10mph so run again
osm_speed_limits$maxspeed_num = sapply(str_split(osm_speed_limits$maxspeed, "mph"), `[`,1)
osm_speed_limits$maxspeed_num = sapply(str_split(osm_speed_limits$maxspeed_num, " mpj"), `[`,1)
osm_speed_limits$maxspeed_num = sapply(str_split(osm_speed_limits$maxspeed_num, " mpoh"), `[`,1)
osm_speed_limits$maxspeed_num = as.integer(osm_speed_limits$maxspeed_num)

# Create speed limit groups
osm_speed_limits = osm_speed_limits %>%
  mutate(speed_limit = cut(maxspeed_num,
                           breaks = seq(0, 70, by = 10),
                           labels = c("10mph", "20mph", "30mph", "40mph", "50mph", "60mph", "70mph"))) 
tidy_speed_limit = osm_speed_limits %>%
  st_drop_geometry() %>%
  group_by(speed_limit) %>%
  summarise(count = n())

# speed_limit count
# <fct>       <int>
# 1 10mph        1037
# 2 20mph       44277
# 3 30mph       22759
# 4 40mph        2261
# 5 50mph        1107
# 6 60mph          96
# 7 70mph         123

mapview(osm_speed_limits, zcol = "speed_limit")

# Select OSM variables to keep 
osm_speed_limits_tidy = osm_speed_limits %>%
  select(c("osm_id", "name", "highway", "maxspeed_num", "speed_limit", "geometry", "lanes"))

#################
# Save OSM data #
#################

#saveRDS(osm_speed_limits_tidy, file = "/home/bananafan/Documents/PhD/Paper1/data/lon_osm_speed_limits_tidy.Rds")






############################################
# Join speed limit data to Cycle lane data #
############################################

# Import cleansed onroad cycle lanes dataframe that has segregation information coded (created in VISUALISE CYCLELANES.r)
cycle_lanes = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_onroad_cyclelanes_segregation.Rds")

# collapse segregation into these categories that match Figure 4.1 in LTN 1/20
# Convert factored numbers to relevant labels
cycle_lanes = cycle_lanes %>%
  select(c(FEATURE_ID, BOROUGH, geometry, length_m, type, Highest_separation)) %>%
  mutate(Highest_separation = fct_collapse(Highest_separation, 
                                           "Stepped/part segregation" = c("Stepped", "Part-segregated"),
                                           "Mandatory/Advisory cycle lane" = c("Mandatory cycle lane", "Advisory cycle lane")))
# Highest_separation            `n()`
# <fct>                         <int>
# 1 Segregated                     1371   -> Any speed limit, any traffic flow
# 2 Stepped/part segregation        354   -> max 30moh, any traffic flow
# 3 Mandatory/Advisory cycle lane  8868   -> max20mph and traffic flow under 5000 
# 4 No separation                  3372   -> max 20mp and traffic flow under 2500


# Import tidied london OSM speed limit dataset
osm_speed_limits_tidy = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/lon_osm_speed_limits_tidy.Rds")
# refactor speed limit
osm_speed_limits_tidy$speed_limit = fct_collapse(osm_speed_limits_tidy$speed_limit, 
               "20mph" = c("10mph", "20mph"))

# import May 2020 ONS LA boundary data
lon_lad_2020_bfe = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

#  Now test how to join the speed limit data to the CID data
# select small amount of data
barnet_cl = cycle_lanes %>%
  filter(BOROUGH == "Barnet") # 93 observations of cycle lanes in Barnet
barnet_borough = lon_lad_2020_bfe %>%
  filter(BOROUGH == "Barnet") # obtain boundaries of Barnet
barnet_speeds_limits = st_intersection(osm_speed_limits_tidy, barnet_borough) # n = 4810, osm objects within Barnet borough

# View data
m1 =  mapview(barnet_cl, zcol = "Highest_separation")
b_speedlimits = mapview(barnet_speeds_limits, zcol = "speed_limit", color = brewer.pal(9, "YlOrRd"))
leafsync::sync(m1, b_speedlimits)  # initial visualisation suggests that unlikely that any cycle lanes are on roads > 30mph

barnet_cl_buffer = st_buffer(barnet_cl, dist = 6) #6m buffer around cycle lanes
barnet_sl_buffer_73 = st_buffer(barnet_speeds_limits, dist = 7.3) # 7.3m around road - allowing for 2 way traffic
barnet_intersects = st_join(barnet_cl_buffer, barnet_sl_buffer_73) # n = 297 (was 93)

barnet_within = st_join(barnet_cl_buffer, barnet_sl_buffer_73, join = st_within) # n = 93)
road_intersects = barnet_speeds_limits[barnet_cl_buffer, ]  # this code spatial selects - see geocomp with R
mapview(road_intersects) + mapview(barnet_cl_buffer)

# Next steps from d/w RL
#try 1) increasing buffer size (10m for roads) and use st_within 
# and if that doesn’t work then 2) use line_segment (?stplanr) to break up road lengths 
# Raise issue on github stplanr 



barnet_intersects %>%
  st_drop_geometry() %>%
  group_by(speed_limit) %>%
  summarise(count = n())

# 1 20mph          22
# 2 30mph         262
# 3 40mph           2
# 4 50mph           7
# 5 NA              4

# examine whether speed limits differ by new segments of each FEATURE_ID
count_na <- function(x) sum(is.na(x))  # function that counts number of NAs in a row
single_speed_limit = barnet_intersects %>%
  st_drop_geometry() %>%
  select(c("FEATURE_ID", "speed_limit")) %>%
  pivot_wider(id_cols = c("FEATURE_ID", "speed_limit"), 
              names_from = speed_limit,
              values_from = speed_limit,
              names_glue = "{speed_limit}_{.value}",
              values_fn = length) %>%
  select(c("FEATURE_ID", "20mph_speed_limit", "30mph_speed_limit", "40mph_speed_limit", "50mph_speed_limit", "NA_speed_limit")) %>%
  mutate(count_na = apply(., 1, count_na)) %>%
  mutate(single_speed_limit = case_when(count_na == 4  ~ TRUE,
                                        TRUE ~ FALSE))  # if there are 4 NAs and 1 speedlimit then set 'single_speed_limit' to TRUE

# 79 only had 1 speed limit, 14 had 2 different speed limits

         
         
##########################################################################################         
barnet_20 = barnet_intersects %>%
  filter(speed_limit == "20mph")
b_20_map = mapview(barnet_20) 
b_20_sl = barnet_speeds_limits %>%
  filter(speed_limit == "20mph")
b_20_sl_map = mapview(b_20_sl)

leafsync::sync(b_20_map, b_20_sl_map)

barnet_NA = barnet_intersects %>%
  filter(is.na(speed_limit)) # n = 4
b_NA_map = mapview(barnet_NA) 
b_NA_map = leafem::addStaticLabels(b_NA_map, label =  barnet_NA$FEATURE_ID)

barnet_NA_map = leafsync::sync(b_NA_map, b_speedlimits)


bar_intersects_map = mapview(barnet_intersects, zcol = "speed_limit")
b_sl_73 = mapview(barnet_buffer_73, zcol = "speed_limit", color = brewer.pal(9, "YlOrRd"))
leafsync::sync(br, b_sl_73)




# Divide barnet_cl dataset into rest, contra and shared as likely to need different sized buffers
rest_barnet_cl = barnet_cl %>%
  filter(type == "Rest") # n = 63
contra_barnet_cl = barnet_cl %>%
  filter(type == "Contraflow") # n = 7
shared_barnet_cl = barnet_cl %>%
  filter(type == "Shared") # n= 23  ? will shared need this - ?most likely to be along a normal road

# # work on contra to start with - the below all works and correctly identifies that the contraflow cycle lanes are 30mph
# mapview(contra_barnet_cl, zcol = "FEATURE_ID")
# contra_barnet_cl_6m = st_buffer(contra_barnet_cl, dist = 6)  #6m buffer around contraflow cycle lane
# barnet_buffer_365 = st_buffer(barnet_os_speeds_limits, dist = 3.65)  # 3.65m buffer around roads
# barnet_contra_intersects = st_join(contra_barnet_cl_6m, barnet_buffer_365) # intersect buffers -> 16 obs (originally was 7)
# bc = mapview(barnet_contra_intersects) 
# b_sl = mapview(barnet_os_speeds_limits)
# leafsync::sync(bc, b_sl)

# Try Barnet rest dataset
mapview(rest_barnet_cl)
rest_barnet_buffer = st_buffer(rest_barnet_cl, dist = 6) #6m buffer around cycle lanes
barnet_buffer_73 = st_buffer(barnet_os_speeds_limits, dist = 7.3) # 7.3m around road - allowing for 2 way traffic
barnet_rest_intersects = st_join(rest_barnet_buffer, barnet_buffer_73) # n = 208 (was 63)
br = mapview(barnet_rest_intersects)
b_sl_73 = mapview(barnet_buffer_73, zcol = "speed_limit", color = brewer.pal(9, "YlOrRd"))
leafsync::sync(br, b_sl_73)




# Try different spatial join approaches to examine impact
# 1) st_join with st_intersects
barnet_speed_intersect_buffer = st_join(barnet_cl, barnet_buffer_365) # => only 22 assets have speed limit data - this uses st_intersects

#2) st_join with nearest neighbour - ? query not granular enough
barnet_speed_nearest = st_join(barnet_cl, barnet_speeds_limits, join = st_nearest_feature) #= all assets have speed limit data

# 3) Create buffer around CID
barnet_cl_buffer2 = st_buffer(barnet_cl, dist = 2) # 2m buffer around cycle lanes
barnet_cl_buffer3 = st_buffer(barnet_cl, dist = 3) # 3m buffer around cycle lanes
barnet_cl_buffer4 = st_buffer(barnet_cl, dist = 4) # 4m buffer around cycle lanes

# visualise the above
mapview(barnet_cl_buffer3) + mapview(barnet_cl, zcol = "Highest_separation") + 
  mapview(barnet_speeds_limits, zcol = "speed_limit", color = brewer.pal(9, "YlOrRd"))
# can see that the osm speed limit is represented by a single line that isnt necessarily touched by the cycle lane buffers.  





# code useful for checking? 
intersections_lp <- st_intersection(lines, polygons) %>% 
  mutate(int_name = paste0(line_name, "-", polygon_name)) # gives a column with a name of the line that intersects with the polygon
#then can plot
ggplot() +
  #--- here are all the original polygons  ---#
  geom_sf(data = polygons, aes(fill = polygon_name), alpha = 0.3) +
  #--- here is what is returned after st_intersection ---#
  geom_sf(data = intersections_lp, aes(color = int_name), size = 1.5)

# osm speed data observations then join based on within certain distance
barnet_sl_buffer = st_buffer(barnet_speeds_limits, dist = 3)
mapview(barnet_sl_buffer)
barnet_speed_buffer_within = st_join(barnet_cl, barnet_sl_buffer, join = st_within) # ie entire cycle lane is within the speed limit buffer
# 7 assets had speed limit data

barnet_speed_buffer_touches = st_join(barnet_cl, barnet_sl_buffer, join = st_touches) 
# 0 assets





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

# draft attempt - might not be correct
barnet_speed_near_test = barnet_speed_nearest %>%
  mutate(seg_appro_speed_limit = case_when((maxspeed_num == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (maxspeed_num == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (maxspeed_num == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (maxspeed_num == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (maxspeed_num <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (maxspeed_num <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (maxspeed_num <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (maxspeed_num <= 20) & (Highest_separation == "No separation") ~ "FALSE",   ### IS tHIS CORRECT??? Lots are shared bus lanes
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








#ADDITIONAL CODE EXAMINING IF OTHER TAGS ARE USEFUL
# Look at other tags to see if useful for obtaining the best spatial joins of OSM to CID data
# unique(osm_speed_limits$width)
# #  [1] NA      "6.5"   "3"     "4"     "2.5 m" "6'6\"" "3.7"   "5"     "2.13"  "7.4"   "2"     "2.5"  
# # [13] "10 m"  "8.5"   "11"    "13"    "1"     "11.4"  "14.6"  "15.4"  "11.7"  "12.6"  "12.5"  "12"   
# # [25] "7"     "6"     "0"  
# 
# unique(osm_speed_limits$maxwidth)
# # [1] NA       "2.1"    "2"      "2.2"    "6'6\""  "1.98"   "1.9"    "2.4"    "7'0\""  "2.13"   "7 ft"  
# # [12] "2.1336" "2.18"   "2.0"    "6'0\""  "3 m"    "7 '"    "5'6\""  "2 m"    "7'2\""  "7'0''" 
# 
# unique(osm_speed_limits$lanes)
# #[1] NA    "2"   "4"   "3"   "1"   "5"   "6"   "0"   "1.5" "8"  
# 
# osm_speed_limits %>%
#   st_drop_geometry() %>%
#   group_by(width) %>%
#   summarise(count = n()) %>%
#   print(n = Inf)  # 71565/71660 were NA
# 
# osm_speed_limits %>%
#   st_drop_geometry() %>%
#   group_by(maxwidth) %>%
#   summarise(count = n()) %>%
#   print(n = Inf)  # 71534/71660 were NA
# 
# osm_speed_limits %>%
#   st_drop_geometry() %>%
#   group_by(lanes) %>%
#   summarise(count = n()) %>%
#   print(n = Inf)  # 60365/71660 were NA


