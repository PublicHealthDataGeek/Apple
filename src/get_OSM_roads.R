#########################################################
#  Get OSM lines data in order to measures road network
#
#### NEED TO DO SAME STUFF WITH GEOSPATIAL CHECKING IN LONDON AND SPATIAL JOIN TO BOAROUGH AS PER TRAFFIC SIGNALS

# load packages
library(osmextract)
library(sf)
library(mapview)
library(tidyverse)

# Set Mapview options to use data CRS rather than OSM projections
mapviewOptions(native.crs = TRUE)

# load OSM dataset
gl_pbf = "/home/bananafan/Documents/PhD/Paper1/data/greater-london-200101.osm.pbf"
gl_osm_lines20 = oe_read(gl_pbf, quiet = FALSE) # Simple feature collection with 355244 features and 10 fields, CRS WGS84

# Change CRS to match ONS and CID
gl_osm_lines20 = st_transform(gl_osm_lines20, crs=27700) # PROJCRS["OSGB 1936 / British National Grid",

# import May 2020 ONS LA boundary data (required for NA management)
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")


names(gl_osm_lines20)
# [1] "osm_id"     "name"       "highway"    "waterway"   "aerialway"  "barrier"   
# [7] "man_made"   "maxspeed"   "z_order"    "other_tags" "geometry"  

unique(gl_osm_lines20$highway)
# [1] "primary"         "residential"     "trunk"           "trunk_link"     
# [5] "footway"         "service"         "unclassified"    "tertiary"       
# [9] "secondary"       "motorway_link"   "cycleway"        NA               
# [13] "motorway"        "tertiary_link"   "bridleway"       "secondary_link" 
# [17] "pedestrian"      "primary_link"    "path"            "living_street"  
# [21] "steps"           "track"           "construction"    "proposed"       
# [25] "raceway"         "road"            "no"              "corridor"       
# [29] "escalator"       "elevator"        "cy"              "stepping_stones"
# [33] "disused"         "crossing"        "access" 

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

# Factor highway columns
gl_osm_lines20$highway = factor(gl_osm_lines20$highway)

# Count the number of highways by type and percent of total count
highway_count = gl_osm_lines20 %>%
  st_drop_geometry() %>%
  group_by(highway) %>%
  summarise(count = n()) %>%
  mutate(percent = round(count/sum(count)*100)) %>%
  arrange(desc(percent), desc(count))

highway_count %>% print(n = Inf)
# A tibble: 35 x 3
# highway         count percent
# <fct>           <int>   <dbl>
# 1 NA              74726      21
# 2 residential     70707      20
# 3 footway         66480      19
# 4 service         62371      18
# 5 cycleway        15329       4
# 6 primary         12709       4
# 7 unclassified    10215       3
# 8 tertiary         9651       3
# 9 path             7799       2
# 10 steps            6897       2
# 11 trunk            6158       2
# 12 secondary        3717       1
# 13 track            2784       1
# 14 pedestrian       1767       0
# 15 trunk_link        990       0
# 16 primary_link      656       0
# 17 bridleway         556       0
# 18 proposed          346       0
# 19 motorway          247       0
# 20 motorway_link     221       0
# 21 living_street     213       0
# 22 tertiary_link     205       0
# 23 construction      151       0
# 24 secondary_link    108       0
# 25 corridor          100       0
# 26 road               94       0
# 27 raceway            15       0
# 28 elevator           11       0
# 29 cy                  6       0
# 30 escalator           5       0
# 31 stepping_stones     4       0
# 32 disused             2       0
# 33 no                  2       0
# 34 access              1       0
# 35 crossing            1       0

# Calculate lengths of highways by type
gl_osm_lines20$length_km = units::set_units(st_length(gl_osm_lines20$geometry), km) 

# Calculate total lengths by highway type and % of total length
highway_length = gl_osm_lines20 %>%
  st_drop_geometry() %>%
  group_by(highway) %>%
  summarise(total_length_km = sum(length_km)) %>%
  mutate(percent = round(total_length_km/sum(total_length_km)*100)) %>%
  arrange(desc(percent), desc(total_length_km))

# highway         total_length_km percent
# <fct>                      [km]     [1]
# 1 NA                 1.205926e+04      29
# 2 residential        1.045943e+04      26
# 3 footway            4.924905e+03      12
# 4 service            4.374163e+03      11
# 5 tertiary           1.635665e+03       4
# 6 primary            1.408283e+03       3
# 7 unclassified       1.176291e+03       3
# 8 cycleway           1.135560e+03       3
# 9 path               9.843752e+02       2
# 10 trunk              8.719488e+02       2
# 11 secondary          5.402464e+02       1
# 12 track              4.978104e+02       1
# 13 bridleway          2.120583e+02       1
# 14 motorway           1.482049e+02       0
# 15 pedestrian         1.173404e+02       0
# 16 trunk_link         1.081232e+02       0
# 17 steps              7.275999e+01       0
# 18 proposed           4.677139e+01       0
# 19 motorway_link      4.276507e+01       0
# 20 primary_link       3.716434e+01       0
# 21 living_street      1.745012e+01       0
# 22 construction       1.281664e+01       0
# 23 tertiary_link      1.207796e+01       0
# 24 road               1.112641e+01       0
# 25 raceway            5.454501e+00       0
# 26 corridor           4.292478e+00       0
# 27 secondary_link     3.446694e+00       0
# 28 no                 1.945314e-01       0
# 29 elevator           9.662563e-02       0
# 30 disused            9.629581e-02       0
# 31 access             7.199988e-02       0
# 32 cy                 5.567040e-02       0
# 33 escalator          3.177798e-02       0
# 34 stepping_stones    2.314595e-02       0
# 35 crossing           9.492236e-03       0

# Example NA category
highway_NA = gl_osm_lines20 %>%
  filter(is.na(highway)) # 74726
unique(highway_NA$other_tags) # 8096 variations, initial 100- include power lines, railway and underground lines, 
# waterways, paths adjacent to rivers/canals 
# so probably best that NAs are excluded.  

# Generate dataset of highways to keep
# List of highway types to keep
keep = c("primary", "residential", "trunk", "trunk_link", "service",
         "unclassified", "tertiary", "secondary", "tertiary_link", 
         "secondary_link", "primary_link", "living_street", "road") 
potential_road_network = gl_osm_lines20 %>%
  filter(highway %in% keep)
unique(potential_road_network$other_tags)  # first 1000 of 23141 seem to be road related with reference to road names eg A40

#BEFORE RUN BELOW CODE WILL NEED TO SPATIALLY JOIN TO BOROUGH DATA
potential_road_length = potential_road_network %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(total_length_km = sum(length_km)) %>%
  mutate(percent = round(total_length_km/sum(total_length_km)*100)) %>%
  arrange(desc(percent), desc(total_length_km))