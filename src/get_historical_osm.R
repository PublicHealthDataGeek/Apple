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

# install.packages
remotes::install_github("ITSLeeds/osmextract")

library(osmextract)
library(sf)

# load OSM dataset
gl_pbf = "/home/bananafan/Documents/PhD/Paper1/data/greater-london-200101.osm.pbf"
gl_pbf = data.file("/data/greater-london-200101.osm.pbf", package = "osmextract")

# Get different osm layers
gl_osm_lines = oe_read(gl_pbf, quiet = FALSE) # 255244 features and 10 fields, linestring, crs= WGS84
gl_osm_points = oe_read(gl_pbf, quiet = FALSE, layer = "points") # 328851 obs, 11 variables, point, crs= WGS84 
gl_osm_mls = oe_read(gl_pbf, quiet = FALSE, layer = "multilinestrings") # 4413 features, 4 fields, multilinestring, crs= WGS84 
gl_osm_multipolygons = oe_read(gl_pbf, quiet = FALSE, layer = "multipolygons") # 546748 features, 25 fields, multipolygon, crs= WGS84 


# examine gl_osm_lines dataset
str(gl_osm_lines) # 255244 obs, 10 variables, all characters except 'z_order'
unique(gl_osm_lines$highway) # 35 unique names inc NA
unique(gl_osm_lines$waterway) # 18 unique names inc NA - ? any relevance? 
unique(gl_osm_lines$aerialway) #  4 unique names inc NA - dont look to be relevant
unique(gl_osm_lines$barrier) # 36 unique names inc NA
unique(gl_osm_lines$man_made) # 45 unique names in NA - dont look to be relevant
unique(gl_osm_lines$z_order) # integer, ranges from -50 to +160
unique(gl_osm_lines$other_tags) # massive amount of data!!!!!! 44340 entries!

# examine gl_osm_points dataset
str(gl_osm_points) # 328851 obs, 11 variables, all characters
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











# CAn add specific tags (e.g. 'max speed') which adds it as an extra column


test = oe_read(gl_pbf, quiet = FALSE, extra_tags = c("maxspeed"))
