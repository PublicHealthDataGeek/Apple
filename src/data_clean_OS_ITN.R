###############################################################################
# Obtaining data from OS INtegrated Transport Network Layer 2019              #          
#                                                                             #
# This code imports the relevent layers from the ITN  dataset.                #
# The data is already limited to a box around London but is subsequently      #
# limited to London boundaries using using ONS dataset.                       #
#                                                                             #
# A dataset of potential cycling road network excluding motorways, enclosed   #
# traffic areas and private roads was created 288450 obs                      #
# Also created a dataset of private roads for STATS19 analysis                #
#                                                                             #
################################################################################

library(tidyverse)
library(sf)
library(mapview)

# set mapview options so that matches crs
mapviewOptions(native.crs = TRUE, legend = FALSE)


# Obtain list of layers within ITN
st_layers("/home/bananafan/Downloads/os_ITN/mastermap-itn_3994768/mastermap-itn_3994768_0.gml")
# Driver: GML 
# Available layers:
#   layer_name geometry_type features fields
# 1             FerryLink            NA        1      7
# 2             FerryNode         Point        2      7
# 3         FerryTerminal            NA        2      8
# 4              RoadLink   Line String   415267     10
# 5              RoadNode         Point   337100      7
# 6      InformationPoint         Point       50      8
# 7   RoadLinkInformation                  80329     12
# 8   RoadNodeInformation            NA     2361     11
# 9  RoadRouteInformation                 113102     20
# 10                 Road            NA    73684      9
# 
# 
# 
# #####################################
# # Import and wrangle road_link data #
# #####################################
# 
# 
# # 1) Read in road link layer
itn_road_link = st_read("/home/bananafan/Downloads/os_ITN/mastermap-itn_3994768/mastermap-itn_3994768_0.gml",
                        layer = "RoadLink")
# Simple feature collection with 415267 features and 10 fields
# geometry type:  LINESTRING
# dimension:      XY
# bbox:           xmin: 495915.7 ymin: 148014 xmax: 566682 ymax: 204868
# projected CRS:  OSGB 1936 / British National Grid
# 
# # Identify variables
names(itn_road_link)
# [1] "fid"              "version"         
# [3] "versionDate"      "theme"           
# [5] "descriptiveGroup" "descriptiveTerm" 
# [7] "natureOfRoad"     "length"          
# [9] "changeDate"       "reasonForChange" 
# [11] "geometry" 

unique(itn_road_link$natureOfRoad)
# [1] "Roundabout"                     
# [2] "Single Carriageway"             
# [3] "Dual Carriageway"               
# [4] "Traffic Island Link"            
# [5] "Traffic Island Link At Junction"
# [6] "Slip Road"                      
# [7] "Enclosed Traffic Area Link"    

unique(itn_road_link$descriptiveTerm)
# [1] "A Road"                            
# [2] "Local Street"                      
# [3] "Private Road - Restricted Access"  
# [4] "Minor Road"                        
# [5] "Private Road - Publicly Accessible"
# [6] "B Road"                            
# [7] "Alley"                             
# [8] "Motorway"                          
# [9] "Pedestrianised Street" 


# 2) Limit Road Link data to London Boroughs only
 
# import May 2020 ONS LA boundary data
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")
 
# Create spatial object for all 33 boroughs
london_union = st_union(lon_lad_2020) 

# Limit road links to within the Outer London Boundary
lon_itn_road_link = st_intersection(itn_road_link, london_union)
 
# # Create object with just Borough names and geometry
lon_boroughs = lon_lad_2020 %>%
   select(c("BOROUGH", "geometry"))  

# Spatially join London road link data to London Borough names 
lon_itn_road_link = st_join(lon_itn_road_link, lon_boroughs)
 
 
# #3) Data wrangling

# Create list of columnd to factor 
columns2factor = c("natureOfRoad", "descriptiveTerm")

#Factor columns
lon_itn_road_link = lon_itn_road_link %>%
   mutate_at(columns2factor, as.factor)
 
 
# Save road link data - not on github
# saveRDS(lon_itn_road_link, file = "/home/bananafan/Documents/PhD/Datasets/lon_itn19_road_link")

# #####################################################
# # Examine London road dataset  variables and values #
# #####################################################
 
str(lon_itn_road_link) # 290664 obs. of  12 variables

# a) Road descriptive term
lon_road_desc_count = lon_itn_road_link %>%
   st_drop_geometry() %>%
   select(descriptiveTerm) %>%
   group_by(descriptiveTerm) %>%
   summarise(count = n())
 
# descriptiveTerm                     count
# * <fct>                               <int>
# 1 A Road                              42486
# 2 Alley                               24690
# 3 B Road                              10683
# 4 Local Street                       145365
# 5 Minor Road                          29395
# 6 Motorway                              420
# 7 Pedestrianised Street                 190
# 8 Private Road - Publicly Accessible   3757
# 9 Private Road - Restricted Access    33678

# b) natureOfRoad
lon_natureOfRoad_count = lon_itn_road_link %>%
   st_drop_geometry() %>%
   select(natureOfRoad) %>%
   group_by(natureOfRoad) %>%
   summarise(count = n())
 
# natureOfRoad                     count
# * <fct>                            <int>
# 1 Dual Carriageway                 11384
# 2 Enclosed Traffic Area Link        2214
# 3 Roundabout                        5539
# 4 Single Carriageway              246817
# 5 Slip Road                         2171
# 6 Traffic Island Link               7194
# 7 Traffic Island Link At Junction  15345


# Create potential cyclable road network
itn_lon_potential_cyclable = lon_itn_road_link %>%
   filter(natureOfRoad != "Enclosed Traffic Area Link") %>%  # remove car parks and other ETA (4 local streets, rest form part of private roads)
   filter(!descriptiveTerm %in% c("Motorway","Private Road - Publicly Accessible", "Private Road - Restricted Access")) 
# 252805 obs. of  12 variables (drops 37855 via !descriptionTerm and a further 4 nonprivate roads that were ETA)

# Save road link data - not on github
# saveRDS(itn_lon_potential_cyclable, file = "/home/bananafan/Documents/PhD/Datasets/itn_lon_potential_cyclable")


   




###########################
# Work on smaller dataset
############################

mm_lon_potential_cyclable = readRDS(file = file = "/home/bananafan/Documents/PhD/Datasets/mm_lon_potential_cyclable")
itn_lon_potential_cyclable = readRDS(file = "/home/bananafan/Documents/PhD/Datasets/itn_lon_potential_cyclable")
# city_private_roads = lon_private_roads_itn %>%
#    filter(BOROUGH == "City of London") # n = 88
# 
# mapview(city_private_roads)

city_potential_cyclable_MM = mm_lon_potential_cyclable %>%
   filter(BOROUGH == "City of London") # n = 1419
# this has dropped motorways, ETAs and roads under construction


city_potential_cyclable_ITN = itn_lon_potential_cyclable %>%
   filter(BOROUGH == "City of London")# n = 1344
# this has dropped motorways, ETAs and private roads

y = city_potential_cyclable_MM %>%
   st_drop_geometry() %>%
   select(directionality) %>%
   group_by(directionality) %>%
   summarise(count = n())

m1 = mapview(city_potential_cyclable_MM) 
m2 = mapview(city_potential_cyclable_ITN$geometry, color = "red")
leafsync::sync(m1,m2)
city_potential_cyclable = st_join(city_potential_cyclable_ITN, city_potential_cyclable_MM, join = st_overlaps)
# 1344 obs. of  51 variables:


### BUT directionallity has been dropped -which versin of join should I use???
city_directionality_count = city_potential_cyclable %>%
   st_drop_geometry() %>%
   select(directionality) %>%
   group_by(directionality) %>%
   summarise(count = n())

####################################################
# Create dataset of private roads for STATS19 work #
####################################################
lon_private_roads_itn = lon_itn_road_link %>%
   filter(descriptiveTerm == "Private Road - Publicly Accessible" | 
             descriptiveTerm == "Private Road - Restricted Access") 
# Save road link data - not on github
# saveRDS(lon_private_roads_itn, file = "/home/bananafan/Documents/PhD/Datasets/lon_private_roads_itn")
