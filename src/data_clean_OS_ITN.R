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

# Spatially join London road link data to get London Borough names for each segment 
lon_itn_road_link = st_join(lon_itn_road_link, lon_boroughs)
 
 
# 3) Data wrangling

# Create list of columns to factor 
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


   




# CAN WE JOIN ITN TO MM IN ORDER TO THEN HAVE DIRECTION 
mm_lon_potential_cyclable = readRDS(file = "/home/bananafan/Documents/PhD/Datasets/mm_lon_potential_cyclable")
itn_lon_potential_cyclable = readRDS(file = "/home/bananafan/Documents/PhD/Datasets/itn_lon_potential_cyclable")

# names(mm_lon_potential_cyclable)
# [1] "TOID"                           "identifier"                     "identifierVersionId"           
# [4] "beginLifespanVersion"           "fictitious"                     "validFrom"                     
# [7] "reasonForChange"                "roadClassification"             "routeHierarchy"                
# [10] "formOfWay"                      "trunkRoad"                      "primaryRoute"                  
# [13] "roadClassificationNumber"       "roadName1"                      "roadName2"                     
# [16] "roadName1_Language"             "roadName2_Language"             "operationalState"              
# [19] "provenance"                     "directionality"                 "length"                        
# [22] "matchStatus"                    "alternateIdentifier1"           "alternateIdentifier2"          
# [25] "alternateIdentifier3"           "alternateIdentifier4"           "alternateIdentifier5"          
# [28] "startGradeSeparation"           "endGradeSeparation"             "roadStructure"                 
# [31] "cycleFacility"                  "roadWidthMinimum"               "roadWidthAverage"              
# [34] "elevationGainInDirection"       "elevationGainOppositeDirection" "startNode"                     
# [37] "endNode"                        "SHAPE_Length"                   "BOROUGH"                       
# [40] "SHAPE"   

names(itn_lon_potential_cyclable)
# [1] "fid"              "version"          "versionDate"      "theme"            "descriptiveGroup" "descriptiveTerm" 
# [7] "natureOfRoad"     "length"           "changeDate"       "reasonForChange"  "BOROUGH"          "geometry"  

# WOrk on smaller dataset - City of London only

city_potential_cyclable_MM = mm_lon_potential_cyclable %>%
   filter(BOROUGH == "City of London") # n = 1419
city_potential_cyclable_ITN = itn_lon_potential_cyclable %>%
   filter(BOROUGH == "City of London")# n = 1344

m1 = mapview(city_potential_cyclable_ITN)
m2 = mapview(city_potential_cyclable_MM)
leafsync::sync(m1,m2) # can see that there are extra roads in MM e.g. Middle Temple - which is a restricated access road - ? private

# create columns from MM I want
city_mm_want = city_potential_cyclable_MM %>%
   select(c("directionality", "SHAPE"))

#city_test = city_potential_cyclable_ITN[city_potential_cyclable_MM,] ~doesnt work

#city_test2 = st_join(city_potential_cyclable_ITN, city_potential_cyclable_MM["directionality"], join = st_overlaps) doesnt work

city_test3 = st_join(city_potential_cyclable_ITN, city_potential_cyclable_MM["directionality"], join = st_nearest_feature)
# st_nearest feature seems to work



city_Test3_directionality_count = city_test3 %>%
   group_by(directionality) %>%
   summarise(count = n())

print(city_Test3_directionality_count)


# create smaller dataset
keep = c("fid", "descriptiveTerm", "natureOfRoad", "length", "BOROUGH", "directionality", "geometry")
city_test4 = city_test3 %>%
   select(matches(keep))

city_test4$new_segment_length = round(digits = 2, st_length(city_test4))
#check new_segment_length matches length column - YES it does
city_test4$length = units::set_units(city_test4$length, m) # converts length to units metres (currently it is just numeric)

# create new column for number of lanes
levels(city_test4$natureOfRoad)
city_test5 = city_test4 %>%
   mutate(lanes = as.numeric(as.character(fct_collapse(natureOfRoad, 
                         "2" = "Dual Carriageway",
                         "1" = c("Enclosed Traffic Area Link", "Roundabout",
                                 "Single Carriageway", "Slip Road", 
                                 "Traffic Island Link", 
                                 "Traffic Island Link At Junction")))))
# need to use as.numeric(as.character()) as if just use as.numeric then the number represents the factor level not the "2" or "1"

# create new column for number of directions
levels(city_test4$directionality)
city_test6 = city_test5 %>%
   mutate(direction = as.numeric(as.character(fct_collapse(directionality, 
                                                       "2" = "bothDirections",
                                                       "1" = c("inDirection", "inOppositeDirection")))))
# need to use as.numeric(as.character()) as if just use as.numeric then the number represents the factor level not the "2" or "1"

## Therefore to calculate road availability
# multiple segment length by number of lanes (1 or 2) and by direction (1 or 2)
# so a dual carriageway which is recorded with travel in both directions would multipl segment length by 4 

city_test6$road_segment_equivalent_length = (city_test6$length * city_test6$lanes) * city_test6$direction
# total road segment equivalent length for city of london 
sum(city_test6$road_segment_equivalent_length) # =118713.5 [m]
 
sum(city_potential_cyclable_MM$length)
#[1] 72479.96
sum(city_potential_cyclable_ITN$length)
#[1] 67989.73
















###########################
# Work on smaller dataset
############################


# city_private_roads = lon_private_roads_itn %>%
#    filter(BOROUGH == "City of London") # n = 88
# 
# mapview(city_private_roads)

city_potential_cyclable_MM = mm_lon_potential_cyclable %>%
   filter(BOROUGH == "City of London") # n = 1419
# this has dropped motorways, ETAs and roads under construction
total_length_city_MM = sum(city_potential_cyclable_MM$length)

city_potential_cyclable_ITN = itn_lon_potential_cyclable %>%
   filter(BOROUGH == "City of London")# n = 1344
# this has dropped motorways, ETAs and private roads
total_length_city_ITN = sum(city_potential_cyclable_ITN$length)
y = city_potential_cyclable_MM %>%
   st_drop_geometry() %>%
   select(directionality) %>%
   group_by(directionality) %>%
   summarise(count = n())

m1 = mapview(city_potential_cyclable_MM) 
m2 = mapview(city_potential_cyclable_ITN$geometry, color = "red")
leafsync::sync(m1,m2)
city_potential_cyclable = st_join(city_potential_cyclable_ITN, city_potential_cyclable_MM, join = st_equals)
total_length_city_joined = sum(city_potential_cyclable$length.x)
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
