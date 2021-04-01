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
# #########################################################
# # Import and wrangle road_link data and limit to London #
# #########################################################
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
 
# Create object with just Borough names and geometry
lon_boroughs = lon_lad_2020 %>%
   select(c("BOROUGH", "geometry"))  

# Split road links by Borough boundaries 
lon_itn_road_link = st_intersection(itn_road_link, lon_boroughs) # n = 290701

# Calculate new road link length (due to spliting length by borough boundaries)
lon_itn_road_link$segment_length = sf::st_length(lon_itn_road_link)
lon_itn_road_link$total_toid_length = lon_itn_road_link$length
sum(lon_itn_road_link$segment_length) #19698080 [m]

#### Validation step - Check total road link length is what expect
# lon_itn_road_link_union = st_intersection(itn_road_link, london_union)
# sum(lon_itn_road_link_union$length) # 19747651
# sum(units::set_units(lon_itn_road_link_union$length, "m")) - sum(lon_itn_road_link$length) # 49571.54 [m] 
# #ie 49 km - this is acceptable in terms of all roads across Greater London

# 3) Data wrangling

# Create list of columns to factor 
columns2factor = c("natureOfRoad", "descriptiveTerm")

#Factor columns
lon_itn_road_link = lon_itn_road_link %>%
   mutate_at(columns2factor, as.factor) %>%
   select(-c("length")) # and drop length column to minimise confusion
 
 
# Save road link data - not on github
saveRDS(lon_itn_road_link, file = "/home/bananafan/Documents/PhD/Datasets/lon_itn19_road_link")


#################################################################
# Create potentially cyclable ITN road links dataset for London #
#################################################################
 
str(lon_itn_road_link) # 290701 obs. of  12 variables

# a) Road descriptive term
lon_itn_road_link %>%
   st_drop_geometry() %>%
   select(descriptiveTerm) %>%
   group_by(descriptiveTerm) %>%
   summarise(count = n())
 
# descriptiveTerm                     count
# * <fct>                               <int>
# 1 A Road                              42501
# 2 Alley                               24690
# 3 B Road                              10683
# 4 Local Street                       145378
# 5 Minor Road                          29401
# 6 Motorway                              420
# 7 Pedestrianised Street                 190
# 8 Private Road - Publicly Accessible   3760
# 9 Private Road - Restricted Access    33678

# b) natureOfRoad
lon_itn_road_link %>%
   st_drop_geometry() %>%
   select(natureOfRoad) %>%
   group_by(natureOfRoad) %>%
   summarise(count = n())
 
# natureOfRoad                     count
# * <fct>                            <int>
# 1 Dual Carriageway                 11391
# 2 Enclosed Traffic Area Link        2215
# 3 Roundabout                        5539
# 4 Single Carriageway              246844
# 5 Slip Road                         2171
# 6 Traffic Island Link               7194
# 7 Traffic Island Link At Junction  15347


# Create potential cyclable road network
itn_lon_potential_cyclable = lon_itn_road_link %>%
   filter(natureOfRoad != "Enclosed Traffic Area Link") %>%  # remove car parks and other ETA (4 local streets, rest form part of private roads)
   filter(!descriptiveTerm %in% c("Motorway","Private Road - Publicly Accessible", 
                                  "Private Road - Restricted Access")) 
# 252839 obs. of  12 variables 

# identify list of road links that have the same fid(TOID)
same_fid = lon_itn_road_link %>%
   st_drop_geometry() %>%
   group_by(fid) %>%
   summarise(count = n()) %>%
   filter(count > 1) # n = 3235 so have 3235 TOIDS have > 1 segment

# Save road link data - not on github
#saveRDS(itn_lon_potential_cyclable, file = "/home/bananafan/Documents/PhD/Datasets/itn_lon_potential_cyclable")


###################################
# Join ITN to MM to get direction #
###################################
# Open ITN london potential cyclable data
#itn_lon_potential_cyclable = readRDS(file = "/home/bananafan/Documents/PhD/Datasets/itn_lon_potential_cyclable")

# # Read in OS Highways Master Map road link layer
# os_HMM_road_links = st_read("/home/bananafan/Downloads/os_highways/MasterMap Highways Network_3984103/Highways_Data_March19.gdb",
#                        layer = "RoadLink")
# 
# os_HMM_road_links = os_HMM_road_links %>%
#    st_drop_geometry() %>%
#    select(c("TOID", "directionality", "roadWidthMinimum")) # n = 287957

# # Save OS HMM road link data for joining - not on github
# saveRDS(os_HMM_road_links, file = "/home/bananafan/Documents/PhD/Datasets/os_HMM_road_links")

# load os_HMM_road_links to join
os_HMM_road_links = readRDS(file = "/home/bananafan/Documents/PhD/Datasets/os_HMM_road_links")

# fiddle with ITN dataset
potential_cyclable_ITN = itn_lon_potential_cyclable %>%
   rename("TOID" = "fid") %>%  # so will be able to join with MM
   select(c("TOID", "BOROUGH", "descriptiveTerm", "natureOfRoad", "segment_length", "total_toid_length",  
            "geometry"))  # n = 252839

# Join datasets by TOID 
joined_potential_cyclable_links = left_join(potential_cyclable_ITN, os_HMM_road_links, by = "TOID") # n = 252839

# THIS IS WHERE I FINISHED ON THURSDAY NEED TO WORK ON THE BELOW TO ENSURE ALL CORRECT
saveRDS(joined_potential_cyclable_links, file = "/home/bananafan/Documents/PhD/Datasets/joined_potential_cyclable_links")


# Ths is more than initially started with ITN so examine further
x = joined_potential_cyclable_links %>%
   st_drop_geometry() %>%
   group_by(TOID) %>%
   summarise(count = n()) %>%
   filter(count > 1) # n = 2945 so have 2945 TOIDS have 

toids = c("osgb4000000030101748", "osgb5000005241400576")
duplicate_toids_eg = joined_potential_cyclable_links %>%
   filter(TOID %in% toids) # represent being split by Borough BUT LENGTH IS THE SAME FOR EACH SEGMENT.  NOT APPROPRIATE

y = joined_potential_cyclable_links %>%
   st_drop_geometry() %>%
   group_by(TOID) %>%
   summarise(totallength = sum(segment_length))



# VAlidation for whole dataset - TO DO ONCE SORT OUT ISSUE RE REPEATING TOIDS
# Check that length in df is correct by utilising st_length
houns_test3$length2 = st_length(houns_test3)   

# check no dual caaragw has both directions
valid_directions = houns_test5 %>%
   st_drop_geometry() %>%
   group_by(natureOfRoad, directionality) %>%
   summarise(count = n())  # this looks ok


# convert road width minimum into numeric 
houns_test3$roadWidthMinimum2 = as.numeric(substr(houns_test3$roadWidthMinimum, 1, nchar(houns_test3$roadWidthMinimum) -1))
units(houns_test3$roadWidthMinimum2) = "m"
units(houns_test3$length) = "m"

# create new column for number of lanes
levels(houns_test3$natureOfRoad)
houns_test4 = houns_test3 %>%
   mutate(lanes = as.numeric(as.character(fct_collapse(natureOfRoad, 
                                                       "2" = "Dual Carriageway",
                                                       "1" = c("Enclosed Traffic Area Link", "Roundabout",
                                                               "Single Carriageway", "Slip Road", 
                                                               "Traffic Island Link", 
                                                               "Traffic Island Link At Junction")))))
# need to use as.numeric(as.character()) as if just use as.numeric then the number represents the factor level not the "2" or "1"

# create new column for number of directions
levels(houns_test4$directionality)
houns_test5 = houns_test4 %>%
   mutate(direction = as.numeric(as.character(fct_collapse(directionality, 
                                                           "2" = "bothDirections",
                                                           "1" = c("inDirection", "inOppositeDirection")))))
# need to use as.numeric(as.character()) as if just use as.numeric then the number represents the factor level not the "2" or "1"

# Identify segments that dont have  direction
houns_no_direction = houns_test5 %>%
   filter(is.na(direction))

#correct their direction
houns_test5$direction[is.na(houns_test5$direction)] = 2 

## Therefore to calculate road availability
# multiple segment length by number of lanes (1 or 2) and by direction (1 or 2)
# so a dual carriageway which is recorded with travel in both directions would multipl segment length by 4 



houns_test5$road_segment_equivalent_length = (houns_test5$length * houns_test5$lanes) * houns_test5$direction
# total road segment equivalent length for city of london 

units::drop_units(houns_test5$road_segment_equivalent_length)
sum(houns_test5$road_segment_equivalent_length)

sum(houns_test5$length)
sum(houns_test5$length2)
sum(hounslow_potential_cyclable_MM$length)
#[1] 72479.96
sum(hounslow_potential_cyclable_ITN$length)
#[1] 67989.73









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





### REpeat on outer london borough

hounslow_potential_cyclable_MM = mm_lon_potential_cyclable %>%
   st_drop_geometry() %>%
   filter(BOROUGH == "Hounslow") %>%
   select(c("TOID", "directionality", "roadWidthMinimum")) # n = 11356

hounslow_potential_cyclable_ITN = itn_lon_potential_cyclable %>%
   filter(BOROUGH == "Hounslow") %>%
   rename("TOID" = "fid") %>%
   select(c("TOID", "descriptiveTerm", "natureOfRoad", "length", "BOROUGH", 
            "geometry"))  # n = 9960 

# Join datasets by TOID 
houns_test3 = left_join(by = "TOID", hounslow_potential_cyclable_ITN, hounslow_potential_cyclable_MM)

# convert road width minimum into numeric 
houns_test3$roadWidthMinimum2 = as.numeric(substr(houns_test3$roadWidthMinimum, 1, nchar(houns_test3$roadWidthMinimum) -1))
units(houns_test3$roadWidthMinimum2) = "m"
units(houns_test3$length) = "m"

# create new column for number of lanes
levels(houns_test3$natureOfRoad)
houns_test4 = houns_test3 %>%
   mutate(lanes = as.numeric(as.character(fct_collapse(natureOfRoad, 
                                                       "2" = "Dual Carriageway",
                                                       "1" = c("Enclosed Traffic Area Link", "Roundabout",
                                                               "Single Carriageway", "Slip Road", 
                                                               "Traffic Island Link", 
                                                               "Traffic Island Link At Junction")))))
# need to use as.numeric(as.character()) as if just use as.numeric then the number represents the factor level not the "2" or "1"

# create new column for number of directions
levels(houns_test4$directionality)
houns_test5 = houns_test4 %>%
   mutate(direction = as.numeric(as.character(fct_collapse(directionality, 
                                                           "2" = "bothDirections",
                                                           "1" = c("inDirection", "inOppositeDirection")))))
# need to use as.numeric(as.character()) as if just use as.numeric then the number represents the factor level not the "2" or "1"

# Identify segments that dont have  direction
houns_no_direction = houns_test5 %>%
   filter(is.na(direction))

#correct their direction
houns_test5$direction[is.na(houns_test5$direction)] = 2 

## Therefore to calculate road availability
# multiple segment length by number of lanes (1 or 2) and by direction (1 or 2)
# so a dual carriageway which is recorded with travel in both directions would multipl segment length by 4 



houns_test5$road_segment_equivalent_length = (houns_test5$length * houns_test5$lanes) * houns_test5$direction
# total road segment equivalent length for city of london 

units::drop_units(houns_test5$road_segment_equivalent_length)
sum(houns_test5$road_segment_equivalent_length)

sum(houns_test5$length)
sum(houns_test5$length2)
sum(hounslow_potential_cyclable_MM$length)
#[1] 72479.96
sum(hounslow_potential_cyclable_ITN$length)
#[1] 67989.73




# VAlidation
# Check that length in df is correct by utilising st_length
houns_test3$length2 = st_length(houns_test3)   




links = c("osgb4000000030081765", "osgb4000000030096169", "osgb4000000030322085",
        "osgb4000000030081765", "osgb5000005191034519", "osgb5000005230509527")

houns_small_test = houns_test3 %>%
   filter(TOID %in% links)

# validation checks
# check no dual caaragw has both directions
valid_directions = houns_test5 %>%
   st_drop_geometry() %>%
   group_by(natureOfRoad, directionality) %>%
   summarise(count = n())  # this looks ok


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
