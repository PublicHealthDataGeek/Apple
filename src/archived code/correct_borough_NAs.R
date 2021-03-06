####################################################
# Address assests that have no Borough stated      #
#     
# This script takes the data cleansed CID files
# and then addresses the NAs in the Borough by:
# 
# for ASL -
# for crossings
# for cycle lanes and tracks
# for ????
# for ????

###################################################

# load librarys
# install packages
library(tidyverse)
library(CycleInfraLnd)
library(sf)

library(mapview)
library(leaflet)
library(leafem)
#library(leafsync)
#library(summarytools)
library(forcats)
#library(geojsonsf)

# Run data_clean_CID code
#source("/home/bananafan/Documents/PhD/Paper1/src/data_clean_CID.R")

###############################################################################
# ASL                                                                         #
###############################################################################
# Summary:
#
# 1 ASL has no borough 
# used visual check to see where it starts i.e. this is where the traffic lights 
# must be (greenwich) and finishes (lewisham)
# used st_intersection to split ASL by Borough
# can see that split bit is longer in Greenwich and
# calculated length (longer in Greenwich)
# so coded the NA as Greenwich
################################################################################

# download CID data using the Cycle Infra Lnd package
advanced_stop_line = get_cid_lines(type = "advanced_stop_line")

# check and convert CRS so matches ONS boundary data CRS
st_crs(advanced_stop_line) # CRS = WGS 84
advanced_stop_line = st_transform(advanced_stop_line, crs=27700) 
st_crs(advanced_stop_line) # PROJCRS["OSGB 1936 / British National Grid",

# convert character columns to factors
f_variables = c("ASL_FDR", "ASL_FDRLFT", "ASL_FDCENT", "ASL_FDRIGH", 
                "ASL_SHARED", "ASL_COLOUR")

# convert columns to factors (CLT_ACCESS not converted as 721 different values)
f_advanced_stop_line = advanced_stop_line %>%
  mutate_at(f_variables, as.factor)

# identify missing Borough details
asl_borough_NA = f_advanced_stop_line %>%
  filter(is.na(BOROUGH)) # 1 observation ie 1 ASL has no Borough

# CALCULATE LENGTH OF ASL 

asl_borough_NA$total_asl_length = st_length(asl_borough_NA)

# import May 2020 ONS LA boundary data
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

# Map ASL observation that does not have a Borough with Borough boundaries
mapview(asl_borough_NA$geometry, color = "red") + mapview(lon_lad_2020, alpha.regions = 0.05, zcol = "BOROUGH") 
# Visual inspection shows that this ASL starts in Greenwich & finishes in Lewisham (direction of travel is towards Greenwich)
# so let's code it as Greenwich 

# code written using st_intersection to produce two objects of the ASL, one composing of the LEwisham section and the other the Greenwich
asl_na_i = st_intersection(lon_lad_2020, asl_borough_NA)
names(asl_na_i)
asl_na_i$borough_length = st_length(asl_na_i)
mapview(asl_na_i, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.05, zcol = "BOROUGH")
#can visualise that the Greenwich section is longer than lewisham on map and dataset shows that
# the greenwich section is 2.9m whilst the lewisham is 2.1m 

borough_asl_na_length = asl_na_i %>% 
  group_by(FEATURE_ID) %>%
  slice(which.max(borough_length)) # this code keeps the observations for each feature with the longest length
#this code gives me the Borough name that has the longest bit of ASL = greenwich


##############
# recode ASL #
##############

# Count number of ASLs by Borough
count_asl_borough = f_advanced_stop_line %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Count = n()) # 112 Greenwich, 1 NA

# Recode this NA as Greenwich and factor the BOROUGH variable
f_advanced_stop_line$BOROUGH = factor(f_advanced_stop_line$BOROUGH) %>%
  fct_explicit_na(na_level = "Greenwich")
anyNA(f_advanced_stop_line$BOROUGH) # = FALSE so no NAs

# Recount to check coded properly
recount_asl_borough = f_advanced_stop_line %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(recount = n()) # 113 Greenwich, no NA

# Combine count/recount tables so can check recoding
asl_na_coding_check = left_join(count_asl_borough, recount_asl_borough) # CORRECT


###############################################################################
# Crossing                                                                    #
###############################################################################
# Summary:
# 28 crossings have no borough

################################################################################

# download crossings CID data and transform CRS to match ONS boundary data
crossings = get_cid_lines(type = "crossing")
crossings = st_transform(crossings, crs=27700) 

# convert certain columns to factors
f_v = c("CRS_SIGNAL", "CRS_SEGREG", "CRS_CYGAP", "CRS_PEDEST", "CRS_LEVEL")

# convert columns to factors (BOROUGH needs doing separately as it has some NAs in, CLT_ACCESS not converted as 721 different values)
f_crossings = crossings %>%
  mutate_at(f_v, as.factor)
anyNA(f_crossings$BOROUGH) # = TRUE

# identify missing Borough details
crossings_borough_NA = f_crossings %>%
  filter(is.na(BOROUGH)) # 28 observations ie 28 crossings have no Borough

# import May 2020 ONS LA boundary data
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

#create map of Crossings with missing Boroughs and label them with the feature ID
mNA = mapview(crossings_borough_NA, color = "red") + mapview(lon_lad_2020, alpha.regions = 0.05, zcol = "BOROUGH")
leafem::addStaticLabels(mNA, label = crossings_borough_NA$FEATURE_ID) # plot mNA with the FEATURE_IDs labelled so can identify

# Examination performed - visual inspection of asset photos 1 and 2, Open Street Map
# Decisions made according to......
# Decision added to excel spreadsheet, where a crossing is in:
##### one borough only then it is given that borough e.g RWG236647 # n= 7
##### two boroughs but the road (or the majority of the road) it is crossing is in one borough (ie where the crossing is, is on a road that is in one borough) 
#       it is coded as that borough e.g. RWG273946 # n = 16
##### two boroughs but the boundary between the boroughs goes through the centre of the road 
#       and that crossing then it is coded as ? both eg RWG293100  # n = 5
# excel spreadsheet imported and then processed below....


# RWG065992 and RWG150947 - seem to be duplicates - ? need to check definition of what is measured by a crossing



# USe st_intersection to produce two observations for the crossings - split by geographical borough boundaries
crossings_borough_NA_i = st_intersection(lon_lad_2020,crossings_borough_NA) # 49 observations, geometry column is from the crossings dataset
names(crossings_borough_NA_i) # BOROUGH is the column for the lon_lad_2020 Borough that the crossing is in, whereas BOROUGH.1 
#is the BOROUGH from the crossing dataset (all NAs)
mapview(crossings_borough_NA_i, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.05, zcol = "BOROUGH")
#this map shows the crossings coloured by the BOROUGH they are in

crossings_borough_NA_i$borough_length = st_length(crossings_borough_NA_i) # measures length of each crossing (using crossing geometry)by BOROUGH
crossings_borough_NA_length = crossings_borough_NA_i %>% 
  group_by(FEATURE_ID) %>%
  slice(which.max(borough_length)) # keep observations for each feature with the longest length

# compare borough_NA_length with my visual check => only 2 observations where BOROUGH doesnt match my proposed Borough
# RWG153061 city of london Type 3
# RWG049417 Hammersmith & fulham type 3
# type 3 =  two boroughs but the boundary between the boroughs goes through the centre of the road 
# d/w RB on 7/1/21 - to create two observations (duplicates) for these feature_Ids and assign one to each of the boroughs
# so create:
# RWG153061 city of london Type 3
# RWG049417 Hammersmith & fulham type 3

# Create final dataset that gives the propose Borough for each observation based on the above rules 
# create congruent dataset where visual check and st_length check matches
congruent = crossings_borough_NA_i %>%
  filter(!FEATURE_ID %in% c("RWG153061", "RWG049417"))

congruent = congruent %>% 
  group_by(FEATURE_ID) %>%
  slice(which.max(borough_length)) # keep the observation (borough) which has the longest length

# create incongruent dataset where visual check and st_length check does not match
# ie observations "RWG153061", "RWG049417" 
# recode Feature_ID with 'a' and 'b'
# crosschecked that a/b corresponds with the right borough
incongruent = crossings_borough_NA_i %>%
  filter(FEATURE_ID %in% c("RWG153061", "RWG049417")) %>%
  group_by(FEATURE_ID) %>%
  arrange(desc(borough_length)) %>%
  ungroup()
incongruent$FEATURE_ID= replace(incongruent$FEATURE_ID, which(incongruent$FEATURE_ID == "RWG153061"), values = c("RWG153061a", "RWG153061b"))
incongruent$FEATURE_ID= replace(incongruent$FEATURE_ID, which(incongruent$FEATURE_ID == "RWG049417"), values = c("RWG049417a", "RWG049417b"))

# Create df of all the observations that were Borough NA with correct Boroughs
# and convert to MLS so has same geometry type as crossings
crossings_borough_NA_corrected = rbind(congruent, incongruent) %>%
   select(c("FEATURE_ID","SVDATE","CRS_SIGNAL", "CRS_SEGREG", "CRS_CYGAP", 
            "CRS_PEDEST", "CRS_LEVEL", "BOROUGH", "PHOTO1_URL", "PHOTO2_URL",
            "geometry")) 


# VAlidating that have corrected NAs
#count Boroughs before transformation
count_crossings_borough = f_crossings %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Count = n()) 

# drop obs with no boroughs from f_crossings
f_crossings = f_crossings %>%
  filter(!is.na(BOROUGH)) # 1659 observations ie  the 1687 - 28 NAs
anyNA(f_crossings$BOROUGH) # = FALSE ie all dropped

# cast geometry
crossings_borough_NA_corrected = crossings_borough_NA_corrected %>%
  st_cast("MULTILINESTRING") %>%
  mutate(BOROUGH = as.character(BOROUGH))

# join corrected observations to the f_crossings
f_crossings = rbind(f_crossings, crossings_borough_NA_corrected) 
anyNA(f_crossings$BOROUGH) # = FALSE

# recount Boroughs after transformation
recount_crossings_borough = f_crossings %>%  #####EWILL NEED TO CHANGE THIS TO A DIFFERENT NAME (POST CHANGE)
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Recount = n()) # 

number_recoded = crossings_borough_NA_corrected %>%  #####EWILL NEED TO CHANGE THIS TO A DIFFERENT NAME (POST CHANGE)
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(No_recoded = n())

# join borough counts together to check done correctly
crossing_boroughs = left_join(count_crossings_borough, number_recoded) %>%
  left_join(recount_crossings_borough) # This looks to be correct

# Making map of crossings coloured by Borough
mapview(f_crossings, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, zcol = "BOROUGH", legend =FALSE)


###############################################################################
# Cycle lanes and tracks
#
# Summary:
# 354 cycle lanes/tracks have no Borough
# visual inspections shows that some go over multiple boroughs, outside
# london boroughs or over bridges that cross the Thames
#

# download CID cycle lane track data and correct CRS
cycle_lane_track = get_cid_lines(type = "cycle_lane_track") # n = 24976
cycle_lane_track = st_transform(cycle_lane_track, crs=27700) 

# convert certain columns to factors (CLT_ACCESS and BOROUGH not done)
f_variables = c("CLT_CARR", "CLT_SEGREG", "CLT_STEPP", "CLT_PARSEG", "CLT_SHARED", "CLT_MANDAT", 
                "CLT_ADVIS", "CLT_PRIORI", "CLT_CONTRA", "CLT_BIDIRE", "CLT_CBYPAS", "CLT_BBYPAS",
                "CLT_PARKR", "CLT_WATERR", "CLT_PTIME", "CLT_COLOUR")

# convert columns to factors (BOROUGH needs doing separately as it has some NAs in, CLT_ACCESS not converted as 721 different values)
f_cycle_lane_track = cycle_lane_track %>%
  mutate_at(f_variables, as.factor)

# correct TRE to TRUE in CLT_PRIORI
f_cycle_lane_track$CLT_PRIORI = fct_recode(f_cycle_lane_track$CLT_PRIORI, 
                                           "TRUE" = "TRE") # convert TRE to TRUE


#INITIAL WORK ON CYCLE LANES?TRACKS re BOROUGH NA
###################################################
anyNA(f_cycle_lane_track$BOROUGH) # = TRUE

# identify missing Borough details
cycle_lane_borough_NA = f_cycle_lane_track %>%
  filter(is.na(BOROUGH)) # 354 observations ie 354 cycle lanes/tracks have no Borough

# import May 2020 ONS LA boundary data
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

#create map of cycle_lane/tracks  with missing Boroughs and label them with the feature ID
mNA = mapview(cycle_lane_borough_NA, color = "red") + mapview(lon_lad_2020, alpha.regions = 0.05, zcol = "BOROUGH")
leafem::addStaticLabels(mNA, label = cycle_lane_borough_NA$FEATURE_ID) # plot mNA with the FEATURE_IDs labelled so can identify
#looks like similar issue to ASL - the cycle lanes and tracks have been measured end to end but cross a borough boundary
# some cross multiple boundaries, some cross into non greater london areas, ones that include bridges over river seem to be coded NA
# some only just cross - others are 50/50

# What is the total length affected by NAs?
f_cycle_lane_track$length = st_length(f_cycle_lane_track$geometry)
length_cycle_lanesBYborough = f_cycle_lane_track %>%
  group_by(BOROUGH) %>%
  summarise(length = sum(length)) 
length_cycle_lanesBYborough = st_drop_geometry(length_cycle_lanesBYborough)
# total length of those observations without a Borough is 197374.84 [m]
sum(length_cycle_lanesBYborough$length) # total length of all cycle lanes = 2905876m
p = 197374.84/sum(length_cycle_lanesBYborough$length)  *100
# p = 6.79% so although only 1.4% of assets dont have a Borough, these observations account for nearly 7% of the total length 
# of cycle lanes and tracks so probably worth sorting out. 



# Try to sort out using st_intersection 
lanes_borough_NA_i = st_intersection(lon_lad_2020, cycle_lane_borough_NA) # 621 observations, geometry column is from the lanes dataset
# this has dropped cycle lanes sections that are outside london
# this has broken each unique FEATURE_ID into segments based on whether they cross a borough line NB each segment may contain a MLS
# but contains multiple geometry types
summary(lanes_borough_NA_i$geometry) # -> 519 linestrings and 102 multilinestrings
lanes_borough_NA_i = lanes_borough_NA_i %>%
  st_cast("MULTILINESTRING") # convert geometry type to MLS for all so can mapview
summary(lanes_borough_NA_i$geometry) # -> 621 MLS.  Now can mapview(lanes_borough_NA_i)

mapview(lanes_borough_NA_i, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, zcol = "BOROUGH", legend = FALSE)

# count number of observations for each FEATURE_ID
count_obs = lanes_borough_NA_i %>%
  st_drop_geometry() %>%
  group_by(FEATURE_ID) %>%
  summarise(num_obs = n()) %>%
  group_by(num_obs) %>%
  count()
# num_obs   n
#     1    88   # 88 feature_ids have 1 observation  NB observation may contain multiple lines but all have same characteristics
#     2   265
#     3     1
# so 88 are unique so ok but the ones with 2 and 3 need recoding with separate FEATURE_IDs

# Group 621 by FEATURE_ID then add a cumulative sum to each grouped observation (will be 1, 2 or 3)
lanes_borough_NA_corrected = lanes_borough_NA_i %>%
  mutate(n = 1) %>%
  group_by(FEATURE_ID) %>%
  mutate(cum_count = cumsum(n)) %>%
  select(-n)

# relabel the FEATURE_ID with the cumulative number so each observation has a unique ID
lanes_borough_NA_corrected$FEATURE_ID = paste(lanes_borough_NA_corrected$FEATURE_ID, "_", lanes_borough_NA_corrected$cum_count)

# Check that the correct number have 1, 2 or 3 appended to the FEATURE_ID
lanes_borough_NA_corrected %>%
  st_drop_geometry() %>%
  group_by(cum_count) %>%
  count()
# cum_count   n
#       1   354  (= 1 + 265 + 88)
#       2   266  (= 1+ 265)
#       3     1

# Create df of all the observations that were Borough NA with correct Boroughs
lanes_borough_NA_corrected = lanes_borough_NA_corrected %>%
  select(c("FEATURE_ID", "SVDATE", "CLT_CARR", "CLT_SEGREG", "CLT_STEPP", 
           "CLT_PARSEG", "CLT_SHARED", "CLT_MANDAT", "CLT_ADVIS",  "CLT_PRIORI",
           "CLT_CONTRA", "CLT_BIDIRE", "CLT_CBYPAS", "CLT_BBYPAS", "CLT_PARKR", 
           "CLT_WATERR", "CLT_PTIME",  "CLT_ACCESS", "CLT_COLOUR", "BOROUGH", 
           "PHOTO1_URL", "PHOTO2_URL", "geometry")) %>%
  mutate(BOROUGH = as.character(BOROUGH)) # change borough to character so can match back to f_cycle_lane_track
# NB geometry is already Multiline string so no need to convert


# VAlidating that have corrected NAs
#count Boroughs before transformation
count_lanes_borough = f_cycle_lane_track %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Count = n()) 

# drop obs with no boroughs from f_cycle_lane_track
f_cycle_lane_track = f_cycle_lane_track %>%
  filter(!is.na(BOROUGH)) # 24622 observations ie  the 24976 - 354 NAs
anyNA(f_cycle_lane_track$BOROUGH) # = FALSE ie all dropped


# join corrected observations to the f_cycle_lane_track
f_cycle_lane_track = rbind(f_cycle_lane_track, lanes_borough_NA_corrected) 
anyNA(f_cycle_lane_track$BOROUGH) # = FALSE

# recount Boroughs after transformation
recount_lanes_borough = f_cycle_lane_track %>%  
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Recount = n()) 

number_recoded = lanes_borough_NA_corrected %>%  
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Number_recoded = n())

# join borough counts together to check done correctly
lanes_boroughs = left_join(count_lanes_borough, number_recoded) %>%
  left_join(recount_lanes_borough) # This looks to be correct

# Making map of crossings coloured by Borough
mapview(f_crossings, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, zcol = "BOROUGH", legend =FALSE)






#####################
# restricted routes 
#####################
#
# Summary:
# 18 restricted routes dont have a borough
# visual inspection shows that some cross borough or multiple borough boundaries and others cross the Thames
#
#

# download restricted routes and transform crs
restricted_route = get_cid_lines(type = "restricted_route")
restricted_route = st_transform(restricted_route, crs=27700) 

f_variables = c("RES_PEDEST", "RES_BRIDGE", "RES_TUNNEL", "RES_STEPS", "RES_LIFT")

# convert columns to factors (BOROUGH needs doing separately as it has some NAs in, CLT_ACCESS not converted as 721 different values)
f_restricted_route = restricted_route %>%
  mutate_at(f_variables, as.factor)

anyNA(f_restricted_route$BOROUGH) # = TRUE

# identify missing Borough details
restricted_route_borough_NA = f_restricted_route %>%
  filter(is.na(BOROUGH)) # 18 observations ie 18 restricted routes no Borough

# import May 2020 ONS LA boundary data
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

#create map of restricted routes  with missing Boroughs and label them with the feature ID
mNA = mapview(restricted_route_borough_NA, color = "red") + mapview(lon_lad_2020, alpha.regions = 0.05, zcol = "BOROUGH")
leafem::addStaticLabels(mNA, label = restricted_route_borough_NA$FEATURE_ID) # plot mNA with the FEATURE_IDs labelled so can identify
# some cross multiple boundaries, some (n =2)  cross over include bridges over river seem to be coded NA

# RWF109094 is Greenwich foot tunnel
# RWG109327 is Millenium Bridge

# What is the total length affected by NAs?
f_restricted_route$length = st_length(f_restricted_route$geometry)
length_restricted_routesBYborough = f_restricted_route %>%
  group_by(BOROUGH) %>%
  summarise(length = sum(length)) 
length_restricted_routesBYborough = st_drop_geometry(length_restricted_routesBYborough)
sum(length_restricted_routesBYborough$length) # =  307067.8 [m]
# total length of those observations without a Borough is 25209.39032 [m]
sum(length_restricted_routesBYborough$length) # total length of all RR = 307067.8 [m]
per = 25209.39032/sum(length_restricted_routesBYborough$length)  *100
# p = 8.2% so although only 1.3% of assets dont have a Borough, these observations account for 8% of the total length 
# of restricted routes so probably worth sorting out. 

# Try to sort out using st_intersection 
restricted_route_borough_NA_i = st_intersection(lon_lad_2020, restricted_route_borough_NA) # 36 observations, 
#geometry column is from the lanes dataset
# this has broken each unique FEATURE_ID into segments based on whether they cross a borough line NB each segment may contain a MLS
# but contains multiple geometry types
summary(restricted_route_borough_NA_i$geometry) # -> 27 linestrings and 9 multilinestrings
restricted_route_borough_NA_i = restricted_route_borough_NA_i %>%
  st_cast("MULTILINESTRING") # convert geometry type to MLS for all so can mapview
summary(restricted_route_borough_NA_i$geometry) # -> 36 MLS.  Now can mapview(lanes_borough_NA_i)

mapview(restricted_route_borough_NA_i, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, zcol = "BOROUGH", legend = FALSE)

# count number of observations for each FEATURE_ID
count_obs = restricted_route_borough_NA_i %>%
  st_drop_geometry() %>%
  group_by(FEATURE_ID) %>%
  summarise(num_obs = n()) %>%
  group_by(num_obs) %>%
  count()
#   num_obs     n
#1       1      1 # 1 FEATURE_ID has one observation
#2       2     16 # 16 FEATURE_IDs have two observations
#3       3      1  #  1 FEATURE_ID has 3 observations


# Group 36 by FEATURE_ID then add a cumulative sum to each grouped observation (will be 1, 2 or 3)
restricted_route_NA_corrected = restricted_route_borough_NA_i %>%
  mutate(n = 1) %>%
  group_by(FEATURE_ID) %>%
  mutate(cum_count = cumsum(n)) %>%
  select(-n)

# relabel the FEATURE_ID with the cumulative number so each observation has a unique ID
restricted_route_NA_corrected$FEATURE_ID = paste(restricted_route_NA_corrected$FEATURE_ID, "_", restricted_route_NA_corrected$cum_count)

# Check that the correct number have 1, 2 or 3 appended to the FEATURE_ID
restricted_route_NA_corrected %>%
  st_drop_geometry() %>%
  group_by(cum_count) %>%
  count()

# cum_count     n   # This looks to be correct
#         1    18  = 1 + 16 + 1
#         2    17  = 1 + 16
#         3     1

# Create df of all the observations that were Borough NA with correct Boroughs
restricted_route_NA_corrected = restricted_route_NA_corrected %>%
  select(c("FEATURE_ID", "SVDATE", "RES_PEDEST", "RES_BRIDGE", "RES_TUNNEL", 
           "RES_STEPS", "RES_LIFT", "BOROUGH", "PHOTO1_URL", "PHOTO2_URL", 
           "geometry", )) %>%
  mutate(BOROUGH = as.character(BOROUGH)) %>% 
  mutate(length = st_length(geometry))
  # change borough to character and add length column so can match back to f_restricted_route
# NB geometry is already Multiline string so no need to convert

# VAlidating that have corrected NAs
#count Boroughs before transformation
count_restricted_route_borough = f_restricted_route %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Count = n()) 

# drop obs with no boroughs from f_restricted_route
f_restricted_route = f_restricted_route %>%
  filter(!is.na(BOROUGH)) # 1360 observations ie  the 1378 - 18 NAs
anyNA(f_restricted_route$BOROUGH) # = FALSE ie all dropped


# join corrected observations to the f_restricted_route
f_restricted_route = rbind(f_restricted_route, restricted_route_NA_corrected) 
anyNA(f_restricted_route$BOROUGH) # = FALSE

# recount Boroughs after transformation
recount_restricted_route_borough = f_restricted_route %>%  
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Recount = n()) 

number_recoded = restricted_route_NA_corrected %>%  
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Number_recoded = n())

# join borough counts together to check done correctly
restricted_route_boroughs = left_join(count_restricted_route_borough, number_recoded) %>%
  left_join(recount_restricted_route_borough) # This looks to be correct



# Making map of crossings coloured by Borough
mapview(f_restricted_route, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, zcol = "BOROUGH", legend =FALSE)













#########################################################################################
# Signage                                                                               #
# ########                                                                              #
#                                                                                       #
# Summary:                                                                              #
# 2 signs have no Borough                                                               #  
# visual examination using ONS boundaries suggests these are outside London boundaries  #
#                                                                                       #
# Action: ? delete them                                                                 #
#                                                                                       #
#########################################################################################



signage = get_cid_points(type = "signage")
signage = st_transform(signage, crs=27700) 

# convert certain columns to factors
f_variables = c("SS_ROAD", "SS_PATCH", "SS_FACING", "SS_NOCYC", "SS_NOVEH",
                "SS_NOLEFT", "SS_NORIGH", "SS_LEFT", "SS_RIGHT", "SS_NOEXCE",
                "SS_DISMOU", "SS_END", "SS_CYCSMB", "SS_PEDSMB", "SS_BUSSMB",
                "SS_SMB", "SS_LNSIGN", "SS_ARROW", "SS_NRCOL", "SS_NCN",
                "SS_LCN", "SS_SUPERH", "SS_QUIETW", "SS_GREENW",
                "SS_DESTN", "SS_CIRC", "SS_EXEMPT")

# convert columns to factors 
f_signage = signage %>%
  mutate_at(f_variables, as.factor)

# identify missing Borough details
signage_borough_NA = f_signage %>%
  filter(is.na(BOROUGH)) # 1 observation ie 1 ASL has no Borough

# import May 2020 ONS LA boundary data
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

# Map 2 signage observation that does not have a Borough with Borough boundaries
mapview(signage_borough_NA$geometry, color = "red") + mapview(lon_lad_2020, alpha.regions = 0.05, zcol = "BOROUGH") 

# Visual inspection using the mapview indicates that these are outside london boroughs so ? drop them?





##################
# No Borough NAS:
# cycle_parking
# signals
# traffic calming
# restricted points
#

