#
# Obtaining and cleaning CID for Borough level analysis
#
# This code downloads the CID and cleans up the variables 
#
#

# install packages
library(tidyverse)
library(CycleInfraLnd)
library(sf)
#library(tmap)
library(mapview)
library(leaflet)
library(leafem)
library(leafpop)
#library(leafsync)
#library(summarytools) dont load if want to use mapview
library(forcats)
library(units)

# import May 2020 ONS LA boundary data (required for NA management)
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

#######################
# Advanced Stop Lines #
#######################

# download CID data using the Cycle Infra Lnd package
advanced_stop_line = get_cid_lines(type = "advanced_stop_line")

class(advanced_stop_line) # "sf"         "tbl_df"     "tbl"        "data.frame"
str(advanced_stop_line)

# check and convert CRS so matches ONS boundary data CRS
st_crs(advanced_stop_line) # CRS = WGS 84
advanced_stop_line = st_transform(advanced_stop_line, crs=27700) 
st_crs(advanced_stop_line) # PROJCRS["OSGB 1936 / British National Grid",

# check completeness of variables
unique(advanced_stop_line$FEATURE_ID) # 3775 unique variables
unique(advanced_stop_line$BOROUGH) # 33 Boroughs plus a NA group
unique(advanced_stop_line$SVDATE) # 290 unique survey dates, all of which are valid date

# the below all have just true and false apart from colour that has 6 options
unique(advanced_stop_line$ASL_FDR)
unique(advanced_stop_line$ASL_FDRLFT)
unique(advanced_stop_line$ASL_FDCENT)
unique(advanced_stop_line$ASL_FDRIGH)
unique(advanced_stop_line$ASL_SHARED)
unique(advanced_stop_line$ASL_COLOUR)  # "NONE" "GREEN" "RED" "BUFF/YELLOW" "BLUE" "OTHER"

# examine URL data
count_photo1 =  advanced_stop_line %>%
  count(PHOTO1_URL) # 48 have no asset photo 1
count_photo2 =  advanced_stop_line %>%
  count(PHOTO2_URL) # 51 have no asset photo 2

# convert character columns to factors
f_variables = c("ASL_FDR", "ASL_FDRLFT", "ASL_FDCENT", "ASL_FDRIGH", 
                "ASL_SHARED", "ASL_COLOUR")

# convert columns to factors (CLT_ACCESS not converted as 721 different values)
f_advanced_stop_line = advanced_stop_line %>%
  mutate_at(f_variables, as.factor)
anyNA(f_advanced_stop_line$BOROUGH) # = TRUE

# Count number of ASLs by Borough
asl_borough_count= f_advanced_stop_line %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(ASL = n()) # 33 boroughs plus 1 asl with no borough assigned

# Correct NA as Greenwich and factor the BOROUGH variable - SEE correct_borough_NAs file for why Greenwich
f_advanced_stop_line$BOROUGH = factor(f_advanced_stop_line$BOROUGH) %>%
  fct_explicit_na(na_level = "Greenwich")
anyNA(f_advanced_stop_line$BOROUGH) # = FALSE so no NAs

##### Final admin steps on ASL dataset #####
# check all variables in correct format
glimpse(f_advanced_stop_line) 

# create new df without geometry that enables faster analysis of data
non_geom_f_advanced_stop_line = st_drop_geometry(f_advanced_stop_line)

# create summary of df
view(dfSummary(non_geom_f_advanced_stop_line))



#############
# Crossings #
#############

# download crossings CID data using the Cycle Infra Lnd package
crossings = get_cid_lines(type = "crossing")


class(crossings) # => "sf"         "tbl_df"     "tbl"        "data.frame"
str(crossings) # 1687 obs, 11 variables

# check and convert CRS so matches ONS boundary data CRS
st_crs(crossings) # CRS = WGS 84
crossings = st_transform(crossings, crs=27700) 
st_crs(crossings) # PROJCRS["OSGB 1936 / British National Grid",


# check completeness of variables
unique(crossings$FEATURE_ID) # 1687 unique variables
unique(crossings$BOROUGH) # 33 Boroughs plus a NA group
unique(crossings$SVDATE) # 257 unique survey dates, all of which are valid date
# the below all have just true and false
unique(crossings$CRS_SIGNAL)
unique(crossings$CRS_CYGAP)
unique(crossings$CRS_LEVEL)
unique(crossings$CRS_PEDEST)
unique(crossings$CRS_SEGREG)

# examine URL data
count_photo1 =  crossings %>%
  count(PHOTO1_URL) # 31 have no asset photo 1
count_photo2 =  crossings %>%
  count(PHOTO2_URL) # 32 have no asset photo 2


# convert certain columns to factors
f_v = c("CRS_SIGNAL", "CRS_SEGREG", "CRS_CYGAP", "CRS_PEDEST", "CRS_LEVEL")

# convert columns to factors (BOROUGH needs doing separately as it has some NAs in, CLT_ACCESS not converted as 721 different values)
f_crossings = crossings %>%
  mutate_at(f_v, as.factor)
anyNA(f_crossings$BOROUGH) # = TRUE

# Count number of Crossings by Borough
crossings_borough_count = f_crossings %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Crossings = n()) # 33 boroughs plus 28 crossings with no borough

# Correct NAs as per correct_borough_NAs working
#  1) Create dataset of 28 crossings that have no Borough
crossings_borough_NA = f_crossings %>%
  filter(is.na(BOROUGH)) 

#  2) Use st_intersection to produce two observations for the crossings
crossings_borough_NA_i = st_intersection(lon_lad_2020, crossings_borough_NA) 
# 49 observations, geometry column is from the crossings dataset but split by ONS borough boundaries
# BOROUGH is the column from the lon_lad_2020 dataset and represents the Borough that the crossing is in, 
# whereas BOROUGH.1 is the BOROUGH from the crossing dataset (all NAs)

mapview(crossings_borough_NA_i, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.05, zcol = "BOROUGH")
#this map shows the crossings coloured by the BOROUGH they are in

# 3) Add column for length of each observation (using crossing geometry)by BOROUGH and keep obs with longest length
crossings_borough_NA_i$borough_length = st_length(crossings_borough_NA_i) 

# 4) Create final dataset that gives the proposed Borough for each observation
# 4a) create congruent dataset where visual check and st_length check matches (n = 26)
congruent = crossings_borough_NA_i %>%
  filter(!FEATURE_ID %in% c("RWG153061", "RWG049417")) #n = 45 (4 obs with these FEATURE_IDs removed)

congruent = congruent %>% 
  group_by(FEATURE_ID) %>%
  slice(which.max(borough_length)) # keep the observation which has the longest length - n = 26

# 4b) create incongruent dataset where visual check and st_length check does not match and relabel FEATURE_IDs
# ie for observations "RWG153061", "RWG049417" 
# recode Feature_ID with 'a' and 'b'
# crosschecked that a/b corresponds with the right borough
incongruent = crossings_borough_NA_i %>%
  filter(FEATURE_ID %in% c("RWG153061", "RWG049417")) %>%
  group_by(FEATURE_ID) %>%
  arrange(desc(borough_length)) %>%
  ungroup() #arrange order of data by grouping by FEATURE_ID then descending length before relabelling FEATURE_ID in subsequent lines
incongruent$FEATURE_ID = replace(incongruent$FEATURE_ID, which(incongruent$FEATURE_ID == "RWG153061"), values = c("RWG153061_1", "RWG153061_2"))
incongruent$FEATURE_ID = replace(incongruent$FEATURE_ID, which(incongruent$FEATURE_ID == "RWG049417"), values = c("RWG049417_1", "RWG049417_2"))

# 5) Create df of all the observations that were Borough NA with correct Boroughs
# and convert to MLS so has same geometry type as crossings
# 5a) create df of corrected Boroughs with columns that match Crossings dataset
crossings_borough_NA_corrected = rbind(congruent, incongruent) %>%
  select(c("FEATURE_ID","SVDATE","CRS_SIGNAL", "CRS_SEGREG", "CRS_CYGAP", 
           "CRS_PEDEST", "CRS_LEVEL", "BOROUGH", "PHOTO1_URL", "PHOTO2_URL",
           "geometry")) 


# 5b) To validate that have corrected NAs, create df of counted Boroughs before transformation
count_crossings_borough = f_crossings %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Count = n()) 

# 5c) change geometry to multilinestring
# cast geometry
unique(st_geometry_type(crossings_borough_NA_corrected)) # Linestring
crossings_borough_NA_corrected = crossings_borough_NA_corrected %>%
  st_cast("MULTILINESTRING") %>%
  mutate(BOROUGH = as.character(BOROUGH)) # and unfactor BOROUGH for step 6

# 6) Add observations with correct Boroughs to main Crossing dataset
# 6a) Drop 28 observations with no boroughs from f_crossings dataset
f_crossings = f_crossings %>%
  filter(!is.na(BOROUGH)) # 1659 observations ie  the 1687 - 28 NAs
anyNA(f_crossings$BOROUGH) # = FALSE ie all dropped

# 6b) join corrected observations to the f_crossings
f_crossings = rbind(f_crossings, crossings_borough_NA_corrected) 
anyNA(f_crossings$BOROUGH) # = FALSE

# 7) Validate have correctly 
# 7a) Recount Boroughs after transformation
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
x = crossing_boroughs %>%
  replace_na(list(Count = 0, No_recoded = 0, Recount = 0))
total <- sapply(x[,2:4], sum) # this gives figures of:
# 1687 total count
# 30 observation recoded (26 plus the 2+2 observations that had the same FEATURE_ID)
# new total number of observations in the crossins dataset of 1689 - 
## ie the original 1687 that now have the correct Borough plus the extra 2 observations that have the FEATURE_ID_2 codes

# create new df without geometry that enables faster analysis of data
non_geom_f_crossings = st_drop_geometry(f_crossings)
str(non_geom_f_crossings)
count_borough = non_geom_f_crossings %>%
  count(BOROUGH)  


##########################
# Cycle Lanes and Tracks #
##########################

# download CID data using the Cycle Infra Lnd package
cycle_lane_track = get_cid_lines(type = "cycle_lane_track") # n = 24976

class(cycle_lane_track)
str(cycle_lane_track)

# check and convert CRS so matches ONS boundary data CRS
st_crs(cycle_lane_track) # CRS = WGS 84
cycle_lane_track = st_transform(cycle_lane_track, crs=27700) 
st_crs(cycle_lane_track) # PROJCRS["OSGB 1936 / British National Grid",

# check completeness of variables
unique(cycle_lane_track$FEATURE_ID) # 24976 unique variables
unique(cycle_lane_track$BOROUGH) # 33 Boroughs plus a NA group
unique(cycle_lane_track$SVDATE) # 345 unique survey dates, and 1 "6482-04-01" date
count= cycle_lane_track %>% 
  st_drop_geometry() %>%
  group_by(SVDATE) %>% 
  summarise(Count = n()) 

# the below all have just true and false except where stated
unique(cycle_lane_track$CLT_CARR)
unique(cycle_lane_track$CLT_SEGREG)
unique(cycle_lane_track$CLT_STEPP)
unique(cycle_lane_track$CLT_PARSEG)
unique(cycle_lane_track$CLT_SHARED) # "FALSE" "TRUE"  "TCB" 
unique(cycle_lane_track$CLT_MANDAT) # "FALSE" "TRUE"  "TCB" 
unique(cycle_lane_track$CLT_ADVIS)
unique(cycle_lane_track$CLT_PRIORI) # FALSE" "TRUE"  "TRE"
unique(cycle_lane_track$CLT_CONTRA)
unique(cycle_lane_track$CLT_BIDIRE)
unique(cycle_lane_track$CLT_CBYPAS)
unique(cycle_lane_track$CLT_BBYPAS)
unique(cycle_lane_track$CLT_PARKR)
unique(cycle_lane_track$CLT_WATERR)
unique(cycle_lane_track$CLT_PTIME)
unique(cycle_lane_track$CLT_ACCESS) # NA plus 724 other unique text responses 
unique(cycle_lane_track$CLT_COLOUR) # "NONE"        "GREEN"       "RED"         "BUFF/YELLOW" "BLUE"        "OTHER"       "BUFF" 

# examine URL data
count_photo1 =  cycle_lane_track %>%
  count(PHOTO1_URL) # 588 have no asset photo 1
count_photo2 =  cycle_lane_track %>%
  count(PHOTO2_URL) # 605 have no asset photo 2

# convert certain columns to factors (CLT_ACCESS and BOROUGH not done)
f_variables = c("CLT_CARR", "CLT_SEGREG", "CLT_STEPP", "CLT_PARSEG", "CLT_SHARED", "CLT_MANDAT", 
                "CLT_ADVIS", "CLT_PRIORI", "CLT_CONTRA", "CLT_BIDIRE", "CLT_CBYPAS", "CLT_BBYPAS",
                "CLT_PARKR", "CLT_WATERR", "CLT_PTIME", "CLT_COLOUR")

# convert columns to factors (BOROUGH needs doing separately as it has some NAs in, CLT_ACCESS not converted as 721 different values)
f_cycle_lane_track = cycle_lane_track %>%
  mutate_at(f_variables, as.factor)


# Tidy up variables 

#a) CLT_SHARED and CLT_MANDAT have factor levels of TRUE, FALSE and TCB
#- ? bother or ? ignore - only 5 observations, all in greenwhich

tidy_TCB = f_cycle_lane_track %>%
  filter(CLT_SHARED == "TCB" | CLT_MANDAT == "TCB")
# NB all this infrastructure is in GReenwich
# so RWG999509 is on carriageway but CLT_MANDAT == TCB ie not true or false - photos suggest it is parallel to carriageway and separate

# import May 2020 ONS LA boundary data
#lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds"))

#create map of cycle lanes/tracks with TCB and label them with the feature ID
mNA = mapview(tidy_TCB, color = "red") + mapview(lon_lad_2020, alpha.regions = 0.05, zcol = "BOROUGH")
leafem::addStaticLabels(mNA, label = tidy_TCB$FEATURE_ID) # plot mNA with the FEATURE_IDs labelled so can identify

# b) correct TRE to TRUE in CLT_PRIORI
fct_count(f_cycle_lane_track$CLT_PRIORI) # TRE = 1, TRUE = 2264
f_cycle_lane_track$CLT_PRIORI = fct_recode(f_cycle_lane_track$CLT_PRIORI, "TRUE" = "TRE") # convert TRE to TRUE
fct_count(f_cycle_lane_track$CLT_PRIORI) # levels are only True and False with 2265 TRUE


########## Correct Borough NAs
anyNA(f_cycle_lane_track$BOROUGH) # = TRUE

# 1) identify missing Borough details
cycle_lane_borough_NA = f_cycle_lane_track %>%
  filter(is.na(BOROUGH)) # 354 observations ie 354 cycle lanes/tracks have no Borough

# 2) Split each observation into segments using ONS borough boundaries 
lanes_borough_NA_i = st_intersection(lon_lad_2020, cycle_lane_borough_NA) # 621 observations
# geometry column is from the CID lanes dataset
# cycle lane segments outside London boroughs have been dropped
# this has broken each unique FEATURE_ID into segments based on whether they cross a borough line 
# NB each segment may contain more than one geometry type
summary(lanes_borough_NA_i$geometry) # -> 519 linestrings and 102 multilinestrings
lanes_borough_NA_i = lanes_borough_NA_i %>%
  st_cast("MULTILINESTRING") # convert geometry type to MLS for all so can mapview
summary(lanes_borough_NA_i$geometry) # -> 621 MLS.  Now can mapview(lanes_borough_NA_i)

cycle_lanes_NA_map = mapview(lanes_borough_NA_i, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, zcol = "BOROUGH", legend = FALSE)


# 3) Count number of observations for each FEATURE_ID
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

# 4) Relabel FEATURE_ID so each segment can be identified
# Group 621 by FEATURE_ID then add a cumulative sum to each grouped observation (will be 1, 2 or 3)
lanes_borough_NA_corrected = lanes_borough_NA_i %>%
  mutate(n = 1) %>%
  group_by(FEATURE_ID) %>%
  mutate(cum_count = cumsum(n)) %>%
  select(-n)

# relabel the FEATURE_ID with the cumulative number so each observation has a unique ID
lanes_borough_NA_corrected$FEATURE_ID = paste(lanes_borough_NA_corrected$FEATURE_ID, "_", lanes_borough_NA_corrected$cum_count)

# 5) Check that the correct number have 1, 2 or 3 appended to the FEATURE_ID
lanes_borough_NA_corrected %>%
  st_drop_geometry() %>%
  group_by(cum_count) %>%
  count()
# cum_count   n
#       1   354  (= 1 + 265 + 88)
#       2   266  (= 1+ 265)
#       3     1

# 6) Create df of all the observations with correct Boroughs that can be joined
lanes_borough_NA_corrected = lanes_borough_NA_corrected %>%
  select(c("FEATURE_ID", "SVDATE", "CLT_CARR", "CLT_SEGREG", "CLT_STEPP", 
           "CLT_PARSEG", "CLT_SHARED", "CLT_MANDAT", "CLT_ADVIS",  "CLT_PRIORI",
           "CLT_CONTRA", "CLT_BIDIRE", "CLT_CBYPAS", "CLT_BBYPAS", "CLT_PARKR", 
           "CLT_WATERR", "CLT_PTIME",  "CLT_ACCESS", "CLT_COLOUR", "BOROUGH", 
           "PHOTO1_URL", "PHOTO2_URL", "geometry")) %>%
  mutate(BOROUGH = as.character(BOROUGH)) # change borough to character so can match back to f_cycle_lane_track

# NB geometry is already Multilinestring so no need to convert

# 7) Validate that have corrected NAs so count NAs before transformation
count_lanes_borough = f_cycle_lane_track %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Count = n()) 

# 8) Join main lanes dataset to corrected borough dataset
#  8a) drop obs with no boroughs from f_cycle_lane_track
f_cycle_lane_track = f_cycle_lane_track %>%
  filter(!is.na(BOROUGH)) # 24622 observations ie  the 24976 - 354 NAs
anyNA(f_cycle_lane_track$BOROUGH) # = FALSE ie all dropped

#  8b) join corrected observations to the f_cycle_lane_track
f_cycle_lane_track = rbind(f_cycle_lane_track, lanes_borough_NA_corrected) 
anyNA(f_cycle_lane_track$BOROUGH) # = FALSE

# 9) Validate have correctly transformed
#  9a) Recount Boroughs after transformation
recount_lanes_borough = f_cycle_lane_track %>%  
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Recount = n()) 

#  9b) Check how many recoded
number_recoded = lanes_borough_NA_corrected %>%  
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Number_recoded = n())

#  9c) Create df of the counts
lanes_boroughs = left_join(count_lanes_borough, number_recoded) %>%
  left_join(recount_lanes_borough) # This looks to be correct

#  9d) Compare totals to make sure they match
x = lanes_boroughs %>%
  replace_na(list(Count = 0, Number_recoded = 0, Recount = 0))
total <- sapply(x[,2:4], sum) # this gives figures of:
# 24976 total count
# 621 observation recoded 
# new total number of observations in the lanes dataset of 25243 
# 25243 = 24976 (original number) - 354 (no borough) + 621 (recoded)


# 10) Visually inspect cycle lanes and tracks to check now coded correct Borough
mapview(lon_lad_2020, alpha.regions = 0.1, lwd = 1) + 
  mapview(f_cycle_lane_track, zcol = "BOROUGH") 
# there appears to be a cycle lane/track that is yellow (Wandsworth) that is in the Boundaries of Merton

wandsworth = f_cycle_lane_track %>%
  filter(BOROUGH == "Wandsworth")
wandsworth_map = mapview(wandsworth, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, lwd = 1)
leafem::addStaticLabels(m1, label = wandsworth$FEATURE_ID) # plot mNA with the FEATURE_IDs labelled so can identify







# Borough level analysis
#####
# Count number of cycle lanes by Borough
cycle_lane_track_borough_count = f_cycle_lane_track %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(CycleLanesAndTracks = n()) # 33 boroughs no NAs

# Length of cycle lanes and tracks by Borough
f_cycle_lane_track$length_m = st_length(f_cycle_lane_track$geometry)
f_cycle_lane_track$length_km = set_units(st_length(f_cycle_lane_track$geometry), km)

cycle_lane_track_borough_length = f_cycle_lane_track %>%
  group_by(BOROUGH) %>%
  summarise(across(c(length_m, length_km), sum))
# drop geometry cant be done within the pipe as it causes weirdness in the measurements (see issue 1)
# it needs to be dropped to make dataset manageable
cycle_lane_track_borough_length = st_drop_geometry(cycle_lane_track_borough_length) 


# length of cycle lanes and tracks by on/off road by Borough ???
# ??? other questions

# create new df without geometry that enables faster analysis of data
non_geom_f_cycle_lane_track = st_drop_geometry(f_cycle_lane_track)
str(non_geom_f_cycle_lane_track)

#####################
# Restricted Routes #
#####################

# download CID data using the Cycle Infra Lnd packages
restricted_route = get_cid_lines(type = "restricted_route")

class(restricted_route)
str(restricted_route)

# check and convert CRS so matches ONS boundary data CRS
st_crs(restricted_route) # CRS = WGS 84
restricted_route = st_transform(restricted_route, crs=27700) 
st_crs(restricted_route) # PROJCRS["OSGB 1936 / British National Grid",

unique(restricted_route$FEATURE_ID) # 1378 unique variables
unique(restricted_route$BOROUGH) # 33 Boroughs plus a NA group
unique(restricted_route$SVDATE) # 196 unique survey dates, all of which are valid date

# the below all have just true and false 
unique(restricted_route$RES_PEDEST)
unique(restricted_route$RES_BRIDGE)
unique(restricted_route$RES_TUNNEL)
unique(restricted_route$RES_STEPS)
unique(restricted_route$RES_LIFT)

# examine URL data
count_photo1 =  restricted_route %>%
  count(PHOTO1_URL) # 71 have no asset photo 1
count_photo2 =  restricted_route %>%
  count(PHOTO2_URL) # 52 have no asset photo 2

# convert certain columns to factors
levels(restricted_route$RES_PEDEST) # => NULL

f_variables = c("RES_PEDEST", "RES_BRIDGE", "RES_TUNNEL", "RES_STEPS", "RES_LIFT")

# convert columns to factors (BOROUGH needs doing separately as it has some NAs in, CLT_ACCESS not converted as 721 different values)
f_restricted_route = restricted_route %>%
  mutate_at(f_variables, as.factor)
anyNA(f_restricted_route$BOROUGH) # = TRUE


########## Correct Borough NAs

# 1) identify missing Borough details
restricted_route_borough_NA = f_restricted_route %>%
  filter(is.na(BOROUGH)) # 18 observations ie 18 restricted routes no Borough










glimpse(f_restricted_route) # check converted ok
levels(f_restricted_route$BOROUGH) # check have 34 (33 actual boroughs plus 1 NA value)

# Borough level analysis
# Count number of restricted routes by Borough
restricted_route_borough_count = f_restricted_route %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(RestrictedRoutes = n()) # 33 boroughs plus 18 restricted routes with no borough

# Length of restricted routes by Borough
f_restricted_route$length_m = st_length(f_restricted_route$geometry)
f_restricted_route$length_km = set_units(st_length(f_restricted_route$geometry), km)

restricted_route_borough_length = f_restricted_route %>%
  group_by(BOROUGH) %>%
  summarise(across(c(length_m, length_km), sum))
# drop geometry cant be done within the pipe as it causes weirdness in the measurements (see issue 1)
# it needs to be dropped to make dataset manageable
restricted_route_borough_length = st_drop_geometry(restricted_route_borough_length) 



# create new df without geometry that enables faster analysis of data
non_geom_f_restricted_route = st_drop_geometry(f_restricted_route)
str(non_geom_f_restricted_route)
non_geom_f_restricted_route %>%
  count(BOROUGH)  

# create summary of df
view(dfSummary(non_geom_f_restricted_route))



#####################
# Cycle Parking     #
#####################

# download CID data using the Cycle Infra Lnd package
cycle_parking = get_cid_points(type = "cycle_parking")
class(cycle_parking)
str(cycle_parking)

# check and convert CRS so matches ONS boundary data CRS
st_crs(cycle_parking) # CRS = WGS 84
cycle_parking = st_transform(cycle_parking, crs=27700) 
st_crs(cycle_parking) # PROJCRS["OSGB 1936 / British National Grid",

# check completeness of variables
unique(cycle_parking$FEATURE_ID) # 23758 unique variables
unique(cycle_parking$BOROUGH) # 33 Boroughs no NAS
unique(cycle_parking$SVDATE) # 331 unique survey dates, all of which are valid date
# the below all have just true and false unless stated
unique(cycle_parking$PRK_CARR)
unique(cycle_parking$PRK_COVER)
unique(cycle_parking$PRK_SECURE)
unique(cycle_parking$PRK_LOCKER)
unique(cycle_parking$PRK_SHEFF)
unique(cycle_parking$PRK_MSTAND)
unique(cycle_parking$PRK_PSTAND)
unique(cycle_parking$PRK_HOOP)
unique(cycle_parking$PRK_POST)
unique(cycle_parking$PRK_BUTERF)
unique(cycle_parking$PRK_WHEEL)
unique(cycle_parking$PRK_HANGAR)
unique(cycle_parking$PRK_TIER)
unique(cycle_parking$PRK_OTHER)
unique(cycle_parking$PRK_CPT) # contains NA
unique(cycle_parking$PRK_PROVIS) # contains NA

sum(is.na(cycle_parking$PRK_PROVIS)) # 2 NAs
sum(is.na(cycle_parking$PRK_CPT)) # 2 NAs


# examine URL data
count_photo1 =  cycle_parking %>%
  count(PHOTO1_URL) # 299 have no asset photo 1
count_photo2 =  cycle_parking %>%
  count(PHOTO2_URL) # 298 have no asset photo 2

# convert certain columns to factors
f_variables = c("PRK_CARR", "PRK_COVER", "PRK_SECURE", "PRK_LOCKER", "PRK_SHEFF", "PRK_MSTAND",
                "PRK_PSTAND", "PRK_HOOP", "PRK_POST", "PRK_BUTERF", "PRK_WHEEL", "PRK_HANGAR",
                "PRK_TIER", "PRK_OTHER", "BOROUGH")

# convert columns to factors (BOROUGH done too as no NAs)
f_cycle_parking = cycle_parking %>%
  mutate_at(f_variables, as.factor)

glimpse(f_cycle_parking) # check converted ok
levels(f_cycle_parking$BOROUGH) # have 33 and no NA value


# Borough level analysis
# a) Number of cycle parking locations/sites
cycle_parking_sites_borough_count = f_cycle_parking %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(CycleParkingSites = n())


# b) Number of cycling parking spaces (capacity)
# PRK_PROVIS (number of stands etc) and PRK_CPT (number of bikes that can be 
# parked) have NAs.  These need correcting before can calculate no of 
# bikes that can be parked per borough

# NEEDS TO ADD CODE FOR CYCLE PARKING SPACES
# PRK_CPT = number of bikes that can be easily parked
sum(is.na(cycle_parking$PRK_PROVIS)) # 2 NA
sum(is.na(cycle_parking$PRK_CPT)) # 2 NAs
CPcount_borough_NA = f_cycle_parking %>% # identify which observations are NA
  st_drop_geometry() %>%
  filter(is.na(PRK_CPT)) # 2 observations have NA - both for PROVIS & CPT

# Asset images reviewed - able to see how many stands and spaces each have
# RWG999580 4 stands (PROVIS) 8 spaces (CPT)
# RWG999458 3 stands, 6 spaces
# therefore can correct missing data
f_cycle_parking$PRK_PROVIS[f_cycle_parking$FEATURE_ID == "RWG999580"] = 4
f_cycle_parking$PRK_CPT[f_cycle_parking$FEATURE_ID == "RWG999580"] = 8
f_cycle_parking$PRK_PROVIS[f_cycle_parking$FEATURE_ID == "RWG999458"] = 3
f_cycle_parking$PRK_CPT[f_cycle_parking$FEATURE_ID == "RWG999458"] = 6

# check coded correctly
x = f_cycle_parking %>%
  filter(FEATURE_ID == "RWG999580" | FEATURE_ID == "RWG999458") # = yes coded correctly
# now calculate number of cycle parking spaces by borough
cycle_parking_spaces_borough_count = f_cycle_parking %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(CycleParkingSpaces = sum(PRK_CPT))

# create new df without geommetry that enables faster analysis of data
non_geom_f_cycle_parking = st_drop_geometry(f_cycle_parking)
str(non_geom_f_cycle_parking)

###########
# Signals #
###########

# download CID data using the Cycle Infra Lnd package

signal = get_cid_points(type = "signal") #n = 443
class(signal)
str(signal)

# check and convert CRS so matches ONS boundary data CRS
st_crs(signal) # CRS = WGS 84
signal = st_transform(signal, crs=27700) 
st_crs(signal) # PROJCRS["OSGB 1936 / British National Grid",


# check completeness of variables
unique(signal$FEATURE_ID) # 443 unique variables
unique(signal$BOROUGH) # 23 Boroughs, no NAS
unique(signal$SVDATE) # 111 unique survey dates, all of which are valid dates

# the below all have just true and false
unique(signal$SIG_HEAD)
unique(signal$SIG_SEPARA)
unique(signal$SIG_EARLY)
unique(signal$SIG_TWOSTG)
unique(signal$SIG_GATE)

# examine URL data
count_photo1 =  f_signal %>%
  count(PHOTO1_URL) # 8 have no asset photo 1
count_photo2 =  signal %>%
  count(PHOTO2_URL) # 8 have no asset photo 2

# convert certain columns to factors
f_variables = c("SIG_HEAD", "SIG_SEPARA", "SIG_EARLY", "SIG_TWOSTG", "SIG_GATE", "BOROUGH")

# convert columns to factors
f_signal = signal %>%
  mutate_at(f_variables, as.factor)

glimpse(f_signal) # check converted ok
levels(f_signal$BOROUGH) # only have 23 and no NA value

# Borough level analysis
signal_borough_count = f_signal %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Signals = n())

# create new df without geommetry that enables faster analysis of data
non_geom_f_signal = st_drop_geometry(f_signal)
str(non_geom_f_signal)


# create summary of df
view(dfSummary(non_geom_f_signal))


###################
# Traffic calming #
###################

# download CID data using the Cycle Infra Lnd package
traffic_calming = get_cid_points(type = "traffic_calming")
class(traffic_calming)
str(traffic_calming)

# check and convert CRS so matches ONS boundary data CRS
st_crs(traffic_calming) # CRS = WGS 84
traffic_calming = st_transform(traffic_calming, crs=27700) 
st_crs(traffic_calming) # PROJCRS["OSGB 1936 / British National Grid",

# check completeness of variables
unique(traffic_calming$FEATURE_ID) # 58565 unique variables
unique(traffic_calming$BOROUGH) # 33 Boroughs, no NAs
unique(traffic_calming$SVDATE) # 334 unique survey dates, all of which are valid date
# the below all have just true and false
unique(traffic_calming$TRF_RAISED)
unique(traffic_calming$TRF_ENTRY)
unique(traffic_calming$TRF_CUSHI)
unique(traffic_calming$TRF_HUMP)
unique(traffic_calming$TRF_SINUSO)
unique(traffic_calming$TRF_BARIER)
unique(traffic_calming$TRF_NAROW)
unique(traffic_calming$TRF_CALM)

# convert certain columns to factors
f_variables = c("TRF_RAISED", "TRF_ENTRY", "TRF_CUSHI", "TRF_HUMP", "TRF_SINUSO",
                "TRF_BARIER", "TRF_NAROW", "TRF_CALM", "BOROUGH")

# convert columns to factors 
f_traffic_calming = traffic_calming %>%
  mutate_at(f_variables, as.factor)

glimpse(f_traffic_calming) # check converted ok
levels(f_traffic_calming$BOROUGH) # have 33 and no NA value

count_photo1 =  f_traffic_calming %>%
  count(PHOTO1_URL) # 805 have no asset photo 1
count_photo2 =  f_traffic_calming %>%
  count(PHOTO2_URL) # 810 have no asset photo 2

# Borough level analysis
traffic_calming_borough_count = f_traffic_calming %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(TrafficCalming = n())



# create new df without geometry that enables faster analysis of data
non_geom_f_traffic_calming = st_drop_geometry(f_traffic_calming)
str(non_geom_f_traffic_calming)

# create summary of df
view(dfSummary(non_geom_f_traffic_calming))


###########
# Signage #
###########

# download CID data using the Cycle Infra Lnd package

signage = get_cid_points(type = "signage")
class(signage)
str(signage)

# check and convert CRS so matches ONS boundary data CRS
st_crs(signage) # CRS = WGS 84
signage = st_transform(signage, crs=27700) 
st_crs(signage) # PROJCRS["OSGB 1936 / British National Grid",

# check completeness of variables
unique(signage$FEATURE_ID) # 118833 unique variables RWG999275 is present twice. 
unique(signage$BOROUGH) # 33 Boroughs plus a NA group, 2 observation have NA for Borough x = f_signage %>% count(BOROUGH) %>% st_drop_geometry()
unique(signage$SVDATE) # 349 unique survey dates, all of which are valid date
# the below all have just true and false except where specified:
unique(signage$SS_ROAD) # TRUE FALSE NA - 1 observations has NA
unique(signage$SS_PATCH)
unique(signage$SS_FACING)
unique(signage$SS_NOCYC)
unique(signage$SS_NOVEH)
unique(signage$SS_CIRC)
unique(signage$SS_EXEMPT)
unique(signage$SS_NOLEFT)
unique(signage$SS_NORIGH)
unique(signage$SS_LEFT)
unique(signage$SS_RIGHT)
unique(signage$SS_NOEXCE)
unique(signage$SS_DISMOU)
unique(signage$SS_END)
unique(signage$SS_CYCSMB) # TRUE FALSE FASLE
unique(signage$SS_PEDSMB)
unique(signage$SS_BUSSMB)
unique(signage$SS_SMB)
unique(signage$SS_LNSIGN)
unique(signage$SS_ARROW)
unique(signage$SS_NRCOL)
unique(signage$SS_NCN)
unique(signage$SS_LCN)
unique(signage$SS_SUPERH)
unique(signage$SS_QUIETW)
unique(signage$SS_GREENW)
unique(signage$SS_ROUTEN)  # 429 different names including 105883 observations with NA and 3 wih no data
unique(signage$SS_DESTN)
unique(signage$SS_ACCESS) # 802 different names including 115602 observations with NA and 5 with no data
unique(signage$SS_NAME) # 65 unique labels including 3156 NA plus 2648 obs with no data
unique(signage$SS_COLOUR) #  NONE GREEN RED BLUE NA <Null> BUFF/YELLOW: 3 NA, 2 <NULL>, 117188 NONE

# examine URL data
count_photo1 =  signage %>%
  count(PHOTO1_URL) # 1347 have no asset photo 1
count_photo2 =  signage %>%
  count(PHOTO2_URL) # 1336 have no asset photo 2

# examine RWG999275
RWG_duplicate = signage %>%
  filter(FEATURE_ID == "RWG999275")
mapview(RWG_duplicate)
# create new FEATURE_IDs so that observations can be differentiated
signage$FEATURE_ID[signage$BOROUGH == "Barnet"] = "RWG999275a"
signage$FEATURE_ID[signage$BOROUGH == "Haringey"] = "RWG999275b"

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

f_signage$BOROUGH = factor(signage$BOROUGH, exclude = NULL) # these variables are categorical done separately as have NAs
f_signage$SS_ROAD = factor(signage$SS_ROAD, exclude = NULL) 
f_signage$SS_COLOUR = factor(signage$SS_COLOUR, exclude = NULL)


# recode SS_CYCSMB where values include "FALSE" and "FASLE"
f_signage$SS_CYCSMB= fct_collapse(f_signage$SS_CYCSMB, 
                                           "FALSE" = c("FALSE", "FASLE"))

# Borough level analysis
signage_borough_count = f_signage %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Signage = n())

glimpse(f_signage) # check converted ok

# recode SS_CYCSMB where values include "FALSE" and "FASLE"
non_geom_f_signage$SS_COLOUR = fct_collapse(non_geom_f_signage$SS_COLOUR, 
                                            "NONE" = NA) 


#####################
# Restricted points #
#####################

# download CID data using the Cycle Infra Lnd package
restricted_points = get_cid_points(type = "restricted_point")
class(restricted_points)
str(restricted_points)

# check and convert CRS so matches ONS boundary data CRS
st_crs(restricted_points) # CRS = WGS 84
restricted_points = st_transform(restricted_points, crs=27700) 
st_crs(signage) # PROJCRS["OSGB 1936 / British National Grid",


# check completeness of variables
unique(restricted_points$FEATURE_ID) # 180 unique variables
unique(restricted_points$BOROUGH) # 27 Boroughs, no NAS
unique(restricted_points$SVDATE) # 73 unique survey dates, all of which are valid dates

# the below all have just true and false
unique(restricted_points$RST_STEPS)
unique(restricted_points$RST_LIFT)

# examine URL data
count_photo1 =  restricted_points %>%
  count(PHOTO1_URL) # 12 have no asset photo 1
count_photo2 =  restricted_points %>%
  count(PHOTO2_URL) # 12 have no asset photo 2

# convert certain columns to factors
f_variables = c("RST_STEPS", "RST_LIFT", "BOROUGH")

# convert columns to factors (BOROUGH needs doing separately as it has some NAs in, CLT_ACCESS not converted as 721 different values)
f_restricted_points = restricted_points %>%
  mutate_at(f_variables, as.factor)

glimpse(f_restricted_points) # check converted ok

# Borough level analysis
restricted_points_borough_count = f_restricted_points %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(RestrictedPoints = n())


# create new df without geommetry that enables faster analysis of data
non_geom_f_restricted_points = st_drop_geometry(f_restricted_points)
str(non_geom_f_restricted_points)

# create summary of df
view(dfSummary(non_geom_f_restricted_points))










##################################################################################
# Create data frames for Borough level analysis of assets by type
#
# 
# 

####### Count of assets by Borough
CID_by_borough = list(asl_borough_count, crossings_borough_count, 
                      cycle_lane_track_borough_count, 
                      restricted_route_borough_count,
                      cycle_parking_sites_borough_count,
                      cycle_parking_space_borough_count
                      signal_borough_count,
                      traffic_calming_borough_count,
                      signage_borough_count,
                      restricted_points_borough_count) %>%
  reduce(left_join, by = "BOROUGH")

# need to replace NA in Borough with 'Borough not stated' and NAs in other columns with 0
sum(is.na(CID_by_borough)) # = 21

# Replace 'Borough NA' with text and count NAs with 0
CID_by_borough = CID_by_borough %>% 
  replace_na(list(BOROUGH = "No Borough stated in CID")) %>%
  replace(is.na(.), 0)
sum(is.na(CID_by_borough)) # checks that no NAs now exist (= 0)

saveRDS(CID_by_borough, file = "/home/bananafan/Documents/PhD/Paper1/output/CID_count_by_borough")

###### Length of line assets by Borough
length_CID_by_borough = list(cycle_lane_track_borough_length,
                             restricted_route_borough_length) %>%
  reduce(left_join, by = "BOROUGH") %>%
  mutate(across(2:5, round, 1)) %>%
  rename(c(CycleLaneTrack_km = length_km.x, RestrictedRoute_km = length_km.y,
           CycleLaneTrack_m = length_m.x, RestrictedRoute_m = length_m.y)) %>%
  replace_na(list(BOROUGH = "No Borough stated in CID")) # Correct NA in Borough column

saveRDS(length_CID_by_borough, file = "/home/bananafan/Documents/PhD/Paper1/output/CID_length_by_borough")

