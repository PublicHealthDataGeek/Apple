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
#library(leafsync)
#library(summarytools)
library(forcats)
library(units)
#library(geojsonsf)

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
#lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020.Rds")

#create map of cycle lanes/tracks with TCB and label them with the feature ID
mNA = mapview(tidy_TCB, color = "red") + mapview(lon_lad_2020, alpha.regions = 0.05, zcol = "BOROUGH")
leafem::addStaticLabels(mNA, label = tidy_TCB$FEATURE_ID) # plot mNA with the FEATURE_IDs labelled so can identify

# b) correct TRE to TRUE in CLT_PRIORI
fct_count(f_cycle_lane_track$CLT_PRIORI) # TRE = 1, TRUE = 2264
f_cycle_lane_track$CLT_PRIORI = fct_recode(f_cycle_lane_track$CLT_PRIORI, "TRUE" = "TRE") # convert TRE to TRUE
fct_count(f_cycle_lane_track$CLT_PRIORI) # levels are only True and False with 2265 TRUE

# Borough level analysis
#####
# Count number of Crossings by Borough
cycle_lane_track_borough_count = f_cycle_lane_track %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(CycleLanesAndTracks = n()) # 33 boroughs plus 28 crossings with no borough

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
f_restricted_route$BOROUGH = factor(restricted_route$BOROUGH, exclude = NULL)

glimpse(f_restricted_route) # check converted ok
levels(f_restricted_route$BOROUGH) # check have 34 (33 actual boroughs plus 1 NA value)






# create new df without geommetry that enables faster analysis of data
non_geom_f_restricted_route = st_drop_geometry(f_restricted_route)
str(non_geom_f_restricted_route)
non_geom_f_restricted_route %>%
  count(BOROUGH)  

# create summary of df
view(dfSummary(non_geom_f_restricted_route))






############
# Join all datasets to give numbers by Borough

CID_by_borough = list(asl_borough_count, crossings_borough_count, cycle_lane_track_borough_count) %>%
  reduce(left_join, by = "BOROUGH")




