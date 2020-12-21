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

# identify missing Borough details
borough_NA = f_advanced_stop_line %>%
  filter(is.na(BOROUGH)) # 1 observation ie 1 ASL has no Borough

# import May 2020 ONS LA boundary data
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020.Rds")

# Map ASL observation that does not have a Borough with Borough boundaries
mapview(borough_NA$geometry, color = "red") + mapview(lon_lad_2020, alpha.regions = 0.05, zcol = "BOROUGH") 
# Visual inspection shows that this ASL starts in Greenwich & finishes in Lewisham (direction of travel is towards Greenwich)
# so let's code it as Greenwich 

# Count number of ASLs by Borough
count= f_advanced_stop_line %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Count = n()) # 112 Greenwich, 1 NA

# Recode this NA as Greenwich and factor the BOROUGH variable
f_advanced_stop_line$BOROUGH = factor(f_advanced_stop_line$BOROUGH) %>%
  fct_explicit_na(na_level = "Greenwich")
anyNA(f_advanced_stop_line$BOROUGH) # = FALSE so no NAs

# Recount to check coded properly
recount= f_advanced_stop_line %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(recount = n()) # 113 Greenwich, no NA

# Combine count/recount tables so can check recoding
na_coding_check = left_join(count, recount) # CORRECT

##### Final admin steps on ASL dataset #####
# check all variables in correct format
glimpse(f_advanced_stop_line) 

# create new df without geometry that enables faster analysis of data
non_geom_f_advanced_stop_line = st_drop_geometry(f_advanced_stop_line)

# create summary of df
view(dfSummary(non_geom_f_advanced_stop_line))



#######################
# Crossings #
#######################

# download crossings CID data using the Cycle Infra Lnd package
crossings = get_cid_lines(type = "crossing")


class(crossings) # => "sf"         "tbl_df"     "tbl"        "data.frame"
str(crossings) # 1687 obs, 11 variables

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

# identify missing Borough details
borough_NA = f_crossings %>%
  filter(is.na(BOROUGH)) # 28 observations ie 28 crossings have no Borough

# import May 2020 ONS LA boundary data
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020.Rds")

#create map of Crossings with missing Boroughs and label them with the feature ID
mNA = mapview(borough_NA, color = "red") + mapview(lon_lad_2020, alpha.regions = 0.05, zcol = "BOROUGH")
leafem::addStaticLabels(mNA, label = borough_NA$FEATURE_ID) # plot mNA with the FEATURE_IDs labelled so can identify

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
 



#count Boroughs before transformation
count= f_crossings %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Count = n()) # 28 NA, 99 Hillingdon

# Code Borough NAs with Borough decided by researcher utilising unique FEATURE_ID to ensure correct obs is coded
f_crossings$BOROUGH = replace(f_crossings$BOROUGH, which(f_crossings$FEATURE_ID == "RWG236647"), values = 'Hillingdon')
# ? wite code to loop through xls spreadsheet changing 


# recount Boroughs after transformation
recount= f_crossings %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Recount = n()) # 27 NA, 100 Hillingdon

# join borough counts together to check done correctly
crossing_boroughs = left_join(count, recount)




# create new df without geommetry that enables faster analysis of data
non_geom_f_crossings = st_drop_geometry(f_crossings)
str(non_geom_f_crossings)
count_borough = non_geom_f_crossings %>%
  count(BOROUGH)  







#######################
# Cycle Lanes and Tracks #
#######################

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

# c) look at cycle_lanes/tracks with NA Boroughs

anyNA(f_cycle_lane_track$BOROUGH) # = TRUE

# identify missing Borough details
borough_NA = f_cycle_lane_track %>%
  filter(is.na(BOROUGH)) # 354 observations ie 354 cycle lanes/tracks have no Borough

# import May 2020 ONS LA boundary data
#lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020.Rds")

#create map of cycle_lane/tracks  with missing Boroughs and label them with the feature ID
mNA = mapview(borough_NA, color = "red") + mapview(lon_lad_2020, alpha.regions = 0.05, zcol = "BOROUGH")
leafem::addStaticLabels(mNA, label = borough_NA$FEATURE_ID) # plot mNA with the FEATURE_IDs labelled so can identify
#looks like similar issue to ASL - the cycle lanes and tracks have been measured end to end but cross a borough boundary
# some cross multiple boundaries, some cross into non greater london areas
# some only just cross - others are 50/50

f_cycle_lane_track$length = st_length(f_cycle_lane_track$geometry)
length_cycle_lanesBYborough = f_cycle_lane_track %>%
  group_by(BOROUGH) %>%
  summarise(length = sum(length)) 
# total length of those observations without a Borough is 197374.84 [m]
sum(length_cycle_lanesBYborough$length) # = 2905876
p = 197374.84/sum(length_cycle_lanesBYborough$length)  *100
# p = 6.79% so although only 1.4% of assets dont have a Borough, these observations account for nearly 7% of the total length 
# of cycle lanes and tracks so probably worth sorting out. 








