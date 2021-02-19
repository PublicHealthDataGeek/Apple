###################################################################################
# Data cleaning CID - cycle lane and tracks                                       #
#                                                                                 #
# This code downloads the CID and cleans up the variables                         #
# It recodes the observations that has no Borough assigned                        #
# It also checks that the Boroughs are correctly assigned where they exist        #
# Where cycle lanes and tracks cross more than one London Borough they are split  #
# into multiple observations based on their Borough location                      #
#                                                                                 #
###################################################################################

######################################
# Install packages and load datasets #
######################################

# install packages
library(tidyverse)
library(CycleInfraLnd)
library(sf)
library(mapview)
library(leaflet)
library(leafem)
library(forcats)
library(units)
#library(tmap)
#library(leafsync)
#library(summarytools) dont load if want to use mapview

# import May 2020 ONS LA boundary data (required for NA management)
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

# download CID data using the Cycle Infra Lnd package
cycle_lane_track = get_cid_lines(type = "cycle_lane_track") # n = 24976

# Convert CRS so matches ONS boundary data CRS
cycle_lane_track = st_transform(cycle_lane_track, crs=27700) 
st_crs(cycle_lane_track) # PROJCRS["OSGB 1936 / British National Grid",

###################################
# Check completeness of variables #
###################################
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

# Examine url data for completeness
count_photo1 =  cycle_lane_track %>%
  count(PHOTO1_URL) # 588 have no asset photo 1
count_photo2 =  cycle_lane_track %>%
  count(PHOTO2_URL) # 605 have no asset photo 2

###############################################
# Tidy up variables and data - except BOROUGH #
###############################################

# Convert columns to factors (CLT_ACCESS and BOROUGH not done)
### create list of columns to factor
f_variables = c("CLT_CARR", "CLT_SEGREG", "CLT_STEPP", "CLT_PARSEG", "CLT_SHARED", "CLT_MANDAT", 
                "CLT_ADVIS", "CLT_PRIORI", "CLT_CONTRA", "CLT_BIDIRE", "CLT_CBYPAS", "CLT_BBYPAS",
                "CLT_PARKR", "CLT_WATERR", "CLT_PTIME", "CLT_COLOUR")
# NB BOROUGH needs doing separately as it has some NAs in, CLT_ACCESS not converted as 721 different values

### factor columns 
f_cycle_lane_track = cycle_lane_track %>%
  mutate_at(f_variables, as.factor)

# Correct CLT_PRIORI: TRE to TRUE
#fct_count(f_cycle_lane_track$CLT_PRIORI) # TRE = 1, TRUE = 2264
f_cycle_lane_track$CLT_PRIORI = fct_recode(f_cycle_lane_track$CLT_PRIORI, "TRUE" = "TRE") # convert TRE to TRUE
#fct_count(f_cycle_lane_track$CLT_PRIORI) # levels are only True and False with 2265 TRUE

# Explore and correct CLT_SHARED and CLT_MANDAT 
f_cycle_lane_track$CLT_SHARED = fct_recode(f_cycle_lane_track$CLT_SHARED, "TRUE" = "TCB")
f_cycle_lane_track$CLT_MANDAT = fct_recode(f_cycle_lane_track$CLT_MANDAT, "TRUE" = "TCB") # convert TRE to TRUE# convert TRE to TRUE
# These two variables have have factor levels of TRUE, FALSE and TCB
#tidy_TCB = f_cycle_lane_track %>%
#  filter(CLT_SHARED == "TCB" | CLT_MANDAT == "TCB") # subset dataset
# mapview(tidy_TCB, zcol = "FEATURE_ID")
#CLT_MAND has 1 observation with TCB
#CLT_SHARED has 4 observations with TCB
# NB all this infrastructure is in Greenwich

# a) CLT_MANDAT = TCB
# 1 observation (RWG999509), photos and google street view examined
# This is on carriageway on a one way road that has a left cycle lane and a
# right sided bus lane.  There is a painted cycle at the end of cycle lane
# Conclusion - it appears to be a mandatory cycle lane according to CID guidance
# but is just missing a painted cycle at the start.  Therefore recode as TRUE

# b) CLT_SHARED = TCB
# 4 observations, photos and google street view examined.  These are all 'off carriageway'.
# According to CID asset guidance, off carriageway CLT_SHARED should be set to 
# true if there is a footway, footpath or other area of public space shared 
# between pedestrians and cyclists.  It appears that this is generally true for
# lengths these cycle tracks.  Some have painted cycles on the tarmac but then dont
# have lines split up the space.  Signs are difficult to see.  
# Conclusion set all to true. 


###################
# Tidy up BOROUGH #
###################

# There are 354 cycle lanes and tracks that have no Borough assigned ('NA')
# When mapped, it can been seen that some cycle lanes/tracks that are coloured
# ie have BOROUGH assigned actually cross into other Boroughs.  Most obvious example
# is a yellow track in Merton (near Wimbledon)
mapview(f_cycle_lane_track, zcol = "BOROUGH", na.color = "red") + 
  mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
## NB above map - Westminster legend is coloured red but its lines are a yellow. 
## Will need sorting if publishing.  

# As comparing Boroughs by count and length, need to ensure that each bit of
# cycle lane/track is correctly assigned to Borough.  

# Will deal with observations that have a Borough coded first then move onto sorting
# out those without a Borough. 

# Create datasets for those with and without a Borough coded - totals add up to 24976 (correct!)
cycle_lane_borough = f_cycle_lane_track %>%
  filter(!is.na(BOROUGH)) # 24622 observations 

cycle_lane_borough_NA = f_cycle_lane_track %>%
  filter(is.na(BOROUGH)) # 354 observations ie 354 cycle lanes/tracks have no Borough

##### PART 1 - ensure that all observations have the correct Borough attached
cycle_lane_borough_split = st_intersection(lon_lad_2020, cycle_lane_borough) 
# n = 24694 ie 72 more than 24622
# all observations in this dataset have BOROUGH from the ONS dataset and BOROUGH.1 from the CID 
# there are no missing values in this dataset
#anyNA(cycle_lane_borough_split$BOROUGH) # FALSE
#anyNA(cycle_lane_borough_split$BOROUGH.1) # FALSE

# find newly created segments by identifying FEATURE_IDs with more than 1 observation
multi_feature_id = cycle_lane_borough_split %>%
  st_drop_geometry() %>%
  group_by(FEATURE_ID) %>%
  summarise(num_obs = n())%>%
  group_by(num_obs) %>%
  filter(num_obs == 2) %>%
  ungroup() 

# create list of FEATURE_IDs that have 2 segments
multi_feature_id_list = pull(multi_feature_id, FEATURE_ID) # create list of the FEATURE_ID 

# create dataset containing the observations with these FEATURE_IDs
new_segments = cycle_lane_borough_split %>%
  filter(FEATURE_ID %in% multi_feature_id_list) # n = 144, the 72 original ones 
# plus the additional 72 created when they were split

mapview(new_segments, zcol = "BOROUGH") + 
  mapview(lon_lad_2020, alpha.regions = 0.1, zcol = "BOROUGH", legend = FALSE, lwd = 1)

# therefore, would expect that for 72 the BOROUGH will match BOROUGH.1 but for 72 it wont.
new_segment_match_pre = new_segments %>%
  st_drop_geometry() %>%
  select(c("FEATURE_ID", "BOROUGH", "BOROUGH.1")) %>%
  mutate(match = ifelse(BOROUGH == BOROUGH.1, TRUE, FALSE)) %>%
  rename(c("ONS" = "BOROUGH", "CID" = "BOROUGH.1"))  # Yes - this what we see.

# Now to ensure the BOROUGH.1 (CID) are correctly coded with the ONS BOROUGH 
## 1) Need to factor BOROUGH.1 
borough_levels = c("Barking & Dagenham", "Barnet", "Bexley", "Brent",  
                   "Bromley", "Camden", "City of London", "Croydon", 
                   "Ealing", "Enfield", "Greenwich", "Hackney",  
                   "Hammersmith & Fulham", "Haringey", "Harrow", 
                   "Havering", "Hillingdon", "Hounslow", "Islington", 
                   "Kensington & Chelsea", "Kingston upon Thames",  
                   "Lambeth", "Lewisham", "Merton", "Newham", 
                   "Redbridge", "Richmond upon Thames", "Southwark",  
                   "Sutton", "Tower Hamlets", "Waltham Forest",   
                   "Wandsworth", "Westminster") 
new_segments$BOROUGH.1 = factor(new_segments$BOROUGH.1, levels = borough_levels)

## 2) Run loop to run through and recode BOROUGH.1 (CID) with BOROUGH (ONS)
for (i in seq_along(new_segments$BOROUGH)) {
  new_segments$BOROUGH.1[[i]] = new_segments$BOROUGH[[i]]
}

## 3) Check segments have been recoded correctly
new_segment_match_post = new_segments %>%
  st_drop_geometry() %>%
  select(c("FEATURE_ID", "BOROUGH", "BOROUGH.1")) %>%
  mutate(match = ifelse(BOROUGH == BOROUGH.1, TRUE, FALSE)) %>%
  rename(c("ONS" = "BOROUGH", "CID" = "BOROUGH.1")) # Now all match

# Relabel FEATURE_ID so each segment can be identified
##  Group 144 by FEATURE_ID then add a cumulative sum to each grouped observation (will be 1 or 2)
new_segments_corrected = new_segments %>%
  mutate(n = 1) %>%
  group_by(FEATURE_ID) %>%
  mutate(count = cumsum(n)) %>%
  select(-n)

## relabel the FEATURE_ID with the cumulative number so each observation has a unique ID
new_segments_corrected$FEATURE_ID = 
  paste(new_segments_corrected$FEATURE_ID, "_", new_segments_corrected$count)

## Check that the correct number should have 1 or 2 appended to the FEATURE_ID
new_segments_corrected %>%
  st_drop_geometry() %>%
  group_by(count) %>%
  count()
#   count   n
#       1   72
#       2   72

# check geometry 
st_geometry_type(lanes_borough_corrected) # mix of linestring and multilinestring

# Create df of all the observations with correct Boroughs that can be joined
lanes_borough_corrected = new_segments_corrected %>%
  select(c("FEATURE_ID", "SVDATE", "CLT_CARR", "CLT_SEGREG", "CLT_STEPP", 
           "CLT_PARSEG", "CLT_SHARED", "CLT_MANDAT", "CLT_ADVIS",  "CLT_PRIORI",
           "CLT_CONTRA", "CLT_BIDIRE", "CLT_CBYPAS", "CLT_BBYPAS", "CLT_PARKR", 
           "CLT_WATERR", "CLT_PTIME",  "CLT_ACCESS", "CLT_COLOUR", "BOROUGH", 
           "PHOTO1_URL", "PHOTO2_URL", "geometry")) %>%
  st_cast("MULTILINESTRING") %>%
  mutate(BOROUGH = as.character(BOROUGH)) # change borough to character so can match back to f_cycle_lane_track

# Remove duplicates from f_cycle_lane_track
f_cycle_lane_track = f_cycle_lane_track %>%
  filter(!FEATURE_ID %in% multi_feature_id_list) # n = 24904 ie 24976 - 72 THis is correct

# Join lanes_borough_corrected to f_cycle_lane_track - will then have 25048 observations
f_cycle_lane_track = rbind(f_cycle_lane_track, lanes_borough_corrected) # n= 25048

# If visualise again, now looks correct - just need to sort out observations with BOROUGH NAs
mapview(f_cycle_lane_track, zcol = "BOROUGH", na.color = "red") + 
  mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)




########## Correct Borough NAs  ####THIS CODE SHOULD ALL BE CORRECT
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
unique(st_geometry_type(crossings_borough_NA_corrected)) # Linestring
crossings_borough_NA_corrected = crossings_borough_NA_corrected %>%
  st_cast("MULTILINESTRING") %>%
  mutate(BOROUGH = as.character(BOROUGH)) # and unfactor BOROUGH for step 6
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



###############################################################################
##############################
all_cycle_lanes_split = st_intersection(lon_lad_2020, f_cycle_lane_track) #25315
#nb original no of obs 24976
# so 339 extra obs
# BOROUGH is from ONS, BOROUGH.1 is from CID
cl_NA_b = all_cycle_lanes_split%>%
  filter(is.na(BOROUGH.1)) # = 621
cl_not_NA = all_cycle_lanes_split%>%
  filter(!is.na(BOROUGH.1)) # = 24694
anyNA(all_cycle_lanes_split$BOROUGH) # = FALSE - so all have a BOROUGH from ONS

#create smaller dataset Wandsworth according to CID
wandsworth = all_cycle_lanes_split %>%
  filter(BOROUGH.1 == "Wandsworth") # n= 974 
anyNA(wandsworth$BOROUGH.1) # = FALSE so all have Wandsworth from 

unique(wandsworth$BOROUGH.1) # so just wandsworth in the CID 
#[1] Wandsworth

unique(wandsworth$BOROUGH) # so even though CID labels as Wandsworth, ONS labels 
# as something else
#[1] Wandsworth           Merton              
#[3] Richmond upon Thames Lambeth             
#[5] Hammersmith & Fulham

wandsworth %>%
  group_by(BOROUGH) %>%
  count()  # counts how many 'Wandsworth' CID obs are labelled as other Boroughs by ONS data
# hammersmith 1
# lambeth     3
# merton      1
# Richmond    1
# wandworth     968

# check by comparing BOROUGH to BOROUGH.1
compare_pre = wandsworth$BOROUGH %in% wandsworth$BOROUGH.1
summary(compare_pre)
#   Mode   FALSE    TRUE 
# logical       6     968  # false fits with the 6 from above

# Need to factor BOROUGH.1 
borough_levels = c("Barking & Dagenham", "Barnet", "Bexley", "Brent",  
                   "Bromley", "Camden", "City of London", "Croydon", 
                   "Ealing", "Enfield", "Greenwich", "Hackney",  
                   "Hammersmith & Fulham", "Haringey", "Harrow", 
                   "Havering", "Hillingdon", "Hounslow", "Islington", 
                   "Kensington & Chelsea", "Kingston upon Thames",  
                   "Lambeth", "Lewisham", "Merton", "Newham", 
                   "Redbridge", "Richmond upon Thames", "Southwark",  
                   "Sutton", "Tower Hamlets", "Waltham Forest",   
                   "Wandsworth", "Westminster") 
wandsworth$BOROUGH.1 = factor(wandsworth$BOROUGH.1, levels = borough_levels)
levels(wandsworth$BOROUGH.1) # check has worked

# create loop to run through and recode 
for (i in seq_along(wandsworth$BOROUGH)) {
  wandsworth$BOROUGH.1[[i]] = wandsworth$BOROUGH[[i]]
}

wandsworth %>%
  group_by(BOROUGH.1) %>%
  count()  # counts to check we have the right number recoded 
# hammersmith 1
# lambeth     3
# merton      1
# Richmond    1
# wandsworth    968  YES IT WORKS!

compare_post = wandsworth$BOROUGH %in% wandsworth$BOROUGH.1
summary(compare_post) # all match

##############################################################################