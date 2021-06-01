###################################################################################
# Data cleaning CID - crossings                                                   #
#                                                                                 #
# This code downloads the CID and cleans up the variables                         #
# It recodes the observations that have no Borough assigned (n = 28)              #
# It also checks that the Boroughs are correctly assigned where they exist        #
# 9 were identified that may not be correctly assigned but only 2 needing         #
# correcting.  One of the two needed splitting into two observations              #
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

# Set Mapview options to use data CRS rather than OSM projections
mapviewOptions(native.crs = TRUE)

# import May 2020 ONS LA boundary data (required for NA management)
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

# download CID data using the Cycle Infra Lnd package
crossings = get_cid_lines(type = "crossing") # n = 1687

# Convert CRS so matches ONS boundary data CRS
crossings = st_transform(crossings, crs=27700) 
st_crs(crossings) # PROJCRS["OSGB 1936 / British National Grid",

###################################
# Check completeness of variables #
###################################

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

###############################################
# Tidy up variables and data - except BOROUGH #
###############################################

# Convert certain columns to factors
## Create list of columns to factor
f_v = c("CRS_SIGNAL", "CRS_SEGREG", "CRS_CYGAP", "CRS_PEDEST", "CRS_LEVEL")

## Factor columns 
# (BOROUGH needs doing separately as it has some NAs in, CLT_ACCESS not converted as 721 different values)
f_crossings = crossings %>%
  mutate_at(f_v, as.factor)

##########################################################################
# Create dataset where each observation is a single crossing             #
# (as 265 crossing observations actually contain more than one crossing) #
##########################################################################

# Examine original 1687 obs
unique(st_geometry_type(f_crossings)) # Multilinestring
sum(st_length(f_crossings)) # = 19907.02 [m]

# convert multilinestrings to linestrings
crossings_ls = f_crossings %>%
  st_cast("LINESTRING")
unique(st_geometry_type(crossings_ls)) # Linestring, 1987 observations
sum(st_length(crossings_ls)) # = 19907.02 [m] # check length is the same

crossings_ls %>%
  st_drop_geometry() %>%
  group_by(FEATURE_ID) %>%
  summarise(number_LS = n()) %>%
  group_by(number_LS) %>%
  summarise(obs = n())
#   number_LS   obs
# 1         1  1422  ie 1422 observations just contained 1 crossing
# 2         2   238  238 observations contain 2 crossings
# 3         3    19  19 contain 3 crossings
# 4         4     8  8 contain 4 crossings
# TOTAL SUM = 1987

# Create new IDs for these crossings so each crossing can be identified
# Group by FEATURE_ID then add a cumulative sum to each grouped observation (will be 1, 2. 3 or 4)
crossings_ls_corrected = crossings_ls %>%
  mutate(n = 1) %>%
  group_by(FEATURE_ID) %>%
  mutate(count = cumsum(n)) %>%
  select(-n)

## Create new FEATURE_ID with the cumulative number so each observation has a unique ID
crossings_ls_corrected$FEATURE_ID_crossings =
  paste0(crossings_ls_corrected$FEATURE_ID, "_", crossings_ls_corrected$count)

## Check that the correct number should have 1 or 2 appended to the FEATURE_ID
crossings_ls_corrected %>%
  st_drop_geometry() %>%
  group_by(count) %>%
  count()
# count     n
# 1     1  1687  1687 crossings labelled as 1 (1422 + 238 + 19 + 8)
# 2     2   265  265 have 2 (238 + 19 + 8)
# 3     3    27  27 have 3 (19 + 8)
# 4     4     8  8 have 4 


#  BELOW CODE WAS TO PROVE THE THINKING WAS CORRECT
# mls_crossings = crossings_ls %>%
#   group_by(FEATURE_ID) %>%
#   summarise(number_LS = n()) %>%
#   filter(number_LS > 1) %>%
#   ungroup() # n = 265 observations that contain 2 or more crossings (238 + 19 + 8)
# 
# m1 = mapview(f_crossings) 
# m2 = mapview(mls_crossings)
# leafsync::sync(m1,m2)
# 
# #get list of crossings that have more than one crossing
# multi_crossing_id_list = pull(mls_crossings, FEATURE_ID)
# 
# # create dataset containing the observations with these FEATURE_IDs
# # add column with number of crossings per feature ID
# crossings_ls = crossings_ls %>%
#   group_by(FEATURE_ID) %>%
#   mutate(number_crossings = n()) %>%
#   ungroup()
# multi_crossing_crossings = crossings_ls %>%
#   filter(FEATURE_ID %in% multi_crossing_id_list) # n = 565  
# # 565 = 2x238 + 3*19 + 4*8

# # Examine a few
# examples = c("RWG278949", "RWG278932", "RWG278979", "RWG278980")
# example_multicross = multi_crossing_crossings %>%
#   filter(FEATURE_ID %in% examples)
# mapview(example_multicross, zcol = "FEATURE_ID")
# mapview(example_multicross, zcol = "number_crossings")




###################
# Tidy up BOROUGH #
###################

anyNA(crossings_ls_corrected$BOROUGH) # = TRUE, n = 29

# Create dataset for those with a Borough coded 
crossings_borough = crossings_ls_corrected %>%
  filter(!is.na(BOROUGH)) # 1958 observations (ie 1987 original obs - 29 NAs) 

crossings_borough_split = st_intersection(lon_lad_2020, crossings_borough) # n = 1967 ie 9 extra

anyNA(crossings_borough_split$BOROUGH) # FALSE  # ONS data
anyNA(crossings_borough_split$BOROUGH.1) # FALSE # CID data

# Create df of crossings where the ONS borough disagrees with the CID borough
multi_feature_id = crossings_borough_split %>%
  st_drop_geometry() %>%
  group_by(FEATURE_ID_crossings) %>%
  summarise(num_obs = n())%>%
  group_by(num_obs) %>%
  filter(num_obs == 2) %>%  # number is only 1 or 2
  ungroup() # n = 9
#   FEATURE_ID_crossings num_obs
# 1 RWG055875_1                2
# 2 RWG107384_1                2
# 3 RWG108304_1                2
# 4 RWG135327_1                2
# 5 RWG135346_1                2
# 6 RWG153062_1                2
# 7 RWG154478_1                2
# 8 RWG154502_1                2
# 9 RWG273925_2                2

# check to see if any of these may have been part of a multicrossing to start with
# (important as post split some might be entirely in a new borough but not picked up
# because current code only identifies where new segments have been created by split)
multi_crossings_check = as.data.frame(pull(multi_feature_id, FEATURE_ID_crossings))
colnames(multi_crossings_check) = "FEATURE_ID_crossings"
multi_crossings_check$FEATURE_ID = sub("_.*", "", multi_crossings_check$FEATURE_ID_crossings)
multi_crossings_check_df = left_join(multi_crossings_check, crossings_ls_corrected, by = "FEATURE_ID")

# the above code identifies these two observations as originally having more than one crossing 
# so some of their crossings may also be affected by incorrect Borough even if not split.                                      
#RWG108304
#RWG273925

# Check the location of these multicrossing original observations
RWG273925_check = crossings_ls_corrected %>%
  filter(FEATURE_ID =="RWG273925")
mapview(test, zcol = "FEATURE_ID_crossings") + mapview(lon_lad_2020, alpha.regions = 0)
# RWG273925_4 defo in H&F

RWG108304_check = crossings_ls_corrected %>%
  filter(FEATURE_ID =="RWG108304")
mapview(RWG108304_check, zcol = "FEATURE_ID_crossings") + mapview(lon_lad_2020, alpha.regions = 0)
# ?RWG108304_1 city
#  ?RWG108304_2 TH




                                      
# create list of FEATURE_IDs that have 2 segments
multi_feature_id_list = pull(multi_feature_id, FEATURE_ID_crossings)

# create dataset containing the observations with these FEATURE_IDs
new_segments = crossings_borough_split %>%
  filter(FEATURE_ID_crossings %in% multi_feature_id_list) # n = 18, the 9 original ones 
# plus the additional 9 created when they were split

mapview(new_segments, zcol = "BOROUGH") + 
  mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)

new_segment_match_pre = new_segments %>%
  st_drop_geometry() %>%
  select(c("FEATURE_ID_crossings", "BOROUGH", "BOROUGH.1")) %>%
  mutate(match = ifelse(BOROUGH == BOROUGH.1, TRUE, FALSE)) %>%
  rename(c("ONS" = "BOROUGH", "CID" = "BOROUGH.1"))  # Yes - this what we see

# Detailed inspection using maps ( see commented out code below)
# Only FEATURE_IDs where the Boroughs needs changing are:
# 1) RWG108304 - this is in Tower Hamlets 
crossings_ls_corrected$BOROUGH = replace(crossings_ls_corrected$BOROUGH, 
                                 which(crossings_ls_corrected$FEATURE_ID == "RWG108304"), 
                                 values = "Tower Hamlets")

# 2) RWG273925_2  - this is in 
crossings_ls_corrected$BOROUGH = replace(crossings_ls_corrected$BOROUGH, 
                                         which(crossings_ls_corrected$FEATURE_ID_crossings == "RWG273925_2"), 
                                         values = "Hammersmith & Fulham")




# 2) RWG273925 - there are 4 crossings in this observation, 3 are in Ealing and 1 
# is in Hammersmith and Fullham.  SPlit into two observation
## select RWG273925 degments
RWG273925 = new_segments %>%
  filter(FEATURE_ID == "RWG273925")
## Factor the BOROUGH.1 variable
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
RWG273925$BOROUGH.1 = factor(RWG273925$BOROUGH.1, levels = borough_levels)

## Run loop to run through and recode BOROUGH.1 (CID) with BOROUGH (ONS)
for (i in seq_along(RWG273925$BOROUGH)) {
  RWG273925$BOROUGH.1[[i]] = RWG273925$BOROUGH[[i]]
}

# Create unique FEATURE_IDs
RWG273925$FEATURE_ID = replace(RWG273925$FEATURE_ID, which(RWG273925$FEATURE_ID == "RWG273925"), 
                               values = c("RWG273925_1", "RWG273925_2"))

# # Create df RWG273925 correct Boroughs that can be joined
RWG273925_corrected = RWG273925 %>%
  select(c("FEATURE_ID", "SVDATE", "CRS_SIGNAL", "CRS_SEGREG", "CRS_CYGAP", 
           "CRS_PEDEST", "CRS_LEVEL", "BOROUGH", 
           "PHOTO1_URL", "PHOTO2_URL", "geometry")) %>%
  mutate(BOROUGH = as.character(BOROUGH)) # change borough to character so can match back to f_crossings

# Remove and then add the observations
f_crossings = f_crossings %>%
  filter(!FEATURE_ID == "RWG273925") # n = 1686 ie 1 removed from the 1687 
# Join RWG273925_corrected to f_crossings - will then have 1688 ie 2 more
f_crossings = rbind(f_crossings, RWG273925_corrected) # n= 1688


### CODE for checking each crossing segment 
# new_segment_1 = new_segments %>%
#   filter(FEATURE_ID_crossings == multi_feature_id_list[1])
# mapview(new_segment_1, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# new_segment_1, RWG055875 = K&C
# 
# new_segment_2 = new_segments %>%
#   filter(FEATURE_ID_crossings == multi_feature_id_list[2])
# mapview(new_segment_2, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# # new_segment_2, RWG107384 = Southwark
# 
# new_segment_3 = new_segments %>%
#   filter(FEATURE_ID_crossings == multi_feature_id_list[3])
# mapview(new_segment_3, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# new_segment_3, RWG108304 = Tower Hamlets
# 
# new_segment_4 = new_segments %>%
#   filter(FEATURE_ID_crossings == multi_feature_id_list[4])
# mapview(new_segment_4, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# # new_segment_4, RWG135327 = SOuthwark
# 
# new_segment_5 = new_segments %>%
#   filter(FEATURE_ID_crossings == multi_feature_id_list[5])
# mapview(new_segment_5, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# # new_segment_5, RRWG135346 = Wandsworth
# 
# new_segment_6 = new_segments %>%
#   filter(FEATURE_ID_crossings == multi_feature_id_list[6])
# mapview(new_segment_6, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# # new_segment_6, RWG153062 = Camden
# 
# new_segment_7 = new_segments %>%
#   filter(FEATURE_ID_crossings == multi_feature_id_list[7])
# mapview(new_segment_7, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# # new_segment_7, RWG154478 = Newham
# 
# new_segment_8 = new_segments %>%
#   filter(FEATURE_ID_crossings == multi_feature_id_list[8])
# mapview(new_segment_8, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# new_segment_8, RWG154502 = Newham
# 
# new_segment_9 = new_segments %>%
#   filter(FEATURE_ID_crossings == multi_feature_id_list[9])
# mapview(new_segment_9, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# new_segment_9,m
# RWG273925_2 = Hammersmith and Fullham

#######################
# Correct Borough NAs #
#######################
# # Correct NAs as per correct_borough_NAs working
# #  1) Create dataset of 28 crossings that have no Borough
# crossings_borough_NA = f_crossings %>%
#   filter(is.na(BOROUGH)) 
# 
# #  2) Use st_intersection to produce two observations for the crossings
# crossings_borough_NA_i = st_intersection(lon_lad_2020, crossings_borough_NA) 
# # 49 observations, geometry column is from the crossings dataset but split by ONS borough boundaries
# # BOROUGH is the column from the lon_lad_2020 dataset and represents the Borough that the crossing is in, 
# # whereas BOROUGH.1 is the BOROUGH from the crossing dataset (all NAs)
# 
# mapview(crossings_borough_NA_i, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.05, zcol = "BOROUGH")
# #this map shows the crossings coloured by the BOROUGH they are in
# 
# # 3) Add column for length of each observation (using crossing geometry)by BOROUGH and keep obs with longest length
# crossings_borough_NA_i$borough_length = st_length(crossings_borough_NA_i) 
# 
# # 4) Create final dataset that gives the proposed Borough for each observation
# # 4a) create congruent dataset where visual check and st_length check matches (n = 26)
# congruent = crossings_borough_NA_i %>%
#   filter(!FEATURE_ID %in% c("RWG153061", "RWG049417")) #n = 45 (4 obs with these FEATURE_IDs removed)
# 
# congruent = congruent %>% 
#   group_by(FEATURE_ID) %>%
#   slice(which.max(borough_length)) # keep the observation which has the longest length - n = 26
# 
# # 4b) create incongruent dataset where visual check and st_length check does not match and relabel FEATURE_IDs
# # ie for observations "RWG153061", "RWG049417" 
# # recode Feature_ID with 'a' and 'b'
# # crosschecked that a/b corresponds with the right borough
# incongruent = crossings_borough_NA_i %>%
#   filter(FEATURE_ID %in% c("RWG153061", "RWG049417")) %>%
#   group_by(FEATURE_ID) %>%
#   arrange(desc(borough_length)) %>%
#   ungroup() #arrange order of data by grouping by FEATURE_ID then descending length before relabelling FEATURE_ID in subsequent lines
# incongruent$FEATURE_ID = replace(incongruent$FEATURE_ID, which(incongruent$FEATURE_ID == "RWG153061"), 
#                                  values = c("RWG153061_1", "RWG153061_2"))
# incongruent$FEATURE_ID = replace(incongruent$FEATURE_ID, which(incongruent$FEATURE_ID == "RWG049417"), 
#                                  values = c("RWG049417_1", "RWG049417_2"))
# 
# # 5) Create df of all the observations that were Borough NA with correct Boroughs
# # and convert to MLS so has same geometry type as crossings
# # 5a) create df of corrected Boroughs with columns that match Crossings dataset
# crossings_borough_NA_corrected = rbind(congruent, incongruent) %>%
#   select(c("FEATURE_ID","SVDATE","CRS_SIGNAL", "CRS_SEGREG", "CRS_CYGAP", 
#            "CRS_PEDEST", "CRS_LEVEL", "BOROUGH", "PHOTO1_URL", "PHOTO2_URL",
#            "geometry")) 
# 
# 
# # 5b) To validate that have corrected NAs, create df of counted Boroughs before transformation
# count_crossings_borough = f_crossings %>%
#   st_drop_geometry() %>%
#   group_by(BOROUGH) %>%
#   summarise(Count = n()) 
# 
# # 5c) change geometry to multilinestring
# # cast geometry
# unique(st_geometry_type(crossings_borough_NA_corrected)) # Linestring
# crossings_borough_NA_corrected = crossings_borough_NA_corrected %>%
#   st_cast("MULTILINESTRING") %>%
#   mutate(BOROUGH = as.character(BOROUGH)) # and unfactor BOROUGH for step 6
# 
# # 6) Add observations with correct Boroughs to main Crossing dataset
# # 6a) Drop 28 observations with no boroughs from f_crossings dataset
# f_crossings = f_crossings %>%
#   filter(!is.na(BOROUGH)) # 1660 observations ie  the 1688 - 28 NAs
# anyNA(f_crossings$BOROUGH) # = FALSE ie all dropped
# 
# # 6b) join corrected observations to the f_crossings
# f_crossings = rbind(f_crossings, crossings_borough_NA_corrected) 
# anyNA(f_crossings$BOROUGH) # = FALSE
# 
# # 7) Validate have correctly 
# # 7a) Recount Boroughs after transformation
# recount_crossings_borough = f_crossings %>%  
#   st_drop_geometry() %>%
#   group_by(BOROUGH) %>%
#   summarise(Recount = n()) # 
# 
# number_recoded = crossings_borough_NA_corrected %>%  
#   st_drop_geometry() %>%
#   group_by(BOROUGH) %>%
#   summarise(No_recoded = n())
# 
# # join borough counts together to check done correctly
# crossing_boroughs = left_join(count_crossings_borough, number_recoded) %>%
#   left_join(recount_crossings_borough) # This looks to be correct
# x = crossing_boroughs %>%
#   replace_na(list(Count = 0, No_recoded = 0, Recount = 0))
# total <- sapply(x[,2:4], sum) # this gives figures of:
# # 1688 total count
# # 30 observation recoded (26 plus the 2+2 observations that had the same FEATURE_ID)
# 
# # new total number of observations in the crossings dataset of 1690 - 
# ## ie the original 1688 that now have the correct Borough (1 extra because RWG273925 
# # was split) plus the extra 2 observations that have the FEATURE_ID_2 codes

######################
# SAVE CLEAN DATASET #
######################
saveRDS(f_crossings, file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_crossings")







##################################################
#  COPY OF CODE IN CASE I MESS UP!
# 
# 
# ###################
# # Tidy up BOROUGH #
# ###################
# 
# anyNA(f_crossings$BOROUGH) # = TRUE
# 
# # Create dataset for those with a Borough coded 
# crossings_borough = f_crossings %>%
#   filter(!is.na(BOROUGH)) # 1659 observations (ie 1687 original obs - 28 NAs) 
# 
# crossings_borough_split = st_intersection(lon_lad_2020, crossings_borough) # n = 1668 ie 9 extra
# 
# anyNA(crossings_borough_split$BOROUGH) # FALSE  # ONS data
# anyNA(crossings_borough_split$BOROUGH.1) # FALSE # CID data
# 
# # Create df of crossings...
# multi_feature_id = crossings_borough_split %>%
#   st_drop_geometry() %>%
#   group_by(FEATURE_ID) %>%
#   summarise(num_obs = n())%>%
#   group_by(num_obs) %>%
#   filter(num_obs == 2) %>%  # number is only 1 or 2
#   ungroup() # n = 9
# # FEATURE_ID num_obs
# # 1 RWG055875        2
# # 2 RWG107384        2
# # 3 RWG108304        2
# # 4 RWG135327        2
# # 5 RWG135346        2
# # 6 RWG153062        2
# # 7 RWG154478        2
# # 8 RWG154502        2
# # 9 RWG273925        2
# 
# # create list of FEATURE_IDs that have 2 segments
# multi_feature_id_list = pull(multi_feature_id, FEATURE_ID)
# 
# # create dataset containing the observations with these FEATURE_IDs
# new_segments = crossings_borough_split %>%
#   filter(FEATURE_ID %in% multi_feature_id_list) # n = 18, the 9 original ones 
# # plus the additional 9 created when they were split
# 
# mapview(new_segments, zcol = "BOROUGH") + 
#   mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# 
# new_segment_match_pre = new_segments %>%
#   st_drop_geometry() %>%
#   select(c("FEATURE_ID", "BOROUGH", "BOROUGH.1")) %>%
#   mutate(match = ifelse(BOROUGH == BOROUGH.1, TRUE, FALSE)) %>%
#   rename(c("ONS" = "BOROUGH", "CID" = "BOROUGH.1"))  # Yes - this what we see
# 
# # Detailed inspection using maps
# # Only FEATURE_IDs where the Boroughs needs changing are:
# # 1) RWG108304 - this is in Tower Hamlets 
# f_crossings$BOROUGH = replace(f_crossings$BOROUGH, 
#                               which(f_crossings$FEATURE_ID == "RWG108304"), 
#                               values = "Tower Hamlets")
# 
# # 2) RWG273925 - there are 4 crossings in this observation, 3 are in Ealing and 1 
# # is in Hammersmith and Fullham.  SPlit into two observation
# ## select RWG273925 degments
# RWG273925 = new_segments %>%
#   filter(FEATURE_ID == "RWG273925")
# ## Factor the BOROUGH.1 variable
# borough_levels = c("Barking & Dagenham", "Barnet", "Bexley", "Brent",  
#                    "Bromley", "Camden", "City of London", "Croydon", 
#                    "Ealing", "Enfield", "Greenwich", "Hackney",  
#                    "Hammersmith & Fulham", "Haringey", "Harrow", 
#                    "Havering", "Hillingdon", "Hounslow", "Islington", 
#                    "Kensington & Chelsea", "Kingston upon Thames",  
#                    "Lambeth", "Lewisham", "Merton", "Newham", 
#                    "Redbridge", "Richmond upon Thames", "Southwark",  
#                    "Sutton", "Tower Hamlets", "Waltham Forest",   
#                    "Wandsworth", "Westminster") 
# RWG273925$BOROUGH.1 = factor(RWG273925$BOROUGH.1, levels = borough_levels)
# 
# ## Run loop to run through and recode BOROUGH.1 (CID) with BOROUGH (ONS)
# for (i in seq_along(RWG273925$BOROUGH)) {
#   RWG273925$BOROUGH.1[[i]] = RWG273925$BOROUGH[[i]]
# }
# 
# # Create unique FEATURE_IDs
# RWG273925$FEATURE_ID = replace(RWG273925$FEATURE_ID, which(RWG273925$FEATURE_ID == "RWG273925"), 
#                                values = c("RWG273925_1", "RWG273925_2"))
# 
# # # Create df RWG273925 correct Boroughs that can be joined
# RWG273925_corrected = RWG273925 %>%
#   select(c("FEATURE_ID", "SVDATE", "CRS_SIGNAL", "CRS_SEGREG", "CRS_CYGAP", 
#            "CRS_PEDEST", "CRS_LEVEL", "BOROUGH", 
#            "PHOTO1_URL", "PHOTO2_URL", "geometry")) %>%
#   mutate(BOROUGH = as.character(BOROUGH)) # change borough to character so can match back to f_crossings
# 
# # Remove and then add the observations
# f_crossings = f_crossings %>%
#   filter(!FEATURE_ID == "RWG273925") # n = 1686 ie 1 removed from the 1687 
# # Join RWG273925_corrected to f_crossings - will then have 1688 ie 2 more
# f_crossings = rbind(f_crossings, RWG273925_corrected) # n= 1688
# 
# 
# 
# new_segment_1 = new_segments %>%
#   filter(FEATURE_ID == multi_feature_id_list[1])
# mapview(new_segment_1, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# # new_segment_1, RWG055875 = K&C
# # 
# # new_segment_2 = new_segments %>%
# #   filter(FEATURE_ID == multi_feature_id_list[2])
# # mapview(new_segment_2, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# # # new_segment_2, RWG107384 = Southwark
# # 
# # new_segment_3 = new_segments %>%
# #   filter(FEATURE_ID == multi_feature_id_list[3])
# # mapview(new_segment_3, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# # # new_segment_3, RWG108304 = Tower Hamlets
# # 
# # new_segment_4 = new_segments %>%
# #   filter(FEATURE_ID == multi_feature_id_list[4])
# # mapview(new_segment_4, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# # # new_segment_4, RWG135327 = SOuthwark
# # 
# # new_segment_5 = new_segments %>%
# #   filter(FEATURE_ID == multi_feature_id_list[5])
# # mapview(new_segment_5, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# # # new_segment_5, RRWG135346 = Wandsworth
# # 
# # new_segment_6 = new_segments %>%
# #   filter(FEATURE_ID == multi_feature_id_list[6])
# # mapview(new_segment_6, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# # # new_segment_6, RWG153062 = Camden
# # 
# # new_segment_7 = new_segments %>%
# #   filter(FEATURE_ID == multi_feature_id_list[7])
# # mapview(new_segment_7, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# # # new_segment_7, RWG154478 = Newham
# # 
# # new_segment_8 = new_segments %>%
# #   filter(FEATURE_ID == multi_feature_id_list[8])
# # mapview(new_segment_8, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# # # new_segment_8, RWG154502 = Newham
# # 
# # new_segment_9 = new_segments %>%
# #   filter(FEATURE_ID == multi_feature_id_list[9])
# # mapview(new_segment_9, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# # # new_segment_9,m
# # # RWG273925 = 3 are Ealing, 1 is Hammersmith and Fullham
# 
# #######################
# # Correct Borough NAs #
# #######################
# # Correct NAs as per correct_borough_NAs working
# #  1) Create dataset of 28 crossings that have no Borough
# crossings_borough_NA = f_crossings %>%
#   filter(is.na(BOROUGH)) 
# 
# #  2) Use st_intersection to produce two observations for the crossings
# crossings_borough_NA_i = st_intersection(lon_lad_2020, crossings_borough_NA) 
# # 49 observations, geometry column is from the crossings dataset but split by ONS borough boundaries
# # BOROUGH is the column from the lon_lad_2020 dataset and represents the Borough that the crossing is in, 
# # whereas BOROUGH.1 is the BOROUGH from the crossing dataset (all NAs)
# 
# mapview(crossings_borough_NA_i, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.05, zcol = "BOROUGH")
# #this map shows the crossings coloured by the BOROUGH they are in
# 
# # 3) Add column for length of each observation (using crossing geometry)by BOROUGH and keep obs with longest length
# crossings_borough_NA_i$borough_length = st_length(crossings_borough_NA_i) 
# 
# # 4) Create final dataset that gives the proposed Borough for each observation
# # 4a) create congruent dataset where visual check and st_length check matches (n = 26)
# congruent = crossings_borough_NA_i %>%
#   filter(!FEATURE_ID %in% c("RWG153061", "RWG049417")) #n = 45 (4 obs with these FEATURE_IDs removed)
# 
# congruent = congruent %>% 
#   group_by(FEATURE_ID) %>%
#   slice(which.max(borough_length)) # keep the observation which has the longest length - n = 26
# 
# # 4b) create incongruent dataset where visual check and st_length check does not match and relabel FEATURE_IDs
# # ie for observations "RWG153061", "RWG049417" 
# # recode Feature_ID with 'a' and 'b'
# # crosschecked that a/b corresponds with the right borough
# incongruent = crossings_borough_NA_i %>%
#   filter(FEATURE_ID %in% c("RWG153061", "RWG049417")) %>%
#   group_by(FEATURE_ID) %>%
#   arrange(desc(borough_length)) %>%
#   ungroup() #arrange order of data by grouping by FEATURE_ID then descending length before relabelling FEATURE_ID in subsequent lines
# incongruent$FEATURE_ID = replace(incongruent$FEATURE_ID, which(incongruent$FEATURE_ID == "RWG153061"), 
#                                  values = c("RWG153061_1", "RWG153061_2"))
# incongruent$FEATURE_ID = replace(incongruent$FEATURE_ID, which(incongruent$FEATURE_ID == "RWG049417"), 
#                                  values = c("RWG049417_1", "RWG049417_2"))
# 
# # 5) Create df of all the observations that were Borough NA with correct Boroughs
# # and convert to MLS so has same geometry type as crossings
# # 5a) create df of corrected Boroughs with columns that match Crossings dataset
# crossings_borough_NA_corrected = rbind(congruent, incongruent) %>%
#   select(c("FEATURE_ID","SVDATE","CRS_SIGNAL", "CRS_SEGREG", "CRS_CYGAP", 
#            "CRS_PEDEST", "CRS_LEVEL", "BOROUGH", "PHOTO1_URL", "PHOTO2_URL",
#            "geometry")) 
# 
# 
# # 5b) To validate that have corrected NAs, create df of counted Boroughs before transformation
# count_crossings_borough = f_crossings %>%
#   st_drop_geometry() %>%
#   group_by(BOROUGH) %>%
#   summarise(Count = n()) 
# 
# # 5c) change geometry to multilinestring
# # cast geometry
# unique(st_geometry_type(crossings_borough_NA_corrected)) # Linestring
# crossings_borough_NA_corrected = crossings_borough_NA_corrected %>%
#   st_cast("MULTILINESTRING") %>%
#   mutate(BOROUGH = as.character(BOROUGH)) # and unfactor BOROUGH for step 6
# 
# # 6) Add observations with correct Boroughs to main Crossing dataset
# # 6a) Drop 28 observations with no boroughs from f_crossings dataset
# f_crossings = f_crossings %>%
#   filter(!is.na(BOROUGH)) # 1660 observations ie  the 1688 - 28 NAs
# anyNA(f_crossings$BOROUGH) # = FALSE ie all dropped
# 
# # 6b) join corrected observations to the f_crossings
# f_crossings = rbind(f_crossings, crossings_borough_NA_corrected) 
# anyNA(f_crossings$BOROUGH) # = FALSE
# 
# # 7) Validate have correctly 
# # 7a) Recount Boroughs after transformation
# recount_crossings_borough = f_crossings %>%  
#   st_drop_geometry() %>%
#   group_by(BOROUGH) %>%
#   summarise(Recount = n()) # 
# 
# number_recoded = crossings_borough_NA_corrected %>%  
#   st_drop_geometry() %>%
#   group_by(BOROUGH) %>%
#   summarise(No_recoded = n())
# 
# # join borough counts together to check done correctly
# crossing_boroughs = left_join(count_crossings_borough, number_recoded) %>%
#   left_join(recount_crossings_borough) # This looks to be correct
# x = crossing_boroughs %>%
#   replace_na(list(Count = 0, No_recoded = 0, Recount = 0))
# total <- sapply(x[,2:4], sum) # this gives figures of:
# # 1688 total count
# # 30 observation recoded (26 plus the 2+2 observations that had the same FEATURE_ID)
# 
# # new total number of observations in the crossings dataset of 1690 - 
# ## ie the original 1688 that now have the correct Borough (1 extra because RWG273925 
# # was split) plus the extra 2 observations that have the FEATURE_ID_2 codes





