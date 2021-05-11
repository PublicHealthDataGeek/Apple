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


###################
# Tidy up BOROUGH #
###################

anyNA(f_crossings$BOROUGH) # = TRUE

# Create dataset for those with a Borough coded 
crossings_borough = f_crossings %>%
  filter(!is.na(BOROUGH)) # 1659 observations (ie 1687 original obs - 28 NAs) 

crossings_borough_split = st_intersection(lon_lad_2020, crossings_borough) # n = 1668 ie 9 extra

anyNA(crossings_borough_split$BOROUGH) # FALSE  # ONS data
anyNA(crossings_borough_split$BOROUGH.1) # FALSE # CID data

# Create df of crossings...
multi_feature_id = crossings_borough_split %>%
  st_drop_geometry() %>%
  group_by(FEATURE_ID) %>%
  summarise(num_obs = n())%>%
  group_by(num_obs) %>%
  filter(num_obs == 2) %>%  # number is only 1 or 2
  ungroup() # n = 9
# FEATURE_ID num_obs
# 1 RWG055875        2
# 2 RWG107384        2
# 3 RWG108304        2
# 4 RWG135327        2
# 5 RWG135346        2
# 6 RWG153062        2
# 7 RWG154478        2
# 8 RWG154502        2
# 9 RWG273925        2

# create list of FEATURE_IDs that have 2 segments
multi_feature_id_list = pull(multi_feature_id, FEATURE_ID)

# create dataset containing the observations with these FEATURE_IDs
new_segments = crossings_borough_split %>%
  filter(FEATURE_ID %in% multi_feature_id_list) # n = 18, the 9 original ones 
# plus the additional 9 created when they were split

mapview(new_segments, zcol = "BOROUGH") + 
  mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)

new_segment_match_pre = new_segments %>%
  st_drop_geometry() %>%
  select(c("FEATURE_ID", "BOROUGH", "BOROUGH.1")) %>%
  mutate(match = ifelse(BOROUGH == BOROUGH.1, TRUE, FALSE)) %>%
  rename(c("ONS" = "BOROUGH", "CID" = "BOROUGH.1"))  # Yes - this what we see

# Detailed inspection using maps
# Only FEATURE_IDs where the Boroughs needs changing are:
# 1) RWG108304 - this is in Tower Hamlets 
f_crossings$BOROUGH = replace(f_crossings$BOROUGH, 
                                 which(f_crossings$FEATURE_ID == "RWG108304"), 
                              values = "Tower Hamlets")

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



new_segment_1 = new_segments %>%
  filter(FEATURE_ID == multi_feature_id_list[1])
mapview(new_segment_1, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# new_segment_1, RWG055875 = K&C
# 
# new_segment_2 = new_segments %>%
#   filter(FEATURE_ID == multi_feature_id_list[2])
# mapview(new_segment_2, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# # new_segment_2, RWG107384 = Southwark
# 
# new_segment_3 = new_segments %>%
#   filter(FEATURE_ID == multi_feature_id_list[3])
# mapview(new_segment_3, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# # new_segment_3, RWG108304 = Tower Hamlets
# 
# new_segment_4 = new_segments %>%
#   filter(FEATURE_ID == multi_feature_id_list[4])
# mapview(new_segment_4, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# # new_segment_4, RWG135327 = SOuthwark
# 
# new_segment_5 = new_segments %>%
#   filter(FEATURE_ID == multi_feature_id_list[5])
# mapview(new_segment_5, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# # new_segment_5, RRWG135346 = Wandsworth
# 
# new_segment_6 = new_segments %>%
#   filter(FEATURE_ID == multi_feature_id_list[6])
# mapview(new_segment_6, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# # new_segment_6, RWG153062 = Camden
# 
# new_segment_7 = new_segments %>%
#   filter(FEATURE_ID == multi_feature_id_list[7])
# mapview(new_segment_7, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# # new_segment_7, RWG154478 = Newham
# 
# new_segment_8 = new_segments %>%
#   filter(FEATURE_ID == multi_feature_id_list[8])
# mapview(new_segment_8, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# # new_segment_8, RWG154502 = Newham
# 
# new_segment_9 = new_segments %>%
#   filter(FEATURE_ID == multi_feature_id_list[9])
# mapview(new_segment_9, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
# # new_segment_9,m
# # RWG273925 = 3 are Ealing, 1 is Hammersmith and Fullham

#######################
# Correct Borough NAs #
#######################
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
incongruent$FEATURE_ID = replace(incongruent$FEATURE_ID, which(incongruent$FEATURE_ID == "RWG153061"), 
                                 values = c("RWG153061_1", "RWG153061_2"))
incongruent$FEATURE_ID = replace(incongruent$FEATURE_ID, which(incongruent$FEATURE_ID == "RWG049417"), 
                                 values = c("RWG049417_1", "RWG049417_2"))

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
  filter(!is.na(BOROUGH)) # 1660 observations ie  the 1688 - 28 NAs
anyNA(f_crossings$BOROUGH) # = FALSE ie all dropped

# 6b) join corrected observations to the f_crossings
f_crossings = rbind(f_crossings, crossings_borough_NA_corrected) 
anyNA(f_crossings$BOROUGH) # = FALSE

# 7) Validate have correctly 
# 7a) Recount Boroughs after transformation
recount_crossings_borough = f_crossings %>%  
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Recount = n()) # 

number_recoded = crossings_borough_NA_corrected %>%  
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(No_recoded = n())

# join borough counts together to check done correctly
crossing_boroughs = left_join(count_crossings_borough, number_recoded) %>%
  left_join(recount_crossings_borough) # This looks to be correct
x = crossing_boroughs %>%
  replace_na(list(Count = 0, No_recoded = 0, Recount = 0))
total <- sapply(x[,2:4], sum) # this gives figures of:
# 1688 total count
# 30 observation recoded (26 plus the 2+2 observations that had the same FEATURE_ID)

# new total number of observations in the crossings dataset of 1690 - 
## ie the original 1688 that now have the correct Borough (1 extra because RWG273925 
# was split) plus the extra 2 observations that have the FEATURE_ID_2 codes

######################
# SAVE CLEAN DATASET #
######################
saveRDS(f_crossings, file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_crossings")
