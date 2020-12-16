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

boroughs <- st_read("./map_data/London_Borough_Excluding_MHW.shp")
boroughs = rename(boroughs, BOROUGH = NAME)
boroughs$BOROUGH = fct_recode(boroughs$BOROUGH, "Kensington & Chelsea" = "Kensington and Chelsea", 
                              "Barking & Dagenham" = "Barking and Dagenham",
                              "Hammersmith & Fulham" = "Hammersmith and Fulham")

mapview(borough_NA$geometry, color = "red") + mapview(boroughs, alpha.regions = 0.05, zcol = "BOROUGH") 
# Visual inspection shows that this ASL starts in Greenwich & finishes in Lewisham so code it as Greenwich 

# Count number of ASLs by Borough
count= f_advanced_stop_line %>%
  group_by(BOROUGH) %>%
  summarise(Count = n()) # 112 Greenwich, 1 NA

# Recode this NA as Greenwich and factor the BOROUGH variable
f_advanced_stop_line$BOROUGH = factor(f_advanced_stop_line$BOROUGH) %>%
  fct_explicit_na(na_level = "Greenwich")
anyNA(f_advanced_stop_line$BOROUGH) # = FALSE so no NAs

# Recount to check coded properly
recount= f_advanced_stop_line %>%
  group_by(BOROUGH) %>%
  summarise(Count = n()) # 113 Greenwich, no NA


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

mapview(borough_NA, color = "red")
mapview(boroughs, alpha.regions = 0.05) 
#create map of Crossings with NAs and label them with the feature ID
mNA = mapview(borough_NA, color = "red") + mapview(boroughs, alpha.regions = 0.05)
leafem::addStaticLabels(mNA, label = borough_NA$FEATURE_ID) # plot mNA with the FEATURE_IDs labelled so can identify


mapview(boroughs, alpha.regions = 0.05) 
# Visual inspection shows that this ASL starts in Greenwich & finishes in Lewisham so code it as Greenwich 
leaflet() %>%
  addTiles() %>%
  leafem::addStaticLabels(borough_NA$FEATURE_ID, colour = 'blue')

# changing individual values by row name and column name

myDataFrame["rowName", "columnName"] <- value
#replace all 

borough_NA[FEATURE_ID = "RWG236647", "BOROUGH"] = "Hillingdon"

borough_NA %>% mutate_at()

borough_NA$BOROUGH = replace(borough_NA$BOROUGH, 13, 'Hillingdon') # this code works to replace an NA value - need to do it so dont specify ros
borough_NA$BOROUGH = replace(borough_NA$BOROUGH, which(borough_NA$FEATURE_ID == "RWG236647"), values = 'Hillingdon')



#count Boroughs before transformation
count= f_crossings %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Count = n()) # 28 NA, 99 Hillingdon

# Code Borough NAs with Borough decided by researcher utilising unique FEATURE_ID to ensure correct obs is coded
f_crossings$BOROUGH = replace(f_crossings$BOROUGH, which(f_crossings$FEATURE_ID == "RWG236647"), values = 'Hillingdon')

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
