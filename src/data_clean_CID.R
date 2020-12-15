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
#library(leafsync)
library(summarytools)
library(forcats)
library(geojsonsf)

#########################################
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
                "ASL_SHARED", "ASL_COLOUR", "BOROUGH")

# convert columns to factors (, CLT_ACCESS not converted as 721 different values)
f_advanced_stop_line = advanced_stop_line %>%
  mutate_at(f_variables, as.factor)
anyNA(f_advanced_stop_line$BOROUGH) # = TRUE

# identify missing Borough details
borough_NA = x %>%
  filter(is.na(BOROUGH))

boroughs <- st_read("./map_data/London_Borough_Excluding_MHW.shp")
boroughs = rename(boroughs, BOROUGH = NAME)
boroughs$BOROUGH = fct_recode(boroughs$BOROUGH, "Kensington & Chelsea" = "Kensington and Chelsea", 
                              "Barking & Dagenham" = "Barking and Dagenham",
                              "Hammersmith & Fulham" = "Hammersmith and Fulham")

mapview(borough_NA$geometry, color = "red") + mapview(boroughs, alpha.regions = 0.05, zcol = "BOROUGH") 
# The ASL starts in Greenwich & finishes in Lewsiham so code it as Greenwich 


# Attempting to get the NA changed!!!  really struggling - may have to do before factoring? 
f_advanced_stop_line$BOROUGH %>% replace_na(Greenwich)
anyNA(f_advanced_stop_line$BOROUGH)
unique(f_advanced_stop_line$BOROUGH)
#mutate(BOROUGH = replace_na(BOROUGH, "Greenwich"))

fct_explicit_na(x$BOROUGH, na_level = "Greenwich")
anyNA(x$BOROUGH)
unique(x$BOROUGH)


glimpse(f_advanced_stop_line) # check converted ok
levels(f_advanced_stop_line$BOROUGH) # check have 34 (33 actual boroughs plus 1 NA value)



# create new df without geommetry that enables faster analysis of data
non_geom_f_advanced_stop_line = st_drop_geometry(f_advanced_stop_line)
str(non_geom_f_advanced_stop_line)
count_borough = non_geom_f_advanced_stop_line %>%
  count(BOROUGH)  # => 1 NA  NB will need to add Borough to it at some point


# create summary of df
view(dfSummary(non_geom_f_advanced_stop_line))


