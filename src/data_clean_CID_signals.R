###################################################################################
# Data cleaning CID - Signals                                                     #
#                                                                                 #
# This code downloads the CID and cleans up the variables                         #
# It recodes the observations that has no Borough assigned                        #
# It also checks that the Boroughs are correctly assigned where they exist        #

                    #
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

# set mapview options so that matches crs
mapviewOptions(native.crs = TRUE)

# import May 2020 ONS LA boundary data (required for NA management)
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

# download CID data using the Cycle Infra Lnd package
signal = get_cid_points(type = "signal") #n = 443


# Convert CRS so matches ONS boundary data CRS
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

mapviewOptions(native.crs = TRUE)
mapview(f_signal, zcol = "BOROUGH") + 
  mapview(lon_lad_2020, alpha.regions = 0.5, zcol = "BOROUGH",
          lwd = 1, legend = FALSE) # Visual inspection all appears to be fine

CID_enfield = f_signal %>%
  filter(BOROUGH == "Enfield") # 5 observations
enfield_ONS = lon_lad_2020 %>%
  filter(BOROUGH == "Enfield")
st_contains(CID_enfield$geometry, enfield_ONS$geometry)
st_geometry_type(CID_enfield)
st_geometry_type(enfield_ONS)

st_contains(CID_enfield, enfield_ONS, sparse = FALSE)
st_contains(enfield_ONS, CID_enfield, sparse = FALSE) # this works
x = st_contains(lon_lad_2020, f_signal, sparse = FALSE) # this works

for (i in 1:nrow(lon_lad_2020)) {
  assign(paste("ddat",i,sep="_"), data)
}
####? do I want a list of boroughs as a vector then use for loop for each borough, check against
# dataset tjem dp st_contains
test_borough = c("Enfield", "City of London")

for (i in test_borough) {
  st_contains(lon_lad_2020, f_signal, sparse = FALSE)
}

for (i in seq_along(test_borough)) {
  st_contains(lon_lad_2020, f_signal, sparse = FALSE)
}

## https://discdown.org/rprogramming/loops.html

for (i in test_borough) {
  print(paste("contains", i))
}
# [1] "contains Enfield"
# [1] "contains City of London"

boroughs = c("Barking & Dagenham", "Barnet", "Bexley", "Brent",  
             "Bromley", "Camden", "City of London", "Croydon", 
             "Ealing", "Enfield", "Greenwich", "Hackney",  
             "Hammersmith & Fulham", "Haringey", "Harrow", 
             "Havering", "Hillingdon", "Hounslow", "Islington", 
             "Kensington & Chelsea", "Kingston upon Thames",  
             "Lambeth", "Lewisham", "Merton", "Newham", 
             "Redbridge", "Richmond upon Thames", "Southwark",  
             "Sutton", "Tower Hamlets", "Waltham Forest",   
             "Wandsworth", "Westminster") 



st_contains(lon_lad_2020[i], f_signal[i], sparse = FALSE)
# doesnt work



for (i in seq_along(lon_lad_2020$BOROUGH)) {
  if (st_contains(lon_lad_2020[i], f_signal[i])) {
    print("signal is in ONS")
  } else{
    print("FALSE")
  }
}

if (st_contains(f_signal$geometry, lon_lad_2020$geometry)) {
  print("signal is in ONS")
} else{
  print("FALSE")
}
}

