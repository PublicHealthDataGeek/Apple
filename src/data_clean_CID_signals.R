###################################################################################
# Data cleaning CID - Signals                                                     #
#                                                                                 #
# This code downloads the CID and cleans up the variables                         #
# It recodes the observations that has no Borough assigned                        #
# It also checks that the Boroughs are correctly assigned                         #
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
signal = st_transform(signal, crs=27700) 
st_crs(signal) # PROJCRS["OSGB 1936 / British National Grid",

###################################
# Check completeness of variables #
###################################
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

###############################################
# Tidy up variables and data - except BOROUGH #
###############################################
# Convert columns to factors 
### create list of columns to factor (Borough included as no NAs)
f_variables = c("SIG_HEAD", "SIG_SEPARA", "SIG_EARLY", "SIG_TWOSTG", "SIG_GATE", "BOROUGH")

### factor columns
f_signal = signal %>%
  mutate_at(f_variables, as.factor)

######################################
# Check to see if BOROUGH is correct #
######################################
# Visually inspect 
mapview(f_signal, zcol = "BOROUGH") + 
  mapview(lon_lad_2020, alpha.regions = 0.5, zcol = "BOROUGH",
          lwd = 1, legend = FALSE) # Visual inspection all appears to be fine

# Check use st_Contains to see if boroughs in CID are within ONS Boroughs 
# - outcome is that they are - see commented out appendix

######################
# SAVE CLEAN DATASET #
######################
saveRDS(f_signal, file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_signals")


# APPENDIX - checking that signals within specific Boroughs are within the same ONS borough
# 
# signal_boroughs = str_sort(as.character(unique(signal$BOROUGH))) # 23 Boroughs, no NAS
# str_sort(signal_boroughs)
# # [1] "Brent"                "Bromley"              "Camden"               "City of London"      
# # [5] "Croydon"              "Ealing"               "Enfield"              "Hackney"             
# # [9] "Hammersmith & Fulham" "Harrow"               "Hillingdon"           "Islington"           
# # [13] "Kensington & Chelsea" "Kingston upon Thames" "Lambeth"              "Lewisham"            
# # [17] "Merton"               "Redbridge"            "Southwark"            "Tower Hamlets"       
# # [21] "Waltham Forest"       "Wandsworth"           "Westminster"          
# 
# # Brent
# CID_bren = f_signal %>%
#   filter(BOROUGH == "Brent") # 1 observations
# ONS_bren = lon_lad_2020 %>%
#   filter(BOROUGH == "Brent")
# st_contains(ONS_bren, CID_bren, sparse = FALSE)  # all TRUE
# 
# # Bromley
# CID_brom = f_signal %>%
#   filter(BOROUGH == "Bromley") # 2 observations
# ONS_brom = lon_lad_2020 %>%
#   filter(BOROUGH == "Bromley")
# st_contains(ONS_brom, CID_brom, sparse = FALSE)  # all TRUE
# 
# # Camden
# CID_camden = f_signal %>%
#   filter(BOROUGH == "Camden") # 36 observations
# ONS_camden = lon_lad_2020 %>%
#   filter(BOROUGH == "Camden")
# st_contains(ONS_camden, CID_camden, sparse = FALSE)  # all TRUE
# 
# # City of London
# CID_city = f_signal %>%
#   filter(BOROUGH == "City of London") # 58 observations
# ONS_city = lon_lad_2020 %>%
#   filter(BOROUGH == "City of London")
# st_contains(ONS_city, CID_city, sparse = FALSE)  # all TRUE
# 
# # Croydon
# CID_croy = f_signal %>%
#   filter(BOROUGH == "Croydon") # 2 observations
# ONS_croy = lon_lad_2020 %>%
#   filter(BOROUGH == "Croydon")
# st_contains(ONS_croy, CID_croy, sparse = FALSE)  # all TRUE
# 
# # Ealing
# CID_eal = f_signal %>%
#   filter(BOROUGH == "Ealing") # 1 observations
# ONS_eal = lon_lad_2020 %>%
#   filter(BOROUGH == "Ealing")
# st_contains(ONS_eal, CID_eal, sparse = FALSE)  # all TRUE
# 
# # Enfield
# CID_enf = f_signal %>%
#   filter(BOROUGH == "Enfield") # 5 observations
# ONS_enf = lon_lad_2020 %>%
#   filter(BOROUGH == "Enfield")
# st_contains(ONS_enf, CID_enf, sparse = FALSE)  # all TRUE
# 
# # "Hackney"  
# CID_hac = f_signal %>%
#   filter(BOROUGH == "Hackney") # 25 observations
# ONS_hac = lon_lad_2020 %>%
#   filter(BOROUGH == "Hackney")
# st_contains(ONS_hac, CID_hac, sparse = FALSE)  # all TRUE
# 
# # Hammersmith & Fulham
# CID_hf = f_signal %>%
#   filter(BOROUGH == "Hammersmith & Fulham") # 3 observations
# ONS_hf = lon_lad_2020 %>%
#   filter(BOROUGH == "Hammersmith & Fulham")
# st_contains(ONS_hf, CID_hf, sparse = FALSE)  # all TRUE
# 
# # Harrow
# CID_har = f_signal %>%
#   filter(BOROUGH == "Harrow") # 6 observations
# ONS_har = lon_lad_2020 %>%
#   filter(BOROUGH == "Harrow")
# st_contains(ONS_har, CID_har, sparse = FALSE)  # all TRUE
# 
# # Hillingdon
# CID_hil = f_signal %>%
#   filter(BOROUGH == "Hillingdon") # 4 observations
# ONS_hil = lon_lad_2020 %>%
#   filter(BOROUGH == "Hillingdon")
# st_contains(ONS_hil, CID_hil, sparse = FALSE)  # all TRUE
# 
# # Islington  
# CID_isl = f_signal %>%
#   filter(BOROUGH == "Islington") # 16 observations
# ONS_isl = lon_lad_2020 %>%
#   filter(BOROUGH == "Islington")
# st_contains(ONS_isl, CID_isl, sparse = FALSE)  # all TRUE
# 
# # Kensington & Chelsea
# CID_ken = f_signal %>%
#   filter(BOROUGH == "Kensington & Chelsea") # 5 observations
# ONS_ken = lon_lad_2020 %>%
#   filter(BOROUGH == "Kensington & Chelsea")
# st_contains(ONS_ken, CID_ken, sparse = FALSE)  # all TRUE
# 
# # Kingston upon Thames
# CID_king = f_signal %>%
#   filter(BOROUGH == "Kingston upon Thames") # 4 observations
# ONS_king= lon_lad_2020 %>%
#   filter(BOROUGH == "Kingston upon Thames")
# st_contains(ONS_king, CID_king, sparse = FALSE)  # all TRUE
# 
# # Lambeth
# CID_lam = f_signal %>%
#   filter(BOROUGH == "Lambeth") # 44 observations
# ONS_lam = lon_lad_2020 %>%
#   filter(BOROUGH == "Lambeth")
# st_contains(ONS_lam, CID_lam, sparse = FALSE)  # all TRUE
# 
# # Lewisham
# CID_lew = f_signal %>%
#   filter(BOROUGH == "Lewisham") # 1 observations
# ONS_lew = lon_lad_2020 %>%
#   filter(BOROUGH == "Lewisham")
# st_contains(ONS_lew, CID_lew, sparse = FALSE)  # all TRUE
# 
# # Merton
# CID_mer = f_signal %>%
#   filter(BOROUGH == "Merton") # 2 observations
# ONS_mer = lon_lad_2020 %>%
#   filter(BOROUGH == "Merton")
# st_contains(ONS_mer, CID_mer, sparse = FALSE)  # all TRUE
# 
# # Redbridge
# CID_red = f_signal %>%
#   filter(BOROUGH == "Redbridge") # 1 observations
# ONS_red = lon_lad_2020 %>%
#   filter(BOROUGH == "Redbridge")
# st_contains(ONS_red, CID_red, sparse = FALSE)  # all TRUE
# 
# # Southwark
# CID_south = f_signal %>%
#   filter(BOROUGH == "Southwark") # 44 observations
# ONS_south = lon_lad_2020 %>%
#   filter(BOROUGH == "Southwark")
# st_contains(ONS_south, CID_south, sparse = FALSE)  # all TRUE
# 
# # Tower Hamlets
# CID_tower = f_signal %>%
#   filter(BOROUGH == "Tower Hamlets") # 59 observations
# ONS_tower = lon_lad_2020 %>%
#   filter(BOROUGH == "Tower Hamlets")
# st_contains(ONS_tower, CID_tower, sparse = FALSE)  # all TRUE
# 
# # Waltham Forest
# CID_walt = f_signal %>%
#   filter(BOROUGH == "Waltham Forest") # 11 observations
# ONS_walt = lon_lad_2020 %>%
#   filter(BOROUGH == "Waltham Forest")
# st_contains(ONS_walt, CID_walt, sparse = FALSE)  # all TRUE
# 
# # Wandsworth
# CID_wan = f_signal %>%
#   filter(BOROUGH == "Wandsworth") # 17 observations
# ONS_wan = lon_lad_2020 %>%
#   filter(BOROUGH == "Wandsworth")
# st_contains(ONS_wan, CID_wan, sparse = FALSE)  # all TRUE
# 
# # "Westminster"  
# CID_wes = f_signal %>%
#   filter(BOROUGH == "Westminster") # 96 observations
# ONS_wes = lon_lad_2020 %>%
#   filter(BOROUGH == "Westminster")
# st_contains(ONS_wes, CID_wes, sparse = FALSE)  # all TRUE
