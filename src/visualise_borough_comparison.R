####################################################################################
# Visualise Borough level asset data 
####################################
# Date created: 11/1/21
#                                    
# This code takes the Borough level asset data by count and length and visualises
# the raw numbers and numbers per various denominators such as population, 
# borough area etc
#
####################################################################################

# Load packages
library(tidyverse)
library(mapview)


# Load datasets
# CID asset data
count_CID_by_borough = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/output/CID_count_by_borough")
length_CID_by_borough = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/output/CID_length_by_borough")

# b) ONS Borough area 
# import May 2020 ONS LA boundary data clipped to coastline
lon_lad_2020_c2c = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFC.Rds")
mapview(lon_lad_2020_c2c)

borough_area = lon_lad_2020_c2c %>%
  mutate(Borough_Area = (units::set_units(sf::st_area(geometry), km^2))) %>% # change area units to km^2 from m^2
  sf::st_drop_geometry() %>%
  select(c("BOROUGH", "Borough_Area"))

# c) Borough population
# ONS Mid year population estimates 2013-2019 
download.file("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2020localauthoritydistrictcodes/ukmidyearestimates20192020ladcodes.xls",
              "/home/bananafan/Downloads/ONS_pop_estimates")

ons_pop_estimates = readxl::read_excel("/home/bananafan/Downloads/ONS_pop_estimates", 
                       sheet = "MYE 5", skip = 3) # skip top few excel rows that arent relevent
lon_pop_estimates_2019 = ons_pop_estimates %>%
  filter(Geography1 == "London Borough") %>% # select London Boroughs
  select(c("Name", "Estimated Population mid-2019")) %>% # keep 2019 data only
  rename("BOROUGH" = "Name") %>% # rename to match CID
  rename("Population" = "Estimated Population mid-2019")

# rename values to match those names used in CID
lon_pop_estimates_2019$BOROUGH[lon_pop_estimates_2019$BOROUGH == "Kensington and Chelsea"] <- "Kensington & Chelsea"
lon_pop_estimates_2019$BOROUGH[lon_pop_estimates_2019$BOROUGH == "Barking and Dagenham"] <- "Barking & Dagenham"
lon_pop_estimates_2019$BOROUGH[lon_pop_estimates_2019$BOROUGH == "Hammersmith and Fulham"] <- "Hammersmith & Fulham"

             
###########################
# Working with count data #
###########################

# Join ???? to CID asset data
count_CID_by_borough = count_CID_by_borough %>%
  left_join(borough_area) %>%
  left_join(lon_pop_estimates_2019)


# produce counts by area and per head population (NB may want to consider per 100,000 pop at some point)
count_CID_by_borough_denom <- count_CID_by_borough %>%
  mutate(across(.cols = c("ASL", "Crossings", "CycleLanesAndTracks", "RestrictedRoutes", "CycleParking",
                          "Signals", "TrafficCalming", "Signage", "RestrictedPoints"),
                .fns = ~.x/Borough_Area,
                .names = "{.col}_count_by_area")) %>%
  mutate(across(.cols = c("ASL", "Crossings", "CycleLanesAndTracks", "RestrictedRoutes", "CycleParking",
                          "Signals", "TrafficCalming", "Signage", "RestrictedPoints"),
                .fns = ~.x/Population,
                .names = "{.col}_count_per_head"))  # this does work# this does work


# rank each borough
# use rank(-.x) to get ranking so that borough with most assets has highest ranking
count_CID_by_borough_rank = count_CID_by_borough_denom %>%
  mutate(across(.cols = c("ASL_count_by_area", "Crossings_count_by_area", 
                          "CycleLanesAndTracks_count_by_area", "RestrictedRoutes_count_by_area", 
                          "CycleParking_count_by_area", "Signals_count_by_area", 
                          "TrafficCalming_count_by_area", "Signage_count_by_area", 
                          "RestrictedPoints_count_by_area"),
                 .fns = ~round(rank(-.x)),                         
                 .names = "{.col}_rank")) %>%
  mutate(across(.cols = c("ASL_count_per_head", "Crossings_count_per_head", 
                          "CycleLanesAndTracks_count_per_head", "RestrictedRoutes_count_per_head", 
                          "CycleParking_count_per_head", "Signals_count_per_head", 
                          "TrafficCalming_count_per_head", "Signage_count_per_head", 
                          "RestrictedPoints_count_per_head"),
                .fns = ~round(rank(-.x)),
                .names = "{.col}_rank")) %>%
  mutate(across(.cols = c("ASL", "Crossings", 
                          "CycleLanesAndTracks", "RestrictedRoutes", 
                          "CycleParking", "Signals", 
                          "TrafficCalming", "Signage", 
                          "RestrictedPoints"),
                .fns = ~round(rank(-.x)), # rank with the highest ranking Borough having the most assets
                .names = "{.col}_count_rank")) 


# Validate ranking by comparing ranking with assets, used one example (ASL)
# boroughs with most assets given highest ranking
# a) raw counts
rc1 = ggplot(data = count_CID_by_borough_rank) +
  geom_bar(aes(x = ASL_count_rank, y = reorder(BOROUGH, -ASL_count_rank)), stat = "identity") # ranking 
#is fewest (rank 1 is 1 ASL) to most (34 Lambeth which has 336), so need to rank the other way
rc2 = ggplot(data = count_CID_by_borough_rank) +
  geom_bar(aes(x = ASL, y = reorder(BOROUGH, ASL)), stat = "identity") 
gridExtra::grid.arrange(rc1, rc2)

# b) count of asset by head of pop (? need to do per 100,000 for results table)
cph1 = ggplot(data = count_CID_by_borough_rank) +
  geom_bar(aes(x = ASL_count_per_head_rank, y = reorder(BOROUGH, -ASL_count_per_head_rank)), stat = "identity") # ranking 
#is fewest (rank 1 is 1 ASL) to most (34 Lambeth which has 336), so need to rank the other way
cph2 = ggplot(data = count_CID_by_borough_rank) +
  geom_bar(aes(x = ASL_count_per_head, y = reorder(BOROUGH, ASL_count_per_head)), stat = "identity") 
gridExtra::grid.arrange(cph1, cph2)

# c) count of asset by area (per km^2)
cba1 = ggplot(data = count_CID_by_borough_rank) +
  geom_bar(aes(x = ASL_count_by_area_rank, y = reorder(BOROUGH, -ASL_count_by_area_rank)), stat = "identity") # ranking 
#is fewest (rank 1 is 1 ASL) to most (34 Lambeth which has 336), so need to rank the other way
cba2 = ggplot(data = count_CID_by_borough_rank) +
  geom_bar(aes(x = as.numeric(ASL_count_by_area), y = reorder(BOROUGH, ASL_count_by_area)), stat = "identity") #convert area to numeric
gridExtra::grid.arrange(cba1, cba2)

# NEXT STEPS _ DRAW GRAPHS












#########################
# Work with length data #
#########################


length_CID_by_borough = length_CID_by_borough %>%
  left_join(borough_area) %>%
  left_join(lon_pop_estimates_2019)

#produce lengths by area and per head population 
length_CID_by_borough_denom <- length_CID_by_borough %>%
  mutate(across(.cols = c("CycleLaneTrack_km", "RestrictedRoute_km"),
                .fns = ~.x/Borough_Area,
                .names = "{.col}_length_by_area")) %>%
  mutate(across(.cols = c("CycleLaneTrack_km", "RestrictedRoute_km"),
                .fns = ~.x/Population,
                .names = "{.col}_length_per_head"))  # this does work


# rank each borough
# use rank(-.x) to get ranking so that borough with greatest length of assets has highest ranking
length_CID_by_borough_rank = length_CID_by_borough_denom %>%
  mutate(across(.cols = c("CycleLaneTrack_km_length_by_area", "RestrictedRoute_km_length_by_area"),
                .fns = ~round(rank(-.x)),                         
                .names = "{.col}_rank")) %>%
  mutate(across(.cols = c("CycleLaneTrack_km_length_per_head", "RestrictedRoute_km_length_per_head"),
                .fns = ~round(rank(-.x)),
                .names = "{.col}_rank")) %>%
  mutate(across(.cols = c("CycleLaneTrack_km", "RestrictedRoute_km"),
                .fns = ~round(rank(-.x)), # rank with the highest ranking Borough having the most assets
                .names = "{.col}_length_rank")) 


# Validate ranking by comparing ranking with assets, used one example (cycle lanes)
# boroughs with most length given highest ranking
# a) raw lengths
rl1 = ggplot(data = length_CID_by_borough_rank) +
  geom_bar(aes(x = as.numeric(CycleLaneTrack_km_length_rank), 
               y = reorder(BOROUGH, -CycleLaneTrack_km_length_rank)), stat = "identity") 
rl2 = ggplot(data = length_CID_by_borough_rank) +
  geom_bar(aes(x = as.numeric(CycleLaneTrack_km), 
               y = reorder(BOROUGH, CycleLaneTrack_km)), stat = "identity") 
gridExtra::grid.arrange(rl1, rl2)

# b) length of asset by head of pop (? need to do per 100,000 for results table)
lph1 = ggplot(data = length_CID_by_borough_rank) +
  geom_bar(aes(x = as.numeric(CycleLaneTrack_km_length_per_head_rank), 
               y = reorder(BOROUGH, -CycleLaneTrack_km_length_per_head_rank)), stat = "identity") 
lph2 = ggplot(data = length_CID_by_borough_rank) +
  geom_bar(aes(x = as.numeric(CycleLaneTrack_km_length_per_head), 
               y = reorder(BOROUGH, CycleLaneTrack_km_length_per_head)), stat = "identity") 
gridExtra::grid.arrange(lph1, lph2)

# c) length of asset by area (per km^2)
lba1 = ggplot(data = length_CID_by_borough_rank) +
  geom_bar(aes(x = as.numeric(CycleLaneTrack_km_length_by_area_rank),
                 y = reorder(BOROUGH, -CycleLaneTrack_km_length_by_area_rank)), stat = "identity")
lba2 = ggplot(data = length_CID_by_borough_rank) +
  geom_bar(aes(x = as.numeric(CycleLaneTrack_km_length_by_area),
               y = reorder(BOROUGH, -CycleLaneTrack_km_length_by_area)), stat = "identity")
gridExtra::grid.arrange(lba1, lba2)



