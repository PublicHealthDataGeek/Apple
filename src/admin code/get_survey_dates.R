##############################################################################################
# Checking dates of surveying of CID assets                                                  #
#                                                                                            #
# Date created 17/3/21                                                                       #
#                                                                                            #
# This code extracts the dates of survey for each asset and groups by year to then determine #
# which version of OSM to use                                                                #
#                                                                                            #
##############################################################################################

# load packages
library(tidyverse)
library(lubridate)

# Load datasets
c_asl = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_asl")
c_crossings = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_crossings")
c_cyclelanetrack = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_cycle_lane_track")
c_Rroutes = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_restricted_route")
c_Rpoints = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_restrictedpoints")
c_parking = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_parking")
c_signage = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_signage")
c_signals = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_signals")
c_trafficcalming = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_trafficcalming")

# Create summary tables of year by asset type
asl_year = c_asl %>%
  st_drop_geometry() %>%
  select(SVDATE) %>%
  mutate(year = year(SVDATE)) %>%
  group_by(year) %>%
  summarise(ASL = n())

crossings_year = c_crossings %>%
  st_drop_geometry() %>%
  select(SVDATE) %>%
  mutate(year = year(SVDATE)) %>%
  group_by(year) %>%
  summarise(Crossings = n())

clt_year = c_cyclelanetrack %>%
  st_drop_geometry() %>%
  select(SVDATE) %>%
  mutate(year = year(SVDATE)) %>%
  group_by(year) %>%
  summarise(CycleLanesTracks = n())

rr_year = c_Rroutes %>%
  st_drop_geometry() %>%
  select(SVDATE) %>%
  mutate(year = year(SVDATE)) %>%
  group_by(year) %>%
  summarise(Rroutes = n())

rp_year = c_Rpoints %>%
  st_drop_geometry() %>%
  select(SVDATE) %>%
  mutate(year = year(SVDATE)) %>%
  group_by(year) %>%
  summarise(Rpoints = n())

cp_year = c_parking %>%
  st_drop_geometry() %>%
  select(SVDATE) %>%
  mutate(year = year(SVDATE)) %>%
  group_by(year) %>%
  summarise(CycleParking = n())

signage_year = c_signage %>%
  st_drop_geometry() %>%
  select(SVDATE) %>%
  mutate(year = year(SVDATE)) %>%
  group_by(year) %>%
  summarise(Signage = n())

signals_year = c_signals %>%
  st_drop_geometry() %>%
  select(SVDATE) %>%
  mutate(year = year(SVDATE)) %>%
  group_by(year) %>%
  summarise(Signals = n())

tc_year = c_trafficcalming %>%
  st_drop_geometry() %>%
  select(SVDATE) %>%
  mutate(year = year(SVDATE)) %>%
  group_by(year) %>%
  summarise(TrafficCalming = n())

# Join all survey year data 
survey_year = full_join(asl_year, crossings_year) %>%
  full_join(clt_year) %>%
  full_join(rr_year) %>%
  full_join(rp_year) %>%
  full_join(signage_year) %>%
  full_join(signals_year) %>%
  full_join(cp_year) %>%
  full_join(tc_year) %>%
  mutate_all(funs(replace_na(., 0))) # convert NAs to 0

survey_year$Total = rowSums(survey_year[,c(-1)]) # Calculate totals for each survey year

# Save data 
saveRDS(survey_year, file = "/home/bananafan/Documents/PhD/Paper1/output/survey_year_table")
