################################################################################
#                                                                              #
#                     Summary statistics for Paper 1                          #
#                                 Part 2                                       #
#                     ##############################                           #
#                                                                              #
# This code generates the summary statistics used in Paper 1                   #
# 1) Detailed examination of variables for each of the 5 safety datasets



# load packages
library(tidyverse)
library(sf)
library(summarytools)
library(units)

# load datasets
# These datasets were downloaded from TFL 25th February 2021 and data cleansed
c_asl = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_asl")
c_crossings = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_crossings")
c_cyclelanetrack = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_cycle_lane_track")
c_signals = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_signals")
c_trafficcalming = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_trafficcalming")


################################################################################
#                     
#                             Crossings (n = 1690)
#
################################################################################

# CRS_SIGNAL    1. FALSE                        434 (25.7%)                                
# [factor]      2. TRUE                         1256 (74.3%)            
# 
# CRS_SEGREG    1. FALSE                        1372 (81.2%)          
# [factor]      2. TRUE                          318 (18.8%)          
# 
# CRS_CYGAP     1. FALSE                        1566 (92.7%)          
# [factor]      2. TRUE                          124 ( 7.3%)          

# CRS_PEDEST    1. FALSE                        1648 (97.5%)        
# [factor]      2. TRUE                           42 ( 2.5%)          
# 
# CRS_LEVEL     1. FALSE                        1670 (98.8%)        
# [factor]      2. TRUE                           20 ( 1.2%)       

# Are these unique signals or do some signals have more than one characteristic? 
print(ctable(x = c_crossings$CRS_SIGNAL, y = c_crossings$CRS_SEGREG))
print(ctable(x = c_crossings$CRS_SIGNAL, y = c_crossings$CRS_CYGAP))
# some definately have more than one characteristic so investigate below

# # Convert Factors to numeric (converts all False to 1 and True to 2)
# # e.g.  CLT_SEGREG: Factor w/ 2 levels "FALSE","TRUE": 1 1 1 1 1 1 1 1 1 1 ...
crossings_numeric = c_crossings %>%
  mutate(CRS_SIGNAL_NUMERIC = as.numeric(c_crossings$CRS_SIGNAL))  %>%
  mutate(CRS_SEGREG_NUMERIC = as.numeric(c_crossings$CRS_SEGREG))  %>%
  mutate(CRS_CYGAP_NUMERIC = as.numeric(c_crossings$CRS_CYGAP))  %>%
  mutate(CRS_PEDEST_NUMERIC = as.numeric(c_crossings$CRS_PEDEST))  %>%
  mutate(CRS_LEVEL_NUMERIC = as.numeric(c_crossings$CRS_LEVEL))

# Convert 1(false) to 0 and 2(true) to 1
crossings_numeric$CRS_SIGNAL_NUMERIC = ifelse(crossings_numeric$CRS_SIGNAL_NUMERIC == 1, 0, 1)
crossings_numeric$CRS_SEGREG_NUMERIC = ifelse(crossings_numeric$CRS_SEGREG_NUMERIC == 1, 0, 1)
crossings_numeric$CRS_CYGAP_NUMERIC = ifelse(crossings_numeric$CRS_CYGAP_NUMERIC == 1, 0, 1)
crossings_numeric$CRS_PEDEST_NUMERIC = ifelse(crossings_numeric$CRS_PEDEST_NUMERIC == 1, 0, 1)
crossings_numeric$CRS_LEVEL_NUMERIC = ifelse(crossings_numeric$CRS_LEVEL_NUMERIC == 1, 0, 1)

# # Check now gives the count that I expect  -YES THEY DO
# sum(crossings_numeric$CRS_SIGNAL_NUMERIC) # n = 1256
# sum(crossings_numeric$CRS_SEGREG_NUMERIC) # n = 318
# sum(crossings_numeric$CRS_CYGAP_NUMERIC) # n = 124
# sum(crossings_numeric$CRS_PEDEST_NUMERIC) # n = 42
# sum(crossings_numeric$CRS_LEVEL_NUMERIC) # n = 20


# Recode to give weighted value with so can distinguish between different characteristics
crossings_numeric$CRS_SIGNAL_weight = ifelse(crossings_numeric$CRS_SIGNAL_NUMERIC == 1, 10000, 0)
crossings_numeric$CRS_SEGREG_weight = ifelse(crossings_numeric$CRS_SEGREG_NUMERIC == 1, 1000, 0)
crossings_numeric$CRS_CYGAP_weight = ifelse(crossings_numeric$CRS_CYGAP_NUMERIC == 1, 100, 0)
crossings_numeric$CRS_PEDEST_weight = ifelse(crossings_numeric$CRS_PEDEST_NUMERIC == 1, 10, 0)
crossings_numeric$CRS_LEVEL_weight = ifelse(crossings_numeric$CRS_LEVEL_NUMERIC == 1, 1, 0)

# Create new column with the sum of the weights for the 5 classes of separation
crossings_numeric = crossings_numeric %>%
  rowwise() %>%
  mutate(weight_5 = sum(c_across(CRS_SIGNAL_weight:CRS_LEVEL_weight)))

crossings_characteristics = crossings_numeric %>%
  st_drop_geometry() %>%
  group_by(weight_5) %>%
  summarise(count = n())
sum(crossings_characteristics$count) # n = 1690
#     weight_5 count
# 1         0   200  Not signal controlled, not segre, no gap, not ped only, no level crossing
# 2         1    15 Just level cross
# 3        10    16 Just pedestrian only
# 4        11     5  Level crossing and pedestrian only
# 5       100    14  Just cycle gao
# 6      1000   116  Just segreg
# 7      1100    68  Segreg with gap
# 8     10000  1100  Just signal controlled
# 9     10010    21  Signal controlled by pedestrial only crossing
# 10    10100     1  Signal controlled with gap
# 11    11000    93  Signal controlled with segregation
# 12    11100    41  Signal controlled with segreg and gap
