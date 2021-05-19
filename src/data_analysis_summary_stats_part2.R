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
#                             ASL (n = 3775)
#
################################################################################
c_asl %>%
  st_drop_geometry() %>%
  summarytools::dfSummary()

# ASL_FDR       1. FALSE                        1992 (52.8%)               
# [factor]      2. TRUE                         1783 (47.2%)        
# 
# ASL_FDRLFT    1. FALSE                        2080 (55.1%)              
# [factor]      2. TRUE                         1695 (44.9%)          
# 
# ASL_FDCENT    1. FALSE                        3697 (97.9%)                
# [factor]      2. TRUE                           78 ( 2.1%)               
# 
# ASL_FDRIGH    1. FALSE                        3748 (99.3%)                
# [factor]      2. TRUE                           27 ( 0.7%)                    
# 
# ASL_SHARED    1. FALSE                        3768 (99.8%)                 
# [factor]      2. TRUE                            7 ( 0.2%)                            
# 
# ASL_COLOUR    1. BLUE                           85 ( 2.3%)                                
# [factor]      2. BUFF/YELLOW                    21 ( 0.6%)                                   
#               3. GREEN                         889 (23.5%)                                              
#               4. NONE                         2713 (71.9%)                                   
#               5. OTHER                           4 ( 0.1%)                                                    
#               6. RED                            63 ( 1.7%)                                                    
       

# Are these unique signals or do some signals have more than one characteristic? 
print(ctable(x = c_asl$ASL_FDR, y = c_asl$ASL_FDRLFT))

# some definately have more than one characteristic so investigate below

# # Convert Factors to numeric (converts all False to 1 and True to 2)

asl_numeric = c_asl %>%
  mutate(ASL_FDR_NUMERIC = as.numeric(c_asl$ASL_FDR)) %>%
  mutate(ASL_FDRLFT_NUMERIC = as.numeric(c_asl$ASL_FDRLFT)) %>%
  mutate(ASL_FDCENT_NUMERIC = as.numeric(c_asl$ASL_FDCENT)) %>%
  mutate(ASL_FDRIGH_NUMERIC = as.numeric(c_asl$ASL_FDRIGH)) %>%
  mutate(ASL_SHARED_NUMERIC = as.numeric(c_asl$ASL_SHARED)) %>%
  mutate(ASL_COLOUR_NUMERIC = as.numeric(c_asl$ASL_COLOUR))

# Convert 1(false) to 0 and 2(true) to 1
asl_numeric$ASL_FDR_NUMERIC = ifelse(asl_numeric$ASL_FDR_NUMERIC == 1, 0, 1)
asl_numeric$ASL_FDRLFT_NUMERIC = ifelse(asl_numeric$ASL_FDRLFT_NUMERIC == 1, 0, 1)
asl_numeric$ASL_FDCENT_NUMERIC = ifelse(asl_numeric$ASL_FDCENT_NUMERIC == 1, 0, 1)
asl_numeric$ASL_FDRIGH_NUMERIC = ifelse(asl_numeric$ASL_FDRIGH_NUMERIC == 1, 0, 1)
asl_numeric$ASL_SHARED_NUMERIC = ifelse(asl_numeric$ASL_SHARED_NUMERIC == 1, 0, 1)
asl_numeric$ASL_COLOUR_NUMERIC = ifelse(asl_numeric$ASL_COLOUR_NUMERIC == 4, 0, 1) # if coded 4 (no colour) then 0 so coloured = 1

# # # Check now gives the count that I expect (sum will count all the ones)  -YES THEY DO
# sum(asl_numeric$ASL_FDR_NUMERIC) # n = 1783
# sum(asl_numeric$ASL_FDRLFT_NUMERIC) # n = 1695
# sum(asl_numeric$ASL_FDCENT_NUMERIC) # n = 78
# sum(asl_numeric$ASL_FDRIGH_NUMERIC) # n = 27
# sum(asl_numeric$ASL_SHARED_NUMERIC) # n = 7
# sum(asl_numeric$ASL_COLOUR_NUMERIC) # n = 1062

# # Recode to give weighted value with so can distinguish between different characteristics
asl_numeric$ASL_FDR__weight = ifelse(asl_numeric$ASL_FDR_NUMERIC == 1, 10000, 0)
asl_numeric$ASL_FDRLFT_weight = ifelse(asl_numeric$ASL_FDRLFT_NUMERIC == 1, 1000, 0)
asl_numeric$ASL_FDCENT_weight = ifelse(asl_numeric$ASL_FDCENT_NUMERIC == 1, 100, 0)
asl_numeric$ASL_FDRIGH_weight = ifelse(asl_numeric$ASL_FDRIGH_NUMERIC == 1, 10, 0)
asl_numeric$ASL_SHARED_weight = ifelse(asl_numeric$ASL_SHARED_NUMERIC == 1, 1, 0)
asl_numeric$ASL_COLOUR_weight = ifelse(asl_numeric$ASL_COLOUR_NUMERIC == 1, 5, 0)

# # Create new column with the sum of the weights for the 5 classes of separation
asl_numeric = asl_numeric %>%
  rowwise() %>%
  mutate(weight_5 = sum(c_across(ASL_FDR__weight:ASL_COLOUR_weight)))

asl_characteristics = asl_numeric %>%
  st_drop_geometry() %>%
  group_by(weight_5) %>%
  summarise(count = n())
sum(asl_characteristics$count) # n = 3775
   # weight_5 count
#  1        0  1568 No characteristics
#  2        1     2 Shared
#  3        5   405 coloured
#  4        6     5 coloured and shared
#  5     1000    11 feeder left
#  6     1005     1 feeder left and coloured
#  7    10000     1 feeder present
#  8    10005     1 feeder present & coloured
#  9    10010    15 feeder right
# 10    10015     8 feeder right and coloured
# 11    10100    45 feeder present & in centre
# 12    10105    30 feeder present, in centre and coloured
# 13    11000  1067 feeder present & on left
# 14    11005   609 feeder prsent, on left & coloured
# 15    11010     3 feeder present, left and right
# 16    11015     1 feeder present, left, right and coloured
# 17    11100     1 feeder present, on left & centre
# 18    11105     2 feeder present, on left& centre, and coloured





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
