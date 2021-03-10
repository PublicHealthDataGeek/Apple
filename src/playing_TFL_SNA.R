#########################
# Playing with TFL Strategic Neighbourhood Analysis data #
# 10th March 2020
####

library(tidyverse)
library(sf)
library(mapview)

# set mapview options so that matches crs
mapviewOptions(native.crs = TRUE, legend = FALSE)

# Import TFL SNA
tfl_sna_raw <- st_read("/home/bananafan/Downloads/SNA V1 PLAYBOOK_region.shp")
# Simple feature collection with 2573 features and 17 fields
# geometry type:  MULTIPOLYGON
# dimension:      XYZ
# projected CRS:  OSGB 1936 / British National Grid # same as used for rest of analysis

names(tfl_sna_raw)
# [1] "Neighbourh" "Borough"    "Area_sqkm"  "Population" "PC_Househo" "Modelled_t" "Walk_cycle"
# [8] "Modelled_c" "Average_fo" "Populati00" "NumSchool2" "Highest_de" "PC_Populat" "PC_Popul00"
# [15] "Filtering_" "General_sc" "LSP.bid"    "geometry"  

str(tfl_sna_raw)
# Classes ‘sf’ and 'data.frame':	2573 obs. of  18 variables:
#   $ Neighbourh: num  1 2 3 4 5 6 7 9 10 12 ...
# $ Borough   : chr  "Hillingdon" "Hillingdon" "Hillingdon" "Hillingdon" ...
# $ Area_sqkm : num  10.45 0.03 0.43 0.15 0.06 ...
# $ Population: num  1417 11 2224 976 26 ...
# $ PC_Househo: num  0.219 0 0 0 0 0 0.386 0 0 0 ...
# $ Modelled_t: chr  "500+ vehicles" "No Data" "200-500 vehicles" "Less than 200 vehicles" ...
# $ Walk_cycle: num  0 0 0 0 0 0 3 1 0 0 ...
# $ Modelled_c: num  0.229 0.068 0.107 0.05 0 0.04 0.241 0.282 0 0.061 ...
# $ Average_fo: num  9.46 3.89 7.06 5.92 3.42 ...
# $ Populati00: num  136 367 5172 6507 433 ...
# $ NumSchool2: num  0 0 0 0 0 0 1 0 2 0 ...
# $ Highest_de: num  4 0 0 0 0 0 3 0 0 0 ...
# $ PC_Populat: num  0.024 0 0.059 0 0 0 0.126 0 0.059 0 ...
# $ PC_Popul00: num  0.008 0 0.026 0 0 0 0.046 0 0.061 0 ...
# $ Filtering_: num  3 0 2 1 0 0 2 3 1 0 ...
# $ General_sc: num  0 2 0 0 2 2 1 0 1 0 ...
# $ LSP.bid   : int  NA NA NA NA NA NA NA NA NA NA ...
# $ geometry  :sfc_MULTIPOLYGON of length 2573;

mapview(tfl_sna_raw$geometry)
str(tfl_sna_raw$geometry)
plot(tfl_sna_raw)
plot(tfl_sna_raw$geometry)
plot(tfl_sna_raw$Neighbourh)
plot(tfl_sna_raw$Borough)
unique(st_geometry_type(tfl_sna_raw$geometry))

# a) Neighbourhood number - unique
unique(tfl_sna_raw$Neighbourh) # n = 2573

#b) Borough details - character
unique(tfl_sna_raw$Borough) # 33 plus NA
borough_count = tfl_sna_raw %>%
  st_drop_geometry() %>%
  group_by(Borough) %>%
  summarise(Count= n()) #  11 NA

# c) Size in kilometers not including open space, railway land and industrial land
unique(tfl_sna_raw$Area_sqkm) # n = 212 plus NA
summary(tfl_sna_raw$Area_sqkm)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.0100  0.0400  0.1300  0.3235  0.3800 10.4500      11 

# d) Estimated population (2018)
summary(tfl_sna_raw$Population) 
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#     1.0   548.5  1693.5  3471.0  4687.0 62770.0      11

# e) Proportion of residents who do not have household access to car
summary(tfl_sna_raw$PC_Househo) 
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.0000  0.0000  0.1520  0.2216  0.4210  0.8150      11 

# f) Estimate of through traffic based on TFLs strategic highway models (character)
unique(tfl_sna_raw$Modelled_t)
# [1] "500+ vehicles"          "No Data"                "200-500 vehicles"      
# [4] "Less than 200 vehicles" NA   

# g) Estimate of road danger based on pedestrian and cycle casualties 
summary(tfl_sna_raw$Walk_cycle)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.000   0.000   1.000   2.884   3.000 118.000      11 

# h) Cycle connectivity - cycle flow potential 
summary(tfl_sna_raw$Modelled_c)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.0000  0.1180  0.4050  0.4158  0.6870  1.0000      11 

# i) Average pavement width
summary(tfl_sna_raw$Average_fo)
# Min.    1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.000   4.550   5.440   5.468   6.490  31.440      11 

# j) Population density (2018) people per square kilometre
summary(tfl_sna_raw$Populati00)
# Min.    1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#      33    7810   11880   14593   18722  124500      11 

# k) Access to schools - Number of schools 2020
summary(tfl_sna_raw$NumSchool2)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.000   0.000   0.000   1.016   1.000  17.000      11 

# l) Highest level of deprivation within a neighourhood
summary(tfl_sna_raw$Highest_de)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.000   0.000   2.000   2.369   4.000  10.000      11 

# m) Proportion of population aged 0-17
summary(tfl_sna_raw$PC_Populat)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00000 0.04000 0.10850 0.09953 0.15400 0.31000      11 

# n) ?????Proportion of population aged 70+
summary(tfl_sna_raw$PC_Popul00) 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00000 0.02000 0.05100 0.05613 0.07700 1.30000      11 

# o) Traffic filtering score - based on:
# - modelled through traffic
# - recorded p&c casualties
# - modelled potential cycling flows
summary(tfl_sna_raw$Filtering_)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.000   0.000   2.000   1.968   3.000   6.000      11 
  
# p) General score -based on
# - social distancing (pavement widths and pop density)
# - number of schools
# - levels of deprivation
# - total population and low car ownership
summary(tfl_sna_raw$General_sc)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00    0.00    2.00    1.81    3.00    8.00      11 



################
# Data wrangling

tfl_sna = tfl_sna_raw
tfl_sna$Borough = factor(tfl_sna$Borough) # factor Borough

tfl_sna_na = tfl_sna %>%
  filter(is.na(Borough)) # identify observations with NA

# drop observations with nil apart from geometry and LSP bid
tfl_sna_clean = tfl_sna %>%
  filter(!is.na(Borough))

# Get rid of Z aspect in geometry (keep just X&Y)
tfl_sna_clean = st_zm(tfl_sna_clean, drop = T, what = 'ZM')

# import May 2020 ONS LA boundary data (required for NA management)
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

# Map london borough boundaries and Neighbourhoods
mapview(tfl_sna_clean, zcol = 'Borough') + 
  mapview(lon_lad_2020$geometry, color = "#555555", lwd = 1, alpha.regions = 0.01)

Neigh_area_borough = tfl_sna_clean %>%
  st_drop_geometry() %>%
  group_by(Borough) %>%
  summarise(Total_area = sum(Area_sqkm))
