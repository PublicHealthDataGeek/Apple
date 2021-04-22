#################################################
# Visualise cycle lanes 
# 
# This code aims to visualise on road cycle lanes by the degree of separation
# from other road users.  The visualisation also aims to take into account speed
# limit (as that determines degree of recommended separation by the DfT) and 
# cycling usage from the Propensity to Cycle Tool.
#
# Initial code sorts out the assets that have more than one label for separation
# e.g. jointly labelled as being segregated and stepped.  




#!diagnostics suppress = CLT_MANDAT2, CLT_ADVIS2, CLT_SEGREG2


#Load packages
library(tidyverse)
#library(mapview)
library(tmap)
#library(cowplot)
#library(patchwork)
library(summarytools) # dont run if want to use mapview as it stops it working
library(sf)
library(tmaptools) # for palette explorer 
#library(ggpubr) # for text grobs
#library(ggspatial) # get north arrow and bar

################################
# Load and manipulate datasets #
################################


# 1) CID data

# Import Cycle Lanes and Tracks dataset (created from TFL datasets downloaded 25/2/21)
c_cyclelanetrack = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_cycle_lane_track")
# n = 25315

# names(c_cyclelanetrack)
# [1] "FEATURE_ID" "SVDATE"     "CLT_CARR"   "CLT_SEGREG" "CLT_STEPP" 
# [6] "CLT_PARSEG" "CLT_SHARED" "CLT_MANDAT" "CLT_ADVIS"  "CLT_PRIORI"
# [11] "CLT_CONTRA" "CLT_BIDIRE" "CLT_CBYPAS" "CLT_BBYPAS" "CLT_PARKR" 
# [16] "CLT_WATERR" "CLT_PTIME"  "CLT_ACCESS" "CLT_COLOUR" "BOROUGH"   
# [21] "PHOTO1_URL" "PHOTO2_URL" "geometry"   "length_m"   "length_km" 

# Details of variables
# CLT_CARR   on/off
# CLT_SEGREG lanes or tracks - TRUE = physical separation of cyclist from other users using continuous/near continuous kerb, upstand or planted verge.
# CLT_STEPP - onroad only, cycle lane/track (?) is at a level between the carriageway and footway 
# CLT_PARSEG - on and off, on - involves objects eg wants, off is a visual feature eg white line
# TRUE = additional forms of delineation NB as often used by Mand/Adv cycle lanes can have this set to TRUE and mand/advi set to TRUE
# CLT_SHARED - on and off, on - TRUE = shared with bus lane, off - TRUE = shared with other users eg pedestrians
# CLT_MANDAT - onroad only, TRUE = mandatory cycle lane
# CLT_ADVIS - onroad only, TRUE = advisory cycle lane
# NB onroad can only have MAND TRUE, ADVIS TRUE or both FALSE
# CLT_PRIORI
# "CLT_PTIME" - is the on/off cycle lane track part time or not?  
# [11] "CLT_CONTRA" "CLT_BIDIRE" "CLT_CBYPAS" "CLT_BBYPAS" "CLT_PARKR" 
# [16] "CLT_WATERR"   "CLT_ACCESS" "CLT_COLOUR" 

# Order of Protection from motor traffic on highways (DFT guidance pg 33)
# Fully kerbed > stepped > light segregation > Mandatory/Advisory 
# FK/S/LS suitable for most people at 20/30 mph only FK suitable for 40mph+
# M/A only suitable for most poepl on 20mph roads with motor vehicle flow of <5000

# Correspnd in CID to:
# CLT_SEGREG > CLT_STEPP > CLT_PARSEG > CLT_MANDAT > CLT_ADVIS
#  NB seems to be little difference in CID between SEGREG and STEPP - majority of 
# stepped are also labelled as segreg and only 5 are labelled as just stepped and they
# look very similar to those that are segreg in the photos

on_road = c_cyclelanetrack %>%
  filter(CLT_CARR == TRUE) # n = 13965

on_road_drop = on_road %>%
  st_drop_geometry() %>%
  select(-c("length_m", "length_km"))
view(dfSummary(on_road_drop))

# seg = 1371
# stepped = 94
# part seg = 349
# manda = 1854
# advi = 7273
# shared = 2845
# NB total is 13965 - what are the rest of the on road cycle lanes????



# Examining the dataset as think that some assets will be coded with more than one level of segregation
test_code_seg = on_road_drop %>%
  select(c("FEATURE_ID", "CLT_SEGREG", "CLT_STEPP", "CLT_PARSEG", "CLT_MANDAT", "CLT_ADVIS"))

# Findings:
# 1) CLT_SEGREG
view(ctable(x = test_code_seg$CLT_SEGREG, y = test_code_seg$CLT_STEPP))
# Of the 1371 on road segregated cycle lanes 1282 are not stepped but 89 are also coded as stepped
view(ctable(x = test_code_seg$CLT_SEGREG, y = test_code_seg$CLT_PARSEG))
# none are part segregated
view(ctable(x = test_code_seg$CLT_SEGREG, y = test_code_seg$CLT_MANDAT))
# 6 are mandatory cycle lanes
view(ctable(x = test_code_seg$CLT_SEGREG, y = test_code_seg$CLT_ADVIS))
# 2 are advisory cycle lanes

# # Therefore want to label the 1282 as Segregated, 89 as stepped not segregated but instead stepped
# # want to check up on those coded as mandat and advisory where seg and mand or seg and adv = TRUE
# seg_step = on_road %>%
#   filter(CLT_SEGREG == TRUE & CLT_STEPP == TRUE) 
# x = on_road %>%
#   filter(CLT_SEGREG == FALSE & CLT_STEPP == TRUE)
# 
# seg_mand = on_road %>%
#   filter(CLT_SEGREG == TRUE & CLT_MANDAT == TRUE)
# seg_advis = on_road %>%
#   filter(CLT_SEGREG == TRUE & CLT_ADVIS == TRUE)
# seg_step = on_road %>%
#   filter(CLT_SEGREG == TRUE & CLT_STEPP == TRUE)
# # examined both datast - recode both of these as CLT_ADVIS/MANDAT as false
# 
# # Correct these - this code is not working correctly.  ? related to https://github.com/rstudio/rstudio/issues/7372 affecting case_When?  
# # a) CLT_MAND
# # on_road = on_road %>%
# #   mutate(CLT_MANDAT2 = case_when(CLT_SEGREG == TRUE & CLT_MANDAT == TRUE ~ "FALSE")) 
# # on_road$CLT_MANDAT2[is.na(on_road$MANDAT2)] = "FALSE"
# # seg_mand2 = on_road %>%
# #   filter(CLT_SEGREG == TRUE & CLT_MANDAT2 == TRUE) # 0 observations
# # # b) CLT_ADVIS
# # on_road = on_road %>%
# #   mutate(CLT_ADVIS2 = case_when(CLT_SEGREG == TRUE & CLT_ADVIS == TRUE ~ "FALSE")) 
# # on_road$CLT_ADVIS2[is.na(on_road$CLT_ADVIS2)] = "FALSE"
# # seg_advis2 = on_road %>%
# #   filter(CLT_SEGREG == TRUE & CLT_ADVIS2 == TRUE) # 0 observations
# # 
# # # c) CLT_STEPP  ### DO I WANT TO DO THAT - ARE THEY ACTUALLY SEGREGATED AND STEPPED IN WHICH CASE KEEP SEGREGATED....
# # # want to recode CLT_SEGREG as false where CLT_SEGREG == TRUE & CLT_STEPP == TRUE
# on_road = on_road %>%
#   mutate(CLT_SEGREG2 = case_when(CLT_SEGREG == TRUE & CLT_STEPP == TRUE ~ "FALSE",
#                                  CLT_SEGREG == TRUE & CLT_STEPP == FALSE ~ "TRUE"))
# on_road$CLT_SEGREG2[is.na(on_road$CLT_SEGREG2)] = "FALSE"
# seg_step2 = on_road %>%
#  filter(CLT_SEGREG2 == TRUE & CLT_STEPP== TRUE) # 0 observations
# view(ctable(x = on_road$CLT_SEGREG2, y = on_road$CLT_STEPP)) # now have 1282 true segregated (false stepped) and 94 false seg but true stepped
# 






# 2) CLT_STEPP
view(ctable(x = test_code_seg$CLT_STEPP, y = test_code_seg$CLT_SEGREG))
# of the 94 that are stepped 89 are also coded as segregated
# now check on the updated on_road dataset following the new column of SEGREG2 - 
#view(ctable(x = on_road$CLT_STEPP, y = on_road$CLT_SEGREG2)) # all 94 stepped are not miscoded as segregated 

view(ctable(x = test_code_seg$CLT_STEPP, y = test_code_seg$CLT_PARSEG))
# none are coded as part segregated
view(ctable(x = test_code_seg$CLT_STEPP, y = test_code_seg$CLT_MANDAT))
# none are coded as mandatory
view(ctable(x = test_code_seg$CLT_STEPP, y = test_code_seg$CLT_ADVIS))
# 2 are coded as advisory

# Want to code all 94 as stepped once check up on those coded as advisory ie stepp and advis = TRUE

seg_levels = c("Segregated", "Stepped", "Partially segregated", "Mandatory cycle lane", "Advisory cycle lane")

https://www.marsja.se/r-add-column-to-dataframe-based-on-other-columns-conditions-dplyr/

# Create new variable that indicates degree of separation from traffic 
# seg_levels = c("Segregated", "Stepped", "Partially segregated", "Mandatory cycle lane", "Advisory cycle lane")

# # COnvert factor variables into numeric  
# cols = c("CLT_SEGREG", "CLT_STEPP", "CLT_PARSEG", "CLT_MANDAT", "CLT_ADVIS") 
# on_road_test = on_road %>% as.integer(as.character("CLT_SEGREG"))

# try smaller dataset of on_road

test_df = on_road %>%
  st_drop_geometry() %>%
  select(c("FEATURE_ID", "CLT_SEGREG"))
# set.seed(321)
# test_df = sample_n(test_df, 50) # 5 TRUE
# str(test_df)
# tibble[,2] [50 × 2] (S3: tbl_df/tbl/data.frame)
# $ FEATURE_ID: chr [1:50] "RWG236927" "RWG279152" "RWG207149" "RWG042572" ...
# $ CLT_SEGREG: Factor w/ 2 levels "FALSE","TRUE": 1 1 1 1 1 1 1 1 1 1 ...
# test_df$CLT_SEGREG = as.numeric(test_df$CLT_SEGREG) # false -> 1, true -> 2
# test_df$CLT_SEGREG[test_df$CLT_SEGREG == 1] <- 0
# test_df$CLT_SEGREG[test_df$CLT_SEGREG == 2] <- 1
# sum(test_df$CLT_SEGREG)
# # This now works
# 
# test_df = on_road %>%
#   st_drop_geometry() %>%
#   select(c("FEATURE_ID", "CLT_SEGREG", "CLT_STEPP"))
# set.seed(321)
# test_df = sample_n(test_df, 50) # 5 TRUE
# str(test_df)
# # tibble[,2] [50 × 2] (S3: tbl_df/tbl/data.frame)
# # $ FEATURE_ID: chr [1:50] "RWG236927" "RWG279152" "RWG207149" "RWG042572" ...
# # $ CLT_SEGREG: Factor w/ 2 levels "FALSE","TRUE": 1 1 1 1 1 1 1 1 1 1 ...
# test_df = test_df %>%
#   mutate(CLT_SEGREG_NUMERIC = as.numeric(test_df$CLT_SEGREG)) %>%
#   mutate(CLT_STEPP_NUMERIC = as.numeric(test_df$CLT_STEPP)) 
#   # false -> 1, true -> 2
# test_df$CLT_SEGREG_NUMERIC[test_df$CLT_SEGREG_NUMERIC == 1] <- 0
# test_df$CLT_SEGREG_NUMERIC[test_df$CLT_SEGREG_NUMERIC == 2] <- 1
# test_df$CLT_STEPP_NUMERIC[test_df$CLT_STEPP_NUMERIC == 1] <- 0
# test_df$CLT_STEPP_NUMERIC[test_df$CLT_STEPP_NUMERIC == 2] <- 1
# sum(test_df$CLT_SEGREG_NUMERIC) # n = 1371
# sum(test_df$CLT_STEPP_NUMERIC) # n = 94
# 
# test_df$CLT_SEGREG_NUMERIC[test_df$CLT_SEGREG_NUMERIC == 1] <- 5
# test_df$CLT_STEPP_NUMERIC[test_df$CLT_STEPP_NUMERIC == 1] <- 4
# test_df$row_sum = rowSums(test_df[, c(4, 5)])
# # This now works
# 
# # if do on_road then get unique values in row)sm of 0, 5, 9, 4 
# # will need to figure out if this works when add in other columns

# Create new variable that divides observations into shared, contraflow and rest 
on_road = on_road %>%
  mutate(type = case_when(CLT_SHARED == TRUE ~ "Shared",
                          CLT_CONTRA == TRUE ~ "Contraflow",
                          TRUE ~ "Rest"))
on_road %>%
  st_drop_geometry() %>%
  group_by(type) %>%
  summarise(count = n())
#   type       count
#   <chr>      <int>
# 1 Contraflow  1435
# 2 Rest        9685
# 3 Shared      2845
1435+9685+2845 
# = 13965 


# Convert Factors to numeric
# $ CLT_SEGREG: Factor w/ 2 levels "FALSE","TRUE": 1 1 1 1 1 1 1 1 1 1 ...
on_road_numeric = on_road %>%
  mutate(CLT_SEGREG_NUMERIC = as.numeric(on_road$CLT_SEGREG)) %>%
  mutate(CLT_STEPP_NUMERIC = as.numeric(on_road$CLT_STEPP))  %>%
  mutate(CLT_PARSEG_NUMERIC = as.numeric(on_road$CLT_PARSEG))  %>%
  mutate(CLT_MANDAT_NUMERIC = as.numeric(on_road$CLT_MANDAT))  %>%
  mutate(CLT_ADVIS_NUMERIC = as.numeric(on_road$CLT_ADVIS)) %>%
  mutate(CLT_SHARED_NUMERIC = as.numeric(on_road$CLT_SHARED)) %>%
  mutate(CLT_CONTRA_NUMERIC = as.numeric(on_road$CLT_CONTRA)) %>%
  mutate(CLT_PARKR_NUMERIC = as.numeric(on_road$CLT_PARKR))
  # converts all False to 1 and True to 2

# Convert 1(false) to 0 and 2(true) to 1
on_road_numeric$CLT_SEGREG_NUMERIC = ifelse(on_road_numeric$CLT_SEGREG_NUMERIC == 1, 0, 1)
on_road_numeric$CLT_STEPP_NUMERIC = ifelse(on_road_numeric$CLT_STEPP_NUMERIC == 1, 0, 1)
on_road_numeric$CLT_PARSEG_NUMERIC = ifelse(on_road_numeric$CLT_PARSEG_NUMERIC == 1, 0, 1)
on_road_numeric$CLT_MANDAT_NUMERIC = ifelse(on_road_numeric$CLT_MANDAT_NUMERIC == 1, 0, 1)
on_road_numeric$CLT_ADVIS_NUMERIC = ifelse(on_road_numeric$CLT_ADVIS_NUMERIC == 1, 0, 1)
on_road_numeric$CLT_SHARED_NUMERIC = ifelse(on_road_numeric$CLT_SHARED_NUMERIC == 1, 0, 1)
on_road_numeric$CLT_CONTRA_NUMERIC = ifelse(on_road_numeric$CLT_CONTRA_NUMERIC == 1, 0, 1)
on_road_numeric$CLT_PARKR_NUMERIC = ifelse(on_road_numeric$CLT_PARKR_NUMERIC == 1, 0, 1)

# Check now gives the count that I expect
sum(on_road_numeric$CLT_SEGREG_NUMERIC) # n = 1371
sum(on_road_numeric$CLT_STEPP_NUMERIC) # n = 94
sum(on_road_numeric$CLT_PARSEG_NUMERIC) # n = 349
sum(on_road_numeric$CLT_MANDAT_NUMERIC) # n = 1854
sum(on_road_numeric$CLT_ADVIS_NUMERIC) # n = 7273
sum(on_road_numeric$CLT_SHARED_NUMERIC) # n = 2845
sum(on_road_numeric$CLT_CONTRA_NUMERIC) # n = 1463
sum(on_road_numeric$CLT_PARKR_NUMERIC) # n = 108

# Recode to give weighted value
on_road_numeric$CLT_SEGREG_weight = ifelse(on_road_numeric$CLT_SEGREG_NUMERIC == 1, 10000, 0)
on_road_numeric$CLT_STEPP_weight = ifelse(on_road_numeric$CLT_STEPP_NUMERIC == 1, 1000, 0)
on_road_numeric$CLT_PARSEG_weight = ifelse(on_road_numeric$CLT_PARSEG_NUMERIC == 1, 100, 0)
on_road_numeric$CLT_MANDAT_weight = ifelse(on_road_numeric$CLT_MANDAT_NUMERIC == 1, 10, 0)
on_road_numeric$CLT_ADVIS_weight = ifelse(on_road_numeric$CLT_ADVIS_NUMERIC == 1, 1, 0)

# Create new column with weights for the 5 classes of separation
on_road_numeric = on_road_numeric %>%
  rowwise() %>%
  mutate(weight_5 = sum(c_across(CLT_SEGREG_weight:CLT_ADVIS_weight)))
unique(on_road_numeric$weight_5)
# 1    10   100 10000   110     0   101 11000  1001 10010  1000 10001
on_road_numeric %>%
  st_drop_geometry() %>%
  group_by(weight_5) %>%
  summarise(count = n())

#       weight_5 count
#           <dbl> <int>
# 1            0  3372  # none of the 5 categories - might be shared or contraf
# 2            1  7196  advisory cycle lane only
# 3           10  1672  mand cycle lane only
# 4          100   100  part segregated only
# 5          101    73  part seg + advisory
# 6          110   176  part seg + mand
# 7         1000     3  stepped only
# 8         1001     2  stepped + advisory 
# 9        10000  1274  segregated only
# 10       10001     2  segregated + advisory
# 11       10010     6  segregated + mandatory
# 12       11000    89  segregated + stepped


##  Create factored column where labelled by the 'highest' degree of separation
# factor numeric
on_road_factor = on_road_numeric %>%
  mutate(Highest_separation = factor(weight_5))

# convert factored numbers to relevent labels
on_road_factor = on_road_factor %>%
  mutate(Highest_separation = fct_collapse(Highest_separation, 
           "Segregated" = c("10000","10001","10010", "11000"),
           "Stepped" = c("1000", "1001"),
           "Part-segregated" = c("100", "101", "110"),
           "Mandatory cycle lane" = c("10"),
           "Advisory cycle lane" = c("1"),
           "No separation" = c("0")))

# relevel order of factors in the degree of separation
on_road_factor = on_road_factor %>%
  mutate(Highest_separation = fct_relevel(Highest_separation, 
                                           c("Segregated", "Stepped", "Part-segregated",
                                             "Mandatory cycle lane", "Advisory cycle lane",
                                             "No separation")))
# # Check to see works ok
# on_road_factor %>%
#   st_drop_geometry() %>%
#   group_by(Highest_separation) %>%
#   summarise(count = n())
# 
# #   Highest_separation     count
# #   <fct>                  <int>
# # 1 Segregated              1371
# # 2 Stepped                    5
# # 3 Part-segregated          349
# # 4 Mandatory cycle lane    1672
# # 5 Advisory cycle lane     7196
# # 6 No level of separation  3372                            
                                    

# create datasets by type
contra = on_road_factor %>%
  filter(type == "Contraflow") # n = 1435


# create shared dataset
shared = on_road_factor %>%
  filter(type == "Shared") # n = 2845



# create rest dataset
rest = on_road_factor %>%
  filter(type == "Rest") # n= 9685



# create df of degrees of separation by type
rest_sep = rest %>%
  st_drop_geometry() %>%
  group_by(Highest_separation) %>%
  summarise(rest_count = n())
contra_sep = contra %>%
  st_drop_geometry() %>%
  group_by(Highest_separation) %>%
  summarise(contra_count = n())
shared_sep = shared %>%
  st_drop_geometry() %>%
  group_by(Highest_separation) %>%
  summarise(shared_count = n())
summary_high_sep = left_join(rest_sep, contra_sep) %>%
  left_join(shared_sep)
summary_high_sep[is.na(summary_high_sep)] <- 0
# Highest_separation   rest_count contra_count shared_count
# <fct>                     <int>        <int>        <int>
# 1 Segregated                  976          393            2
# 2 Stepped                       5            0            0
# 3 Part-segregated             273           72            4
# 4 Mandatory cycle lane       1501          165            6
# 5 Advisory cycle lane        6877          283           36
# 6 No separation                53          522         2797




contra_weight_5_count = contra %>%
  st_drop_geometry() %>%
  group_by(weight_5) %>%
  summarise(count = n())
sum(weight_5_count$count) # = 13965 ie this totals the total number of on_road obs



on_road_numeric = on_road_numeric %>%
  rowwise() %>%
  mutate(weight_3 = sum(c_across(CLT_SHARED_weight:CLT_PARKR_weight)))

unique(on_road_numeric$weight_3)
#  0  20 200 220   2 202  22

weight_3_count = on_road_numeric %>%
  st_drop_geometry() %>%
  group_by(weight_3) %>%
  summarise(count = n())
sum(weight_3_count$count)  # n = 13965

# weight_3 count
# <dbl> <int>
# 1        0  9594 Not park, contra or shared
# 2        2    91 Park
# 3       20  1429 Contra
# 4       22     6 Contra + Park
# 5      200  2806 Shared
# 6      202    11 Shared + Park
# 7      220    28 Shared + Contra



# total_weight count
# <dbl> <int>
#  1            0    48  none of the categories
#  2            1  6838 advisory cycle lane only
#  3            2     5  Park 
#  4            3    39  Park + advisory
#  5           10  1488  mand cycle lane only
#  6           12    13  mand & park
#  7           20   521  contra
#  8           21   282  contra + advisory
#  9           22     1  contra + park
# 10           23     1  contra + park + advisory
# 11           30   163  contra + mand
# 12           32     2  contra + park + mand
# 13          100    60 part segregated only
# 14          101    58 part seg + advisory
# 15          102     2 part seg + park
# 16          103     2 part seg + park + advisory
# 17          110   150 part seg + mand
# 18          112     1 part seg + park + mand 
# 19          120    34 part seg + contra
# 20          121    13 part seg + contra + advisory
# 21          130    25 part seg + contra + mand
# 22          200  2758 shared
# 23          201    36 shared + advisory
# 24          202    11 shared + park
# 25          210     6 shared + mandatory
# 26          220    28 shared + contra
# 27          300     4 shared + partseg
# 28         1000     3 stepped only
# 29         1001     2 stepped + advisory 
# 30        10000   861 segregated only
# 31        10001     2 segregated + advisory
# 32        10002    26 segregated + park
# 33        10010     5 segregated + mandatory
# 34        10020   384 segregated + contra
# 35        10022     2 segregated + contra + park
# 36        10200     1 segregated + shared
# 37        10210     1 segregated + shared + mandat
# 38        11000    79 segregated + stepped
# 39        11002     3 segregated + stepped + park
# 40        11020     7 segregated + stepped + contra













# 4) PCT data
# The code for obtaining this data is in: get_pct_km_cycled.R file
pct_borough_commuting = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/Borough_commuting.rds") %>%
  mutate(total_100000km_cycled_for_commuting_per_year_estimated = total_km_cycled_for_commuting_per_year_estimated / 100000) 
























# 1) Local Authority spatial data  - which dataset do I wnat this or BRE?
# # import May 2020 ONS LA boundary data clipped to coastline (used so that River Thames appears)
# lon_lad_2020_c2c = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFC.Rds")
# 
# # simply borough shapes
# lon_lad_2020_c2c <- rmapshaper::ms_simplify(lon_lad_2020_c2c, keep=0.015) #Simplify boroughs
# # lon_lad_2020_c2c$BOROUGH_short = fct_recode(lon_lad_2020_c2c$BOROUGH, 
# #                                       "Kens & Chel" = "Kensington & Chelsea",
# #                                       "Bark & Dage" = "Barking & Dagenham",
# #                                       "Hamm & Fulh" = "Hammersmith & Fulham",
# #                                       "Kingston" = "Kingston upon Thames",
# #                                       "Richmond" = "Richmond upon Thames",
# #                                       "City" = "City of London",
# #                                       "T Hamlets" = "Tower Hamlets",
# #                                       "W Forest" = "Waltham Forest") # rename Boroughs to reduce text length
# 
# # Create new variable that labels the Boroughs by number (matches the overall map)
# lon_lad_2020_c2c$Borough_number = fct_recode(lon_lad_2020_c2c$BOROUGH, 
#                                              "7" = "Kensington & Chelsea",
#                                              "32" = "Barking & Dagenham",
#                                              "8" = "Hammersmith & Fulham",
#                                              "25" = "Kingston upon Thames",
#                                              "24" = "Richmond upon Thames",
#                                              "1" = "City of London",
#                                              "15" = "Waltham Forest",
#                                              "28" = "Croydon",
#                                              "29" = "Bromley",
#                                              "23" = "Hounslow",
#                                              "20" = "Ealing",
#                                              "31" = "Havering",
#                                              "22" = "Hillingdon",
#                                              "21" = "Harrow",
#                                              "19" = "Brent",
#                                              "18" = "Barnet",
#                                              "10" = "Lambeth",
#                                              "11" = "Southwark", 
#                                              "12" = "Lewisham",
#                                              "13" = "Greenwich",
#                                              "30" = "Bexley",
#                                              "17" = "Enfield",
#                                              "33" = "Redbridge",
#                                              "27" = "Sutton",
#                                              "26" = "Merton",
#                                              "9" = "Wandsworth",
#                                              "6" = "Westminster",
#                                              "5" = "Camden",
#                                              "2" = "Tower Hamlets",
#                                              "4" = "Islington",
#                                              "3" = "Hackney",
#                                              "16" = "Haringey",
#                                              "14" = "Newham")
# 
# # Convert borough area into km^2 from m^2 
# lon_lad_2020_c2c$Shape__Are = units::set_units(lon_lad_2020_c2c$Shape__Are, m^2)
# lon_lad_2020_c2c = lon_lad_2020_c2c %>%
#   mutate(Borough_Area_km2 = (units::set_units(lon_lad_2020_c2c$Shape__Are, km^2)))# change area units to km^2 from m^2
# 
# 
# # Select variables of interest
# lon_lad_2020_c2c_reduced = lon_lad_2020_c2c %>%
#   select(c("BOROUGH", "Borough_number", "Borough_Area_km2", "geometry"))



