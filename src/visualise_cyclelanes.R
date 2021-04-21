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
test_df$CLT_SEGREG = as.numeric(test_df$CLT_SEGREG) # false -> 1, true -> 2
test_df$CLT_SEGREG[test_df$CLT_SEGREG == 1] <- 0
test_df$CLT_SEGREG[test_df$CLT_SEGREG == 2] <- 1
sum(test_df$CLT_SEGREG)
# This now works

test_df = on_road %>%
  st_drop_geometry() %>%
  select(c("FEATURE_ID", "CLT_SEGREG", "CLT_STEPP"))
set.seed(321)
test_df = sample_n(test_df, 50) # 5 TRUE
str(test_df)
# tibble[,2] [50 × 2] (S3: tbl_df/tbl/data.frame)
# $ FEATURE_ID: chr [1:50] "RWG236927" "RWG279152" "RWG207149" "RWG042572" ...
# $ CLT_SEGREG: Factor w/ 2 levels "FALSE","TRUE": 1 1 1 1 1 1 1 1 1 1 ...
test_df = test_df %>%
  mutate(CLT_SEGREG_NUMERIC = as.numeric(test_df$CLT_SEGREG)) %>%
  mutate(CLT_STEPP_NUMERIC = as.numeric(test_df$CLT_STEPP)) 
  # false -> 1, true -> 2
test_df$CLT_SEGREG_NUMERIC[test_df$CLT_SEGREG_NUMERIC == 1] <- 0
test_df$CLT_SEGREG_NUMERIC[test_df$CLT_SEGREG_NUMERIC == 2] <- 1
test_df$CLT_STEPP_NUMERIC[test_df$CLT_STEPP_NUMERIC == 1] <- 0
test_df$CLT_STEPP_NUMERIC[test_df$CLT_STEPP_NUMERIC == 2] <- 1
sum(test_df$CLT_SEGREG_NUMERIC) # n = 1371
sum(test_df$CLT_STEPP_NUMERIC) # n = 94

test_df$CLT_SEGREG_NUMERIC[test_df$CLT_SEGREG_NUMERIC == 1] <- 5
test_df$CLT_STEPP_NUMERIC[test_df$CLT_STEPP_NUMERIC == 1] <- 4
test_df$row_sum = rowSums(test_df[, c(4, 5)])
# This now works

# if do on_road then get unique values in row)sm of 0, 5, 9, 4 
# will need to figure out if this works when add in other columns




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



