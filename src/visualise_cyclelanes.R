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




#Load packages
library(tidyverse)
library(mapview)

library(tmap)
#library(cowplot)
#library(patchwork)
#library(summarytools) # dont run if want to use mapview as it stops it working
library(sf)
library(tmaptools) # for palette explorer 
library(viridis)
#library(ggpubr) # for text grobs
#library(ggspatial) # get north arrow and bar

# Package options
mapviewOptions(native.crs = TRUE)
tmap_design_mode(design.mode = FALSE)


################################
# Load and manipulate datasets #
################################

# 1) Local Authority spatial data

# import May 2020 ONS LA boundary data clipped to coastline (used so that River Thames appears)
lon_lad_2020_c2c = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFC.Rds")

# simply borough shapes
lon_lad_2020_c2c <- rmapshaper::ms_simplify(lon_lad_2020_c2c, keep=0.015) #Simplify boroughs

# Create new variable that labels the Boroughs by number (matches the overall map)
lon_lad_2020_c2c$Borough_number = fct_recode(lon_lad_2020_c2c$BOROUGH, 
                                             "7" = "Kensington & Chelsea",
                                             "32" = "Barking & Dagenham",
                                             "8" = "Hammersmith & Fulham",
                                             "25" = "Kingston upon Thames",
                                             "24" = "Richmond upon Thames",
                                             "1" = "City of London",
                                             "15" = "Waltham Forest",
                                             "28" = "Croydon",
                                             "29" = "Bromley",
                                             "23" = "Hounslow",
                                             "20" = "Ealing",
                                             "31" = "Havering",
                                             "22" = "Hillingdon",
                                             "21" = "Harrow",
                                             "19" = "Brent",
                                             "18" = "Barnet",
                                             "10" = "Lambeth",
                                             "11" = "Southwark", 
                                             "12" = "Lewisham",
                                             "13" = "Greenwich",
                                             "30" = "Bexley",
                                             "17" = "Enfield",
                                             "33" = "Redbridge",
                                             "27" = "Sutton",
                                             "26" = "Merton",
                                             "9" = "Wandsworth",
                                             "6" = "Westminster",
                                             "5" = "Camden",
                                             "2" = "Tower Hamlets",
                                             "4" = "Islington",
                                             "3" = "Hackney",
                                             "16" = "Haringey",
                                             "14" = "Newham")

# Select variables of interest
lon_lad_2020_c2c_reduced = lon_lad_2020_c2c %>%
  select(c("BOROUGH", "Borough_number", "geometry"))

# 1) CID data

# Import Cycle Lanes and Tracks dataset (created from TFL datasets downloaded 25/2/21)
c_cyclelanetrack = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_cycle_lane_track")
# n = 25315

# Join to borough geometry  ?do I want to do that???
# geom_cyclelanetrack = left_join(lon_lad_2020_c2c_reduced, c_cyclelanetrack) 


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

# Limit CID to on road infrastructure only 
on_road = c_cyclelanetrack %>%
  filter(CLT_CARR == TRUE) # n = 13965

# Create dataframe so can obtain a df summary
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



# Examine the dataset as think that some assets are coded with more than one level of segregation
test_code_seg = on_road_drop %>%
  select(c("FEATURE_ID", "CLT_SEGREG", "CLT_STEPP", "CLT_PARSEG", "CLT_MANDAT", "CLT_ADVIS"))

# Findings when looking at CLT_SEGREG
view(ctable(x = test_code_seg$CLT_SEGREG, y = test_code_seg$CLT_STEPP))
# Of the 1371 on road segregated cycle lanes 89 are also coded as stepped
view(ctable(x = test_code_seg$CLT_SEGREG, y = test_code_seg$CLT_PARSEG))
# none are part segregated
view(ctable(x = test_code_seg$CLT_SEGREG, y = test_code_seg$CLT_MANDAT))
# 6 are also coded as mandatory cycle lanes
view(ctable(x = test_code_seg$CLT_SEGREG, y = test_code_seg$CLT_ADVIS))
# 2 are also coded as advisory cycle lanes

#################################################################
# CID dataset manipulation to enable analysis and visualisation #
#################################################################

# Create new variable that divides observations into shared, contraflow and rest 
on_road = on_road %>%
  mutate(type = case_when(CLT_SHARED == TRUE ~ "Shared",
                          CLT_CONTRA == TRUE ~ "Contraflow",
                          TRUE ~ "Rest"))
on_road$type = factor(on_road$type, levels = c("Rest", "Shared", "Contraflow"))

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
# on_road_numeric$CLT_SHARED_NUMERIC = ifelse(on_road_numeric$CLT_SHARED_NUMERIC == 1, 0, 1)
# on_road_numeric$CLT_CONTRA_NUMERIC = ifelse(on_road_numeric$CLT_CONTRA_NUMERIC == 1, 0, 1)
# on_road_numeric$CLT_PARKR_NUMERIC = ifelse(on_road_numeric$CLT_PARKR_NUMERIC == 1, 0, 1)

# Check now gives the count that I expect
sum(on_road_numeric$CLT_SEGREG_NUMERIC) # n = 1371
sum(on_road_numeric$CLT_STEPP_NUMERIC) # n = 94
sum(on_road_numeric$CLT_PARSEG_NUMERIC) # n = 349
sum(on_road_numeric$CLT_MANDAT_NUMERIC) # n = 1854
sum(on_road_numeric$CLT_ADVIS_NUMERIC) # n = 7273
# sum(on_road_numeric$CLT_SHARED_NUMERIC) # n = 2845
# sum(on_road_numeric$CLT_CONTRA_NUMERIC) # n = 1463
# sum(on_road_numeric$CLT_PARKR_NUMERIC) # n = 108

# Recode to give weighted value with segregated weighted highest and advisory cycle lane weighted lowest
on_road_numeric$CLT_SEGREG_weight = ifelse(on_road_numeric$CLT_SEGREG_NUMERIC == 1, 10000, 0)
on_road_numeric$CLT_STEPP_weight = ifelse(on_road_numeric$CLT_STEPP_NUMERIC == 1, 1000, 0)
on_road_numeric$CLT_PARSEG_weight = ifelse(on_road_numeric$CLT_PARSEG_NUMERIC == 1, 100, 0)
on_road_numeric$CLT_MANDAT_weight = ifelse(on_road_numeric$CLT_MANDAT_NUMERIC == 1, 10, 0)
on_road_numeric$CLT_ADVIS_weight = ifelse(on_road_numeric$CLT_ADVIS_NUMERIC == 1, 1, 0)

# Create new column with the sum of the weights for the 5 classes of separation
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

# Relevel order of factors in the degree of separation
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
shared = on_road_factor %>%
  filter(type == "Shared") # n = 2845
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

##################################################
# Create visualisations of degrees of separation #
##################################################

separation_map = tm_shape(lon_lad_2020_c2c) +
  tm_polygons(col = "gray98", border.col = "gray70") +
  tm_shape(on_road_factor) +
  tm_lines("Highest_separation",
           palette = "viridis",
           lwd = 2.5, 
           alpha = 0.75, 
           title.col = "Degree of separation") +
  tm_layout(title = "On road cycle lane separation from other road users", 
            frame = FALSE)

separation_map_type = tm_shape(lon_lad_2020_c2c) +
  tm_polygons(col = "gray98", border.col = "gray70") +
  tm_shape(on_road_factor) +
  tm_lines("Highest_separation",
           palette = "viridis",
           lwd = 2.5, 
           alpha = 0.75) + 
  tm_facets(by = "type",
        free.coords = FALSE) +
  tm_layout(legend.position = c("left", "top"))

# Other colour options
# purp_green_sep = tm_shape(lon_lad_2020_c2c) +
#   tm_polygons(col = "white") +
#   tm_shape(on_road_factor) +
#   tm_lines("Highest_separation",
#            palette = "PiYG",
#            lwd = 2)  
# 
# magma_sep = tm_shape(lon_lad_2020_c2c) +
#   tm_polygons(col = "white") +
#   tm_shape(on_road_factor) +
#   tm_lines("Highest_separation",
#            palette = "magma",
#            lwd = 2)  

####################################################################
# Generating borough level data on lengths by degree of separation #
# and visualisations                                               #
####################################################################

borough_separation_length = on_road_factor %>%
  st_drop_geometry() %>%
  group_by(BOROUGH, Highest_separation) %>%
  summarise(total_length = sum(length_m))

total_borough_separation_length = on_road_factor %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(total_borough_length = sum(length_m))# max = Croydon 58331.287 [m], min = Sutton 5832.761 [m]

# remove units to allow plotting
borough_separation_length$total_length = as.integer(round(units::drop_units(borough_separation_length$total_length)))
total_borough_separation_length$total_borough_length = as.integer(round(units::drop_units(total_borough_separation_length$total_borough_length)))

# join total borough lengths to borough_separation_length
borough_separation_length = left_join(borough_separation_length, total_borough_separation_length)

# Now try with full dataset
borough_separation_length_reorder <- borough_separation_length # reorder so that Segregated appears at top
borough_separation_length_reorder$Highest_separation = fct_rev(borough_separation_length_reorder$Highest_separation)

# Create multiple bar charts of each Borough by degree of separation
ggplot(borough_separation_length_reorder) +
  geom_bar(aes(x = Highest_separation, y = total_length, fill = Highest_separation), 
           stat = "identity") +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, direction = -1, guide = guide_legend(reverse = TRUE)) +
  facet_wrap(~ BOROUGH) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# This creates multiple deep stacked bar charts faceted by borough - easier to see size of bar than the second approach before
ggplot(borough_separation_length_reorder) +
  geom_bar(aes(x = -Highest_separation, y = total_length, fill = Highest_separation), 
           stat = "identity")+
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, direction = -1, guide = guide_legend(reverse = TRUE)) +
  facet_wrap(~ BOROUGH) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# This creates multiple stacked bar charts faceted by borough - bar charts are located in various y positions relating to the order of the Borough
ggplot(borough_separation_length_reorder) +
  geom_bar(aes(x = BOROUGH, y = total_length, fill = Highest_separation), 
           stat = "identity")+
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, direction = -1, guide = guide_legend(reverse = TRUE)) +
  facet_wrap(~ BOROUGH) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())



# Now want to get arranged spatially as per https://github.com/rogerbeecham/od-flowvis-ggplot2
# source data  = "https://github.com/aftertheflood/londonsquared/blob/master/site/data/grid.csv"

# get london squares positions
london_squared = read.table(file = "/home/bananafan/Downloads/londonsquared.txt", header = TRUE, sep = ",")
# rename columns
london_squared$fY <-london_squared$y
london_squared$fX <-london_squared$x

# Helper function for rescaling
map_scale <- function(value, min1, max1, min2, max2) {
  return  (min2+(max2-min2)*((value-min1)/(max1-min1)))
}
# Used to reverse london_squared layout for use in ggplot2 facet_grid().
max_y <- max(london_squared$fY)
min_y <- min(london_squared$fY)

london_squared <- london_squared %>% mutate(fY=map_scale(fY, min_y, max_y, max_y, min_y))
rm(min_y,max_y)

# relabel values in london_squared so can join to borough_separation
london_squared$BOROUGH <- london_squared$name
london_squared$BOROUGH = as.factor(london_squared$BOROUGH) 
london_squared$BOROUGH = fct_recode(london_squared$BOROUGH, 
                                      "Kensington & Chelsea" = "Kensington and Chelsea", 
                                      "Barking & Dagenham" = "Barking and Dagenham",
                                      "Hammersmith & Fulham" = "Hammersmith and Fulham")                            

# Create new variable that labels the Boroughs by number (matches the overall map)
london_squared$Borough_number = fct_recode(london_squared$BOROUGH, 
                                             "7" = "Kensington & Chelsea",
                                             "32" = "Barking & Dagenham",
                                             "8" = "Hammersmith & Fulham",
                                             "25" = "Kingston upon Thames",
                                             "24" = "Richmond upon Thames",
                                             "1" = "City of London",
                                             "15" = "Waltham Forest",
                                             "28" = "Croydon",
                                             "29" = "Bromley",
                                             "23" = "Hounslow",
                                             "20" = "Ealing",
                                             "31" = "Havering",
                                             "22" = "Hillingdon",
                                             "21" = "Harrow",
                                             "19" = "Brent",
                                             "18" = "Barnet",
                                             "10" = "Lambeth",
                                             "11" = "Southwark", 
                                             "12" = "Lewisham",
                                             "13" = "Greenwich",
                                             "30" = "Bexley",
                                             "17" = "Enfield",
                                             "33" = "Redbridge",
                                             "27" = "Sutton",
                                             "26" = "Merton",
                                             "9" = "Wandsworth",
                                             "6" = "Westminster",
                                             "5" = "Camden",
                                             "2" = "Tower Hamlets",
                                             "4" = "Islington",
                                             "3" = "Hackney",
                                             "16" = "Haringey",
                                             "14" = "Newham")

# simplify dataset for joining
london_squared_tidy = london_squared %>%
  select(c("BOROUGH", "Borough_number", "fY", "fX"))

# Join london_squared to cycle lane lengths
borough_separation_length_spatial = left_join(borough_separation_length_reorder, london_squared_tidy, by = "BOROUGH")

# basic plot works ok - keep as gives the basic layout
ggplot(borough_separation_length_spatial, aes(x = -Highest_separation, y = total_length, fill = Highest_separation)) +
  geom_bar(stat = "identity")+
  #geom_text(aes(x = fX, y = fY, label = Borough_number)) +
  geom_text(aes(label = Borough_number, y = Borough_number)) +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, direction = -1, guide = guide_legend(reverse = TRUE)) +
  facet_grid(-fY ~ fX) +  # need to do -fY to get correct orientation with enfield top row and sutton bottom row
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.spacing=unit(-0.5, "lines"))  # THis works in a basic way. 

#fiddling to make look better
ggplot(borough_separation_length_spatial, aes(x = -Highest_separation, y = total_length, fill = Highest_separation)) +
  geom_bar(stat = "identity")+  # geom_bar(stat = "identity", color = "black", size = 0.1) +
  #geom_text(aes(label = Borough_number, y = Borough_number)) +  # labels facets by borough number but looks at bit rubbish - need to convert borough number to numeric to use
  coord_flip() +
  geom_abline(slope=0, intercept= 60000,  col = "red",lty=2) +
  scale_fill_viridis(discrete = TRUE, direction = -1, guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(breaks = c(0, 60000)) +  # 58000 uis the highest total value (Croydon)
  facet_grid(-fY ~ fX) +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),axis.line = element_blank(),
        axis.title = element_blank(),
        strip.text = element_blank(),
        panel.spacing=unit(-0.5, "lines"),
        #panel.border = element_rect(fill = NA, color = "black"),
        plot.title = element_text(hjust = 0.5))


# Things that need sorting
# - label facets by borough number  CANT SEEM TO DO THIS APART FROM IT LOOKING BAD
# - draw rectangle so that can directly can compare with croydon (max value) - AGAIN CANT SEEM TO DO THIS
# ? do another one that just shows seg,step, part seg, cycle lane, no sep?  
# ? can fiddle with panel spacing to make look better?  


# Create bar chart (like for the assets) for total length by borough
ggplot(borough_separation_length_spatial) +
  geom_bar(aes(x = reorder(Borough_number, -total_borough_length), y = total_length, fill = Highest_separation), 
           stat = "identity") +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, direction = -1, guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(limits = c(0, 62000), expand = c(0,0), breaks = c(20000, 40000, 60000)) +
  theme_classic() +
  labs(y = "Total length (km)", x = NULL) +
  theme(axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        legend.position = "none")


# Speed data 
# load packages
library(osmextract)
library(sf)
library(mapview)
library(tidyverse)

# Set Mapview options to use data CRS rather than OSM projections
mapviewOptions(native.crs = TRUE)

# Load 2019 OSM dataset
gl_pbf19 = "/home/bananafan/Documents/PhD/Paper1/data/greater-london-190101.osm.pbf"
gl_osm_lines19 = oe_read(gl_pbf, quiet = FALSE) # Simple feature collection with 355244 features and 10 fields, CRS WGS84

# Change CRS to match ONS and CID
gl_osm_lines19 = st_transform(gl_osm_lines19, crs=27700) # PROJCRS["OSGB 1936 / British National Grid",

# import May 2020 ONS LA boundary data (required for NA management)
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")


names(gl_osm_lines19)
# [1] "osm_id"     "name"       "highway"    "waterway"   "aerialway"  "barrier"   
# [7] "man_made"   "maxspeed"   "z_order"    "other_tags" "geometry"  

unique(gl_osm_lines19$maxspeed) # looks like all sorts of speeds for different types of infrastructure
# [1] "30 mph"   "40 mph"   NA         "20 mph"   "70 mph"   "10 mph"  
# [7] "60 mph"   "50 mph"   "85 mph"   "80 mph"   "15 mph"   "75 mph"  
# [13] "5 mph"    "45 mph"   "110 mph"  "100 mph"  "25 mph"   "12 mph"  
# [19] "115 mph"  "230"      "105 mph"  "90 mph"   "7"        "30"      
# [25] "60"       "100"      "35 mph"   "65 mph"   "55 mph"   "20"      
# [31] "10mph"    "95 mph"   "64"       "none"     "5"        "16.09"   
# [37] "walk"     "signals"  "variable" "50"       "4 mph"    "national"
# [43] "15"       "10"       "125 mph"  "40"      

unique(gl_osm_lines19$highway)
# [1] "primary"         "residential"     "trunk"           "trunk_link"     
# [5] "footway"         "service"         "unclassified"    "tertiary"       
# [9] "secondary"       "motorway_link"   "cycleway"        NA               
# [13] "motorway"        "tertiary_link"   "bridleway"       "secondary_link" 
# [17] "pedestrian"      "primary_link"    "path"            "living_street"  
# [21] "steps"           "track"           "construction"    "proposed"       
# [25] "raceway"         "road"            "no"              "corridor"       
# [29] "escalator"       "elevator"        "cy"              "stepping_stones"
# [33] "disused"         "crossing"        "access" 

# https://wiki.openstreetmap.org/wiki/Key:highway
# highway=* distinguishes roads by function and importance rather by their physical characteristic and legal classification.

### Make decisions about which values from highway to keep

# [1] "primary"      KEEP      "residential"   KEEP  "trunk"         KEEP  "trunk_link"     KEEP
# [5] "footway"      DROP      "service"       KEEP  "unclassified"  KEEP  "tertiary"       KEEP       
# [9] "secondary"    KEEP      "motorway_link" DROP  "cycleway"      DROP        NA         ??      
# [13] "motorway"    DROP      "tertiary_link" KEEP  "bridleway"     DROP  "secondary_link" KEEP
# [17] "pedestrian"  FALSE     "primary_link"  KEEP   "path"         DROP  "living_street"  KEEP
# [21] "steps"       DROP      "track"                "construction" DROP  "proposed"       DROP       
# [25] "raceway"     DROP      "road" ????            "no"                  "corridor"      DROP 
# [29] "escalator"   DROP      "elevator"     DROP        "cy"      DROP    "stepping_stones" DROP
# [33] "disused"     DROP      "crossing"     DROP        "access"  DROP

highways_to_keep = c("primary", "residential", "trunk", "trunk_link", "service", "unclassified", "tertiary",
                     "secondary", "tertiary_link", "secondary_link", "primary_link", "living_street")

os_relevent_highways = gl_osm_lines19 %>%
  filter(highway %in% highways_to_keep) # n = 1777000
unique(os_relevent_highways$maxspeed) 
max_speed_count = os_relevent_highways %>%
  st_drop_geometry() %>%
  group_by(maxspeed) %>%
  summarise(count = n())

max_speed_count %>% print(n = Inf)
#maxspeed count
# <chr>    <int>
# 1 10           7
# 2 10 mph     459
# 3 100          1
# 4 10mph        5
# 5 12 mph      34
# 6 15           4
# 7 15 mph     150
# 8 16.09        1
# 9 20          13
# 10 20 mph   49513
# 11 25 mph       2
# 12 30          40
# 13 30 mph   24239
# 14 4 mph       12
# 15 40           1
# 16 40 mph    2528
# 17 5            8
# 18 5 mph      775
# 19 50           3
# 20 50 mph    1133
# 21 60           1
# 22 60 mph     138
# 23 64           1
# 24 7            4
# 25 70 mph     170
# 26 national     1
# 27 signals      7
# 28 variable     1
# 29 NA       98449

# drop observations with no speed limit data
os_speed_limits = os_relevent_highways %>%
  filter(!is.na(maxspeed)) # n = 79251

# drop observations with signals or variable in the maxspeed
os_speed_limits = os_speed_limits %>%
  filter(maxspeed != "variable") %>%
  filter(maxspeed != "signals") %>%
  filter(maxspeed != "national") %>%
  filter(maxspeed != "100") # n = 79241

# REmove mph and convert to integer 
os_speed_limits$maxspeed_num = sapply(str_split(os_speed_limits$maxspeed, " mph"), `[`,1) # works byt still have 10mph so run again
os_speed_limits$maxspeed_num = sapply(str_split(os_speed_limits$maxspeed, "mph"), `[`,1)
os_speed_limits$maxspeed_num = as.integer(os_speed_limits$maxspeed_num)

# Create speed limit groups
os_speed_limits = os_speed_limits %>%
  mutate(speed_limit = cut(maxspeed_num,
                           breaks = seq(0, 70, by = 10),
                           labels = c("10mph", "20mph", "30mph", "40mph", "50mph", "60mph", "70mph"))) 
tidy_speed_limit = os_speed_limits %>%
  st_drop_geometry() %>%
  group_by(speed_limit) %>%
  summarise(count = n())
# speed_limit count
# 1 10mph        1270
# 2 20mph       49715
# 3 30mph       24281
# 4 40mph        2529
# 5 50mph        1136
# 6 60mph         139
# 7 70mph         171

mapview(os_speed_limits, zcol = "speed_limit")


# Limit OS data to inside London Boundary
# import May 2020 ONS LA boundary data
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

# Create spatial object for all 33 boroughs
london_union = st_union(lon_lad_2020) 

# Limit road links to within the Outer London Boundary
lon_os_speed_limits = st_intersection(os_speed_limits, london_union) # n = 78757

lon_os_speed_limits_tidy = lon_os_speed_limits %>%
  select(c("osm_id", "name", "highway", "maxspeed_num", "speed_limit", "geometry"))

#  Now test how to join the speed limit data to the CID data
# select small amount of data
barnet = on_road_factor %>%
  filter(BOROUGH == "Barnet")

mapview(barnet, zcol = "Highest_separation")

barnet_speed_intersects = st_join(barnet, lon_os_speed_limits_tidy) # => 22 assets have speed limit data - this uses st_intersects
barnet_speed_nearest = st_join(barnet, lon_os_speed_limits_tidy, join = st_nearest_feature) #= all assets have speed limit data
barnet_speed_within = st_join(barnet, lon_os_speed_limits_tidy, join = st_nearest_feature)

mapview(barnet_speed_nearest, zcol = "Highest_separation") # this doesnt appear to work propertly


# now write some logic that checks to see if degree of separation matches the road speed limit.  
# use max speed nuermic from os

#40+ can onlyl be fully seg
# 30+ can be seg, step or par
# 20mph can be any

# if speed limit = 40+ then higest seg must be segregate 
# if speed limit - 30 + then seg must be seg step or part seg_levels
# if speed limit =20 then seg can be any value.  

#column = is seg approp to speed limit - do want t/f

barnet_speed_near_test = barnet_speed_nearest %>%
  mutate(seg_appro_speed_limit = case_when((maxspeed_num == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (maxspeed_num == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (maxspeed_num == 30) & (Highest_separation == "Stepped") ~ "TRUE",
                                           (maxspeed_num == 30) & (Highest_separation == "Part-segregated") ~ "TRUE",
                                           (maxspeed_num <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (maxspeed_num <= 20) & (Highest_separation == "Stepped") ~ "TRUE",
                                           (maxspeed_num <= 20) & (Highest_separation == "Part-segregated") ~ "TRUE",
                                           (maxspeed_num <= 20) & (Highest_separation == "Mandatory cycle lane") ~ "TRUE",
                                           (maxspeed_num <= 20) & (Highest_separation == "Advisory cycle lane") ~ "TRUE",
                                           (maxspeed_num <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           TRUE ~ "FALSE"))
# THis seems to work for barnet - need to check on other datasets

test = st_join(on_road_factor, lon_os_speed_limits_tidy, join = st_nearest_feature)

# Count the number of highways by type and percent of total count
highway_count = gl_osm_lines20 %>%
  st_drop_geometry() %>%
  group_by(highway) %>%
  summarise(count = n()) %>%
  mutate(percent = round(count/sum(count)*100)) %>%
  arrange(desc(percent), desc(count))






#########################################
#  Next steps:

# lengths - by borough summed by type
# ? any focus on seg/step/partseg

# ? heatmap of x axis speed, y axis borough, colour = proportion infrastructure that is segreg etc?



















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





# Want to code all 94 as stepped once check up on those coded as advisory ie stepp and advis = TRUE

seg_levels = c("Segregated", "Stepped", "Partially segregated", "Mandatory cycle lane", "Advisory cycle lane")

https://www.marsja.se/r-add-column-to-dataframe-based-on-other-columns-conditions-dplyr/
  
  # Create new variable that indicates degree of separation from traffic 
  # seg_levels = c("Segregated", "Stepped", "Partially segregated", "Mandatory cycle lane", "Advisory cycle lane")
  
  # # COnvert factor variables into numeric  
  # cols = c("CLT_SEGREG", "CLT_STEPP", "CLT_PARSEG", "CLT_MANDAT", "CLT_ADVIS") 
  # on_road_test = on_road %>% as.integer(as.character("CLT_SEGREG"))
  
  # try smaller dataset of on_road
  
  # test_df = on_road %>%
#   st_drop_geometry() %>%
#   select(c("FEATURE_ID", "CLT_SEGREG"))
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











# on_road_numeric = on_road_numeric %>%
#   rowwise() %>%
#   mutate(weight_3 = sum(c_across(CLT_SHARED_weight:CLT_PARKR_weight)))
# 
# unique(on_road_numeric$weight_3)
# #  0  20 200 220   2 202  22
# 
# weight_3_count = on_road_numeric %>%
#   st_drop_geometry() %>%
#   group_by(weight_3) %>%
#   summarise(count = n())
# sum(weight_3_count$count)  # n = 13965

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



