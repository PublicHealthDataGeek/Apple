####################################################################################
#                            Visualise cycle lanes 
# 
# This code aims to visualise on road cycle lanes by the degree of separation
# from other road users.  
#
# Initial code sorts out the assets that have more than one label for separation
# e.g. jointly labelled as being segregated and stepped.  


#Load packages
library(tidyverse)
library(mapview)
library(tmap)
library(sf)
library(tmaptools) # for palette explorer 
library(viridis)

# Package options
mapviewOptions(native.crs = TRUE)
tmap_design_mode(design.mode = FALSE)


#######################################################################
# Load and manipulate Local Authority spatial data for visualisations #
#######################################################################

# import May 2020 ONS LA boundary data clipped to coastline (used so that River Thames appears)
lon_lad_2020_c2c = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFC.Rds")

# simply borough shapes
lon_lad_2020_c2c <- rmapshaper::ms_simplify(lon_lad_2020_c2c, keep=0.015) #Simplify boroughs

# Add three letter acronym column
lon_lad_2020_c2c$b_acronym = fct_recode(lon_lad_2020_c2c$BOROUGH, 
                                     "K&C" = "Kensington & Chelsea",
                                     "B&D" = "Barking & Dagenham",
                                     "H&F" = "Hammersmith & Fulham",
                                     "Kin" = "Kingston upon Thames",
                                     "Ric" = "Richmond upon Thames",
                                     "City" = "City of London",
                                     "Wal" = "Waltham Forest",
                                     "Cro" = "Croydon",
                                     "Bro" = "Bromley",
                                     "Hou" = "Hounslow",
                                     "Eal" = "Ealing",
                                     "Hav" = "Havering",
                                     "Hil" = "Hillingdon",
                                     "Har" = "Harrow",
                                     "Bre" = "Brent",
                                     "Bar" = "Barnet",
                                     "Lam" = "Lambeth",
                                     "Sou" = "Southwark", 
                                     "Lew" = "Lewisham",
                                     "Gre" = "Greenwich",
                                     "Bex" = "Bexley",
                                     "Enf" = "Enfield",
                                     "Red" = "Redbridge",
                                     "Sut" = "Sutton",
                                     "Mer" = "Merton",
                                     "Wan" = "Wandsworth",
                                     "Wes" = "Westminster",
                                     "Cam" = "Camden",
                                     "Tow" = "Tower Hamlets",
                                     "Isl" = "Islington",
                                     "Hac" = "Hackney",
                                     "Har" = "Haringey",
                                     "New" = "Newham")


# Select variables of interest
lon_lad_2020_c2c = lon_lad_2020_c2c %>%
  select(c("BOROUGH", "b_acronym", "geometry"))


###########################################################################################
# Import and manipulate london_squared dataset to enable spatial arrangement of barcharts #
###########################################################################################

# source data  = "https://github.com/aftertheflood/londonsquared/blob/master/site/data/grid.csv"

# Import london squared dataset
london_squared = read.table(file = "/home/bananafan/Downloads/londonsquared.txt", header = TRUE, sep = ",")

# Rename columns
london_squared$fY <-london_squared$y
london_squared$fX <-london_squared$x

# Helper function for rescaling
map_scale <- function(value, min1, max1, min2, max2) {
  return  (min2+(max2-min2)*((value-min1)/(max1-min1)))
}

# Manipulate london_squared layout for use in ggplot2 facet_grid().
max_y <- max(london_squared$fY)
min_y <- min(london_squared$fY)

london_squared <- london_squared %>% 
  mutate(fY=map_scale(fY, min_y, max_y, max_y, min_y))
rm(min_y,max_y)

# Relabel values in london_squared so can join to Boroughs
london_squared$BOROUGH <- london_squared$name
london_squared$BOROUGH = as.factor(london_squared$BOROUGH) 
london_squared$BOROUGH = fct_recode(london_squared$BOROUGH, 
                                    "Kensington & Chelsea" = "Kensington and Chelsea", 
                                    "Barking & Dagenham" = "Barking and Dagenham",
                                    "Hammersmith & Fulham" = "Hammersmith and Fulham")                            

london_squared$b_acronym = fct_recode(london_squared$BOROUGH, 
                                        "K&C" = "Kensington & Chelsea",
                                        "B&D" = "Barking & Dagenham",
                                        "H&F" = "Hammersmith & Fulham",
                                        "Kin" = "Kingston upon Thames",
                                        "Ric" = "Richmond upon Thames",
                                        "City" = "City of London",
                                        "Wal" = "Waltham Forest",
                                        "Cro" = "Croydon",
                                        "Bro" = "Bromley",
                                        "Hou" = "Hounslow",
                                        "Eal" = "Ealing",
                                        "Hav" = "Havering",
                                        "Hil" = "Hillingdon",
                                        "Har" = "Harrow",
                                        "Bre" = "Brent",
                                        "Bar" = "Barnet",
                                        "Lam" = "Lambeth",
                                        "Sou" = "Southwark", 
                                        "Lew" = "Lewisham",
                                        "Gre" = "Greenwich",
                                        "Bex" = "Bexley",
                                        "Enf" = "Enfield",
                                        "Red" = "Redbridge",
                                        "Sut" = "Sutton",
                                        "Mer" = "Merton",
                                        "Wan" = "Wandsworth",
                                        "Wes" = "Westminster",
                                        "Cam" = "Camden",
                                        "Tow" = "Tower Hamlets",
                                        "Isl" = "Islington",
                                        "Hac" = "Hackney",
                                        "Har" = "Haringey",
                                        "New" = "Newham")


# simplify dataset for joining
london_squared_tidy = london_squared %>%
  select(c("BOROUGH", "b_acronym", "fY", "fX"))



##############################################################################
# Import cycle lane data and manipulate to enable analysis and visualisation #
##############################################################################

# Import Cycle Lanes and Tracks dataset 
c_cyclelanetrack = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_cycle_lane_track")
# n = 25315

# Details of variables
# CLT_CARR   on/off
# CLT_SEGREG lanes or tracks - TRUE = physical separation of cyclist from other users using continuous/near continuous kerb, upstand or planted verge.
# CLT_STEPP - onroad only, cycle lane/track is at a level between the carriageway and footway 
# CLT_PARSEG - on and off, on - involves objects eg wants, off is a visual feature eg white line
# TRUE = additional forms of delineation NB as often used by Mand/Adv cycle lanes can have this set to TRUE and mand/advi set to TRUE
# CLT_SHARED - on and off, on - TRUE = shared with bus lane, off - TRUE = shared with other users eg pedestrians
# CLT_MANDAT - onroad only, TRUE = mandatory cycle lane
# CLT_ADVIS - onroad only, TRUE = advisory cycle lane
# NB onroad can only have MAND TRUE, ADVIS TRUE or both FALSE

# Order of Protection from motor traffic on highways (DFT guidance pg 33)
# Fully kerbed > stepped > light segregation > Mandatory/Advisory
# FK/S/LS suitable for most people at 20/30 mph only FK suitable for 40mph+
# M/A only suitable for most poepl on 20mph roads with motor vehicle flow of <5000

# Correspond in CID to:
# CLT_SEGREG > CLT_STEPP > CLT_PARSEG > CLT_MANDAT > CLT_ADVIS
#  NB seems to be little difference in CID between SEGREG and STEPP - majority of 
# stepped are also labelled as segreg and only 5 are labelled as just stepped and they
# look very similar to those that are segreg in the photos

# Limit CID to on road infrastructure only 
on_road = c_cyclelanetrack %>%
  filter(CLT_CARR == TRUE) # n = 13965

# Create dataframe so can obtain a df summary of number of observations
# on_road_drop = on_road %>%
#   st_drop_geometry() %>%
#   select(-c("length_m", "length_km"))
#view(dfSummary(on_road_drop))

# seg = 1371
# stepped = 94
# part seg = 349
# manda = 1854
# advi = 7273
# shared = 2845
# NB total is 13965 - what are the rest of the on road cycle lanes????


# Create new variable that divides observations into shared, contraflow and rest as these seem to have different patterns of segregation
on_road = on_road %>%
  mutate(type = case_when(CLT_SHARED == TRUE ~ "Shared",
                          CLT_CONTRA == TRUE ~ "Contraflow",
                          TRUE ~ "Rest"))
on_road$type = factor(on_road$type, levels = c("Rest", "Shared", "Contraflow"))

on_road %>%
  st_drop_geometry() %>%
  group_by(type) %>%
  summarise(count = n(), sum_length = sum(length_km))
# type       count sum_length
# <fct>      <int>       [km]
# 1 Rest        9685       595.
# 2 Shared      2845       236.
# 3 Contraflow  1435       112.


# Convert Factors to numeric
# e.g.  CLT_SEGREG: Factor w/ 2 levels "FALSE","TRUE": 1 1 1 1 1 1 1 1 1 1 ...
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

# Check now gives the count that I expect
sum(on_road_numeric$CLT_SEGREG_NUMERIC) # n = 1371
sum(on_road_numeric$CLT_STEPP_NUMERIC) # n = 94
sum(on_road_numeric$CLT_PARSEG_NUMERIC) # n = 349
sum(on_road_numeric$CLT_MANDAT_NUMERIC) # n = 1854
sum(on_road_numeric$CLT_ADVIS_NUMERIC) # n = 7273

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

#unique(on_road_numeric$weight_5)
# 1    10   100 10000   110     0   101 11000  1001 10010  1000 10001

# on_road_numeric %>%
#   st_drop_geometry() %>%
#   group_by(weight_5) %>%
#   summarise(count = n())

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
# Factor weight_5
on_road_factor = on_road_numeric %>%
  mutate(Highest_separation = factor(weight_5))
rm(on_road_numeric) # remove this dataframe to avoid confusion

# Convert factored numbers to relevant labels
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
on_road_factor %>%
  st_drop_geometry() %>%
  group_by(Highest_separation) %>%
  summarise(count = n(), sum_length = sum(length_km))

# Highest_separation   count sum_length
# <fct>                <int>       [km]
# 1 Segregated            1371     39.2  
# 2 Stepped                  5      0.713
# 3 Part-segregated        349     15.7  
# 4 Mandatory cycle lane  1672     85.3  
# 5 Advisory cycle lane   7196    487.0   
# 6 No separation         3372    316.1 


# Create datasets by type
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
  summarise(rest_count = n(), sum_rest_length_km = sum(length_km))
contra_sep = contra %>%
  st_drop_geometry() %>%
  group_by(Highest_separation) %>%
  summarise(contra_count = n(), sum_contra_length_km = sum(length_km))
shared_sep = shared %>%
  st_drop_geometry() %>%
  group_by(Highest_separation) %>%
  summarise(shared_count = n(), sum_shared_length_km = sum(length_km))
summary_high_sep = left_join(rest_sep, contra_sep) %>%
  left_join(shared_sep) %>%
  units::drop_units()
summary_high_sep[is.na(summary_high_sep)] <- 0


# Highest_separation   rest_count sum_rest_length_km contra_count sum_contra_length_km shared_count sum_shared_length_km
# <fct>                     <int>              <dbl>        <int>                <dbl>        <int>                <dbl>
# 1 Segregated                  976             33.2            393                 5.75            2                0.268
# 2 Stepped                       5              0.713            0                 0               0                0    
# 3 Part-segregated             273             12.2             72                 3.25            4                0.288
# 4 Mandatory cycle lane       1501             74.4            165                 9.93            6                0.924
# 5 Advisory cycle lane        6877            464.             283                20.8            36                2.48 
# 6 No separation                53             11.0            522                72.7          2797              232.   

rm(rest, rest_sep, shared, shared_sep, contra, contra_sep) # remove redundant objects

# Save onroad cycle lane dataset for use with LTN 1/20 analysis
saveRDS(on_road_factor, file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_onroad_cyclelanes_segregation.Rds")

##########################################################################
# Create maps of cycle lanes coloured by degrees of separation #
##########################################################################

# Separation map
tm_shape(lon_lad_2020_c2c) +
  tm_polygons(col = "gray98", border.col = "gray70") +
  tm_shape(on_road_factor) +
  tm_lines("Highest_separation",
           palette = "viridis",
           lwd = 2.5, 
           alpha = 0.75, legend.col.show = FALSE) +
  tm_layout(frame = FALSE)




# Plot facetted by type
on_road_factor = on_road_factor %>%
  mutate(type = fct_relevel(type, c("Shared", "Contraflow", "Rest")))

tm_shape(lon_lad_2020_c2c) +
  tm_polygons(col = "gray98", border.col = "gray70") +
  tm_shape(on_road_factor) +
  tm_lines("Highest_separation",
           palette = "viridis",
           lwd = 2.5, 
           alpha = 0.75,
           title.col = "Degree of separation") +
  tm_facets(by = "type",
        free.coords = FALSE, ncol = 2)


####################################################################
# Generating borough level data on lengths by degree of separation #
# and visualisations                                              #
#    PART 1 - all assets together                                  #
####################################################################

# Create variable for length by Highest Separation by Borough
borough_separation_length = on_road_factor %>%
  st_drop_geometry() %>%
  group_by(BOROUGH, Highest_separation) %>%
  summarise(total_length = sum(length_m))

# Create variable for total length by Borough of all on road cycle lanes - needed for inset barchart
total_borough_separation_length = on_road_factor %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(total_borough_length = sum(length_m))# max = Croydon 58331.287 [m], min = Sutton 5832.761 [m]

# remove units to allow plotting
borough_separation_length$total_length = as.integer(round(units::drop_units(borough_separation_length$total_length)))
total_borough_separation_length$total_borough_length = as.integer(round(units::drop_units(
  total_borough_separation_length$total_borough_length)))

# join total borough lengths to borough_separation_length
borough_separation_length = left_join(borough_separation_length, total_borough_separation_length)

# Reorder factors so Segregated is 'highest' value
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


####################################################################
# Generating borough level data on lengths by degree of separation #
# and visualisations                                               #
#  PART 2 - by rest, contra and shared                             #
####################################################################



#####NEED TO UPDATE BELOW CODE ONCE HAVE GOT AGREEMENT ON THE ABOVE SPATIAL BAR PLOT STYLE


# Create variable for length by Highest Separation by Borough for each type
type_borough_separation_length = on_road_factor %>%
  st_drop_geometry() %>%
  group_by(BOROUGH, type, Highest_separation) %>%
  summarise(total_length_by_type = sum(length_m))

total_type_borough_separation_length = on_road_factor %>%  # needed for insert bar chart
  st_drop_geometry() %>%
  group_by(BOROUGH, type) %>%
  summarise(total_borough_length_by_type = sum(length_m))#  

# remove units to allow plotting
type_borough_separation_length$total_length_by_type = as.integer(round(units::drop_units(
  type_borough_separation_length$total_length_by_type)))
total_type_borough_separation_length$total_borough_length_by_type = as.integer(round(units::drop_units(
  total_type_borough_separation_length$total_borough_length_by_type)))

# join total borough lengths to borough_separation_length
type_borough_separation_length = left_join(type_borough_separation_length, total_type_borough_separation_length)

# Reorder factors so Segregated is 'highest' value
type_borough_separation_length_reorder <- type_borough_separation_length # reorder so that Segregated appears at top
type_borough_separation_length_reorder$Highest_separation = fct_rev(type_borough_separation_length_reorder$Highest_separation)

# Create stacked bar charts for each type faceted by Borough
type_borough_separation_length_reorder %>%
  filter(type == "Rest") %>%
  ggplot() +
  geom_bar(aes(x = -Highest_separation, y = total_length_by_type, fill = Highest_separation), stat = "identity") +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, direction = -1, guide = guide_legend(reverse = TRUE)) +
  facet_wrap(~ BOROUGH) +
  labs(title = "Rest") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

type_borough_separation_length_reorder %>%
  filter(type == "Shared") %>%
  ggplot() +
  geom_bar(aes(x = -Highest_separation, y = total_length_by_type, fill = Highest_separation), stat = "identity") +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, direction = -1, guide = guide_legend(reverse = TRUE)) +
  facet_wrap(~ BOROUGH) +
  labs(title = "Shared") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

type_borough_separation_length_reorder %>%
  filter(type == "Contraflow") %>%
  ggplot() +
  geom_bar(aes(x = -Highest_separation, y = total_length_by_type, fill = Highest_separation), stat = "identity") +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, direction = -1, guide = guide_legend(reverse = TRUE)) +
  facet_wrap(~ BOROUGH) +
  labs(title = "Contraflow") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())



###############################################################
# Create bar charts spatially located by Borough + standalone #
###############################################################

############################
# PART 1 - all cycle lanes #
############################


# Join london_squared to cycle lane lengths
borough_separation_length_spatial = left_join(borough_separation_length_reorder, london_squared_tidy, by = "BOROUGH")

# basic plot works ok - keep as gives the basic layout
ggplot(borough_separation_length_spatial, aes(x = -Highest_separation, y = total_length, fill = Highest_separation)) +
  geom_bar(stat = "identity")+
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, direction = -1, guide = guide_legend(reverse = TRUE)) +
  facet_grid(-fY ~ fX) +  # need to do -fY to get correct orientation with enfield top row and sutton bottom row
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.spacing=unit(-0.5, "lines"))  # THis works in a basic way. 

#fiddling to make look better
ggplot(borough_separation_length_spatial, aes(x = -Highest_separation, y = total_length, fill = Highest_separation)) +
  geom_bar(stat = "identity") +  
  coord_flip() +
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
        plot.title = element_text(hjust = 0.5))

# Code developed with Roger THIS WORKS
# borough_separation_length_spatial %>%
#   ungroup() %>%
#   mutate(total_length2 = total_length/max(total_length)) %>%
#   ggplot() +
#   geom_rect(data=.%>% filter(Highest_separation == "No separation"), xmin = 0.2, xmax = 1.8, ymin = -0.2, ymax = 1.5, fill = "#aeaeae") +
#   geom_bar(aes(x = -Highest_separation, y = total_length2, fill = Highest_separation), stat = "identity") +
#   geom_text(data=.%>% filter(Highest_separation == "No separation"), x = 1, y = 0.5, aes(label = Borough_number)) +
#   coord_flip() +
#   scale_fill_viridis(discrete = TRUE, direction = -1, guide = guide_legend(reverse = TRUE)) +
#   facet_grid(-fY ~ fX) +  # need to do -fY to get correct orientation with enfield top row and sutton bottom row
#   theme(axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         panel.spacing=unit(-0.5, "lines")) +
#   theme_void()

# Amended roger code - panel spacing may not be right depending on saving size
borough_separation_length_spatial %>%
  ungroup() %>%
  mutate(total_length2 = total_length/max(total_length)) %>%
  ggplot() +
  geom_rect(data=.%>% filter(Highest_separation == "No separation"), xmin = 0.5, xmax = 1.5, ymin = -0.2, ymax = 1.5, fill = "#cdcdcd") +
  geom_bar(aes(x = -Highest_separation, y = total_length2, fill = Highest_separation), stat = "identity") +
  geom_text(data=.%>% filter(Highest_separation == "No separation"), x = 1, y = 0.7, aes(label = Borough_number)) +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, direction = -1, guide = guide_legend(reverse = TRUE)) +
  facet_grid(-fY ~ fX) +  # need to do -fY to get correct orientation with enfield top row and sutton bottom row
  theme_void() +
  theme(strip.text = element_blank(),
        panel.spacing.y = unit(-0.8, "lines"))

# Amended roger code with 3 letter acronyms - panel spacing may not be right depending on saving size
borough_separation_length_spatial %>%
  ungroup() %>%
  mutate(total_length2 = total_length/max(total_length)) %>%
  ggplot() +
  geom_rect(data=.%>% filter(Highest_separation == "No separation"), xmin = 0.5, xmax = 1.5, ymin = -0.2, ymax = 1.5, fill = "#cdcdcd") +
  geom_bar(aes(x = -Highest_separation, y = total_length2, fill = Highest_separation), stat = "identity") +
  geom_text(data=.%>% filter(Highest_separation == "No separation"), x = 1, y = 0.7, aes(label = b_acronym)) +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, direction = -1, guide = guide_legend(reverse = TRUE), name = "") +
  facet_grid(-fY ~ fX) +  # need to do -fY to get correct orientation with enfield top row and sutton bottom row
  theme_void() +
  theme(strip.text = element_blank(),
        panel.spacing.y = unit(-0.8, "lines"))


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



############################
# PART 2 - by type   lanes #
############################


# Join london_squared to cycle lane lengths by type
type_borough_separation_length_spatial = left_join(type_borough_separation_length_reorder, london_squared_tidy, by = "BOROUGH")

# FOr Rest
type_borough_separation_length_spatial %>%
  filter(type == "Rest") %>%
  ggplot() +
  geom_bar(aes(x = -Highest_separation, y = total_length_by_type, fill = Highest_separation), stat = "identity")+
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, direction = -1, guide = guide_legend(reverse = TRUE)) +
  facet_grid(-fY ~ fX) +  # need to do -fY to get correct orientation with enfield top row and sutton bottom row
  labs(title = "Rest") +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),axis.line = element_blank(),
        axis.title = element_blank(),
        strip.text = element_blank(),
        panel.spacing=unit(-0.5, "lines"),
        plot.title = element_text(hjust = 0.5))

# FOr SHared
type_borough_separation_length_spatial %>%
  filter(type == "Shared") %>%
  ggplot() +
  geom_bar(aes(x = -Highest_separation, y = total_length_by_type, fill = Highest_separation), stat = "identity")+
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, direction = -1, guide = guide_legend(reverse = TRUE)) +
  facet_grid(-fY ~ fX) +  # need to do -fY to get correct orientation with enfield top row and sutton bottom row
  labs(title = "Shared") +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),axis.line = element_blank(),
        axis.title = element_blank(),
        strip.text = element_blank(),
        panel.spacing=unit(-0.5, "lines"),
        plot.title = element_text(hjust = 0.5))

# FOr COntraflow
type_borough_separation_length_spatial %>%
  filter(type == "Contraflow") %>%
  ggplot() +
  geom_bar(aes(x = -Highest_separation, y = total_length_by_type, fill = Highest_separation), stat = "identity")+
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, direction = -1, guide = guide_legend(reverse = TRUE)) +
  facet_grid(-fY ~ fX) +  # need to do -fY to get correct orientation with enfield top row and sutton bottom row
  labs(title = "Contraflow") +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),axis.line = element_blank(),
        axis.title = element_blank(),
        strip.text = element_blank(),
        panel.spacing=unit(-0.5, "lines"),
        plot.title = element_text(hjust = 0.5))

# Create bar chart (like for the assets) for total length by borough faceted by type
ggplot(type_borough_separation_length_spatial) +
  geom_bar(aes(x = reorder(Borough_number, -total_borough_length_by_type), y = total_length_by_type, fill = Highest_separation),
           stat = "identity") +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, direction = -1, guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(limits = c(0, 62000), expand = c(0,0), breaks = c(20000, 40000, 60000)) +
  theme_classic() +
  facet_wrap(~ type) +
  labs(y = "Total length (km)", x = NULL) +
  theme(axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        legend.position = "none")

# Create individual bar charts for each type
type_borough_separation_length_spatial %>%
  filter(type == "Rest") %>%
  ggplot() +
  geom_bar(aes(x = reorder(Borough_number, -total_borough_length_by_type), y = total_length_by_type, fill = Highest_separation),
           stat = "identity") +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, direction = -1, guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(limits = c(0, 50000), expand = c(0,0), breaks = c(20000, 40000)) +
  theme_classic() +
  labs(y = "Total length (km)", x = NULL, title = "Rest") +
  theme(axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        legend.position = "none")

type_borough_separation_length_spatial %>%
  filter(type == "Shared") %>%
  ggplot() +
  geom_bar(aes(x = reorder(Borough_number, -total_borough_length_by_type), y = total_length_by_type, fill = Highest_separation),
           stat = "identity") +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, direction = -1, guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(limits = c(0, 25000), expand = c(0,0), breaks = 20000) +
  theme_classic() +
  labs(y = "Total length (km)", x = NULL, title = "Shared") +
  theme(axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        legend.position = "none")

type_borough_separation_length_spatial %>%
  filter(type == "Contraflow") %>%
  ggplot() +
  geom_bar(aes(x = reorder(Borough_number, -total_borough_length_by_type), y = total_length_by_type, fill = Highest_separation),
           stat = "identity") +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, direction = -1, guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(limits = c(0, 12500), expand = c(0,0), breaks = 10000) +
  theme_classic() +
  labs(y = "Total length (km)", x = NULL, title = "Contraflow") +
  theme(axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        legend.position = "none")






















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



