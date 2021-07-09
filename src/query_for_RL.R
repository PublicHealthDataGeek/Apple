###SCript for robin


# Import cleansed onroad cycle lanes dataframe that has segregation information coded (created in VISUALISE CYCLELANES.r)
cycle_lanes = readRDS(file = "data/cleansed_onroad_cyclelanes_segregation.Rds")

# collapse segregation into these categories that match Figure 4.1 in LTN 1/20
# Convert factored numbers to relevant labels
cycle_lanes = cycle_lanes %>%
  select(c(FEATURE_ID, BOROUGH, geometry, length_m, type, Highest_separation)) %>%
  mutate(Highest_separation = fct_collapse(Highest_separation, 
                                           "Stepped/part segregation" = c("Stepped", "Part-segregated"),
                                           "Mandatory/Advisory cycle lane" = c("Mandatory cycle lane", "Advisory cycle lane")))

# Import tidied london OSM speed limit dataset
osm_speed_limits_tidy = readRDS(file = "data/lon_osm_speed_limits_tidy.Rds")
# Refactor speed limit
osm_speed_limits_tidy$speed_limit = fct_collapse(osm_speed_limits_tidy$speed_limit, 
                                                 "20mph" = c("10mph", "20mph"))
# Manage road name = "NA"
sum(is.na(osm_speed_limits_tidy$name)) # n = 6452
osm_speed_limits_tidy$name[is.na(osm_speed_limits_tidy$name)] = "Unknown"
sum(is.na(osm_speed_limits_tidy$name)) # n = 0

# import May 2020 ONS LA boundary data
lon_lad_2020_bfe = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")




# Now test how to join the speed limit data to the CID data for BARNET
# select small amount of data
barnet_cl = cycle_lanes %>%
  filter(BOROUGH == "Barnet") # 93 observations of cycle lanes in Barnet
barnet_borough = lon_lad_2020_bfe %>%
  filter(BOROUGH == "Barnet") # obtain boundaries of Barnet
barnet_speeds_limits = st_intersection(osm_speed_limits_tidy, barnet_borough) # n = 4810, osm objects within Barnet borough

# Put 7.3m buffer around osm speed limit lines (allows for two way traffic on single carriageway)
barnet_sl_buffer_73 = st_buffer(barnet_speeds_limits, dist = 7.3) 

# Use an spatial join where the cycle lane has to be entirely within the speed limit buffer
barnet_intersects_within = st_join(barnet_cl, barnet_sl_buffer_73, join = sf::st_within) # n = 95
nrow(barnet_intersects_within) / nrow(barnet_cl) # 1.02 matches per CID cycleway

# How many of these new observations are not fully within an osm_id (ieNA) and therefore dont have a speed limit?
summary(is.na(barnet_intersects_within$osm_id)) # 35 NA so 60 cycle lanes are totally within an osm sl and have a speed limit

# Examining these observations that are not fully within an osm buffer
barnet_sl_touching_cl = barnet_sl_buffer_73[barnet_cl, ]  # n = 137
mapview(barnet_sl_touching_cl) + barnet_cl
# some are where side roads touch a main road
# some are where osm users have covered parts of the same road but in small overlapping chunks.  

# Get dataframe of unmatched cycle lanes
barnet_cl_unmatched_within = barnet_cl  %>%
  filter(FEATURE_ID %in% barnet_intersects_within$FEATURE_ID[is.na(barnet_intersects_within$speed_limit)]) # n= 35

# Which osm buffers are these touching??
barnet_sl_touching_cl_unmatched_within = barnet_sl_touching_cl[barnet_cl_unmatched_within, ] # n = 97

# How many of the unmatched cycle lanes are on the same roads? 
barnet_sl_touching_cl_unmatched_within %>%
  st_drop_geometry() %>%
  group_by(name) %>%
  summarise(number_of_times_roadname_appears = n()) %>%
  group_by(number_of_times_roadname_appears) %>%
  summarise(count = n())

# number_of_times_roadname_appears count
# 1    28
# 2    13
# 3     2   Burnt Oak Broadway, Hillside Avenue
# 4     4   Church Lane, Deansbrook Road, Friern Barnet Lane, The Hyde
# 5     1   High Road
# 6     1   West Hendon Broadway
# 10    1  This is for road name = Unknown

# Do the speed limits match for each road name? 
sl_match_road_name = barnet_sl_touching_cl_unmatched_within %>%
  st_drop_geometry() %>%
  group_by(name, speed_limit) %>% 
  summarise(count = n()) # some names appear more than once eg Colindeep lane has a 30 and 40mph

count_na_fx <- function(x) sum(is.na(x))  # function that counts number of NAs in a row
sl_match_road_name = sl_match_road_name %>%
  ungroup() %>%
  select(c("name", "speed_limit")) %>%
  pivot_wider(id_cols = c("name", "speed_limit"), 
              names_from = speed_limit,
              values_from = speed_limit,
              names_glue = "{.value}_{speed_limit}",
              values_fn = length) %>%
  select(c("name", "speed_limit_20mph", "speed_limit_30mph", "speed_limit_40mph", "speed_limit_50mph")) %>% # may need to add  "speed_limit_NA" for other boroughs
  mutate(count_nas = apply(., 1, count_na_fx)) %>%
  mutate(single_speed_limit = case_when(count_nas == 3  ~ TRUE,
                                        TRUE ~ FALSE))  # if there are 3 NAs and 1 speedlimit then set 'single_speed_limit' to TRUE, 
# if there are <3 NAs then this means >1 speed limit so set to FALSE

# How many roads have more that one speed limit? 3 roads
nrow(sl_match_road_name %>% filter(single_speed_limit == FALSE)) # n = 3
nrow(sl_match_road_name %>% filter(single_speed_limit == TRUE)) # n = 47

# if we take the road names where there is a single speed limit and can we add those to the segments that dont have speed limits using a spatial join (see minimal example below)
#get list of road names with single speed limit
true_road_names = as.data.frame(pull(sl_match_road_name %>% filter(single_speed_limit == TRUE), name))
true_road_names = true_road_names %>% rename(t_road_name = "pull(sl_match_road_name %>% filter(single_speed_limit == TRUE), name)") 


# Want to make the below minimal example a loop in order and run it over the true_road_names.  
high_road = barnet_sl_touching_cl_unmatched_within %>% filter(name == "High Road") # pull out osm_id that are on "High Road" n = 5
#mapview(high_road)
#nrow(high_road)
high_road_joined = high_road %>% 
  group_by(name, speed_limit) %>% 
  summarise() # groups the road name by speed limit, in this case all 30mph
#nrow(high_road_joined) 
cl_high_road = barnet_cl_unmatched_within[high_road, ] # pulls out the unmatched cl that have high road as the name using spatial subsetting
cl_high_road_joined = st_join(cl_high_road, high_road_joined, join = sf::st_within) # spatially joins CID data to road name  
#table(cl_high_road_joined$speed_limit) # now the two CID obs that are on High Road

