##################################################################################################
# Issue 1: issue with cycle lane length not matching dissertation
# 
# Date: 5_1_2021
#
# Issue summary: initial data cleansing and then calculation of length of cycle lanes
# by Borough gave different total lengths compared to my dissertation
# 
# Script action:
# This script takes both original and new crs and checks that there is no substantial difference
# between lengths calculated by using the two different CRS projections
#
# Finding: no sig difference between two CRS projections.  Findings match those of dissertation
# Need to reexamine 'data_clean_CID' code to find mistake in there.
# 
# Final finding: in this code:
# cycle_lane_track_borough_length = f_cycle_lane_track %>%
#+   st_drop_geometry() %>%
#  +   group_by(BOROUGH) %>%
#  +   summarise(length = sum(round(length_km)))
# the st_drop_geometry does something weird.  Need to do the st_drop_geometry outside of this 
# eg. cycle_lane_track_borough_length = st_drop_geometry(cycle_lane_track_borough_length)
##################################################################################################


original_crs = get_cid_lines(type = "cycle_lane_track") # n = 24976

# current original crs
st_crs(original_crs) # CRS = WGS 84

#created new dataframe with crs 27700
changed_crs = st_transform(original_crs, crs=27700) 
st_crs(changed_crs) # PROJCRS["OSGB 1936 / British National Grid",

# calculate lengths
original_crs$length_o = st_length(original_crs$geometry)
original_crs$length_km_o = set_units(st_length(original_crs$geometry), km)

changed_crs$length_c = st_length(changed_crs$geometry)
changed_crs$length_km_c = set_units(st_length(changed_crs$geometry), km)

original_crs = st_drop_geometry(original_crs)
changed_crs = st_drop_geometry(changed_crs)
length_comparison = left_join(original_crs, changed_crs) %>%
  select(FEATURE_ID, BOROUGH, length_c, length_o, length_km_o, length_km_c)
# this shows the lengths are the same

# so now calculate grouped lengths by Borough

original_crs_borough_length = original_crs %>%
  group_by(BOROUGH) %>%
  summarise(across(c(length_o, length_km_o), sum))


changed_crs_borough_length = changed_crs %>%
  group_by(BOROUGH) %>%
  summarise(across(c(length_c, length_km_c), sum))

# Compare original Crs dataframe with changed crs dataframe
comparison_borough_length = left_join(original_crs_borough_length, changed_crs_borough_length)

# calculate discrepancy in length between the two crs sets
comparison_borough_length$discrepancy = comparison_borough_length$length_o - comparison_borough_length$length_c
  










