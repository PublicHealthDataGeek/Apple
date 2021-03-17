###############################################################
# What has changed since Feb 2020 in CID?                     #
#                                                             #
# This pulls down most recent CID data                        #
# and examines to see what data has been added since Feb 2020 #
#                                                             #
# This was run on 15/12/20                                    #
###############################################################

library(CycleInfraLnd)
library(tidyverse)
library(sf)

asl_12 = get_cid_lines(type = "advanced_stop_line")
crossings_12 = get_cid_lines(type = "crossing")
cycle_lane_track_12 = get_cid_lines(type = "cycle_lane_track")
restricted_route_12 = get_cid_lines(type = "restricted_route")
signals_12 = get_cid_points(type = "signal")
cycle_parking_12 = get_cid_points(type = "cycle_parking")
restricted_point_12 = get_cid_points(type = "restricted_point")
signage_12 = get_cid_points(type = "signage")
traffic_calming_12 = get_cid_points(type = "traffic_calming")


summary(asl_12$SVDATE) # Min "2017-01-06" Max "2018-08-01" 
summary(crossings_12$SVDATE) # Min 2017-05-31 Max "2019-09-02"
summary(cycle_lane_track_12$SVDATE) # Min 2017-01-06   Max "6482-04-01"
summary(restricted_route_12$SVDATE) # Min 2017-06-04   Max "2018-08-06"
summary(signals_12$SVDATE) # Min 2017-02-06   Max "2018-05-15"
summary(cycle_parking_12$SVDATE) # Min 2017-01-06   Max "2019-06-10"
summary(restricted_point_12$SVDATE) # Min 2017-06-13   Max "2018-08-21"
summary(signage_12$SVDATE) # Min 2017-01-06   Max "2018-08-21"
summary(traffic_calming_12$SVDATE) # Min 2017-01-06   Max "2018-08-21"

unique(cycle_lane_track_12$SVDATE)
non_geom_cycle_lane_track_12 = st_drop_geometry(cycle_lane_track_12)
x = non_geom_cycle_lane_track_12 %>%
  count(SVDATE)

# So cycle_lane_track has 1 obs with "6482-04-01", the latest obs is "6482-04-01"

# Looking at the number of observations these are identical to the number of observations I used in my 
# dissertation.  Also the date ranges also match what I used in my dissertation
