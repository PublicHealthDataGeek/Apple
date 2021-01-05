############################
# get CID data every month #
############################

# Need to update date each month

library(CycleInfraLnd)

asl = get_cid_lines(type = "advanced_stop_line")
crossings = get_cid_lines(type = "crossing")
cycle_lane_track = get_cid_lines(type = "cycle_lane_track")
restricted_route = get_cid_lines(type = "restricted_route")

signals = get_cid_points(type = "signal")
cycle_parking = get_cid_points(type = "cycle_parking")
restricted_point = get_cid_points(type = "restricted_point")
signage = get_cid_points(type = "signage")
traffic_calming = get_cid_points(type = "traffic_calming")

mypath = "/home/bananafan/Downloads/"
dataset_names = c("asl", "crossings", "cycle_lane_track", "restricted_route", "signals", "cycle_parking",
                  "restricted_point", "signage", "traffic_calming")
#date = paste0(Sys.Date(),"-CID")

date = "2021_01_03_CID_"

for(i in 1:length(dataset_names)) {
  saveRDS(get(dataset_names[i]),
          file = paste0(mypath, date,
                 dataset_names[i]))
}

# Check can read files in ok

x_asl = readRDS(paste0(mypath,date, "asl"))
        
        
       
