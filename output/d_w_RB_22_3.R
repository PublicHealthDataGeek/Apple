#####
#  Code needed to support discussion with Roger redecisions

# Crossings and Traffic signals

# Load datasets
crossings_1920_prop_rank_comparison = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/crossings_1910_prop_rank_comparison")
asl_tl_1920_prop_rank = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/asl_tl_1910_prop_rank_comparison")
signals_tl_1920_prop_rank = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/signals_tl_1910_prop_rank_comparison")


# Crossings
total_CID_crossings = sum(crossings_1920_prop_rank_comparison$Crossings) # n= 1690
total_crossings_2019 = sum(crossings_1920_prop_rank_comparison$Total_number_crossings19) # n = 12900
total_crossings_2020 = sum(crossings_1920_prop_rank_comparison$Total_number_crossings_20) # n = 16044

# can see difference in proportions - having > 100% for some boroughs!!!

# Traffic signals
total_traffic_signals_2019 = sum(asl_tl_1920_prop_rank$Total_number_traffic_signals19) # n = 5755
total_traffic_signals_2020 = sum(asl_tl_1920_prop_rank$Total_number_traffic_signals_20) # n = 7050

# Currently there are 6,416 traffic signals in the London. TFL FOI
# Transport for London (TfL)  is responsible for all traffic signals on the roads in the Greater London Authority area. 