################################################################################
# Create summaries and tables for Borough level data                           #
#                                                                              #  
# This file contains code for:                                                 #
# - obtaining borough level summary data on raw counts and length              #
# - creates tables for Borough level counts with ranks                         # 
# - creates a table with count/length and ranks                                #
# - creates a table that compares raw count and raw length rank for CLT and RR #
# - creates a table for assets that are safety data                            #
#                                                                              #
################################################################################

# install packages
library(tidyverse)
library(gt)


# Load datasets - these datasets were created 2_3_2021 from TFL datasets downloaded 25/2/21
CID_count = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/CID_count_by_borough")
CID_length = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/CID_length_by_borough")


# Add column for inner/outer london
Inner = c("City of London", "Camden", "Greenwich", "Hackney", "Hammersmith & Fulham", 
          "Islington", "Kensington & Chelsea", "Lambeth", "Lewisham", "Southwark",  
          "Tower Hamlets", "Wandsworth", "Westminster") 

CID_count = CID_count %>%
  mutate(London = ifelse(BOROUGH %in% Inner, "Inner London", "Outer London")) # 14 variables

CID_length = CID_length %>%
  mutate(London = ifelse(BOROUGH %in% Inner, "Inner London", "Outer London")) # 14 variables



# Create ranking for Count dataset
# rank each borough
# use rank(-.x) to get ranking so that borough with most assets has highest ranking
CID_count_rank = CID_count %>%
  mutate(across(.cols = c("ASL", "Crossings", 
                          "CycleLanesAndTracks", "RestrictedRoutes", 
                          "CycleParkingSites", "CycleParkingSpaces", 
                          "Signals", "TrafficCalming", "Signage", 
                          "Restricted_Points"),
                .fns = ~round(rank(-.x)), 
                .names = "{.col}_count_rank")) # now 22 variables

# mutate to get rank in with number ie n(rank)
CID_count_rank$ASL = paste(CID_count_rank$ASL, "(", CID_count_rank$ASL_count_rank, ")")
CID_count_rank$Crossings = paste(CID_count_rank$Crossings, "(", CID_count_rank$Crossings_count_rank, ")")
CID_count_rank$CycleLanesAndTracks = paste(CID_count_rank$CycleLanesAndTracks, "(", CID_count_rank$CycleLanesAndTracks_count_rank, ")")
CID_count_rank$RestrictedRoutes = paste(CID_count_rank$RestrictedRoutes, "(", CID_count_rank$RestrictedRoutes_count_rank, ")")
CID_count_rank$CycleParkingSites = paste(CID_count_rank$CycleParkingSites, "(", CID_count_rank$CycleParkingSites_count_rank, ")")
CID_count_rank$CycleParkingSpaces = paste(CID_count_rank$CycleParkingSpaces, "(", CID_count_rank$CycleParkingSpaces_count_rank, ")")
CID_count_rank$Signals = paste(CID_count_rank$Signals, "(", CID_count_rank$Signals_count_rank, ")")
CID_count_rank$TrafficCalming = paste(CID_count_rank$TrafficCalming, "(", CID_count_rank$TrafficCalming_count_rank, ")")
CID_count_rank$Signage = paste(CID_count_rank$Signage, "(", CID_count_rank$Signage_count_rank, ")")
CID_count_rank$Restricted_Points = paste(CID_count_rank$Restricted_Points, "(", CID_count_rank$Restricted_Points_count_rank, ")")

# Select columns I want to keep
CID_count_rank = CID_count_rank %>%
  select(c("BOROUGH", "ASL", "Crossings", 
           "CycleLanesAndTracks", "RestrictedRoutes", 
           "CycleParkingSites", "CycleParkingSpaces", 
           "Signals", "TrafficCalming", "Signage", 
           "Restricted_Points", "London"))



# Calculate summary statistics for asset type  
# a) Create smaller dataset and then add min, max, mean and sd
df = CID_count %>%
  select(-c(BOROUGH, London))
summ = df %>%
  summarise(across(everything(), list(min = min, max = max, median = median, 
                                      Q1 = ~quantile(., probs = 0.25), 
                                      Q3 = ~quantile(., probs = 0.75),
                                      mean = mean, sd = sd)))

# b) Pivot and manipulate to get data into asset, min, max, mean, sd column format
summ2 = summ %>% pivot_longer(
  everything(),
  names_to = c("Asset", ".value"),
  names_pattern = "(.*)_(.*)")

summ2 = summ2 %>%
  mutate_if(is.numeric, round) # round to whole numbers
summ2 = summ2 %>%
  mutate(across(.cols = c("mean", "sd"), round, 1))

# c) Create new columns for range plus mean(sd) then drop single columns (min, max, mean, sd)
summ2$Range = paste(summ2$min, "-", summ2$max)
summ2$Mean = paste(summ2$mean, "(", summ2$sd, ")")
summ2$MedianIQR = paste(summ2$median, "(", summ2$Q1, " - ", summ2$Q3, ")")
summ2 = summ2 %>%
  select(c(Asset, Range, Mean, MedianIQR))

# d) Rotate dataframe and change columns names so that it is in original table format 
summ3 = as.data.frame(t(summ2))  

names(summ3) <- summ3 %>% slice(1) %>% unlist() # change column labels to the ones I want rather than V1 etc
summ3 <- summ3 %>% slice(-1) # and drop the first row that had the column names in

# e) Add columns in order to match the original table plus use labels that will make sense with gt                 
summ3$BOROUGH = c("Range", "Mean(SD)", "Median(IQR)") 
summ3$London = c("Summary statistics", "Summary statistics", "Summary statistics")               
final_count_summ = summ3 %>%
  select(c("BOROUGH", "ASL", "Crossings", 
           "CycleLanesAndTracks", "RestrictedRoutes", 
           "CycleParkingSites", "CycleParkingSpaces", 
           "Signals", "TrafficCalming", "Signage", 
           "Restricted_Points", "London"))

# f) Bind summary statistics to original table and ensure factor order will make summary statistics at top
count_df = rbind(CID_count_rank, final_count_summ)
count_df$London = factor(count_df$London, levels = c("Summary statistics", "Inner London", "Outer London")) 


###############################################################################
# Create table with asset counts by Borough with ranks and summary statistics #
#                                                                             #
# Having manipulated data into df that enables gt to produce the table I want #
#... make the table                                                           #    
###############################################################################
count_summary_table = count_df %>%
  relocate(c(CycleParkingSites, CycleParkingSpaces), .after = last_col())  %>% 
  arrange(London) %>% 
  gt(rowname_col = "BOROUGH", 
     groupname_col = "London") %>%
  cols_align(align = "center") %>%
  tab_options(
    container.width = px(1300),
    table.width = pct(100)) %>%
  tab_stubhead(label = md("**Borough**")) %>%
  tab_header(
    title = md("**Infrastructure count by type and London Borough**"),
    subtitle = md("*Number (Borough rank)*, apart from Summary statistics section")) %>%
  tab_spanner(
    label = "Cycle Parking",
    columns = vars(CycleParkingSites,	CycleParkingSpaces),
    gather = TRUE) %>%
  cols_label(
    BOROUGH = "Borough", CycleLanesAndTracks = "Lanes and tracks",
    RestrictedRoutes = "Restricted routes", CycleParkingSites = "Sites",
    CycleParkingSpaces = "Spaces", TrafficCalming = "Traffic calming",
    Restricted_Points = "Restricted points") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = TRUE)) %>%
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_row_groups())
  
# To save  - save table as object then pipe to gtmsave
gtsave(
  count_summary_table,
  filename = "count_summary_table.png",
  vwidth = 1300,
  path = "/home/bananafan/Documents/PhD/Paper1/output"
)


############################################################################
# Creating count(rank) table but with length instead of count for lanes/RR #
#                                                                          #
# This code using similar method to create summary table where length      #
# rather than number(count) is used for lanes/tracks and restricted routes #
############################################################################           

# Get rank for CID length in km (some joint ranking as values withing 100m of each other in length)
CID_length_rank = CID_length %>%
  mutate(across(.cols = c("CycleLaneTrack_km", "RestrictedRoute_km"),
                .fns = ~round(rank(-.x)),
                .names = "{.col}_length_rank")) # now 8variables

# Mutate to get rank in with number
CID_length_rank$CycleLaneTrack_km = paste(CID_length_rank$CycleLaneTrack_km, "km (", CID_length_rank$CycleLaneTrack_km_length_rank, ")")
CID_length_rank$RestrictedRoute_km = paste(CID_length_rank$RestrictedRoute_km, "km (", CID_length_rank$RestrictedRoute_km_length_rank, ")")

CID_length_rank = CID_length_rank %>%
   select(c("BOROUGH", "CycleLaneTrack_km", "RestrictedRoute_km", "London"))

# Calculate summary statistics for asset type  
df = CID_length %>%
  select(c("CycleLaneTrack_km", "RestrictedRoute_km"))
summ = df %>%
  summarise(across(everything(), list(min = min, max = max, median = median, 
                                      Q1 = ~quantile(., probs = 0.25), 
                                      Q3 = ~quantile(., probs = 0.75),
                                      mean = mean, sd = sd)))
summ2 = summ %>% pivot_longer(
  everything(),
  names_to = c("Asset", ".value"),
  names_pattern = "(.*)_(.*)") 
summ2 = summ2 %>%
  mutate_if(is.numeric, round, 1)

# Create new columns for range plus mean(sd) then drop single columns (min, max, mean, sd)
summ2$Range = paste(summ2$min, "-", summ2$max, "km")
summ2$Mean = paste(summ2$mean, "km (", summ2$sd, "km)")
summ2$MedianIQR = paste(summ2$median, "km (", summ2$Q1, " - ", summ2$Q3, "km)")
summ2 = summ2 %>%
  select(c(Asset, Range, Mean, MedianIQR))

summ3 = as.data.frame(t(summ2))  # rotate dataframe to get back into same shape as table

names(summ3) <- summ3 %>% slice(1) %>% unlist() # change column labels to the ones I want rather than V1 etc
summ3 <- summ3 %>% slice(-1) # and drop the first row that had the column names in

# add columns in order to match the table plus use labels that will make sense with gt                 
summ3$BOROUGH = c("Range", "Mean(SD)", "Median(IQR)") 
summ3$London = c("Summary statistics", "Summary statistics", "Summary statistics")               
final_len_summ = summ3 

len_df = rbind(CID_length_rank, final_len_summ)
len_df$London = factor(len_df$London, levels = c("Summary statistics", "Inner London", "Outer London")) 

#####
# column bind count_df with length_df

count_len_summary_df = left_join(count_df, len_df) %>%
  select(-c("CycleLanesAndTracks", "RestrictedRoutes"))

# Create table
count_leng_summary_table = count_len_summary_df %>%
  relocate(c(CycleParkingSites, CycleParkingSpaces), .after = last_col())  %>% 
  arrange(London) %>% 
  gt(rowname_col = "BOROUGH", 
     groupname_col = "London") %>%
  cols_align(align = "center") %>%
  tab_options(
    container.width = px(1350),
    table.width = pct(100)) %>%
  tab_stubhead(label = md("**Borough**")) %>%
  tab_header(
    title = md("**Infrastructure by type and London Borough**"),
    subtitle = md("*Number or length with Borough rank in brackets* except for Summary statistics section")) %>%
  tab_spanner(
    label = "Cycle Parking",
    columns = vars(CycleParkingSites,	CycleParkingSpaces),
    gather = TRUE) %>%
  cols_label(
    BOROUGH = "Borough", CycleLaneTrack_km = "Lanes and tracks",
    RestrictedRoute_km = "Restricted routes", CycleParkingSites = "Sites",
    CycleParkingSpaces = "Spaces", TrafficCalming = "Traffic calming",
    Restricted_Points = "Restricted points") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = TRUE)) %>%
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_row_groups())

# To save  - save table as object then pipe to gtmsave
gtsave(
  count_leng_summary_table,
  filename = "count_leng_summary_table.png",
  vwidth = 1350,
  path = "/home/bananafan/Documents/PhD/Paper1/output"
)




###########################################################################################
# Table showing comparison between count and length for Cycle lanes and Restricted routes #
#

clt_rr_ranks = left_join(CID_count_rank, CID_length_rank) %>%
  select(c("BOROUGH", "CycleLanesAndTracks", "RestrictedRoutes",
           "CycleLaneTrack_km", "RestrictedRoute_km"))
saveRDS(clt_rr_ranks, file = "/home/bananafan/Documents/PhD/Paper1/data/clt_rr_rankcomparison")

###################################################################################################



############################################################################
# Creating count/length (rank) table for safety infrastructure             #
#                                                                          #
# This code using similar method to create summary table where length      #
# rather than number(count) is used for lanes/tracks and restricted routes #
############################################################################           

safety_df = count_len_summary_df %>%
  select(c("BOROUGH", "ASL", "Crossings", "Signals", "TrafficCalming",
           "London", "CycleLaneTrack_km"))

# Create table
safety_summary_table = safety_df %>%
  arrange(London) %>% 
  gt(rowname_col = "BOROUGH", 
     groupname_col = "London") %>%
  cols_align(align = "center") %>%
  tab_options(
    container.width = px(950),
    table.width = pct(100)) %>%
  #tab_stubhead(label = md("**Borough**")) %>%
  tab_header(
    title = md("**Safety infrastructure by type and London Borough**"),
    subtitle = md("*Number or length with Borough rank in brackets except for Summary statistics section*")) %>%
  cols_label(
    BOROUGH = "Borough", CycleLaneTrack_km = "Lanes and tracks",
    TrafficCalming = "Traffic calming") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups())

# To save  - save table as object then pipe to gtmsave
gtsave(
  safety_summary_table,
  filename = "safety_summary_table.png",
  vwidth = 950,
  path = "/home/bananafan/Documents/PhD/Paper1/output"
)


