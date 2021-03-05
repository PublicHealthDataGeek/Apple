##############################################################
# Create tables for Borough level summary data #
#
#

####SOME ACTIONS TO TIDY THINGS UP BUT ALSO PROBABLY WANT TO DROP COUNTS FOR lanes and RR and instead use length
# ? do this as separate code and have just counts as appendix in case want to use that again???



# install packages
library(tidyverse)
library(gt)

#library(sf)
#library(mapview)
#library(units)

# Load datasets - these datasets were created 2_3_2021 from TFL datasets downloaded 25/2/21
CID_count = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/CID_count_by_borough")
CID_length = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/CID_length_by_borough")


# add column for inner/outer london
Inner = c("City of London", "Camden", "Greenwich", "Hackney", "Hammersmith & Fulham", 
          "Islington", "Kensington & Chelsea", "Lambeth", "Lewisham", "Southwark",  
          "Tower Hamlets", "Wandsworth", "Westminster") 

CID_count = CID_count %>%
  mutate(London = ifelse(BOROUGH %in% Inner, "Inner London", "Outer London")) # 14 variables

# Create ranking
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

# mutate to get rank in with number
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

CID_count_rank = CID_count_rank %>%
  select(c("BOROUGH", "ASL", "Crossings", 
           "CycleLanesAndTracks", "RestrictedRoutes", 
           "CycleParkingSites", "CycleParkingSpaces", 
           "Signals", "TrafficCalming", "Signage", 
           "Restricted_Points", "London"))
  

https://github.com/rich-iannone/gt-workshop-2020
https://gt.rstudio.com/articles/creating-summary-lines.html
https://www.youtube.com/watch?v=lXHpFRVfMfM&list=PLXCrMzQaI6c3EAh10hDZectzBALWPk19w&index=7


CID_count_rank %>%
  relocate(c(CycleParkingSites, CycleParkingSpaces), .after = last_col())  %>% 
  arrange(London) %>%
  gt(rowname_col = "BOROUGH", 
     groupname_col = "London") %>%
  tab_stubhead(label = md("**Borough**")) %>%
  tab_header(
    title = md("**Infrastructure count by type and London Borough**"),
    subtitle = md("*Number (Borough rank)*")) %>%
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
    locations = cells_column_labels(columns = TRUE)) # will need to add style code so that 'Cycle Parking is also bold


# CAlculate summary statistics for asset type  # Needs tidying up 
x = CID_count %>%
  select(-c(BOROUGH, London))
summ = x %>%
  summarise(across(everything(), list(min = min, max = max, mean = mean, sd = sd)))

y = summ %>% pivot_longer(
  everything(),
  names_to = c("Asset", ".value"),
  names_pattern = "(.*)_(.*)") 
y = y %>%
  mutate_if(is.numeric, round)

# extra code
y$Range = paste(y$min, "-", y$max)
y$Mean = paste(y$mean, "(", y$sd, ")")
y = y %>%
  select(c(Asset, Range, Mean))

x = as.data.frame(t(y))  # rotate dataframe

names(x) <- x %>% slice(1) %>% unlist()
x <- x %>% slice(-1)
                 
x$BOROUGH = c("Range", "Mean(SD)")
x$London = c("Summary", "Summary")               
x = x %>%
  select(c("BOROUGH", "ASL", "Crossings", 
           "CycleLanesAndTracks", "RestrictedRoutes", 
           "CycleParkingSites", "CycleParkingSpaces", 
           "Signals", "TrafficCalming", "Signage", 
           "Restricted_Points", "London"))

new_df = rbind(CID_count_rank, x)

new_df %>%
  relocate(c(CycleParkingSites, CycleParkingSpaces), .after = last_col())  %>% 
  arrange(London) %>%   # NEED TO ARRANGE WITH SUMMARY AT TOP AND MAKE LONDON LABELS ITALIC
  gt(rowname_col = "BOROUGH", 
     groupname_col = "London") %>%
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
    locations = cells_column_labels(columns = TRUE)) # will ne

# NEED TO ALSO CENTRALISE COUTS UNDER TEXT 
  
# To save  - save table as object then pipe to gtmsave
gtsave(
  data,
  filename,  #png, pdf, html,
  path = NULL
)


# Old code
CID_count %>%
  relocate(c(CycleParkingSites, CycleParkingSpaces), .after = last_col())  %>%
  gt(rowname_col = "BOROUGH") %>%
  tab_stubhead(label = md("**Borough**")) %>%
  tab_spanner(
    label = "Cycle Parking",
    columns = vars(CycleParkingSites,	CycleParkingSpaces)) %>%
  cols_label(
    BOROUGH = "Borough", CycleLanesAndTracks = "Lanes and tracks",
    RestrictedRoutes = "Restricted routes", CycleParkingSites = "Sites",
    CycleParkingSpaces = "Spaces", TrafficCalming = "Traffic calming",
    Restricted_Points = "Restricted points")

