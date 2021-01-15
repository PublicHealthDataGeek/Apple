####################################################################################
# Visualise Borough level asset data 
####################################
# Date created: 11/1/21
#                                    
# This code takes the Borough level asset data by count and length, ranks these by Borough
# for the raw numbers and numbers per various denominators such as population, 
# borough area etc.  It then ranks the Boroughs within each category.  Finally it visualises
# the ranks using parallel coordinate plots
#
# Due to way paracord draws, it uses column index so I subsetted the dataframes to
# ensure that correct columns selected for plots (can't select by name)
#
# ACTIONS STILL REQUIRED
# - rerun once have NAs sorted
#
# Code improvements:
# - to automate graph construction to reduce risk of errors
# - ? grey background may improve ability to distinguish between boroughs
# - ? tall and think better at doing the comparison
# - ? worth highlighting one or two boroughs by name
# - ? other denominators?
####################################################################################

# Load packages
library(tidyverse)
library(mapview)
library(GGally) # parallel coordinate extension of ggplot

# create function for legend
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Load datasets
# CID asset data
count_CID_by_borough = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/output/CID_count_by_borough")  #11 variables
length_CID_by_borough = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/output/CID_length_by_borough")

# b) ONS Borough area 
# import May 2020 ONS LA boundary data clipped to coastline
lon_lad_2020_c2c = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFC.Rds")
mapview(lon_lad_2020_c2c)

borough_area = lon_lad_2020_c2c %>%
  mutate(Borough_Area = (units::set_units(sf::st_area(geometry), km^2))) %>% # change area units to km^2 from m^2
  sf::st_drop_geometry() %>%
  select(c("BOROUGH", "Borough_Area"))

# c) Borough population
# ONS Mid year population estimates 2013-2019 
download.file("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2020localauthoritydistrictcodes/ukmidyearestimates20192020ladcodes.xls",
              "/home/bananafan/Downloads/ONS_pop_estimates")

ons_pop_estimates = readxl::read_excel("/home/bananafan/Downloads/ONS_pop_estimates", 
                       sheet = "MYE 5", skip = 3) # skip top few excel rows that arent relevent
lon_pop_estimates_2019 = ons_pop_estimates %>%
  filter(Geography1 == "London Borough") %>% # select London Boroughs
  select(c("Name", "Estimated Population mid-2019")) %>% # keep 2019 data only
  rename("BOROUGH" = "Name") %>% # rename to match CID
  rename("Population" = "Estimated Population mid-2019")

# rename values to match those names used in CID
lon_pop_estimates_2019$BOROUGH[lon_pop_estimates_2019$BOROUGH == "Kensington and Chelsea"] <- "Kensington & Chelsea"
lon_pop_estimates_2019$BOROUGH[lon_pop_estimates_2019$BOROUGH == "Barking and Dagenham"] <- "Barking & Dagenham"
lon_pop_estimates_2019$BOROUGH[lon_pop_estimates_2019$BOROUGH == "Hammersmith and Fulham"] <- "Hammersmith & Fulham"

             
###########################
# Working with count data #
###########################

# Join ???? to CID asset data
count_CID_by_borough = count_CID_by_borough %>%
  left_join(borough_area) %>%
  left_join(lon_pop_estimates_2019)  # 13 variables as area and pop are added

# add column for inner/outer london
Inner = c("City of London", "Camden", "Greenwich", "Hackney", "Hammersmith & Fulham", 
          "Islington", "Kensington & Chelsea", "Lambeth", "Lewisham", "Southwark",  
          "Tower Hamlets", "Wandsworth", "Westminster") 

count_CID_by_borough = count_CID_by_borough %>%
  mutate(London = ifelse(BOROUGH %in% Inner, "Inner", 
                         ifelse(BOROUGH %in% "No Borough stated in CID", "Unknown", "Outer"))) # 14 variables

          
# produce counts by area and per head population (NB may want to consider per 100,000 pop at some point)
count_CID_by_borough_denom <- count_CID_by_borough %>%
  mutate(across(.cols = c("ASL", "Crossings", "CycleLanesAndTracks", "RestrictedRoutes", 
                          "CycleParkingSites", "CycleParkingSpaces",
                          "Signals", "TrafficCalming", "Signage", "RestrictedPoints"),
                .fns = ~.x/Borough_Area,
                .names = "{.col}_count_by_area")) %>%
  mutate(across(.cols = c("ASL", "Crossings", "CycleLanesAndTracks", "RestrictedRoutes", 
                          "CycleParkingSites", "CycleParkingSpaces",
                          "Signals", "TrafficCalming", "Signage", "RestrictedPoints"),
                .fns = ~.x/Population,
                .names = "{.col}_count_per_head"))  # now 34 variables (20 onto the 14)


# rank each borough
# use rank(-.x) to get ranking so that borough with most assets has highest ranking
count_CID_by_borough_rank = count_CID_by_borough_denom %>%
  mutate(across(.cols = c("ASL_count_by_area", "Crossings_count_by_area", 
                          "CycleLanesAndTracks_count_by_area", "RestrictedRoutes_count_by_area", 
                          "CycleParkingSites_count_by_area", "CycleParkingSpaces_count_by_area",
                          "Signals_count_by_area", 
                          "TrafficCalming_count_by_area", "Signage_count_by_area", 
                          "RestrictedPoints_count_by_area"),
                 .fns = ~round(rank(-.x)),                         
                 .names = "{.col}_rank")) %>%
  mutate(across(.cols = c("ASL_count_per_head", "Crossings_count_per_head", 
                          "CycleLanesAndTracks_count_per_head", "RestrictedRoutes_count_per_head", 
                          "CycleParkingSites_count_per_head", "CycleParkingSpaces_count_per_head",
                          "Signals_count_per_head", 
                          "TrafficCalming_count_per_head", "Signage_count_per_head", 
                          "RestrictedPoints_count_per_head"),
                .fns = ~round(rank(-.x)),
                .names = "{.col}_rank")) %>%
  mutate(across(.cols = c("ASL", "Crossings", 
                          "CycleLanesAndTracks", "RestrictedRoutes", 
                          "CycleParkingSites", "CycleParkingSpaces", 
                          "Signals", "TrafficCalming", "Signage", 
                          "RestrictedPoints"),
                .fns = ~round(rank(-.x)), 
                .names = "{.col}_count_rank")) # now 64 variables -another 30 onto the 34


# Validate ranking by comparing ranking with assets, used one example (ASL)
# boroughs with most assets given highest ranking
# a) raw counts
rc1 = ggplot(data = count_CID_by_borough_rank) +
  geom_bar(aes(x = ASL_count_rank, y = reorder(BOROUGH, -ASL_count_rank)), stat = "identity") 
rc2 = ggplot(data = count_CID_by_borough_rank) +
  geom_bar(aes(x = ASL, y = reorder(BOROUGH, ASL)), stat = "identity") 
gridExtra::grid.arrange(rc1, rc2)

# b) count of asset by head of pop (? need to do per 100,000 for results table)
cph1 = ggplot(data = count_CID_by_borough_rank) +
  geom_bar(aes(x = ASL_count_per_head_rank, y = reorder(BOROUGH, -ASL_count_per_head_rank)), stat = "identity") 
cph2 = ggplot(data = count_CID_by_borough_rank) +
  geom_bar(aes(x = ASL_count_per_head, y = reorder(BOROUGH, ASL_count_per_head)), stat = "identity") 
gridExtra::grid.arrange(cph1, cph2)

# c) count of asset by area (per km^2)
cba1 = ggplot(data = count_CID_by_borough_rank) +
  geom_bar(aes(x = ASL_count_by_area_rank, y = reorder(BOROUGH, -ASL_count_by_area_rank)), stat = "identity") 
cba2 = ggplot(data = count_CID_by_borough_rank) +
  geom_bar(aes(x = as.numeric(ASL_count_by_area), y = reorder(BOROUGH, ASL_count_by_area)), stat = "identity") #convert area to numeric
gridExtra::grid.arrange(cba1, cba2)

# NEXT STEPS _ DRAW GRAPHS

#########################
# Work with length data #
#########################


length_CID_by_borough = length_CID_by_borough %>%
  left_join(borough_area) %>%
  left_join(lon_pop_estimates_2019)

# add column for inner/outer london
Inner = c("City of London", "Camden", "Greenwich", "Hackney", "Hammersmith & Fulham", 
          "Islington", "Kensington & Chelsea", "Lambeth", "Lewisham", "Southwark",  
          "Tower Hamlets", "Wandsworth", "Westminster") 

length_CID_by_borough = length_CID_by_borough %>%
  mutate(London = ifelse(BOROUGH %in% Inner, "Inner", 
                         ifelse(BOROUGH %in% "No Borough stated in CID", "Unknown", "Outer")))

#produce lengths by area and per head population 
length_CID_by_borough_denom <- length_CID_by_borough %>%
  mutate(across(.cols = c("CycleLaneTrack_km", "RestrictedRoute_km"),
                .fns = ~.x/Borough_Area,
                .names = "{.col}_length_by_area")) %>%
  mutate(across(.cols = c("CycleLaneTrack_km", "RestrictedRoute_km"),
                .fns = ~.x/Population,
                .names = "{.col}_length_per_head")) 


# rank each borough
# use rank(-.x) to get ranking so that borough with greatest length of assets has highest ranking
length_CID_by_borough_rank = length_CID_by_borough_denom %>%
  mutate(across(.cols = c("CycleLaneTrack_km_length_by_area", "RestrictedRoute_km_length_by_area"),
                .fns = ~round(rank(-.x)),                         
                .names = "{.col}_rank")) %>%
  mutate(across(.cols = c("CycleLaneTrack_km_length_per_head", "RestrictedRoute_km_length_per_head"),
                .fns = ~round(rank(-.x)),
                .names = "{.col}_rank")) %>%
  mutate(across(.cols = c("CycleLaneTrack_km", "RestrictedRoute_km"),
                .fns = ~round(rank(-.x)), # rank with the highest ranking Borough having the most assets
                .names = "{.col}_length_rank")) 


# Validate ranking by comparing ranking with assets, used one example (cycle lanes)
# boroughs with most length given highest ranking
# a) raw lengths
rl1 = ggplot(data = length_CID_by_borough_rank) +
  geom_bar(aes(x = as.numeric(CycleLaneTrack_km_length_rank), 
               y = reorder(BOROUGH, -CycleLaneTrack_km_length_rank)), stat = "identity") 
rl2 = ggplot(data = length_CID_by_borough_rank) +
  geom_bar(aes(x = as.numeric(CycleLaneTrack_km), 
               y = reorder(BOROUGH, CycleLaneTrack_km)), stat = "identity") 
gridExtra::grid.arrange(rl1, rl2)

# b) length of asset by head of pop (? need to do per 100,000 for results table)
lph1 = ggplot(data = length_CID_by_borough_rank) +
  geom_bar(aes(x = as.numeric(CycleLaneTrack_km_length_per_head_rank), 
               y = reorder(BOROUGH, -CycleLaneTrack_km_length_per_head_rank)), stat = "identity") 
lph2 = ggplot(data = length_CID_by_borough_rank) +
  geom_bar(aes(x = as.numeric(CycleLaneTrack_km_length_per_head), 
               y = reorder(BOROUGH, CycleLaneTrack_km_length_per_head)), stat = "identity") 
gridExtra::grid.arrange(lph1, lph2)

# c) length of asset by area (per km^2)
lba1 = ggplot(data = length_CID_by_borough_rank) +
  geom_bar(aes(x = as.numeric(CycleLaneTrack_km_length_by_area_rank),
                 y = reorder(BOROUGH, -CycleLaneTrack_km_length_by_area_rank)), stat = "identity")
lba2 = ggplot(data = length_CID_by_borough_rank) +
  geom_bar(aes(x = as.numeric(CycleLaneTrack_km_length_by_area),
               y = reorder(BOROUGH, -CycleLaneTrack_km_length_by_area)), stat = "identity")
gridExtra::grid.arrange(lba1, lba2)


######################################################
# Visualise length data in parallel coordinate plots #
######################################################

# testing approach
ggparcoord(length_CID_by_borough_rank, 
           columns= c(16, 14, 18, 15, 13, 17), 
           scale = "globalminmax",    # uses the rank rather that scaling to something else
           groupColumn = 8,
           alphaLines = 0.8,
           #showPoints = TRUE,
           title = "Length of infrastructure ranked by Borough") +
  scale_color_manual(values =c("maroon", "dark gray", "black")) +  # inner, outer, missing
  coord_flip()  +
  theme_minimal()

# relevent columns in dataset
#[1] "BOROUGH"                                
#[8] "London"                                 
#[13] "CycleLaneTrack_km_length_by_area_rank"  
#[14] "RestrictedRoute_km_length_by_area_rank" 
#[15] "CycleLaneTrack_km_length_per_head_rank" 
#[16] "RestrictedRoute_km_length_per_head_rank"
#[17] "CycleLaneTrack_km_length_rank"          
#[18] "RestrictedRoute_km_length_rank"    

# Ordering of columns and column names in ggparacoord:
# first column in columns = c() or discrete_labels, 
#is the bottom one on the chart
 

############################
# Visualising by Boroughs #
############################

borough_lengths_eg = ggparcoord(length_CID_by_borough_rank, 
                      columns= c(15, 13, 17), 
                      scale = "globalminmax", 
                      groupColumn = 1,
                      alphaLines = 0.8,
                      title = "Cycle lanes and tracks") +
  coord_flip()  +
  ylab("Rank") +
  xlab(NULL) +
  scale_y_continuous(breaks = c(0, 11, 22, 33)) +
  scale_x_discrete(labels = c("Length per head", "Length by km^2", "Total length"), 
                   expand = c(0.01,0.01)) +
  theme_minimal()

# save boroughs plot as example
ggsave(file = "/home/bananafan/Documents/PhD/Paper1/output/borough_lengths_eg.png", 
       borough_lengths_eg, width = 95 * (14/5), height = 53 * (14/5), units = "mm")

###########################################
# Visualised by length options across top #
###########################################
ll = ggparcoord(length_CID_by_borough_rank, 
               columns= c(18, 17), 
               scale = "globalminmax", 
               groupColumn = 8,
               alphaLines = 0.8,
               title = "Total length") +
  scale_color_manual(values =c("maroon", "dark gray", "black")) +
  coord_flip()  +
  ylab("Rank") +
  xlab(NULL) +
  scale_y_continuous(breaks = c(0, 11, 22, 33)) +
  scale_x_discrete(labels = c("Restricted Routes", "Cycle Lanes and Tracks"), expand = c(0.01,0.01)) +
  theme_minimal()

la = ggparcoord(length_CID_by_borough_rank, 
                columns= c(14, 13), 
                scale = "globalminmax", 
                groupColumn = 8,
                alphaLines = 0.8,
                title = "Length per km^2") +
  scale_color_manual(values =c("maroon", "dark gray", "black")) +
  coord_flip()  +
  ylab("Rank") +
  xlab(NULL) +
  scale_y_continuous(breaks = c(0, 11, 22, 33)) +
  scale_x_discrete(expand = c(0.01,0.01), labels = NULL) +
  theme_minimal()

lph = ggparcoord(length_CID_by_borough_rank, 
                columns= c(16, 15), 
                scale = "globalminmax", 
                groupColumn = 8,
                alphaLines = 0.8,
                title = "Length per head") +
  scale_color_manual(values =c("maroon", "dark gray", "black")) +
  coord_flip()  +
  ylab("Rank") +
  xlab(NULL) +
  scale_y_continuous(breaks = c(0, 11, 22, 33)) +
  scale_x_discrete(expand = c(0.01,0.01), labels = NULL) +
  theme_minimal()

# get legend
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend1 <- get_legend(ll) # create legend for the 3 plots
ll = ll + theme(legend.position="none") # remove legends from plots
la = la + theme(legend.position="none")
lph = lph + theme(legend.position="none")

# plot all three plots together
gridExtra::grid.arrange(ll, la, lph, legend1, nrow=1, widths= c(1.6, 1, 1, 0.5))

# save length plot
plot_length1 = gridExtra::grid.arrange(ll, la, lph, legend1, nrow=1, widths= c(1.5, 1, 1, 0.5))
ggsave(file = "/home/bananafan/Documents/PhD/Paper1/output/2assets_by_3lengths.png", 
       plot_length1, width = 95 * (14/5), height = 53 * (14/5), units = "mm")
#the above seems to give perfect size for these 3 plots

##########################################################
# Visualised by cycle lanes/restricted routes across top #
##########################################################

clt_leng = ggparcoord(length_CID_by_borough_rank, 
                columns= c(15, 13, 17), 
                scale = "globalminmax", 
                groupColumn = 8,
                alphaLines = 0.8,
                title = "Cycle lanes and tracks") +
  scale_color_manual(values =c("forest green", "dark gray", "black")) +
  coord_flip()  +
  ylab("Rank") +
  xlab(NULL) +
  scale_y_continuous(breaks = c(0, 11, 22, 33)) +
  scale_x_discrete(labels = c("Length per head", "Length by km^2", "Total length"), 
                   expand = c(0.01,0.01)) +
  theme_minimal()

rr_leng = ggparcoord(length_CID_by_borough_rank, 
                columns= c(16, 14, 18), 
                scale = "globalminmax", 
                groupColumn = 8,
                alphaLines = 0.8,
                #showPoints = TRUE,
                title = "Restricted routes") +
  scale_color_manual(values =c("forest green", "dark gray", "black")) +
  coord_flip()  +
  ylab("Rank") +
  xlab(NULL) +
  scale_y_continuous(breaks = c(0, 11, 22, 33)) +
  scale_x_discrete(expand = c(0.01,0.01), labels = NULL) + # remove labels so can do grid arrange
  theme_minimal()



legend2 <- get_legend(clt_leng) # create legend for the  plots
clt_leng = clt_leng + theme(legend.position="none") # remove legends from plots
rr_leng = rr_leng + theme(legend.position="none") 

# plot all two plots together
gridExtra::grid.arrange(clt_leng, rr_leng, legend2, 
                        nrow=1, widths= c(1.25, 1, 0.5))

# save length plot
plot_length2 = gridExtra::arrangeGrob(clt_leng, rr_leng, legend2, 
                                      nrow=1, widths= c(1.25, 1, 0.5))
ggsave(file = "/home/bananafan/Documents/PhD/Paper1/output/3lengths_by_2assets.png", 
       plot_length2, width = 95 * (14/5), height = 53 * (14/5), units = "mm")




######################################################
# Visualise count data in parallel coordinate plots #
######################################################

# example plot
brgh_count_plot = ggparcoord(count_CID_by_borough_rank, 
           columns = rev(55:64), 
           scale = "globalminmax",    # uses the rank rather that scaling to something else
           groupColumn = 14,  # inner outer etc
           alphaLines = 0.8,
           title = "Count of infrastructure ranked by Borough") +
  scale_color_manual(values =c("maroon", "dark gray", "black")) +  # inner, outer, missing
  coord_flip()  +
  theme_minimal()
# save borough count plot
ggsave(file = "/home/bananafan/Documents/PhD/Paper1/output/brgh_count_plot.png", 
       brgh_count_plot, width = 95 * (14/5), height = 53 * (14/5), units = "mm")

# As ggparcoord requires to give column by index and can be hard to ensure picked 
# correct column and name plan is to subset data before drawing plots


###########################################
# Visualised by count options across top #
###########################################

# each chart validated by comparing the pattern seen with an equivalent version
# of the chart above so make sure I have labelled the columns properly. 

# TOTAL COUNT 
# subset df
total_count_rank = count_CID_by_borough_rank %>%
  select(c(1, 14,55:64))
names(total_count_rank)
# [1] "BOROUGH"                        "London"                        
# [3] "ASL_count_rank"                 "Crossings_count_rank"          
# [5] "CycleLanesAndTracks_count_rank" "RestrictedRoutes_count_rank"   
# [7] "CycleParkingSites_count_rank"   "CycleParkingSpaces_count_rank" 
# [9] "Signals_count_rank"             "TrafficCalming_count_rank"     
# [11] "Signage_count_rank"             "RestrictedPoints_count_rank" 

# test = ggparcoord(total_count_rank, 
#                 columns = rev(3:12),  # this puts ASL at the top and RP at bottom
#                 scale = "globalminmax", 
#                 groupColumn = 2, # london columns
#                 alphaLines = 0.8,
#                 title = "Total count") +
#   scale_color_manual(values =c("maroon", "dark gray", "black")) +
#   coord_flip()  +
#   ylab("Rank") +
#   xlab(NULL) +
#   scale_y_continuous(breaks = c(0, 11, 22, 33)) +
#   scale_x_discrete(expand = c(0.01,0.01)) +
#   theme_minimal()

# test_2 = ggparcoord(total_count_rank, 
#                   columns = rev(3:12),  # this puts ASL at the top and RP at bottom
#                   scale = "globalminmax", 
#                   groupColumn = 2, # london columns
#                   alphaLines = 0.8,
#                   title = "Total count") +
#   scale_color_manual(values =c("maroon", "dark gray", "black")) +
#   coord_flip()  +
#   ylab("Rank") +
#   xlab(NULL) +
#   scale_y_continuous(breaks = c(0, 11, 22, 33)) +
#   scale_x_discrete(expand = c(0.01,0.01), labels = c("Restricted Points", "Signage", "Traffic calming", 
#                                                     "Signals", "Cycle parking spaces", "Cycle parking sites",
#                                                     "Restricted routes", "Cycle Lanes and Tracks", 
#                                                     "Crossings", "ASL")) +
#   theme_minimal()



# check correctly plotted and labelled
# gridExtra::grid.arrange(test, test_2, nrow=1) # ->  Yes correct

# do actual plot 
tc_plot = ggparcoord(total_count_rank, 
                    columns = rev(3:12),  # this puts ASL at the top and RP at bottom
                    scale = "globalminmax", 
                    groupColumn = 2, # london columns
                    alphaLines = 0.8,
                    title = "Total count") +
  scale_color_manual(values =c("maroon", "dark gray", "black")) +
  coord_flip()  +
  ylab("Rank") +
  xlab(NULL) +
  scale_y_continuous(breaks = c(0, 11, 22, 33)) +
  scale_x_discrete(expand = c(0.01,0.01), labels = c("Restricted Points", "Signage", "Traffic calming", 
                                                     "Signals", "Cycle parking spaces", "Cycle parking sites",
                                                     "Restricted routes", "Cycle Lanes and Tracks", 
                                                     "Crossings", "ASL")) +
  theme_minimal()



# COUNT BY AREA
# subset df
count_by_area_rank = count_CID_by_borough_rank %>%
  select(c(1, 14,35:44))
names(count_by_area_rank)
# [1] "BOROUGH"                               
# [2] "London"                                
# [3] "ASL_count_by_area_rank"                
# [4] "Crossings_count_by_area_rank"          
# [5] "CycleLanesAndTracks_count_by_area_rank"
# [6] "RestrictedRoutes_count_by_area_rank"   
# [7] "CycleParkingSites_count_by_area_rank"  
# [8] "CycleParkingSpaces_count_by_area_rank" 
# [9] "Signals_count_by_area_rank"            
# [10] "TrafficCalming_count_by_area_rank"     
# [11] "Signage_count_by_area_rank"            
# [12] "RestrictedPoints_count_by_area_rank"  

# test = ggparcoord(count_by_area_rank,
#                       columns = rev(3:12),  # this puts ASL at the top and RP at bottom
#                       scale = "globalminmax",
#                       groupColumn = 2, # london columns) +
#   scale_color_manual(values =c("maroon", "dark gray", "black"))) +
#   coord_flip()
# 
# test_2 = ggparcoord(count_by_area_rank, 
#                      columns = rev(3:12),  # this puts ASL at the top and RP at bottom
#                      scale = "globalminmax", 
#                      groupColumn = 2, # london columns
#                      alphaLines = 0.8,
#                      title = "Count by km^2") +
#   scale_color_manual(values =c("maroon", "dark gray", "black")) +
#   coord_flip()  +
#   ylab("Rank") +
#   xlab(NULL) +
#   scale_y_continuous(breaks = c(0, 11, 22, 33)) +
#   scale_x_discrete(expand = c(0.01,0.01), labels = c("Restricted Points", "Signage", "Traffic calming", 
#                                                      "Signals", "Cycle parking spaces", "Cycle parking sites",
#                                                      "Restricted routes", "Cycle Lanes and Tracks", 
#                                                      "Crossings", "ASL")) +
#   theme_minimal()
# 
# # check correctly plotted and labelled
# gridExtra::grid.arrange(test, test_2, nrow=1) # ->  Yes correct

#NOw do actual plot with labels removed so can do the 3 plots together
cba_plot = ggparcoord(count_by_area_rank, 
                    columns = rev(3:12),  # this puts ASL at the top and RP at bottom
                    scale = "globalminmax", 
                    groupColumn = 2, # london columns
                    alphaLines = 0.8,
                    title = "Count by km^2") +
  scale_color_manual(values =c("maroon", "dark gray", "black")) +
  coord_flip()  +
  ylab("Rank") +
  xlab(NULL) +
  scale_y_continuous(breaks = c(0, 11, 22, 33)) +
  scale_x_discrete(expand = c(0.01,0.01), labels = NULL) +
  theme_minimal()


# COUNT PER HEAD
# subset df
count_per_head_rank = count_CID_by_borough_rank %>%
  select(c(1, 14,45:54))
names(count_per_head_rank)
# [1] "BOROUGH"                                
# [2] "London"                                 
# [3] "ASL_count_per_head_rank"                
# [4] "Crossings_count_per_head_rank"          
# [5] "CycleLanesAndTracks_count_per_head_rank"
# [6] "RestrictedRoutes_count_per_head_rank"   
# [7] "CycleParkingSites_count_per_head_rank"  
# [8] "CycleParkingSpaces_count_per_head_rank" 
# [9] "Signals_count_per_head_rank"            
# [10] "TrafficCalming_count_per_head_rank"     
# [11] "Signage_count_per_head_rank"            
# [12] "RestrictedPoints_count_per_head_rank" 

# test = ggparcoord(count_per_head_rank,
#                        columns = rev(3:12),  # this puts ASL at the top and RP at bottom
#                        scale = "globalminmax",
#                        groupColumn = 2, # london columns) +
#    scale_color_manual(values =c("maroon", "dark gray", "black"))) +
#    coord_flip()

# test_2 = ggparcoord(count_per_head_rank, 
#                       columns = rev(3:12),  # this puts ASL at the top and RP at bottom
#                       scale = "globalminmax", 
#                       groupColumn = 2, # london columns
#                       alphaLines = 0.8,
#                       title = "Count per head") +
#   scale_color_manual(values =c("maroon", "dark gray", "black")) +
#   coord_flip()  +
#   ylab("Rank") +
#   xlab(NULL) +
#   scale_y_continuous(breaks = c(0, 11, 22, 33)) +
#   scale_x_discrete(expand = c(0.01,0.01), labels = c("Restricted Points", "Signage", "Traffic calming", 
#                                                      "Signals", "Cycle parking spaces", "Cycle parking sites",
#                                                      "Restricted routes", "Cycle Lanes and Tracks", 
#                                                      "Crossings", "ASL")) +
#   theme_minimal()
# 
# # check correctly plotted and labelled
# gridExtra::grid.arrange(test, test_2, nrow=1) # ->  Yes correct

#NOw do actual plot with labels removed so can do the 3 plots together

cph_plot = ggparcoord(count_per_head_rank, 
                    columns = rev(3:12),  # this puts ASL at the top and RP at bottom
                    scale = "globalminmax", 
                    groupColumn = 2, # london columns
                    alphaLines = 0.8,
                    title = "Count per head") +
  scale_color_manual(values =c("maroon", "dark gray", "black")) +
  coord_flip()  +
  ylab("Rank") +
  xlab(NULL) +
  scale_y_continuous(breaks = c(0, 11, 22, 33)) +
  scale_x_discrete(expand = c(0.01,0.01), labels = NULL) +
  theme_minimal()



# create plot of all 3 count rank plots
legend3 <- get_legend(tc_plot) # create legend for the 3 plots
tc_plot = tc_plot + theme(legend.position="none") # remove legends from plots
cba_plot = cba_plot + theme(legend.position="none")
cph_plot = cph_plot + theme(legend.position="none")

gridExtra::grid.arrange(tc_plot, cba_plot, cph_plot, legend3, nrow=1, widths= c(0.8, 0.5, 0.5, 0.2))

# save count plot
plot_count1 = gridExtra::grid.arrange(tc_plot, cba_plot, cph_plot, legend3, nrow=1, widths= c(0.8, 0.5, 0.5, 0.2))
ggsave(file = "/home/bananafan/Documents/PhD/Paper1/output/7assets_by_3counts.png", 
       plot_count1, width = 75 * (14/5), height = 53 * (14/5), units = "mm")
#the above seems to give perfect size for these 3 plots



##########################################################
# Visualised by cycle lanes/restricted routes across top #
##########################################################
# each chart validated by comparing the pattern seen with an equivalent version
# of the chart above so make sure I have labelled the columns properly. 

#### ASL
# subset df
asl_rank = count_CID_by_borough_rank %>%
  select(c(1, 14, 35, 45, 55))
names(asl_rank)
# [1] "BOROUGH"                 "London"                 
# [3] "ASL_count_by_area_rank"  "ASL_count_per_head_rank"
# [5] "ASL_count_rank"  
# test = ggparcoord(asl_rank, 
#                   columns= c(3, 4, 5), 
#                   scale = "globalminmax", 
#                   groupColumn = 2) +
#   scale_color_manual(values =c("forest green", "dark gray", "black")) +
#   coord_flip()
# 
# test_2 = ggparcoord(asl_rank, columns= c(3, 4, 5),
#                     scale = "globalminmax",
#                     groupColumn = 2, # london columns
#                     alphaLines = 0.8,
#                     title = "ASL") +
#    scale_color_manual(values =c("forest green", "dark gray", "black")) +
#    coord_flip()  +
#    ylab("Rank") +
#    xlab(NULL) +
#    scale_y_continuous(breaks = c(0, 11, 22, 33)) +
#    scale_x_discrete(expand = c(0.01,0.01), labels = c("Count by km^2", "Count per head", "Count")) +
#    theme_minimal()
# 
# # check correctly plotted and labelled
# gridExtra::grid.arrange(test, test_2, nrow=1) # ->  Yes correct

asl_plot = ggparcoord(asl_rank, columns= c(3, 4, 5),
                    scale = "globalminmax",
                    groupColumn = 2, # london columns
                    alphaLines = 0.8,
                    title = "ASL") +
   scale_color_manual(values =c("forest green", "dark gray", "black")) +
   coord_flip()  +
   ylab("Rank") +
   xlab(NULL) +
   scale_y_continuous(breaks = c(0, 11, 22, 33)) +
   scale_x_discrete(expand = c(0.01,0.01), labels = c("Count by km^2", "Count per head", "Count")) +
   theme_minimal()

#######CROSSINGS
# subset df
crossings_rank = count_CID_by_borough_rank %>%
  select(c(1, 14, 36, 46, 56))
names(crossings_rank)
# 1] "BOROUGH"                       "London"                       
# [3] "Crossings_count_by_area_rank"  "Crossings_count_per_head_rank"
# [5] "Crossings_count_rank" 

# test = ggparcoord(crossings_rank,
#                   columns= c(3, 4, 5),
#                   scale = "globalminmax",
#                   groupColumn = 2) +
#   scale_color_manual(values =c("forest green", "dark gray", "black")) +
#   coord_flip()
# 
# test_2 = ggparcoord(crossings_rank, columns= c(3, 4, 5),
#                     scale = "globalminmax",
#                     groupColumn = 2, # london columns
#                     alphaLines = 0.8,
#                     title = "Crossings") +
#    scale_color_manual(values =c("forest green", "dark gray", "black")) +
#    coord_flip()  +
#    ylab("Rank") +
#    xlab(NULL) +
#    scale_y_continuous(breaks = c(0, 11, 22, 33)) +
#    scale_x_discrete(expand = c(0.01,0.01), labels = c("Count by km^2", "Count per head", "Count")) +
#    theme_minimal()
# 
# # check correctly plotted and labelled
# gridExtra::grid.arrange(test, test_2, nrow=1) # ->  Yes correct

crossings_plot = ggparcoord(crossings_rank, columns= c(3, 4, 5),
                    scale = "globalminmax",
                    groupColumn = 2, # london columns
                    alphaLines = 0.8,
                    title = "Crossings") +
  scale_color_manual(values =c("forest green", "dark gray", "black")) +
  coord_flip()  +
  ylab("Rank") +
  xlab(NULL) +
  scale_y_continuous(breaks = c(0, 11, 22, 33)) +
  scale_x_discrete(expand = c(0.01,0.01), labels = NULL) +
  theme_minimal()

######### CYCLE LANES AND TRACKS

# subset df
clt_rank = count_CID_by_borough_rank %>%
  select(c(1, 14, 37, 47, 57))
names(clt_rank)
# [1] "BOROUGH"                                
# [2] "London"                                 
# [3] "CycleLanesAndTracks_count_by_area_rank" 
# [4] "CycleLanesAndTracks_count_per_head_rank"
# [5] "CycleLanesAndTracks_count_rank"    

# test = ggparcoord(clt_rank,
#                   columns= c(3, 4, 5),
#                   scale = "globalminmax",
#                   groupColumn = 2) +
#   scale_color_manual(values =c("forest green", "dark gray", "black")) +
#   coord_flip()
# 
# test_2 = ggparcoord(clt_rank, 
#                     columns= c(3, 4, 5),
#                     scale = "globalminmax",
#                     groupColumn = 2, # london columns
#                     alphaLines = 0.8,
#                     title = "Cycle lanes and tracks") +
#    scale_color_manual(values =c("forest green", "dark gray", "black")) +
#    coord_flip()  +
#    ylab("Rank") +
#    xlab(NULL) +
#    scale_y_continuous(breaks = c(0, 11, 22, 33)) +
#    scale_x_discrete(expand = c(0.01,0.01), labels = c("Count by km^2", "Count per head", "Count")) +
#    theme_minimal()
# 
# # check correctly plotted and labelled
# gridExtra::grid.arrange(test, test_2, nrow=1) # ->  Yes correct

cycle_lanes_plot = ggparcoord(clt_rank,
                    columns= c(3, 4, 5),
                    scale = "globalminmax",
                    groupColumn = 2, # london columns
                    alphaLines = 0.8,
                    title = "Cycle lanes and tracks") +
   scale_color_manual(values =c("forest green", "dark gray", "black")) +
   coord_flip()  +
   ylab("Rank") +
   xlab(NULL) +
   scale_y_continuous(breaks = c(0, 11, 22, 33)) +
   scale_x_discrete(expand = c(0.01,0.01), labels = NULL) +
   theme_minimal()


######### RESTRICTED ROUTES

# subset df
rr_rank = count_CID_by_borough_rank %>%
  select(c(1, 14, 38, 48, 58))
names(rr_rank)
# [1] "BOROUGH"                             
# [2] "London"                              
# [3] "RestrictedRoutes_count_by_area_rank" 
# [4] "RestrictedRoutes_count_per_head_rank"
# [5] "RestrictedRoutes_count_rank"   

# test = ggparcoord(rr_rank,
#                   columns= c(3, 4, 5),
#                   scale = "globalminmax",
#                   groupColumn = 2) +
#   scale_color_manual(values =c("forest green", "dark gray", "black")) +
#   coord_flip()
# 
# test_2 = ggparcoord(rr_rank,
#                     columns= c(3, 4, 5),
#                     scale = "globalminmax",
#                     groupColumn = 2, # london columns
#                     alphaLines = 0.8,
#                     title = "Restricted routes") +
#    scale_color_manual(values =c("forest green", "dark gray", "black")) +
#    coord_flip()  +
#    ylab("Rank") +
#    xlab(NULL) +
#    scale_y_continuous(breaks = c(0, 11, 22, 33)) +
#    scale_x_discrete(expand = c(0.01,0.01), labels = c("Count by km^2", "Count per head", "Count")) +
#    theme_minimal()
# 
# # check correctly plotted and labelled
# gridExtra::grid.arrange(test, test_2, nrow=1) # ->  Yes correct

restrictedroutes_plot = ggparcoord(rr_rank,
                    columns= c(3, 4, 5),
                    scale = "globalminmax",
                    groupColumn = 2, # london columns
                    alphaLines = 0.8,
                    title = "Restricted routes") +
   scale_color_manual(values =c("forest green", "dark gray", "black")) +
   coord_flip()  +
   ylab("Rank") +
   xlab(NULL) +
   scale_y_continuous(breaks = c(0, 11, 22, 33)) +
   scale_x_discrete(expand = c(0.01,0.01), labels = NULL) +
   theme_minimal()


######### CYCLE PARKING SITES

# subset df
parking_sites_rank = count_CID_by_borough_rank %>%
  select(c(1, 14, 39, 49, 59))
names(parking_sites_rank)
# [1] "BOROUGH"                              
# [2] "London"                               
# [3] "CycleParkingSites_count_by_area_rank" 
# [4] "CycleParkingSites_count_per_head_rank"
# [5] "CycleParkingSites_count_rank  

# test = ggparcoord(parking_sites_rank,
#                   columns= c(3, 4, 5),
#                   scale = "globalminmax",
#                   groupColumn = 2) +
#   scale_color_manual(values =c("forest green", "dark gray", "black")) +
#   coord_flip()
# 
# test_2 = ggparcoord(parking_sites_rank,
#                     columns= c(3, 4, 5),
#                     scale = "globalminmax",
#                     groupColumn = 2, # london columns
#                     alphaLines = 0.8,
#                     title = "Cycle parking sites") +
#    scale_color_manual(values =c("forest green", "dark gray", "black")) +
#    coord_flip()  +
#    ylab("Rank") +
#    xlab(NULL) +
#    scale_y_continuous(breaks = c(0, 11, 22, 33)) +
#    scale_x_discrete(expand = c(0.01,0.01), labels = c("Count by km^2", "Count per head", "Count")) +
#    theme_minimal()
# 
# # check correctly plotted and labelled
# gridExtra::grid.arrange(test, test_2, nrow=1) # ->  Yes correct

# do plot
parking_sites_plot = ggparcoord(parking_sites_rank,
                    columns= c(3, 4, 5),
                    scale = "globalminmax",
                    groupColumn = 2, # london columns
                    alphaLines = 0.8,
                    title = "Cycle parking sites") +
  scale_color_manual(values =c("forest green", "dark gray", "black")) +
  coord_flip()  +
  ylab("Rank") +
  xlab(NULL) +
  scale_y_continuous(breaks = c(0, 11, 22, 33)) +
  scale_x_discrete(expand = c(0.01,0.01), labels = c("Count by km*2", "Count per head", "Count")) +
  theme_minimal()


######### CYCLE PARKING SPACES
# subset df
parking_spaces_rank = count_CID_by_borough_rank %>%
  select(c(1, 14, 40, 50, 60))
names(parking_spaces_rank)
# [1] "BOROUGH"                               
# [2] "London"                                
# [3] "CycleParkingSpaces_count_by_area_rank" 
# [4] "CycleParkingSpaces_count_per_head_rank"
# [5] "CycleParkingSpaces_count_rank"    

# test = ggparcoord(parking_spaces_rank,
#                   columns= c(3, 4, 5),
#                   scale = "globalminmax",
#                   groupColumn = 2) +
#   scale_color_manual(values =c("forest green", "dark gray", "black")) +
#   coord_flip()
# 
# test_2 = ggparcoord(parking_spaces_rank,
#                     columns= c(3, 4, 5),
#                     scale = "globalminmax",
#                     groupColumn = 2, # london columns
#                     alphaLines = 0.8,
#                     title = "Cycle parking spaces") +
#    scale_color_manual(values =c("forest green", "dark gray", "black")) +
#    coord_flip()  +
#    ylab("Rank") +
#    xlab(NULL) +
#    scale_y_continuous(breaks = c(0, 11, 22, 33)) +
#    scale_x_discrete(expand = c(0.01,0.01), labels = c("Count by km^2", "Count per head", "Count")) +
#    theme_minimal()
# 
# # check correctly plotted and labelled
# gridExtra::grid.arrange(test, test_2, nrow=1) # ->  Yes correct

# draw plot
parking_spaces_plot = ggparcoord(parking_spaces_rank,
                    columns= c(3, 4, 5),
                    scale = "globalminmax",
                    groupColumn = 2, # london columns
                    alphaLines = 0.8,
                    title = "Cycle parking spaces") +
  scale_color_manual(values =c("forest green", "dark gray", "black")) +
  coord_flip()  +
  ylab("Rank") +
  xlab(NULL) +
  scale_y_continuous(breaks = c(0, 11, 22, 33)) +
  scale_x_discrete(expand = c(0.01,0.01), labels = NULL) +
  theme_minimal()


######### SIGNALS
# subset df
signals_rank = count_CID_by_borough_rank %>%
  select(c(1, 14, 41, 51, 61))
names(signals_rank)
# [1] "BOROUGH"                     "London"                     
# [3] "Signals_count_by_area_rank"  "Signals_count_per_head_rank"
# [5] "Signals_count_rank"    

# test = ggparcoord(signals_rank,
#                   columns= c(3, 4, 5),
#                   scale = "globalminmax",
#                   groupColumn = 2) +
#   scale_color_manual(values =c("forest green", "dark gray", "black")) +
#   coord_flip()
# 
# test_2 = ggparcoord(signals_rank,
#                     columns= c(3, 4, 5),
#                     scale = "globalminmax",
#                     groupColumn = 2, # london columns
#                     alphaLines = 0.8,
#                     title = "Signals") +
#    scale_color_manual(values =c("forest green", "dark gray", "black")) +
#    coord_flip()  +
#    ylab("Rank") +
#    xlab(NULL) +
#    scale_y_continuous(breaks = c(0, 11, 22, 33)) +
#    scale_x_discrete(expand = c(0.01,0.01), labels = c("Count by km^2", "Count per head", "Count")) +
#    theme_minimal()
# 
# # check correctly plotted and labelled
# gridExtra::grid.arrange(test, test_2, nrow=1) # ->  Yes correct

# draw plot
signals_plot = ggparcoord(signals_rank,
                    columns= c(3, 4, 5),
                    scale = "globalminmax",
                    groupColumn = 2, # london columns
                    alphaLines = 0.8,
                    title = "Signals") +
  scale_color_manual(values =c("forest green", "dark gray", "black")) +
  coord_flip()  +
  ylab("Rank") +
  xlab(NULL) +
  scale_y_continuous(breaks = c(0, 11, 22, 33)) +
  scale_x_discrete(expand = c(0.01,0.01), labels = NULL) +
  theme_minimal()



######### TRAFFIC CALMING
# subset df
traffic_calming_rank = count_CID_by_borough_rank %>%
  select(c(1, 14, 42, 52, 62))
names(traffic_calming_rank)
# [1] "BOROUGH"                            "London"                            
# [3] "TrafficCalming_count_by_area_rank"  "TrafficCalming_count_per_head_rank"
# [5] "TrafficCalming_count_rank"       

# test = ggparcoord(traffic_calming_rank,
#                   columns= c(3, 4, 5),
#                   scale = "globalminmax",
#                   groupColumn = 2) +
#   scale_color_manual(values =c("forest green", "dark gray", "black")) +
#   coord_flip()
# 
# test_2 = ggparcoord(traffic_calming_rank,
#                     columns= c(3, 4, 5),
#                     scale = "globalminmax",
#                     groupColumn = 2, # london columns
#                     alphaLines = 0.8,
#                     title = "Traffic calming") +
#   scale_color_manual(values =c("forest green", "dark gray", "black")) +
#   coord_flip()  +
#   ylab("Rank") +
#   xlab(NULL) +
#   scale_y_continuous(breaks = c(0, 11, 22, 33)) +
#   scale_x_discrete(expand = c(0.01,0.01), labels = c("Count by km^2", "Count per head", "Count")) +
#   theme_minimal()
# 
# # check correctly plotted and labelled
# gridExtra::grid.arrange(test, test_2, nrow=1) # ->  Yes correct

# create plot
traffic_calming_plot = ggparcoord(traffic_calming_rank,
                    columns= c(3, 4, 5),
                    scale = "globalminmax",
                    groupColumn = 2, # london columns
                    alphaLines = 0.8,
                    title = "Traffic calming") +
  scale_color_manual(values =c("forest green", "dark gray", "black")) +
  coord_flip()  +
  ylab("Rank") +
  xlab(NULL) +
  scale_y_continuous(breaks = c(0, 11, 22, 33)) +
  scale_x_discrete(expand = c(0.01,0.01), labels = NULL) +
  theme_minimal()


######### SIGNAGE
# subset df
signage_rank = count_CID_by_borough_rank %>%
  select(c(1, 14, 43, 53, 63))
names(signage_rank)
# [1] "BOROUGH"                     "London"                     
# [3] "Signage_count_by_area_rank"  "Signage_count_per_head_rank"
# [5] "Signage_count_rank"         

# test = ggparcoord(signage_rank,
#                   columns= c(3, 4, 5),
#                   scale = "globalminmax",
#                   groupColumn = 2) +
#   scale_color_manual(values =c("forest green", "dark gray", "black")) +
#   coord_flip()
# 
# test_2 = ggparcoord(signage_rank,
#                     columns= c(3, 4, 5),
#                     scale = "globalminmax",
#                     groupColumn = 2, # london columns
#                     alphaLines = 0.8,
#                     title = "Signage") +
#   scale_color_manual(values =c("forest green", "dark gray", "black")) +
#   coord_flip()  +
#   ylab("Rank") +
#   xlab(NULL) +
#   scale_y_continuous(breaks = c(0, 11, 22, 33)) +
#   scale_x_discrete(expand = c(0.01,0.01), labels = c("Count by km^2", "Count per head", "Count")) +
#   theme_minimal()
# 
# # check correctly plotted and labelled
# gridExtra::grid.arrange(test, test_2, nrow=1) # ->  Yes correct

signage_plot = ggparcoord(signage_rank,
                    columns= c(3, 4, 5),
                    scale = "globalminmax",
                    groupColumn = 2, # london columns
                    alphaLines = 0.8,
                    title = "Signage") +
  scale_color_manual(values =c("forest green", "dark gray", "black")) +
  coord_flip()  +
  ylab("Rank") +
  xlab(NULL) +
  scale_y_continuous(breaks = c(0, 11, 22, 33)) +
  scale_x_discrete(expand = c(0.01,0.01), labels = c("Count by km^2", "Count per head", "Count")) +
  theme_minimal()

######### RESTRICTED POINTS
# subset df
restricted_points_rank = count_CID_by_borough_rank %>%
  select(c(1, 14, 44, 54, 64))
names(restricted_points_rank)
# [1] "BOROUGH"                             
# [2] "London"                              
# [3] "RestrictedPoints_count_by_area_rank" 
# [4] "RestrictedPoints_count_per_head_rank"
# [5] "RestrictedPoints_count_rank"    

# test = ggparcoord(restricted_points_rank,
#                   columns= c(3, 4, 5),
#                   scale = "globalminmax",
#                   groupColumn = 2) +
#   scale_color_manual(values =c("forest green", "dark gray", "black")) +
#   coord_flip()
# 
# test_2 = ggparcoord(restricted_points_rank,
#                     columns= c(3, 4, 5),
#                     scale = "globalminmax",
#                     groupColumn = 2, # london columns
#                     alphaLines = 0.8,
#                     title = "Restricted points") +
#   scale_color_manual(values =c("forest green", "dark gray", "black")) +
#   coord_flip()  +
#   ylab("Rank") +
#   xlab(NULL) +
#   scale_y_continuous(breaks = c(0, 11, 22, 33)) +
#   scale_x_discrete(expand = c(0.01,0.01), labels = c("Count by km^2", "Count per head", "Count")) +
#   theme_minimal()
# 
# # check correctly plotted and labelled
# gridExtra::grid.arrange(test, test_2, nrow=1) # ->  Yes correct

#draw plot
restricted_points_plot = ggparcoord(restricted_points_rank,
                    columns= c(3, 4, 5),
                    scale = "globalminmax",
                    groupColumn = 2, # london columns
                    alphaLines = 0.8,
                    title = "Restricted points") +
  scale_color_manual(values =c("forest green", "dark gray", "black")) +
  coord_flip()  +
  ylab("Rank") +
  xlab(NULL) +
  scale_y_continuous(breaks = c(0, 11, 22, 33)) +
  scale_x_discrete(expand = c(0.01,0.01), labels = NULL) +
  theme_minimal()


# draw all the asset graphs together
legend4 <- get_legend(asl_plot) # create legend
asl_plot = asl_plot + theme(legend.position="none") # remove legends from plots
crossings_plot = crossings_plot + theme(legend.position="none")
cycle_lanes_plot = cycle_lanes_plot + theme(legend.position="none")
restrictedroutes_plot = restrictedroutes_plot + theme(legend.position="none")
parking_sites_plot = parking_sites_plot + theme(legend.position="none")
parking_spaces_plot = parking_spaces_plot + theme(legend.position="none")
signals_plot = signals_plot + theme(legend.position="none")
traffic_calming_plot = traffic_calming_plot + theme(legend.position="none")
signage_plot = signage_plot + theme(legend.position="none")
restricted_points_plot = restricted_points_plot + theme(legend.position="none")

gridExtra::grid.arrange(asl_plot, crossings_plot, cycle_lanes_plot, restrictedroutes_plot,
                        parking_sites_plot, parking_spaces_plot, signals_plot, traffic_calming_plot,
                        signage_plot, restricted_points_plot, legend4, nrow = 3, 
                        widths = c(0.7, 0.5, 0.5, 0.5))
# save count plot
plot_count2 = gridExtra::grid.arrange(asl_plot, crossings_plot, cycle_lanes_plot, restrictedroutes_plot,
                                      parking_sites_plot, parking_spaces_plot, signals_plot, traffic_calming_plot,
                                      signage_plot, restricted_points_plot, legend4, nrow = 3, 
                                      widths = c(0.7, 0.5, 0.5, 0.5))
  
ggsave(file = "/home/bananafan/Documents/PhD/Paper1/output/3counts_by_7assets.png", 
       plot_count2, width = 75 * (14/5), height = 65 * (14/5), units = "mm")
#the above seems to give perfect size for these 3 plots
