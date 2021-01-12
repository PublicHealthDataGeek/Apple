####################################################################################
# Visualise Borough level asset data 
####################################
# Date created: 11/1/21
#                                    
# This code takes the Borough level asset data by count and length and visualises
# the raw numbers and numbers per various denominators such as population, 
# borough area etc
#
####################################################################################

##########################
# NEED TO DO CYCLE PARKING SPACE NUMBERS
#


# Load packages
library(tidyverse)
library(mapview)
library(GGally) # parallel coordinate extension of ggplot


# Load datasets
# CID asset data
count_CID_by_borough = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/output/CID_count_by_borough")
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
  left_join(lon_pop_estimates_2019)

# add column for inner/outer london
Inner = c("City of London", "Camden", "Greenwich", "Hackney", "Hammersmith & Fulham", 
          "Islington", "Kensington & Chelsea", "Lambeth", "Lewisham", "Southwark",  
          "Tower Hamlets", "Wandsworth", "Westminster") 

count_CID_by_borough = count_CID_by_borough %>%
  mutate(London = ifelse(BOROUGH %in% Inner, "Inner", 
                         ifelse(BOROUGH %in% "No Borough stated in CID", "Unknown", "Outer")))

          
# produce counts by area and per head population (NB may want to consider per 100,000 pop at some point)
count_CID_by_borough_denom <- count_CID_by_borough %>%
  mutate(across(.cols = c("ASL", "Crossings", "CycleLanesAndTracks", "RestrictedRoutes", "CycleParking",
                          "Signals", "TrafficCalming", "Signage", "RestrictedPoints"),
                .fns = ~.x/Borough_Area,
                .names = "{.col}_count_by_area")) %>%
  mutate(across(.cols = c("ASL", "Crossings", "CycleLanesAndTracks", "RestrictedRoutes", "CycleParking",
                          "Signals", "TrafficCalming", "Signage", "RestrictedPoints"),
                .fns = ~.x/Population,
                .names = "{.col}_count_per_head"))  


# rank each borough
# use rank(-.x) to get ranking so that borough with most assets has highest ranking
count_CID_by_borough_rank = count_CID_by_borough_denom %>%
  mutate(across(.cols = c("ASL_count_by_area", "Crossings_count_by_area", 
                          "CycleLanesAndTracks_count_by_area", "RestrictedRoutes_count_by_area", 
                          "CycleParking_count_by_area", "Signals_count_by_area", 
                          "TrafficCalming_count_by_area", "Signage_count_by_area", 
                          "RestrictedPoints_count_by_area"),
                 .fns = ~round(rank(-.x)),                         
                 .names = "{.col}_rank")) %>%
  mutate(across(.cols = c("ASL_count_per_head", "Crossings_count_per_head", 
                          "CycleLanesAndTracks_count_per_head", "RestrictedRoutes_count_per_head", 
                          "CycleParking_count_per_head", "Signals_count_per_head", 
                          "TrafficCalming_count_per_head", "Signage_count_per_head", 
                          "RestrictedPoints_count_per_head"),
                .fns = ~round(rank(-.x)),
                .names = "{.col}_rank")) %>%
  mutate(across(.cols = c("ASL", "Crossings", 
                          "CycleLanesAndTracks", "RestrictedRoutes", 
                          "CycleParking", "Signals", 
                          "TrafficCalming", "Signage", 
                          "RestrictedPoints"),
                .fns = ~round(rank(-.x)), 
                .names = "{.col}_count_rank")) 


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

# get legend
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

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


# NB no cycle parking numbers data

# relevent columns in dataset

#[1] "BOROUGH"                                
# [13] "London"                                 
# [32] "ASL_count_by_area_rank"                 
# [33] "Crossings_count_by_area_rank"           
# [34] "CycleLanesAndTracks_count_by_area_rank" 
# [35] "RestrictedRoutes_count_by_area_rank"    
# [36] "CycleParking_count_by_area_rank"        
# [37] "Signals_count_by_area_rank"             
# [38] "TrafficCalming_count_by_area_rank"      
# [39] "Signage_count_by_area_rank"             
# [40] "RestrictedPoints_count_by_area_rank"    
# [41] "ASL_count_per_head_rank"                
# [42] "Crossings_count_per_head_rank"          
# [43] "CycleLanesAndTracks_count_per_head_rank"
# [44] "RestrictedRoutes_count_per_head_rank"   
# [45] "CycleParking_count_per_head_rank"       
# [46] "Signals_count_per_head_rank"            
# [47] "TrafficCalming_count_per_head_rank"     
# [48] "Signage_count_per_head_rank"            
# [49] "RestrictedPoints_count_per_head_rank"   
# [50] "ASL_count_rank"                         
# [51] "Crossings_count_rank"                   
# [52] "CycleLanesAndTracks_count_rank"         
# [53] "RestrictedRoutes_count_rank"            
# [54] "CycleParking_count_rank"                
# [55] "Signals_count_rank"                     
# [56] "TrafficCalming_count_rank"              
# [57] "Signage_count_rank"                     
# [58] "RestrictedPoints_count_rank"  



# Borough level data

brgh_count_plot = ggparcoord(count_CID_by_borough_rank, 
           columns = rev(50:58), 
           scale = "globalminmax",    # uses the rank rather that scaling to something else
           groupColumn = 13,
           alphaLines = 0.8,
           title = "Count of infrastructure ranked by Borough") +
  scale_color_manual(values =c("maroon", "dark gray", "black")) +  # inner, outer, missing
  coord_flip()  +
  theme_minimal()
# save borough count plot
ggsave(file = "/home/bananafan/Documents/PhD/Paper1/output/brgh_count_plot.png", 
       brgh_count_plot, width = 95 * (14/5), height = 53 * (14/5), units = "mm")

