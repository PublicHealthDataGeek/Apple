################################################################################################
# This code looks to address the issue where observations in the CID that have a Borough coded #
# actually cross into other boroughs.  Therefore they need splitting and then assigning to the #
# correct borough.                                                                             #
################################################################################################



all_cycle_lanes_split = st_intersection(lon_lad_2020, f_cycle_lane_track) #25315
#nb original no of obs 24976
# so 339 extra obs
# BOROUGH is from ONS, BOROUGH.1 is from CID
cl_NA_b = all_cycle_lanes_split%>%
  filter(is.na(BOROUGH.1)) # = 621
cl_not_NA = all_cycle_lanes_split%>%
  filter(!is.na(BOROUGH.1)) # = 24694
anyNA(all_cycle_lanes_split$BOROUGH) # = FALSE - so all have a BOROUGH from ONS

#create smaller dataset Wandsworth according to CID
wandsworth = all_cycle_lanes_split %>%
  filter(BOROUGH.1 == "Wandsworth") # n= 974 
anyNA(wandsworth$BOROUGH.1) # = FALSE so all have Wandsworth from 

unique(wandsworth$BOROUGH.1) # so just wandsworth in the CID 
#[1] Wandsworth

unique(wandsworth$BOROUGH) # so even though CID labels as Wandsworth, ONS labels 
# as something else
#[1] Wandsworth           Merton              
#[3] Richmond upon Thames Lambeth             
#[5] Hammersmith & Fulham

wandsworth %>%
  group_by(BOROUGH) %>%
  count()  # counts how many 'Wandsworth' CID obs are labelled as other Boroughs by ONS data
# hammersmith 1
# lambeth     3
# merton      1
# Richmond    1
# wandworth     968

# check by comparing BOROUGH to BOROUGH.1
compare_pre = wandsworth$BOROUGH %in% wandsworth$BOROUGH.1
summary(compare_pre)
#   Mode   FALSE    TRUE 
# logical       6     968  # false fits with the 6 from above

# Need to factor BOROUGH.1 
borough_levels = c("Barking & Dagenham", "Barnet", "Bexley", "Brent",  
                  "Bromley", "Camden", "City of London", "Croydon", 
                  "Ealing", "Enfield", "Greenwich", "Hackney",  
                  "Hammersmith & Fulham", "Haringey", "Harrow", 
                  "Havering", "Hillingdon", "Hounslow", "Islington", 
                  "Kensington & Chelsea", "Kingston upon Thames",  
                  "Lambeth", "Lewisham", "Merton", "Newham", 
                  "Redbridge", "Richmond upon Thames", "Southwark",  
                  "Sutton", "Tower Hamlets", "Waltham Forest",   
                  "Wandsworth", "Westminster") 
wandsworth$BOROUGH.1 = factor(wandsworth$BOROUGH.1, levels = borough_levels)
levels(wandsworth$BOROUGH.1) # check has worked

# create loop to run through and recode 
for (i in seq_along(wandsworth$BOROUGH)) {
  wandsworth$BOROUGH.1[[i]] = wandsworth$BOROUGH[[i]]
}

wandsworth %>%
  group_by(BOROUGH.1) %>%
  count()  # counts to check we have the right number recoded 
# hammersmith 1
# lambeth     3
# merton      1
# Richmond    1
# wandsworth    968  YES IT WORKS!

compare_post = wandsworth$BOROUGH %in% wandsworth$BOROUGH.1
summary(compare_post) # all match



mapview(wandsworth, zcol = "BOROUGH.1") + 
  mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE)

wandsworth$length = st_length(wandsworth)
x = st_length(wandsworth)


wandsworth = all_cycle_lanes_split %>%
  filter(BOROUGH.1 == "Wandsworth" | is.na(BOROUGH.1)) # n = 1595 (974 + 621)



count(unique(wandsworth$BOROUGH))

mapview(all_cycle_lanes_split)
mapview(all_cycle_lanes_split, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE)
cycle_lanes_NA_map = mapview(lanes_borough_NA_i, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.1, zcol = "BOROUGH", legend = FALSE)



