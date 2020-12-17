##########################################
# Obtain latest local authority geometry #
##########################################

# https://geoportal.statistics.gov.uk/datasets/7f83b82ef6ce46d3a5635d371e8a3e7c_0
# Local_Authority_Districts__May_2020__Boundaries_UK_BFC-shp.zip
# This file contains the digital vector boundaries for Local Authority Districts, in the United Kingdom, as at May 2020. 

library(sf)
library(tidyverse)

# Import all May 2020 Local Authority District Boundaries (ONS dataset)
lad_2020 <- st_read("/home/bananafan/Downloads/Local_Authority_Districts__May_2020__Boundaries_UK_BFC.shp")

# check CRS
st_crs(lad_2020) # PROJCRS["OSGB 1936 / British National Grid",

# rename column name so can join
lad_2020 = lad_2020 %>% rename(BOROUGH = LAD20NM)

# create df of London Borough names
lon_lad_names = data.frame(BOROUGH = c("Barking and Dagenham",
                 "Barnet",  
                 "Bexley", 
                 "Brent", 
                 "Bromley",
                 "Camden",
                 "City of London",
                 "Croydon",
                 "Ealing",
                 "Enfield",
                 "Greenwich", 
                 "Hackney", 
                 "Hammersmith and Fulham", 
                 "Haringey",
                 "Harrow",
                 "Havering",
                 "Hillingdon",
                 "Hounslow",
                 "Islington",
                 "Kensington and Chelsea", 
                 "Kingston upon Thames", 
                 "Lambeth", 
                 "Lewisham", 
                 "Merton",
                 "Newham",
                 "Redbridge",
                 "Richmond upon Thames",
                 "Southwark", 
                 "Sutton", 
                 "Tower Hamlets", 
                 "Waltham Forest",  
                 "Wandsworth", 
                 "Westminster"))
                           
# Join London LA names to the LA boundaries so that only keep London LA details
lon_lad_2020 = st_as_sf(left_join(lon_lad_names, lad_2020, "BOROUGH"))              
                           
# Convert Borough names to factors and recode so that Borough names match characters used in TFL CID
lon_lad_2020$BOROUGH = as.factor(lon_lad_2020$BOROUGH) 
lon_lad_2020$BOROUGH = fct_recode(lon_lad_2020$BOROUGH, "Kensington & Chelsea" = "Kensington and Chelsea", 
                              "Barking & Dagenham" = "Barking and Dagenham",
                              "Hammersmith & Fulham" = "Hammersmith and Fulham")                            
                                 
# Save dataframe
saveRDS(lon_lad_2020, file = "./map_data/lon_LAD_boundaries_May_2020.Rds")
