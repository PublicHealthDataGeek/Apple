##########################################
# Obtain latest local authority geometry #
##########################################



# Need full resoluation boundary data to accurately allocate geographical data
# Need extent of realm boundary sets because these allocate based on mean low water so necessary as many
# London boroughs have the Thames running through (if use clipped to coastline then they show mean high water)
# see this document for details:
# https://ago-item-storage.s3.us-east-1.amazonaws.com/13e58e58db52423b91a3eb99c67a8c40/Boundary_Dataset_Guidance_2011_Onwards.pdf?X-Amz-Security-Token=IQoJb3JpZ2luX2VjEGoaCXVzLWVhc3QtMSJHMEUCIAgsfQKuTGnVerFjTZUwmgs%2BlVOvao1AEDXtiF%2F%2Fx8aUAiEAs2sOLgBt5ilcjzTO8HZjUVZ2bwWOJ%2BQd%2BvqSknacV3AqtAMIMhAAGgw2MDQ3NTgxMDI2NjUiDNT%2BOPWveAkthS7zLSqRA%2Bfgc3%2BdGbRiMOhCSTbYxiCrsxdCcYNIIJW4A5W6c75UTyRPyl9WVbGC4QNWL6Yf0ttDDhSUEGNE1EsYMeJ4jM%2B9q3hE19qB15v1BdF78%2BkZFQkI52mj6FtX7no2TwTcm3K87CIJofHY2kSV%2BA89F2xtqiR79icyvFmogaUNn%2FSMgM85VFhjMBK%2FKcMC8RM%2Fsvi6%2B0IOT8LJSGWXrsPtMBHZ7m3VLPxOdmdImpCAFJ5ZK%2FiNBIQ9nD6oUdqCqMP%2BoCCDfEvYg%2Fr3fkCOX%2F0vI0tO4WddvqV192swsPPn7Eb4vNzaBEaPgwpdggt2btWJH%2B7fpDjzYepKrpPPiRpSLCD6tWWBkr%2Blzx6zGYQ8jhGa5I%2FsuxF6DfziGwVp5zEyp79x5Ri7YZzNK1UzovaOzPZfq3ZxcVnAMy02K%2F0YXFalKP82RXWG9Zgma%2FNA2ZS5n8hdqiRgqu4bNNy8tKq3%2FQhHPTcU2IvY%2FI7HYgUod4I4fKn2TdiwlzGw6CZcpWez%2FDHqSgr%2FFljCwj%2FfQTvi0tkXMJbf1%2F8FOusBMC1aPD9i81DZMZtsPsUgjxrALewdoY7wj1hpgQ%2Fhc1p7Sz0OYXFAWLTWFxWWYudK21KHlXvVIPXyhJ%2B4mWOQRAPhg0F%2BBtqMA1Of5hPTj%2F%2FcrIIkiJzq5xzDS%2FgG3dUM7QpGbwBEO4vLCl3XdsNPjUfNQc2rhZeKAv%2Bcd4ksXMbkdg7yak9OQ2jK2b2gRDYbZ1CX1l6VnwYYgz1frPgBGK3n2Q42rNGLUhhPx5W28xT9%2FFqVXprV%2FRvTpoyOj1VhO6omMEEPViHkYjgpsCsa18ERi9ByRrD7RuiwBns429Mg86tGjLWIvCY7Bg%3D%3D&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20210106T175151Z&X-Amz-SignedHeaders=host&X-Amz-Expires=300&X-Amz-Credential=ASIAYZTTEKKEU4WZDWED%2F20210106%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=f5fdc2dd510ea45843221fcdd84424e4b223832db8ad5732886ea6282540324a

# Therefore need th following ONS dataset
# Local Authority Districts (May 2020) Boundaries UK BFE
# Available at: 
# https://geoportal.statistics.gov.uk/datasets/local-authority-districts-may-2020-boundaries-uk-bfe
# This file contains the digital vector boundaries for Local Authority Districts, in the United Kingdom, as at May 2020. 

# The boundaries available are: (BFE) Full resolution - extent of the realm (usually this is the Mean Low Water mark 
# but in some cases boundaries extend beyond this to include off shore islands).


library(sf)
library(tidyverse)

# Import all May 2020 Local Authority District Boundaries (ONS dataset)
lad_2020_bfe <- st_read("/home/bananafan/Downloads/Local_Authority_Districts__May_2020__Boundaries_UK_BFE.shp")

# check CRS
st_crs(lad_2020_bfe) # PROJCRS["OSGB 1936 / British National Grid",

# rename column name so can join
lad_2020_bfe = lad_2020_bfe %>% rename(BOROUGH = lad20nm)

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
lon_lad_2020_bfe = st_as_sf(left_join(lon_lad_names, lad_2020_bfe, "BOROUGH"))              

# Convert Borough names to factors and recode so that Borough names match characters used in TFL CID
lon_lad_2020_bfe$BOROUGH = as.factor(lon_lad_2020_bfe$BOROUGH) 
lon_lad_2020_bfe$BOROUGH = fct_recode(lon_lad_2020_bfe$BOROUGH, "Kensington & Chelsea" = "Kensington and Chelsea", 
                                  "Barking & Dagenham" = "Barking and Dagenham",
                                  "Hammersmith & Fulham" = "Hammersmith and Fulham")                            

# Save dataframe
saveRDS(lon_lad_2020_bfe, file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")



################################################################################################################################
# OLD CODE - DONT USE THIS DATASET
#
# THis is old code when I used the BFC boundaries but this meant that the boundaries for LA across the Thames were not included:
#

# https://geoportal.statistics.gov.uk/datasets/7f83b82ef6ce46d3a5635d371e8a3e7c_0
# Local_Authority_Districts__May_2020__Boundaries_UK_BFC-shp.zip
# This file contains the digital vector boundaries for Local Authority Districts, in the United Kingdom, as at May 2020. 

# library(sf)
# library(tidyverse)
# 
# # Import all May 2020 Local Authority District Boundaries (ONS dataset)
# lad_2020 <- st_read("/home/bananafan/Downloads/Local_Authority_Districts__May_2020__Boundaries_UK_BFC.shp")
# 
# # check CRS
# st_crs(lad_2020) # PROJCRS["OSGB 1936 / British National Grid",
# 
# # rename column name so can join
# lad_2020 = lad_2020 %>% rename(BOROUGH = LAD20NM)
# 
# # create df of London Borough names
# lon_lad_names = data.frame(BOROUGH = c("Barking and Dagenham",
#                  "Barnet",  
#                  "Bexley", 
#                  "Brent", 
#                  "Bromley",
#                  "Camden",
#                  "City of London",
#                  "Croydon",
#                  "Ealing",
#                  "Enfield",
#                  "Greenwich", 
#                  "Hackney", 
#                  "Hammersmith and Fulham", 
#                  "Haringey",
#                  "Harrow",
#                  "Havering",
#                  "Hillingdon",
#                  "Hounslow",
#                  "Islington",
#                  "Kensington and Chelsea", 
#                  "Kingston upon Thames", 
#                  "Lambeth", 
#                  "Lewisham", 
#                  "Merton",
#                  "Newham",
#                  "Redbridge",
#                  "Richmond upon Thames",
#                  "Southwark", 
#                  "Sutton", 
#                  "Tower Hamlets", 
#                  "Waltham Forest",  
#                  "Wandsworth", 
#                  "Westminster"))
#                            
# # Join London LA names to the LA boundaries so that only keep London LA details
# lon_lad_2020 = st_as_sf(left_join(lon_lad_names, lad_2020, "BOROUGH"))              
#                            
# # Convert Borough names to factors and recode so that Borough names match characters used in TFL CID
# lon_lad_2020$BOROUGH = as.factor(lon_lad_2020$BOROUGH) 
# lon_lad_2020$BOROUGH = fct_recode(lon_lad_2020$BOROUGH, "Kensington & Chelsea" = "Kensington and Chelsea", 
#                               "Barking & Dagenham" = "Barking and Dagenham",
#                               "Hammersmith & Fulham" = "Hammersmith and Fulham")                            
#                                  
# Save dataframe
# saveRDS(lon_lad_2020_BFC, file = "./map_data/lon_LAD_boundaries_May_2020.Rds")
# Renames this dataset as BFC in the map_data folder
