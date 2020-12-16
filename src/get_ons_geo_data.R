##########################################
# Obtain latest local authority geometry #

# https://geoportal.statistics.gov.uk/datasets/7f83b82ef6ce46d3a5635d371e8a3e7c_0
# Local_Authority_Districts__May_2020__Boundaries_UK_BFC-shp.zip
# This file contains the digital vector boundaries for Local Authority Districts, in the United Kingdom, as at May 2020. 



lad_2020 <- st_read("/home/bananafan/Downloads/Local_Authority_Districts__May_2020__Boundaries_UK_BFC.shp")
# crs = OSGB 1936
boroughs$BOROUGH

lon_lad_names = data.frame(LAD20NM = c("Barking and Dagenham",
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
                           
 
lon_lad_2020 = left_join(lon_lad_names, lad_2020, "LAD20NM")              
                           
                                 
                  
                 



unlon_lad_2020

boroughs = rename(boroughs, BOROUGH = NAME)
boroughs$BOROUGH = fct_recode(boroughs$BOROUGH, "Kensington & Chelsea" = "Kensington and Chelsea", 
                              "Barking & Dagenham" = "Barking and Dagenham",
                              "Hammersmith & Fulham" = "Hammersmith and Fulham")