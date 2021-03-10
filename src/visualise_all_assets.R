##########################
# Create maps of all assets


# install packages
library(tidyverse)
library(sf)
library(mapview)
library(leafsync)
# library(leaflet)
# library(leafem)
# library(forcats)
# library(units)

# set mapview options so that matches crs
mapviewOptions(native.crs = TRUE, legend = FALSE)

# color options
# https://www.pagetutor.com/common/bgcolors1536.png

# import May 2020 ONS LA boundary data (required for NA management)
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

# Create spatial data for Inner/Outer London areas
Inner = c("City of London", "Camden", "Greenwich", "Hackney", "Hammersmith & Fulham", 
          "Islington", "Kensington & Chelsea", "Lambeth", "Lewisham", "Southwark",  
          "Tower Hamlets", "Wandsworth", "Westminster") 

inner_outer_london = lon_lad_2020 %>%
  mutate(London = ifelse(BOROUGH %in% Inner, "Inner", "Outer")) # 14 variables

city_london = inner_outer_london %>%
  filter(BOROUGH == "City of London")

inner_london = inner_outer_london %>%
  filter(London == "Inner")
inner_london_union = sf::st_union(inner_london)

outer_london = inner_outer_london %>%
  filter(London == "Outer")
outer_london_union = sf::st_union(outer_london)

# # Example of city_inner_outer boundaries with pale blue background 
# mapview(city_london, alpha.regions = 0.1, lwd = 0.5) + 
#   mapview(inner_london_union, alpha.regions = 0.1, lwd = 0.5) + 
#   mapview(outer_london_union,alpha.regions = 0.1, lwd = 0.5)




# Load asset datasets - these datasets were downloaded from TFL 25th February 2021
c_asl = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_asl")
c_crossings = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_crossings")
c_cyclelanetrack = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_cycle_lane_track")
c_Rroutes = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_restricted_route")
c_Rpoints = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_restrictedpoints")
c_parking = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_parking")
c_signage = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_signage")
c_signals = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_signals")
c_trafficcalming = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_trafficcalming")

# Example maps
# viridis points on viridis boroughs
m1 = mapview(c_asl, zcol = "BOROUGH", cex = 2) + mapview(lon_lad_2020, alpha.regions = 0.1, zcol = "BOROUGH", lwd = 0.5)
m1 = removeMapJunk(m1, c("zoomControl", "layersControl", "homeButton", "drawToolbar", "easyButton"))

# black points on viridis 
m2 = mapview(c_asl$geometry, color = "black", cex = 2) + mapview(lon_lad_2020, alpha.regions = 0.1, zcol = "BOROUGH", lwd = 0.5)
m2 = removeMapJunk(m2, c("zoomControl", "layersControl", "homeButton", "drawToolbar", "easyButton"))

# dark grey points on viridis 
m3 = mapview(c_asl$geometry, color = "#555555", cex = 2) + mapview(lon_lad_2020, alpha.regions = 0.1, zcol = "BOROUGH", lwd = 0.5)
m3 = removeMapJunk(m3, c("zoomControl", "layersControl", "homeButton", "drawToolbar", "easyButton"))

# city_inner_outer boundaries with pale blue background 
m4 = mapview(c_asl, zcol = "BOROUGH", cex = 2) + 
  mapview(inner_london_union, alpha.regions = 0.05, lwd = 1) + 
  mapview(outer_london_union,alpha.regions = 0.2, lwd = 1)
m4 = removeMapJunk(m4, c("zoomControl", "layersControl", "homeButton", "drawToolbar", "easyButton"))

# viridis points on viridis boroughs
m5 = mapview(c_cyclelanetrack, zcol = "BOROUGH", lwd = 0.9) + mapview(lon_lad_2020, alpha.regions = 0.1, zcol = "BOROUGH", lwd = 0.5)
m5 = removeMapJunk(m5, c("zoomControl", "layersControl", "homeButton", "drawToolbar", "easyButton"))

# black points on viridis 
m6 = mapview(c_cyclelanetrack$geometry, color = "black", lwd = 0.9) + mapview(lon_lad_2020, alpha.regions = 0.1, zcol = "BOROUGH", lwd = 0.5)
m6 = removeMapJunk(m6, c("zoomControl", "layersControl", "homeButton", "drawToolbar", "easyButton"))

# dark grey points on viridis 
m7 = mapview(c_cyclelanetrack$geometry, color = "#555555", lwd = 0.9) + mapview(lon_lad_2020, alpha.regions = 0.1, zcol = "BOROUGH", lwd = 0.5)
m7 = removeMapJunk(m7, c("zoomControl", "layersControl", "homeButton", "drawToolbar", "easyButton"))

# city_inner_outer boundaries with pale blue background 
m8 = mapview(c_cyclelanetrack, zcol = "BOROUGH", lwd = 0.9) + 
  mapview(inner_london_union, alpha.regions = 0.05, lwd = 1) + 
  mapview(outer_london_union,alpha.regions = 0.1, lwd = 1)
m8 = removeMapJunk(m8, c("zoomControl", "layersControl", "homeButton", "drawToolbar", "easyButton"))

points_eg = latticeView(m1,m2,m3,m4, ncol = 4, sync = "none")
lines_eg = latticeView(m5,m6,m7,m8, ncol = 4, sync = "none")



