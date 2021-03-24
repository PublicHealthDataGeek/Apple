#############################
# Create maps of Safety assets
#
# This script creates maps for the paper
# Needs to include for Journal of Transport Geography
# - scale
# - north
# - legend
# - TIFF JPEG EPS or PDF
# - fonts - Arial (or Helvetica), Courier, Symbol, Times (or Times New Roman)
# - Double column (full width) 	190 mm (539 pt) 	2244 	3740 	7480


# install packages
library(tidyverse)
library(sf)
library(mapview)
library(ggspatial) # get north arrow and bar
library(patchwork) # arrange ggplots
#library(leafsync)
# library(leaflet)
# library(leafem)
# library(forcats)
# library(units)
#library(sp)
#library(rmapshaper)
#library(ggforce)
#library(gridExtra)
#library(geojsonsf)

# set mapview options so that matches crs
mapviewOptions(native.crs = TRUE, legend = FALSE)

# color options
# https://www.pagetutor.com/common/bgcolors1536.png


# Load asset datasets - these datasets were downloaded from TFL 25th February 2021
c_asl = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_asl")
c_crossings = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_crossings")
c_cyclelanetrack = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_cycle_lane_track")
c_signals = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_signals")
c_trafficcalming = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_trafficcalming")

# convert certain assets to point data
c_asl_point = st_centroid(c_asl)
c_crossings_point = st_centroid(c_crossings)
c_signals_point = st_centroid(c_signals)
c_trafficcalming = st_centroid(c_trafficcalming)

# import May 2020 ONS LA boundary data (required for NA management)
#lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

# Create context for map
boroughs <- st_read("/home/bananafan/Documents/PhD/Paper1/map_data/London_Borough_Excluding_MHW.shp")
borough_areas <- rmapshaper::ms_simplify(boroughs, keep=0.015) #Simplify boroughs
borough_areas = rename(boroughs, BOROUGH = NAME)
borough_areas$SHORT = fct_recode(borough_areas$BOROUGH, 
                                    "KNS" = "Kensington and Chelsea",
                                    "BAR" = "Barking and Dagenham",
                                    "HMS" = "Hammersmith and Fulham",
                                    "KNG" = "Kingston upon Thames",
                                    "RCH" = "Richmond upon Thames",
                                    "CTY" = "City of London",
                                    "WTH" = "Waltham Forest",
                                    "CRD" = "Croydon",
                                    "BRM" = "Bromley",
                                    "HNS" = "Hounslow",
                                    "ELG" = "Ealing",
                                    "HVG" = "Havering",
                                    "HDN" = "Hillingdon",
                                    "HRW" = "Harrow",
                                    "BRT" = "Brent",
                                    "BRN" = "Barnet",
                                    "LAM" = "Lambeth",
                                    "SWR" = "Southwark", 
                                    "LSH" = "Lewisham",
                                    "GRN" = "Greenwich",
                                    "BXL" = "Bexley",
                                    "ENF" = "Enfield",
                                    "RDB" = "Redbridge",
                                    "STN" = "Sutton",
                                    "MRT" = "Merton",
                                    "WNS" = "Wandsworth",
                                    "WST" = "Westminster",
                                    "CMD" = "Camden",
                                    "TOW" = "Tower Hamlets",
                                    "ISL" = "Islington",
                                    "HCK" = "Hackney",
                                    "HGY" = "Haringey",
                                    "NWM" = "Newham")
### using short names in tmap
# mapped_boroughs1 = tm_shape(mapping_boroughs) +
#   tm_fill(col = "ivory2") +
#   tm_borders() +
#   tm_text("SHORT", size = 0.7) +
#   tm_layout(bg.color = "lightblue")

riverthames = st_read("/home/bananafan/Documents/PhD/Paper1/map_data/riverthames.shp")
riverthames_simplify = rmapshaper::ms_simplify(riverthames)

motorways <- st_read("/home/bananafan/Documents/PhD/Paper1/map_data/motorways_outer.json") %>% 
  st_transform(crs=27700) 
box_new = c(xmin = 498745.5, ymin = 149044.6, xmax = 564000.0, ymax = 205391.0)
motorways = st_crop(motorways, box_new)
#box_orig = c(xmin = 498745.5, ymin = 149044.6, xmax = 569602.4, ymax = 205391.0)







# Create base maps
 # with and without scales and arrows
baseplot_arrow_scale = ggplot()+
  geom_sf(data=motorways, fill="#EEEEEE",  colour="#EEEEEE")+
  geom_sf(data=borough_areas, fill="#d4d4d4",  colour="#444444", alpha=0.3, size=0.05)+
  geom_sf(data=riverthames_simplify, fill="#99CCEE",  colour="#99CCEE")+
  theme_bw() +
  coord_sf(crs=st_crs(riverthames_simplify), datum=NA) +
  annotation_scale(location = "br", width_hint = 0.3, bar_cols = c("Gray83", "white"),
                   text_cex = 0.5, line_width = 0.5, line_col = "#222222") +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         height = unit(1.3, "cm"), width = unit(1.3, "cm"),
                         #pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering(line_width = 0.5, 
                                                                line_col = "#222222",
                                                                fill = c("white", "Gray83"),
                                                                text_size = 8))

baseplot = ggplot()+
  geom_sf(data=motorways, fill="#EEEEEE",  colour="#EEEEEE")+
  geom_sf(data=borough_areas, fill="#d4d4d4",  colour="#444444", alpha=0.3, size=0.05)+
  geom_sf(data=riverthames_simplify, fill="#99CCEE",  colour="#99CCEE")+
  theme_bw() +
  coord_sf(crs=st_crs(riverthames_simplify), datum=NA)





# Create all 5 plots
# transparency = 0
p1 = ggplot()+
  geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
  geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
  geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
  geom_sf(data = c_asl_point, colour = "red", size = 0.1) +
  theme_bw() +
  theme(plot.title = element_text(vjust = - 9, hjust = 0.1, size = 18), 
        plot.margin = unit(c(0, 0.5, 0, 0.5), "cm")) +
  ggtitle("ASL at traffic-controlled signals") +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA) 

p2 = ggplot()+
  geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
  geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
  geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
  geom_sf(data = c_crossings_point, colour = "red", size = 0.1) +
  theme_bw() +
  theme(plot.title = element_text(vjust = - 9, hjust = 0.1, size = 18),
        plot.margin = unit(c(0, 0.5, 0, 0.5), "cm")) +  # makes bottom margin 0
  ggtitle("Cycle crossings") +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA) 

p3 = ggplot()+
  geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
  geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
  geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
  geom_sf(data = c_cyclelanetrack, colour = "red") +
  theme_bw() +
  theme(plot.title = element_text(vjust = - 9, hjust = 0.1, size = 18),
        plot.margin = unit(c(0, 0.5, 0, 0.5), "cm")) + 
  ggtitle("Cycle lanes and tracks") +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA) 


p4 = ggplot()+
  geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
  geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
  geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
  geom_sf(data = c_signals_point, colour = alpha("red", 0.2), size = 0.1) +
  theme_bw() +
  theme(plot.title = element_text(vjust = - 9, hjust = 0.1, size = 18),
        plot.margin = unit(c(0, 0.5, 0.5, 0.5), "cm")) + 
  ggtitle("Traffic signals for cycles") +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA) 

p5 = ggplot()+
  geom_sf(data=motorways, fill="#EEEEEE",  colour="#EEEEEE")+
  geom_sf(data=borough_areas, fill="#d4d4d4",  colour="#444444", alpha=0.3, size=0.05)+
  geom_sf(data=riverthames_simplify, fill="#99CCEE",  colour="#99CCEE") +
  geom_sf(data = c_trafficcalming, colour = "red", size = 0.1) +
  theme_bw() +
  theme(plot.title = element_text(vjust = - 9, hjust = 0.1, size = 18), 
        plot.margin = unit(c(0, 0.5, 0.5, 0.5), "cm")) +
  ggtitle("Traffic calming measures") +
  coord_sf(crs=st_crs(riverthames_simplify), datum=NA) +
  annotation_scale(location = "br", width_hint = 0.3, bar_cols = c("Gray83", "white"),
                   text_cex = 1, line_width = 0.5, line_col = "#222222") +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         height = unit(1.3, "cm"), width = unit(1.3, "cm"),
                         pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering(line_width = 0.5, 
                                                                line_col = "#222222",
                                                                fill = c("white", "Gray83"),
                                                                text_size = 12))



# Use patchwork to create plot
locations_plot = (p1 | p2 | p3) / (p4 | p5)


#### CREATE NEW EAXMPLE WITH TRANSPARENCY _ below code alters transpoarent
geom_sf(data = c_signals_point, colour = alpha("red", 0.2), size = 0.1) +


# Save image
## GGSAVE NOT WORKING HAD SIMILAT ISSUE WHEN DOING INTERNSHIP
##ggsave("/home/bananafan/Documents/PhD/Paper1/output/locations_map_plot.pdf", plot = locations_plot, 
##       dpi = 1000, width = 190 * (14/5), height = 142 * (14/5), units = "mm")



  
  
  
# # Getting river thames   
# gl_pbf = "/home/bananafan/Documents/PhD/Paper1/data/greater-london-200101.osm.pbf"
# gl_osm_multipolygons = oe_read(gl_pbf, quiet = FALSE, layer = "multipolygons") # 546748 features, 25 fields, multipolygon, crs= WGS84 
# gl_osm_multipolygons = st_transform(gl_osm_multipolygons, crs=27700) 
#water = gl_osm_multipolygons %>%
#  filter(natural == "water")
# riverthames = water %>%
#   filter(name == "River Thames")
# riverthames = st_union(riverthames)
# m1 = mapview(riverthames)
# rt_simplify <- rmapshaper::ms_simplify(riverthames)
# m2 = mapview(rt_simplify)
# leafsync::sync(m1,m2)
# st_write(riverthames, "/home/bananafan/Documents/PhD/Paper1/map_data/riverthames.shp")

