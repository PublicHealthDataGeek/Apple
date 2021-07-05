################################################################################
# Create maps of Safety assets
#
# This script creates maps of all the safety assets plus an orientation map and 
# a separate legend 
# 
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
library(ggspatial) # get north arrow and bar

# set mapview options so that matches crs
# mapviewOptions(native.crs = TRUE, legend = FALSE)

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


# Create context for map

# 1) Borough boundaries and labelling
boroughs <- st_read("/home/bananafan/Documents/PhD/Paper1/map_data/London_Borough_Excluding_MHW.shp")
borough_areas <- rmapshaper::ms_simplify(boroughs, keep=0.015) #Simplify boroughs
borough_areas = rename(boroughs, BOROUGH = NAME)

# borough_areas$B_numbered = fct_recode(borough_areas$BOROUGH, 
#                                  "7" = "Kensington and Chelsea",
#                                  "32" = "Barking and Dagenham",
#                                  "8" = "Hammersmith and Fulham",
#                                  "25" = "Kingston upon Thames",
#                                  "24" = "Richmond upon Thames",
#                                  "1" = "City of London",
#                                  "15" = "Waltham Forest",
#                                  "28" = "Croydon",
#                                  "29" = "Bromley",
#                                  "23" = "Hounslow",
#                                  "20" = "Ealing",
#                                  "31" = "Havering",
#                                  "22" = "Hillingdon",
#                                  "21" = "Harrow",
#                                  "19" = "Brent",
#                                  "18" = "Barnet",
#                                  "10" = "Lambeth",
#                                  "11" = "Southwark", 
#                                  "12" = "Lewisham",
#                                  "13" = "Greenwich",
#                                  "30" = "Bexley",
#                                  "17" = "Enfield",
#                                  "33" = "Redbridge",
#                                  "27" = "Sutton",
#                                  "26" = "Merton",
#                                  "9" = "Wandsworth",
#                                  "6" = "Westminster",
#                                  "5" = "Camden",
#                                  "2" = "Tower Hamlets",
#                                  "4" = "Islington",
#                                  "3" = "Hackney",
#                                  "16" = "Haringey",
#                                  "14" = "Newham")

borough_areas$b_acronym = fct_recode(borough_areas$BOROUGH, 
                                      "K&C" = "Kensington and Chelsea",
                                      "B&D" = "Barking and Dagenham",
                                      "H&F" = "Hammersmith and Fulham",
                                      "Kin" = "Kingston upon Thames",
                                      "Ric" = "Richmond upon Thames",
                                      "City" = "City of London",
                                      "Wal" = "Waltham Forest",
                                      "Cro" = "Croydon",
                                      "Bro" = "Bromley",
                                      "Hou" = "Hounslow",
                                      "Eal" = "Ealing",
                                      "Hav" = "Havering",
                                      "Hil" = "Hillingdon",
                                      "Har" = "Harrow",
                                      "Bre" = "Brent",
                                      "Bar" = "Barnet",
                                      "Lam" = "Lambeth",
                                      "Sou" = "Southwark", 
                                      "Lew" = "Lewisham",
                                      "Gre" = "Greenwich",
                                      "Bex" = "Bexley",
                                      "Enf" = "Enfield",
                                      "Red" = "Redbridge",
                                      "Sut" = "Sutton",
                                      "Mer" = "Merton",
                                      "Wan" = "Wandsworth",
                                      "Wes" = "Westminster",
                                      "Cam" = "Camden",
                                      "Tow" = "Tower Hamlets",
                                      "Isl" = "Islington",
                                      "Hac" = "Hackney",
                                      "Har" = "Haringey",
                                      "New" = "Newham")


# Create Inner London Borough list
inn_lon_B_list = c("City of London", "Camden", "Greenwich", "Hackney", "Hammersmith and Fulham", 
                   "Islington", "Kensington and Chelsea", "Lambeth", "Lewisham", "Southwark",  
                   "Tower Hamlets", "Wandsworth", "Westminster")
# Create inner London spatial object for boundary
inn_lon_union = borough_areas %>%
  filter(BOROUGH %in% inn_lon_B_list) %>%
  st_union()
# Create Outer London Spatial object for boundary
out_lon_union = borough_areas %>%
  filter(!BOROUGH %in% inn_lon_B_list) %>%
  st_union()

# 2) River thames
riverthames = st_read("/home/bananafan/Documents/PhD/Paper1/map_data/riverthames.shp")
riverthames_simplify = rmapshaper::ms_simplify(riverthames)

# 3) Motorways
motorways <- st_read("/home/bananafan/Documents/PhD/Paper1/map_data/motorways_outer.json") %>% 
  st_transform(crs=27700) 
box_new = c(xmin = 498745.5, ymin = 149044.6, xmax = 564000.0, ymax = 205391.0)
motorways = st_crop(motorways, box_new)
#box_orig = c(xmin = 498745.5, ymin = 149044.6, xmax = 569602.4, ymax = 205391.0)

# 4) Create map so can copy and paste legend
legend_plot = 
  ggplot()+
  geom_sf(data = out_lon_union, aes(colour = "myline1"), fill="white", show.legend = "line") +
  geom_sf(data = motorways, aes(colour = "myline3"), size = 0.14, show.legend = "line") +
  geom_sf(data = inn_lon_union, aes(colour = "myline2"), show.legend = "line") +
  geom_sf(data = borough_areas, fill="#d4d4d4",  colour="black", alpha=0.3, size=0.15)+
  geom_sf(data = riverthames_simplify, aes(colour = "myline4"), fill="#99CCEE",  show.legend = "line")+
  geom_sf_label(data = borough_areas, aes(label = b_acronym), label.padding = unit(0.15, "lines")) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        text = element_text(family = "Arial"),
        legend.title = element_blank()) + 
  coord_sf(crs=st_crs(riverthames_simplify), datum=NA) +
  ggtitle("a) Geographical features of London *") +
  xlab(label = NULL) +
  ylab(label = NULL) +
  scale_colour_manual(values = c(myline3 = "#b77107", myline1 = "black", myline2 = "#991100", myline4 = "#99CCEE"),
                      labels = c("Motorways", "Outer London boundary", "Inner London boundary", "River Thames")) +
  annotation_scale(location = "br", width_hint = 0.3, bar_cols = c("Gray83", "white"),
                   text_cex = 0.65, line_width = 0.5, line_col = "#222222",
                   text_family = "Arial") +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         height = unit(1.3, "cm"), width = unit(1.3, "cm"),
                         style = north_arrow_fancy_orienteering(line_width = 0.5, 
                                                                line_col = "#222222",
                                                                fill = c("white", "Gray83"),
                                                                text_size = 8, text_family = "Arial"))

# extract and convert to ggplot (so can save as jpeg)
legend = ggpubr::get_legend(legend_plot)
ggpubr::as_ggplot(legend)


# 6) Create 6 panel maps
# Create Orientation map
p0 = ggplot()+
  geom_sf(data = out_lon_union, colour = "black", fill="white") +
  geom_sf(data = motorways, colour = "#b77107", size = 0.14) +
  geom_sf(data = inn_lon_union, colour = "#991100") +
  geom_sf(data = borough_areas, fill="#d4d4d4",  colour="black", alpha=0.3, size=0.15) +
  geom_sf(data = riverthames_simplify, colour = "#99CCEE", fill="#99CCEE") +
  geom_sf_label(data = borough_areas, aes(label = b_acronym), label.padding = unit(0.15, "lines"), 
                label.size = 0.1, size = 2.8) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        text = element_text(family = "Arial")) + 
  coord_sf(crs=st_crs(riverthames_simplify), datum=NA) +
  ggtitle("a) Geographical features of London") +
  xlab(label = NULL) +
  ylab(label = NULL) +
  annotation_scale(location = "br", width_hint = 0.3, bar_cols = c("Gray83", "white"),
                   text_cex = 0.65, line_width = 0.5, line_col = "#222222",
                   text_family = "Arial") +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         height = unit(1.3, "cm"), width = unit(1.3, "cm"),
                         style = north_arrow_fancy_orienteering(line_width = 0.5, 
                                                                line_col = "#222222",
                                                                fill = c("white", "Gray83"),
                                                                text_size = 8, text_family = "Arial"))


p1 = ggplot()+
  geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
  geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
  geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
  geom_sf(data = c_asl_point, colour = alpha("blue", 0.2), size = 0.1) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
        text = element_text(family = "Arial")) +
  ggtitle("b) ASL") +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA) 

p2 = ggplot()+
  geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
  geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
  geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
  geom_sf(data = c_crossings_point, colour = alpha("blue", 0.2), size = 0.1) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
        text = element_text(family = "Arial")) + 
  ggtitle("c) Cycle crossings") +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA) 

p3 = ggplot()+
  geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
  geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
  geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
  geom_sf(data = c_cyclelanetrack, colour = alpha("blue", 0.2)) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
        text = element_text(family = "Arial")) + 
  ggtitle("d) Cycle lanes and tracks") +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA) 

p4 = ggplot()+
  geom_sf(data=motorways, fill="#EEEEEE",  colour="#EEEEEE")+
  geom_sf(data=borough_areas, fill="#d4d4d4",  colour="#444444", alpha=0.3, size=0.05)+
  geom_sf(data=riverthames_simplify, fill="#99CCEE",  colour="#99CCEE") +
  geom_sf(data = c_trafficcalming, colour = alpha("blue", 0.05), size = 0.1) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
        text = element_text(family = "Arial")) +
  ggtitle("e) Traffic calming measures") +
  coord_sf(crs=st_crs(riverthames_simplify), datum=NA) 

p5 = ggplot()+
  geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
  geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
  geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
  geom_sf(data = c_signals_point, colour = alpha("blue", 0.2), size = 0.1) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        text = element_text(family = "Arial")) + 
  ggtitle("f) Cycle signals") +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA) 

  
all_assets_map = gridExtra::grid.arrange(p0, p1, p2, p3, p4, p5, ncol = 3)



# Save image
## GGSAVE NOT WORKING HAD SIMILAT ISSUE WHEN DOING INTERNSHIP
##ggsave("/home/bananafan/Documents/PhD/Paper1/output/locations_map_plot.pdf", plot = locations_plot, 
##       dpi = 1000, width = 190 * (14/5), height = 142 * (14/5), units = "mm")


# Create 6 panel maps in black 
# Create Orientation map
# b0 = ggplot()+
#   geom_sf(data = out_lon_union, colour = "black", fill="white") +
#   geom_sf(data = motorways, colour = "#b77107", size = 0.14) +
#   geom_sf(data = inn_lon_union, colour = "#991100") +
#   geom_sf(data = borough_areas, fill="#d4d4d4",  colour="black", alpha=0.3, size=0.15) +
#   geom_sf(data = riverthames_simplify, colour = "#99CCEE", fill="#99CCEE") +
#   geom_sf_label(data = borough_areas, aes(label = b_acronym), label.padding = unit(0.15, "lines"), 
#                 label.size = 0.1, size = 2.8) +
#   theme_classic() +
#   theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
#         text = element_text(family = "Arial")) + 
#   coord_sf(crs=st_crs(riverthames_simplify), datum=NA) +
#   ggtitle("a) Geographical features of London") +
#   xlab(label = NULL) +
#   ylab(label = NULL) +
#   annotation_scale(location = "br", width_hint = 0.3, bar_cols = c("Gray83", "white"),
#                    text_cex = 0.65, line_width = 0.5, line_col = "#222222",
#                    text_family = "Arial") +
#   annotation_north_arrow(location = "tr", which_north = "true", 
#                          height = unit(1.3, "cm"), width = unit(1.3, "cm"),
#                          style = north_arrow_fancy_orienteering(line_width = 0.5, 
#                                                                 line_col = "#222222",
#                                                                 fill = c("white", "Gray83"),
#                                                                 text_size = 8, text_family = "Arial"))
# 
# 
# b1 = ggplot()+
#   geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
#   geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
#   geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
#   geom_sf(data = c_asl_point, colour = alpha("black", 0.2), size = 0.1) +
#   theme_classic() +
#   theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
#         text = element_text(family = "Arial")) +
#   ggtitle("b) ASL") +
#   coord_sf(crs = st_crs(riverthames_simplify), datum = NA) 
# 
# b2 = ggplot()+
#   geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
#   geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
#   geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
#   geom_sf(data = c_crossings_point, colour = alpha("black", 0.2), size = 0.1) +
#   theme_classic() +
#   theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
#         text = element_text(family = "Arial")) + 
#   ggtitle("c) Cycle crossings") +
#   coord_sf(crs = st_crs(riverthames_simplify), datum = NA) 
# 
# b3 = ggplot()+
#   geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
#   geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
#   geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
#   geom_sf(data = c_cyclelanetrack, colour = alpha("black", 0.2)) +
#   theme_classic() +
#   theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
#         text = element_text(family = "Arial")) + 
#   ggtitle("d) Cycle lanes and tracks") +
#   coord_sf(crs = st_crs(riverthames_simplify), datum = NA) 
# 
# b4 = ggplot()+
#   geom_sf(data=motorways, fill="#EEEEEE",  colour="#EEEEEE")+
#   geom_sf(data=borough_areas, fill="#d4d4d4",  colour="#444444", alpha=0.3, size=0.05)+
#   geom_sf(data=riverthames_simplify, fill="#99CCEE",  colour="#99CCEE") +
#   geom_sf(data = c_trafficcalming, colour = alpha("black", 0.05), size = 0.1) +
#   theme_classic() +
#   theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
#         text = element_text(family = "Arial")) +
#   ggtitle("e) Traffic calming measures") +
#   coord_sf(crs=st_crs(riverthames_simplify), datum=NA) 
# 
# b5 = ggplot()+
#   geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
#   geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
#   geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
#   geom_sf(data = c_signals_point, colour = alpha("black", 0.2), size = 0.1) +
#   theme_classic() +
#   theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
#         text = element_text(family = "Arial")) + 
#   ggtitle("f) Cycle signals") +
#   coord_sf(crs = st_crs(riverthames_simplify), datum = NA) 
# 
# 
# gridExtra::grid.arrange(b0, b1, b2, b3, b4, b5, ncol = 3)


