################################################################################
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

# Outstanding actions/ CURRENT STATUS as of 10 June2021
# – have legend built ?include or not 
# just need to sort out how this looks with the overall map - 
#   ? Make the plot margins for the overall map bigger then inset the legend and wording inside?  
#   NEED to agree transparency thing 




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

borough_areas$B_numbered = fct_recode(borough_areas$BOROUGH, 
                                 "7" = "Kensington and Chelsea",
                                 "32" = "Barking and Dagenham",
                                 "8" = "Hammersmith and Fulham",
                                 "25" = "Kingston upon Thames",
                                 "24" = "Richmond upon Thames",
                                 "1" = "City of London",
                                 "15" = "Waltham Forest",
                                 "28" = "Croydon",
                                 "29" = "Bromley",
                                 "23" = "Hounslow",
                                 "20" = "Ealing",
                                 "31" = "Havering",
                                 "22" = "Hillingdon",
                                 "21" = "Harrow",
                                 "19" = "Brent",
                                 "18" = "Barnet",
                                 "10" = "Lambeth",
                                 "11" = "Southwark", 
                                 "12" = "Lewisham",
                                 "13" = "Greenwich",
                                 "30" = "Bexley",
                                 "17" = "Enfield",
                                 "33" = "Redbridge",
                                 "27" = "Sutton",
                                 "26" = "Merton",
                                 "9" = "Wandsworth",
                                 "6" = "Westminster",
                                 "5" = "Camden",
                                 "2" = "Tower Hamlets",
                                 "4" = "Islington",
                                 "3" = "Hackney",
                                 "16" = "Haringey",
                                 "14" = "Newham")



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


### using short names in tmap
# mapped_boroughs1 = tm_shape(mapping_boroughs) +
#   tm_fill(col = "ivory2") +
#   tm_borders() +
#   tm_text("SHORT", size = 0.7) +
#   tm_layout(bg.color = "lightblue")

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
  geom_sf_label(data = borough_areas, aes(label = B_numbered)) +
  theme_bw() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        text = element_text(family = "Arial")) + 
  coord_sf(crs=st_crs(riverthames_simplify), datum=NA) +
  ggtitle("a) Geographical features of London *") +
  xlab(label = NULL) +
  ylab(label = NULL) +
  scale_colour_manual(name = "Legend", 
                      values = c(myline3 = "#b77107", myline1 = "black", myline2 = "#991100", myline4 = "#99CCEE"),
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

# Create basemap
p0 = ggplot()+
  geom_sf(data = out_lon_union, colour = "black", fill="white") +
  geom_sf(data = motorways, colour = "#b77107", size = 0.14) +
  geom_sf(data = inn_lon_union, colour = "#991100") +
  geom_sf(data = borough_areas, fill="#d4d4d4",  colour="black", alpha=0.3, size=0.15) +
  geom_sf(data = riverthames_simplify, colour = "#99CCEE", fill="#99CCEE") +
  geom_sf_label(data = borough_areas, aes(label = B_numbered)) +
  theme_bw() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        text = element_text(family = "Arial")) + 
  coord_sf(crs=st_crs(riverthames_simplify), datum=NA) +
  ggtitle("a) Geographical features of London *") +
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

# Create all 5 plots
# transparency = 0
p1 = ggplot()+
  geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
  geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
  geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
  geom_sf(data = c_asl_point, colour = "red", size = 0.1) +
  theme_bw() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
        text = element_text(family = "Arial")) +
  ggtitle("b) ASL") +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA) 

p2 = ggplot()+
  geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
  geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
  geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
  geom_sf(data = c_crossings_point, colour = "red", size = 0.1) +
  theme_bw() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) + 
  ggtitle("c) Cycle crossings") +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA) 

p3 = ggplot()+
  geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
  geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
  geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
  geom_sf(data = c_signals_point, colour = "red", size = 0.1) +
  theme_bw() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
        text = element_text(family = "Arial")) + 
  ggtitle("d) Cycle signals") +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA) 

p4 = ggplot()+
  geom_sf(data=motorways, fill="#EEEEEE",  colour="#EEEEEE")+
  geom_sf(data=borough_areas, fill="#d4d4d4",  colour="#444444", alpha=0.3, size=0.05)+
  geom_sf(data=riverthames_simplify, fill="#99CCEE",  colour="#99CCEE") +
  geom_sf(data = c_trafficcalming, colour = "red", size = 0.1) +
  theme_bw() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
        text = element_text(family = "Arial")) +
  ggtitle("e) Traffic calming measures") +
  coord_sf(crs=st_crs(riverthames_simplify), datum=NA) 

p5 = ggplot()+
  geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
  geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
  geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
  geom_sf(data = c_cyclelanetrack, colour = "red") +
  theme_bw() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        text = element_text(family = "Arial")) + 
  ggtitle("f) Cycle lanes and tracks") +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA) 


# Use patchwork to create plot
locations_plot = (p1 | p2 | p3) / (p4 | p5)

test = (pm | p1|p2) / (p3 | p4 | p5)

test = gridExtra::grid.arrange(basemap, p1, p2, p3, p4, p5, ncol = 3)

pl = replicate(6, ggplot(), FALSE)
grid.arrange(grobs = pl)  # default settings

library(cowplot)
plot_grid(p0, p1, p2, p3, p4, p5, ncol = 3)

#### CREATE NEW EAXMPLE WITH TRANSPARENCY _ below code alters transpoarent
#colour = alpha("red", 0.2)  Transparency changed to 0.2
t1 = ggplot()+
  geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
  geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
  geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
  geom_sf(data = c_asl_point, colour = alpha("red", 0.2), size = 0.1) +
  theme_bw() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
        text = element_text(family = "Arial")) +
  ggtitle("b) ASL") +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA) 

t2 = ggplot()+
  geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
  geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
  geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
  geom_sf(data = c_crossings_point, colour = alpha("red", 0.2), size = 0.1) +
  theme_bw() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
        text = element_text(family = "Arial")) + 
  ggtitle("c) Cycle crossings") +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA) 

t3 = ggplot()+
  geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
  geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
  geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
  geom_sf(data = c_signals_point, colour = alpha("red", 0.2), size = 0.1) +
  theme_bw() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        text = element_text(family = "Arial")) + 
  ggtitle("d) Cycle signals") +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA) 

t4 = ggplot()+
  geom_sf(data=motorways, fill="#EEEEEE",  colour="#EEEEEE")+
  geom_sf(data=borough_areas, fill="#d4d4d4",  colour="#444444", alpha=0.3, size=0.05)+
  geom_sf(data=riverthames_simplify, fill="#99CCEE",  colour="#99CCEE") +
  geom_sf(data = c_trafficcalming, colour = alpha("red", 0.2), size = 0.1) +
  theme_bw() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
        text = element_text(family = "Arial")) +
  ggtitle("e) Traffic calming measures") +
  coord_sf(crs=st_crs(riverthames_simplify), datum=NA) 

t5 = ggplot()+
  geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
  geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
  geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
  geom_sf(data = c_cyclelanetrack, colour = alpha("red", 0.2)) +
  theme_bw() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
        text = element_text(family = "Arial")) + 
  ggtitle("f) Cycle lanes and tracks") +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA) 

plot_grid(p1, t1, ncol = 2) # seems ok for ASL
plot_grid(p2, t2, ncol = 2) # seems ok for crossings
plot_grid(p3, t3, ncol = 2) #? needs to be less transparant
plot_grid(p4, t4, ncol = 2) # could go more transparant
plot_grid(p5, t5, u5, ncol = 3)
  
q1 = ggplot()+
  geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
  geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
  geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
  geom_sf(data = c_asl_point, colour = alpha("red", 0.5), size = 0.1) +
  theme_bw() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
        text = element_text(family = "Arial")) +
  ggtitle("b) ASL") +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA) 

q2 = ggplot()+
  geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
  geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
  geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
  geom_sf(data = c_crossings_point, colour = alpha("red", 0.5), size = 0.1) +
  theme_bw() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
        text = element_text(family = "Arial")) + 
  ggtitle("c) Cycle crossings") +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA) 

q3 = ggplot()+
  geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
  geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
  geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
  geom_sf(data = c_signals_point, colour = alpha("red", 0.5), size = 0.1) +
  theme_bw() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
        text = element_text(family = "Arial")) + 
  ggtitle("d) Cycle signals") +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA) 

q4 = ggplot()+
  geom_sf(data=motorways, fill="#EEEEEE",  colour="#EEEEEE")+
  geom_sf(data=borough_areas, fill="#d4d4d4",  colour="#444444", alpha=0.3, size=0.05)+
  geom_sf(data=riverthames_simplify, fill="#99CCEE",  colour="#99CCEE") +
  geom_sf(data = c_trafficcalming, colour = alpha("red", 0.05), size = 0.1) +
  theme_bw() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
        text = element_text(family = "Arial")) +
  ggtitle("e) Traffic calming measures") +
  coord_sf(crs=st_crs(riverthames_simplify), datum=NA) 

q5 = ggplot()+
  geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
  geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
  geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
  geom_sf(data = c_cyclelanetrack, colour = alpha("red", 0.2)) +
  theme_bw() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
        text = element_text(family = "Arial")) + 
  ggtitle("f) Cycle lanes and tracks") +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA) 
  
plot_grid(p1, t1, q1, ncol = 3) # t1 seems good
plot_grid(p2, t2, q2, ncol = 3) # t2 seems ok for crossings
plot_grid(p3, t3, q3, ncol = 3) # t3
plot_grid(p4, t4, q4, ncol = 3) # ideally more transparent than t4
plot_grid(p5, t5, q5, ncol = 3) # q5 but chekc how it looks with the others
  
# traffic calming 0.05, everything else 0.2


original_trans = gridExtra::grid.arrange(p0, p1, p2, p3, p4, p5, ncol = 3)
proposed_trans = gridExtra::grid.arrange(p0, t1, t2, t3, q4, q5, ncol = 3)








  
  
  
  
# Save image
## GGSAVE NOT WORKING HAD SIMILAT ISSUE WHEN DOING INTERNSHIP
##ggsave("/home/bananafan/Documents/PhD/Paper1/output/locations_map_plot.pdf", plot = locations_plot, 
##       dpi = 1000, width = 190 * (14/5), height = 142 * (14/5), units = "mm")



# figuring out bounding box issue
st_bbox


# geom_sf(data = out_lon_union, fill="white", colour = "black") +
#   geom_sf(data=motorways, colour = "#b77107", size = 0.14)+
#   geom_sf(data = inn_lon_union, colour = "#991100") +
#   geom_sf(data=borough_areas, fill="#d4d4d4",  colour="black", alpha=0.3, size=0.15)+
#   geom_sf(data=riverthames_simplify, fill="#99CCEE",  colour="#99CCEE")+
#   geom_sf_label(data = borough_areas, aes(label = B_numbered)) +  
# 
#   geom_sf(data=motorways, fill="#EEEEEE",  colour="#EEEEEE")+
#   geom_sf(data=borough_areas, fill="#d4d4d4",  colour="#444444", alpha=0.3, size=0.05)+
#   geom_sf(data=riverthames_simplify, fill="#99CCEE",  colour="#99CCEE") +
#   geom_sf(data = c_trafficcalming, colour = "red", size = 0.1) + 

st_bbox(motorways)  
# xmin     ymin     xmax     ymax
# 504043.6 157140.7 557784.9 200403.1
st_bbox(borough_areas)
# xmin     ymin     xmax     ymax 
# 503568.2 155850.8 561957.5 200933.9 
st_bbox(riverthames_simplify)
# xmin     ymin     xmax     ymax 
# 515256.1 167107.4 556887.1 182214.0 
st_bbox(inn_lon_union)
# xmin     ymin     xmax     ymax 
# 521054.9 169648.0 547612.0 188327.4 
st_bbox(out_lon_union)
# xmin     ymin     xmax     ymax 
# 503568.2 155850.8 561957.5 200933.9 

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



# Legend and other map work
# borough_text = paste("1 City of London", "2 Tower Hamlets", "3 Hackney",
#                      "4 Islington", "5 Camden", "6 Westminster",
#                      "7 Kensington and Chelsea", "8 Hammersmith and Fulham",
#                      "9 Wandsworth", "10 Lambeth", "11 Southwark", 
#                      "12 Lewisham", "13 Greenwich", "14 Newham",
#                      "15 Waltham Forest", "16 Haringey", "17 Enfield", 
#                      "18 Barnet", "19 Brent", "20 Ealing", "21 Harrow", 
#                      "22 Hillingdon", "23 Hounslow", "24 Richmond upon Thames",
#                      "25 Kingston upon Thames", "26 Merton", "27 Sutton", 
#                      "28 Croydon", "29 Bromley", "30 Bexley", "31 Havering", 
#                      "32 Barking and Dagenham", " 33 Redbridge", sep = "\n")



# df = data.frame(ma = rep(c("Motorway network", "Outer London Boundary", "Inner London Boundary", "River Thames"), each = 4),
#   x1 = rep(c(1,2), each =  8),
#   #x2 = rep(2, each = 8),
#   y1 = rep(seq(1:8), each = 2))
# 
# df2 = data.frame(ma = rep("Motorway network" , each = 4),
#                 x1 = c(1,2, 1,2),
#                 #x2 = rep(2, each = 8),
#                 y1 = c(1,1,2,2))
# 
# df3 = data.frame(ma = rep(c("Motorway network", "Outer London Boundary", "Inner London Boundary", "River Thames"), each = 1),
#                  x1 = rep(1, each = 1),
#                  x2 = rep(2, each = 1),
#                  y1 = c(1,2,3,4),
#                  y2 = c(1,2,3,4))
#                  
# 
# 
# 
# x2 = rep(2, each = 4),
#                  y1 = 1,
#                  y2 = 1)
# 
# 
# ggplot()+
#   geom_segment(data = df3, aes(x = x1, y = y1, xend = x2, yend = y2),
#                colour = c("#99CCEE", "#991100", "black", "#b77107"),
#                size = 4, 
#                labels = ma)
# 
# legend_names = (c("Motorway network", "Outer London Boundary", "Inner London Boundary", 
#                    "River Thames"))
# 
# 
# 
# legend_lines = ggplot(data = df2, aes(x = x1, y = y1, group = ma)) +
#   geom_point(alpha = 0) +
#   geom_hline(yintercept = 1, colour="#99CCEE", size = 2) +
#   geom_hline(yintercept = 2, colour = "#991100", size = 2) +
#   geom_hline(yintercept = 3, colour = "black", size = 2) +
#   geom_hline(yintercept = 4, color = "#b77107", size = 2) +
#   theme_void()


# cod



# # 4) Create map legend
# # create legend text
# legend_text =  data.frame(
#   lineend = c("River", "Inner London", "Outer London", "Motorway"),
#   linejoin = c("Thames", "Boundary", "Boundary", "Network"))
# 
# # create legend dataframe
# df = data.frame(legend_text, y = c(1, 1.5, 2, 2.5))
# 
# # create ggplot for map legend
# legend = ggplot(df, aes(x = 1, y = y, xend = 1.5, yend = y, label = paste(lineend, linejoin))) +
#   geom_segment(size = 3, colour = c("#99CCEE", "#991100", "black", "#b77107")) +
#   geom_text(hjust = 'outside', nudge_x = -0.25) +
#   xlim(0.5, 1.5) +
#   theme_void() +
#   theme(plot.margin = unit(c(0.25, 0.25, 0.25, -7.5), "cm"),
#         text = element_text(family = "Arial"))
# # fiddling with legend
# ggplot(df, aes(x = 1, y = y, xend = 1.5, yend = y, label = paste(lineend, linejoin))) +
#   geom_segment(size = 5, colour = c("#99CCEE", "#991100", "black", "#b77107")) +
#   geom_text(hjust = 'outside', nudge_x = - 0.25) +
#   xlim(0.61, 1.5) +
#   theme_void() +
#   theme(plot.margin = unit(c(-0.1, -0.2, 0, -0.60), "cm"),
#         text = element_text(family = "Arial"))
