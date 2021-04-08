#####################################################
# Visualising borough counts/lengths by chloropleth #
#
# Code development started on 7/4/21
# 
# This code generates chlorpleths with bar charts showing the count of assets by Borough
#

# Load packages
library(tidyverse)
library(mapview)
library(tmap)
library(cowplot)


# Load and manipulate datasets
# import May 2020 ONS LA boundary data clipped to coastline
lon_lad_2020_c2c = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFC.Rds")
lon_lad_2020_c2c <- rmapshaper::ms_simplify(lon_lad_2020_c2c, keep=0.015) #Simplify boroughs
lon_lad_2020_c2c$BOROUGH_short = fct_recode(lon_lad_2020_c2c$BOROUGH, 
                                      "Kens & Chel" = "Kensington & Chelsea",
                                      "Bark & Dage" = "Barking & Dagenham",
                                      "Hamm & Fulh" = "Hammersmith & Fulham",
                                      "Kingston" = "Kingston upon Thames",
                                      "Richmond" = "Richmond upon Thames",
                                      "City" = "City of London",
                                      "T Hamlets" = "Tower Hamlets",
                                      "W Forest" = "Waltham Forest") # rename Boroughs to reduce text length


# Import CID borough counts - these datasets were created 2_3_2021 from TFL datasets downloaded 25/2/21
CID_count = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/CID_count_by_borough")
CID_length = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/CID_length_by_borough")

# keep safety related assets
CID_count_safety = CID_count %>%
  select(c("BOROUGH", "ASL", "Crossings", "CycleLanesAndTracks", "Signals", "TrafficCalming"))
CID_length_safety = CID_length %>%
  select(c("BOROUGH", "CycleLaneTrack_m", "CycleLaneTrack_km"))

# Join datasets together
safety_borough_counts = left_join(CID_count_safety, CID_length_safety) 
safety_borough_counts = left_join(lon_lad_2020_c2c, safety_borough_counts)

#######
# ASL #
#######

# create chloropleth
asl_chloro = tm_shape(safety_borough_counts) +
  tm_polygons("ASL", title = "Count") + 
  tm_layout(title = "ASL",
            legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1,
            inner.margins = c(0.05,0.05,0.05,0.42), # creates wide right margin for barchart
            frame = FALSE) 

# # convert chloro to grob
asl_chloro = tmap_grob(asl_chloro) 

# Generate barchart
# Generate new column that divides ASL count into groups
safety_borough_counts <- safety_borough_counts %>%
  mutate(asl_group = cut(ASL,
                         breaks = seq(1, 351, by = 50),
                         labels = c("1 to 50", "51 to 100", "101 to 150", "151 to 200", 
                                    "201 to 250", "251 to 300", "301 to 350"),
                         right = FALSE)) # this means that 50 is included in 1 to 50

# Create vector of colours that match the chloropleth
my_colours = c("#ffffd4","#fee391", "#fec44f", "#fe9929", 
               "#ec7014", "#cc4c02", "#8c2d04")

# create Bar chart
asl_bar = ggplot(safety_borough_counts, aes(x = reorder(BOROUGH_short, -ASL), y = ASL, fill = asl_group)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +  # adds borders to bars
  coord_flip() +
  labs(y = "Count", x = NULL) +
  theme_classic() + 
  scale_y_continuous(limits = c(0, 350), expand = c(0,0)) +  # ensures axis starts at 0 so no gap
  scale_fill_manual(values = my_colours) +
  theme(axis.line.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none")

# Create cowplot of both plots
asl_chloro_bar = ggdraw() +
  draw_plot(asl_chloro) +
  draw_plot(asl_bar,
            width = 0.3, height = 0.6,
            x = 0.57, y = 0.19) 

# Save plots
aspect_ratio = 1.6

ggsave("/home/bananafan/Documents/PhD/Paper1/output/asl_chloro_bar.png", plot = asl_chloro_bar,
       height = 170, width = 170 * aspect_ratio, unit = "mm")



#Colour schemes
# 
# use palette_explorer() in tmap
# tm_polygons(..., palette = "YlOrBr", n = 7, ...)
# 
# # and then this webpage to get the correct colour codes
# https://loading.io/color/feature/
# $palette-name: 'YlOrBr / 7'
# $palette-color1: #ffffd4;
#   $palette-color2: #fee391;
#   $palette-color3: #fec44f;
#   $palette-color4: #fe9929;
#   $palette-color5: #ec7014;
#   $palette-color6: #cc4c02;
#   $palette-color7: #8c2d04;



#Initial code - probably can be deleted

# # create ggplot - use geom_col
# asl_bar1 = ggplot(safety_borough_counts) +
#   geom_col(aes(reorder(BOROUGH_short, -ASL), y = ASL)) +
#   coord_flip() +
#   labs(y = "Count", x = NULL) +
#   theme_classic() + 
#   scale_y_continuous(limits = c(0, 350), expand = c(0,0)) +
#   theme(axis.line.y = element_blank(), 
#         axis.ticks.y = element_blank(),
#         axis.line.x = element_blank())
# 
# # convert chloro to grob
# asl_chloro = tmap_grob(asl_chloro) 
# 
# # combine chlorpleth and bar chart using cowplot
# asl1 = ggdraw() +
#   draw_plot(asl_chloro) +
#   draw_plot(asl_bar1,
#             width = 0.3, height = 0.6,
#             x = 0.58, y = 0.16) # position bar chart to the right of the chloropleth
# asl1

