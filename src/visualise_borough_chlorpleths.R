###################################################################
# Visualising borough counts/lengths by chloropleth and bar chart #
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
library(patchwork)
library(sf)
library(tmaptools) # for palette explorer 

################################
# Load and manipulate datasets #
################################

# 1) Local Authority spatial data

# import May 2020 ONS LA boundary data clipped to coastline (used so that River Thames appears)
lon_lad_2020_c2c = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFC.Rds")

# simply borough shapes
lon_lad_2020_c2c <- rmapshaper::ms_simplify(lon_lad_2020_c2c, keep=0.015) #Simplify boroughs
# lon_lad_2020_c2c$BOROUGH_short = fct_recode(lon_lad_2020_c2c$BOROUGH, 
#                                       "Kens & Chel" = "Kensington & Chelsea",
#                                       "Bark & Dage" = "Barking & Dagenham",
#                                       "Hamm & Fulh" = "Hammersmith & Fulham",
#                                       "Kingston" = "Kingston upon Thames",
#                                       "Richmond" = "Richmond upon Thames",
#                                       "City" = "City of London",
#                                       "T Hamlets" = "Tower Hamlets",
#                                       "W Forest" = "Waltham Forest") # rename Boroughs to reduce text length

# Create new variable that labels the Boroughs by number (matches the overall map)
lon_lad_2020_c2c$Borough_number = fct_recode(lon_lad_2020_c2c$BOROUGH, 
                                      "7" = "Kensington & Chelsea",
                                      "32" = "Barking & Dagenham",
                                      "8" = "Hammersmith & Fulham",
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

# Convert borough area into km^2 from m^2 
lon_lad_2020_c2c$Shape__Are = units::set_units(lon_lad_2020_c2c$Shape__Are, m^2)
lon_lad_2020_c2c = lon_lad_2020_c2c %>%
  mutate(Borough_Area_km2 = (units::set_units(lon_lad_2020_c2c$Shape__Are, km^2)))# change area units to km^2 from m^2


# Select variables of interest
lon_lad_2020_c2c_reduced = lon_lad_2020_c2c %>%
  select(c("BOROUGH", "Borough_number", "Borough_Area_km2", "geometry"))


# 2) CID data

# Import CID borough counts - these datasets were created 2_3_2021 from TFL datasets downloaded 25/2/21
CID_count = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/CID_count_by_borough")
CID_length = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/CID_length_by_borough")


# Create new column (SignalsNA) where 0 are changed to NA - Signals is the only dataset where some Boroughs have 0 assets
CID_count$SignalsNA = CID_count$Signals
CID_count$SignalsNA[CID_count$SignalsNA == 0] = NA

# keep safety related assets
CID_count_safety = CID_count %>%
  select(c("BOROUGH", "ASL", "Crossings", "Signals", "SignalsNA", "TrafficCalming"))
CID_length_safety = CID_length %>%
  select(c("BOROUGH", "CycleLaneTrack_km"))



# 3) Population estimates
# ONS Mid year population estimates 2013-2019 
download.file("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2020localauthoritydistrictcodes/ukmidyearestimates20192020ladcodes.xls",
              "/home/bananafan/Downloads/ONS_pop_estimates")

ons_pop_estimates = readxl::read_excel("/home/bananafan/Downloads/ONS_pop_estimates", 
                                       sheet = "MYE 5", skip = 3) # skip top few excel rows that arent relevent
lon_pop_estimates_2019 = ons_pop_estimates %>%
  filter(Geography1 == "London Borough") %>% # select London Boroughs
  select(c("Name", "Estimated Population mid-2019")) %>% # keep 2019 data only
  rename("BOROUGH" = "Name") %>% # rename to match CID
  rename("Population" = "Estimated Population mid-2019") %>%
  mutate(Population_100000 = Population / 100000)

# rename values to match those names used in CID
lon_pop_estimates_2019$BOROUGH[lon_pop_estimates_2019$BOROUGH == "Kensington and Chelsea"] <- "Kensington & Chelsea"
lon_pop_estimates_2019$BOROUGH[lon_pop_estimates_2019$BOROUGH == "Barking and Dagenham"] <- "Barking & Dagenham"
lon_pop_estimates_2019$BOROUGH[lon_pop_estimates_2019$BOROUGH == "Hammersmith and Fulham"] <- "Hammersmith & Fulham"

# 4) PCT data
# The code for obtaining this data is in: get_pct_km_cycled.R file
pct_borough_commuting = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/Borough_commuting.rds") %>%
  mutate(total_100000km_cycled_for_commuting_per_year_estimated = total_km_cycled_for_commuting_per_year_estimated / 100000) 


# 5) STATS19 data 
# ? 3 years beofre CID?
  



# Join datasets together
CID_counts = left_join(CID_count_safety, CID_length_safety) 
denominators = left_join(lon_lad_2020_c2c_reduced, lon_pop_estimates_2019) %>%
  left_join(pct_borough_commuting)

chloropleth_dataset = left_join(denominators, CID_counts)


#??? CONSIDER DROPPING KM2 units here rather that at each chloropleth - ?will need to do same for cycled distance
# need to drop km2 for the barcharts

# produce counts by area and per head population (NB may want to consider per 100,000 pop at some point)
chloropleth_dataset <- chloropleth_dataset %>%
  mutate(across(.cols = c("ASL", "Crossings", "CycleLaneTrack_km", "Signals", "SignalsNA", "TrafficCalming"),
                .fns = ~.x/Borough_Area_km2,
                .names = "{.col}_by_area")) %>%
  mutate(across(.cols = c("ASL", "Crossings", "CycleLaneTrack_km", "Signals", "SignalsNA", "TrafficCalming"),
                .fns = ~.x/Population_100000,
                .names = "{.col}_per_100000pop")) %>%
  mutate(across(.cols = c("ASL", "Crossings", "CycleLaneTrack_km", "Signals","SignalsNA", "TrafficCalming"),
                .fns = ~.x/total_100000km_cycled_for_commuting_per_year_estimated,
                .names = "{.col}_per_100000km_cycled"))







###############################################################################
#                                   RAW COUNTS                                # 
###############################################################################


#######
# ASL #
#######


# create chloropleth
asl_raw_chloro = tm_shape(chloropleth_dataset) +
  tm_polygons("ASL", title = "Count") + 
  tm_layout(title = "ASL",
            legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1,
            inner.margins = c(0.1,0.1,0.1,0.42), # creates wide right margin for barchart
            frame = FALSE) 

# # convert chloro to grob
asl_raw_chloro = tmap_grob(asl_raw_chloro) 

# Generate barchart
# Generate new column that divides ASL count into groups
chloropleth_dataset <- chloropleth_dataset %>%
  mutate(asl_group = cut(ASL,
                         breaks = seq(1, 351, by = 50),
                         labels = c("1 to 50", "51 to 100", "101 to 150", "151 to 200", 
                                    "201 to 250", "251 to 300", "301 to 350"),
                         right = FALSE)) # this means that 50 is included in 1 to 50

# Create vector of colours that match the chloropleth
asl_raw_colours = c("#ffffd4","#fee391", "#fec44f", "#fe9929", 
               "#ec7014", "#cc4c02", "#8c2d04")

# create Bar chart
asl_raw_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -ASL), y = ASL, fill = asl_group)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +  # adds borders to bars
  coord_flip() +
  labs(y = "Count", x = NULL) +
  theme_classic() + 
  scale_y_continuous(limits = c(0, 350), expand = c(0,0)) +  # ensures axis starts at 0 so no gap
  scale_fill_manual(values = asl_raw_colours) +
  theme(axis.line.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none")

# Create cowplot of both plots
asl_raw_chloro_bar = ggdraw() +
  draw_plot(asl_raw_chloro) +
  draw_plot(asl_raw_bar,
            width = 0.3, height = 0.6,
            x = 0.57, y = 0.19) 


#############
# Crossings #
#############

# create chloropleth
crossings_raw_chloro = tm_shape(chloropleth_dataset) +
  tm_polygons("Crossings", title = "Count") + 
  tm_layout(title = "Crossings",
            legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1,
            inner.margins = c(0.1,0.1,0.1,0.42), # creates wide right margin for barchart
            frame = FALSE) 

# # # convert chloro to grob
crossings_raw_chloro = tmap_grob(crossings_raw_chloro) 
 
# Generate barchart
# Generate new column that divides Crossing count into groups
chloropleth_dataset <- chloropleth_dataset %>%
  mutate(crossings_group = cut(Crossings,
                               breaks = seq(1, 141, by = 20),
                               labels = c("1 to 20", "21 to 40", "41 to 60", "61 to 80", 
                                     "81 to 100", "101 to 120", "121 to 140"), 
                               right = FALSE)) # this means that 20 is included in 1 to 20
 
# # Create vector of colours that match the chloropleth - only 6 colours as 6th category had no values
crossings_raw_colours = c("#ffffd4","#fee391", "#fec44f", "#fe9929", "#ec7014", "#8c2d04")

# create Bar chart
crossings_raw_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -Crossings), y = Crossings, 
                                                  fill = crossings_group)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +  # adds borders to bars
  coord_flip() +
  labs(y = "Count", x = NULL) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 140), expand = c(0,0), 
                     breaks = c(0, 40, 80, 120)) +  # ensures axis starts at 0 so no gap
  scale_fill_manual(values = crossings_raw_colours) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none")

# # Create cowplot of both plots
crossings_raw_chloro_bar = ggdraw() +
   draw_plot(crossings_raw_chloro) +
   draw_plot(crossings_raw_bar,
             width = 0.3, height = 0.6,
             x = 0.57, y = 0.19) 


###########
# Signals #
###########


# # create chloropleth - use SignalsNA as the polygon as this then colours the NAs grey (missing)
signals_raw_chloro = tm_shape(chloropleth_dataset) +
  tm_polygons("SignalsNA", legend.show = FALSE) +
  tm_layout(title = "Signals",
            legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1,
            inner.margins = c(0.1,0.1,0.1,0.42), # creates wide right margin for barchart
            frame = FALSE) +
  tm_add_legend(type = "fill", 
    labels = c("0", "1 to 20", "21 to 40", "41 to 60", "61 to 80", "81 to 100"), # enables relabelling of 'Missing/NA' as 0
    col = c("grey", "#ffffd4", "#fed98e", "#fe9929", "#d95f0e", "#993404"),
    border.lwd = 0.5,
    title = "Count")

# convert chloro to grob
signals_raw_chloro = tmap_grob(signals_raw_chloro)


# Generate barchart
# Generate new column that divides signal count into groups

chloropleth_dataset <- chloropleth_dataset %>%
  mutate(signals_raw_group = cut(SignalsNA,
                             breaks = seq(1, 101, by = 20),
                             labels = c("1 to 20", "21 to 40", "41 to 60", "61 to 80",
                                        "81 to 100"),
                             right = FALSE))


# # Create vector of colours that match the chloropleth
signals_raw_colours = c("#ffffd4", "#fed98e", "#fe9929", "#993404")

# # create Bar chart
signals_raw_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -Signals), 
                                                  y = SignalsNA, # this means those that are missing are shown with no bars
                                                  fill = signals_raw_group)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  labs(y = "Count", x = NULL) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 100), expand = c(0,0), 
                     breaks = c(0, 20, 40, 60, 80)) +  # ensures axis starts at 0 so no gap
  scale_fill_manual(values = signals_raw_colours) +
  theme(axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none")
#axis.line.y = element_line(size = 0.2),

# # Create cowplot of both plots
signals_raw_chloro_bar = ggdraw() +
  draw_plot(signals_raw_chloro) +
  draw_plot(signals_raw_bar,
            width = 0.3, height = 0.6,
            x = 0.57, y = 0.19)





###################
# Traffic calming #   THIS ONE WILL NEED CHANGING AS IT DOESNT WORK QUITE RIGHT ? worth changing bbox for all chloropleths? 
###################

# # make some bbox magic
# bbox_new <- st_bbox(safety_borough_counts) # current bounding box
# 
# xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
# yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
# 
# bbox_new[1] <- bbox_new[1] - (0.5 * xrange) # xmin - left
# bbox_new[3] <- bbox_new[3] + (0.2 * xrange) # xmax - right
# #bbox_new[2] <- bbox_new[2] - (0.2 * yrange) # ymin - bottom
# bbox_new[4] <- bbox_new[4] + (0.25 * yrange) # ymax - top
# 
# bbox_new <- bbox_new %>%  # take the bounding box ...
#   st_as_sfc() # ... and make it a sf polygon

#and will need this in the chloropleth code if use bbox
#tc_raw_chloro = tm_shape(chloropleth_dataset, bbox = bbox_new) +


# create chloropleth
tc_raw_chloro = tm_shape(chloropleth_dataset) +
  tm_polygons("TrafficCalming", title = "Count") +
  tm_layout(title = "Traffic calming",
            legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1,
            inner.margins = c(0.1,0.1,0.1,0.42), # creates wide right margin for barchart
            frame = FALSE)
 
# # convert chloro to grob
tc_raw_chloro = tmap_grob(tc_raw_chloro) 

# Generate barchart
# Generate new column that divides traffic calming count into groups
chloropleth_dataset <- chloropleth_dataset %>%
  mutate(traffic_calming_group = cut(TrafficCalming,
                         breaks = seq(1, 4001, by = 500),
                         labels = c("1 to 500", "501 to 1000", "1001 to 1500", "1501 to 2000",
                                    "2001 to 2500", "2501 to 3000", "3001 to 3500", "3501 to 4000"),
                         right = FALSE))

# Create vector of colours that match the chloropleth
tc_raw_colours = c("#ffffe5","#fff7bc", "#fee391", "#fec44f", "#fe9929", 
                               "#ec7014", "#cc4c02", "#8c2d04")
 
# create Bar chart
tc_raw_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -TrafficCalming), y = TrafficCalming, fill = traffic_calming_group)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) + # adds borders to bars
  coord_flip() +
  labs(y = "Count", x = NULL) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 4000), expand = c(0,0), 
                     breaks = c(0, 1000, 2000, 3000)) +  # ensures axis starts at 0 so no gap
  scale_fill_manual(values = tc_raw_colours) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none")

# Create cowplot of both plots
tc_raw_chloro_bar = ggdraw() +
  draw_plot(tc_raw_chloro) +
  draw_plot(tc_raw_bar,
            width = 0.3, height = 0.6,
            x = 0.57, y = 0.19)



###################################
# Cycle lanes and Tracks - LENGTH #
###################################

# Commented code may be deleted if subsequent code works best
# Convert clt length to integer (and rounded) so that intervals are plotted ok
#chloropleth_dataset$CLT_km_integer = as.integer(round(units::drop_units(chloropleth_dataset$CycleLaneTrack_km))) 
# converts the 140.5 into 140
# # create chloropleth
# clt_raw_chloro = tm_shape(chloropleth_dataset) +
#   tm_polygons("CLT_km_integer", title = "Total length (km)") +
#   tm_layout(title = "Cycle lanes and tracks",
#             legend.title.size = 1,
#             legend.text.size = 0.7,
#             legend.position = c("left","bottom"),
#             legend.bg.alpha = 1,
#             inner.margins = c(0.1,0.1,0.1,0.42), # creates wide right margin for barchart
#             frame = FALSE)

# create chloropleth (if just used length as opposed to integer)
clt_raw_chloro = tm_shape(chloropleth_dataset) +
  tm_polygons("CycleLaneTrack_km", title = "Total length (km)", 
              legend.format = list(text.separator = "<")) +
  tm_layout(title = "Cycle lanes and tracks",
            legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1,
            inner.margins = c(0.1,0.1,0.1,0.42), # creates wide right margin for barchart
            frame = FALSE)

# # # convert chloro to grob
clt_raw_chloro = tmap_grob(clt_raw_chloro) 

# Generate barchart

# create new column where units (km^2) are removed (need units to be removed to draw bar chart)
chloropleth_dataset$clt_raw_numeric = round(units::drop_units(chloropleth_dataset$CycleLaneTrack_km), digits = 2)

chloropleth_dataset <- chloropleth_dataset %>%
  mutate(clt_raw_group = cut(clt_raw_numeric,
                             breaks = seq(20, 160, by = 20),
                             labels = c("20 < 40", "41 < 60", "60 < 80", "80 < 100",
                                        "100 < 120", "120 < 140", "140 < 160"),
                             right = FALSE)) # this means that 140.5 is included in 140 < 150

# # Create vector of colours that match the chloropleth
clt_raw_colours = c("#ffffd4","#fee391", "#fec44f", "#fe9929", "#ec7014", "#cc4c02", "#8c2d04")

# create Bar chart
clt_raw_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -clt_raw_numeric), 
                                  y = clt_raw_numeric, fill = clt_raw_group)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +  # adds borders to bars
  coord_flip() +
  labs(y = "Total length in km", x = NULL) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 150), expand = c(0,0)) +  # ensures axis starts at 0 so no gap
  scale_fill_manual(values = clt_raw_colours) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none")

# Create cowplot of both plots
clt_raw_chloro_bar = ggdraw() +
  draw_plot(clt_raw_chloro) +
  draw_plot(clt_raw_bar,
            width = 0.3, height = 0.6,
            x = 0.57, y = 0.19)





###############################################################################
#                         Standardised to Borough Area                        # 
###############################################################################


#######
# ASL #
#######

# Pull out City data as not comparable to other Boroughs (42 versus less than 14 for the rest)
city_chloropleth_dataset = chloropleth_dataset %>%
  filter(BOROUGH == "City of London")

# # create chloropleth
asl_area_chloro = tm_shape(chloropleth_dataset) +
  tm_polygons("ASL_by_area", title = "Count per km^2", palette = "Blues",
              breaks = c(0, 2, 4, 6, 8, 10, 12, 14),
              legend.format = list(text.separator = "<")) +
  tm_shape(city_chloropleth_dataset) +
  tm_polygons("ASL_by_area", title = "", palette = "inferno", breaks = c(42, 44),
              legend.format = list(text.separator = "<")) +
  tm_layout(title = "ASL",
            legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1,
            inner.margins = c(0.1,0.1,0.1,0.42), # creates wide right margin for barchart
            frame = FALSE) 

# convert chloro to grob
asl_area_chloro = tmap_grob(asl_area_chloro) 
 
##  Generate barchart
# create new column where units (km^2) are removed (need units to be removed to draw bar chart)
chloropleth_dataset$ASL_by_area_numeric = round(units::drop_units(chloropleth_dataset$ASL_by_area), digits = 2) 

# Generate new column that divides ASL count into groups
chloropleth_dataset <- chloropleth_dataset %>%
   mutate(asl_area_group = cut(ASL_by_area_numeric,
                         breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 43),
                         labels = c("0 < 2", "2 < 4", "4 < 6", "6 < 8",
                                    "8 < 10", "10 < 12", "12 < 14", "42 < 44"),
                         right = FALSE)) 

# create variable that identifies Boroughs that need to be in facet based on a value (in this example City)
#chloropleth_dataset$asl_area_facet <- (chloropleth_dataset$ASL_by_area_numeric >= 20.00)

# # Create vector of colours that match the chloropleth
asl_area_colours = c("#eff3ff","#c6dbef", "#9ecae1", "#6baed6", 
                     "#4292c6", "#2171b5", "#084594", "black")

asl_area_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -ASL_by_area_numeric), y = ASL_by_area_numeric, fill = asl_area_group)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +  # adds borders to bars )
  #facet_grid(.~ asl_area_facet, scales ="free", space="free") +
  coord_flip() +
  #theme(strip.text.x = element_blank()) # get rid of the facet labels +
  labs(y = "Count", x = NULL) +
  theme_classic() +
  scale_y_continuous(expand = c(0,0)) +  # ensures axis starts at 0 so no gap
  scale_fill_manual(values = asl_area_colours) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none", 
        strip.text.x = element_blank())

# # Create cowplot of both plots
asl_area_chloro_bar = ggdraw() +
   draw_plot(asl_area_chloro) +
   draw_plot(asl_area_bar,
             width = 0.3, height = 0.6,
             x = 0.57, y = 0.19) 
 

#############
# Crossings #
#############

# create chloropleth
crossings_area_chloro = tm_shape(chloropleth_dataset) +
  tm_polygons("Crossings_by_area", title = "Count per km^2", palette = "Blues", 
              legend.format = list(text.separator = "<")) +
  tm_layout(title = "Crossings",
            legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1,
            inner.margins = c(0.1,0.1,0.1,0.42), # creates wide right margin for barchart
            frame = FALSE)

# convert chloro to grob
crossings_area_chloro = tmap_grob(crossings_area_chloro) 

# Generate barchart
# Drop units and round so that intervals are plotted ok
chloropleth_dataset$crossings_by_area_numeric = round(units::drop_units(chloropleth_dataset$Crossings_by_area), digits = 2)

# Generate new column that divides Crossing count into groups
chloropleth_dataset <- chloropleth_dataset %>%
  mutate(crossings_area_group = cut(crossings_by_area_numeric,
                               breaks = seq(0, 5, by = 1),
                               labels = c("0 < 1", "1 < 2", "2 < 3", "3 < 4", "4 < 5"),
                               right = FALSE))

# # Create vector of colours that match the chloropleth 
crossings_area_colours = c("#eff3ff","#bdd7e7", "#6baed6", "#3182bd", "#08519c")

# create Bar chart
crossings_area_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -crossings_by_area_numeric), y = crossings_by_area_numeric,
                                                    fill = crossings_area_group)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +  # adds borders to bars
  coord_flip() +
  labs(y = "Count", x = NULL) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 5), expand = c(0,0)) +
  scale_fill_manual(values = crossings_area_colours) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none")

# # # Create cowplot of both plots
crossings_area_chloro_bar = ggdraw() +
   draw_plot(crossings_area_chloro) +
   draw_plot(crossings_area_bar,
             width = 0.3, height = 0.6,
             x = 0.57, y = 0.19) 
# 
# 
# ###########
# # Signals #
# ###########

# Pull out City data as not comparable to other Boroughs (20 versus less than 5 for the rest)
city_chloropleth_dataset = chloropleth_dataset %>%
  filter(BOROUGH == "City of London")

# # create chloropleth - 
signals_area_chloro = tm_shape(chloropleth_dataset) +
  tm_polygons("SignalsNA_by_area", palette = "Blues", 
              breaks = c(0, 1, 2, 3, 4, 5),
              legend.show = FALSE) +  
  tm_shape(city_chloropleth_dataset) +
  tm_polygons("SignalsNA_by_area", title = "", palette = "inferno", breaks = c(20, 21),
              legend.show = FALSE) +
  tm_layout(title = "Signals",
            legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1,
            inner.margins = c(0.1,0.1,0.1,0.42), # creates wide right margin for barchart
            frame = FALSE) +
  tm_add_legend(type = "fill",
                labels = c("0", "> 0 < 1", "1 < 2", "2 < 3", "3 < 4", "4 < 5", "20 < 21"),
                col = c("grey", "#eff3ff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c", "black"),
                border.lwd = 0.5,
                title = "Count per km^2")

# # convert chloro to grob
signals_area_chloro = tmap_grob(signals_area_chloro)
 

# # Generate barchart
# Drop units and round so that intervals are plotted ok
chloropleth_dataset$SignalsNA_by_area_numeric = round(units::drop_units(chloropleth_dataset$SignalsNA_by_area), digits = 2)

# # Generate new column that divides signal count into groups
chloropleth_dataset <- chloropleth_dataset %>%
  mutate(signals_area_group = cut(SignalsNA_by_area_numeric,
                              breaks = c(0, 1, 2, 3, 4, 5, 21),
                              labels = c("> 0 < 1", "1 < 2", "2 < 3", "3 < 4", "4 < 5", "20 < 21"),
                              right = FALSE))

# # Create vector of colours that match the chloropleth 
signals_area_colours = c("#eff3ff", "#bdd7e7", "#6baed6", "#08519c", "black")


signals_area_bar = ggplot(chloropleth_dataset, 
                          aes(x = reorder(Borough_number, -SignalsNA_by_area_numeric), 
                              y = SignalsNA_by_area_numeric, fill = signals_area_group)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  labs(y = "Count by km^2", x = NULL) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 25), expand = c(0,0),
                     breaks = c(0, 5, 10, 15, 20)) +  # ensures axis starts at 0 so no gap
  scale_fill_manual(values = signals_area_colours) +
  theme(axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none")

# # Create cowplot of both plots
signals_area_chloro_bar = ggdraw() +
  draw_plot(signals_area_chloro) +
  draw_plot(signals_area_bar,
            width = 0.3, height = 0.6,
            x = 0.57, y = 0.19)


# ###################
# # Traffic calming #   THIS ONE WILL NEED CHANGING AS IT DOESNT WORK QUITE RIGHT ? worth changing bbox for all chloropleths? 
# ###################
# 
# # # make some bbox magic
# # bbox_new <- st_bbox(safety_borough_counts) # current bounding box
# # 
# # xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
# # yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
# # 
# # bbox_new[1] <- bbox_new[1] - (0.5 * xrange) # xmin - left
# # bbox_new[3] <- bbox_new[3] + (0.2 * xrange) # xmax - right
# # #bbox_new[2] <- bbox_new[2] - (0.2 * yrange) # ymin - bottom
# # bbox_new[4] <- bbox_new[4] + (0.25 * yrange) # ymax - top
# # 
# # bbox_new <- bbox_new %>%  # take the bounding box ...
# #   st_as_sfc() # ... and make it a sf polygon
# 
# #and will need this in the chloropleth code if use bbox
# #tc_raw_chloro = tm_shape(chloropleth_dataset, bbox = bbox_new) +
# 
# 
# create chloropleth
tc_area_chloro = tm_shape(chloropleth_dataset) +
  tm_polygons("TrafficCalming_by_area", title = "Count by km^2", palette = "Blues",
              breaks = (c(0, 40, 80, 120, 160)),
              legend.format = list(text.separator = "<")) +
  tm_layout(title = "Traffic calming",
            legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1,
            inner.margins = c(0.1,0.1,0.1,0.42), # creates wide right margin for barchart
            frame = FALSE)

# # convert chloro to grob
tc_area_chloro = tmap_grob(tc_area_chloro)

# # Generate barchart
# # Drop units and round so that intervals are plotted ok
chloropleth_dataset$TrafficCalming_by_area_numeric = round(units::drop_units(chloropleth_dataset$TrafficCalming_by_area), digits = 2)

# Generate new column that divides traffic calming count into groups
chloropleth_dataset <- chloropleth_dataset %>%
  mutate(tc_area_group = cut(TrafficCalming_by_area_numeric,
                             breaks = seq(0, 160, by = 40),
                             labels = c("> 0 < 40", "40 < 80", "80 < 120", "120 < 140"),
                             right = FALSE))

# # Create vector of colours that match the chloropleth
tc_area_colours = c("#eff3ff", "#bdd7e7", "#6baed6", "#2171b5")
# 
# # create Bar chart
tc_area_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -TrafficCalming_by_area_numeric), 
                                y = TrafficCalming_by_area_numeric, 
                                fill = tc_area_group)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) + # adds borders to bars
  coord_flip() +
  labs(y = "Count per km^2", x = NULL) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 160), expand = c(0,0),
                     breaks = c(0, 40, 80, 120, 160)) +  # ensures axis starts at 0 so no gap
  scale_fill_manual(values = tc_area_colours) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none")
 
# Create cowplot of both plots
tc_raw_chloro_bar = ggdraw() +
  draw_plot(tc_area_chloro) +
  draw_plot(tc_area_bar,
            width = 0.3, height = 0.6,
            x = 0.57, y = 0.19)

 
 

##########################################
# Cycle lanes and Tracks (length) - Area #
##########################################

# create chloropleth
clt_area_chloro = tm_shape(chloropleth_dataset) +
  tm_polygons("CycleLaneTrack_km_by_area", title = "Total length (km) by area (km^2)", 
              legend.format = list(text.separator = "<"),
              breaks = c(0, 1.5, 3, 4.5, 6, 7.5, 9),
              palette = "Blues") +
  tm_layout(title = "Cycle lanes and tracks",
            legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1,
            inner.margins = c(0.1,0.1,0.1,0.42), # creates wide right margin for barchart
            frame = FALSE)

# # # convert chloro to grob
clt_area_chloro = tmap_grob(clt_area_chloro) 

# Generate barchart

# create new column where units (km^2) are removed (need units to be removed to draw bar chart)
chloropleth_dataset$clt_area_numeric = round(units::drop_units(chloropleth_dataset$CycleLaneTrack_km_by_area), digits = 2)

chloropleth_dataset <- chloropleth_dataset %>%
  mutate(clt_area_group = cut(clt_area_numeric,
                             breaks = seq(0, 9, by = 1.5),
                             labels = c("> 0 < 1.5", "1.5 < 3.0", "3.0 < 4.5", "4.5 < 6",
                                        "6 < 7.5", "7.5 < 9"),
                             right = FALSE)) 

# # Create vector of colours that match the chloropleth
clt_area_colours = c("#eff3ff", "#c6dbef", "#9ecae1", "#6baed6", "#08519c")

# create Bar chart
clt_area_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -clt_area_numeric), 
                                              y = clt_area_numeric, fill = clt_area_group)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +  # adds borders to bars
  coord_flip() +
  labs(y = "Total length (km) by area (km^2)", x = NULL) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 9), expand = c(0,0)) +  # ensures axis starts at 0 so no gap
  scale_fill_manual(values = clt_area_colours) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none")

# Create cowplot of both plots
clt_area_chloro_bar = ggdraw() +
  draw_plot(clt_area_chloro) +
  draw_plot(clt_area_bar,
            width = 0.3, height = 0.6,
            x = 0.57, y = 0.19)





###############################################################################
#                         Standardised to Population  (greens)                        # 
###############################################################################


####################
# ASL - Population #
####################

# # create chloropleth
asl_pop_chloro = tm_shape(chloropleth_dataset) +
  tm_polygons("ASL_per_100000pop", title = "Count per 100,000 population", palette = "Greens",
              breaks = c(0, 20, 40, 60, 80, 100, 120),
              legend.format = list(text.separator = "<")) +
  tm_shape(city_chloropleth_dataset) + # city is an outlier
  tm_polygons("ASL_per_100000pop", title = "", palette = "inferno", breaks = c(1240, 1260),
              legend.format = list(text.separator = "<")) +
  tm_layout(title = "ASL",
            legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1,
            inner.margins = c(0.1,0.1,0.1,0.42), # creates wide right margin for barchart
            frame = FALSE) 

# # convert chloro to grob
asl_pop_chloro = tmap_grob(asl_pop_chloro) 
 
##  Generate barchart
#Generate new column that divides ASL count into groups
chloropleth_dataset <- chloropleth_dataset %>%
  mutate(asl_pop_group = cut(ASL_per_100000pop,
                              breaks = c(0, 20, 40, 60, 80, 100, 120, 1240, 1260),
                              labels = c("0 < 20", "20 < 40", "40 < 60", "60 < 80",
                                         "80 < 100", "100 < 120", "120 < 140", "1240 < 1260"),
                              right = FALSE))

# # # Create vector of colours that match the chloropleth
asl_pop_colours = c("#edf8e9", "#c7e9c0", "#a1d99b", "#74c476", "#31a354", "#006d2c", "black")
 
asl_pop_bar = ggplot(chloropleth_dataset, 
                      aes(x = reorder(Borough_number, -ASL_per_100000pop), 
                          y = ASL_per_100000pop, fill = asl_pop_group)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +  
  coord_flip() +
  labs(y = "Count", x = NULL) +
  theme_classic() +
  scale_y_continuous(expand = c(0,0)) +  # ensures axis starts at 0 so no gap
  scale_fill_manual(values = asl_pop_colours) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none",
        strip.text.x = element_blank())
# 
# # # Create cowplot of both plots
asl_pop_chloro_bar = ggdraw() +
  draw_plot(asl_pop_chloro) +
  draw_plot(asl_pop_bar,
            width = 0.3, height = 0.6,
            x = 0.57, y = 0.19)


##########################
# Crossings - Population #
##########################

# create chloropleth
crossings_pop_chloro = tm_shape(chloropleth_dataset) +
  tm_polygons("Crossings_per_100000pop", title = "Count per 100,000 population", palette = "Greens", 
              breaks = c(0, 10, 20, 30, 40),
              legend.format = list(text.separator = "<")) +
  tm_shape(city_chloropleth_dataset) +
  tm_polygons("Crossings_per_100000pop", title = "", palette = "inferno", breaks = c(140, 150),
              legend.format = list(text.separator = "<")) +
  tm_layout(title = "Crossings",
            legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1,
            inner.margins = c(0.1,0.1,0.1,0.42), # creates wide right margin for barchart
            frame = FALSE)

# # convert chloro to grob
crossings_pop_chloro = tmap_grob(crossings_pop_chloro) 
 
# # Generate barchart
# # Drop units and round so that intervals are plotted ok
# chloropleth_dataset$crossings_by_area_numeric = round(units::drop_units(chloropleth_dataset$Crossings_by_area), digits = 2)
# 
# Generate new column that divides Crossing count into groups
chloropleth_dataset <- chloropleth_dataset %>%
  mutate(crossings_pop_group = cut(Crossings_per_100000pop,
                                    breaks = c(0,10,20,30,40,140, 150),
                                    labels = c(">0 < 10", "10 < 20", "20 < 30", "30 < 40", "40 < 140", "140 < 150"),
                                    right = FALSE))
 
# # # Create vector of colours that match the chloropleth 
crossings_pop_colours = c("#edf8e9","#bae4b3", "#74c476", "#238b45", "black")

# create Bar chart
crossings_pop_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -Crossings_per_100000pop), 
                                                    y = Crossings_per_100000pop,
                                                    fill = crossings_pop_group)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +  # adds borders to bars
  coord_flip() +
  labs(y = "Count per 100,000 population", x = NULL) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 150), expand = c(0,0)) +
  scale_fill_manual(values = crossings_pop_colours) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none")

# # # Create cowplot of both plots
crossings_pop_chloro_bar = ggdraw() +
  draw_plot(crossings_pop_chloro) +
  draw_plot(crossings_pop_bar,
            width = 0.3, height = 0.6,
            x = 0.57, y = 0.19)



########################
# Signals - Population #
########################

# # create chloropleth - use SignalsNA as the polygon as this then colours the NAs grey (missing)
signals_pop_chloro = tm_shape(chloropleth_dataset) +
  tm_polygons("SignalsNA_per_100000pop", palette = "Greens", 
              breaks = c(0, 10, 20, 30, 40), legend.show = FALSE) +
  tm_shape(city_chloropleth_dataset) + # city is an outlier so add as separate shape to control colour
  tm_polygons("SignalsNA_per_100000pop", title = "", palette = "inferno", breaks = c(580, 600),
              legend.show = FALSE) +
  tm_layout(title = "Signals",
            legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1,
            inner.margins = c(0.1,0.1,0.1,0.42), # creates wide right margin for barchart
            frame = FALSE) +
  tm_add_legend(type = "fill", 
                labels = c("0", "> 0 < 10", "10 < 20", "20 < 30", "30 < 40", "580 < 600"), # enables relabelling of 'Missing/NA' as 0
                col = c("grey", "#edf8e9", "#bae4b3", "#74c476", "#238b45", "black"),
                border.lwd = 0.5,
                title = "Signals per 100,000 population")

# convert chloro to grob
signals_pop_chloro = tmap_grob(signals_pop_chloro)


# Generate barchart
# Generate new column that divides signal count into groups

chloropleth_dataset <- chloropleth_dataset %>%
  mutate(signals_pop_group = cut(SignalsNA_per_100000pop,
                                 breaks = c(0, 10, 20, 30, 40, 600),
                                 labels = c("> 0 < 10", "10 < 20", "20 < 30", "30 < 40", "580 < 600"),
                                 right = FALSE))


# # # Create vector of colours that match the chloropleth
signals_pop_colours = c("#edf8e9", "#bae4b3", "#238b45", "black")

# # # create Bar chart
signals_pop_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -SignalsNA_per_100000pop),
                                                  y = SignalsNA_per_100000pop, # this means those that are missing are shown with no bars
                                                  fill = signals_pop_group)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  labs(y = "Count", x = NULL) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 600), expand = c(0,0)) +  # ensures axis starts at 0 so no gap
  scale_fill_manual(values = signals_pop_colours) +
  theme(axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none")
#axis.line.y = element_line(size = 0.2),

# # Create cowplot of both plots
signals_pop_chloro_bar = ggdraw() +
  draw_plot(signals_pop_chloro) +
  draw_plot(signals_pop_bar,
            width = 0.3, height = 0.6,
            x = 0.57, y = 0.19)



################################################
# Cycle lanes and Tracks (length) - Population #
################################################

# create chloropleth
clt_pop_chloro = tm_shape(chloropleth_dataset) +
  tm_polygons("CycleLaneTrack_km_per_100000pop", title = "Total length (km) per 100,000 population", 
              legend.show = FALSE,
              breaks = c(10, 20, 30, 40, 50, 60, 70),
              palette = "Greens") +
  tm_shape(city_chloropleth_dataset) + # city is an outlier so add as separate shape to control colour
  tm_polygons("CycleLaneTrack_km_per_100000pop", title = "", palette = "inferno", breaks = c(220, 240),
              legend.show = FALSE) +
  tm_add_legend(type = "fill", 
                labels = c("10 < 20", "20 < 30", "30 < 40", "40 < 50", "50 < 60", "60 < 70", "220 < 240"), # enables relabelling of 'Missing/NA' as 0
                col = c("#edf8e9", "#c7e9c0", "#a1d99b", "#74c476", "#31a354", "#006d2c", "black"),
                border.lwd = 0.5,
                title = "Total length (km) per 100,000 population") +
  tm_layout(title = "Cycle lanes and tracks",
            legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1,
            inner.margins = c(0.1,0.1,0.1,0.42), # creates wide right margin for barchart
            frame = FALSE)

# # # convert chloro to grob
clt_pop_chloro = tmap_grob(clt_pop_chloro) 

# # Generate barchart

# create new column where units (km^2) are removed (need units to be removed to draw bar chart)
chloropleth_dataset$clt_pop_numeric = round(units::drop_units(chloropleth_dataset$CycleLaneTrack_km_per_100000pop), digits = 2)

chloropleth_dataset <- chloropleth_dataset %>%
  mutate(clt_pop_group = cut(clt_pop_numeric,
                              breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 220, 240),
                              labels = c("0 < 10", "10 < 20", "20 < 30", "30 < 40", "40 < 50",
                                         "50 < 60", "60 < 70", " 70 < 220", "220 < 240"),
                              right = FALSE))

# Create vector of colours that match the chloropleth
clt_pop_colours = c("#edf8e9", "#c7e9c0", "#a1d99b", "#74c476", "#31a354", "#006d2c", "black")

# create Bar chart
clt_pop_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -clt_pop_numeric),
                                               y = clt_pop_numeric, fill = clt_pop_group)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +  # adds borders to bars
  coord_flip() +
  labs(y = "Total length (km) per 100,000 population", x = NULL) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 240), expand = c(0,0)) +  # ensures axis starts at 0 so no gap
  scale_fill_manual(values = clt_pop_colours) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none")

# Create cowplot of both plots
clt_pop_chloro_bar = ggdraw() +
  draw_plot(clt_pop_chloro) +
  draw_plot(clt_pop_bar,
            width = 0.3, height = 0.6,
            x = 0.57, y = 0.19)



# #########################################
# # Traffic calming - Population (greens) # 
# #########################################

# # create chloropleth
# tc_area_chloro = tm_shape(chloropleth_dataset) +
#   tm_polygons("TrafficCalming_by_area", title = "Count by km^2", palette = "Greens",
#               breaks = (c(0, 40, 80, 120, 160)),
#               legend.format = list(text.separator = "<")) +
#   tm_layout(title = "Traffic calming",
#             legend.title.size = 1,
#             legend.text.size = 0.7,
#             legend.position = c("left","bottom"),
#             legend.bg.alpha = 1,
#             inner.margins = c(0.1,0.1,0.1,0.42), # creates wide right margin for barchart
#             frame = FALSE)
# 
# # # convert chloro to grob
# tc_area_chloro = tmap_grob(tc_area_chloro)
# 
# # # Generate barchart
# # # Drop units and round so that intervals are plotted ok
# chloropleth_dataset$TrafficCalming_by_area_numeric = round(units::drop_units(chloropleth_dataset$TrafficCalming_by_area), digits = 2)
# 
# # Generate new column that divides traffic calming count into groups
# chloropleth_dataset <- chloropleth_dataset %>%
#   mutate(tc_area_group = cut(TrafficCalming_by_area_numeric,
#                              breaks = seq(0, 160, by = 40),
#                              labels = c("> 0 < 40", "40 < 80", "80 < 120", "120 < 140"),
#                              right = FALSE))
# 
# # # Create vector of colours that match the chloropleth
# tc_area_colours = c("#eff3ff", "#bdd7e7", "#6baed6", "#2171b5")
# # 
# # # create Bar chart
# tc_area_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -TrafficCalming_by_area_numeric), 
#                                               y = TrafficCalming_by_area_numeric, 
#                                               fill = tc_area_group)) +
#   geom_bar(stat = "identity", color = "black", size = 0.1) + # adds borders to bars
#   coord_flip() +
#   labs(y = "Count per km^2", x = NULL) +
#   theme_classic() +
#   scale_y_continuous(limits = c(0, 160), expand = c(0,0),
#                      breaks = c(0, 40, 80, 120, 160)) +  # ensures axis starts at 0 so no gap
#   scale_fill_manual(values = tc_area_colours) +
#   theme(axis.line.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.line.x = element_blank(),
#         legend.position = "none")
# 
# # Create cowplot of both plots
# tc_raw_chloro_bar = ggdraw() +
#   draw_plot(tc_area_chloro) +
#   draw_plot(tc_area_bar,
#             width = 0.3, height = 0.6,
#             x = 0.57, y = 0.19)






















####################################################
# Use patchwork to create arrangement of all plots #
####################################################

# Use patchwork to create plot of all raw plots
raw_chloro_bar_plots = (asl_raw_chloro_bar | crossings_raw_chloro_bar) / 
  (signals_raw_chloro_bar | tc_raw_chloro_bar) /  
  clt_raw_chloro_bar



##################
# SAVE ALL PLOTS #
##################

ggsave("/home/bananafan/Documents/PhD/Paper1/output/all_chloro_bar_plots.pdf", plot = all_chloro_bar_plots, 
       dpi = 1000, width = 210 * (14/5), height = 190 * (14/5), units = "mm")

# # try cowplot to save
# cowplot = plot_grid(asl_chloro_bar, crossings_chloro_bar, signals_chloro_bar, traffic_calming_chloro_bar, clt_chloro_bar)
# save_plot("/home/bananafan/Documents/PhD/Paper1/output/all_chloro_bar_plots.jpeg", 
#           cowplot,
#           ncol = 1, nrow = 5)

# ggsave("/home/bananafan/Documents/PhD/Paper1/output/asl_chloro_bar.png", plot = asl_chloro_bar,
#        height = 170, width = 170 * aspect_ratio, unit = "mm")
# 
# ggsave("/home/bananafan/Documents/PhD/Paper1/output/crossings_chloro_bar.png", plot = crossings_chloro_bar,
#        height = 170, width = 170 * aspect_ratio, unit = "mm")
# 
# ggsave("/home/bananafan/Documents/PhD/Paper1/output/signals_chloro_bar.png", plot = signals_chloro_bar,
#        height = 170, width = 170 * aspect_ratio, unit = "mm")















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




# # Generate barchart ATEEMPT 1
# # Generate new column that divides signal count into groups
# safety_borough_counts$Signals[is.na(safety_borough_counts$Signals)] = 0 # convert NA to 0
# safety_borough_counts <- safety_borough_counts %>%
#   mutate(signals_group = cut(Signals,
#                              breaks = c(0, 1, 20, 40, 60, 80, 100),
#                              labels = c("0", "1 to 20", "21 to 40", "41 to 60", "61 to 80",
#                                         "81 to 100"),
#                              right = FALSE))
# 
# 
# # # Create vector of colours that match the chloropleth
# my_colours_signals = c("#ffffd4", "#fed98e", "#fe9929", "#d95f0e", "#993404")
# # create Bar chart  ### THIS DOESNT WORK AS THE COLOURS ARE WRONG DUE TO 0 (zeros are dropped)
# ggplot(safety_borough_counts, aes(x = reorder(BOROUGH_short, -Signals), y = Signals, fill = signals_group)) +
#   geom_bar(stat = "identity", color = "black", size = 0.1) +
#   coord_flip() +
#   labs(y = "Count", x = NULL) +
#   theme_classic() +
#   scale_y_continuous(limits = c(0, 100), expand = c(0,0), 
#                      breaks = c(0, 20, 40, 60, 80, 100)) +  # ensures axis starts at 0 so no gap
#   scale_fill_manual(values = my_colours_signals) +
#   theme(axis.line.y = element_line(size = 0.2),
#         axis.ticks.y = element_blank(),
#         axis.line.x = element_line(size = 0.1),
#         legend.position = "none")


# facet plots to cope with difference in scale of values
# https://stackoverflow.com/questions/7194688/using-ggplot2-can-i-insert-a-break-in-the-axis?noredirect=1&lq=1

# df <- data.frame(myLetter=LETTERS[1:4], myValue=runif(12) + rep(c(4,0,0),2))  # cluster a few values well above 1
# df$myFacet <- df$myValue > 3
# (ggplot(df, aes(y=myLetter, x=myValue)) 
#   + geom_point() 
#   + facet_grid(. ~ myFacet, scales="free", space="free")
#   + scale_x_continuous(breaks = seq(0, 5, .25)) # this gives both facets equal interval spacing.
#   + theme(strip.text.x = element_blank()) # get rid of the facet labels
# 

# other code that might enable split axis
# 
# library(plotrix)
# gap.barplot(df$a, gap=c(5,495),horiz=T)
# 
# scale_x_discrete(labels=c("5", "", "","", "Extremely\ndifficult")) # this can alter the axis label


###############################################################################
###############################################################################
# Problem solving code for managing NAs, units etc

# # FOr ASL all values > 0
# # For raw - we cut from 1 upwards for bar chart
# # for area, units are in km^2 so had to drop units and round to 2dp in order to do the cut into categories for the bar chart
# # the parameters for cut were:
# #breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 43),
# #labels = c("0 < 2", "2 < 4", "4 < 6", "6 < 8",
# #           "8 < 10", "10 < 12", "12 < 14", "42 < 44"),
# 
# # FOr signals, we have values of 0 (signals) and NA (signalsNA)
# # can chloropleth SignalsNA_by_area ok with breaks at 0, 1 and then adding a legend that relabels grey missing as 0
# # Issue is then with generating the bar chart
# # PLan is to try different ways of cutting and potentially converting to numeric etc to see what works to generate bar chart
# 
# # atttempt 1)
# 
# smaller_dataset <- smaller_dataset %>%
#   mutate(signals_group1 = cut(Signals_by_area,
#                               breaks = c(0, 1, 2, 3, 4, 5, 21),
#                               labels = c("> 0 < 1", "1 < 2", "2 < 3", "3 < 4", "4 < 5", "20 < 21"),
#                               right = FALSE)) 
# 
# # attempt 2)
# # Drop units and round so that intervals are plotted ok
# smaller_dataset$Signals_by_area_numeric = round(units::drop_units(smaller_dataset$Signals_by_area), digits = 2)
# 
# smaller_dataset <- smaller_dataset %>%
#   mutate(signals_group2 = cut(Signals_by_area_numeric ,
#                               breaks = c(0, 1, 2, 3, 4, 5, 21),
#                               labels = c("> 0 < 1", "1 < 2", "2 < 3", "3 < 4", "4 < 5", "20 < 21"),
#                               right = FALSE)) 
# 
# # attempts 1 and 2 result in the right groups for everything > 1 but values of 0 and <1 are grouped together
# 
# # attemp 3) using SIgnalsNA_by_narea
# smaller_dataset <- smaller_dataset %>%
#   mutate(signals_group3 = cut(SignalsNA_by_area,
#                               breaks = c(0, 1, 2, 3, 4, 5, 21),
#                               labels = c("> 0 < 1", "1 < 2", "2 < 3", "3 < 4", "4 < 5", "20 < 21"),
#                               right = FALSE))  
# #-> 0s labelled as NA, but other groups done ok.  
# 
# # try barchar with this
# ggplot(smaller_dataset, 
#        aes(x = reorder(Borough_number, -SignalsNA_by_area), 
#            y = SignalsNA_by_area, fill = signals_group3)) +
#   geom_bar(stat = "identity", color = "black", size = 0.1)   # adds borders to bars )
# # Don't know how to automatically pick scale for object of type units. Defaulting to continuous.
# # Error in Ops.units(x, range[1]) : 
# #   both operands of the expression should be "units" objects
# 
# # attempt 4)
# # as above doesnt work as SignalsNA_by_area is in units then remove units
# smaller_dataset$SignalsNA_by_area_numeric = round(units::drop_units(smaller_dataset$SignalsNA_by_area), digits = 2) 
# 
# # now need to recut
# smaller_dataset <- smaller_dataset %>%
#   mutate(signals_group4 = cut(SignalsNA_by_area_numeric,
#                               breaks = c(0, 1, 2, 3, 4, 5, 21),
#                               labels = c("> 0 < 1", "1 < 2", "2 < 3", "3 < 4", "4 < 5", "20 < 21"),
#                               right = FALSE))
# 
# # now try bar chart again
# ggplot(smaller_dataset, 
#        aes(x = reorder(Borough_number, -SignalsNA_by_area_numeric), 
#            y = SignalsNA_by_area_numeric, fill = signals_group3)) +
#   geom_bar(stat = "identity", color = "black", size = 0.1)   # adds borders to bars ) # THis looks to work

