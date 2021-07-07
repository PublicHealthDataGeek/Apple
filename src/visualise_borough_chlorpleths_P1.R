###################################################################
# Visualising borough counts/lengths by chloropleth and bar chart #
#
# Code development started on 7/4/21
# 
# This code generates chlorpleths with bar charts showing the count of assets by Borough
# Cowplot Images saved 617x520 for chloro with bar

# Load packages
library(tidyverse)
library(tmap)
library(sf)
 library(cowplot)


################################
# Load and manipulate datasets #
################################

# 1) Local Authority spatial data

# import May 2020 ONS LA boundary data clipped to coastline (used so that River Thames appears)
lon_lad_2020_c2c = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFC.Rds")

# simply borough shapes
lon_lad_2020_c2c <- rmapshaper::ms_simplify(lon_lad_2020_c2c, keep=0.015) #Simplify boroughs

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

# Import CID borough counts 
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


# Join datasets together
CID_counts = left_join(CID_count_safety, CID_length_safety) 
denominators = left_join(lon_lad_2020_c2c_reduced, lon_pop_estimates_2019) %>%
  left_join(pct_borough_commuting)
chloropleth_dataset = left_join(denominators, CID_counts)


# Create variables with dropped units (ggplot doesnt like units)
chloropleth_dataset$Borough_Area_km2_no_units = round(units::drop_units(chloropleth_dataset$Borough_Area_km2), digits = 2)
chloropleth_dataset$clt_raw_numeric = round(units::drop_units(chloropleth_dataset$CycleLaneTrack_km), digits = 2)
chloropleth_dataset$clt_area_numeric = round(units::drop_units(chloropleth_dataset$CycleLaneTrack_km_by_area), digits = 2)
#? need to do for cycle commuting too

# produce counts by area and per 100000 head population
chloropleth_dataset <- chloropleth_dataset %>%
  mutate(across(.cols = c("ASL", "Crossings", "CycleLaneTrack_km", "Signals", "SignalsNA", "TrafficCalming"),
                .fns = ~.x/Borough_Area_km2_no_units,
                .names = "{.col}_by_area")) %>%
  mutate(across(.cols = c("ASL", "Crossings", "CycleLaneTrack_km", "Signals", "SignalsNA", "TrafficCalming"),
                .fns = ~.x/Population_100000,
                .names = "{.col}_per_100000pop")) %>%
  mutate(across(.cols = c("ASL", "Crossings", "CycleLaneTrack_km", "Signals","SignalsNA", "TrafficCalming"),
                .fns = ~.x/total_100000km_cycled_for_commuting_per_year_estimated,
                .names = "{.col}_per_100000km_cycled"))

# Create df of just City of London data as this is an outlier and often needs to be handled differently in the visualisations
city_chloropleth_dataset = chloropleth_dataset %>%
   filter(BOROUGH == "City of London")

# Create df without City of London data for some of the visualisations
drop_city = chloropleth_dataset %>%
  filter(BOROUGH != "City of London") 

###############################################################################
#                             Orientation maps                                #
###############################################################################

# 1) Boroughs areas
area_chloro = ggplot(chloropleth_dataset, 
                     aes(fill = Borough_Area_km2_no_units)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Blues",
                       na.value = "transparent", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# plot bar chart
area_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -Borough_Area_km2_no_units),
                                           y = Borough_Area_km2_no_units, fill = Borough_Area_km2_no_units)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +  # adds borders to bars
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 160), expand = c(0,0),
                     breaks = c(0, 120)) +
  geom_hline(aes(yintercept = mean(Borough_Area_km2_no_units)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(Borough_Area_km2_no_units)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 16, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
one_two = plot_grid(area_chloro, area_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))


# 2) Raw population
pop_chloro = ggplot(chloropleth_dataset, 
                     aes(fill = Population)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Greens",
                       na.value = "transparent", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
  
# plot bar chart
pop_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -Population_100000),
                                          y = Population_100000, fill = Population_100000)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +  
  coord_flip() +
  theme_classic() +
  scale_y_continuous(expand = c(0,0), breaks = c(0,2)) +  
  geom_hline(aes(yintercept = mean(Population_100000)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(Population_100000)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 16, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))
       
# Create cowplot of both plots
one_three = plot_grid(pop_chloro, pop_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))


# 3) PCT cycling chloropleth 
pct_chloro = ggplot(chloropleth_dataset, 
                    aes(fill = total_km_cycled_for_commuting_per_year_estimated)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       na.value = "transparent", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# create Bar chart
pct_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -total_km_cycled_for_commuting_per_year_estimated),
                                             y = total_km_cycled_for_commuting_per_year_estimated,
                                             fill = total_km_cycled_for_commuting_per_year_estimated)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 25000000), expand = c(0,0), breaks = c(0, 12500000), labels = c("0", "12.5mil")) +
  geom_hline(aes(yintercept = mean(total_km_cycled_for_commuting_per_year_estimated)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(total_km_cycled_for_commuting_per_year_estimated)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 16, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
one_four = plot_grid(pct_chloro, pct_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))



# # Create top row:
# plot_grid(one_two, one_three, one_four, nrow = 1) # doesnt look great
# 
# # try doing it by adding all plots - again doesnt look great
# plot_grid(area_chloro, area_bar, pop_chloro, pop_bar, pct_chloro, pct_bar, 
#           #rel_widths = c(1, 0.5, 1, 0.5, 1, 0.5), 
#           scale = c(1, 0.3, 1, 0.3, 1, 0.3),
#           nrow = 1)



###############################################################################
#                                   RAW COUNTS                                # 
###############################################################################


#######
# ASL #
#######

# create chloropleth
asl_raw_chloro = ggplot(chloropleth_dataset, 
                    aes(fill = ASL)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "YlOrBr", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# create Bar chart
asl_raw_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -ASL),
                                          y = ASL, fill = ASL)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 350), expand = c(0,0), breaks = c(0, 200)) +
  geom_hline(aes(yintercept = mean(ASL)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(ASL)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 16, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
two_one = plot_grid(asl_raw_chloro, asl_raw_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))


#############
# Crossings #
#############
cross_raw_chloro = ggplot(chloropleth_dataset, 
                        aes(fill = Crossings)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "YlOrBr", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

cross_raw_bar = ggplot(chloropleth_dataset, 
                       aes(x = reorder(Borough_number, -Crossings), y = Crossings, 
                           fill = Crossings)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 150), expand = c(0,0), breaks = c(0, 100)) +
  geom_hline(aes(yintercept = mean(Crossings)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(Crossings)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 16, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
three_one = plot_grid(cross_raw_chloro, cross_raw_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))


###################################
# Cycle lanes and Tracks - LENGTH #
###################################

# create chloropleth
clt_raw_chloro = ggplot(chloropleth_dataset, 
                        aes(fill = clt_raw_numeric)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "YlOrBr", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# create Bar chart
clt_raw_bar = ggplot(chloropleth_dataset, 
                     aes(x = reorder(Borough_number, -clt_raw_numeric), y = clt_raw_numeric, 
                         fill = clt_raw_numeric)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 150), expand = c(0,0), breaks = c(0, 75)) +
  geom_hline(aes(yintercept = mean(clt_raw_numeric)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(clt_raw_numeric)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 16, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
four_one = plot_grid(clt_raw_chloro, clt_raw_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))


###########
# Signals #
###########

# # create chloropleth - use SignalsNA as the polygon as this then colours the NAs white)
signals_raw_chloro = ggplot(chloropleth_dataset, 
                          aes(fill = SignalsNA)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "YlOrBr",
                       na.value = "light grey", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create bar
signals_raw_bar = ggplot(chloropleth_dataset, 
                       aes(x = reorder(Borough_number, -Signals), y = Signals, 
                           fill = SignalsNA)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 100), expand = c(0,0), breaks = c(0, 50)) +
  geom_hline(aes(yintercept = mean(Signals)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(Signals)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 16, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
five_one = plot_grid(signals_raw_chloro, signals_raw_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))

###################
# Traffic calming #   
###################
# create chloropleth
tc_raw_chloro = ggplot(chloropleth_dataset, 
                          aes(fill = TrafficCalming)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "YlOrBr", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# create bar chart
tc_raw_bar = ggplot(chloropleth_dataset, 
                         aes(x = reorder(Borough_number, -TrafficCalming), y = TrafficCalming, 
                             fill = TrafficCalming)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 3800), expand = c(0,0), breaks = c(0, 2000)) +
  geom_hline(aes(yintercept = mean(TrafficCalming)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(TrafficCalming)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 16, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
six_one = plot_grid(tc_raw_chloro, tc_raw_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))









###############################################################################
#                         Standardised to Borough Area -Blues                 # 
###############################################################################


#######
# ASL #
#######

asl_area_chloro = ggplot() + 
  geom_sf(data = drop_city, aes(fill = ASL_by_area), show.legend = F) +
  geom_sf(data = city_chloropleth_dataset, aes(fill = ASL_by_area), fill = "black") +
  scale_fill_distiller(type = "seq",
                       palette = "Blues",
                       na.value = "transparent", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))



# create Bar chart
# drop_city_reorder = drop_city %>%
#   arrange(ASL_by_area) %>%
#   mutate(Borough_number = row_number())
# drop_city_reorder2 = drop_city %>%
#   arrange(desc(ASL_by_area)) %>%
#   mutate(Borough_number = row_number())
drop_city_reorder = drop_city %>%
  arrange(desc(ASL_by_area)) %>%
  mutate(Borough_number = (row_number() + 1))

#FINAL BAR CHART CODE!!!!
asl_area_bar = ggplot() +
  geom_bar(data = city_chloropleth_dataset, aes(x = Borough_number, y = ASL_by_area),
           stat = "identity", color = "black", size = 0.1, fill = "black") +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  geom_bar(data = drop_city_reorder, aes(x = Borough_number, y = ASL_by_area, fill = ASL_by_area),
           stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 45), expand = c(0,0), breaks = c(0, 20)) +
  geom_hline(data = chloropleth_dataset, aes(yintercept = mean(ASL_by_area)),
             linetype = "solid") +
  geom_hline(data = chloropleth_dataset, aes(yintercept = median(ASL_by_area)),
             linetype = "dashed") +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 16, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
two_two = plot_grid(asl_area_chloro, asl_area_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))


# ggplot() +
#   geom_bar(data = drop_city_reorder, aes(x = Borough_number, y = ASL_by_area, fill = ASL_by_area),
#            stat = "identity", color = "black", size = 0.1) +
#   scale_fill_distiller(palette = "Blues", direction = 1) +
#   geom_bar(data = city_chloropleth_dataset, aes(x = Borough_number, y = ASL_by_area, fill = ASL_by_area),
#            stat = "identity", size = 0.1, fill = "black") +
#   coord_flip() +
#   theme_classic() +
# 
# # This works but the order of the non-city things are missed
# ggplot() +
#   geom_bar(data = city_chloropleth_dataset, aes(x = Borough_number, y = ASL_by_area),
#            stat = "identity", color = "black", size = 0.1) +
#   geom_bar(data = drop_city, aes(x = reorder(Borough_number, -ASL_by_area), y = ASL_by_area, fill = ASL_by_area),
#            stat = "identity", color = "black", size = 0.1) +
#   coord_flip() +
#   theme_classic()
# 
# # This returns an erro saysing can add gplot to ggplot!!!
# ggplot() +
#   geom_bar(data = city_chloropleth_dataset, aes(x = Borough_number, y = ASL_by_area),
#            stat = "identity", color = "black", size = 0.1) +
#   geom_bar(data = drop_city_reorder, aes(x = reorder(Borough_number, -ASL_by_area), y = ASL_by_area, fill = ASL_by_area),
#            stat = "identity", color = "black", size = 0.1) +
#   coord_flip() +
#   theme_classic()
# 
# # this works but one bar overlaps city
# ggplot() +
#   geom_bar(data = city_chloropleth_dataset, aes(x = Borough_number, y = ASL_by_area),
#            stat = "identity", color = "black", size = 0.1) +
#   geom_bar(data = drop_city_reorder2, aes(x = Borough_number, y = ASL_by_area, fill = ASL_by_area),
#            stat = "identity", color = "black", size = 0.1) +
#   coord_flip() +
#   theme_classic()
# 
# # this works!
# ggplot() +
#   geom_bar(data = city_chloropleth_dataset, aes(x = Borough_number, y = ASL_by_area),
#            stat = "identity", color = "black", size = 0.1) +
#   scale_fill_distiller(palette = "Blues", direction = 1) +
#   geom_bar(data = drop_city_reorder3, aes(x = Borough_number, y = ASL_by_area, fill = ASL_by_area),
#            stat = "identity", color = "black", size = 0.1) +
#   coord_flip() +
#   theme_classic()
# 
# #this works but city bar is grey
# ggplot() +
#   geom_bar(data = city_chloropleth_dataset, aes(x = Borough_number, y = ASL_by_area),
#            stat = "identity", color = "black", size = 0.1) +
#   scale_fill_distiller(palette = "Blues", direction = 1) +
#   geom_bar(data = drop_city_reorder3, aes(x = Borough_number, y = ASL_by_area, fill = ASL_by_area),
#            stat = "identity", color = "black", size = 0.1) +
#   coord_flip() +
#   theme_classic() +
#   scale_y_continuous(limits = c(0, 45), expand = c(0,0), breaks = c(0, 20)) +
#   geom_hline(data = chloropleth_dataset, aes(yintercept = mean(ASL_by_area)),
#              linetype = "solid") +
#   geom_hline(data = chloropleth_dataset, aes(yintercept = median(ASL_by_area)),
#              linetype = "dashed") +
#   theme(axis.line = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.text = element_text(size = 16, colour = "grey25"),
#         legend.position = "none",
#         axis.title = element_blank(),
#         plot.margin = unit(c(0, 0, 0, 0), "cm"))




#############
# Crossings #
#############

# create chloropleth
cross_area_chloro = ggplot() + 
  geom_sf(data = chloropleth_dataset, aes(fill = Crossings_by_area), show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Blues",
                       na.value = "transparent", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# create bar chart
cross_area_bar = ggplot(chloropleth_dataset, 
                    aes(x = reorder(Borough_number, -Crossings_by_area), y = Crossings_by_area, 
                        fill = Crossings_by_area)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 6), expand = c(0,0), breaks = c(0, 3)) +
  geom_hline(aes(yintercept = mean(Crossings_by_area)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(Crossings_by_area)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 16, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
three_two = plot_grid(cross_area_chloro, cross_area_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))


##########################################
# Cycle lanes and Tracks (length) - Area #
##########################################

# create chloropleth
clt_area_chloro = ggplot(chloropleth_dataset, 
                        aes(fill = clt_area_numeric)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Blues", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# create Bar chart
clt_area_bar = ggplot(chloropleth_dataset, 
                     aes(x = reorder(Borough_number, -clt_area_numeric), y = clt_area_numeric, 
                         fill = clt_area_numeric)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 8), expand = c(0,0), breaks = c(0, 4)) +
  geom_hline(aes(yintercept = mean(clt_area_numeric)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(clt_area_numeric)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 16, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
four_two = plot_grid(clt_area_chloro, clt_area_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))


###########
# Signals #
###########

# create chloropleth - 
sig_area_chloro = ggplot() + 
  geom_sf(data = drop_city, aes(fill = SignalsNA_by_area), show.legend = F) +
  geom_sf(data = city_chloropleth_dataset, aes(fill = SignalsNA_by_area), fill = "black") +
  scale_fill_distiller(type = "seq",
                       palette = "Blues",
                       na.value = "light grey", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create nar chart
drop_city_reorder = drop_city %>%
  arrange(desc(Signals_by_area)) %>%
  mutate(Borough_number = (row_number() + 1))

sig_area_bar = ggplot() +
  geom_bar(data = city_chloropleth_dataset, aes(x = Borough_number, y = Signals_by_area),
           stat = "identity", color = "black", size = 0.1, fill = "black") +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  geom_bar(data = drop_city_reorder, aes(x = Borough_number, y = Signals_by_area, fill = Signals_by_area),
           stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0,21), expand = c(0,0), breaks = c(0, 10)) +
  geom_hline(data = chloropleth_dataset, aes(yintercept = mean(Signals_by_area)),
             linetype = "solid") +
  geom_hline(data = chloropleth_dataset, aes(yintercept = median(Signals_by_area)),
             linetype = "dashed") +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 16, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
five_two = plot_grid(sig_area_chloro, sig_area_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))


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
# create chloropleth
tc_area_chloro = ggplot(chloropleth_dataset, 
                       aes(fill = TrafficCalming_by_area)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Blues", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))




# tc_area_chloro = tm_shape(chloropleth_dataset) +
#   tm_polygons("TrafficCalming_by_area", title = "Count by km^2", palette = "Blues",
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
# tc_area_chloro_grob = tmap_grob(tc_area_chloro)
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
# # create Bar chart
tc_area_bar = ggplot(chloropleth_dataset, 
                        aes(x = reorder(Borough_number, -TrafficCalming_by_area), y = TrafficCalming_by_area, 
                            fill = TrafficCalming_by_area)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 160), expand = c(0,0), breaks = c(0, 75)) +
  geom_hline(aes(yintercept = mean(TrafficCalming_by_area)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(TrafficCalming_by_area)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 16, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
six_two = plot_grid(tc_area_chloro, tc_area_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))

 
 



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
asl_pop_chloro_grob = tmap_grob(asl_pop_chloro) 
 
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
  draw_plot(asl_pop_chloro_grob) +
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
crossings_pop_chloro_grob = tmap_grob(crossings_pop_chloro) 
 
# # Generate barchart

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
  draw_plot(crossings_pop_chloro_grob) +
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
signals_pop_chloro_grob = tmap_grob(signals_pop_chloro)


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
  draw_plot(signals_pop_chloro_grob) +
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
clt_pop_chloro_grob = tmap_grob(clt_pop_chloro) 

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
  draw_plot(clt_pop_chloro_grob) +
  draw_plot(clt_pop_bar,
            width = 0.3, height = 0.6,
            x = 0.57, y = 0.19)



# #########################################
# # Traffic calming - Population (greens) # 
# #########################################

# # create chloropleth
tc_pop_chloro = tm_shape(chloropleth_dataset) +
  tm_polygons("TrafficCalming_per_100000pop", title = "Count per 100,000 population", palette = "Greens",
              breaks = c(0, 400, 800, 1200, 1600, 2000),
              legend.format = list(text.separator = "<")) +
  tm_layout(title = "Traffic calming",
            legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1,
            inner.margins = c(0.1,0.1,0.1,0.42), # creates wide right margin for barchart
            frame = FALSE)
 
# convert chloro to grob
tc_pop_chloro_grob = tmap_grob(tc_pop_chloro)

# # Generate barchart
# Generate new column that divides traffic calming count into groups
chloropleth_dataset <- chloropleth_dataset %>%
  mutate(tc_pop_group = cut(TrafficCalming_per_100000pop,
                             breaks = seq(0, 2000, by = 400),
                             labels = c("> 0 < 400", "400 < 800", "800 < 1200", "1200 < 1600", "1600 < 2000"),
                             right = FALSE))

# # Create vector of colours that match the chloropleth
tc_pop_colours = c("#edf8e9", "#bae4b3", "#74c476", "#006d2c") # only 4 colours as none in 1200<1600 group
#
# # create Bar chart
tc_pop_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -TrafficCalming_per_100000pop),
                                              y = TrafficCalming_per_100000pop,
                                              fill = tc_pop_group)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) + # adds borders to bars
  coord_flip() +
  labs(y = "Count per 100,000 population", x = NULL) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 1900), expand = c(0,0),
                     breaks = c(0, 400, 800, 1200, 1600, 2000)) +  # ensures axis starts at 0 so no gap
  scale_fill_manual(values = tc_pop_colours) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none")
# 
# # Create cowplot of both plots
tc_pop_chloro_bar = ggdraw() +
  draw_plot(tc_pop_chloro_grob) +
  draw_plot(tc_pop_bar,
            width = 0.3, height = 0.6,
            x = 0.57, y = 0.19)




###############################################################################
#       Standardised to PCT data(km cycled trhough borough) - Reds            # 
###############################################################################


#############
# ASL - PCT #
#############

# # create chloropleth
asl_pct_chloro = tm_shape(chloropleth_dataset) +
  tm_polygons("ASL_per_100000km_cycled", title = "Count per 100,000 km cycle commute", palette = "Reds",
              breaks = c(0, 2, 4, 6, 8, 10, 12, 14),
              legend.format = list(text.separator = "<")) +
  tm_layout(title = "ASL",
            legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1,
            inner.margins = c(0.1,0.1,0.1,0.42), # creates wide right margin for barchart
            frame = FALSE) 

# convert chloro to grob
asl_pct_chloro_grob = tmap_grob(asl_pct_chloro) 

# ##  Generate barchart

# Generate new column that divides ASL count into groups
chloropleth_dataset <- chloropleth_dataset %>%
  mutate(asl_pct_group = cut(ASL_per_100000km_cycled,
                              breaks = c(0, 2, 4, 6, 8, 10, 12, 14),
                              labels = c("> 0 < 2", "2 < 4", "4 < 6", "6 < 8",
                                         "8 < 10", "10 < 12", "12 < 14"),
                              right = FALSE))

# # Create vector of colours that match the chloropleth
asl_pct_colours = c("#fee5d9","#fcbba1", "#fc9272", "#fb6a4a",
                     "#ef3b2c", "#cb181d", "#99000d")

asl_pct_bar = ggplot(chloropleth_dataset, 
                      aes(x = reorder(Borough_number, -ASL_per_100000km_cycled), y = ASL_per_100000km_cycled, 
                          fill = asl_pct_group)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +  # adds borders to bars )
  coord_flip() +
  labs(y = "Count per 100,000km cycle commute", x = NULL) +
  theme_classic() +
  scale_y_continuous(expand = c(0,0)) +  # ensures axis starts at 0 so no gap
  scale_fill_manual(values = asl_pct_colours) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none",
        strip.text.x = element_blank())

# # Create cowplot of both plots
asl_pct_chloro_bar = ggdraw() +
  draw_plot(asl_pct_chloro_grob) +
  draw_plot(asl_pct_bar,
            width = 0.3, height = 0.6,
            x = 0.57, y = 0.19)


##########################
# Crossings - PCT - reds #
##########################

# create chloropleth
crossings_pct_chloro = tm_shape(chloropleth_dataset) +
  tm_polygons("Crossings_per_100000km_cycled", title = "Count per 100,000 km cycle commute", palette = "Reds", 
              legend.format = list(text.separator = "<")) +
  tm_layout(title = "Crossings",
            legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1,
            inner.margins = c(0.1,0.1,0.1,0.42), # creates wide right margin for barchart
            frame = FALSE)

# # convert chloro to grob
crossings_pct_chloro_grob = tmap_grob(crossings_pct_chloro) 

# # Generate barchart
# Generate new column that divides Crossing count into groups
chloropleth_dataset <- chloropleth_dataset %>%
  mutate(crossings_pct_group = cut(Crossings_per_100000km_cycled,
                                   breaks = c(0,2,4,6,8,10),
                                   labels = c(">0 < 2", "2 < 4", "4 < 6", "6 < 8", "8 < 10"),
                                   right = FALSE))

# # # Create vector of colours that match the chloropleth
crossings_pct_colours = c("#fee5d9","#fcae91", "#fb6a4a", "#de2d26", "#a50f15")

# create Bar chart
crossings_pct_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -Crossings_per_100000km_cycled),
                                                    y = Crossings_per_100000km_cycled,
                                                    fill = crossings_pct_group)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +  # adds borders to bars
  coord_flip() +
  labs(y = "Count per 100,000 km cycle commute", x = NULL) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 10), expand = c(0,0)) +
  scale_fill_manual(values = crossings_pct_colours) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none")

# # # Create cowplot of both plots
crossings_pct_chloro_bar = ggdraw() +
  draw_plot(crossings_pct_chloro_grob) +
  draw_plot(crossings_pct_bar,
            width = 0.3, height = 0.6,
            x = 0.57, y = 0.19)


########################
# Signals - PCT - reds #
########################

# # create chloropleth - use SignalsNA as the polygon as this then colours the NAs grey (missing)
signals_pct_chloro = tm_shape(chloropleth_dataset) +
  tm_polygons("SignalsNA_per_100000km_cycled", palette = "Reds",
              breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5),
              legend.show = FALSE) + 
  tm_layout(title = "Signals",
            legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1,
            inner.margins = c(0.1,0.1,0.1,0.42), # creates wide right margin for barchart
            frame = FALSE) +
  tm_add_legend(type = "fill", 
                labels = c("0", "> 0 < 0.25", "0.25 < 0.50", "0.50 < 0.75", "0.75 < 1.00", 
                           "1.00 < 1.25", "1.25 < 1.50"), # enables relabelling of 'Missing/NA' as 0
                col = c("grey", "#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26", "#a50f15"),
                border.lwd = 0.5,
                title = "Count per 100,000 km cycle commute")

# convert chloro to grob
signals_pct_chloro_grob = tmap_grob(signals_pct_chloro)


# Generate barchart
# Generate new column that divides signal count into groups
chloropleth_dataset <- chloropleth_dataset %>%
  mutate(signals_pct_group = cut(SignalsNA_per_100000km_cycled,
                                 breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5),
                                 labels = c("> 0 < 0.25", "0.25 < 0.50", "0.50 < 0.75", "0.75 < 1.00", 
                                            "1.00 < 1.25", "1.25 < 1.50"),
                                 right = FALSE))


# Create vector of colours that match the chloropleth
signals_pct_colours = c("#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a", "#a50f15")

# # # create Bar chart
signals_pct_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -SignalsNA_per_100000km_cycled),
                                                  y = SignalsNA_per_100000km_cycled, # this means those that are missing are shown with no bars
                                                  fill = signals_pct_group)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  labs(y = "Count per 100,000 km cycle commute", x = NULL) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 1.5), expand = c(0,0)) +  # ensures axis starts at 0 so no gap
  scale_fill_manual(values = signals_pct_colours) +
  theme(axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none")

# # Create cowplot of both plots
signals_pct_chloro_bar = ggdraw() +
  draw_plot(signals_pct_chloro_grob) +
  draw_plot(signals_pct_bar,
            width = 0.3, height = 0.6,
            x = 0.57, y = 0.19)


################################################
# Cycle lanes and Tracks (length) - PCT - reds #
################################################

# create chloropleth
clt_pct_chloro = tm_shape(chloropleth_dataset) +
  tm_polygons("CycleLaneTrack_km_per_100000km_cycled", title = "Total length (km) per 100,000km cycle commute", 
              breaks = c(0, 4, 8, 12, 16, 20, 24),
              palette = "Reds") +
  tm_layout(title = "Cycle lanes and tracks",
            legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1,
            inner.margins = c(0.1,0.1,0.1,0.42), # creates wide right margin for barchart
            frame = FALSE)

# # # convert chloro to grob
clt_pct_chloro_grob = tmap_grob(clt_pct_chloro) 

# # Generate barchart
# create new column where units (km^2) are removed (need units to be removed to draw bar chart)
chloropleth_dataset$clt_pct_numeric = round(units::drop_units(chloropleth_dataset$CycleLaneTrack_km_per_100000km_cycled), digits = 2)

chloropleth_dataset <- chloropleth_dataset %>%
  mutate(clt_pct_group = cut(clt_pct_numeric,
                             breaks = c(0, 4, 8, 12, 16, 20, 24),
                             labels = c("> 0 < 4", "4 < 8", "8 < 12", "12 < 16", "16 < 20",
                                        "20 < 24"),
                             right = FALSE))

# Create vector of colours that match the chloropleth
clt_pct_colours = c("#fee5d9","#fcbba1", "#fc9272", "#fb6a4a",
                      "#de2d26", "#a50f15")

# create Bar chart
clt_pct_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -clt_pct_numeric),
                                              y = clt_pct_numeric, fill = clt_pct_group)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +  # adds borders to bars
  coord_flip() +
  labs(y = "Total length (km) per 100,000km cycle commute", x = NULL) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 24), expand = c(0,0)) +  # ensures axis starts at 0 so no gap
  scale_fill_manual(values = clt_pct_colours) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none")

# # Create cowplot of both plots
clt_pct_chloro_bar = ggdraw() +
  draw_plot(clt_pct_chloro_grob) +
  draw_plot(clt_pct_bar,
            width = 0.3, height = 0.6,
            x = 0.57, y = 0.19)


# ################################
# # Traffic calming - PCT (reds) # 
# ################################

# # create chloropleth
tc_pct_chloro = tm_shape(chloropleth_dataset) +
  tm_polygons("TrafficCalming_per_100000km_cycled", title = "Count per 100,000km cycle commute", palette = "Reds",
              legend.format = list(text.separator = "<")) +
  tm_layout(title = "Traffic calming",
            legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1,
            inner.margins = c(0.1,0.1,0.1,0.42), # creates wide right margin for barchart
            frame = FALSE)

# convert chloro to grob
tc_pct_chloro_grob = tmap_grob(tc_pct_chloro)

# # Generate barchart
# Generate new column that divides traffic calming count into groups
chloropleth_dataset <- chloropleth_dataset %>%
  mutate(tc_pct_group = cut(TrafficCalming_per_100000km_cycled,
                            breaks = seq(0, 350, by = 50),
                            labels = c("> 0 < 50", "50 < 100", "100 < 150", 
                                       "150 < 200", "200 < 250", "250 < 300", "300 < 350"),
                            right = FALSE))

# # Create vector of colours that match the chloropleth
tc_pct_colours = c("#fee5d9","#fcbba1", "#fc9272", "#fb6a4a",
                   "#ef3b2c", "#99000d")
#
# # create Bar chart
tc_pct_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -TrafficCalming_per_100000km_cycled),
                                             y = TrafficCalming_per_100000km_cycled,
                                             fill = tc_pct_group)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) + # adds borders to bars
  coord_flip() +
  labs(y = " Count per 100,000km cycle commute", x = NULL) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 350), expand = c(0,0)) +
  scale_fill_manual(values = tc_pct_colours) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none")
#
# # Create cowplot of both plots
tc_pct_chloro_bar = ggdraw() +
  draw_plot(tc_pct_chloro_grob) +
  draw_plot(tc_pct_bar,
            width = 0.3, height = 0.6,
            x = 0.57, y = 0.19)


###############################################################################
#                                List of charts                               #
###############################################################################

# # Orientation
boroughs_labelled_map
area_chloro_bar
pop_chloro_bar
pct_chloro_bar

# Raw
asl_raw_chloro_bar
crossings_raw_chloro_bar
signals_raw_chloro_bar
tc_raw_chloro_bar
clt_raw_chloro_bar

# Area
asl_area_chloro_bar
crossings_area_chloro_bar
signals_area_chloro_bar
tc_area_chloro_bar
clt_area_chloro_bar

#Population
asl_pop_chloro_bar
crossings_pop_chloro_bar
signals_pop_chloro_bar
tc_pop_chloro_bar
clt_pop_chloro_bar

# PCT
asl_pct_chloro_bar
crossings_pct_chloro_bar
signals_pct_chloro_bar
tc_pct_chloro_bar
clt_pct_chloro_bar


####################################################
# Use patchwork to create arrangement of all plots #
####################################################

# Use patchwork to create all plots
all_orientation = plot_spacer() | area_chloro_bar | pop_chloro_bar | pct_chloro_bar
all_asl_chloro_bar = asl_raw_chloro_bar | asl_area_chloro_bar | asl_pop_chloro_bar | asl_pct_chloro_bar
all_crossings_chloro_bar = crossings_raw_chloro_bar | crossings_area_chloro_bar | crossings_pop_chloro_bar | crossings_pct_chloro_bar
all_signals_chloro_bar = signals_raw_chloro_bar | signals_area_chloro_bar | signals_pop_chloro_bar | signals_pct_chloro_bar
all_tc_chloro_bar = tc_raw_chloro_bar | tc_area_chloro_bar | tc_pop_chloro_bar | tc_pct_chloro_bar
all_clt_chloro_bar = clt_raw_chloro_bar | clt_area_chloro_bar | clt_pop_chloro_bar | clt_pct_chloro_bar


# # Use patchwork to create plot of all raw plots
# raw_chloro_bar_plots = (asl_raw_chloro_bar | crossings_raw_chloro_bar) / 
#   (signals_raw_chloro_bar | tc_raw_chloro_bar) /  
#   clt_raw_chloro_bar

# aso
# asl_raw_chloro_bar/crossings_raw_chloro_bar/signals_raw_chloro_bar/tc_raw_chloro_bar/clt_raw_chloro_bar
# 
# #Cowplot of both plots
# all_orientation_maps = ggdraw() +
#   #draw_plot(boroughs_labelled_map) +
#   draw_plot(area_chloro) + 
#   draw_plot(population_chloro) +
#   draw_plot(pct_chloro)



##################
# SAVE ALL PLOTS #
##################

# Code for saving all bar and chloropleths separately
mypath = "/home/bananafan/Documents/PhD/Paper1/output/maps/"
barchart_names = c("area_bar", "pop_bar", "pct_bar", 
                   "asl_raw_bar", "asl_area_bar", "asl_pop_bar", "asl_pct_bar",
                   "crossings_raw_bar", "crossings_area_bar", "crossings_pop_bar", "crossings_pct_bar",
                   "clt_raw_bar", "clt_area_bar", "clt_pop_bar", "clt_pct_bar",
                   "signals_raw_bar", "signals_area_bar", "signals_pop_bar", "signals_pct_bar",
                   "tc_raw_bar", "tc_area_bar", "tc_pop_bar", "tc_pct_bar")

for(i in 1:length(barchart_names)) {
  ggsave(get(barchart_names[i]),
         file = paste0(mypath, barchart_names[i], ".png"), dpi = 500, width = 25* (14/5), height = 50 * (14/5), units = "mm"
  )
}

chloro_names = c("area_chloro", "pop_chloro", "pct_chloro", 
                 "asl_raw_chloro", "asl_area_chloro", "asl_pop_chloro", "asl_pct_chloro",
                 "crossings_raw_chloro", "crossings_area_chloro", "crossings_pop_chloro", "crossings_pct_chloro",
                 "clt_raw_chloro", "clt_area_chloro", "clt_pop_chloro", "clt_pct_chloro",
                 "signals_raw_chloro", "signals_area_chloro", "signals_pop_chloro", "signals_pct_chloro",
                 "tc_raw_chloro", "tc_area_chloro", "tc_pop_chloro", "tc_pct_chloro")

for(i in 1:length(chloro_names)) {
  tmap_save(get(chloro_names[i]),
            file = paste0(mypath, chloro_names[i], ".png"), dpi = 500, width = 120* (14/5), height = 85 * (14/5), units = "mm"
  )
}













ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/all_orientation_chlorobar.pdf", plot = all_orientation, 
       dpi = 1000, width = 120 * (14/5), height = 60 * (14/5), units = "mm")
ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/all_asl_chlorobar.pdf", plot = all_asl_chloro_bar, 
       dpi = 1000, width = 120 * (14/5), height = 60 * (14/5), units = "mm")
ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/all_crossings_chlorobar.pdf", plot = all_crossings_chloro_bar, 
       dpi = 1000, width = 120 * (14/5), height = 60 * (14/5), units = "mm")
ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/all_signals_chlorobar.pdf", plot = all_signals_chloro_bar, 
       dpi = 1000, width = 120 * (14/5), height = 60 * (14/5), units = "mm")
ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/all_trafficcalming_chlorobar.pdf", plot = all_tc_chloro_bar, 
       dpi = 1000, width = 120 * (14/5), height = 60 * (14/5), units = "mm")
ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/all_clt_chlorobar.pdf", plot = all_clt_chloro_bar, 
       dpi = 1000, width = 120 * (14/5), height = 60 * (14/5), units = "mm")

# ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/all_orientation.pdf", plot = all_orientation, 
#        dpi = 1000, width = 120* (14/5), height = 85 * (14/5), units = "mm")
# 
# ggsave("/home/bananafan/Documents/PhD/Paper1/output/all_chloro_bar_plots.pdf", plot = all_chloro_bar_plots, 
#        dpi = 1000, width = 300 * (14/5), height = 1210 * (14/5), units = "mm")





# ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/area_bar.png", plot = area_bar,
#        dpi = 500, width = 25* (14/5), height = 50 * (14/5), units = "mm")
# tmap_save(tm = area_chloro, "/home/bananafan/Documents/PhD/Paper1/output/maps/area_chloro2.png", 
#        dpi = 500, width = 120* (14/5), height = 85 * (14/5), units = "mm")
# 
# ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/pop_bar.png", plot = pop_bar,
#        dpi = 500, width = 25* (14/5), height = 50 * (14/5), units = "mm")
# tmap_save(tm = pop_chloro, "/home/bananafan/Documents/PhD/Paper1/output/maps/pop_chloro.png", 
#           dpi = 500, width = 120* (14/5), height = 85 * (14/5), units = "mm")
# 
# ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/pct_bar.png", plot = pct_bar,
#        dpi = 500, width = 25* (14/5), height = 50 * (14/5), units = "mm")
# tmap_save(tm = pct_chloro, "/home/bananafan/Documents/PhD/Paper1/output/maps/pct_chloro.png", 
#           dpi = 500, width = 120* (14/5), height = 85 * (14/5), units = "mm")
# 
# ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/asl_raw_bar.png", plot = asl_raw_bar,
#        dpi = 500, width = 25* (14/5), height = 50 * (14/5), units = "mm")
# tmap_save(tm = asl_raw_chloro, "/home/bananafan/Documents/PhD/Paper1/output/maps/asl_raw_chloro.png", 
#           dpi = 500, width = 120* (14/5), height = 85 * (14/5), units = "mm")
# 
# ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/asl_area_bar.png", plot = asl_area_bar,
#        dpi = 500, width = 25* (14/5), height = 50 * (14/5), units = "mm")
# tmap_save(tm = asl_area_chloro, "/home/bananafan/Documents/PhD/Paper1/output/maps/asl_area_chloro.png", 
#           dpi = 500, width = 120* (14/5), height = 85 * (14/5), units = "mm")
# 
# ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/asl_pop_bar.png", plot = asl_pop_bar,
#        dpi = 500, width = 25* (14/5), height = 50 * (14/5), units = "mm")
# tmap_save(tm = asl_pop_chloro, "/home/bananafan/Documents/PhD/Paper1/output/maps/asl_pop_chloro.png", 
#           dpi = 500, width = 120* (14/5), height = 85 * (14/5), units = "mm")
# 
# ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/asl_pct_bar.png", plot = asl_pct_bar,
#        dpi = 500, width = 25* (14/5), height = 50 * (14/5), units = "mm")
# tmap_save(tm = asl_pct_chloro, "/home/bananafan/Documents/PhD/Paper1/output/maps/asl_pct_chloro.png", 
#           dpi = 500, width = 120* (14/5), height = 85 * (14/5), units = "mm")
# 
# ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/crossings_raw_bar.png", plot = crossings_raw_bar,
#        dpi = 500, width = 25* (14/5), height = 50 * (14/5), units = "mm")
# tmap_save(tm = crossings_raw_chloro, "/home/bananafan/Documents/PhD/Paper1/output/maps/crossings_raw_chloro.png", 
#           dpi = 500, width = 120* (14/5), height = 85 * (14/5), units = "mm")
# 
# ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/crossings_area_bar.png", plot = crossings_area_bar,
#        dpi = 500, width = 25* (14/5), height = 50 * (14/5), units = "mm")
# tmap_save(tm = crossings_area_chloro, "/home/bananafan/Documents/PhD/Paper1/output/maps/crossings_area_chloro.png", 
#           dpi = 500, width = 120* (14/5), height = 85 * (14/5), units = "mm")
# 
# ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/crossings_pop_bar.png", plot = crossings_pop_bar,
#        dpi = 500, width = 25* (14/5), height = 50 * (14/5), units = "mm")
# tmap_save(tm = crossings_pop_chloro, "/home/bananafan/Documents/PhD/Paper1/output/maps/crossings_pop_chloro.png", 
#           dpi = 500, width = 120* (14/5), height = 85 * (14/5), units = "mm")
# 
# ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/crossings_pct_bar.png", plot = crossings_pct_bar,
#        dpi = 500, width = 25* (14/5), height = 50 * (14/5), units = "mm")
# tmap_save(tm = crossings_pct_chloro, "/home/bananafan/Documents/PhD/Paper1/output/maps/crossings_pct_chloro.png", 
#           dpi = 500, width = 120* (14/5), height = 85 * (14/5), units = "mm")
# 
# ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/clt_raw_bar.png", plot = clt_raw_bar,
#        dpi = 500, width = 25* (14/5), height = 50 * (14/5), units = "mm")
# tmap_save(tm = clt_raw_chloro, "/home/bananafan/Documents/PhD/Paper1/output/maps/clt_raw_chloro.png", 
#           dpi = 500, width = 120* (14/5), height = 85 * (14/5), units = "mm")
# 
# ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/clt_area_bar.png", plot = clt_area_bar,
#        dpi = 500, width = 25* (14/5), height = 50 * (14/5), units = "mm")
# tmap_save(tm = clt_area_chloro, "/home/bananafan/Documents/PhD/Paper1/output/maps/clt_area_chloro.png", 
#           dpi = 500, width = 120* (14/5), height = 85 * (14/5), units = "mm")
# 
# ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/clt_pop_bar.png", plot = clt_pop_bar,
#        dpi = 500, width = 25* (14/5), height = 50 * (14/5), units = "mm")
# tmap_save(tm = clt_pop_chloro, "/home/bananafan/Documents/PhD/Paper1/output/maps/clt_pop_chloro.png", 
#           dpi = 500, width = 120* (14/5), height = 85 * (14/5), units = "mm")
# 
# ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/clt_pct_bar.png", plot = clt_pct_bar,
#        dpi = 500, width = 25* (14/5), height = 50 * (14/5), units = "mm")
# tmap_save(tm = clt_pct_chloro, "/home/bananafan/Documents/PhD/Paper1/output/maps/clt_pct_chloro.png", 
#           dpi = 500, width = 120* (14/5), height = 85 * (14/5), units = "mm")
# 
# ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/signals_raw_bar.png", plot = signals_raw_bar,
#        dpi = 500, width = 25* (14/5), height = 50 * (14/5), units = "mm")
# tmap_save(tm = signals_raw_chloro, "/home/bananafan/Documents/PhD/Paper1/output/maps/signals_raw_chloro.png", 
#           dpi = 500, width = 120* (14/5), height = 85 * (14/5), units = "mm")
# 
# ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/signals_area_bar.png", plot = signals_area_bar,
#        dpi = 500, width = 25* (14/5), height = 50 * (14/5), units = "mm")
# tmap_save(tm = signals_area_chloro, "/home/bananafan/Documents/PhD/Paper1/output/maps/signals_area_chloro.png", 
#           dpi = 500, width = 120* (14/5), height = 85 * (14/5), units = "mm")
# 
# ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/signals_pop_bar.png", plot = signals_pop_bar,
#        dpi = 500, width = 25* (14/5), height = 50 * (14/5), units = "mm")
# tmap_save(tm = signals_pop_chloro, "/home/bananafan/Documents/PhD/Paper1/output/maps/signals_pop_chloro.png", 
#           dpi = 500, width = 120* (14/5), height = 85 * (14/5), units = "mm")
# 
# ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/signals_pct_bar.png", plot = signals_pct_bar,
#        dpi = 500, width = 25* (14/5), height = 50 * (14/5), units = "mm")
# tmap_save(tm = signals_pct_chloro, "/home/bananafan/Documents/PhD/Paper1/output/maps/signals_pct_chloro.png", 
#           dpi = 500, width = 120* (14/5), height = 85 * (14/5), units = "mm")
# 
# ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/tc_raw_bar.png", plot = tc_raw_bar,
#        dpi = 500, width = 25* (14/5), height = 50 * (14/5), units = "mm")
# tmap_save(tm = tc_raw_chloro, "/home/bananafan/Documents/PhD/Paper1/output/maps/tc_raw_chloro.png", 
#           dpi = 500, width = 120* (14/5), height = 85 * (14/5), units = "mm")
# 
# ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/tc_area_bar.png", plot = tc_area_bar,
#        dpi = 500, width = 25* (14/5), height = 50 * (14/5), units = "mm")
# tmap_save(tm = tc_area_chloro, "/home/bananafan/Documents/PhD/Paper1/output/maps/tc_area_chloro.png", 
#           dpi = 500, width = 120* (14/5), height = 85 * (14/5), units = "mm")
# 
# ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/tc_pop_bar.png", plot = tc_pop_bar,
#        dpi = 500, width = 25* (14/5), height = 50 * (14/5), units = "mm")
# tmap_save(tm = tc_pop_chloro, "/home/bananafan/Documents/PhD/Paper1/output/maps/tc_pop_chloro.png", 
#           dpi = 500, width = 120* (14/5), height = 85 * (14/5), units = "mm")
# 
# ggsave("/home/bananafan/Documents/PhD/Paper1/output/maps/tc_pct_bar.png", plot = tc_pct_bar,
#        dpi = 500, width = 25* (14/5), height = 50 * (14/5), units = "mm")
# tmap_save(tm = tc_pct_chloro, "/home/bananafan/Documents/PhD/Paper1/output/maps/tc_pct_chloro.png", 
#           dpi = 500, width = 120* (14/5), height = 85 * (14/5), units = "mm")



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

