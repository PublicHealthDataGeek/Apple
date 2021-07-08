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
  mutate(total_mil_km_cycled_for_commuting_per_year_estimated = total_km_cycled_for_commuting_per_year_estimated / 1000000) 


# Join datasets together
CID_counts = left_join(CID_count_safety, CID_length_safety) 
denominators = left_join(lon_lad_2020_c2c_reduced, lon_pop_estimates_2019) %>%
  left_join(pct_borough_commuting)
chloropleth_dataset = left_join(denominators, CID_counts)


# Create variables with dropped units (ggplot doesnt like units)
chloropleth_dataset$Borough_Area_km2_no_units = round(units::drop_units(chloropleth_dataset$Borough_Area_km2), digits = 2)

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
                .fns = ~.x/total_mil_km_cycled_for_commuting_per_year_estimated,
                .names = "{.col}_per_mil_km_cycled"))

# Create variables with dropped units (ggplot doesnt like units)
chloropleth_dataset$clt_raw_numeric = round(units::drop_units(chloropleth_dataset$CycleLaneTrack_km), digits = 2)
chloropleth_dataset$clt_area_numeric = round(units::drop_units(chloropleth_dataset$CycleLaneTrack_km_by_area), digits = 2)
chloropleth_dataset$clt_pop_numeric = round(units::drop_units(chloropleth_dataset$CycleLaneTrack_km_per_100000pop), digits = 2)
chloropleth_dataset$clt_pct_numeric = round(units::drop_units(chloropleth_dataset$CycleLaneTrack_km_per_mil_km_cycled), digits = 2)


# Create df of just City of London data as this is an outlier and often needs to be handled differently in the visualisations
city_chloropleth_dataset = chloropleth_dataset %>%
   filter(BOROUGH == "City of London")

# Create df without City of London data for some of the visualisations
drop_city = chloropleth_dataset %>%
  filter(BOROUGH != "City of London") 

###############################################################################
#                             Reference data maps                             #
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
       aes(fill = total_mil_km_cycled_for_commuting_per_year_estimated)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       na.value = "transparent", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# create Bar chart
pct_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -total_mil_km_cycled_for_commuting_per_year_estimated),
                                y = total_mil_km_cycled_for_commuting_per_year_estimated,
                                fill = total_mil_km_cycled_for_commuting_per_year_estimated)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 25), expand = c(0,0), breaks = c(0, 12.5), labels = c("0", "12.5")) +
  geom_hline(aes(yintercept = mean(total_mil_km_cycled_for_commuting_per_year_estimated)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(total_mil_km_cycled_for_commuting_per_year_estimated)),
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

#Create barchart
drop_city_reorder = drop_city %>%
  arrange(desc(ASL_by_area)) %>%
  mutate(Borough_number = (row_number() + 1))  # this ensures that there is 'spare' bar space for the city one to drop into when plotted

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
# # Traffic calming #   
# ###################
 
# create chloropleth
tc_area_chloro = ggplot(chloropleth_dataset, 
                       aes(fill = TrafficCalming_by_area)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Blues", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

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

# Create chloropleth
asl_pop_chloro = ggplot() + 
  geom_sf(data = drop_city, aes(fill = ASL_per_100000pop), show.legend = F) +
  geom_sf(data = city_chloropleth_dataset, aes(fill = ASL_per_100000pop), fill = "black") +
  scale_fill_distiller(type = "seq",
                       palette = "Greens",
                       na.value = "light grey", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create barchart
drop_city_reorder = drop_city %>%
  arrange(desc(ASL_per_100000pop)) %>%
  mutate(Borough_number = (row_number() + 1))  # this ensures that there is 'spare' bar space for the city one to drop into when plotted

asl_pop_bar = ggplot() +
  geom_bar(data = city_chloropleth_dataset, aes(x = Borough_number, y = ASL_per_100000pop),
           stat = "identity", color = "black", size = 0.1, fill = "black") +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  geom_bar(data = drop_city_reorder, aes(x = Borough_number, y = ASL_per_100000pop, fill = ASL_per_100000pop),
           stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 1300), expand = c(0,0), breaks = c(0, 400)) +
  geom_hline(data = chloropleth_dataset, aes(yintercept = mean(ASL_per_100000pop)),
             linetype = "solid") +
  geom_hline(data = chloropleth_dataset, aes(yintercept = median(ASL_per_100000pop)),
             linetype = "dashed") +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 16, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
two_three = plot_grid(asl_pop_chloro, asl_pop_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))


##########################
# Crossings - Population #
##########################

# create chloropleth
cross_pop_chloro = ggplot() + 
  geom_sf(data = drop_city, aes(fill = Crossings_per_100000pop), show.legend = F) +
  geom_sf(data = city_chloropleth_dataset, aes(fill = Crossings_per_100000pop), fill = "black") +
  scale_fill_distiller(type = "seq",
                       palette = "Greens",
                       na.value = "light grey", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create barchart
drop_city_reorder = drop_city %>%
  arrange(desc(Crossings_per_100000pop)) %>%
  mutate(Borough_number = (row_number() + 1))  # this ensures that there is 'spare' bar space for the city one to drop into when plotted

cross_pop_bar = ggplot() +
  geom_bar(data = city_chloropleth_dataset, aes(x = Borough_number, y = Crossings_per_100000pop),
           stat = "identity", color = "black", size = 0.1, fill = "black") +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  geom_bar(data = drop_city_reorder, aes(x = Borough_number, y = Crossings_per_100000pop, fill = Crossings_per_100000pop),
           stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 180), expand = c(0,0), breaks = c(0, 50)) +
  geom_hline(data = chloropleth_dataset, aes(yintercept = mean(Crossings_per_100000pop)),
             linetype = "solid") +
  geom_hline(data = chloropleth_dataset, aes(yintercept = median(Crossings_per_100000pop)),
             linetype = "dashed") +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 16, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
three_three = plot_grid(cross_pop_chloro, cross_pop_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))

################################################
# Cycle lanes and Tracks (length) - Population #
################################################

# Create chloropleth
clt_pop_chloro = ggplot() + 
  geom_sf(data = drop_city, aes(fill = clt_pop_numeric), show.legend = F) +
  geom_sf(data = city_chloropleth_dataset, aes(fill = clt_pop_numeric), fill = "black") +
  scale_fill_distiller(type = "seq",
                       palette = "Greens", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create barchart
drop_city_reorder = drop_city %>%
  arrange(desc(clt_pop_numeric)) %>%
  mutate(Borough_number = (row_number() + 1))  # this ensures that there is 'spare' bar space for the city one to drop into when plotted

clt_pop_bar = ggplot() +
  geom_bar(data = city_chloropleth_dataset, aes(x = Borough_number, y = clt_pop_numeric),
           stat = "identity", color = "black", size = 0.1, fill = "black") +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  geom_bar(data = drop_city_reorder, aes(x = Borough_number, y = clt_pop_numeric, fill = clt_pop_numeric),
           stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 235), expand = c(0,0), breaks = c(0, 50)) +
  geom_hline(data = chloropleth_dataset, aes(yintercept = mean(clt_pop_numeric)),
             linetype = "solid") +
  geom_hline(data = chloropleth_dataset, aes(yintercept = median(clt_pop_numeric)),
             linetype = "dashed") +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 16, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
four_three = plot_grid(clt_pop_chloro, clt_pop_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))


########################
# Signals - Population #
########################

# create chloropleth - 
sig_pop_chloro = ggplot() + 
  geom_sf(data = drop_city, aes(fill = SignalsNA_per_100000pop), show.legend = F) +
  geom_sf(data = city_chloropleth_dataset, aes(fill = Signals_per_100000pop), fill = "black") +
  scale_fill_distiller(type = "seq",
                       palette = "Greens",
                       na.value = "light grey", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create bar chart
drop_city_reorder = drop_city %>%
  arrange(desc(Signals_per_100000pop)) %>%
  mutate(Borough_number = (row_number() + 1))

sig_pop_bar = ggplot() +
  geom_bar(data = city_chloropleth_dataset, aes(x = Borough_number, y = Signals_per_100000pop),
           stat = "identity", color = "black", size = 0.1, fill = "black") +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  geom_bar(data = drop_city_reorder, aes(x = Borough_number, y = Signals_per_100000pop, fill = Signals_per_100000pop),
           stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 600), expand = c(0,0), breaks = c(0, 300)) +
  geom_hline(data = chloropleth_dataset, aes(yintercept = mean(Signals_per_100000pop)),
             linetype = "solid") +
  geom_hline(data = chloropleth_dataset, aes(yintercept = median(Signals_per_100000pop)),
             linetype = "dashed") +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 16, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
five_three = plot_grid(sig_pop_chloro, sig_pop_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))

# #########################################
# # Traffic calming - Population (greens) # 
# #########################################

# create chloropleth
tc_pop_chloro = ggplot(chloropleth_dataset, 
                        aes(fill = TrafficCalming_per_100000pop)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Greens", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# # create Bar chart
tc_pop_bar = ggplot(chloropleth_dataset, 
                     aes(x = reorder(Borough_number, -TrafficCalming_per_100000pop), y = TrafficCalming_per_100000pop, 
                         fill = TrafficCalming_per_100000pop)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 2000), expand = c(0,0), breaks = c(0, 1000)) +
  geom_hline(aes(yintercept = mean(TrafficCalming_per_100000pop)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(TrafficCalming_per_100000pop)),
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
six_three = plot_grid(tc_pop_chloro, tc_pop_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))





###############################################################################
#       Standardised to PCT data(km cycled trhough borough) - Reds            # 
###############################################################################

#############
# ASL - PCT #
#############

# # create chloropleth
asl_pct_chloro = ggplot(chloropleth_dataset, 
                        aes(fill = ASL_per_mil_km_cycled)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       na.value = "transparent", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# # create Bar chart
asl_pct_bar = ggplot(chloropleth_dataset, 
                    aes(x = reorder(Borough_number, -ASL_per_mil_km_cycled), y = ASL_per_mil_km_cycled, 
                        fill = ASL_per_mil_km_cycled)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 140), expand = c(0,0), breaks = c(0, 70)) +
  geom_hline(aes(yintercept = mean(ASL_per_mil_km_cycled)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(ASL_per_mil_km_cycled)),
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
two_four = plot_grid(asl_pct_chloro, asl_pct_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))

##########################
# Crossings - PCT - reds #
##########################

# create chloropleth
cross_pct_chloro = ggplot(chloropleth_dataset, 
                        aes(fill = Crossings_per_mil_km_cycled)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       na.value = "transparent", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# # create Bar chart
cross_pct_bar = ggplot(chloropleth_dataset, 
                     aes(x = reorder(Borough_number, -Crossings_per_mil_km_cycled), y = Crossings_per_mil_km_cycled, 
                         fill = Crossings_per_mil_km_cycled)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 110), expand = c(0,0), breaks = c(0, 50)) +
  geom_hline(aes(yintercept = mean(Crossings_per_mil_km_cycled)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(Crossings_per_mil_km_cycled)),
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
three_four = plot_grid(cross_pct_chloro, cross_pct_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))


################################################
# Cycle lanes and Tracks (length) - PCT - reds #
################################################

# create chloropleth
clt_pct_chloro = ggplot(chloropleth_dataset, 
                          aes(fill = clt_pct_numeric)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       na.value = "transparent", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# # create Bar chart
clt_pct_bar = ggplot(chloropleth_dataset, 
                       aes(x = reorder(Borough_number, -clt_pct_numeric), y = clt_pct_numeric, 
                           fill = clt_pct_numeric)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 205), expand = c(0,0), breaks = c(0, 100)) +
  geom_hline(aes(yintercept = mean(clt_pct_numeric)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(clt_pct_numeric)),
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
four_four = plot_grid(clt_pct_chloro, clt_pct_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))


########################
# Signals - PCT - reds #
########################

# create chloropleth - 
sig_pct_chloro = ggplot() + 
  geom_sf(data = chloropleth_dataset, aes(fill = SignalsNA_per_mil_km_cycled), show.legend = F) +
    scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       na.value = "light grey", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create bar chart
sig_pct_bar = ggplot(chloropleth_dataset, 
                     aes(x = reorder(Borough_number, -Signals_per_mil_km_cycled), y = Signals_per_mil_km_cycled, 
                         fill =Signals_per_mil_km_cycled)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 15), expand = c(0,0), breaks = c(0, 7)) +
  geom_hline(aes(yintercept = mean(Signals_per_mil_km_cycled)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(Signals_per_mil_km_cycled)),
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
five_four = plot_grid(sig_pct_chloro, sig_pct_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))




# ################################
# # Traffic calming - PCT (reds) # 
# ################################

# # create chloropleth
tc_pct_chloro = ggplot() + 
  geom_sf(data = chloropleth_dataset, aes(fill = TrafficCalming_per_mil_km_cycled), show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       na.value = "light grey", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create bar chart
tc_pct_bar = ggplot(chloropleth_dataset, 
                     aes(x = reorder(Borough_number, -TrafficCalming_per_mil_km_cycled), y = TrafficCalming_per_mil_km_cycled, 
                         fill = TrafficCalming_per_mil_km_cycled)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 3200), expand = c(0,0), breaks = c(0, 1500)) +
  geom_hline(aes(yintercept = mean(TrafficCalming_per_mil_km_cycled)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(TrafficCalming_per_mil_km_cycled)),
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
six_four = plot_grid(tc_pct_chloro, tc_pct_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))


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














