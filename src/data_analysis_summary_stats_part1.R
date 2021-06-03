################################################################################
#                                                                              #
#                     Summary statistics for Paper 1                           #
#                                Part 1                                        #
#                     ##############################                           #
#                                                                              #
# This code generates the summary statistics used in Paper 1                   #
# 1) Calculate total number of assets and total length                         #
# 2) Calculate dates and year of surveying                                     #
# 3) URL NAs
# 4) Summary of 5 safety datasets (counts and % and then lengths % for CLT too)
# 5) Comparison of variables of on v off road infrastructure


# load packages
library(tidyverse)
library(sf)
library(summarytools)
library(units)

# load datasets
# These datasets were downloaded from TFL 25th February 2021 and data cleansed
c_asl = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_asl")
c_crossings = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_crossings")
c_cyclelanetrack = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_cycle_lane_track")
c_signals = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_signals")
c_trafficcalming = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_trafficcalming")
c_restrictedroutes = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_restricted_route")
c_restrictedpoints = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_restrictedpoints")
c_signage = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_signage")
c_parking = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/cleansed_parking")

# These datasets were created 2_3_2021 from TFL datasets downloaded 25/2/21
CID_borough_count = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/CID_count_by_borough")
CID_borough_length = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/CID_length_by_borough")

################################################################################
# 1) Calculate total number of assets and total length                         #

totals_counts = CID_borough_count %>%
  select(-c("BOROUGH"))
colSums(totals_counts)

totals_lengths = CID_borough_length %>%
  select(-c("BOROUGH"))
colSums(totals_lengths)

# get average crossing width
total_crossing_width = sum(st_length(c_crossings)) 
total_crossings = nrow(c_crossings)
total_crossing_width/total_crossings 
#10.00341 [m]

# get average asl length
total_asl_length = sum(st_length(c_asl)) 
total_asl = nrow(c_asl)
total_asl_length/total_asl
# 4.596594 [m]



################################################################################
# 2) Calculate dates/years of surveying

survey_dates = c_asl %>% st_drop_geometry() %>%
  select(c("FEATURE_ID", "SVDATE")) %>%
  rbind(c_crossings %>% st_drop_geometry() %>%
          select(c("FEATURE_ID", "SVDATE"))) %>%
  rbind(c_cyclelanetrack %>% st_drop_geometry() %>%
          select(c("FEATURE_ID", "SVDATE"))) %>%
  rbind(c_parking %>% st_drop_geometry() %>%
          select(c("FEATURE_ID", "SVDATE"))) %>%
  rbind(c_restrictedpoints %>% st_drop_geometry() %>%
          select(c("FEATURE_ID", "SVDATE"))) %>%
  rbind(c_restrictedroutes %>% st_drop_geometry() %>%
          select(c("FEATURE_ID", "SVDATE"))) %>%
  rbind(c_signage %>% st_drop_geometry() %>%
          select(c("FEATURE_ID", "SVDATE"))) %>%
  rbind(c_signals %>% st_drop_geometry() %>%
          select(c("FEATURE_ID", "SVDATE"))) %>%
  rbind(c_trafficcalming %>% st_drop_geometry() %>%
          select(c("FEATURE_ID", "SVDATE")))
dim(survey_dates)  # [1] 233951      2

# min/max survey date
min(survey_dates$SVDATE)
max(survey_dates$SVDATE)

# obtain year of survey
survey_dates = survey_dates %>%
  mutate(year = lubridate::year(SVDATE))

# remove observation with inappropriate year
survey_dates = survey_dates[survey_dates$FEATURE_ID != "RWG999394",]  # n = 233951

#summarise
survey_years = survey_dates %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  mutate(percentage = round((count/233950*100), digits = 2))
# year  count percentage
# <dbl>  <int>      <dbl>
# 1  2017 177593      75.9 
# 2  2018  56340      24.1 
# 3  2019     17       0.01


################################################################################
# 3) Examine URL NAs

URL_NAs = c_asl %>% st_drop_geometry() %>%
  select(c("FEATURE_ID", "PHOTO1_URL", "PHOTO2_URL")) %>%
  mutate(type = "asl") %>%
  rbind(c_crossings %>% st_drop_geometry() %>%
          select(c("FEATURE_ID","PHOTO1_URL", "PHOTO2_URL")) %>%
          mutate(type = "crossings")) %>%
  rbind(c_cyclelanetrack %>% st_drop_geometry() %>%
          select(c("FEATURE_ID","PHOTO1_URL", "PHOTO2_URL")) %>%
          mutate(type = "cyclelanetrack")) %>%
  rbind(c_parking %>% st_drop_geometry() %>%
          select(c("FEATURE_ID","PHOTO1_URL", "PHOTO2_URL")) %>%
          mutate(type = "parking")) %>%
  rbind(c_restrictedpoints %>% st_drop_geometry() %>%
          select(c("FEATURE_ID","PHOTO1_URL", "PHOTO2_URL")) %>%
          mutate(type = "restrictedpoints")) %>%
  rbind(c_restrictedroutes %>% st_drop_geometry() %>%
          select(c("FEATURE_ID","PHOTO1_URL", "PHOTO2_URL")) %>%
          mutate(type = "restrictedroutes")) %>%
  rbind(c_signage %>% st_drop_geometry() %>%
          select(c("FEATURE_ID","PHOTO1_URL", "PHOTO2_URL")) %>%
          mutate(type = "signage")) %>%
  rbind(c_signals %>% st_drop_geometry() %>%
          select(c("FEATURE_ID","PHOTO1_URL", "PHOTO2_URL")) %>%
          mutate(type = "signals")) %>%
  rbind(c_trafficcalming %>% st_drop_geometry() %>%
          select(c("FEATURE_ID","PHOTO1_URL", "PHOTO2_URL")) %>%
          mutate(type = "trafficcalming"))
dim(URL_NAs)  # [1] 233951      4

# drop observations that have same original FEATURE_ID (ie drop the ones I created by spatial splitting on boundaries)
original_URL_NAs = URL_NAs %>%
  filter(!str_detect(FEATURE_ID, "_ 2|_ 3|_2")) # n = 233588 observations
# ie the 233951 - the extra 3 crossings, 339 clt and 21 restricted routes

# Checking total of clt, rr and crossings created 
# crossings_created_n = c_crossings %>% st_drop_geometry() %>%
#   select(c("FEATURE_ID","PHOTO1_URL", "PHOTO2_URL")) %>%
#   filter(stringr::str_detect(FEATURE_ID, '_2')) # n = 3
# 
# clt_created_n = c_cyclelanetrack %>% st_drop_geometry() %>%
#   select(c("FEATURE_ID","PHOTO1_URL", "PHOTO2_URL")) %>%
#   filter(str_detect(FEATURE_ID, "_ 2|_ 3")) # n= 339
# 
# rr_created_n = c_restrictedroutes %>% st_drop_geometry() %>%
#   select(c("FEATURE_ID","PHOTO1_URL", "PHOTO2_URL")) %>%
#   filter(str_detect(FEATURE_ID, "_ 2|_ 3")) # n= 21

# calculate number of NAs by type
count_photo1 =  original_URL_NAs %>%
  group_by(type) %>%
  mutate(total = n()) %>%
  filter(str_detect(PHOTO1_URL, "no_asset_photo.png")) %>%
  mutate(photo_1_na_count = n()) %>%
  group_by(type)

totals = original_URL_NAs %>%
  group_by(type) %>%
  summarise(total = n())
photo_1_na_count = original_URL_NAs %>%
  group_by(type) %>%
  mutate(total = n()) %>%
  filter(str_detect(PHOTO1_URL, "no_asset_photo.png")) %>%
  summarise(photo_1_na_count = n()) 
photo_2_na_count = original_URL_NAs %>%
  group_by(type) %>%
  mutate(total = n()) %>%
  filter(str_detect(PHOTO2_URL, "no_asset_photo.png")) %>%
  summarise(photo_2_na_count = n()) 

summary_URL_NAs = left_join(totals, photo_1_na_count) %>%
  left_join(photo_2_na_count) %>%
  mutate(prop_P1_NA = round((photo_1_na_count/total*100), digit = 1)) %>%
  mutate(prop_P2_NA = round((photo_2_na_count/total*100), digit = 1)) %>%
  mutate(total_2 = 2*total) %>%
  mutate(overall_prop_NA = round(((photo_1_na_count + photo_2_na_count)/total_2 * 100), digit = 1))

# type          total photo_1_na_count photo_2_na_count prop_P1_NA prop_P2_NA total_2 overall_prop_NA
# <chr>         <int>            <int>            <int>      <dbl>      <dbl>   <dbl>           <dbl
# 1 asl            3775               48               51        1.3        1.4    7550             1.3
# 2 crossings      1687               31               32        1.8        1.9    3374             1.9
# 3 cyclelanetr…  24976              588              605        2.4        2.4   49952             2.4
# 4 parking       23758              299              298        1.3        1.3   47516             1.3
# 5 restrictedp…    180               12               12        6.7        6.7     360             6.7
# 6 restrictedr…   1378               71               52        5.2        3.8    2756             4.5
# 7 signage      118826             1347             1336        1.1        1.1  237652             1.1
# 8 signals         443                8                8        1.8        1.8     886             1.8
# 9 trafficcalm…  58565              805              810        1.4        1.4  117130             1.4



###############################################################################
# 4) Summary of 5 safety datasets

###### By count
c_asl %>%
  st_drop_geometry() %>%
  summarytools::dfSummary()

c_crossings %>%
  st_drop_geometry() %>%
  summarytools::dfSummary()

c_cyclelanetrack %>%
  st_drop_geometry() %>%
  select(-c("length_m", "length_km")) %>%
  summarytools::dfSummary()  # works out counts

c_signals %>%
  st_drop_geometry() %>%
  summarytools::dfSummary()

c_trafficcalming %>%
  st_drop_geometry() %>%
  summarytools::dfSummary()

##### By length for CLT
c_cyclelanetrack %>%
  st_drop_geometry() %>%
  group_by(CLT_CARR) %>%
  summarise(total_length = round(sum(length_km), digit = 1)) %>%
  mutate(percentage = round((total_length/2903.5*100), digit = 1))
  
c_cyclelanetrack %>%
    st_drop_geometry() %>%
    group_by(CLT_SEGREG) %>%
    summarise(total_length = round(sum(length_km), digit = 1)) %>%
    mutate(percentage = round((total_length/2903.5*100), digit = 1))
  
c_cyclelanetrack %>%
  st_drop_geometry() %>%
  group_by(CLT_STEPP) %>%
  summarise(total_length = round(sum(length_km), digit = 1)) %>%
  mutate(percentage = round((total_length/2903.5*100), digit = 1))

c_cyclelanetrack %>%
  st_drop_geometry() %>%
  group_by(CLT_PARSEG) %>%
  summarise(total_length = round(sum(length_km), digit = 1)) %>%
  mutate(percentage = round((total_length/2903.5*100), digit = 1))

c_cyclelanetrack %>%
  st_drop_geometry() %>%
  group_by(CLT_MANDAT) %>%
  summarise(total_length = round(sum(length_km), digit = 1)) %>%
  mutate(percentage = round((total_length/2903.5*100), digit = 1))

c_cyclelanetrack %>%
  st_drop_geometry() %>%
  group_by(CLT_ADVIS) %>%
  summarise(total_length = round(sum(length_km), digit = 1)) %>%
  mutate(percentage = round((total_length/2903.5*100), digit = 1))

c_cyclelanetrack %>%
  st_drop_geometry() %>%
  group_by(CLT_PRIORI) %>%
  summarise(total_length = round(sum(length_km), digit = 1)) %>%
  mutate(percentage = round((total_length/2903.5*100), digit = 1))

c_cyclelanetrack %>%
  st_drop_geometry() %>%
  group_by(CLT_CONTRA) %>%
  summarise(total_length = round(sum(length_km), digit = 1)) %>%
  mutate(percentage = round((total_length/2903.5*100), digit = 1))

c_cyclelanetrack %>%
  st_drop_geometry() %>%
  group_by(CLT_BIDIRE) %>%
  summarise(total_length = round(sum(length_km), digit = 1)) %>%
  mutate(percentage = round((total_length/2903.5*100), digit = 1))

c_cyclelanetrack %>%
  st_drop_geometry() %>%
  group_by(CLT_CBYPAS) %>%
  summarise(total_length = round(sum(length_km), digit = 1)) %>%
  mutate(percentage = round((total_length/2903.5*100), digit = 1))

c_cyclelanetrack %>%
  st_drop_geometry() %>%
  group_by(CLT_BBYPAS) %>%
  summarise(total_length = round(sum(length_km), digit = 1)) %>%
  mutate(percentage = round((total_length/2903.5*100), digit = 1))

c_cyclelanetrack %>%
  st_drop_geometry() %>%
  group_by(CLT_PARKR) %>%
  summarise(total_length = round(sum(length_km), digit = 1)) %>%
  mutate(percentage = round((total_length/2903.5*100), digit = 1))

c_cyclelanetrack %>%
  st_drop_geometry() %>%
  group_by(CLT_WATERR) %>%
  summarise(total_length = round(sum(length_km), digit = 1)) %>%
  mutate(percentage = round((total_length/2903.5*100), digit = 1))

c_cyclelanetrack %>%
  st_drop_geometry() %>%
  group_by(CLT_PTIME) %>%
  summarise(total_length = round(sum(length_km), digit = 1)) %>%
  mutate(percentage = round((total_length/2903.5*100), digit = 1))

c_cyclelanetrack %>%
  st_drop_geometry() %>%
  group_by(CLT_COLOUR) %>%
  summarise(total_length = round(sum(length_km), digit = 1)) %>%
  mutate(percentage = round((total_length/2903.5*100), digit = 1))
2903.5 - 2483.7 # 419.8
100 - 85.5 # 14.5


##### CLT counts for on and off road

c_cyclelanetrack %>%
  st_drop_geometry() %>%
  select(-c("length_m", "length_km")) %>%
  filter(CLT_CARR == TRUE) %>%
  summarytools::dfSummary()  # works out counts

c_cyclelanetrack %>%
  st_drop_geometry() %>%
  select(-c("length_m", "length_km")) %>%
  filter(CLT_CARR == FALSE) %>%
  summarytools::dfSummary()  # works out counts






################################################################################
#5) Comparison of variables of on v off road infrastructure
# including drawing bar charts


# create new variable which is clearly on/off road
clt_on_off = c_cyclelanetrack %>%
  st_drop_geometry() %>%
  mutate(on_off = case_when(CLT_CARR == 'TRUE' ~ "onroad", 
                            TRUE ~ "offroad")) 

# create multiple datasets that measure length pf variables by on/off road status
seg_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_SEGREG == TRUE) %>%
  summarise(segreg = sum(length_km)) %>%
  mutate(segreg_l_perc = drop_units(round((segreg/sum(segreg)*100), digit = 1)))
stepp_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_STEPP == TRUE) %>%
  summarise(stepp = sum(length_km)) %>%
  mutate(stepp_l_perc = drop_units(round((stepp/sum(stepp)*100), digit = 1)))
partseg_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_PARSEG == TRUE) %>%
  summarise(partsegreg = sum(length_km)) %>%
  mutate(partsegreg_l_perc = drop_units(round((partsegreg/sum(partsegreg)*100), digit = 1)))
shared_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_SHARED == TRUE) %>%
  summarise(shared = sum(length_km)) %>%
  mutate(shared_l_perc = drop_units(round((shared/sum(shared)*100), digit = 1)))
mandat_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_MANDAT == TRUE) %>%
  summarise(mandat = sum(length_km)) %>%
  mutate(mandat_l_perc = drop_units(round((mandat/sum(mandat)*100), digit = 1)))
advis_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_ADVIS == TRUE) %>%
  summarise(advis = sum(length_km)) %>%
  mutate(advis_l_perc = drop_units(round((advis/sum(advis)*100), digit = 1)))
priority_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_PRIORI == TRUE) %>%
  summarise(priority = sum(length_km)) %>%
  mutate(priority_l_perc = drop_units(round((priority/sum(priority)*100), digit = 1)))
contra_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_CONTRA == TRUE) %>%
  summarise(contra = sum(length_km)) %>%
  mutate(contra_l_perc = drop_units(round((contra/sum(contra)*100), digit = 1)))
bidir_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_BIDIRE == TRUE) %>%
  summarise(bidir = sum(length_km)) %>%
  mutate(bidir_l_perc = drop_units(round((bidir/sum(bidir)*100), digit = 1)))
bypass_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_CBYPAS == TRUE) %>%
  summarise(bypass = sum(length_km)) %>%
  mutate(bypass_l_perc = drop_units(round((bypass/sum(bypass)*100), digit = 1)))
busbypass_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_BBYPAS == TRUE) %>%
  summarise(busbypass = sum(length_km)) %>%
  mutate(busbypass_l_perc = drop_units(round((busbypass/sum(busbypass)*100), digit = 1)))
park_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_PARKR == TRUE) %>%
  summarise(park = sum(length_km)) %>%
  mutate(park_l_perc = drop_units(round((park/sum(park)*100), digit = 1)))
water_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_WATERR == TRUE) %>%
  summarise(water = sum(length_km)) %>%
  mutate(water_l_perc = drop_units(round((water/sum(water)*100), digit = 1)))
parttime_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_PTIME == TRUE) %>%
  summarise(parttime = sum(length_km)) %>%
  mutate(parttime_l_perc = drop_units(round((parttime/sum(parttime)*100), digit = 1)))
colour_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_COLOUR != "NONE") %>%
  summarise(colour = sum(length_km)) %>%
  mutate(colour_l_perc = drop_units(round((colour/sum(colour)*100), digit = 1)))

# join these datasets together to get summary of on/off road comparison of lengths
on_off_clt_comparison_lengths = plyr::join_all(
  list(colour_l, parttime_l, water_l, park_l, busbypass_l, bypass_l, bidir_l, contra_l, 
       priority_l, advis_l, mandat_l, shared_l, partseg_l, stepp_l, seg_l),
  by = 'on_off', type = 'left')
  
rm(colour_l, parttime_l, water_l, park_l, busbypass_l, bypass_l, bidir_l, contra_l, 
   priority_l, advis_l, mandat_l, shared_l, partseg_l, stepp_l, seg_l)

# convert NAs to 0 (there are no onroad lanes that are by water)
on_off_clt_comparison_lengths$water[is.na(on_off_clt_comparison_lengths$water)] = 0 
on_off_clt_comparison_lengths$water_l_perc[is.na(on_off_clt_comparison_lengths$water_l_perc)] = 0 

# get in format suitable for ggplot 
perc = on_off_clt_comparison_lengths %>%
  pivot_longer(cols = c(colour_l_perc, parttime_l_perc, water_l_perc, park_l_perc, busbypass_l_perc, bypass_l_perc, 
                        bidir_l_perc, contra_l_perc, priority_l_perc, advis_l_perc, mandat_l_perc, shared_l_perc, 
                        partsegreg_l_perc, stepp_l_perc, segreg_l_perc), 
               names_to = c('characteristic', '.value'),
               names_sep = "\\_l_") %>%
  select(c("on_off", "characteristic", "perc"))
length = on_off_clt_comparison_lengths %>%
  pivot_longer(cols = c("colour", "parttime", "water", "park", "busbypass", "bypass", 
                               "bidir", "contra", "priority", "advis", "mandat", "shared", 
                               "partsegreg", "stepp", "segreg"), 
               names_to = 'characteristic', 
               values_to = "length") %>%
  mutate(round_length = drop_units(round(length, digit = 1))) %>%
  select(c("on_off", "characteristic", "length", "round_length"))

# join together
on_off_comparison_lengths4ggplot = left_join(length, perc)

# Below creates new column that we use to order the bars and gives sensible variable labels
on_off_comparison_lengths4ggplot_order = on_off_comparison_lengths4ggplot %>%
  mutate(variable_order = factor(characteristic,
                                 levels = c("colour", "parttime", "water", "park",
                                            "busbypass", "bypass","bidir", "contra",
                                            "priority", "advis","mandat", "shared",
                                            "partsegreg", "stepp", "segreg"),
                                 labels = c("Coloured tarmac", "Part-time", "Water route", "Park route",
                                            "Continuous facilities through bus stop", "Cycle bypass", "Bidirectional", "Contraflow",
                                            "Given Priority", "Advisory cycle lane","Mandatory cycle lane", 
                                            "Shared cycle lane", "Part-segregated", "Stepped", "Fully-segregated"))) %>%
  mutate(on_off_order = factor(on_off,
                                 levels = c("offroad", "onroad"), 
                                 labels = c("Off-road", "On-road"))) # rename columns for legend label



# Stacked bar chart of % of characteristics on/off road
ggplot() +
  geom_bar(data = on_off_comparison_lengths4ggplot_order,
           aes(x = perc, y = variable_order, fill = on_off_order), stat = "identity") +
  scale_fill_manual(values = c("#993404", "#969696")) +
  labs(title = "Comparison of characteristics of cycle lanes and tracks") +
  xlab(label = " Percentage of length") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        panel.grid = element_blank(), # removes y axis lines
        legend.position = "none") 
# save with width at 439

# create stacked bar chart of proportion
ggplot() +
  geom_bar(data = on_off_comparison_lengths4ggplot_order,
           aes(x = round_length, y = variable_order, fill = on_off_order), stat = "identity") +
  scale_fill_manual(values = c("#993404", "#969696")) +
  labs(title = "") +
  xlab(label = "Length in km") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(), 
        panel.grid = element_blank(),  # removes all grid lines
        axis.line.x = element_line(size=0.1, color="black"),
        legend.title = element_blank(), # adds axis line back in
        legend.text = element_text(size = 12)) +
  scale_x_continuous(expand = c(0,0), breaks = c(0, 1000, 2000), limits = c(0, 2050))   
# save with width at 439




# ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -Borough_Area_km2_no_units), 
#                                            y = Borough_Area_km2_no_units, fill = area_group)) +
#   geom_bar(stat = "identity", color = "black", size = 0.1) +  # adds borders to bars
#   coord_flip() +
#   labs(y = "Area (km^2)", x = NULL) +
#   theme_classic() + 
#   scale_y_continuous(limits = c(0, 160), expand = c(0,0),
#                      breaks = c(0, 100)) +  
#   scale_fill_manual(values = area_colours) +
#   theme(axis.line.y = element_blank(), 
#         axis.ticks.y = element_blank(),
#         axis.line.x = element_blank(),
#         legend.position = "none")



# test1$length =  round(test1$length)
# ggplot(data = test1) +
#   geom_bar(aes(x = perc, y = characteristic, fill = on_off), stat = "identity") +
#   geom_text(aes(label = length))
# 
# ggplot(data = test1, aes(x = perc, y = characteristic, fill = on_off, label = length)) +
#   geom_bar(stat = "identity") +
#   geom_text()
# 
#   scale_fill_manual(values = c("#993404", "#969696")) +
#   labs(title = "Comparison of characteristics of on-road cycle lanes and off-road cycle tracks") +
#   xlab(label = " Percentage of length") +
#   theme_minimal() +
#   theme(axis.title.y = element_blank())



# #options(scipen = 999) # this should work automatically now
# testz = on_off_clt_comparison_lengths %>%
#   select(c("on_off", "colour", "parttime", "water", "park",
#            "busbypass", "bypass","bidir", "contra",
#            "priority", "advis","mandat", "shared",
#            "partsegreg", "stepp", "segreg"))
# 
# testz = drop_units(testz)
# testz = 


#  BELOW CODE DOES SAME FOR CLT BUT FOR COUNTS RATHER THAN LENGTHS 
# seg_c = clt_on_off %>%
#   group_by(on_off) %>%
#   filter(CLT_SEGREG == TRUE) %>%
#   summarise(segreg = n()) %>%
#   mutate(seg_perc = round((segreg/sum(segreg)*100), digit = 1))
# stepp_c = clt_on_off %>%
#   group_by(on_off) %>%
#   filter(CLT_STEPP == TRUE) %>%
#   summarise(stepp = n()) %>%
#   mutate(stepp_perc = round((stepp/sum(stepp)*100), digit = 1))
# partseg_c = clt_on_off %>%
#   group_by(on_off) %>%
#   filter(CLT_PARSEG == TRUE) %>%
#   summarise(partsegreg = n()) %>%
#   mutate(partseg_perc = round((partsegreg/sum(partsegreg)*100), digit = 1))
# shared_c = clt_on_off %>%
#   group_by(on_off) %>%
#   filter(CLT_SHARED == TRUE) %>%
#   summarise(shared = n()) %>%
#   mutate(shared_perc = round((shared/sum(shared)*100), digit = 1))
# mandat_c = clt_on_off %>%
#   group_by(on_off) %>%
#   filter(CLT_MANDAT == TRUE) %>%
#   summarise(mandat = n()) %>%
#   mutate(mandat_perc = round((mandat/sum(mandat)*100), digit = 1))
# advis_c = clt_on_off %>%
#   group_by(on_off) %>%
#   filter(CLT_ADVIS == TRUE) %>%
#   summarise(advis = n()) %>%
#   mutate(advis_perc = round((advis/sum(advis)*100), digit = 1))
# priority_c = clt_on_off %>%
#   group_by(on_off) %>%
#   filter(CLT_PRIORI == TRUE) %>%
#   summarise(priority = n()) %>%
#   mutate(priority_perc = round((priority/sum(priority)*100), digit = 1))
# contra_c = clt_on_off %>%
#   group_by(on_off) %>%
#   filter(CLT_CONTRA == TRUE) %>%
#   summarise(contra = n()) %>%
#   mutate(contra_perc = round((contra/sum(contra)*100), digit = 1))
# bidir_c = clt_on_off %>%
#   group_by(on_off) %>%
#   filter(CLT_BIDIRE == TRUE) %>%
#   summarise(bidir = n()) %>%
#   mutate(bidir_perc = round((bidir/sum(bidir)*100), digit = 1))
# bypass_c = clt_on_off %>%
#   group_by(on_off) %>%
#   filter(CLT_CBYPAS == TRUE) %>%
#   summarise(bypass = n()) %>%
#   mutate(bypass_perc = round((bypass/sum(bypass)*100), digit = 1))
# busbypass_c = clt_on_off %>%
#   group_by(on_off) %>%
#   filter(CLT_BBYPAS == TRUE) %>%
#   summarise(busbypass = n()) %>%
#   mutate(busbypass_perc = round((busbypass/sum(busbypass)*100), digit = 1))
# park_c = clt_on_off %>%
#   group_by(on_off) %>%
#   filter(CLT_PARKR == TRUE) %>%
#   summarise(park = n()) %>%
#   mutate(park_perc = round((park/sum(park)*100), digit = 1))
# water_c = clt_on_off %>%
#   group_by(on_off) %>%
#   filter(CLT_WATERR == TRUE) %>%
#   summarise(water = n()) %>%
#   mutate(water_perc = round((water/sum(water)*100), digit = 1))
# parttime_c = clt_on_off %>%
#   group_by(on_off) %>%
#   filter(CLT_PTIME == TRUE) %>%
#   summarise(parttime = n()) %>%
#   mutate(parttime_perc = round((parttime/sum(parttime)*100), digit = 1))
# colour_c = clt_on_off %>%
#   group_by(on_off) %>%
#   filter(CLT_COLOUR != "NONE") %>%
#   summarise(colour = n()) %>%
#   mutate(colour_perc = round((colour/sum(colour)*100), digit = 1))
# 
# # join these datasets together to get summary of on/off road comparison of counts
# on_off_clt_comparison_counts = plyr::join_all(
#   list(colour_c, parttime_c, water_c, park_c, busbypass_c, bypass_c, bidir_c, contra_c, 
#        priority_c, advis_c, mandat_c, shared_c, partseg_c, stepp_c, seg_c),
#   by = 'on_off', type = 'left')
# 
# rm(colour_c, parttime_c, water_c, park_c, busbypass_c, bypass_c, bidir_c, contra_c, 
#    priority_c, advis_c, mandat_c, shared_c, partseg_c, stepp_c, seg_c)
# 
# 
# # convert NAs to 0 (there are no onroad lanes that are by water)
# on_off_clt_comparison_counts$water[is.na(on_off_clt_comparison_counts$water)] = 0 
# on_off_clt_comparison_counts$water_perc[is.na(on_off_clt_comparison_counts$water_perc)] = 0 
# #    on_off colour colour_perc parttime parttime_perc water water_perc park
# # 1 offroad   1853        29.9      492          17.6   611        100 4086
# # 2  onroad   4338        70.1     2308          82.4     0          0  108
# #   park_perc busbypass busbypass_perc bypass bypass_perc bidir bidir_perc contra
# # 1      97.4        64           48.5     58        92.1 10051       96.3     30
# # 2       2.6        68           51.5      5         7.9   381        3.7   1463
# #   contra_perc priority priority_perc advis advis_perc mandat mandat_perc shared
# # 1           2        1             0     4        0.1      3         0.2   7546
# # 2          98     2285           100  7273       99.9   1854        99.8   2845
# #   shared_perc partsegreg partseg_perc stepp stepp_perc segreg seg_perc
# # 1        72.6       3234         90.3    10        9.6    560       29
# # 2        27.4        349          9.7    94       90.4   1371       71
# 
# # get in format suitable for ggplot 
# on_off_comparison_counts4ggplot = on_off_clt_comparison_counts %>%
#   pivot_longer(cols = c(colour_perc, parttime_perc, water_perc, park_perc, busbypass_perc, bypass_perc, bidir_perc, contra_perc, 
#                         priority_perc, advis_perc, mandat_perc, shared_perc, partseg_perc, stepp_perc, seg_perc), 
#                names_to = c('variable', '.value'),
#                names_sep = "\\_")
# ggplot() +
#   geom_bar(data = on_off_comparison_counts4ggplot, 
#            aes(x = perc, y = variable, fill = on_off), stat = "identity") # this does basic plot - will need tidying
# 
# 
# # Below creates new column that we use to order the bars - may need to add rename list too too
# on_off_comparison_counts4ggplot_order = on_off_comparison_counts4ggplot %>% 
#   mutate(variable_order = factor(variable, 
#                                  levels = c("colour", "parttime", "water", "park", 
#                                             "busbypass", "bypass","bidir", "contra",
#                                             "priority", "advis","mandat", "shared",
#                                             "partseg", "stepp", "seg")))
# ggplot() +
#   geom_bar(data = on_off_comparison_counts4ggplot_order, 
#            aes(x = perc, y = variable_order, fill = on_off), stat = "identity") +
#   scale_fill_manual(values = c("#993404", "#969696")) +
#   labs(title = "Percentage of counts of assets")# this then plots in a much more sensible order
