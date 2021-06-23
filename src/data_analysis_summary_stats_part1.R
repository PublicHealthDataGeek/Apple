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
library(ggpmisc) # adding tables to ggplot


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

# get mean asl length
total_asl_length = sum(st_length(c_asl)) 
total_asl = nrow(c_asl)
total_asl_length/total_asl
# 4.596594 [m]

# get mean crossing width
total_crossing_width = sum(st_length(c_crossings)) 
total_crossings = nrow(c_crossings)
total_crossing_width/total_crossings 
#10.00341 [m]

# get mean clt length
total_clt_length = sum(st_length(c_cyclelanetrack)) 
total_clt = nrow(c_cyclelanetrack)
total_clt_length/total_clt
# 114.6985 [m]

# get mean rr length
total_rr_length = sum(st_length(c_restrictedroutes)) 
total_rr = nrow(c_restrictedroutes)
total_rr_length/total_rr
# 219.491 [m]

# total number of assets
nrow(c_asl) + nrow(c_crossings) + nrow(c_cyclelanetrack) + nrow(c_parking) +
  nrow(c_restrictedpoints) + nrow(c_restrictedroutes) + nrow(c_signage) +
  nrow(c_signals) + nrow(c_trafficcalming)
# n = 234251


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
dim(survey_dates)  # [1] 234251      2

# min/max survey date
min(survey_dates$SVDATE) #[1] "2017-01-06"
max(survey_dates$SVDATE) # [1] "6482-04-01"

new_survey_dates = survey_dates %>%
  filter(SVDATE != "6482-04-01")
max(new_survey_dates$SVDATE) #[1] "2019-09-02"


# obtain year of survey
new_survey_dates = new_survey_dates %>%
  mutate(year = lubridate::year(SVDATE))

#summarise
survey_years = new_survey_dates %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  mutate(percentage = round((count/nrow(new_survey_dates)*100), digits = 2))
# year  count percentage
# <dbl>  <int>      <dbl>
# 2017 177692      75.9 
# 2018  56541      24.1 
# 2019     17       0.01


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
dim(URL_NAs)  # [1] 234251     4

# drop observations that have same original FEATURE_ID (ie drop the ones I created by spatial splitting on boundaries)
original_URL_NAs = URL_NAs %>%
  filter(!str_detect(FEATURE_ID, "_ 2|_ 3|_2|_3|_4")) # n = 233588 observations
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








##### create summary histograms
# write function to create summary
my.summary <- function(x,...){
  c(Mean = mean(x, ...),
    SD = sd(x, ...),
    Median = median(x, ...),
    Min = min(x, ...),
    Max = max(x,...))
}

my.summary2 <- function(x,...){
  c(Mean = paste0(round(mean(x, ...), digits = 1), " m"),
    SD = paste0(round(sd(x, ...), digits = 1), " m"),
    Median = paste0(round(median(x, ...), digits = 1), " m"),
    IQR = paste0(round((quantile(x, 0.25)), digits = 1), " - ", 
                 round((quantile(x, 0.75)), digits = 1), " m"),
    Min = paste0(round(min(x, ...), digits = 1), " m"),
    Max = paste0(round(max(x,...), digits = 1), " m"))
}

my.summary_clt <- function(x,...){
  c(Mean = paste0(round(mean(x, ...), digits = 1), " m"),
    SD = paste0(round(sd(x, ...), digits = 1), " m"),
    Median = paste0(round(median(x, ...), digits = 1), " m"),
    IQR = paste0(round((quantile(x, 0.25)), digits = 1), " - ", 
                 round((quantile(x, 0.75)), digits = 1), " m"),
    Min = paste0(round(min(x, ...), digits = 2), " m"),
    Max = paste0(round(max(x,...), digits = 1), " m"))
}

# sets units so that [] are removed around m units for ggplots
units_options(group = c("", "")) 

#### code to obtain original usmmary statistics 
# library(CycleInfraLnd)
# asl = st_transform(get_cid_lines(type = "advanced_stop_line"), crs = 27700)
# crossings = st_transform(get_cid_lines(type = "crossing"), crs = 27700)
# cycle_lane_track = st_transform(get_cid_lines(type = "cycle_lane_track"), crs = 27700)
# summary(st_length(asl))
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# # 1.222   3.897   4.503   4.597   5.082  20.633 
# summary(st_length(crossings))
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# # 0.7531  7.3700  9.8495 11.8003 14.0031 76.8535 
# summary(st_length(cycle_lane_track))
# # Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# # 0.28    16.05    44.08   116.35   108.71 19814.96 




# 1) ASL
asl_hist_df = c_asl %>%
  mutate(length = drop_units(st_length(geometry))) %>%
  st_drop_geometry()

asl_summ = data.frame(
  Measure = c("Mean", "SD", "Median", "Min", "Max"),
  Values = set_units(round(my.summary(asl_hist_df$length), digits = 1), m)
)

asl_summ2 = data.frame(
  Measure = c("Mean", "SD", "Median","IQR", "Min", "Max"),
  Values = my.summary2(asl_hist_df$length))

asl_df <- tibble(x = 15, y = 1000, tb = list(asl_summ))
asl_df2 <- tibble(x = 15, y = 1000, tb = list(asl_summ2))

ggplot(asl_hist_df) + 
  geom_histogram(aes(x = length), fill = "grey", color = "black") + 
  theme_classic() +
  labs(y = "Count", x = "Length (m)") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_table(data = asl_df, aes(x = x, y = y, label = tb), 
             table.colnames = FALSE, table.theme = ttheme_gtminimal)
# using my.summary2 function
ggplot(asl_hist_df) + 
  geom_histogram(aes(x = length), fill = "grey", color = "black") + 
  theme_classic() +
  labs(y = "Count", x = "Length (m)") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_table(data = asl_df2, aes(x = x, y = y, label = tb), 
             table.colnames = FALSE, table.theme = ttheme_gtminimal)


# or boxplot
asl_box_df <- tibble(x = 15, y = 0.4, tb = list(asl_summ))
ggplot(asl_hist_df) + 
  geom_boxplot(aes(x = length), fill = "grey", color = "black") + 
  theme_classic() +
  labs(x = "Length (m)") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_table(data = asl_box_df, aes(x = x, y = y, label = tb), 
             table.colnames = FALSE, table.theme = ttheme_gtminimal)

# 2) Crossings
crossing_hist_df = c_crossings %>%
  mutate(length = drop_units(st_length(geometry))) %>%
  st_drop_geometry()

cross_summ = data.frame(
  Measure = c("Mean", "SD", "Median", "Min", "Max"),
  Values = set_units(round(my.summary(crossing_hist_df$length), digits = 1), m)
)
cross_summ2 = data.frame(
  Measure = c("Mean", "SD", "Median","IQR", "Min", "Max"),
  Values = my.summary2(crossing_hist_df$length))

cross_df <- tibble(x = 60, y = 600, tb = list(cross_summ))
cross_df2 <- tibble(x = 60, y = 600, tb = list(cross_summ2))

ggplot(crossing_hist_df) + 
  geom_histogram(aes(x = length), fill = "grey", color = "black") + 
  theme_classic() +
  labs(y = "Count", x = "Width (m)") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_table(data = cross_df, aes(x = x, y = y, label = tb), 
             table.colnames = FALSE, table.theme = ttheme_gtminimal)
# using my.summary2: 
ggplot(crossing_hist_df) + 
  geom_histogram(aes(x = length), fill = "grey", color = "black") + 
  theme_classic() +
  labs(y = "Count", x = "Width (m)") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_table(data = cross_df2, aes(x = x, y = y, label = tb), 
             table.colnames = FALSE, table.theme = ttheme_gtminimal)

# 3) Cycle lanes and tracks
clt_hist_df = c_cyclelanetrack %>%
  mutate(length = drop_units(st_length(geometry))) %>%
  st_drop_geometry()

clt_summ = data.frame(
  Measure = c("Mean", "SD", "Median", "Min", "Max"),
  Values = set_units(round(my.summary(clt_hist_df$length), digits = 1), m)
)

clt_summ2 = data.frame(
  Measure = c("Mean", "SD", "Median","IQR", "Min", "Max"),
  Values = my.summary_clt(clt_hist_df$length))

clt_df <- tibble(x = 15000, y = 16000, tb = list(clt_summ))
clt_df2 <- tibble(x = 15000, y = 16000, tb = list(clt_summ2))

ggplot(clt_hist_df) + 
  geom_histogram(aes(x = length), fill = "grey", color = "black", binwidth = 250) + 
  theme_classic() +
  labs(y = "Count", x = "Length (m)") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_table(data = clt_df, aes(x = x, y = y, label = tb), 
             table.colnames = FALSE, table.theme = ttheme_gtminimal)

# using my.summary_clt
ggplot(clt_hist_df) + 
  geom_histogram(aes(x = length), fill = "grey", color = "black", binwidth = 250) + 
  theme_classic() +
  labs(y = "Count", x = "Length (m)") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_table(data = clt_df2, aes(x = x, y = y, label = tb), 
             table.colnames = FALSE, table.theme = ttheme_gtminimal)



###### by length for asl
#  calculate total length
asl_length = sum(st_length(c_asl))

length_no_characteristics = c_asl %>%
  filter(ASL_FDR == FALSE & ASL_FDRLFT == FALSE & ASL_FDCENT == FALSE & ASL_FDRIGH == FALSE &
           ASL_SHARED == FALSE & ASL_COLOUR == "NONE") %>%
  mutate(length = st_length(geometry)) %>%
  summarise(total_length = round(sum(length), digit = 1)) %>%
  mutate(percentage = round((total_length/asl_length*100), digit = 1))
length_no_characteristics$total_length/
  nrow(c_asl %>%
         filter(ASL_FDR == FALSE & ASL_FDRLFT == FALSE & ASL_FDCENT == FALSE & ASL_FDRIGH == FALSE &
                  ASL_SHARED == FALSE & ASL_COLOUR == "NONE"))
# 4.703635 [m]

length_feed_asl = c_asl %>%
  group_by(ASL_FDR) %>%
  mutate(length = st_length(geometry)) %>%
  summarise(total_length = round(sum(length), digit = 1)) %>%
  mutate(percentage = round((total_length/asl_length*100), digit = 1)) %>%
  filter(ASL_FDR == TRUE)
length_feed_asl$total_length/
  nrow(c_asl %>%
         filter(ASL_FDR == TRUE))
# 4.497813 [m]

length_left_asl = c_asl %>%
  group_by(ASL_FDRLFT) %>%
  mutate(length = st_length(geometry)) %>%
  summarise(total_length = round(sum(length), digit = 1)) %>%
  mutate(percentage = round((total_length/asl_length*100), digit = 1)) %>%
  filter(ASL_FDRLFT == TRUE)
length_feed_asl$total_length/
  nrow(c_asl %>%
         filter(ASL_FDRLFT == TRUE))
# 4.731327 [m]

length_centre_asl = c_asl %>%
  group_by(ASL_FDCENT) %>%
  mutate(length = st_length(geometry)) %>%
  summarise(total_length = round(sum(length), digit = 1)) %>%
  mutate(percentage = round((total_length/asl_length*100), digit = 1)) %>%
  filter(ASL_FDCENT == TRUE)
length_centre_asl$total_length/
  nrow(c_asl %>%
         filter(ASL_FDCENT == TRUE))
# 4.367949 [m]

length_right_asl = c_asl %>%
  group_by(ASL_FDRIGH) %>%
  mutate(length = st_length(geometry)) %>%
  summarise(total_length = round(sum(length), digit = 1)) %>%
  mutate(percentage = round((total_length/asl_length*100), digit = 1)) %>%
  filter(ASL_FDRIGH == TRUE)
length_right_asl$total_length/
  nrow(c_asl %>%
         filter(ASL_FDRIGH == TRUE))
# 4.744444 [m]

length_shared_asl = c_asl %>%
  group_by(ASL_SHARED) %>%
  mutate(length = st_length(geometry)) %>%
  summarise(total_length = round(sum(length), digit = 1)) %>%
  mutate(percentage = round((total_length/asl_length*100), digit = 1)) %>%
  filter(ASL_SHARED == TRUE)
length_shared_asl$total_length/
  nrow(c_asl %>%
         filter(ASL_SHARED == TRUE))
# 4.9 [m]

length_coloured_asl = c_asl %>%
  filter(ASL_COLOUR != "NONE") %>%
  mutate(length = st_length(geometry)) %>%
  summarise(total_length = round(sum(length), digit = 1)) %>%
  mutate(percentage = round((total_length/asl_length*100), digit = 1))
length_coloured_asl$total_length/
  nrow(c_asl %>%
         filter(ASL_COLOUR != "NONE"))
# 4.531168 [m]




##### By width for Crossings
crossings_width = sum(st_length(c_crossings))

width_no_characteristics = c_crossings %>%
  filter(CRS_SIGNAL == FALSE & CRS_SEGREG == FALSE & CRS_CYGAP == FALSE & CRS_PEDEST == FALSE &
           CRS_LEVEL == FALSE) %>%
  mutate(width = st_length(geometry)) %>%
  summarise(total_width = round(sum(width), digit = 1)) %>%
  mutate(percentage = round((total_width/crossings_width*100), digit = 1))
width_no_characteristics$total_width/
  nrow(c_crossings %>%
         filter(CRS_SIGNAL == FALSE & CRS_SEGREG == FALSE & CRS_CYGAP == FALSE & CRS_PEDEST == FALSE &
                  CRS_LEVEL == FALSE))
# 9.313839 [m]


width_signalled_crossings = c_crossings %>%
  group_by(CRS_SIGNAL) %>%
  mutate(width = st_length(geometry)) %>%
  summarise(total_width = round(sum(width), digit = 1)) %>%
  mutate(percentage = round((total_width/crossings_width*100), digit = 1)) %>%
  filter(CRS_SIGNAL == TRUE)
width_signalled_crossings$total_width/
  nrow(c_crossings %>%
         filter(CRS_SIGNAL == TRUE))
# 10.14757 [m]

width_seg_crossings = c_crossings %>%
  group_by(CRS_SEGREG) %>%
  mutate(width = st_length(geometry)) %>%
  summarise(total_width = round(sum(width), digit = 1)) %>%
  mutate(percentage = round((total_width/crossings_width*100), digit = 1)) %>%
  filter(CRS_SEGREG == TRUE)
width_seg_crossings$total_width/
  nrow(c_crossings %>%
         filter(CRS_SEGREG == TRUE))
# 11.38314 [m]

width_cycgap_crossings = c_crossings %>%
  group_by(CRS_CYGAP) %>%
  mutate(width = st_length(geometry)) %>%
  summarise(total_width = round(sum(width), digit = 1)) %>%
  mutate(percentage = round((total_width/crossings_width*100), digit = 1)) %>%
  filter(CRS_CYGAP == TRUE)
width_cycgap_crossings$total_width/
  nrow(c_crossings %>%
         filter(CRS_CYGAP == TRUE))
# 12.16866 [m]

width_pedonly_crossings = c_crossings %>%
  group_by(CRS_PEDEST) %>%
  mutate(width = st_length(geometry)) %>%
  summarise(total_width = round(sum(width), digit = 1)) %>%
  mutate(percentage = round((total_width/crossings_width*100), digit = 1)) %>%
  filter(CRS_PEDEST == TRUE)
width_pedonly_crossings$total_width/
  nrow(c_crossings %>%
         filter(CRS_PEDEST == TRUE))
# 8.516667 [m]

width_level_crossings = c_crossings %>%
  group_by(CRS_LEVEL) %>%
  mutate(width = st_length(geometry)) %>%
  summarise(total_width = round(sum(width), digit = 1)) %>%
  mutate(percentage = round((total_width/crossings_width*100), digit = 1)) %>%
  filter(CRS_LEVEL == TRUE)
width_pedonly_crossings$total_width/
  nrow(c_crossings %>%
         filter(CRS_LEVEL == TRUE))
#19.46667 [m]


##### By length for CLT
# length_no_characteristics = c_cyclelanetrack %>%
#   filter(CLT_CARR == FALSE & CLT_SEGREG == FALSE & CLT_STEPP == FALSE & 
#            CLT_PARSEG == FALSE & CLT_SHARED == FALSE & CLT_MANDAT == FALSE &
#            CLT_ADVIS == FALSE & CLT_PRIORI == FALSE & CLT_CONTRA == FALSE &
#            CLT_BIDIRE == FALSE & CLT_CBYPAS == FALSE & CLT_BBYPAS == FALSE &
#            CLT_PARKR == FALSE & CLT_WATERR == FALSE & CLT_PTIME == FALSE &
#            CLT_COLOUR == "NONE") %>%
#   mutate(length = st_length(geometry)) %>%
#   summarise(total_length = round(sum(length), digit = 1)) %>%
#   mutate(percentage = round((total_length/asl_length*100), digit = 1))
# length_no_characteristics$total_length/
#   nrow(c_asl %>%
#          filter(ASL_FDR == FALSE & ASL_FDRLFT == FALSE & ASL_FDCENT == FALSE & ASL_FDRIGH == FALSE &
#                   ASL_SHARED == FALSE & ASL_COLOUR == "NONE"))


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
#
# 5) Create visualisations of the characteristics - bar charts for count/% and 
# density plots for length - this is to replace a table with the actual figures


# create dataframes for count % comparison (insread of table)
# 1) ASL
asl_charac = c_asl %>%
  st_drop_geometry() %>%
  select(contains("ASL")) %>%
  mutate(ASL_COLOUR_F = case_when(ASL_COLOUR == "NONE" ~ "FALSE", 
                                  TRUE ~ "TRUE")) %>%
  select(-c(ASL_COLOUR))

ASL_FDR = asl_charac$ASL_FDR %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(ASL_FDR == "TRUE") %>%
  rename(charac = ASL_FDR)
ASL_FDR[1] <- "ASL_FDR"

ASL_FDRLFT = asl_charac$ASL_FDRLFT %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(ASL_FDRLFT == "TRUE") %>%
  rename(charac = ASL_FDRLFT)
ASL_FDRLFT[1] <- "ASL_FDRLFT"

ASL_FDCENT = asl_charac$ASL_FDCENT %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(ASL_FDCENT == "TRUE") %>%
  rename(charac = ASL_FDCENT)
ASL_FDCENT[1] <- "ASL_FDCENT"

ASL_FDRIGH = asl_charac$ASL_FDRIGH %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(ASL_FDRIGH == "TRUE") %>%
  rename(charac = ASL_FDRIGH)
ASL_FDRIGH[1] <- "ASL_FDRIGH"

ASL_SHARED = asl_charac$ASL_SHARED %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(ASL_SHARED == "TRUE") %>%
  rename(charac = ASL_SHARED)
ASL_SHARED[1] <- "ASL_SHARED"

ASL_COLOUR_F = asl_charac$ASL_COLOUR_F %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(ASL_COLOUR_F == "TRUE") %>%
  rename(charac = ASL_COLOUR_F)
ASL_COLOUR_F[1] <- "ASL_COLOUR_F"

ASL_NIL = c("ASL_NIL", 1568, 41.5)

ASL = rbind(ASL_FDR, ASL_NIL,ASL_FDRLFT, ASL_COLOUR_F, ASL_FDCENT, ASL_FDRIGH, ASL_SHARED) %>%
  mutate(pct = round(as.numeric(pct), digits = 1)) %>%
  mutate(Percentage = paste0(pct, "%")) %>%
  mutate(Count = as.numeric(freq)) %>%
  mutate(charac = factor(charac,
                         levels = c("ASL_SHARED", "ASL_FDRIGH","ASL_FDCENT", "ASL_COLOUR_F", 
                                    "ASL_NIL", "ASL_FDRLFT", "ASL_FDR"),
                         labels = c("Shared e.g. with buses", "Right feeder lane", 
                                    "Centre feeder lane", "Coloured tarmac", 
                                    "No characteristics", "Left feeder lane",  
                                    "Feeder lane present")))

# # create stacked bar chart of ASL count with % in text
ggplot() +
  geom_bar(data = ASL,
           aes(x = Count, y = charac), stat = "identity") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # removes all grid lines
        axis.line.x = element_line(size=0.1, color="black"), # adds axis line back in
        axis.title.y = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(size = 20)) +
  scale_x_continuous(expand = c(0,0), breaks = c(0, 800, 1600), limits = c(0, 2100)) +
  geom_text(data = ASL, aes(x = Count, label = Percentage, y = charac),
            hjust = -0.3, size = 5)

# # create stacked bar chart of %
# ggplot() +
#   geom_bar(data = ASL,
#            aes(x = Percentage, y = charac), stat = "identity") +
#   theme_minimal() +
#   theme(panel.grid = element_blank(),  # removes all grid lines
#         axis.line.x = element_line(size=0.1, color="black"), # adds axis line back in
#         axis.title.y = element_blank(), 
#         axis.text.y = element_blank()) +
#   scale_x_continuous(expand = c(0,0), breaks = c(0, 40))

# create density plot df
asl_df = c_asl %>%
  mutate(length = st_length(geometry)) # gives me the length of all ASL

asl_nil = asl_df %>%
  filter(ASL_FDR == FALSE & ASL_FDRLFT == FALSE & ASL_FDCENT == FALSE & ASL_FDRIGH == FALSE &
           ASL_SHARED == FALSE & ASL_COLOUR == "NONE") %>%
  mutate(charac = "ASL_NIL")
asl_feeder = asl_df %>%
  filter(ASL_FDR == TRUE) %>%
  mutate(charac = "ASL_FDR")
asl_left = asl_df %>%
  filter(ASL_FDRLFT == TRUE) %>%
  mutate(charac = "ASL_FDRLFT")
asl_centre = asl_df %>%
  filter(ASL_FDCENT == TRUE) %>%
  mutate(charac = "ASL_FDCENT")
asl_right = asl_df %>%
  filter(ASL_FDRIGH == TRUE) %>%
  mutate(charac = "ASL_FDRIGH")
asl_shared = asl_df %>%
  filter(ASL_SHARED == TRUE) %>%
  mutate(charac = "ASL_SHARED")
asl_colour = asl_df %>%
  filter(ASL_COLOUR != "NONE") %>%
  mutate(charac = "ASL_COLOUR_F")
asl_density_df = rbind(asl_feeder, asl_nil, asl_left, asl_colour, asl_centre, asl_right, asl_shared) %>%
  st_drop_geometry() %>%
  mutate(charac = factor(charac,
                         levels = c("ASL_FDR", "ASL_FDRLFT", "ASL_NIL", "ASL_COLOUR_F", "ASL_FDCENT",
                                    "ASL_FDRIGH", "ASL_SHARED"),
                         labels = c("Feeder lane present", "Left feeder lane", "No characteristics", 
                                    "Coloured tarmac", "Centre feeder lane", "Right feeder lane", 
                                    "Shared e.g. with buses"))) %>%
  select(c(length, charac)) %>%
  units::drop_units()

# asl_gp_mean = asl_density_df %>%
#   group_by(charac) %>%
#   summarise(grp_mean = mean(length))
asl_gp_med = asl_density_df %>%
  group_by(charac) %>%
  summarise(grp_median = median(length))


# Create ggplot of count of length
ggplot(asl_density_df) +
  geom_density(aes(x = length, fill = "grey"), fill = "grey") +
  facet_wrap(~charac, ncol = 1) +
  scale_x_continuous(expand = c(0,0), breaks = c(0, 5, 10), limits = c(0, 10.4)) +
 # geom_vline(data = asl_gp_mean, aes(xintercept = grp_mean),
#             linetype = "solid") +
  geom_vline(data = asl_gp_med, aes(xintercept = grp_median),
             linetype = "longdash") +
  xlab(label = "Length (m)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # removes all grid lines
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(size = 20))




ggplot(asl_density_df) +
  geom_density(aes(x = length, fill = "grey"), fill = "grey") +
  facet_wrap(~charac, ncol = 1) +
  scale_x_continuous(expand = c(0,0), breaks = c(0, 5, 10), limits = c(0, 10.5)) +
  geom_vline(data = asl_gp_mean, aes(xintercept = grp_mean),
             linetype = "solid") +
  geom_vline(data = asl_gp_med, aes(xintercept = grp_median),
             linetype = "longdash") +
  theme_minimal() +
  geom_rug(aes(x = length, y = 0), position = position_jitter(height = 0)) +
  theme(panel.grid = element_blank(),  # removes all grid lines
        axis.title.y = element_blank(), 
        axis.text.y = element_blank())




asl_shared %>%
  units::drop_units() %>%
  ggplot()+
  geom_density(aes(x = length, fill = "grey"), fill = "grey") +
  #facet_wrap(~charac, ncol = 1) +
  scale_x_continuous(expand = c(0,0), breaks = c(0, 5, 10), limits = c(0, 10.5)) +
  theme_minimal() 
  #theme(panel.grid = element_blank(),  # removes all grid lines
  #      axis.title.y = element_blank(), 
  #      axis.text.y = element_blank())


################################################################################
#6) Comparison of variables of on v off road infrastructure
# including drawing bar charts


# create new variable which is clearly on/off road
clt_on_off = c_cyclelanetrack %>%
  st_drop_geometry() %>%
  mutate(on_off = case_when(CLT_CARR == 'TRUE' ~ "onroad", 
                            TRUE ~ "offroad")) 

# create multiple datasets that measure length of variables by on/off road status
on_off_l = clt_on_off %>%
  group_by(on_off) %>%
  summarise(sum = sum(length_km)) %>%
  mutate(perc = drop_units(round((sum/sum(sum)*100), digit = 1)))

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

#######
# Rejig table so that it is useful for results section
#######
t_on_off_clt_comparison_lengths = setNames(data.frame(t(on_off_clt_comparison_lengths[,-1])), on_off_clt_comparison_lengths[,1])
t_on_off_clt_comparison_lengths= round(t_on_off_clt_comparison_lengths, digits = 1)

# do above for clt counts data
on_off_c = clt_on_off %>%
  group_by(on_off) %>%
  summarise(n = n()) %>%
  mutate(n_perc = round((n/sum(n)*100), digit = 1))

seg_c = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_SEGREG == TRUE) %>%
  summarise(segreg = n()) %>%
  mutate(seg_perc = round((segreg/sum(segreg)*100), digit = 1))
stepp_c = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_STEPP == TRUE) %>%
  summarise(stepp = n()) %>%
  mutate(stepp_perc = round((stepp/sum(stepp)*100), digit = 1))
partseg_c = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_PARSEG == TRUE) %>%
  summarise(partsegreg = n()) %>%
  mutate(partseg_perc = round((partsegreg/sum(partsegreg)*100), digit = 1))
shared_c = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_SHARED == TRUE) %>%
  summarise(shared = n()) %>%
  mutate(shared_perc = round((shared/sum(shared)*100), digit = 1))
mandat_c = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_MANDAT == TRUE) %>%
  summarise(mandat = n()) %>%
  mutate(mandat_perc = round((mandat/sum(mandat)*100), digit = 1))
advis_c = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_ADVIS == TRUE) %>%
  summarise(advis = n()) %>%
  mutate(advis_perc = round((advis/sum(advis)*100), digit = 1))
priority_c = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_PRIORI == TRUE) %>%
  summarise(priority = n()) %>%
  mutate(priority_perc = round((priority/sum(priority)*100), digit = 1))
contra_c = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_CONTRA == TRUE) %>%
  summarise(contra = n()) %>%
  mutate(contra_perc = round((contra/sum(contra)*100), digit = 1))
bidir_c = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_BIDIRE == TRUE) %>%
  summarise(bidir = n()) %>%
  mutate(bidir_perc = round((bidir/sum(bidir)*100), digit = 1))
bypass_c = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_CBYPAS == TRUE) %>%
  summarise(bypass = n()) %>%
  mutate(bypass_perc = round((bypass/sum(bypass)*100), digit = 1))
busbypass_c = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_BBYPAS == TRUE) %>%
  summarise(busbypass = n()) %>%
  mutate(busbypass_perc = round((busbypass/sum(busbypass)*100), digit = 1))
park_c = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_PARKR == TRUE) %>%
  summarise(park = n()) %>%
  mutate(park_perc = round((park/sum(park)*100), digit = 1))
water_c = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_WATERR == TRUE) %>%
  summarise(water = n()) %>%
  mutate(water_perc = round((water/sum(water)*100), digit = 1))
parttime_c = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_PTIME == TRUE) %>%
  summarise(parttime = n()) %>%
  mutate(parttime_perc = round((parttime/sum(parttime)*100), digit = 1))
colour_c = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_COLOUR != "NONE") %>%
  summarise(colour = n()) %>%
  mutate(colour_perc = round((colour/sum(colour)*100), digit = 1))

# join these datasets together to get summary of on/off road comparison of counts
on_off_clt_comparison_counts = plyr::join_all(
  list(colour_c, parttime_c, water_c, park_c, busbypass_c, bypass_c, bidir_c, contra_c,
       priority_c, advis_c, mandat_c, shared_c, partseg_c, stepp_c, seg_c),
  by = 'on_off', type = 'left')

rm(colour_c, parttime_c, water_c, park_c, busbypass_c, bypass_c, bidir_c, contra_c,
   priority_c, advis_c, mandat_c, shared_c, partseg_c, stepp_c, seg_c)


# convert NAs to 0 (there are no onroad lanes that are by water)
on_off_clt_comparison_counts$water[is.na(on_off_clt_comparison_counts$water)] = 0
on_off_clt_comparison_counts$water_perc[is.na(on_off_clt_comparison_counts$water_perc)] = 0

# transpose counts 
t_on_off_clt_comparison_counts = setNames(data.frame(t(on_off_clt_comparison_counts[,-1])), on_off_clt_comparison_counts[,1])

# join into one tables
t_on_off_clt_comparison_counts = t_on_off_clt_comparison_counts %>%
  rename(on_road_counts = onroad) %>%
  rename(off_road_counts = offroad)
t_on_off_clt_comparison_lengths = t_on_off_clt_comparison_lengths %>%
  rename(on_road_lengths = onroad) %>%
  rename(off_road_lengths = offroad)

on_off_clt_comparison_length_counts = cbind(t_on_off_clt_comparison_counts, t_on_off_clt_comparison_lengths) %>%
  rownames_to_column("Characteristic") %>%
  subset(select = c(1, 3, 5, 2, 4)) %>%
  mutate(variable_order = factor(Characteristic,
                                 levels = c("colour", "parttime", "water", "park",
                                            "busbypass", "bypass","bidir", "contra",
                                            "priority", "advis","mandat", "shared",
                                            "partsegreg", "stepp", "segreg"),
                                 labels = c("Coloured tarmac", "Part-time", "Water route", "Park route",
                                            "Continuous facilities through bus stop", "Cycle bypass", "Bidirectional", "Contraflow",
                                            "Given Priority", "Advisory cycle lane","Mandatory cycle lane", 
                                            "Shared cycle lane", "Part-segregated", "Stepped", "Fully-segregated")))




data3 <- rownames_to_column(on_off_clt_comparison_length_counts, "Characteristic")


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
