################################################################################
# IMD rank


##

library(tidyverse)
library(tmap)
library(tmaptools) # for palette explorer 
library(sf) # to drop geometry
library(GGally) # parallel coordinate extension of ggplot
# library(ggpubr) # for text grobs
# library(ggspatial) # get north arrow and bar


########################################################
# Load and manipulate LA spatial data fo visualisation #
########################################################

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

# Select variables of interest
lon_lad_2020_c2c_reduced = lon_lad_2020_c2c %>%
  select(c("BOROUGH", "Borough_number", "geometry"))



##################################################
# Load and manipulate local authority IMD scores #
##################################################

#IMD website  https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019

# Download and import IMD data
download.file("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833995/File_10_-_IoD2019_Local_Authority_District_Summaries__lower-tier__.xlsx",
              "/home/bananafan/Downloads/IMD_19_LA.xlsx")
imd_19 = readxl::read_excel("/home/bananafan/Downloads/IMD_19_LA.xlsx", 
                            sheet = "IMD") # skip top few excel rows that arent relevent

# Rename variables
imd_19 = rename(imd_19, 
                "BOROUGH" = "Local Authority District name (2019)",
                "IMD_ranked_average_rank"  = "IMD - Rank of average rank",
                "IMD_ranked_average_score" = "IMD - Rank of average score")

# extract London Borough data
lon_boroughs = c("Kensington and Chelsea", "Barking and Dagenham", "Hammersmith and Fulham","Kingston upon Thames", "Richmond upon Thames",
                 "City of London", "Waltham Forest", "Croydon", "Bromley", "Hounslow", "Ealing", "Havering", "Hillingdon", "Harrow",
                 "Brent", "Barnet", "Lambeth", "Southwark", "Lewisham", "Greenwich", "Bexley", "Enfield", "Redbridge", "Sutton",
                 "Merton", "Wandsworth", "Westminster", "Camden", "Tower Hamlets", "Islington", "Hackney", "Haringey", "Newham")

lon_imd19 = imd_19 %>%
  filter(BOROUGH %in% lon_boroughs)

# rename values to match those names used in CID
lon_imd19$BOROUGH[lon_imd19$BOROUGH == "Kensington and Chelsea"] <- "Kensington & Chelsea"
lon_imd19$BOROUGH[lon_imd19$BOROUGH == "Barking and Dagenham"] <- "Barking & Dagenham"
lon_imd19$BOROUGH[lon_imd19$BOROUGH == "Hammersmith and Fulham"] <- "Hammersmith & Fulham"

# select variables of interest
lon_imd19 = lon_imd19 %>%
  select(c("BOROUGH", "IMD_ranked_average_rank", "IMD_ranked_average_score"))


# Create new variable which puts each LA rank into a decile

# Ranks are 1-317 where 1 is the Most deprived and 317 is the least deprived

# Average rank
# - summarises the average level of deprivation based on the RANKS of the LSOA in the Borough
# - is population weighted
# - using ranks rather than scores means that highly polarised LA do not tend to score highly as 
# extremely deprived and less deprived LSOAs 'average out'

# Average score
# - summarises the average level of deprivation based on the SCORES of the LSOA in the Borough
# - is population weighted
# - using scores rather than ranks means that more deprived LA (which tend to have more extreme scores than ranks
# will not 'average out'.  Therefore highly polarised areas tend to score higher on average score than average rank

# see https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833947/IoD2019_Research_Report.pdf


lon_imd19 = lon_imd19 %>%
  mutate(decile_ranked_average_rank = cut(IMD_ranked_average_rank,
                                   breaks = 10,
                                   labels = c("Most deprived 10%", "2", "3", "4", "5", "6", "7", "8", "9", "Least deprived 10%"),
                                   ordered_result = TRUE)) %>%
  mutate(decile_ranked_average_score = cut(IMD_ranked_average_score,
                                          breaks = 10,
                                          labels = c("Most deprived 10%", "2", "3", "4", "5", "6", "7", "8", "9", "Least deprived 10%"),
                                          ordered_result = TRUE))


# Join Borough spatial data to IMD19
lon_imd19 = left_join(lon_lad_2020_c2c_reduced, lon_imd19)

###################################
# Visualise IMD with chloropleths #
###################################

tm_shape(lon_imd19) +
  tm_polygons("decile_ranked_average_rank", 
              #breaks = c(0, 30, 60, 90, 120, 150, 180),
              #legend.format = list(text.separator = "<"),
              palette = "-Purples") + 
  tm_layout(title = "Borough Rank of average Index of Multiple deprivation rank for LSOAs in Borough, 2019 (population-weighted)",
            legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1,
            inner.margins = c(0.1,0.1,0.1,0.1),
            frame = FALSE) 
  #+ tm_add_legend(labels = c("Most deprived 10%", "", "", "", "", "", "", "", "", "Least deprived 10%"), 
  #              title = "Decile of ranked average rank")  # will need to add colours using col = argument when ready

#bar chart for this  
# Generate new column that makes imd rank numeric
lon_imd19 <- lon_imd19 %>%
  mutate(numeric_decile_ranked_average_rank = as.numeric(cut(IMD_ranked_average_rank,
                                                  breaks = 10,
                                                  labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                                                  ordered_result = TRUE)))

# # # Create vector of colours that match the chloropleth
#imd_colours = c("#edf8e9", "#bae4b3", "#74c476", "#238b45")

# create Bar chart
ggplot(lon_imd19, aes(x = reorder(Borough_number, -numeric_decile_ranked_average_rank),
                      y = numeric_decile_ranked_average_rank,
                      fill = numeric_decile_ranked_average_rank)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +  # adds borders to bars
  coord_flip() +
  labs(y = "IMD decile ", x = NULL) +
  theme_classic() +
  scale_y_continuous(expand = c(0,0)) +  # ensures axis starts at 0 so no gap
  #scale_fill_manual(values = imd_colours) +
  #scale_fill_brewer(palette = "Purple", direction = -1)
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none")
  
  
tm_shape(lon_imd19) +
  tm_polygons("decile_ranked_average_score", 
              palette = "-Blues") + 
  tm_layout(title = "Borough Rank of average Index of Multiple deprivation score for LSOAs in Borough, 2019 (population-weighted)",
            legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1,
            inner.margins = c(0.1,0.1,0.1,0.42), # creates wide right margin for barchart
            frame = FALSE) +
  tm_add_legend(labels = c("Most deprived 10%", "", "", "", "", "", "", "", "", "Least deprived 10%"), 
                title = "Decile of ranked average score")  # will need to add colours using col = argument when ready


# compare two sets of rankings - needs more work if choose to use
ggparcoord(lon_imd19, 
           columns= c(5, 6), 
           scale = "globalminmax",    # uses the rank rather that scaling to something else
           groupColumn = 1,
           alphaLines = 0.8,
           #showPoints = TRUE,
           title = "Comparision of IMD ranking approaches - rank v score") +
  # scale_color_manual(values =c("maroon", "dark gray", "black")) +  # inner, outer, missing
  theme_minimal()
