################################################################################
#  Examining relationships between infrastructure amount and Borough size, 
#            population and commuter cycling
#
#  Date creation - 9/6/21
#  1) loads and manipulates datasets
#  2) creates correlation plots and simple lm



# Load packages
library(tidyverse)

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

# Add column for inner/outer london
Inner = c("City of London", "Camden", "Greenwich", "Hackney", "Hammersmith & Fulham", 
          "Islington", "Kensington & Chelsea", "Lambeth", "Lewisham", "Southwark",  
          "Tower Hamlets", "Wandsworth", "Westminster") 

lon_lad_2020_c2c = lon_lad_2020_c2c %>%
  mutate(London = ifelse(BOROUGH %in% Inner, "Inner London", "Outer London")) 

# Convert borough area into km^2 from m^2 
lon_lad_2020_c2c$Shape__Are = units::set_units(lon_lad_2020_c2c$Shape__Are, m^2)
lon_lad_2020_c2c = lon_lad_2020_c2c %>%
  mutate(Borough_Area_km2 = (units::set_units(lon_lad_2020_c2c$Shape__Are, km^2)))# change area units to km^2 from m^2

# Select variables of interest
lon_lad_2020_c2c_reduced = lon_lad_2020_c2c %>%
  select(c("BOROUGH", "Borough_number", "London", "Borough_Area_km2", "geometry"))


# 2) CID data

# Import CID borough counts - these datasets were updated on 8/6 to include the new number of crossings (originally creatted 2_3_2021 from TFL datasets downloaded 25/2/21
CID_count = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/CID_count_by_borough")
CID_length = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/CID_length_by_borough")

# keep safety related assets
CID_count_safety = CID_count %>%
  select(c("BOROUGH", "ASL", "Crossings", "Signals", "TrafficCalming"))
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
  left_join(pct_borough_commuting) %>%
  st_drop_geometry() 
  
model_df = left_join(denominators, CID_counts) %>%
  units::drop_units()
# add columns for dummy Inner/outer london variables
model_df$Inner <- as.factor(ifelse(model_df$London == 'Inner London', 1, 0))

# Create scatterplots  with lm regression line and correlation coeefficients by asset and by 
asl1 = ggpubr::ggscatter(model_df, x = "Borough_Area_km2", y = "ASL", add = "reg.line", title = "ASL ~ Area") +
  stat_cor(label.x = 50, label.y = 330) +
  stat_regline_equation(label.x = 50, label.y = 300)

asl2 = ggpubr::ggscatter(model_df, x = "Population_100000", y = "ASL", add = "reg.line", title = "ASL ~ Population") +
  stat_cor(label.x = 0.5, label.y = 330) +
  stat_regline_equation(label.x = 0.5, label.y = 300)

asl3 = ggpubr::ggscatter(model_df, x = "total_100000km_cycled_for_commuting_per_year_estimated", 
                         y = "ASL", add = "reg.line", title = "ASL ~ Commuter cycling") +
  stat_cor(label.x = 25, label.y = 330) +
  stat_regline_equation(label.x = 25, label.y = 300)

cross1 = ggpubr::ggscatter(model_df, x = "Borough_Area_km2", y = "Crossings", add = "reg.line", title = "Crossings ~ Area") +
  stat_cor(label.x = 0.2, label.y = 145) +
  stat_regline_equation(label.x = 0.2, label.y = 130)

cross2 = ggpubr::ggscatter(model_df, x = "Population_100000", y = "Crossings", add = "reg.line", title = "Crossings ~ Population") +
  stat_cor(label.x = 0.5, label.y = 145) +
  stat_regline_equation(label.x = 0.5, label.y = 130)

cross3 = ggpubr::ggscatter(model_df, x = "total_100000km_cycled_for_commuting_per_year_estimated", 
                         y = "Crossings", add = "reg.line", title = "Crossings ~ Commuter cycling") +
  stat_cor(label.x = 50, label.y = 145) +
  stat_regline_equation(label.x = 50, label.y = 130)

sig1 = ggpubr::ggscatter(model_df, x = "Borough_Area_km2", y = "Signals", add = "reg.line", title = "Signals ~ Area") +
  stat_cor(label.x = 55, label.y = 90) +
  stat_regline_equation(label.x = 55, label.y = 80)

sig2 = ggpubr::ggscatter(model_df, x = "Population_100000", y = "Signals", add = "reg.line", title = "Signals ~ Population") +
  stat_cor(label.x = 0.5, label.y = 90) +
  stat_regline_equation(label.x = 0.5, label.y = 80)

sig3 = ggpubr::ggscatter(model_df, x = "total_100000km_cycled_for_commuting_per_year_estimated", 
                         y = "Signals", add = "reg.line", title = "Signals ~ Commuter cycling") +
  stat_cor(label.x = 20, label.y = 90) +
  stat_regline_equation(label.x = 20, label.y = 80)

tc1 = ggpubr::ggscatter(model_df, x = "Borough_Area_km2", y = "TrafficCalming", add = "reg.line", 
                        title = "Traffic calming ~ Area") +
  stat_cor(label.x = 75, label.y = 3400) +
  stat_regline_equation(label.x = 75, label.y = 3000)

tc2 = ggpubr::ggscatter(model_df, x = "Population_100000", y = "TrafficCalming", add = "reg.line",
                        title = "Traffic calming ~ Population") +
  stat_cor(label.x = 0.5, label.y = 3400) +
  stat_regline_equation(label.x = 0.5, label.y = 3000)

tc3 = ggpubr::ggscatter(model_df, x = "total_100000km_cycled_for_commuting_per_year_estimated", 
                         y = "TrafficCalming", add = "reg.line", title = "Traffic calming ~ Commuter cycling") +
  stat_cor(label.x = 55, label.y = 3600) +
  stat_regline_equation(label.x = 55, label.y = 3150)

clt1 = ggpubr::ggscatter(model_df, x = "Borough_Area_km2", y = "CycleLaneTrack_km", add = "reg.line", 
                         title = "Cycle lanes and tracks ~ Area") +
  stat_cor(label.x = 10, label.y = 150) +
  stat_regline_equation(label.x = 10, label.y = 130)

clt2 = ggpubr::ggscatter(model_df, x = "Population_100000", y = "CycleLaneTrack_km", add = "reg.line",
                         title = "Cycle lanes and tracks ~ Population") +
  stat_cor(label.x = 0.4, label.y = 150) +
  stat_regline_equation(label.x = 0.4, label.y = 130)

clt3 = ggpubr::ggscatter(model_df, x = "total_100000km_cycled_for_commuting_per_year_estimated", 
                        y = "CycleLaneTrack_km", add = "reg.line", title = "Cycle lanes and tracks ~ Commuter cycling") +
  stat_cor(label.x = 100, label.y = 150) +
  stat_regline_equation(label.x = 100, label.y = 130)
       
asl_scatter_lm = gridExtra::grid.arrange(asl1, asl2, asl3,  ncol = 3)
cross_scatter_lm = gridExtra::grid.arrange(cross1, cross2, cross3,  ncol = 3)
sig_scatter_lm = gridExtra::grid.arrange(sig1, sig2, sig3,  ncol = 3)
tc_scatter_lm = gridExtra::grid.arrange(tc1, tc2, tc3,  ncol = 3)
clt_scatter_lm = gridExtra::grid.arrange(clt1, clt2, clt3,  ncol = 3)


### Model 1)
m1 = lm(model_df$ASL ~ model_df$Borough_Area_km2)
summary(m1)


# Regresion for Inner v Outer

borough_asl_m = lm(model_df$ASL ~ model_df$Inner); summary(borough_asl_m)

borough_cross_m = lm(model_df$Crossings ~ model_df$Inner); summary(borough_cross_m)

borough_clt_m = lm(model_df$CycleLaneTrack_km ~ model_df$Inner); summary(borough_clt_m)

borough_sig_m = lm(model_df$Signals ~ model_df$Inner); summary(borough_sig_m)

borough_tc_m = lm(model_df$TrafficCalming ~ model_df$Inner); summary(borough_tc_m)

# useful code for adding regression line and R^2 value
ggplot(data = model_df)+
  geom_point(aes(x = Borough_Area_km2, y = ASL), pch = 1)+
  stat_smooth(aes(x = Borough_Area_km2, y = ASL), method = "lm", se = F, fullrange = T)+
  stat_cor(aes(x = Borough_Area_km2, y = ASL, label = ..rr.label..), geom = "label")


#### cREATING DUMMY VARIABLES
# https://www.marsja.se/create-dummy-variables-in-r/
# library('fastDummies')
# # Create dummy variables:
# dataf <- dummy_cols(dataf, select_columns = 'rank')
# #remove dummy columns
# dataf.2 <- dummy_cols(dataf, select_columns = c('rank', 'discipline'),
#                       remove_selected_columns = TRUE)


##### STEPS FOR MODELLING
# 1) Check for correlations
# generate trellis of scatter plots of variables
plot(mydata) 
# there appear to be positive correlations between height & waist, height & weight,
# age & waist, with a strange relationship between age & weight,
# and a nonlinear relationship between waist & weight

# set graphics device to four sections (2x2 grid)
par(mfrow = c(2, 2))

# 2) examine distributions of potential covariates - check to see if normal
# plot density function of all variables to allow examination of distributions
plot(density(mydata$height)) 
plot(density(mydata$age))
plot(density(mydata$waist)) 
plot(density(mydata$weight)) 
# height, age, & waist look approximately normal
# weight does not look approximately normal

# reset graphics device to one section
par(mfrow = c(1, 1))

# 3) fit linear model with waist as outcome and age and height as covariates
mod1 <- lm(waist ~ height + age, data = mydata) 

# 4) Generate summary of model
summary(mod1) 

# a) Check residuals - median should be close to 0, Q1 should be similar in 
# magnitude to Q3 (symetrical dist), min should be similar to max (although to lesser degree than Q1/3)

# b) Examine point coefficent estimates and generate confidence intervals for estimates
confint(mod1) 

# c) Examine R-squared
# the adjusted R-squared is quite high at 0.75
# age and height explain a high proportion of variation in waist

# d) Residual standard error = best estimate of standard deviation of the residuals

# e) Degrees of freedom 
# - tell you how many observations are used to estimate the residual variance

# set graphics device to four sections
par(mfrow = c(2, 2)) 


#5) Produce diagnostic plots for mod1 to check meets assumptions of normality etc
plot(mod1) 

# Top Left Plot (residuals v fitted) - purpose is to check for heterogeneity of variance
# linear model assumes the variance of residuals is constant (ie homoscedasticity)
# heteroscedasticity = where variance increases with a fitted value
# For Top left plot want 
# - approx equal scatter of points around line
# - line to be horizontal (would be a consequence of above)
# - line to be a close to horizontal zero line as possible
# there are no major issues with the diagnostic plots

# Top right Plot (normal QQPlot) - checks normality of residuals
# Residuals should be normal for correct estimates of coefficent SE 
# - they dont affect the point coefficent estimates just our confidence in point estimates
# For QQ plot want - points fall on idealised straight line, can allow some modest deviations at extremes

# Bottom left Plot (Scale-location plot)
# Shows whether residuals are spread equally along range of covariate values
# Want - horizonal line with equally (randomly) spread points

# Bottomr Right (Cook's distance)
# Identiris residuals with large influence or leverage
# Plots Cook's distance
# Only interested in outlying values in upper right or upper left corners


# reset graphics device to one section
par(mfrow = c(1, 1))


# # 6) examine the distribution of residuals in a density plot
plot(density(mod1$residuals)) 
# the residuals should appear normally distributed.

