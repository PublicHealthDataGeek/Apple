##############################################################
# Create tables for Borough level summary data #
#
#


# install packages
library(tidyverse)
#library(sf)
#library(mapview)
#library(units)

# Load datasets - these datasets were created 2_3_2021 from TFL datasets downloaded 25/2/21
CID_count = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/CID_count_by_borough")
CID_length = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/CID_length_by_borough")
