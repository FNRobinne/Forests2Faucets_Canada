## ---------------------------
## Script name: 
##
## Purpose of script:
##
## Author: Dr. François-Nicolas Robinne
##
## Date Created: 2021-10-26
##
## Copyright (c) François-Nicolas Robinne, 2021
## Email: robinne@ualberta.ca
##
## ---------------------------
## Notes:
##   
##
## ---------------------------

## set working directory ---------------------------

setwd("D:/PROJECTS/39_2021_CANADA_F2F_SOURCE2TAP_ACTIVE") 

## general options ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
memory.limit(30000000)     # this is needed on some PCs to increase memory allowance

## load libraries ---------------------------

library(tidyverse)
library(sf)

# source("functions/packages.R")       # loads up all the packages we need

## load functions ---------------------------

# source("functions/summarise_data.R") 


# Load data ---------------------------------------------------------------

  Fraser <- st_read("02_PROCESSED_DATA/Fraser_Watershed_WGS84_Fixed.shp") %>%
    st_transform(Fraser, crs = 3979)
  HYBAS_Fraser <- st_read("02_PROCESSED_DATA/HydroBasin_HUC12_Fraser_Fixed.shp") %>%
    st_transform(Fraser, crs = 3979)
  HYRIV_Fraser <- st_read("02_PROCESSED_DATA/HydroRiver_Fraser_Fixed.shp") %>%
    st_transform(Fraser, crs = 3979)

# Data preparation --------------------------------------------------------

  # Check which watersheds only has Strahler order 1
  Subset_HYBAS <- HYRIV_Fraser %>%
    group_by(HYBAS_L12) %>%
    summarise(cnt = n()) %>%
    filter(cnt == 1)
  
  
  
