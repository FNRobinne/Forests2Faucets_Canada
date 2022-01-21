## ---------------------------
## Script name: CreateCatchmentVariables
##
## Purpose of script: Extract and compile statistics on various variables on a HUC-12 catchment basis
##
## Author: Dr. François-Nicolas Robinne
##
## Date Created: 2022-01-21
##
## Project: Forest to Faucets Canada
##
## Copyright (c) François-Nicolas Robinne, 2022
## Email: francois.robinne@nrcan-rncan.gc.ca
##
## ---------------------------
## Notes: This should better be ran first so all variables are set
##        It is necessary to have these variables ready to create the composite indicator
##        They will remain attached to the centroid as well
##        Make sure to keep the field HYBAS_ID => it's the key for future spatial and tabular joins
## ---------------------------

## set working directory ---------------------------

  # Update as needed
  setwd("D:/PROJECTS/39_2021_CANADA_F2F_SOURCE2TAP_ACTIVE")

## general options ---------------------------

  options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
  memory.limit(30000000)     # this is needed on some PCs to increase memory allowance

## load libraries ---------------------------

  library(tidyverse)
  library(sf)
  library(terra)
  library(exactextractr)
  
# source("functions/packages.R")       # loads up all the packages we need

## load functions ---------------------------

  # source("functions/summarise_data.R") 

## load data ---------------------------
  
  # Beware of CRS => make sure it is the same for all data
  HUC12 <- st_read("02_PROCESSED_DATA/HydroBasin_HUC12_Fraser_Fixed.shp") %>%
    st_transform(crs = 3979)
  Intakes <- st_read("02_PROCESSED_DATA/Canada_Municipal_Surface_Intake_V4_28102021.gpkg")
  CanTreeCover <- rast("NRCAN/NFI_MODIS250m_kNN_LandCover_VegTreed_v0.tif")
  #ConusTreeCover <-
  #AkTreeCover <- 
  

## data processing -------------------------------------------------------

  # Vector data ----------------------------------------------------------


  # Raster data -----------------------------------------------------------

  
    