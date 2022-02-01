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
  library(RColorBrewer)
  
# source("functions/packages.R")       # loads up all the packages we need

## load functions ---------------------------

  # source("functions/summarise_data.R") 

## load data ---------------------------
  
  # Beware of CRS => make sure it is the same for all data
  # National data below were prepared using a GIS software
  # All were set to EPSG:3979
  # All rasters are at a 250m pixel resolution
  HUC12 <- st_read("02_PROCESSED_DATA/HydroBasin_HUC12_Fraser_Fixed.shp") %>%
    st_transform(crs = 3979) %>%
    select(HYBAS_ID, SUB_AREA, dis_m3_pyr)
  Intakes <- st_read("02_PROCESSED_DATA/Canada_Municipal_Surface_Intake_V4_28102021.gpkg") %>%
    st_transform(crs = 3979)
  CanTreeCover <- rast("02_PROCESSED_DATA/NRCAN/CanTreeCover_EPSG3979.tif")
  CanTreeCover_10[CanTreeCover<10] <- NA
  WaterYield <- 
  BurnPro <-
  FireInt <- 
  # CanTreeCoverEPSG3979 <- project(x = CanTreeCover, y = "epsg:3979")
  #ConusTreeCover <-
  #AkTreeCover <- 
  

## data processing -------------------------------------------------------
  
  # Raster data masking (using forest cover layer)
  
  # Vector data extraction/summary per HUC12
  HUC12_WatVol <- HUC12 %>%
    st_join(Intakes) %>%
    group_by(HYBAS_ID) %>%
    summarise(TotMunWat = sum(YR_VOL_M3)) 
  
  HUC12_WatVol_NoNA <- HUC12_WatVol %>%
    mutate(TotMunWat = replace_na(TotMunWat,0))

  # Raster data extraction/summary per HUC12
  HUC12_ForCov <- exact_extract(CanTreeCover_10, HUC12, 'count',
                                progress = F, append_cols = c('HYBAS_ID')) %>% 
    rename(forcov = count)
  HUC12_Forests <- HUC12 %>%
    right_join(HUC12_ForCov) %>%
    mutate(forcov_area = (forcov*0.0625)/SUB_AREA*100) %>% # 250*250m equals 0.0625 km²
    mutate(forcov_area = case_when(
      forcov_area > 100 ~ 100,
      TRUE ~ forcov_area))

## Data viz
  ggplot() +
    geom_sf(data = HUC12_Forests, aes(fill = forcov_area), color = NA) +
    scale_fill_distiller(palette = "Greens", trans = "reverse")
    
  ggplot() +
    geom_sf(data = HUC12_WatVol_NoNA, aes(fill = TotMunWat)) +
    scale_fill_distiller(palette = "Blues", trans = "reverse")
  