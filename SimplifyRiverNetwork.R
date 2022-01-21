## ---------------------------
## Script name: RiverNetwork_Simplication.R
##
## Purpose of script: Simplifies the HydroRiver network so it only keeps streams of Strahler order >2,
##                    except in catchments where only order 1 exist—those are kept.
##
## Author: Dr. François-Nicolas Robinne
##
## Date Created: 2021-10-26
## 
## Version: 1.0
##
## Copyright (c) François-Nicolas Robinne, 2021
## Email: francois.robinne@nrcan-rncan.gc.ca
##
## ---------------------------
## Notes:
##   This is part of the project Forest To Faucets - Canada
##
## ---------------------------

## set working directory ---------------------------

  setwd("D:/PROJECTS/39_2021_CANADA_F2F_SOURCE2TAP_ACTIVE") 

## general options ---------------------------

  options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
  memory.limit(30000000)     # this is needed on some PCs to increase memory allowance

## load libraries --------------------------------------------------------

  library(tidyverse)
  library(sf)
  library(rgdal)

# source("functions/packages.R")       # loads up all the packages we need

## load functions --------------------------------------------------------

# source("functions/summarise_data.R") 


# Load data ---------------------------------------------------------------

  # Note: geometry fixed in QGis prior to loading data
  HYBAS_Fraser <- st_read("02_PROCESSED_DATA/HydroBasin_HUC12_Fraser_Fixed.shp") %>%
    st_transform(crs = 3979)
  HYRIV_Fraser <- st_read("02_PROCESSED_DATA/HydroRiver_Fraser_Fixed.shp") %>%
    st_transform(crs = 3979) %>%
    select(HYRIV_ID, ORD_STRA, HYBAS_L12)

# Data processing ---------------------------------------------------------
  
  # Separate watersheds that only have Strahler order 1
  # Create river layer for watersheds with more than Strahler order 1
  HYRIV_Stral2Plus <- HYRIV_Fraser %>%
    filter(ORD_STRA > 1) %>%
    group_by(HYBAS_L12) %>%
    summarise(cnt = n())
  
  # Tabulate difference between original river layer and river layer for watersheds with more than Strahler order 1
  Diff_Stral2_Stral1 <- anti_join(st_drop_geometry(HYRIV_Fraser), st_drop_geometry(HYRIV_Stral2Plus)) %>%
    group_by(HYBAS_L12) %>%
    summarise(cnt = n())
  
  # Use previous step to filter original river layer and filter watersheds with only Strahler order 1
  HYRIV_Stral1 <-  HYRIV_Fraser %>%
    filter(HYBAS_L12 %in% Diff_Stral2_Stral1$HYBAS_L12)
  
  # Delete all Strahler order 1 in the original dataset and add back the watersheds with order 1 only
  HYRIV_Lite <- HYRIV_Fraser %>%
    filter(ORD_STRA > 1) %>% 
    bind_rows(HYRIV_Stral1) %>%
    group_by(HYBAS_L12) %>%
    summarize() 
  
    # Convert the simplified network to SP format for use in RiverDist
    HYRIV_Lite_ID <- HYRIV_Lite %>%
      mutate(ID = row_number())
    HYRIV_Lite_SP <- as_Spatial(from = HYRIV_Lite_ID)
   
    # ESDA (just checking that process worked)
    ggplot() +
      geom_sf(data = HYRIV_Lite, colour = "blue") +
      geom_sf(data = HYBAS_Fraser, fill = NA)
    
# Data export -------------------------------------------------------------

  # Save clean river network
  # Modify destination path as needed
  st_write(HYRIV_Lite, "02_PROCESSED_DATA/HydroRiver_Fraser_Lite.gpkg", append = F)
  writeOGR(HYRIV_Lite_SP, getwd(), "HydroRiver_Fraser_RD", driver = "ESRI Shapefile") # Version for RiverDist (sp format)
  