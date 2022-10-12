## ---------------------------
## Script name: 01_UpdateHydroAtlas.R
##
## Purpose of script: Select from and add new variables to HydroAtlas for F2F-C
##                    analysis.
##
## Author: Dr. François-Nicolas Robinne
##
## Date Created: 2022-01-21
##
## Version: 1.0
##
## Project: Forest to Faucets Canada
##
## Copyright (c) François-Nicolas Robinne, 2022
## Email: francois.robinne@nrcan-rncan.gc.ca
##
## 
## Notes: This should better be ran first so all variables are set
##        It is necessary to have these variables ready to create the composite indicator
##        They will be attached to the catchment outlets as well
##        Make sure to keep the field HYBAS_ID => it's the primary key for future spatial and tabular joins
## 

## set working directory -------------------------------------------------------

  # Update as needed
  setwd("C:/Users/frobinne/OneDrive - NRCan RNCan/Documents/Professionel/PROJECTS/39_2021_CANADA_F2F_SOURCE2TAP_ACTIVE") 

## general options -------------------------------------------------------------

  options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
  memory.limit(30000000)     # this is needed on some PCs to increase memory allowance

## load libraries --------------------------------------------------------------

  library(tidyverse)
  library(sf)
  library(terra)
  library(exactextractr)
  library(RColorBrewer)
  library(rgdal)

## load data -------------------------------------------------------------------
  
  # All data are in NAD83 Canada Atlas Lambert (EPSG:3979)
  hybas_can <- st_read("02_PROCESSED_DATA/HYDROLAB_MCGILL/HydroBasins_HUC3_WaterSupply_HUC12_RepairedGeometry_EPSG3979.gpkg") %>%
    select(HYBAS_ID, NEXT_DOWN, MAIN_BAS, ENDO, SUB_AREA, COAST, 
           run_mm_syr, lka_pc_sse, dis_m3_pyr, slp_dg_sav, 
           sgr_dk_sav, snw_pc_syr, wet_pc_sg1, wet_pc_sg2,
           for_pc_sse, soc_th_sav, snd_pc_sav, slt_pc_sav)
  intakes_can <- st_read("02_PROCESSED_DATA/NRCAN/Canada_Municipal_Surface_Intake_V4_28102021.gpkg") %>%
    select(YR_VOL_M3)
  burnpro_can <- rast("02_PROCESSED_DATA/NRCAN/BP3/BP_EPSG3979_Masked_FI.tif")
  fireint_can <- rast("02_PROCESSED_DATA/NRCAN/BP3/FI_EPSG3979.tif")
  fire_haz <- rast
  rain_ero_can <- rast("02_PROCESSED_DATA/ESDAC/CANADA_RAINFALL_EROSIVITY_EPSG3979.tif")
  

## data processing -------------------------------------------------------------
  
  # Adds new variables to HydroAtlas at the HUC12 level
  
  # Total water volume extraction per HUC12
  HUC12_WatVol <- hybas_can %>%
    st_join(intakes_can) %>%
    group_by(HYBAS_ID) %>%
    summarise(TotMunWat = sum(YR_VOL_M3)) 
  
  HUC12_WatVol_NoNA <- HUC12_WatVol %>%
    mutate(TotMunWat = replace_na(TotMunWat,0)) %>%
    st_drop_geometry()
  
  hybas_can_updated <- hybas_can %>%
    right_join(HUC12_WatVol_NoNA) %>%
    rename(mww_m3_syr = TotMunWat) # update name to match HydroAtlas format
    
  # Median and mean burn probability per HUC12
  hybas_burnpro_med <- exact_extract(burnpro_can, hybas_can, fun = 'median',
                                progress = T, append_cols = c('HYBAS_ID')) %>% 
    rename(fp_ul_sed = median) %>% # update name to match HydroAtlas format
    mutate(fp_ul_sed = replace_na(fp_ul_sed,0)) %>% # replace NAs to 0
    mutate(fp_ul_sed = round(fp_ul_sed, 4)) # round values to 4 decimals
  
  hybas_burnpro_avg <- exact_extract(burnpro_can, hybas_can, fun = 'mean',
                                     progress = T, append_cols = c('HYBAS_ID')) %>% 
    rename(fp_ul_sav = mean) %>% # update name to match HydroAtlas format
    mutate(fp_ul_sav = replace_na(fp_ul_sav,0)) %>% # replace NAs to 0
    mutate(fp_ul_sav = round(fp_ul_sav, 4)) # round values to 4 decimals
  
  hybas_can_updated_fire <- hybas_can_updated %>%
    right_join(hybas_burnpro_med) %>%
    right_join(hybas_burnpro_avg)
  
  # Median and mean fire intensity per HUC12
  hybas_fireint_med <- exact_extract(fireint_can, hybas_can, fun = 'median',
                                     progress = T, append_cols = c('HYBAS_ID')) %>% 
    rename(fi_kw_sed = median) %>% # update name to match HydroAtlas format
    mutate(fi_kw_sed = replace_na(fi_kw_sed,0)) %>% # replace NAs to 0
    mutate(fi_kw_sed = round(fi_kw_sed, 4)) # round values to 4 decimals
  
  hybas_fireint_avg <- exact_extract(fireint_can, hybas_can, fun = 'mean',
                                     progress = T, append_cols = c('HYBAS_ID')) %>% 
    rename(fi_kw_sav = mean) %>% # update name to match HydroAtlas format
    mutate(fi_kw_sav = replace_na(fi_kw_sav,0)) %>% # replace NAs to 0
    mutate(fi_kw_sav = round(fi_kw_sav, 4)) # round values to 4 decimals
  
  hybas_can_updated_fireint <- hybas_can_updated_fire %>%
    right_join(hybas_fireint_med) %>%
    right_join(hybas_fireint_avg)
  
  # Median and mean rainfall erosivity per HUC12
  hybas_ero_avg <- exact_extract(rain_ero_can, hybas_can, fun = 'mean',
                                     progress = T, append_cols = c('HYBAS_ID')) %>% 
    rename(re_th_sav = mean) %>% # update name to match HydroAtlas format
    mutate(re_th_sav = replace_na(re_th_sav,0)) %>% # replace NAs to 0
    mutate(re_th_sav = round(re_th_sav, 4)) # round values to 4 decimals
  
  hybas_can_updated_ero <- hybas_can_updated_fireint %>%
    right_join(hybas_ero_avg)
  
  # Convert HUC12 outlets to SP format for use in RiverDist
  # hybas_outlets_single <- hybas_outlets %>%
  #   st_cast("POINT") %>%
  #   mutate(ID = row_number())
  # hybas_outlets_SP <- as_Spatial(from = hybas_outlets_single)

## Data viz --------------------------------------------------------------------
  ggplot() +
    geom_sf(data = hybas_can_updated_ero, aes(fill = re_th_sav), color = NA) +
    scale_fill_distiller(palette = "Greens", trans = "reverse")


## Save output -----------------------------------------------------------------

  # Once all data summarized on HUC12
  # Save data as geopackage
  st_write(hybas_can_updated_ero,"02_PROCESSED_DATA/HYDROLAB_MCGILL/HydroBasins_HUC3_WaterSupply_HUC12_RepairedGeometry_EPSG3979_UpdatedVariables.gpkg",
           delete_layer = T)
  
  # Save outlets for RiverDist
  # writeOGR(hybas_outlets_SP, "03_ANALYSIS_GITHUB/INPUTS", "hybas_outlets_RD", driver = "ESRI Shapefile") # Version for RiverDist (sp format)

  