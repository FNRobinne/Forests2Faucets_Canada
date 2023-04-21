## ---------------------------
## Script name: 01_UpdateHydroATLAS.R
##
## Purpose of script: Select from and add new variables to BasinATLAS for F2F-C
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
## Email: robinne@ualberta.ca
##
## 
## Notes: This should better be ran first so all variables are set
##        It is necessary to have these variables ready to create the composite indicator
##        They will be attached to the catchment outlets as well
##        Make sure to keep the field HYBAS_ID => it's the primary key for future spatial and tabular joins
## 

## set working directory -------------------------------------------------------

  # Update as needed
  setwd("C:/Users/frobinne/Documents/Professional/PROJECTS/39_2021_CANADA_F2F_SOURCE2TAP_ACTIVE") 

## general options -------------------------------------------------------------

  options(scipen = 12, digits = 4) # I prefer to view outputs in non-scientific notation

## load libraries --------------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(exactextractr)
library(RColorBrewer)
library(imputeTS)
library(lubridate)
library(xts)

## load data -------------------------------------------------------------------
  
  # 1) load data from HydroATLAS that I need for the analysis
  hybas_can <- st_read("02_PROCESSED_DATA/HYDROLAB_MCGILL/HydroBasins_HUC3_WaterSupply_HUC12_RepairedGeometry_EPSG3979.gpkg") %>%
    select(HYBAS_ID, NEXT_DOWN, MAIN_BAS, ENDO, SUB_AREA, COAST, 
           run_mm_syr, lka_pc_sse, lka_pc_use, dis_m3_pyr, slp_dg_sav, slp_dg_uav, 
           sgr_dk_sav, snw_pc_syr, snw_pc_uyr, wet_pc_sg2, wet_pc_ug2,
           for_pc_sse, for_pc_use, soc_th_sav, soc_th_uav, snd_pc_sav, snd_pc_uav,
           slt_pc_sav, slt_pc_uav)
  # 2) Load datasets to update HydroATLAS
  # Water data
  wat_demand <- st_read("02_PROCESSED_DATA/NRCAN/Canada_Municipal_Surface_Intake_V5_20230221_NonSnap.gpkg") %>%
    select(YR_VOL_M3) # last version of water licence layer
  hist_wat_use <- read_csv("02_PROCESSED_DATA/STATCAN/residentialwateruse_en_1991-2019.csv") # Historical domestic water use
  pop_proj_can_2060 <- read_csv("01_RAW_DATA/STATCAN/POPULATION_PROJECTIONS/1710005701-Canada.csv") # Projections canada population 2060
  # Fire probability data
  burn_pro_can <- rast("02_PROCESSED_DATA/NRCAN/BP3/BP_EPSG3979.tif") # BP3 burn probabilities
  burn_pro_us <- rast("01_RAW_DATA/USFS/CONUS_iBP.tif") # FSim burn probabilities for CONUS
  burn_pro_al <- rast("01_RAW_DATA/USFS/AK_iBP.tif") # FSim burn probabilities for CONUS
  # Fire intensity data
  # For US and Alaska, Intensity probabilities times their median flame length
  fire_int_can <- rast("02_PROCESSED_DATA/NRCAN/BP3/FI_EPSG3979.tif") # BP3 fire intensities
  fire_int_us <- c(rast("01_RAW_DATA/USFS/CONUS_iFLP1.tif")*1, rast("01_RAW_DATA/USFS/CONUS_iFLP2.tif")*3, 
                   rast("01_RAW_DATA/USFS/CONUS_iFLP3.tif")*5, rast("01_RAW_DATA/USFS/CONUS_iFLP4.tif")*7, 
                   rast("01_RAW_DATA/USFS/CONUS_iFLP5.tif")*10, rast("01_RAW_DATA/USFS/CONUS_iFLP6.tif")*14) # FSim fire intensity for CONUS
  fire_int_al <- c(rast("01_RAW_DATA/USFS/AK_iFLP1.tif")*1, rast("01_RAW_DATA/USFS/AK_iFLP2.tif")*3, 
                           rast("01_RAW_DATA/USFS/AK_iFLP3.tif")*5, rast("01_RAW_DATA/USFS/AK_iFLP4.tif")*7, 
                           rast("01_RAW_DATA/USFS/AK_iFLP5.tif")*10, rast("01_RAW_DATA/USFS/AK_iFLP6.tif")*14) # FSim fire intensity for Alaska
  # Rainfall erosivity data
  rain_ero_na <- rast("02_PROCESSED_DATA/ESDAC/Ero_Current_NA.tif") # Rainfall erosivity
  rain_ero_na_2050_rcp26 <- rast("02_PROCESSED_DATA/ESDAC/Ero_2050_rcp26_all.tif") # Rainfall erosivity 2050 rcp26
  rain_ero_na_2050_rcp45 <- rast("02_PROCESSED_DATA/ESDAC/Ero_2050_rcp45_all.tif") # Rainfall erosivity 2050 rcp45
  rain_ero_na_2050_rcp85 <- rast("02_PROCESSED_DATA/ESDAC/Ero_2050_rcp85_all.tif") # Rainfall erosivity 2050 rcp85
  
## data processing -------------------------------------------------------------
  
  # Adds new variables to HydroATLAS at the HUC12 level
  # All data must be in NAD83 Canada Atlas Lambert (EPSG:3979) before data extraction
  # So, make sure to check and reproject as necessary
  
  #########################################
  ####### Water demand (licence) ##########
  #########################################
  
  # Total water volume extraction per HUC12
  HUC12_WatDemand <- hybas_can %>%
    st_join(wat_demand) %>%
    group_by(HYBAS_ID) %>%
    summarise(TotMunWat = sum(YR_VOL_M3)) 
  
  HUC12_WatDemand_NoNA <- HUC12_WatDemand %>%
    mutate(TotMunWat = replace_na(TotMunWat,0)) %>%
    st_drop_geometry()
  
  hybas_can_WatDemand <- hybas_can %>%
    right_join(HUC12_WatDemand_NoNA) %>%
    rename(mwd_m3_syr = TotMunWat) # update name to match HydroATLAS format
    # mwd_m3_syr: municipal water demand, in cubic meters per year, within a catchment
  
  #########################################
  ######## Future water demand ############
  #########################################
  
  # Based on Warziniack 2022
  # => computes the growth rate of water consumption g
  # => computes the decay rate of growth d (slowing growth of consumption)
  # Uses Statcan population projection data
  # Uses low growth and high growth population scenario
  
  # 1) Create a water consumption time series
  wat_dates <- hist_wat_use%>%
    mutate(Year = ymd(Year, truncated = 2L)) # Convert years to date
  wat_dates_xts <- xts(wat_dates$Wat_Use, wat_dates$Year) #Convert dates to TS
  ggplot_na_distribution(wat_dates_xts) # Visualize TS with missing values
  imp_hist_wat_use <- na_ma(wat_dates_xts) # Imputation using moving average
  ggplot_na_imputations(wat_dates_xts, imp_hist_wat_use) # Visualize imputation
  
  imp_hist_wat_use_df <- as.data.frame(imp_hist_wat_use) # convert TS to df
  imp_hist_wat_use_df$Year <- rownames(imp_hist_wat_use_df) # Bring back dates as column
  imp_hist_wat_use_tib <- as_tibble(imp_hist_wat_use_df) %>%
    mutate(Wat_Vol = V1) %>%
    select(-V1) # Back to a clean tibble
  
  # 2) Compute growth rates based on water consumption time series
  growth_rate <- imp_hist_wat_use_tib %>% # Compute water consumption growth rate
    mutate(growth = (Wat_Vol - lag(Wat_Vol))/lag(Wat_Vol))
  g <- signif(mean(growth_rate$growth, na.rm = T), digits = 4) # -0.01608
  
  decay_rate <- growth_rate %>% # Compute decay rate of water consumption
    mutate(decay = (growth - lag(growth))/lag(growth))
  d <- signif(mean(decay_rate$decay, na.rm = T), digits = 4) # -0.5513
  
  # 3) Compute annual population growth rates
  # Low growth scenario
  pop_2060_low <- pop_proj_can_2060 %>%
    select(REF_DATE, VALUE, `Projection scenario`) %>%
    filter(grepl('LG',`Projection scenario` )) %>%
    filter(REF_DATE == 2021 | REF_DATE == 2060)
  # Annual growth => Total growth rate divided by the number of years (39)
  pop_growth_2060_low <- signif(((pop_2060_low$VALUE[2] - pop_2060_low$VALUE[1]) /
    pop_2060_low$VALUE[1])/39, digits = 4) # ~0.004
  # High growth scenario
  pop_2060_high <- pop_proj_can_2060 %>%
    select(REF_DATE, VALUE, `Projection scenario`) %>%
    filter(grepl('HG',`Projection scenario` )) %>%
    filter(REF_DATE == 2021 | REF_DATE == 2060)
  # Annual growth => Total growth rate divided by the number of years (39)
  pop_growth_2060_high <- signif(((pop_2060_high$VALUE[2] - pop_2060_high$VALUE[1]) /
    pop_2060_high$VALUE[1])/39, digits = 4) # 0.0186
  
  # 4) Computes 2060 water demand
  # Based on population changes and modulation in water demand
  Pop_WatDemand_2060 <- hybas_can_WatDemand %>%
    select(HYBAS_ID, mwd_m3_syr) %>%
    mutate(WatDemand_2060_LG = mwd_m3_syr * (1+pop_growth_2060_low*(1+g*(1+d)^1)^39)^39) %>%
    mutate(WatDemand_2060_HG = mwd_m3_syr * (1+pop_growth_2060_high*(1+g*(1+d)^1)^39)^39) %>% 
    arrange(desc(WatDemand_2060_LG))
  
  # 5) Bring new data to master dataset
  hybas_can_WatDemand_2060 <- hybas_can_WatDemand %>%
    right_join(st_drop_geometry(Pop_WatDemand_2060)) %>%
    rename(mwd_m3_syr_lg60 = WatDemand_2060_LG,
           mwd_m3_syr_hg60 = WatDemand_2060_HG) # matches HydroATLAS format
    
  #########################################
  ######## Median fire intensity ##########
  #########################################
  
  # 1) Prepare US intensity layers
  # CONUS
  fire_int_us_avg <- app(fire_int_us, fun="mean", na.rm = T) # mean of 6 fire intensity layers
  fire_int_us_avg_m <- fire_int_us_avg / 0.3048 # Convert feet to meters
  fire_int_us_kWm <- 273*(fire_int_us_avg_m)^2.17 # Convert flame length to intensity
  # 270m to 250m pixel resolution and EPSG:3979
  fire_int_us_EPSG3979 <- terra::project(fire_int_us_kWm, 
                                         crs(fire_int_can), 
                                         res = res(fire_int_can),
                                         filename = "02_PROCESSED_DATA/USFS/CONUS_FI_250m_EPSG3979.tif",
                                         overwrite = T)
  # Alaska
  fire_int_al_avg <- app(fire_int_al, fun="mean", na.rm = T) # mean of 6 fire intensity layers
  fire_int_al_avg_m <- fire_int_al_avg / 0.3048 # Convert feet to meters
  fire_int_al_kWm <- 273*(fire_int_al_avg_m)^2.17 # Convert flame length to intensity
  # 270m to 250m pixel resolution and EPSG:3979
  fire_int_al_EPSG3979 <- terra::project(fire_int_al_kWm, 
                                         crs(fire_int_can), 
                                         res = res(fire_int_can),
                                         filename = "02_PROCESSED_DATA/USFS/AL_FI_250m_EPSG3979.tif",
                                         overwrite = T)
  
  # 2) Merge fire intensity layers
  # Load existing layers if no need to run above steps
  # fire_int_us_EPSG3979 <- rast("02_PROCESSED_DATA/USFS/CONUS_FI_250m_EPSG3979.tif")
  # fire_int_al_EPSG3979 <- rast("02_PROCESSED_DATA/USFS/AL_FI_250m_EPSG3979.tif")
  fire_int_na <- terra::merge(fire_int_can, fire_int_us_EPSG3979, fire_int_al_EPSG3979,
                              first = T,
                              filename = "02_PROCESSED_DATA/NRCAN/NA_FI_250m_EPSG3979_Terra.tif",
                              overwrite = T)
  
  # 3) fill in the holes in Canadian prairies and North with 0s (no or insignificant fire activity)
  na_background <- vect("02_PROCESSED_DATA/US_GOV/US_CAN_Borders_Dissolve_FixGeom_Lines.gpkg") # Vector of North America
  na_background_rast <- rasterize(na_background, fire_int_na, field = "Value") # Convert vector to 250m raster, with pixel value of 0
  potholes_background <- vect("02_PROCESSED_DATA/BIRDS_CANADA/Prairie_Pothole_Region.gpkg") # Prairie pothole region
  potholes_background_rast <- rasterize(potholes_background, fire_int_na, field = 0) # Convert vector to 250m raster, with pixel value of 0
  
  fire_int_na_filled <- terra::merge(potholes_background_rast, fire_int_na, na_background_rast,
                                     first = T,
                                     filename = "02_PROCESSED_DATA/NRCAN/NA_FI_250m_EPSG3979_Terra_Filled.tif",
                                     overwrite = T)
  
  # 4) Extract fire intensities in HUC12
  hybas_fireint_med <- exact_extract(fire_int_na_filled, hybas_can, fun = 'median',
                                     progress = T, append_cols = c('HYBAS_ID')) %>% 
    rename(fi_kw_syr = median) %>% # update name to match HydroATLAS format
    mutate(fi_kw_syr = replace_na(fi_kw_syr, 0)) %>% # replace NAs to 0
    mutate(fi_kw_syr = round(fi_kw_syr, 4)) # round values to 4 decimals
  
  hybas_can_updated_FI <- hybas_can_WatDemand_2060 %>%
    right_join(hybas_fireint_med)
  
  #########################################
  ####### Median burn probability #########
  #########################################
  
  # 1) Prepare US probability layers
  # 270m to 250m pixel resolution and EPSG:3979
  burn_pro_us_EPSG3979 <- terra::project(burn_pro_us, 
                                         crs(burn_pro_can), 
                                         res = res(burn_pro_can),
                                         filename = "02_PROCESSED_DATA/USFS/CONUS_iBP_250m_EPSG3979_Terra.tif",
                                         overwrite = T)
  burn_pro_al_EPSG3979 <- terra::project(burn_pro_al, 
                                         crs(burn_pro_can), 
                                         res = res(burn_pro_can),
                                         filename = "02_PROCESSED_DATA/USFS/AL_iBP_250m_EPSG3979_Terra.tif",
                                         overwrite = T)
  
  # 2) Merge burn probability layers
  burn_pro_na <- terra::merge(burn_pro_can, burn_pro_us_EPSG3979, burn_pro_al_EPSG3979,
                              first = T,
                              filename = "02_PROCESSED_DATA/NRCAN/NA_BP_250m_EPSG3979_Terra.tif",
                              overwrite = T)
  
  # 3) fill in the holes in Canadian prairies and North with 0s (no or insignificant fire activity)
  na_background <- vect("02_PROCESSED_DATA/US_GOV/US_CAN_Borders_Dissolve_FixGeom_Lines.gpkg") # Vector of North America
  na_background_rast <- rasterize(na_background, fire_int_na, field = "Value") # Convert vector to 250m raster, with pixel value of 0
  potholes_background <- vect("02_PROCESSED_DATA/BIRDS_CANADA/Prairie_Pothole_Region.gpkg") # Prairie pothole region
  potholes_background_rast <- rasterize(potholes_background, fire_int_na, field = 0) # Convert vector to 250m raster, with pixel value of 0
  
  burn_pro_na_filled <- terra::merge(potholes_background_rast, burn_pro_na, na_background_rast,
                                     first = T,
                                     filename = "02_PROCESSED_DATA/NRCAN/NA_BP_250m_EPSG3979_Terra_Filled.tif",
                                     overwrite = T) 
  
  # 4) Extract burn probabilities in HUC12
  hybas_burnpro_med <- exact_extract(burn_pro_na_filled, hybas_can, fun = 'median',
                                     progress = T, append_cols = c('HYBAS_ID')) %>% 
    rename(fp_ul_syr = median) %>% # update name to match HydroATLAS format
    mutate(fp_ul_syr = replace_na(fp_ul_syr, 0)) %>% # replace NAs to 0
    mutate(fp_ul_syr = round(fp_ul_syr, 4)) # round values to 4 decimals
  
  hybas_can_updated_BP <- hybas_can_updated_FI %>%
    right_join(hybas_burnpro_med)
  
  #########################################
  ####### Mean rainfall erosivity #######
  #########################################
  
  # 1) Project erosivity layer
  rain_ero_na_EPSG3979 <- terra::project(rain_ero_na,
                                         crs(burn_pro_can),
                                         res = 1000, # 1km pixel resolution
                                         filename = "02_PROCESSED_DATA/ESDAC/rain_ero_na_EPSG3979.tif",
                                         overwrite = T)
                                         
  # 2) Fill the value holes in the Arctic with 0s (no or insignificant fire activity, or erosivity)
  # Load existing layers if no need to run above steps
  # rain_ero_na_EPSG3979 <- rast("02_PROCESSED_DATA/ESDAC/rain_ero_na_EPSG3979.tif")
  na_background <- vect("02_PROCESSED_DATA/US_GOV/US_CAN_Borders_Dissolve_FixGeom_Lines.gpkg") # Vector of North America
  na_background_rast_ero <- rasterize(na_background, rain_ero_na_EPSG3979, field = 249.54) # Convert vector to raster
  # The above layer uses the mean erosivity value of the four northermost HUC3 basins with values
  rain_ero_na_filled <- terra::merge(rain_ero_na_EPSG3979, na_background_rast_ero,
                                     first = T,
                                     filename = "02_PROCESSED_DATA/ESDAC/NA_erosivity_Current_Filled.tif",
                                     overwrite = T)
    
  # 3) Extract erosivity values per HUC12
  # Load existing layers if no need to run above steps
  # rain_ero_na_filled <- rast("02_PROCESSED_DATA/ESDAC/NA_erosivity_Current_Filled.tif")
  hybas_ero_avg <- exact_extract(rain_ero_na_filled, hybas_can, fun = 'mean',
                                     progress = T, append_cols = c('HYBAS_ID')) %>% 
    rename(re_th_syr = mean) %>% # update name to match HydroATLAS format
    mutate(re_th_syr = replace_na(re_th_syr,0)) %>% # replace NAs to 0
    mutate(re_th_syr = round(re_th_syr, 4)) # round values to 4 decimals
  
  hybas_can_updated_ero <- hybas_can_updated_BP %>%
    right_join(hybas_ero_avg)
  
  #########################################
  #### Future mean rainfall erosivity #####
  #########################################
  
  # 1) Project future erosivity layers
  rain_ero_na_rcp26_EPSG3979 <- terra::project(rain_ero_na_2050_rcp26,
                                         crs(burn_pro_can),
                                         res = 1000, # 1km pixel resolution
                                         filename = "02_PROCESSED_DATA/ESDAC/rain_ero_na_rcp26_EPSG3979.tif",
                                         overwrite = T)
  
  rain_ero_na_rcp45_EPSG3979 <- terra::project(rain_ero_na_2050_rcp45,
                                               crs(burn_pro_can),
                                               res = 1000, # 1km pixel resolution
                                               filename = "02_PROCESSED_DATA/ESDAC/rain_ero_na_rcp45_EPSG3979.tif",
                                               overwrite = T)
  
  rain_ero_na_rcp85_EPSG3979 <- terra::project(rain_ero_na_2050_rcp85,
                                               crs(burn_pro_can),
                                               res = 1000, # 1km pixel resolution
                                               filename = "02_PROCESSED_DATA/ESDAC/rain_ero_na_rcp85_EPSG3979.tif",
                                               overwrite = T)
  
  # 2) Extract future erosivity values per HUC12
  # Load existing layers if no need to run above steps
  hybas_ero_avg_rcp26 <- exact_extract(rain_ero_na_rcp26_EPSG3979, hybas_can, fun = 'mean',
                                 progress = T, append_cols = c('HYBAS_ID')) %>% 
    rename(re_th_s26 = mean) %>% # update name to match HydroAtlas format
    mutate(re_th_s26 = replace_na(re_th_s26,0)) %>% # replace NAs to 0
    mutate(re_th_s26 = round(re_th_s26, 4)) # round values to 4 decimals
  
  hybas_ero_avg_rcp45 <- exact_extract(rain_ero_na_rcp45_EPSG3979, hybas_can, fun = 'mean',
                                         progress = T, append_cols = c('HYBAS_ID')) %>% 
    rename(re_th_s45 = mean) %>% # update name to match HydroAtlas format
    mutate(re_th_s45 = replace_na(re_th_s45,0)) %>% # replace NAs to 0
    mutate(re_th_s45 = round(re_th_s45, 4)) # round values to 4 decimals
  
  hybas_ero_avg_rcp85 <- exact_extract(rain_ero_na_rcp85_EPSG3979, hybas_can, fun = 'mean',
                                         progress = T, append_cols = c('HYBAS_ID')) %>% 
    rename(re_th_s85 = mean) %>% # update name to match HydroAtlas format
    mutate(re_th_s85 = replace_na(re_th_s85,0)) %>% # replace NAs to 0
    mutate(re_th_s85 = round(re_th_s85, 4)) # round values to 4 decimals
  
  hybas_can_updated_ero_2050 <- hybas_can_updated_ero %>%
    right_join(hybas_ero_avg_rcp26) %>%
    right_join(hybas_ero_avg_rcp45) %>%
    right_join(hybas_ero_avg_rcp85)

## Save output -----------------------------------------------------------------

  # Once all data summarized on HUC12
  # Save data as geopackage
  st_write(hybas_can_updated_ero_2050,"02_PROCESSED_DATA/HYDROLAB_MCGILL/HydroBasins_HUC3_WaterSupply_HUC12_RepairedGeometry_EPSG3979_UpdatedVariables.gpkg",
           delete_layer = T)
  