## ---------------------------
## Script name: 05_DownstreamRouting_SfNetworks.R
##
## Purpose of script: Creates an OD matrix over the HydroRIVERS network to route
##                    wildfire and erosivity variables (n=5) downstream
##
## Author: Dr. Francois-Nicolas Robinne
## 
## Version: 1.0
##
## Date Created: 2023-04-26
##
## Copyright (c) Francois-Nicolas Robinne, 2023
## Email: francois.robinne@nrcan-rncan.gc.ca or robinne@ualberta.ca
##
## ---------------------------
##
## Notes: Based principally on the creation of an origin-destination matrix
##        It uses the HydroRIVERS network. Sfnetworks uses the shortest path.
##   

## Set working directory -------------------------------------------------------
# setwd("C:/Users/frobinne/Documents/Professionel/39_2021_CANADA_F2F_SOURCE2TAP_ACTIVE/03_ANALYSIS_RESULTS")

# List the directories within which the network analysis will be applied
dirs <- list.dirs("C:/Users/frobinne/Documents/Professional/PROJECTS/39_2021_CANADA_F2F_SOURCE2TAP_ACTIVE/03_ANALYSIS_RESULTS/Canada_Forest2Faucets", 
                  recursive = T)
dirs <- dirs[-1] # Removes the first entry, which is the root of the folder

## Load libraries --------------------------------------------------------------
library(sfnetworks)
library(sf)
sf::sf_use_s2(FALSE)
library(tidygraph)
library(tidyverse)
library(igraph)
library(dbscan)
library(units)
library(tidyr)

## Network analysis loop -------------------------------------------------------

for(i in dirs) {
  setwd(i)
  
  ## Load data -----------------------------------------------------------------
  hyriv <- st_read("hyriv_Valid.gpkg", as_tibble = T, fid_column_name = "IDS")
  outlets <- st_read("outlets_Valid.gpkg")
  hybas <- st_read("huc12_Valid.gpkg")
  
  ## Data processing -----------------------------------------------------------
  
  ##########################
  # Fire intensity routing #
  ##########################
  
  # Reformat HUC12 outlets
  dir_outlets <- st_join(outlets, hybas)%>%
    st_cast(to = "POINT") %>%
    select(HYBAS_ID.x, NEXT_DOWN, fi_kw_syr) %>%
    rename(from = HYBAS_ID.x, to = NEXT_DOWN) %>%
    mutate(name = as.character(from))
  
  hybas_FI <- hybas %>%
    select(fi_kw_syr)
  
  # Reformat HydroRIVERS
  hyriv_line <- st_cast(hyriv, to = "LINESTRING") %>%
    st_join(hybas_FI, join = st_intersects) # Add fire intensity info
  
  # Create a network based on HydroRIVERS
  net <- as_sfnetwork(hyriv_line, directed = T)
  # Update river network with outlets
  new_net <- net %>% # Original river network
    activate("nodes") %>%
    st_network_blend(dir_outlets, tolerance = Inf) %>% # Blend in catchment outlets
    convert(to_spatial_subdivision) %>%
    activate("edges") %>%
    mutate(weight = fi_kw_syr) # Use fire intensity as the weight for each river reach
  # Create origin-destination (OD) matrix (distance-based, here)
  POI_outlets <- st_as_sf(new_net, "nodes") %>%
    filter(is.na(name) == F) # Recreate the outlet layer based on their new position after blending (POIs)
  od_mat <- st_network_cost(new_net, 
                            from = POI_outlets, to = POI_outlets, # from-to point sf
                            direction = "in", # inbound
                            Inf_as_NaN = T)
  
  od_mat_tibble <- tibble::as_tibble(od_mat, rownames = "id", .name_repair = "minimal") # converts matrix to tibble and add HYBAS_ID as column
  od_mat_tibble <- rename(od_mat_tibble, HYBAS_ID = id) # Rename id field to HYBAS_ID for join
  
  # Total downstream fire intensity accumulation
  # Row-wise aggregation (upstream looking downstream, i.e., accumulation)
  routed_FI <- od_mat_tibble %>%
    mutate(fi_kw_uyr = rowSums(pick(where(is.numeric), -HYBAS_ID), na.rm = T)) %>%
    mutate(HYBAS_ID = as.numeric(HYBAS_ID)) %>%
    select(HYBAS_ID, fi_kw_uyr)
  
  # Create spatial layers
  hybas_routed_FI <- inner_join(hybas, routed_FI)
  
  ## Save outputs -------------------------------------------------------------
  st_write(hybas_routed_FI, "huc12_Valid_FI.gpkg", delete_layer = T)
  
  ## Clear memory
  gc(verbose = T, reset = T, full = T)
}