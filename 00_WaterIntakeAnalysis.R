## ---------------------------
## Script name: 00_WaterIntakeAnalysis.R
##
## Purpose of script: Prepare water licence file (i.e., intakes) for analysis 
## and use in the creation of the F2F-C composite indicator. It mixes sources
## of data so water intakes have attributes related to province, population, 
## community, water source type.
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
## Notes: 
## 

## Set working directory -------------------------------------------------------

  # Update as needed
  setwd("C:/Users/frobinne/Documents/Professionel/PROJECTS/39_2021_CANADA_F2F_SOURCE2TAP_ACTIVE")

## General options -------------------------------------------------------------
options(scipen = 100)
options(digits = 3)

## Load packages ---------------------------------------------------------------

  library(sf)
  library(tidyverse)
  library(readr)

## Load data -------------------------------------------------------------------
  # Water Intakes (not open access)
  Intakes <- st_read("02_PROCESSED_DATA/NRCAN/Canada_Municipal_Surface_Intake_V1_13102021.shp") %>%
    mutate(YR_VOL_M3 = as.numeric(YR_VOL_M3))
  # Census 2016 population table
  Pop_2016 <- read_csv("01_RAW_DATA/STATCAN/Census_2016_Population_Subdivision.CSV", 
                        locale = locale(encoding = "latin1")) %>%
    slice(0:5162) # Get read of the notes at the bottom of the file
                    # Ignore the import error message
  # This is Inuit Nunangat, the Inuit territory
  # See source: 
  # https://open.canada.ca/data/en/dataset/f242b881-75e3-40bb-a148-63410b4ce2af
  Nunangat <- st_read("01_RAW_DATA/Region_inuite_Inuit_Region_SHP/Inuit_Region_Region_inuite.shp") %>%
    st_transform(crs = 3979) 
                              
## Processing ------------------------------------------------------------------
  
  # 1) Add population to each community ----------------------------------------
    
  # Creates a condensed version of the intake table for join with census data
  # Group by census subdivision; add sum of volume per subdivision
  Vol_Per_Subdiv <- st_drop_geometry(Intakes) %>%
    group_by(CENS_COD) %>%
    summarise(TOTAL_VOL = sum(YR_VOL_M3), CNT = n())
    
  # Left join on volume per subdivision and census data
  Vol_Join_Cens <- left_join(Vol_Per_Subdiv, Pop_2016 %>%
                                dplyr::select("Geographic code",
                                               "Geographic name, english",
                                               "CSD type, english", 
                                               "Population, 2016",
                                               "Province / territory, english"),
                               by = c("CENS_COD" = "Geographic code")) %>%
    rename(CSD_TYPE = "CSD type, english", POP_2016 = "Population, 2016", 
             PROV_TER = "Province / territory, english", 
             NAME_COM = "Geographic name, english")
  
  
  # 2) Compute water volume based on population for QC and NB ------------------
    
  # Average daily water demand standard equation = 225 L/capita/day + 10%
  # NB Average daily residential use per capita of the population served: 265
  # QC Average daily residential use per capita of the population served: 526
  Water_Supply <- function(pop, ADD){ 
    avg_daily_demand <- ((ADD * pop) + (ADD * pop * 0.1))
    avg_yearly_demand <- avg_daily_demand * 365
    return(as.numeric(round(avg_yearly_demand, digits = 0))*0.001)
  }
    
  # New Brunswick
  # Computes theoretical water volume per NB's water licence
  Vol_Join_Cens_NB <- Vol_Join_Cens %>%
    filter(PROV_TER == "New Brunswick") %>%
    mutate(TOTAL_VOL = Water_Supply(pop = POP_2016, ADD = 265)) %>%
    mutate(VOL_M3 = TOTAL_VOL/CNT)
  
  # Quebec
  # Computes theoretical water volume for QC's communities
  Vol_Join_Cens_QC <- Vol_Join_Cens %>%
    filter(PROV_TER == "Quebec") %>%
    mutate(TOTAL_VOL = Water_Supply(pop = POP_2016, ADD = 526)) %>%
    mutate(VOL_M3 = TOTAL_VOL/CNT)
  # Translate CSD types to English
  # List existing types
  List_CSD_Type <- Vol_Join_Cens %>%
    group_by(CSD_TYPE) %>%
    summarize(cnt = n())
  
  Vol_Join_Cens_QC <- Vol_Join_Cens_QC %>%
    mutate(CSD_TYPE = case_when(
      CSD_TYPE == "Canton (municipalité de)" ~ "County (municipality of)",
      CSD_TYPE == "Cantons unis (municipalité de)" ~ "United counties (municipality of)",
      CSD_TYPE == "Paroisse (municipalité de)" ~ "Parish",
      CSD_TYPE == "Ville" ~ "City",
      CSD_TYPE == "Municipalité" ~ "Municipality",
      CSD_TYPE == "Village" ~ "Village"
    ))
  
  # Aggregate all records (QC, NB, others) together
  Vol_Join_Others <- Vol_Join_Cens %>%
    filter(PROV_TER != "Quebec" & PROV_TER != "New Brunswick") %>%
    mutate(VOL_M3 = 0)

  Vol_Join_Cens_Ready <- bind_rows(Vol_Join_Cens_NB, 
                                   Vol_Join_Cens_QC,
                                   Vol_Join_Others)
  
  
  # 3) Update First Nations flag ----------------------------------------------- 
  
  # For First Nations, Metis, and Inuit
  # This will come on top of existing FN flag in the raw water licence file
  Vol_Join_Cens_Ready_FN <- Vol_Join_Cens_Ready %>%
    mutate(FIRST_NAT_D = case_when(
      CSD_TYPE == "Indian reserve" ~ "Y",
      CSD_TYPE == "Indian Settlement" ~ "Y",
      CSD_TYPE == "Northern hamlet" ~ "Y",
      CSD_TYPE == "Northern village" ~ "Y"
    )) # First Nation flag updated
  
  # Inuit communities don't have specific CSD code, so use the Inuit agreement shapefile
  Nunangat_buff <- Nunangat %>%
    st_union() %>%
    st_buffer(dist = 25000, nQuadSegs = 50, endCapStyle = 'ROUND', joinStyle = 'ROUND')
  # Creates a condensed Intake layer. Use only intended for this FN-update process
  Intakes_Others <- Intakes %>%
    group_by(CENS_COD) %>%
    summarise()
  
  Vol_Join_Cens_Ready_FN <- Vol_Join_Cens_Ready_FN %>%
    mutate(FIRST_NAT_D = case_when(
      (st_intersects(Intakes_Others, Nunangat_buff, sparse = F) == T) ~ "Y",
      TRUE ~ FIRST_NAT_D
    ))
  
  
  # 4) Join condensed community/volume data to original intake data ------------
  
  Intakes_Vol_Update <- Intakes %>%
    select(-c(COM_NAME_S, COM_NAME_L, POP_2016, POP_ADJ, TL_COM_VOL,
              RT_COM_VOL, layer, path)) %>% # drop fields (either redundant or useless)
    left_join(Vol_Join_Cens_Ready_FN, by = c("CENS_COD")) %>%
    mutate(YR_VOL_M3 = case_when(VOL_M3 != 0 ~ VOL_M3,
                                 TRUE ~ YR_VOL_M3))

  # Save created layer as shapefile before further manipulation
  # st_write(Intakes_Vol_Update, "02_PROCESSED_DATA/NRCAN/Canada_Municipal_Surface_Intake_V2_22102021.gpkg",
  #          append=FALSE)
  

  # 5) Finish water intake table -----------------------------------------------
  
  Intakes_Vol_Final <- Intakes_Vol_Update %>%
    mutate(TL_COM_VOL = round(TOTAL_VOL, digits = 0)) %>%
    mutate(FIRST_NAT = case_when(FIRST_NAT_D == "Y" ~ "Y",
                                 FIRST_NAT == "Y" ~ "Y",
                                 TRUE ~ "N")) %>%
    select(-c(PROV_TER, FIRST_NAT_D, TOTAL_VOL, CNT, VOL_M3, TL_COM_VOL)) %>%
    relocate(POD_ID, LIC_ID, YR_VOL_M3, VOL_ACQ, SRCE_TYPE, NAME_COM, CSD_TYPE, 
             CENS_COD, POP_2016, FIRST_NAT, PROV_TERR, POINT_MOVE, DATA_SRCE, 
             CONF_LVL)
  
  # Save new water table
  # Don't forget to update version if necessary
  st_write(Intakes_Vol_Final, "02_PROCESSED_DATA/NRCAN/Canada_Municipal_Surface_Intake_V4_28102021.gpkg",
           append=FALSE)  # Ignore warnings
  

## Data exploration ------------------------------------------------------------

  # Make sure the right licence file is uploaded
  # Load right one
  licence_df <- st_read("C:/Users/frobinne/OneDrive - NRCan RNCan/Documents/Professionel/PROJECTS/39_2021_CANADA_F2F_SOURCE2TAP_ACTIVE/02_PROCESSED_DATA/NRCAN/Canada_Municipal_Surface_Intake_V4_28102021.gpkg")
  # Source type per province/territory
  ggplot(licence_df, aes(x = PROV_TERR, fill = SRCE_TYPE)) +
    geom_bar() +
    scale_fill_brewer(palette = "Spectral")
  
  # SVolume per province/territory
  Vol_tot_prov <- licence_df %>%
    group_by(PROV_TERR) %>%
    summarise(Tot_Vol = sum(YR_VOL_M3)) #%>%
    #mutate(Ln_Tot_Vol = log(Tot_Vol))
  
  ggplot(Vol_tot_prov, aes(x = PROV_TERR, y = Ln_Tot_Vol)) +
    geom_bar(stat = "identity")
  
  # Location of First Nations licences
  ggplot(licence_df) +
    geom_sf(aes(color = FIRST_NAT))
  