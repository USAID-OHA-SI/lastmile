##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa, G.Sarfaty & T.Essam | USAID
##  PURPOSE: Geo-depiction of VL - % not covered
##  LICENCE: MIT
##  DATE:    2020-12-07


# LIBRARIES --------------------------------

  library(tidyverse)
  library(vroom)
  library(sf)
  library(sp)
  library(raster)
  library(gisr)
  library(glitr)
  library(glamr)
  library(janitor)
  library(scales)
  library(patchwork)
  library(here)
  library(ICPIutilities)
  library(extrafont)

# REQUIRED -----------------------------------

  ## Dependencies
  source("./Scripts/00_Utilities.R")
  source("./Scripts/00_Geo_Utilities.R")
  source("./Scripts/00_VL_Utilities.R")

# GLOBALS ------------------------------------

  ## Data & Output folders
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"
  dir_geodata <- "../../GEODATA/PEPFAR"
  dir_terr <- "../../GEODATA/RASTER"
  dir_merdata <- "../../MERDATA"

  ## Reporting Filters
  rep_agency <- c("USAID", "HHS/CDC")

  rep_fy = c(2019, 2020)

  rep_pd = "FY20Q4"

  caption = " - Data Source: FY20Q4i MSD, VLS = (TX_PVLS_N/TX_PVLS_D)
    ARV Disp. adjusted for Months of Treatments based on pill count
    OHA/SIEI - Produced on "

  ## Q4 MER Data
  psnu_im <- "^MER_.*_PSNU_IM_.*_20201113_v1_1.zip$"

  ## File path + name
  file_psnu_im <- list.files(
      path = dir_merdata,
      pattern = psnu_im,
      recursive = TRUE,
      full.names = TRUE
    ) %>%
    sort() %>%
    last()

# DATA ---------------------------------------

  ## MSD PSNU x IM
  df_psnu <- file_psnu_im %>%
    read_msd()

  ## Geodata

  ## Terrain Raster
  #terr <- get_raster(terr_path = dir_terr)

  ## ORGs
  spdf_pepfar <- build_spdf(
    dir_geo = paste0(dir_geodata, "/VcPepfarPolygons_2020.07.24"),
    df_psnu = df_psnu
  )

  ## VLS & TLD
  df_vls_tld <- extract_vls_tld(df_msd = df_psnu,
                                rep_agency = rep_agency,
                                rep_fy = rep_fy,
                                rep_pd = rep_pd)


# VIZ -----------------

  # single country
  cname <- "Zambia"

  viz_vls_tld(df_vl = df_vls_tld,
              spdf = spdf_pepfar,
              terr_path = dir_terr,
              country = cname,
              caption = caption,
              save = FALSE)

  # Batch
  df_vls_tld %>%
    filter(!is.na(VLS), !str_detect(operatingunit, " Region")) %>%
    distinct(operatingunit) %>%
    pull(operatingunit) %>%
    map(.x, .f = ~ viz_vls_tld(df_vl = df_vls_tld,
                               spdf = spdf_pepfar,
                               terr_path = dir_terr,
                               country = .x,
                               caption = caption,
                               save = TRUE))


