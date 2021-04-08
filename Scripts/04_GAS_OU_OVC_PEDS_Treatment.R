##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: PEDS with no access to OVC Services
##  LICENCE: MIT
##  DATE:    2021-04-07


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(glitr)
  library(glamr)
  library(gisr)
  library(ICPIutilities)
  library(sf)
  library(scales)
  library(ggtext)
  library(ggrepel)
  library(tidytext)
  library(patchwork)
  library(glue)

  # Code re-use: placeholder till migration to one of the our packages
  source("./Scripts/00_Geo_Utilities.R")
  source("./Scripts/00_OVC_Utilities.R")

# SETUP ----

  # Project specific folders
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"

  # Ref folders outside of project
  dir_geodata <- si_path("path_vector")
  dir_attrs <- file.path(dir_geodata, "OU-Attributes")
  dir_terr <- si_path("path_raster")
  dir_merdata <- si_path("path_msd")

  # GLOBAL VARS

  # Reporting Filters
  rep_agency = "USAID"
  rep_agencies <- c("USAID", "HHS/CDC")

  rep_fy = 2020
  rep_qtr = 4

  rep_pd = rep_fy %>%
    as.character() %>%
    str_sub(3,4) %>%
    paste0("FY", ., "Q", rep_qtr)


  # MER Data - get the latest MSD PSNU x IM file
  file_psnu_im <- return_latest(
    folderpath = dir_merdata,
    pattern = "^MER_.*_PSNU_IM_.*_\\d{8}_v\\d{1}_1.zip$",
    recursive = FALSE
  )

  # Shapefile path
  file_shp <- return_latest(
    folderpath = dir_geodata,
    pattern = "VcPepfarPolygons.*.shp",
    recursive = TRUE
  )

# Import Data ----

  # MER Data
  df_psnu <- file_psnu_im %>% read_msd()

  # Geo Data
  terr <- gisr::get_raster(terr_path = dir_terr)

  spdf_pepfar <- file_shp %>% sf::read_sf()

  # Ou / Countries
  ous <- gisr::get_ouuids(add_details = TRUE)

  # Extract Orgs Hierarchy
  # ous %>%
  #   filter(!str_detect(operatingunit, " Region$")) %>%
  #   pull(countryname) %>%
  #   map(.x, .f = ~extract_attributes(country = .x, folderpath = dir_attrs))
  #
  extract_attributes(country = "Nigeria")
  extract_attributes(country = "Nigeria", folderpath = "./Dataaa")
  extract_attributes(country = "Nigeria", folderpath = dir_attrs)

  #get_attributes(country = "Nigeria", folderpath = "./Data")
  get_attributes(country = "Nigeria", folderpath = dir_attrs)
  #get_attributes(country = "Nigeria")

  df_attrs <- gisr::get_ouuids() %>%
    filter(!str_detect(operatingunit, " Region$")) %>%
    pull(operatingunit) %>%
    map_dfr(.x, .f = ~get_attributes(country = .x, folderpath = dir_attrs))

  spdf_pepfar <- spdf_pepfar %>%
    left_join(df_attrs, by = c("uid" = "id"))

# MUNGE ----

  ## Proxy OVC Coverage
  df_ovc <- extract_ovc_coverage(df_msd = df_psnu,
                                     rep_fy = rep_fy,
                                     rep_age = "<20",   #age_group, #<15 or <20
                                     rep_agency = NULL, #rep_agency,
                                     rep_pd = rep_pd,
                                     sumlevel = "PSNU") # PSNU or SNU1


  ## OVC & TX Overlap
  df_tx <- extract_ovc_tx_overlap(df_msd = df_psnu,
                                      rep_fy = rep_fy + 1,
                                      rep_agency = rep_agencies)