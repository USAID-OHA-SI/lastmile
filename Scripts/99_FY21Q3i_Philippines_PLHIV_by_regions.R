##  PROJECT: Q2 Review target analysis
##  AUTHOR:  B.Kagniniwa & G.Sarfaty | USAID
##  PURPOSE: Geo-depiction of TX_ML [% of patients transferring out]
##  LICENCE: MIT
##  DATE:    2020-08-23

# Dependancies ----

  library(tidyverse)
  library(readxl)
  library(gophr)
  library(sf)
  library(gisr)
  library(glamr)
  library(glitr)
  library(here)
  library(scales)
  library(RColorBrewer)
  library(patchwork)
  library(extrafont)
  library(glue)

# GLOBALS ----

  ## Data & Output folders
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"
  dir_geodata <- si_path("path_vector")
  dir_merdata <- si_path("path_msd")
  dir_terr <- si_path("path_raster")

  ## GDRIVE
  gdrive_tx_ml <- "1HVtUJZskoWnNN2lcnIhKmEwgrfy5rK0z"

  # MER Data - get the latest MSD PSNU x IM file
  file_nat <- return_latest(
    folderpath = dir_merdata,
    pattern = "^MER_.*_NAT_SUBNAT_.*_\\d{8}_v\\d{1}_\\d{1}.zip$",
    recursive = FALSE
  )

  # MSD File version
  rep_pd <- file_nat %>% identify_pd()

  msd_version <- ifelse(str_detect(file_nat, ".*_\\d{8}_v1_\\d"), "i", "c")

  dir_graphics %<>% paste0("/", rep_pd, msd_version)

  gdrive_dir <- rep_pd %>%
    paste0(msd_version)

  # Shapefile path
  file_shp <- return_latest(
    folderpath = dir_geodata,
    pattern = "VcPepfarPolygons.*.shp",
    recursive = TRUE
  )

# DATA ----

  ## PSNUxIMs
  df_nat <- file_nat %>% read_msd()

  ## Raster data
  terr <- gisr::get_raster(terr_path = dir_terr)

  ## PEPFAR Boundaries
  spdf_pepfar <- file_shp %>% sf::read_sf()

  df_attrs <- gisr::get_ouuids() %>%
    filter(!str_detect(operatingunit, " Region$")) %>%
    pull(operatingunit) %>%
    map_dfr(.x, .f = ~get_attributes(country = .x))

  spdf_pepfar <- spdf_pepfar %>%
    left_join(df_attrs, by = c("uid" = "id"))

# MUNGE ----

  # Country name
  cntry <- "Philippines"

  df_plhiv <- df_nat %>%
    filter(countryname == cntry,
           indicator == "PLHIV")



# VIZ ----

  cntry %>%
    get_admin0() %>%
    gview()

  cntry %>%
    get_admin1() %>%
    gview()

  cntry %>%
    get_admin1() %>%
    dview()

  basemap <- terrain_map(cntry, terr = terr, mask = TRUE)
