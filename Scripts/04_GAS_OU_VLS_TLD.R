##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa, G.Sarfaty & T.Essam | USAID
##  PURPOSE: Geo-depiction of VL - % not covered
##  LICENCE: MIT
##  DATE:    2020-12-07
##  UPDATED: 2021-12-03


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
  library(gophr)
  library(extrafont)
  library(glue)

# REQUIRED -----------------------------------

  ## Dependencies
  source("./Scripts/00_Utilities.R")
  #source("./Scripts/00_Geo_Utilities.R")
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

  gdrive_tx_tld <- "1n66I1qe_9GH93MxSjq-Hke9hwBIn01tJ"

  ## Reporting Filters
  rep_agency <- c("USAID", "HHS/CDC")

  rep_fy = 2021
  rep_fys = rep_fy %>% c(.-1, .)

  rep_qtr = 4

  rep_pd = rep_fy %>%
    as.character() %>%
    str_sub(3, 4) %>%
    paste0("FY", ., "Q", rep_qtr)

  ## MSD Data file
  psnu_im <- "^MER_.*_PSNU_IM_.*_\\d{8}_v\\d{1}_\\d{1}.zip$"

  ## File path + name
  file_psnu_im <- return_latest(
    folderpath = dir_merdata,
    pattern = psnu_im,
    recursive = TRUE
  )

  ## File path + name
  file_shp <- return_latest(
    folderpath = dir_geodata,
    pattern = "VcPepfarPolygons.*.shp",
    recursive = TRUE
  )

  msd_version <- ifelse(str_detect(file_psnu_im, ".*_\\d{8}_v1_\\d"), "i", "c")

  msd_caption <- paste0(rep_pd, msd_version)

  caption = glue(" - Data Source: {msd_caption} MSD, VLS = (TX_PVLS_N/TX_PVLS_D)
    ARV Disp. adjusted for Months of Treatments based on pill count
    Produced by OHA/SIEI/SI/Core Analytics on ")

  dir_graphics %<>%
    paste0("/", rep_pd, msd_version)


# DATA ----

  ## MSD PSNU x IM
  df_psnu <- file_psnu_im %>% read_msd()

  ## Geodata

  ## Terrain Raster
  terr <- get_raster(terr_path = dir_terr)

  ## GEO - PEPFAR Orgs boundaries
  spdf_pepfar <- file_shp %>% sf::read_sf()

  df_attrs <- gisr::get_ouuids() %>%
    filter(!str_detect(operatingunit, " Region$")) %>%
    pull(operatingunit) %>%
    map_dfr(.x, .f = ~get_attributes(country = .x))

  spdf_pepfar <- spdf_pepfar %>%
    left_join(df_attrs, by = c("uid" = "id"))

# MUNGING ----

  ## VLS & TLD
  df_vls_tld <- extract_vls_tld(df_msd = df_psnu,
                                rep_agency = rep_agency,
                                rep_fys = rep_fys,
                                rep_pd = rep_pd)


  # OUs
  vls_cntries <- df_vls_tld %>%
    filter(!is.na(VLS), !str_detect(operatingunit, " Region")) %>%
    distinct(operatingunit) %>%
    pull(operatingunit)


# VIZ -----------------

  # single country => DRC
  cname <- vls_cntries %>% nth(15)

  viz_vls_tld(df_vl = df_vls_tld,
              spdf = spdf_pepfar,
              terr = terr,
              country = cname,
              caption = caption,
              save = FALSE)

  # Batch
  vls_cntries %>%
    map(.x, .f = ~ viz_vls_tld(df_vl = df_vls_tld,
                               spdf = spdf_pepfar,
                               terr = terr,
                               country = .x,
                               caption = caption,
                               save = TRUE))

  # Upload files to google drive
  gdrive_vls_tld <- gdrive_folder(name = msd_caption,
                                  path = gdrive_tx_tld,
                                  add = TRUE)

  dir_graphics %>%
    list.files(pattern = paste0("^", rep_pd, " - .*_VLS_TLD_TLE_Ratio_\\d{8}.png$"),
               full.names = TRUE) %>%
    map_dfr(~drive_upload(.x,
                          path = as_id(gdrive_vls_tld),
                          name = basename(.x),
                          type = "png",
                          overwrite = TRUE))
