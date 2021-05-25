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

  rep_fy = c(2020, 2021)

  rep_pd = "FY21Q2"

  caption = " - Data Source: FY21Q2i MSD, VLS = (TX_PVLS_N/TX_PVLS_D)
    ARV Disp. adjusted for Months of Treatments based on pill count
    OHA/SIEI - Produced on "

  ## Q4 MER Data
  psnu_im <- "^MER_.*_PSNU_IM_.*_20210514_v1_1.zip$"

  ## File path + name
  file_psnu_im <- return_latest(
    folderpath = dir_merdata,
    pattern = psnu_im,
    recursive = TRUE
  )

# DATA ---------------------------------------

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

  ## VLS & TLD
  df_vls_tld <- extract_vls_tld(df_msd = df_psnu,
                                rep_agency = rep_agency,
                                rep_fy = rep_fy,
                                rep_pd = rep_pd)


  # OUs
  vls_cntries <- df_vls_tld %>%
    filter(!is.na(VLS), !str_detect(operatingunit, " Region")) %>%
    distinct(operatingunit) %>%
    pull(operatingunit)


  vls_cntries

  # NE Countries
  ne_countries = rnaturalearth::ne_countries(
      scale = "medium",
      returnclass = "sf"
    ) %>%
    st_set_geometry(NULL) %>%
    dplyr::select(iso3 = sov_a3, sovereignt, admin)

  # There are 3 countries with diff names
  #[1] "Cote d'Ivoire" "Eswatini" "Tanzania"
  setdiff(vls_cntries, ne_countries %>% pull(sovereignt))


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


