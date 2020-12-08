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
  cname <- vls_cntries %>% nth(5)

  viz_vls_tld(df_vl = df_vls_tld,
              spdf = spdf_pepfar,
              terr_path = dir_terr,
              country = cname,
              caption = caption,
              save = FALSE)

  # Batch => Issues to be aware of
  #vls_cntries[1:17] %>%
  #vls_cntries[18] %>% # South Sudan [failed], basemape issue
  #vls_cntries[19:20] %>%
  #vls_cntries[21] %>% # Ukraine [failed], faceting issue
  #vls_cntries[22:24] %>%

  # Batch
  vls_cntries %>%
    map(.x, .f = ~ viz_vls_tld(df_vl = df_vls_tld,
                               spdf = spdf_pepfar,
                               terr_path = dir_terr,
                               country = .x,
                               caption = caption,
                               save = TRUE))


