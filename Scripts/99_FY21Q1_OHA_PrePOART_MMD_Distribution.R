##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: MMD Distribution
##  LICENCE: MIT
##  DATE:    2021-03-02


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(readxl)
  library(janitor)
  library(glitr)
  library(glamr)
  library(gisr)
  library(extrafont)
  library(scales)
  library(ggtext)
  library(sf)
  library(ggrepel)
  library(patchwork)
  library(glue)
  library(ICPIutilities)

  source("./Scripts/00_Geo_Utilities.R")
  source("./Scripts/00_VL_Utilities.R")

# SETUP ----

  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"

  dir_geodata <- si_path("path_vector")
  dir_terr <- si_path("path_raster")
  dir_merdata <- si_path("path_msd")

  ## Reporting Filters
  rep_agency = "USAID"
  rep_agencies <- c("USAID", "HHS/CDC")

  rep_fy = 2021
  rep_qtr = 1

  rep_pd = rep_fy %>%
    as.character() %>%
    str_sub(3,4) %>%
    paste0("FY", ., "Q", rep_qtr)

  # MER Data - get the latest MSD PSNU x IM file
  file_psnu_im <- return_latest(
    folderpath = dir_merdata,
    pattern = "^MER_.*_PSNU_IM_.*_20210212_v1_1.zip$",
    recursive = FALSE
  )

  # NAT Data - get the latest NAT_SUBNAT file
  file_natsub <- return_latest(
    folderpath = dir_merdata,
    pattern = "^MER_.*_NAT_SUBNAT_.*_20210212_v1_1.zip$",
    recursive = FALSE
  )

  # Shapefile path
  file_shp <- return_latest(
    folderpath = dir_geodata,
    pattern = "VcPepfarPolygons.*.shp",
    recursive = TRUE
  )


# FUNCTIONS ----

# DATA ----

  # MSD
  df_psnu <- file_psnu_im %>% read_msd()

  # SPATIAL DATA
  terr <- gisr::get_raster(terr_path = dir_terr)

  spdf_pepfar <- file_shp %>% sf::read_sf()

  df_attrs <- gisr::get_ouuids() %>%
    filter(!str_detect(operatingunit, " Region$")) %>%
    pull(operatingunit) %>%
    map_dfr(.x, .f = ~get_attributes(country = .x))

  spdf_pepfar <- spdf_pepfar %>%
    left_join(df_attrs, by = c("uid" = "id"))


# DATA MMD 3+ months

  df_mmd <- df_psnu %>%
    filter(
      fiscal_year %in% c(2020, 2021), # Needed for Q1 vl
      fundingagency == "USAID",
      indicator %in% c("TX_CURR"),
      standardizeddisaggregate %in% c("Age/Sex/ARVDispense/HIVStatus")
      #otherdisaggregate != "ARV Dispensing Quantity -	Less than 3 months"
    ) %>%
    reshape_msd(clean = TRUE) %>%
    filter(period_type == "results") %>%
    mutate(
      otherdisaggregate = str_remove(otherdisaggregate,
                                     "ARV Dispensing Quantity - "),
      otherdisaggregate = case_when(
        otherdisaggregate == "Less than 3 months" ~ "<3",
        otherdisaggregate == "3 to 5 months" ~ "3-5",
        otherdisaggregate == "6 or more months" ~ "6+"
      )) %>%
    group_by(period, operatingunit, operatingunituid, otherdisaggregate) %>%
    summarise_at(vars(value), sum, na.rm = TRUE) %>%
    ungroup()


  df_mmd_geq3 <- df_mmd %>%
    mutate(
      otherdisaggregate = case_when(
        otherdisaggregate == "3-5" ~ "3+",
        otherdisaggregate == "6+" ~ "3+",
        TRUE ~ otherdisaggregate
      )
    ) %>%
    group_by(period, operatingunit, operatingunituid) %>%
    mutate(mmd_share = value / sum(value)) %>%
    ungroup()
