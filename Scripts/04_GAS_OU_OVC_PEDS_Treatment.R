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
  df_psnu <- file_psnu_im %>%
    read_msd()

  df_psnu <- df_psnu %>%
    rename(countryname = countrynamename) %>%
    clean_agency() %>%
    clean_psnu()

  # Geo Data
  terr <- gisr::get_raster(terr_path = dir_terr)

  spdf_pepfar <- file_shp %>% sf::read_sf()

  # Ou / Countries
  ous <- gisr::get_ouuids(add_details = TRUE)

  df_attrs <- ous %>%
    filter(!str_detect(operatingunit, " Region$")) %>%
    pull(operatingunit) %>%
    map_dfr(.x, .f = ~get_attributes(country = .x, folderpath = dir_attrs))

  spdf_pepfar <- spdf_pepfar %>%
    left_join(df_attrs, by = c("uid" = "id"))

# MUNGE ----

  # OVC
  inds_ovc <- df_psnu %>%
    filter(str_detect(indicator, "OVC")) %>%
    distinct(indicator) %>%
    pull()

  ous_ovc <- df_psnu %>%
    filter(fiscal_year == 2020,
           indicator == "OVC_SERV") %>%
    distinct(operatingunit, countrynamename)

  df_ovc <- df_psnu %>%
    filter(fiscal_year == 2020,
           indicator == "OVC_SERV",
           standardizeddisaggregate == "Total Numerator",
           str_to_lower(fundingagency) != "dedup") %>%
    mutate(ovc_program = if_else(!is.na(cumulative), TRUE, FALSE)) %>%
    filter(ovc_program == TRUE) %>%
    group_by(fundingagency, operatingunit, countryname,
             primepartner, psnu, psnuuid) %>%
    summarise(ovc_serv = sum(cumulative, na.rm = TRUE)) %>%
    ungroup()

  # TX PEDS
  df_psnu %>%
    filter(indicator == "TX_CURR") %>%
    distinct(indicator, standardizeddisaggregate) %>%
    prinf()

  df_tx <- df_psnu %>%
    filter(fiscal_year == 2020,
           indicator == "TX_CURR",
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           str_to_lower(fundingagency) != "dedup",
           trendscoarse == "<15") %>%
    mutate(clhiv = if_else(!is.na(cumulative), TRUE, FALSE)) %>%
    filter(clhiv == TRUE) %>%
    group_by(fundingagency, operatingunit, countryname,
             primepartner, psnu, psnuuid) %>%
    summarise(tx_curr = sum(cumulative, na.rm = TRUE)) %>%
    ungroup()


  # PEDS OVC Coverage
  df_ovc_cov <- df_tx %>%
    full_join(df_ovc,
              by = c("fundingagency", "operatingunit",
                     "countryname", "primepartner", "psnu", "psnuuid"))


  df_ou_ovc_cov <- df_ovc_cov %>%
    group_by(operatingunit, countryname, psnu, psnuuid) %>%
    summarise(across(c(tx_curr, ovc_serv), sum, na.rm = TRUE)) %>%
    ungroup

  spdf_ou_cov <- spdf_pepfar %>%
    left_join(df_ou_ovc_cov, by = c("uid" = "psnuuid")) %>%
    filter(!is.na(tx_curr))

# VIZ ----

  cntry <- "Nigeria"

  spdf_nga <- spdf_ou_cov %>%
    filter(countryname == cntry)

  spdf_nga_tx <- spdf_nga %>% filter(tx_curr > 0)
  spdf_nga_tx_noovc <- spdf_nga %>% filter(tx_curr > 0, ovc_serv == 0)
  spdf_nga_tx_noovc2 <- spdf_nga_tx_noovc %>%
    summarise() %>%
    st_geometry() %>%
    st_sample(size = 100)
  spdf_nga_ovc <- spdf_nga %>% filter(ovc_serv > 0)

  # admin 0 and 1 for basemap
  admin0 <- spdf_pepfar %>%
    filter(operatingunit == cntry,
           label == "country")

  admin1 <- spdf_pepfar %>%
    filter(operatingunit == cntry,
           label == "snu1")

  admin0 %>% gview()

  # Produce basemap
  basemap <- terrain_map(countries = admin0,
                         adm0 = admin0,
                         adm1 = admin1,
                         mask = TRUE,
                         terr = terr)

  # Produce thematic map
  map <- basemap +
    geom_sf(data = spdf_nga_tx,
            aes(fill = tx_curr),
            color = grey10k,
            lwd = .3,
            alpha = .3) +
    geom_sf(data = spdf_nga_tx_noovc2,
            fill = usaid_darkgrey,
            color = grey10k,
            size = 3) +
    # geom_sf(data = spdf_nga_ovc,
    #         fill = scooter,
    #         color = grey10k,
    #         lwd = .3,
    #         alpha = .8) +
    scale_fill_si(discrete = F) +
    si_style_map()

