## PROJECT:  PEPFAR Geospatial Analytics
## AUTHOR:   B.Kagniniwa, G.Sarfaty | USAID
## LICENSE:  MIT
## PURPOSE:  Lesotho - PrEP NEW vs HTS POS
## Date:     2022-09-21

# LIBRARIES ----

  library(tidyverse)
  library(gophr)
  library(grabr)
  library(gisr)
  library(glitr)
  library(glamr)
  library(janitor)
  library(scales)
  library(patchwork)
  library(extrafont)

# SETUP ----

  ## Directories ----
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"

  dir_geodata <- si_path("path_vector")
  dir_terr <- si_path("path_raster")
  dir_merdata <- si_path("path_msd")

  ## Reporting Filters ----
  rep_agency = "USAID"

  rep_agencies <- c("USAID", "CDC")

  cntries <- c("Nigeria", "Lesotho", "Eswatini", "Namibia", "South Africa")

  ## Indicators
  inds <- c("HTS_TST", "HTS_TST_POS", "PrEP_CURR", "PrEP_NEW", "PrEP_CT")

  ## Tech areas
  tech_areas <- inds %>%
    map(word, sep = "_") %>%
    unlist() %>%
    unique()

  ## Disaggs
  disaggs <- list(
    tn = "Total Numerator",
    td = "Total Denominator",
    as = "Age/Sex",
    kp = "KeyPop"
  )

  ## Files ----

  # Global OUxIM
  file_ou_im <- glamr::return_latest(
    folderpath = dir_merdata,
    pattern = "OU_IM_FY20-23_.*.zip$"
  )

  # Global PSNUxIM
  file_psnu_im <- glamr::return_latest(
    folderpath = dir_merdata,
    pattern = "PSNU_IM_FY20-23_.*.zip$"
  )

  # OU - SitexIm
  cntries %>%
    map(function(.c) {
      f <- list.files(
        path = dir_merdata,
        pattern = paste0("Site_IM_FY20-23_.*_", .c)
      )

      if(length(f) == 0) {
        return(.c)
      }

      return(NA)
    }) %>%
    unlist() %>%
    magrittr::extract(!is.na(.)) %>%
    walk(function(.c){
      pano_extract_msd(operatingunit = .c,
                       version = "initial",
                       fiscal_year = 2022,
                       quarter = 3,
                       level = "site",
                       dest_path = dir_merdata)
    })


  files_site_im <- list.files(
    path = dir_merdata,
    pattern = paste0(paste0("Site_IM_FY20.*_", cntries, ".zip$"), collapse = "|")
  )

  files_site_im

  ## Data Sources
  msd_source <- file_ou_im %>% source_info()

  # LOAD DATA ----

  ## Geodata
  ras <- get_raster()
  spdf_pepfar <- get_vcpolygons()

  df_attrs <- cntries %>%
    map_dfr(get_attributes)

  spdf_nga <- spdf_pepfar %>%
    dplyr::left_join(df_attr, by = c("uid" = "id"))

  # Country sub-units
  spdf_cntry <- spdf_nga %>% filter(label == "country")
  spdf_psnu <- spdf_nga %>% filter(label == "prioritization")
  spdf_lga <- spdf_nga %>% filter(label == "community")

  # Global PSNUxIM Dataset
  df_psnu <- file_psnu_im %>%
    read_msd() %>%
    filter(str_detect(indicator, paste0(tech_areas, collapse = "|"))) %>%
    clean_agency()

  df_psnu %>% distinct(indicator) %>% prinf()
  df_psnu %>% distinct(indicator, standardizeddisaggregate) %>%
    arrange(indicator, standardizeddisaggregate) %>% prinf()

  # Reporting Periods
  curr_pd <- df_psnu %>% identifypd(pd_type = "full")
  curr_fy <- df_psnu %>% identifypd(pd_type = "year")

  # Global PSNUxIM Dataset
  df_psnu <- files_site_im %>%
    read_msd() %>%
    filter(str_detect(indicator, paste0(tech_areas, collapse = "|"))) %>%
    clean_agency()


  # MUNGING ----

  ## PSNUxIM
  df_psnu <- df_psnu %>%
    filter(fiscal_year == curr_fy,
           funding_agency == rep_agency,
           operatingunit %in% cntries,
           indicator %in% inds,
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(fiscal_year, funding_agency, operatingunit, country,
             psnuuid, psnu, indicator) %>%
    summarise(across(c(starts_with("qtr"), cumulative, targets),
                     sum, na.rm = T), .groups = "drop") %>%
    reshape_msd()

  ## SitexIM
  df_site <- filter(fiscal_year == curr_fy,
                    funding_agency == rep_agency,
                    operatingunit %in% cntries,
                    indicator %in% inds,
                    standardizeddisaggregate == "Total Numerator") %>%
    group_by(fiscal_year, funding_agency, operatingunit, country,
             psnuuid, psnu, indicator) %>%
    summarise(across(c(starts_with("qtr"), cumulative, targets),
                     sum, na.rm = T), .groups = "drop") %>%
    reshape_msd()


