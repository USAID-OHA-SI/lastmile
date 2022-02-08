##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: Notes for quarterly updates
##  LICENCE: MIT
##  DATE:    2021-08-23


# DIRECTORIES ----

  ## Data & Output folders
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"
  dir_geodata <- si_path("path_vector")
  dir_merdata <- si_path("path_msd")
  dir_terr <- si_path("path_raster")

# FILES ----

  ## File path + name
  file_psnu_im <- return_latest(
    folderpath = dir_merdata,
    pattern = "^MER_.*_PSNU_IM_.*_\\d{8}_v\\d{1}_\\d{1}.zip$",
    recursive = TRUE
  )

  # Reporting Periods
  rep_pd <- file_psnu_im %>% identify_pd()

  rep_qtr <- rep_pd %>%
    str_sub(-1) %>%
    as.integer()

  rep_fy <- rep_pd %>%
    str_sub(3,4) %>%
    paste0("20", .) %>%
    as.integer()

  # MSD File version
  msd_version <- ifelse(str_detect(file_psnu_im, ".*_\\d{8}_v1_\\d"), "i", "c")

  msd_caption <- paste0(rep_pd, msd_version)

  dir_graphics <- dir_graphics %>%
    paste0("/", rep_pd, msd_version)

  if(!dir.exists(dir_graphics)) {
    dir.create(dir_graphics)
  }

# REPORTING PERIODS ----

  rep_fy <- 2021

  rep_qtr <- 4

  rep_fy2 <- rep_fy %>%
    as.character() %>%
    str_sub(3,4) %>%
    paste0("FY", .)

  rep_fys <- c(rep_fy - 1, rep_fy)

  rep_fys2 <- rep_fys %>%
    as.character() %>%
    str_sub(3,4) %>%
    paste0("FY", .)

  rep_pd <- rep_fy %>%
    as.character() %>%
    str_sub(3,4) %>%
    paste0("FY", ., "Q", rep_qtr)

  rep_ref_pd <- rep_fys %>%
    first() %>%
    str_sub(3,4) %>%
    paste0("FY", ., "Q4")

  rep_init_pd <- rep_fys %>%
    first() %>%
    str_sub(3,4) %>%
    paste0("FY", ., "Q1")

  rep_pds <- c(rep_ref_pd, rep_pd)
  rep_pds2 <- c(rep_init_pd, rep_pd)

# FUNCTIONS ----

  clear_workspace <- function() {
    # clear all objects includes hidden objects.
    rm(list = ls(all.names = TRUE))

    #free up memory and report the memory usage.
    gc()
  }

# LOAD DATA ----

  # MSD
  #df_psnu <- file_psnu_im %>% read_msd()

  # SPATIAL DATA

  ## Raster
  #terr <- gisr::get_raster(terr_path = dir_terr)

  ## PEPFAR Boundaries
  #spdf_pepfar <- file_shp %>% sf::read_sf()

  # df_attrs <- gisr::get_ouuids() %>%
  #   filter(!str_detect(operatingunit, " Region$")) %>%
  #   pull(operatingunit) %>%
  #   map_dfr(.x, .f = ~get_attributes(country = .x))
  #
  # spdf_pepfar <- spdf_pepfar %>%
  #   left_join(df_attrs, by = c("uid" = "id"))

# Tech Areas ----

  # TX VL
  # VLC, VLnC and VLS
  # VLS x TLD
  source("./Scripts/04_GAS_OU_ViralLoad_NotCovered.R")
  clear_workspace()

  # TX MMD
  source("./Scripts/99_GAS_OU_MMD_Distribution.R")
  clear_workspace()

  #source("./Scripts/99_GAS_OU_MMD_Regiments.R")
  clear_workspace()

  # TX ART
  source("./Scripts/04_GAS_TX_ML_SpatialDistribution.R")
  clear_workspace()

  # OVC & Proxy Coverage & Treatment Overlap
  source("./Scripts/04_GAS_OU_OVC_Proxy_and_TreatmentOverlap.R")
  clear_workspace()