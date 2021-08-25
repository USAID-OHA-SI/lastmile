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


# REPORTING PERIODS ----

  rep_fy <- 2021

  rep_qtr <- 3

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
  #

  # TX MMD
  # TX ART
  #