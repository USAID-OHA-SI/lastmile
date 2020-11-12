##  PROJECT: Geospatal Analytics
##  AUTHOR:  B.Kagniniwa, G.Sarfaty, T. Essam | USAID
##  PURPOSE: % ompletion of primary package among those in DREAMS 13+ mo
##  LICENCE: MIT
##  DATE:    2020-09-17
##  UPDATED: 2020-11-06

# Libraries
library(extrafont)
library(tidyverse)
library(sf)
library(glitr)
library(gisr)
library(here)
library(scales)
library(patchwork)
library(ICPIutilities)


# GLOBAL -------------------------------------------------

  # Dependencies
  source("./Scripts/00_Geo_Utilities.R")
  source("./Scripts/00_DREAMS_Utilities.R")

  dir_data <- here("Data")
  dir_dataout <- here("Dataout")

  dir_geo <- "C:/Users/gsarfaty/Documents/GIS/DATA/Boundaries"
  dir_terr <- "C:/Users/gsarfaty/Documents/GIS/DATA/Topography"
  dir_merdata <- "C:/Users/gsarfaty/Documents/DATIM"

  dir_img <- here("Images")
  dir_graphs <- here("Graphics")

  # Fiscal Year
  fy <- 2020
  pd <- 2
  curr_pd <- paste0("qtr", pd)
  remv_pd <- paste0("qtr", 1:4)
  remv_pd <- remv_pd[!remv_pd == curr_pd]

  # Reporting Period & Data Source
  caption <- "Rep. Period: FY20Q2; Source: FY20Q3c MSD"

  # overwrite paths with your own.
  source("../_setup/00_Setup.R")

# FUNCTIONS ----------------------------------------------

  batch_agyw_prev <-
    function(country, df, fy, pd, spdf, terr, caption, save = FALSE) {

    # Notification
    print(toupper(country))

    # AGYW_PREV Data
    agyw <- extract_agyw_prevalence(df_msd = df,
                                    country = country,
                                    rep_fy = fy,
                                    rep_pd = pd)

    # Is data valid?
    if (is.null(agyw)) {
      stop("No valid data")
    }

    # Map - % Completion 13+ Months
    map <- map_agyw_prevalence(spdf = spdf,
                               terr = terr,
                               df_agyw = agyw)

    # Dot plot - % Completion 13+ Months
    dots <- plot_agyw_prevalence(df_agyw = agyw, type = "dots")

    # Bar chart - % Dreams Totals
    bars <- plot_agyw_prevalence(df_agyw = agyw, type = "bars")

    # Combine graphs
    graphic <- (map + dots + bars) +
      plot_annotation(caption = paste0(toupper(country), " - ", caption)) &
      theme(plot.title = element_text(size = 9, family = "Gill Sans MT", face = 1))

    print(graphic)

    # Save on demand
    if (save == TRUE) {
      ggsave(here("./Graphics",
                  paste0("FY", str_sub(fy, 3,4),
                         "Q", pd,
                         " - DREAMS_PercentCompletion_13plus_months_",
                         toupper(country), "_",
                         format(Sys.Date(), "%Y%m%d"), ".png")),
             plot = last_plot(),
             scale = 1.2,
             dpi = 310,
             width = 10,
             height = 7,
             units = "in")
    }

  }

# DATA ---------------------------------------------------

  # MER Data - get the latest MSD PSNU x IM file
  # This should return
  # Q3 file => MER_Structured_Datasets_PSNU_IM_FY18-21_20200918_v2_1.zip
  file_psnu_im <- list.files(
      path = dir_merdata,
      pattern = "Structured_.*_PSNU_IM_.*_\\d{8}_v.*.zip",
      recursive = FALSE,
      full.names = TRUE
    ) %>%
    sort() %>%
    last()

  # Read data
  df <- read_msd(file_psnu_im)

  # PEPFAR GEO Data
  spdf <- build_spdf(
    dir_geo = dir_geo,
    name = "VcPepfarPolygons.shp",
    df_psnu = df
  )

  # Terrain raster file
  terr <- get_raster(terr_path = dir_terr)

  # Identify OU with DREAMS programs
  dreams_ous <- df %>%
    filter(indicator == "AGYW_PREV",
           !str_detect(operatingunit, " Region$")) %>%
    distinct(operatingunit) %>%
    pull()

  dreams_ous

# VIZ ----------------------------------------------------------


  # Test plots ----------------------------------------------
  df_agyw <- extract_agyw_prevalence(
    df_msd = df,
    country = dreams_ous %>% nth(11)
  )

  map <- map_agyw_prevalence(spdf, terr, df_agyw)

  dots <- plot_agyw_prevalence(df_agyw = df_agyw, type = "dots")

  bars <- plot_agyw_prevalence(df_agyw = df_agyw, type = "bars")

  (map + dots + bars) +
    plot_annotation(caption = caption) &
    theme(plot.title = element_text(size = 9, family = "Gill Sans MT", face = 1))

  # Batch
  dreams_ous %>%
    map(.x, .f = ~ batch_agyw_prev(country = .x,
                                   df, fy, pd, spdf, terr, caption,
                                   save = TRUE))

