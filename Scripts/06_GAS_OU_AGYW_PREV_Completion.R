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
  period <- 2
  caption <- "FY20Q2; Source: FY20Q3c MSD"

  # Update paths with your owns
  source("../_setup/00_Setup.R")

# FUNCTIONS ----------------------------------------------

  batch_agyw_prev <-
    function(country, df, fy, pd, spdf, terr, caption) {


    # AGYW_PREV Data
    agyw <- extract_agyw_prevalence(df_msd = df,
                                    country = country,
                                    rep_fy = fy,
                                    rep_pd = pd)

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
      plot_annotation(caption = caption) &
      theme(plot.title = element_text(size = 9, family = "Gill Sans MT", face = 1))

    print(graphic)

    # ggsave(here(dir_graphs,
    #             paste0(toupper(country),
    #                    " - Q",
    #                    period,
    #                    " - DREAMS_PercentCompletion_13plus_months_",
    #                    format(Sys.Date(), "%Y%m%d"),
    #                    ".png")),
    #        scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")

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


# VIZ ----------------------------------------------------------

ous <- df %>%
  filter(!str_detect(operatingunit, " Region$")) %>%
  distinct(operatingunit) %>%
  pull()

ous %>%
  nth(21) %>%  # Uganda [pass]
  #nth(24) %>% # Zambia [pass]
  #nth(6) %>%  # DRC [fail]
  map(.x, .f = ~ batch_agyw_prev(country = .x, df, fy, period, spdf, terr, caption))

