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
    source("./Scripts/00_Utilities.R")
    source("./Scripts/00_Geo_Utilities.R")
    source("./Scripts/00_DREAMS_Utilities.R")

  ## Directories

    dir_data <- here("Data")
    dir_dataout <- here("Dataout")

    dir_geo <- "C:/Users/gsarfaty/Documents/GIS/DATA/Boundaries"
    dir_terr <- "C:/Users/gsarfaty/Documents/GIS/DATA/Topography"
    dir_merdata <- "C:/Users/gsarfaty/Documents/DATIM"

    dir_img <- here("Images")
    dir_graphs <- here("Graphics")

  ## Reporting vars

    rep_fy <- 2020
    rep_qtr <- 4
    rep_pd <- paste0("FY", str_sub(rep_fy, 3, 4), "Q", rep_qtr)
    curr_pd <- paste0("qtr", rep_qtr)
    remv_pd <- paste0("qtr", 1:4)
    remv_pd <- remv_pd[!remv_pd == curr_pd]

  ## Output Caption

    caption <- "Reporting Period: FY20Q4; Data Source: FY20Q4i MSD"

  ## overwrite paths with your own.
    source("../_setup/00_Setup.R")

# FUNCTIONS ----------------------------------------------

  batch_agyw_prev <-
    function(df_agyw, country, rep_pd,
             spdf, terr, caption, save = FALSE) {

      # params
      cntry <- {{country}}
      pd <- {{rep_pd}}

      # Notification
      print(toupper(cntry))

      df <- df_agyw %>%
        filter(operatingunit == cntry)

      # Is data valid?
      if (is.null(df) | nrow(df) == 0) {
        stop("No valid data")
      }

      # Map - % Completion 13+ Months
      map <- map_agyw_prevalence(spdf = spdf,
                                 terr = terr,
                                 df_agyw = df)

      # Dot plot - % Completion 13+ Months
      dots <- plot_agyw_prevalence(df_agyw = df, type = "dots")

      # Bar chart - % Dreams Totals
      bars <- plot_agyw_prevalence(df_agyw = df, type = "bars")

      # Combine graphs
      graphic <- (map + dots + bars) +
        plot_annotation(
          title = "% who completed at least primary package after being in DREAMS for 13+ months",
          caption = paste0(caption, "\n",
                           toupper(country), " - Produced on ",
                           format(Sys.Date(), "%Y%m%d"), " by OHA/SIEI")
        ) &
        theme(
          text = element_text(family = "Gill Sans MT"),
          plot.title = element_text(family = "Gill Sans MT",
                                    face = 1, hjust = .5)
        )

      # Save on demand
      if (save == TRUE) {

        print(graphic)

        ggsave(here("./Graphics",
                    paste0(pd,
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

      return(graphic)
    }

# DATA ---------------------------------------------------

  # MER Data - get the latest MSD PSNU x IM file
  # Q4 file => MER_Structured_Datasets_PSNU_IM_FY18-21_20201113_v2_1.zip
  file_psnu_im <- list.files(
      path = dir_merdata,
      pattern = "^MER_.*_PSNU_IM_.*_20201113_v1_1.zip$",
      recursive = FALSE,
      full.names = TRUE
    ) %>%
    sort() %>%
    last()

  # Read data
  df <- read_msd(file_psnu_im)

  # PEPFAR GEO Data
  spdf <- build_spdf(
    dir_geo = paste0(dir_geo, "/PEPFAR/VcPepfarPolygons_2020.07.24"),
    name = "VcPepfarPolygons.shp",
    df_psnu = df
  )

  # Terrain raster file
  terr <- get_raster(terr_path = dir_terr)

  # DREAMS - AGYW Prevalence
  df_agyw <- extract_agyw_prevalence(
    df_msd = df,
    country = NULL,
    rep_fy = rep_fy,
    rep_qtr = rep_qtr
  )

  # Identify OU with DREAMS programs
  dreams_ous <- df_agyw %>%
    filter(!str_detect(operatingunit, " Region$")) %>%
    distinct(operatingunit) %>%
    pull()

# VIZ ----------------------------------------------------------


  # Test plots

  dreams_ous

  cname <- dreams_ous %>% nth(14)

  df_cntry <- df_agyw %>% filter(operatingunit == cname)

  map <- map_agyw_prevalence(spdf, terr, df_agyw = df_cntry)

  dots <- plot_agyw_prevalence(df_agyw = df_cntry, type = "dots")

  bars <- plot_agyw_prevalence(df_agyw = df_cntry, type = "bars")

  (map + dots + bars) +
    plot_annotation(
      title = "% who completed at least primary package after being in DREAMS for 13+ months",
      caption = caption
    ) &
    theme(
      text = element_text(family = "Gill Sans MT"),
      plot.title = element_text(family = "Gill Sans MT",
                                face = 1, hjust = .5))

  # Batch
  dreams_ous %>%
    map(.x, .f = ~ batch_agyw_prev(df_agyw = df_agyw,
                                   country = .x,
                                   rep_pd = rep_pd,
                                   spdf = spdf,
                                   terr = terr,
                                   caption = caption,
                                   save = TRUE))

