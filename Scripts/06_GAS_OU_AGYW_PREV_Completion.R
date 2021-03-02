##  PROJECT: Geospatal Analytics
##  AUTHOR:  B.Kagniniwa, G.Sarfaty, T. Essam | USAID
##  PURPOSE: % ompletion of primary package among those in DREAMS 13+ mo
##  LICENCE: MIT
##  DATE:    2020-09-17
##  UPDATED: 2020-11-06

# Libraries
library(tidyverse)
library(sf)
library(glitr)
library(gisr)
library(here)
library(scales)
library(patchwork)
library(extrafont)
library(ICPIutilities)


# GLOBAL -------------------------------------------------

  # Dependencies
    source("./Scripts/00_Utilities.R")
    source("./Scripts/00_Geo_Utilities.R")
    source("./Scripts/00_DREAMS_Utilities.R")

  ## Directories

    dir_data <- here("Data")
    dir_dataout <- here("Dataout")

    dir_geo <- si_path("path_vector")
    dir_terr <- si_path("path_raster")
    dir_merdata <- si_path("path_msd")

    dir_img <- here("Images")
    dir_graphs <- here("Graphics")

  ## Reporting vars

    rep_fy <- 2021
    rep_qtr <- 1
    rep_pd <- paste0("FY", str_sub(rep_fy, 3, 4), "Q", rep_qtr)
    curr_pd <- paste0("qtr", rep_qtr)
    remv_pd <- paste0("qtr", 1:4)
    remv_pd <- remv_pd[!remv_pd == curr_pd]

  ## Output Caption

    caption <- "Reporting Period: FY21Q1; Data Source: FY21Q1i MSD"

  ## MER Data - get the latest MSD PSNU x IM file

    file_psnu_im <- return_latest(
        folderpath = dir_merdata,
        pattern = "^MER_.*_PSNU_IM_.*_20210212_v1_1.zip$",
        recursive = FALSE
      )

  ## Shapefile path
    file_shp <- return_latest(
      folderpath = dir_geo,
      pattern = "VcPepfarPolygons.*.shp",
      recursive = TRUE
    )

    file_shp

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

  # Read data
  df <- read_msd(file_psnu_im)

  # Terrain raster file
  terr <- get_raster(terr_path = dir_terr)

  # PEPFAR GEO Data
  spdf_pepfar <- file_shp %>% sf::read_sf()

  df_attrs <- gisr::get_ouuids() %>%
    filter(!str_detect(operatingunit, " Region$")) %>%
    pull(operatingunit) %>%
    map_dfr(.x, .f = ~get_attributes(country = .x))

  spdf_pepfar <- spdf_pepfar %>%
    left_join(df_attrs, by = c("uid" = "id"))

  # DREAMS - AGYW Prevalence
  df_agyw <- extract_agyw_prevalence(
    df_msd = df,
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

