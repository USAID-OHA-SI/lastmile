##  PROJECT: Geospatal Analytics
##  AUTHOR:  B.Kagniniwa, G.Sarfaty, T. Essam | USAID
##  PURPOSE: % ompletion of primary package among those in DREAMS 13+ mo
##  LICENCE: MIT
##  DATE:    2020-09-17
##  UPDATED: 2020-05-27

# Libraries ----

  library(tidyverse)
  library(sf)
  library(glitr)
  library(gisr)
  library(here)
  library(scales)
  library(patchwork)
  library(extrafont)
  library(ICPIutilities)
  library(glue)
  library(ggtext)

# GLOBAL ----

  # Dependencies
    source("./Scripts/00_Utilities.R")
    source("./Scripts/00_Geo_Utilities.R")
    source("./Scripts/00_DREAMS_Utilities.R")

  # Directories
    dir_data <- here("Data")
    dir_dataout <- here("Dataout")

    dir_geo <- si_path("path_vector")
    dir_terr <- si_path("path_raster")
    dir_merdata <- si_path("path_msd")

    dir_img <- here("Images")
    dir_graphs <- here("Graphics")

  ## Reporting vars
    rep_fy <- 2021
    rep_qtr <- 2
    rep_pd <- paste0("FY", str_sub(rep_fy, 3, 4), "Q", rep_qtr)
    curr_pd <- paste0("qtr", rep_qtr)
    remv_pd <- paste0("qtr", 1:4)
    remv_pd <- remv_pd[!remv_pd == curr_pd]
    msd_version <- "i"

  ## Output Caption
    caption <- glue("Reporting Period: {rep_pd};",
                    " Data Source: {rep_pd}{msd_version} MSD")

  ## MER Data - get the latest MSD PSNU x IM file
    file_psnu_im <- return_latest(
        folderpath = dir_merdata,
        pattern = "^MER_.*_PSNU_IM_.*_\\d{8}_v\\d{1}_\\d{1}.zip$",
        recursive = FALSE
      )

  ## Shapefile path
    file_shp <- return_latest(
      folderpath = dir_geo,
      pattern = "VcPepfarPolygons.*.shp",
      recursive = TRUE
    )


# FUNCTIONS ----------------------------------------------

  #' @title Create AGYW_PREV Visuals
  #'
  #' @param df_agyw
  #' @param country
  #' @param rep_pd
  #' @param spdf
  #' @param terr
  #' @param caption
  #' @param save
  #'
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

      # OU level summary
      df_ou <- df %>%
        filter(str_detect(indicator, "t.*")) %>%
        pivot_wider(fiscal_year:shortname,
                    names_from = "indicator",
                    values_from = "value") %>%
        group_by(fiscal_year, operatingunit) %>%
        summarise(across(starts_with("t"), sum, nr.rm = TRUE)) %>%
        ungroup() %>%
        rowwise() %>%
        mutate(
          prp_leq12m = ttl_leq12m / total,
          prp_leq12m = percent(prp_leq12m, .1),
          prp_gt12m = ttl_gt12m / total,
          prp_gt12m = percent(prp_gt12m, .1),
          prp_completed = ttl_completed / total,
          prp_completed = percent(prp_completed, .1),
          prp_completed_gt12m = ttl_completed_gt12m / ttl_gt12m,
          prp_completed_gt12m = percent(prp_completed_gt12m, .1)
        ) %>%
        ungroup()

      #ou_prp_leq12m <- df_ou %>% pull(prp_leq12m)
      ou_prp_gt12m <- df_ou %>% pull(prp_gt12m)
      ou_prp_cgt12m <- df_ou %>% pull(prp_completed_gt12m)
      ou_prp_complete <- df_ou %>% pull(prp_completed)

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
          subtitle = glue("At the OU level, <span style='color:#1e87a5'>{ou_prp_gt12m}</span> are in their 13th+ months with <span style='color:#1e87a5'>{ou_prp_cgt12m}</span> (<span style='color:#212721'>{ou_prp_complete}</span> overall) completing at least the primary package"),
          caption = paste0(caption, "\n",
                           toupper(country), " - Produced on ",
                           format(Sys.Date(), "%Y%m%d"), " by OHA/SIEI")
        ) &
        theme(
          text = element_text(family = "Source Sans Pro"),
          plot.title = element_text(family = "Source Sans Pro", hjust = .5),
          plot.subtitle = element_markdown(family = "Source Sans Pro", hjust = .5)
        )

      # Save on demand
      if (save == TRUE) {

        si_save(here("./Graphics",
                     paste0(pd,
                           " - DREAMS_PercentCompletion_13plus_months_",
                           toupper(country), "_",
                           format(Sys.Date(), "%Y%m%d"), ".png")),
               plot = graphic,
               scale = 1.2)
      }

      return(graphic)
    }

# DATA ----

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

  cname <- dreams_ous %>% nth(13)

  df_cntry <- df_agyw %>% filter(operatingunit == cname)

  map <- map_agyw_prevalence(spdf_pepfar, terr, df_agyw = df_cntry)

  dots <- plot_agyw_prevalence(df_agyw = df_cntry, type = "dots")

  bars <- plot_agyw_prevalence(df_agyw = df_cntry, type = "bars")

  (map + dots + bars) +
    plot_annotation(
      title = "% who completed at least primary package after being in DREAMS for 13+ months",
      caption = caption
    ) &
    theme(
      text = element_text(family = "Source Sans Pro"),
      plot.title = element_text(family = "Source Sans Pro",
                                face = 1, hjust = .5))

  # Batch
  dreams_ous %>%
    map(.x, .f = ~ batch_agyw_prev(df_agyw = df_agyw,
                                   country = .x,
                                   rep_pd = rep_pd,
                                   spdf = spdf_pepfar,
                                   terr = terr,
                                   caption = caption,
                                   save = TRUE))

