##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: ART Suturation by OU
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
  library(ggnewscale)
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
  rep_qtr = 2

  rep_fy2 = rep_fy %>%
    as.character() %>%
    str_sub(3,4) %>%
    paste0("FY", .)

  rep_fys = c(rep_fy - 1, rep_fy)

  rep_fys2 = rep_fys %>%
    as.character() %>%
    str_sub(3,4) %>%
    paste0("FY", .)

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

  #' @title ART Saturation Map
  #' @param spdf_art Spatial data containing var to map
  #' @param spdf     PEPFAR Polygons
  #' @param terr     Terrain Raster
  #' @param country  Ou / Country name
  #' @param rep_pd   Reporting Period
  #'
  vlc_map <-
    function(spdf_vl, spdf,
             terr, country,
             rep_pd = "FY21Q2",
             lbl_size = 3,
             add_caption = TRUE) {

    # ART Sat
    spdf_lv <- spdf_vl %>%
        filter(operatingunit == country)

    print(paste0(country, ": ", (spdf_vl %>% nrow())))

    # Extract admin 0 and 1 for basemap
    admin0 <- spdf %>%
      filter(operatingunit == country,
             label == "country")

    admin1 <- spdf %>%
      filter(operatingunit == country,
             label == "snu1")

    # Produce basemap
    basemap <- terrain_map(countries = admin0,
                           adm0 = admin0,
                           adm1 = admin1,
                           mask = TRUE,
                           terr = terr)

    # Produce thematic map
    map <- basemap +
      geom_sf(
        data = spdf_lv,
        aes(fill = VLC),
        lwd = .2,
        color = grey10k
      ) +
      scale_fill_si(
        palette = "genoas",
        discrete = FALSE,
        alpha = 0.7,
        na.value = grey40k,
        breaks = c(0, .25, .50, .75, 1.00),
        limits = c(0, 1),
        labels = percent
      ) +
      geom_sf(data = admin0,
              colour = grey10k,
              fill = NA,
              size = 1) +
      geom_sf(data = admin0,
              colour = grey90k,
              fill = NA,
              size = .3) +
      geom_sf_text(data = spdf_vl,
                   aes(label = paste0(psnu, "\n", percent(VLC, 1))),
                   size = lbl_size,
                   color = grey10k)
      # geom_sf_text(data = spdf_vl %>%
      #                filter(VLC >= .9),
      #              aes(label = percent(VLC, 1)),
      #              size = lbl_size,
      #              color = grey10k)

    # Add caption
    if (add_caption == TRUE) {

      map <- map +
        labs(
          #title = "ART SATUTATION IN USAID SUPPORTED PSNUs",
          #subtitle = "PSNUs with labelled",
          caption = paste0(glue("{rep_pd} VLC = TX_PVLS_D / lag(TX_CURR, 2)\nVLC Change = VLC({rep_pd}) - VLC(FY20Q4)\nSource: {rep_pd} PSNU x IM MSDs, Produced on "),
                           format(Sys.Date(), "%Y-%m-%d")))

      }

    # Add theme
    map <- map +
      si_style_map() +
      theme(plot.caption = element_text(size = 6, family = "Source Sans Pro"))

    return(map)
  }


  #' VLC Change
  #'
  vlc_change <- function(spdf_vl, country) {

    viz <- spdf_vl %>%
      st_drop_geometry() %>%
      mutate(label = paste0(psnu, " (", percent(VLC, 1), ")"),
             change_color = ifelse(VLC_Diff > 0,
                                   genoa_light,
                                   usaid_red)) %>%
      filter(operatingunit == country,
             fundingagency == "USAID") %>%
      ggplot(aes(x = reorder(label, VLC), VLC)) +
      geom_hline(yintercept = .9,
                 lty = "dashed", lwd = 1,
                 color = usaid_darkgrey) +
      geom_segment(aes(xend = label,
                       y = VLC_Prev, yend = VLC,
                       color = change_color),
                   size = 1, alpha = .7) +
      geom_point(aes(y = VLC_Prev),
                 shape = 21, fill = grey50k,
                 size = 4 ,
                 color = grey10k) +
      geom_point(aes(y = VLC, fill = VLC),
                 shape = 21, size = 5,
                 color = grey10k,
                 show.legend = F) +
      scale_fill_si(
        palette = "genoas",
        discrete = FALSE,
        alpha = 1
      ) +
      scale_y_continuous(labels = percent, position = "right") +
      scale_color_identity() +
      coord_flip() +
      labs(x = "", y = "") +
      si_style()

    return(viz)
  }

# LOAD DATA ----

  # Data
  df_psnu <- file_psnu_im %>% read_msd()

  df_nat <- file_natsub %>% read_msd()

  # SPATIAL DATA
  terr <- gisr::get_raster(terr_path = dir_terr)

  spdf_pepfar <- file_shp %>% sf::read_sf()

  df_attrs <- gisr::get_ouuids() %>%
    filter(!str_detect(operatingunit, " Region$")) %>%
    pull(operatingunit) %>%
    map_dfr(.x, .f = ~get_attributes(country = .x))

  spdf_pepfar <- spdf_pepfar %>%
    left_join(df_attrs, by = c("uid" = "id"))


# MUNGE ----

  # Check
  df_psnu %>%
    filter(operatingunit == "Nigeria",
           indicator == "TX_CURR") %>%
    distinct(fundingagency)

  # TX Viral Load
  df_vl <- df_psnu %>%
    filter(
      fiscal_year %in% rep_fys, # Needed for Q1 vl
      str_to_lower(fundingagency) != "dedup",
      fundingagency == 'USAID',
      indicator %in% c("TX_PVLS", "TX_CURR"),
      standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")
    ) %>%
    mutate(
      indicator = if_else(
        indicator == "TX_PVLS" & numeratordenom == "D",
        paste0(indicator, "_D"),
        indicator
      )
    ) %>%
    group_by(fiscal_year,
             fundingagency,
             operatingunit,
             countryname,
             snu1,
             psnuuid,
             psnu,
             indicator) %>%
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    reshape_msd(clean = TRUE) %>%
    dplyr::select(-period_type) %>%
    pivot_wider(names_from = indicator, values_from = value) %>%
    group_by(fundingagency, operatingunit, countryname, snu1, psnuuid, psnu) %>%
    mutate(VLC = TX_PVLS_D / lag(TX_CURR, 2, order_by = period),
           VLC_Prev = lag(VLC, 1, order_by = period),
           VLC_Diff = VLC - lag(VLC, 2, order_by = period)) %>%
    ungroup() %>%
    mutate(
      VLS = (TX_PVLS / TX_PVLS_D) * VLC,
      VLnC = case_when(
        VLC > 1 ~ 0,
        TRUE ~ 1 - VLC
      ),
      fiscal_year = str_sub(period, 1, 4)
    ) %>%
    relocate(fiscal_year, .before = period) %>%
    filter(period == rep_pd) %>%
    clean_psnu()

  # Check
  df_vl %>% filter(operatingunit == "Nigeria")

  # Join to spatial file
  spdf_vl <- spdf_pepfar %>%
    left_join(df_vl, by = c("uid" = "psnuuid",
                            "operatingunit" = "operatingunit",
                            "countryname" = "countryname")) %>%
    filter(label == "prioritization",
           !is.na(VLC)) %>%
    clean_psnu()


# VIZ ----

  # Countries with valid data
  spdf_vl %>%
    st_set_geometry(NULL) %>%
    distinct(operatingunit) %>%
    pull()

  # maps for Pre-POART Slide deck
  #c("Nigeria", "Uganda", "Zambia", "Tanzania") %>%
  #c("Uganda") %>%
  #c("Tanzania") %>%
  #c("Zambia") %>%
  c("Nigeria") %>%
    map(function(cntry) {

      # Data
      spdf_vl_cntry <- spdf_vl %>%
        filter(fundingagency == "USAID",
               operatingunit == cntry)

      # map
      vlc_map <- vlc_map(spdf_vl = spdf_vl_cntry,
                         spdf = spdf_pepfar,
                         terr = terr,
                         country = cntry,
                         lbl_size = 2)

      # change plot => only show psnu with negative change
      vlc_ch <- vlc_change(spdf_vl = spdf_vl_cntry, country = cntry)
      # vlc_ch <- vlc_change(spdf_vl = spdf_vl_cntry %>%
      #                        filter(VLC_Diff < 0),
      #                      country = cntry)

      # viz
      vlc_plot <- (vlc_map + vlc_ch) +
        plot_annotation(
          title = "NIGERIA - FY20Q2 Virial Load Coverage",
          subtitle = "Significant increas in VLC between <span style='color:#939598;'>**FY20Q4**</span> and <span style='color:#287c6f'>**FY21Q2**</span> in most states<br/>States are sorted by FY21Q2 % VL Coverage",
          theme = theme(
              axis.text.x = element_text(size = 5, family = "Source Sans Pro"),
              axis.text.y = element_text(size = 6, family = "Source Sans Pro"),
              plot.title = element_text(size = 10, family = "Source Sans Pro", hjust = .5),
              plot.subtitle = element_markdown(size = 8,  family = "Source Sans Pro", hjust = .5),
              plot.caption = element_text(hjust = .5, family = "Source Sans Pro")
          )
        )

      print(vlc_plot)

      # Both
      si_save(
        filename = file.path(
          dir_graphics,
          glue("{rep_pd} - {str_to_upper(cntry)} - TX VLC Change - {format(Sys.Date(), '%Y%m%d')}.png")),
        plot = vlc_plot,
        width = 10,
        height = 5)

      si_save(
        filename = file.path(
          dir_graphics,
          glue("{rep_pd} - {str_to_upper(cntry)} - TX VLC Change - {format(Sys.Date(), '%Y%m%d')}.svg")),
        plot = vlc_plot,
        width = 10,
        height = 5)

      # Map only
      # si_save(
      #   filename = file.path(
      #     dir_graphics,
      #     glue("{rep_pd} - {str_to_upper(cntry)} - TX VLC Change Map - {format(Sys.Date(), '%Y%m%d')}.png")),
      #   plot = vlc_map,
      #   width = 7,
      #   height = 7)
      #
      # si_save(
      #   filename = file.path(
      #     dir_graphics,
      #     glue("{rep_pd} - {str_to_upper(cntry)} - TX VLC Change Map - {format(Sys.Date(), '%Y%m%d')}.svg")),
      #   plot = vlc_map,
      #   width = 7,
      #   height = 7)

      # Plot only
      # si_save(
      #   filename = file.path(
      #     dir_graphics,
      #     glue("{rep_pd} - {str_to_upper(cntry)} - TX VLC Change plot - {format(Sys.Date(), '%Y%m%d')}.png")),
      #   plot = vlc_ch,
      #   width = 10,
      #   height = 7)
      #
      # si_save(
      #   filename = file.path(
      #     dir_graphics,
      #     glue("{rep_pd} - {str_to_upper(cntry)} - TX VLC Change plot - {format(Sys.Date(), '%Y%m%d')}.svg")),
      #   plot = vlc_ch,
      #   width = 10,
      #   height = 7)

      return(cntry)
    })



  # Nigeria
  spdf_vl_nga <- spdf_vl %>%
    filter(fundingagency == "USAID",
           operatingunit == "Nigeria")


  # map
  nga_vlc_map <- vlc_map(spdf_vl = spdf_vl_nga,
                            spdf = spdf_pepfar,
                            terr = terr,
                            country = "Nigeria",
                            lbl_size = 2)

  nga_vlc_ch <- vlc_change(spdf_vl = spdf_vl_nga,
                           country = "Nigeria")


  nga_vlc_plot <- (nga_vlc_map + nga_vlc_ch) +
    theme(axis.text.x = element_text(family = "Source Sans Pro"),
          plot.caption = element_text(hjust = .5, family = "Source Sans Pro")
    )

  si_save(
    filename = file.path(
      dir_graphics,
      paste0("FY21Q1 - NIGERIA - TX VLC Change - ",
             format(Sys.Date(), "%Y%m%d"),
             ".png")),
    plot = nga_vlc_plot,
    width = 10,
    height = 5)

