##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: MMD Distribution
##  LICENCE: MIT
##  DATE:    2021-03-02


# DEPENDENCIES ------------------------------------------------------------

  # Data munging & viz
  library(tidyverse)
  # S/GAC & OHA/SIEI Utlitities
  library(glitr)
  library(glamr)
  library(gisr)
  library(ICPIutilities)
  # Other packages
  library(sf)
  library(extrafont)
  library(scales)
  library(ggtext)
  library(ggrepel)
  library(tidytext)
  library(patchwork)
  library(glue)

  # Code re-use: placeholder till migration to one of the our packages
  source("./Scripts/00_Geo_Utilities.R")

# SETUP ----

  # Project specific folders
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"

  # Ref folders outside of project
  dir_geodata <- si_path("path_vector")
  dir_terr <- si_path("path_raster")
  dir_merdata <- si_path("path_msd")

# GLOBAL VARS

  # Reporting Filters
  rep_agency = "USAID"
  rep_agencies <- c("USAID", "HHS/CDC")

  rep_fy = 2021
  rep_qtr = 2

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

  # NAT Data - get the latest NAT_SUBNAT file
  file_natsub <- return_latest(
    folderpath = dir_merdata,
    pattern = "^MER_.*_NAT_SUBNAT_.*_\\d{8}_v\\d{1}_1.zip$",
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
  #'
  art_saturation_map <-
    function(spdf_art, spdf,
             terr, country,
             lbl_size = 3,
             full_label = FALSE,
             add_caption = TRUE) {

      # ART Sat
      spdf_art <- spdf_art %>%
        filter(operatingunit == country)

      # Set max to 1
      max <- spdf_art %>%
        pull(ART_SAT) %>%
        max()

      max <- ifelse(max < 1, 1, max)

      # Footnote
      caption <- ifelse(country == "Nigeria",
                        paste0("ART Saturation = TX_CURR / PLHIV (FY21)",
                               "\nUSAID's PSNUs are labelled with name + percent saturation",
                               "\nSource: FY21Q1i PSNU x IM MSDs, Produced on ",
                               format(Sys.Date(), "%Y-%m-%d")),
                        paste0("ART Saturation = TX_CURR_SUBNAT / PLHIV (FY21)",
                               "\nUSAID's PSNUs are labelled with name + percent saturation",
                               "\nSource: FY21Q1c NAT_SUBNAT & PSNU x IM MSDs, Produced on ",
                               format(Sys.Date(), "%Y-%m-%d"))
      )

      print(paste0(country, ": ", (spdf_art %>% nrow())))

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
        geom_sf(data = spdf_art,
                aes(fill = ART_SAT),
                lwd = .3,
                color = grey10k) +
        scale_fill_si(
          palette = "burnt_siennas",
          discrete = FALSE,
          alpha = 0.7,
          na.value = NA,
          breaks = seq(0, max, .25),
          limits = c(0, max),
          labels = percent
        ) +
        geom_sf(data = admin0,
                colour = grey10k,
                fill = NA,
                size = 1) +
        geom_sf(data = admin0,
                colour = grey90k,
                fill = NA,
                size = .3)

      # label control
      if (full_label == TRUE) {
        map <- map +
          geom_sf_text(data = spdf_art,
                       aes(label = paste0(psnu, "\n", percent(ART_SAT, 1))),
                       size = lbl_size,
                       color = grey10k)
      }
      else {
        map <- map +
          geom_sf_text(data = spdf_art %>%
                         filter(ART_SAT >= .9, usaid_flag == "USAID"),
                       aes(label = percent(ART_SAT, 1)),
                       size = lbl_size,
                       color = grey10k)
      }

      # Add caption
      if (add_caption == TRUE) {

        map <- map +
          labs(
            #title = "ART SATUTATION IN USAID SUPPORTED PSNUs",
            #subtitle = "PSNUs with labelled",
            caption = caption)
      }

      # Add theme
      map <- map +
        si_style_map()

      return(map)
    }

  #' @title ART Saturation Bar Chart
  #'
  art_saturation_bars <-
    function(df_tx, country) {

      # filter
      df <- df_tx %>%
        mutate(label = paste0(psnu, " (", percent(ART_SAT, 1), " ", comma(PLHIV, 1), ")")) %>%
        filter(operatingunit == country, usaid_flag == "USAID")

      # get max value
      max_art <- df %>%
        pull(ART_SAT) %>%
        max()

      # Viz
      bars <- df %>%
        ggplot(aes(reorder(label, PLHIV), PLHIV)) +
        geom_col(aes(fill = ART_SAT), show.legend = F) +
        scale_fill_si(
          palette = "burnt_siennas",
          discrete = FALSE,
          alpha = 0.7,
          breaks = seq(0, max_art, .25),
          limits = c(0, max_art),
          labels = percent
        ) +
        scale_y_continuous(labels = comma, position = "right") +
        coord_flip() +
        labs(x = "", y = "") +
        si_style_xgrid()

      print(bars)

      return(bars)

    }

# LOAD DATA ----

  # MER Data
  df_psnu <- file_psnu_im %>% read_msd()

  df_nat <- file_natsub %>% read_msd()

  # Geo Data
  terr <- gisr::get_raster(terr_path = dir_terr)

  spdf_pepfar <- file_shp %>% sf::read_sf()

  # Orgs Hierarchy
  df_attrs <- gisr::get_ouuids() %>%
    filter(!str_detect(operatingunit, " Region$")) %>%
    pull(operatingunit) %>%
    map_dfr(.x, .f = ~get_attributes(country = .x))

  spdf_pepfar <- spdf_pepfar %>%
    left_join(df_attrs, by = c("uid" = "id"))

# MUNGE ----

  # PLHIV
  #
  df_nat %>% glimpse()

  df_plhiv <- df_nat %>%
    reshape_msd(clean = TRUE) %>%
    filter(period %in% c("FY20", "FY21"),
           indicator %in% c("PLHIV", "TX_CURR_SUBNAT"),
           #standardizeddisaggregate == "Total Numerator",
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex"),
           psnuuid != "?") %>%
    rename(countryname = countrynamename) %>%
    group_by(period, operatingunit, countryname, snu1, psnuuid, psnu, indicator) %>%
    summarise_at(vars(value), sum, na.rm = TRUE) %>%
    ungroup() %>%
    pivot_wider(names_from = indicator, values_from = value)

    df_plhiv %>% glimpse()

  # USAID Treatment

  df_tx_locs <- df_psnu %>%
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator",
           fundingagency == "USAID",
           str_detect(psnu, "_Military", negate = TRUE),
           fiscal_year == 2021) %>%
    distinct(operatingunit, psnuuid) %>%
    mutate(usaid_flag = "USAID")


  df_tx <- df_plhiv %>%
    rowwise() %>%
    mutate(ART_SAT = TX_CURR_SUBNAT / PLHIV) %>%
    ungroup() %>%
    filter(period == "FY21") %>%
    left_join(df_tx_locs, by = c("operatingunit", "psnuuid")) %>%
    clean_psnu()


  # Join to spatial file
  spdf_tx <- spdf_pepfar %>%
    left_join(df_tx, by = c("uid" = "psnuuid",
                            "operatingunit" = "operatingunit",
                            "countryname" = "countryname")) %>%
    filter(label == "prioritization",
           usaid_flag == "USAID",
           !is.na(ART_SAT))


# VIZ ----

  # Countries with valid data
  ous <- spdf_tx %>%
    st_set_geometry(NULL) %>%
    distinct(operatingunit) %>%
    pull()

  # Test viz
  art_saturation_map(
    spdf_art = spdf_tx,
    spdf = spdf_pepfar,
    terr = terr,
    country = "Nigeria",
    lbl_size = 4)

  art_saturation_map(
    spdf_art = spdf_tx,
    spdf = spdf_pepfar,
    terr = terr,
    country = "Nigeria",
    full_label = TRUE,
    lbl_size = 4)

  # Batch - maps for Pre-POART Slide deck
  ous[1] %>%
    map(function(cntry) {

      # Label size
      size <- ifelse(cntry == "Nigeria", 4, 3)

      # Map
      map <- art_saturation_map(
        spdf_art = spdf_tx,
        spdf = spdf_pepfar,
        terr = terr,
        country = cntry,
        lbl_size = size)

      # Export map
      si_save(
        filename = file.path(
          dir_graphics,
          paste0("FY21Q1 - ",
                 str_to_upper(cntry),
                 " - ART Saturation - ",
                 format(Sys.Date(), "%Y%m%d"),
                 ".png")),
        plot = map,
        width = 7,
        height = 7)

      return(map)
    })

  # Nigeria

  # map
  nga_map <- art_saturation_map(spdf_art = spdf_tx,
                                spdf = spdf_pepfar,
                                terr = terr,
                                country = "Nigeria",
                                full_label = TRUE,
                                lbl_size = 1.5)

  # bar chart
  nga_bars <- art_saturation_bars(df_tx, "Nigeria")

  # Combine
  nga_plot <- (nga_map + nga_bars)

  # Save
  si_save(
    filename = file.path(
      dir_graphics,
      paste0("FY21Q1 - NIGERIA - ART Saturation 2 - ",
             format(Sys.Date(), "%Y%m%d"),
             ".png")),
    plot = nga_plot,
    width = 10,
    height = 5)

  # Batch viz
  ous[1] %>%
    map(function(cntry) {

      # map
      map <- art_saturation_map(spdf_art = spdf_tx,
                                    spdf = spdf_pepfar,
                                    terr = terr,
                                    country = cntry,
                                    full_label = TRUE,
                                    lbl_size = 1.5)

      # bar chart
      bars <- art_saturation_bars(df_tx, cntry)

      # Combine
      viz <- (map + bars)

      print(viz)
    })

