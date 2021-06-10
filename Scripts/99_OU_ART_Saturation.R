##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: ART Suturation by OU
##  LICENCE: MIT
##  DATE:    2021-03-02
##  UPDATE:  2021-06-09


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(glitr)
  library(glamr)
  library(gisr)
  library(extrafont)
  library(scales)
  library(ggtext)
  library(sf)
  library(ggrepel)
  library(patchwork)
  library(glue)
  library(ICPIutilities)

  # Quiet summary messages
  options(dplyr.summarise.inform = FALSE)

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
    pattern = "^MER_.*_PSNU_IM_.*_\\d{8}_v\\d{1}_\\d{1}.zip$",
    recursive = FALSE
  )

  # NAT Data - get the latest NAT_SUBNAT file
  file_natsub <- return_latest(
    folderpath = dir_merdata,
    pattern = "^MER_.*_NAT_SUBNAT_.*_\\d{8}_v\\d{1}_\\d{1}.zip$",
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
             rep_pd = "FY21Q2",
             lbl_size = 3,
             full_label = FALSE,
             add_caption = TRUE) {

    # ART Sat
    spdf_art <- spdf_art %>%
        filter(operatingunit == country)

    max <- spdf_art %>%
      pull(ART_SAT) %>%
      max()

    max <- ifelse(max < 1, 1, max)

    caption <- ifelse(country == "Nigeria",
                      paste0("ART Saturation = TX_CURR / PLHIV (", str_sub(rep_pd,1,4), ")",
                             "\nUSAID's PSNUs are labelled with name + percent saturation",
                             "\nSource: ", rep_pd, " PSNU x IM MSDs, Produced on ",
                             format(Sys.Date(), "%Y-%m-%d")),
                      paste0("ART Saturation = TX_CURR_SUBNAT / PLHIV (", str_sub(rep_pd,1,4), ")",
                             "\nUSAID's PSNUs are labelled with name + percent saturation",
                             "\nSource: ", rep_pd, " NAT_SUBNAT & PSNU x IM MSDs, Produced on ",
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
              size = 1.5) +
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

  df_nat %>% glimpse()

  df_nat %>%
    filter(fiscal_year %in% c("2020", "2021"),
           indicator %in% c("PLHIV", "TX_CURR_SUBNAT")) %>%
    select(fiscal_year, indicator, standardizeddisaggregate) %>%
    distinct(fiscal_year, indicator, standardizeddisaggregate)

  # PLHIV
  df_plhiv <- df_nat %>%
    filter(
      fiscal_year %in% rep_fys,
      indicator %in% c("PLHIV", "TX_CURR_SUBNAT"),
      standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex"),
      psnuuid != "?"
    ) %>%
    reshape_msd(clean = TRUE) %>%
    group_by(period, operatingunit, countryname, snu1, psnuuid, psnu, indicator) %>%
    summarise(across(value, sum, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = indicator, values_from = value)

  # USAID Treatment Areas

  df_tx_locs <- df_psnu %>%
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator",
           fundingagency == rep_agency,
           str_detect(psnu, "_Military", negate = TRUE),
           fiscal_year == rep_fy) %>%
    distinct(operatingunit, psnuuid) %>%
    mutate(usaid_flag = rep_agency)

  df_tx_locs_sa <- df_psnu %>%
    filter(operatingunit == "South Africa",
           indicator == "TX_CURR",
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "KeyPop/HIVStatus"),
           fundingagency == rep_agency,
           str_detect(psnu, "_Military", negate = TRUE),
           fiscal_year == rep_fy) %>%
    distinct(operatingunit, psnuuid, psnu, standardizeddisaggregate) %>%
    mutate(usaid_flag = rep_agency) %>%
    mutate(
      standardizeddisaggregate = str_replace_all(
        standardizeddisaggregate,
        "/", "_"
      ),
      disagg = case_when(
        standardizeddisaggregate == "Age_Sex_HIVStatus" ~ "REG",
        standardizeddisaggregate == "KeyPop_HIVStatus" ~ "KP",
        TRUE ~ standardizeddisaggregate
      )) %>%
    pivot_wider(names_from = standardizeddisaggregate, values_from = disagg) %>%
    clean_psnu()


  # ART Saturation: TX_CURR / PLHIV
  # df_tx <- df_vl %>%
  #   left_join(df_plhiv,
  #             by = c("fiscal_year" = "period",
  #                    "operatingunit" = "operatingunit",
  #                    "countryname" = "countryname",
  #                    "snu1" = "snu1",
  #                    "psnuuid" = "psnuuid",
  #                    "psnu" = "psnu"
  #                    )) %>%
  #   rowwise() %>%
  #   mutate(ART_SAT = TX_CURR_SUBNAT / PLHIV,
  #          ART_SAT2 = TX_CURR / PLHIV,
  #          ART_SAT3 = max(TX_CURR, TX_CURR_SUBNAT) / PLHIV,
  #          MOH_ADD = TX_CURR_SUBNAT > TX_CURR) %>%
  #   ungroup() %>%
  #   filter(fiscal_year == "FY21") %>%
  #   left_join(df_tx_locs, by = c("operatingunit", "psnuuid"))

  df_tx <- df_plhiv %>%
    rowwise() %>%
    mutate(ART_SAT = TX_CURR_SUBNAT / PLHIV) %>%
    ungroup() %>%
    filter(period == rep_fy2) %>%
    left_join(df_tx_locs, by = c("operatingunit", "psnuuid"))

  # Deal with NGA Case: TX_CURR_NAT is lower that TX_CURR
  df_psnu_tx <- df_psnu %>%
    filter(fiscal_year == rep_fy,
           indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator",
           fundingagency != "Dedup") %>%
    reshape_msd(clean = TRUE) %>%
    filter(period_type == "results",
           period == rep_pd)

  df_psnu_tx %>% glimpse()


  df_tx2 <- df_psnu_tx %>%
    group_by(operatingunit, countryname, snu1, psnuuid, psnu, indicator) %>%
    summarise_at(vars(value), sum, na.rm = TRUE) %>%
    ungroup() %>%
    pivot_wider(names_from = indicator, values_from = value) %>%
    left_join(df_plhiv %>% filter(period == rep_fy2),
              by = c("operatingunit", "countryname",
                     "snu1", "psnuuid", "psnu")) %>%
    rowwise() %>%
    mutate(ART_SAT = if_else(operatingunit == "Nigeria",
                             TX_CURR / PLHIV,
                             TX_CURR_SUBNAT / PLHIV)) %>%
    ungroup() %>%
    filter(period == rep_fy2) %>%
    left_join(df_tx_locs, by = c("operatingunit", "psnuuid"))

  df_tx2 %>% glimpse()

  # Join to spatial file
  spdf_tx <- spdf_pepfar %>%
    left_join(df_tx,
              by = c("uid" = "psnuuid",
                     "operatingunit" = "operatingunit",
                     "countryname" = "countryname")) %>%
    filter(label == "prioritization", !is.na(ART_SAT)) %>%
    clean_psnu()

  spdf_tx2 <- spdf_pepfar %>%
    left_join(df_tx2,
              by = c("uid" = "psnuuid",
                     "operatingunit" = "operatingunit",
                     "countryname" = "countryname")) %>%
    filter(label == "prioritization", !is.na(ART_SAT)) %>%
    clean_psnu()


# VIZ ----

  # Countries with valid data
  spdf_tx %>%
    st_set_geometry(NULL) %>%
    distinct(operatingunit) %>%
    pull()

  # maps for Pre-POART Slide deck
  #c("Nigeria", "Uganda", "Zambia") %>%
  #c("South Africa", "Eswatini", "Mozambique") %>%
  #c("Zambia") %>%
  c("Nigeria") %>%
    map(function(cntry) {

      size <- ifelse(cntry == "Nigeria", 3, 3)

      map <- art_saturation_map(
        spdf_art = spdf_tx2 %>%
          filter(label == "prioritization",
                 usaid_flag == "USAID"),
        spdf = spdf_pepfar,
        terr = terr,
        country = cntry,
        rep_pd = rep_pd,
        lbl_size = size,
        full_label = TRUE)

      # PNG File
      si_save(
        filename = file.path(
          dir_graphics,
          paste0(rep_pd, " - ",
                 str_to_upper(cntry),
                 " - ", rep_agency,
                 " - ART Saturation - ",
                 format(Sys.Date(), "%Y%m%d"),
                 ".png")),
        plot = map,
        width = 7,
        height = 7)

      # SVG File
      si_save(
        filename = file.path(
          dir_graphics,
          paste0(rep_pd, " - ",
                 str_to_upper(cntry),
                 " - ", rep_agency,
                 " - ART Saturation - ",
                 format(Sys.Date(), "%Y%m%d"),
                 ".svg")),
        plot = map,
        width = 7,
        height = 7)

      return(map)
    })

  # Nigeria

  cntry = "Nigeria"

  spdf_tx_nga <- spdf_tx %>% filter(operatingunit == cntry)

  # map
  nga_map <- art_saturation_map(spdf_art = spdf_tx_nga,
                            spdf = spdf_pepfar,
                            terr = terr,
                            country = "Nigeria",
                            rep_pd = rep_pd,
                            full_label = TRUE,
                            lbl_size = 1.5)

  # bar chart
  max_art <- spdf_tx_nga %>%
    st_drop_geometry() %>%
    pull(ART_SAT) %>%
    max()

  nga_high_plhiv <- spdf_tx_nga %>%
    st_drop_geometry() %>%
    mutate(label = paste0(psnu, " (", percent(ART_SAT, 1), " ", comma(PLHIV, 1), ")")) %>%
    filter(PLHIV == max(PLHIV))

  nga_bar <- spdf_tx_nga %>%
    st_drop_geometry() %>%
    mutate(label = paste0(psnu, " (", percent(ART_SAT, 1), " ", comma(PLHIV, 1), ")")) %>%
    filter(operatingunit == "Nigeria",
           usaid_flag == "USAID") %>%
    ggplot(aes(reorder(label, PLHIV), PLHIV)) +
    geom_col(aes(fill = ART_SAT), show.legend = F) +
    annotate(
      geom = "curve",
      x = 13, #nga_high_plhiv$label,
      y = (nga_high_plhiv$PLHIV - 50000),
      xend = 16,#nga_high_plhiv$label,
      yend = (nga_high_plhiv$PLHIV - 30000),
      curvature = -.3, arrow = arrow(length = unit(4, "mm")),
      color = grey50k
    ) +
    annotate(geom = "text",
             x = 13, #nga_high_plhiv$label,
             y = (nga_high_plhiv$PLHIV - 51000),
             label = paste0("High PLHIV (",
                            comma(nga_high_plhiv$PLHIV, 1),
                            ") \nwith low (",
                            percent(nga_high_plhiv$ART_SAT, 1),
                            ") saturation"),
             size = 2,
             hjust = 1,
             color = grey50k, family = "Source Sans Pro") +
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
    si_style_xgrid() +
    theme(axis.text = element_text(size = 5))

  # Map + plot
  nga_plot <- (nga_map + nga_bar)

  si_save(
    filename = file.path(
      dir_graphics,
      paste0(rep_pd, " - ",
             str_to_upper(cntry),
             " - ", rep_agency,
             " - ART Saturation 2 - ",
             format(Sys.Date(), "%Y%m%d"),
             ".png")),
    plot = nga_plot,
    width = 10,
    height = 5)

  si_save(
    filename = file.path(
      dir_graphics,
      paste0(rep_pd, " - ",
             str_to_upper(cntry),
             " - ", rep_agency,
             " - ART Saturation 3 - ",
             format(Sys.Date(), "%Y%m%d"),
             ".svg")),
    plot = nga_plot,
    width = 10,
    height = 5)

  # Plot only
  si_save(
    filename = file.path(
      dir_graphics,
      paste0(rep_pd, " - ",
             str_to_upper(cntry),
             " - ", rep_agency,
             " - ART Saturation 3 - ",
             format(Sys.Date(), "%Y%m%d"),
             ".png")),
    plot = nga_bar,
    width = 7,
    height = 7)

  si_save(
    filename = file.path(
      dir_graphics,
      paste0(rep_pd, " - ",
             str_to_upper(cntry),
             " - ", rep_agency,
             " - ART Saturation 3 - ",
             format(Sys.Date(), "%Y%m%d"),
             ".svg")),
    plot = nga_plot,
    width = 7,
    height = 7)

  # Map only
  si_save(
    filename = file.path(
      dir_graphics,
      paste0(rep_pd, " - ",
             str_to_upper(cntry),
             " - ART Saturation 4 - ",
             format(Sys.Date(), "%Y%m%d"),
             ".png")),
    plot = nga_map,
    width = 7,
    height = 7)

  si_save(
    filename = file.path(
      dir_graphics,
      paste0(rep_pd, " - ",
             str_to_upper(cntry),
             " - ART Saturation 4 - ",
             format(Sys.Date(), "%Y%m%d"),
             ".svg")),
    plot = nga_map,
    width = 7,
    height = 7)

  # SA
  spdf_tx_sa <- spdf_tx %>%
    filter(operatingunit == "South Africa",
           usaid_flag == "USAID",
           #!psnu %in% c("City of Tshwane", "Ekurhuleni", "Ethekwini"))
           !psnu %in% c("Vhembe", "Nelson Mandela Bay", "Cape Winelands",
                        "City of Tshwane", "Ekurhuleni"))

  spdf_tx_sa %>%
    filter(ART_SAT >= .9, usaid_flag == "USAID") %>%
    pull(psnu)

  # map
  sa_map <- art_saturation_map(spdf_art = spdf_tx_sa,
                            spdf = spdf_pepfar,
                            terr = terr,
                            country = "South Africa",
                            lbl_size = 2)

  # bar chart
  sa_bar <- spdf_tx_sa %>%
    st_drop_geometry() %>%
    mutate(label = paste0(psnu, " (", percent(ART_SAT, 1), " ", comma(PLHIV, 1), ")")) %>%
    ggplot(aes(reorder(label, PLHIV), PLHIV)) +
    geom_col(aes(fill = ART_SAT), show.legend = F) +
    # geom_text(aes(label = percent(ART_SAT, 1)),
    #           hjust = -.2,
    #           size = 3,
    #           color = usaid_darkgrey) +
    scale_fill_si(
      palette = "burnt_siennas",
      discrete = FALSE,
      alpha = 0.7,
      breaks = seq(0, 1, .25),
      limits = c(0, 1),
      labels = percent
    ) +
    scale_y_continuous(breaks = seq(0, max(spdf_tx_sa$PLHIV), 200000),
                       labels = comma,
                       position = "right") +
    coord_flip() +
    labs(x = "", y = "") +
    si_style_xgrid()

  sa_plot <- (sa_map + sa_bar)

  si_save(
    filename = file.path(
      dir_graphics,
      paste0("FY21Q1 - SOUTH AFRICA - ART Saturation 2 - ",
             format(Sys.Date(), "%Y%m%d"),
             ".png")),
    plot = sa_plot,
    width = 10,
    height = 5)


