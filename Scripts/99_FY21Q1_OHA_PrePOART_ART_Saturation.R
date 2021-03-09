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
  rep_qtr = 1

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
  #'
  art_saturation_map <-
    function(spdf_art, spdf,
             terr, country,
             lbl_size = 3,
             add_caption = TRUE) {

    # ART Sat
    spdf_art <- spdf_art %>%
        filter(operatingunit == country)

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
        palette = "burnt_siennas", #"scooters", #genoas", #, #
        discrete = FALSE,
        alpha = 0.7,
        na.value = NA,
        breaks = seq(0, 1, .25),
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
      # geom_sf_text(data = spdf_art,
      #              aes(label = paste0(psnu, "\n", percent(ART_SAT, 1))),
      #              size = lbl_size,
      #              color = grey10k) +
      geom_sf_text(data = spdf_art %>%
                     filter(ART_SAT >= .9, usaid_flag == "USAID"),
                   aes(label = percent(ART_SAT, 1)),
                   size = lbl_size,
                   color = grey10k)

    # Add caption
    if (add_caption == TRUE) {

      map <- map +
        labs(
          #title = "ART SATUTATION IN USAID SUPPORTED PSNUs",
          #subtitle = "PSNUs with labelled",
          caption = paste0("ART Saturation = TX_CURR_SUBNAT / PLHIV (FY21)",
                           "\nUSAID's PSNUs are labelled with name + percent saturation",
                           "\nSource: FY21Q1i NAT_SUBNAT & PSNU x IM MSDs, Produced on ",
                           format(Sys.Date(), "%Y-%m-%d")))

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

  # TX Viral Load
  df_vl <- df_psnu %>%
    filter(
      fiscal_year %in% c(2020, 2021), # Needed for Q1 vl
      str_to_lower(fundingagency) != "dedup",
      indicator %in% c("TX_PVLS", "TX_CURR"),
      standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")
    ) %>%
    rename(countryname = countrynamename) %>%
    mutate(
      indicator = if_else(
        indicator == "TX_PVLS" & numeratordenom == "D",
        paste0(indicator, "_D"),
        indicator
      )
    ) %>%
    group_by(fiscal_year,
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
    pivot_wider(names_from = indicator,
                values_from = value) %>%
    group_by(operatingunit, countryname, snu1, psnuuid, psnu) %>%
    mutate(VLC = TX_PVLS_D / lag(TX_CURR, 2, order_by = period)) %>%
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
    clean_psnu()

  # PLHIV
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

  # USAID Treatment

  df_tx_locs <- df_psnu %>%
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator",
           fundingagency == "USAID",
           str_detect(psnu, "_Military", negate = TRUE),
           fiscal_year == 2021) %>%
    distinct(operatingunit, psnuuid) %>%
    mutate(usaid_flag = "USAID")

  df_tx_locs_sa <- df_psnu %>%
    filter(operatingunit == "South Africa",
           indicator == "TX_CURR",
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "KeyPop/HIVStatus"),
           fundingagency == "USAID",
           str_detect(psnu, "_Military", negate = TRUE),
           fiscal_year == 2021) %>%
    distinct(operatingunit, psnuuid, psnu, standardizeddisaggregate) %>%
    mutate(usaid_flag = "USAID") %>%
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
    pivot_wider(names_from = standardizeddisaggregate,
                values_from = disagg) %>%
    #filter(is.na(KeyPop_HIVStatus)) %>%
    clean_psnu()

  df_tx_locs_sa %>% pull(psnu)


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
    filter(period == "FY21") %>%
    left_join(df_tx_locs, by = c("operatingunit", "psnuuid"))


  # Join to spatial file
  spdf_tx <- spdf_pepfar %>%
    left_join(df_tx, by = c("uid" = "psnuuid",
                            "operatingunit" = "operatingunit",
                            "countryname" = "countryname")) %>%
    filter(label == "prioritization",
           !is.na(ART_SAT)) %>%
    clean_psnu()


# VIZ ----

  # Countries with valid data
  spdf_tx %>%
    st_set_geometry(NULL) %>%
    distinct(operatingunit) %>%
    pull()

  # maps for Pre-POART Slide deck
  #c("Nigeria", "Uganda", "Zambia") %>%
  c("South Africa", "Eswatini", "Mozambique") %>%
  #c("Zambia") %>%
    map(function(cntry) {

      size <- ifelse(cntry == "Nigeria", 4, 3)

      map <- art_saturation_map(spdf_art = spdf_tx %>%
                                  filter(usaid_flag == "USAID"),
                         spdf = spdf_pepfar,
                         terr = terr,
                         country = cntry,
                         lbl_size = size)

      #print(map)

      # si_save(
      #   filename = file.path(
      #     dir_graphics,
      #     paste0("FY21Q1 - ",
      #     str_to_upper(cntry),
      #     " - ART Saturation - ",
      #     format(Sys.Date(), "%Y%m%d"),
      #     ".svg")),
      #   plot = map,
      #   width = 7,
      #   height = 7)

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
  spdf_tx_nga <- spdf_tx %>%
    filter(operatingunit == "Nigeria",
           label == "snu1")

  spdf_tx_nga %>%
    st_set_geometry(NULL) %>%
    view()

  # map
  nga_map <- art_saturation_map(spdf_art = spdf_tx_nga,
                            spdf = spdf_pepfar,
                            terr = terr,
                            country = "Nigeria",
                            lbl_size = 2)

  # bar chart
  nga_bar <- spdf_tx_nga %>%
    st_drop_geometry() %>%
    mutate(label = paste0(psnu, " (", comma(PLHIV, 1), ")")) %>%
    filter(operatingunit == "Nigeria",
           usaid_flag == "USAID") %>%
    ggplot(aes(reorder(label, PLHIV), PLHIV)) +
    geom_col(aes(fill = ART_SAT), show.legend = F) +
    geom_text(aes(label = percent(ART_SAT, 1)),
              hjust = -.2,
              size = 3,
              color = usaid_darkgrey) +
    scale_fill_si(
      palette = "burnt_siennas",
      discrete = FALSE,
      alpha = 0.7,
      breaks = seq(0, 1, .25),
      limits = c(0, 1),
      labels = percent
    ) +
    scale_y_continuous(labels = comma) +
    coord_flip() +
    labs(x = "", y = "") +
    si_style_xgrid()

  nga_plot <- (nga_map + nga_bar)

  si_save(
    filename = file.path(
      dir_graphics,
      paste0("FY21Q1 - NIGERIA - ART Saturation 2 - ",
             format(Sys.Date(), "%Y%m%d"),
             ".png")),
    plot = nga_plot,
    width = 10,
    height = 5)

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


