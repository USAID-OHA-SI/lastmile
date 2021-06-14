##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: ART Suturation by OU
##  LICENCE: MIT
##  DATE:    2021-03-02
##  UPDATE:  2021-06-14


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

  rep_ref_pd = rep_fys %>%
    first() %>%
    as.character() %>%
    str_sub(3,4) %>%
    paste0("FY", ., "Q4")

  rep_pds <- c(rep_ref_pd, rep_pd)

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
    filter(fiscal_year %in% rep_fys,
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
    filter(fiscal_year %in% rep_fys,
           indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator",
           fundingagency != "Dedup") %>%
    reshape_msd(clean = TRUE) %>%
    filter(period_type == "results", period %in% rep_pds)

  df_psnu_tx %>% glimpse()


  df_tx2 <- df_psnu_tx %>%
    group_by(period, operatingunit, countryname, snu1, psnuuid, psnu, indicator) %>%
    summarise_at(vars(value), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(period2 = str_sub(period, 1, 4)) %>%
    pivot_wider(names_from = indicator, values_from = value) %>%
    left_join(df_plhiv,
              by = c("operatingunit", "countryname",
                     "snu1", "psnuuid", "psnu", "period2" = "period")) %>%
    rowwise() %>%
    mutate(ART_SAT = if_else(operatingunit == "Nigeria",
                             TX_CURR / PLHIV,
                             TX_CURR_SUBNAT / PLHIV)) %>%
    ungroup() %>%
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

  spdf_tx_nga <- spdf_tx2 %>%
    filter(operatingunit == cntry,
           period2 == rep_fy2)

  # ART Sat trend
  df_tx3 <- df_tx2 %>%
    filter(operatingunit == cntry, usaid_flag == rep_agency) %>%
    select(period, psnu, art_sat = ART_SAT) %>%
    pivot_wider(names_from = period, values_from = art_sat) %>%
    mutate(psnu = paste0(psnu, " (", percent(FY21Q2, 1), ")"),
           change_color = if_else(FY21Q2 - FY20Q4 > 0, burnt_sienna, usaid_red))

  # map
  nga_map <- art_saturation_map(spdf_art = spdf_tx_nga,
                            spdf = spdf_pepfar,
                            terr = terr,
                            country = "Nigeria",
                            rep_pd = rep_pd,
                            full_label = TRUE,
                            lbl_size = 1.5)

  nga_usaid_map <- art_saturation_map(spdf_art = spdf_tx_nga %>%
                                        filter(usaid_flag == "USAID"),
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

  # Change plot
  nga_dots <- df_tx3 %>%
    ggplot(aes(x = reorder(psnu, FY21Q2),
               y = FY21Q2)) +
      geom_hline(yintercept = .9,
                 lty = "dashed", lwd = 1,
                 color = usaid_darkgrey) +
      geom_segment(aes(xend = psnu,
                       y = FY20Q4,
                       yend = FY21Q2,
                       color = FY21Q2),
                   size = 1, alpha = .7,
                   show.legend = FALSE) +
      geom_point(aes(y = FY20Q4),
                 shape = 21,
                 fill = grey50k,
                 size = 4 ,
                 color = grey10k) +
      geom_point(aes(y = FY21Q2, fill = FY21Q2),
                 shape = 21, size = 5,
                 color = grey10k,
                 show.legend = F) +
      scale_fill_si(
        palette = "burnt_siennas",
        discrete = FALSE,
        alpha = 1,
        breaks = c(.25, .5, .75, 1)
      ) +
      scale_color_si(
        palette = "burnt_siennas",
        discrete = FALSE
      ) +
      scale_y_continuous(labels = percent, position = "right") +
      #scale_color_identity() +
      coord_flip() +
      labs(x = "", y = "") +
      si_style()

  # Map + bar plot
  nga_plot <- (nga_map + nga_bar)

  si_save(
    filename = file.path(
      dir_graphics,
      paste0(rep_pd, " - ",
             str_to_upper(cntry),
             " - ", rep_agency,
             " - ART Saturation MapOU and BarsUSAID - ",
             format(Sys.Date(), "%Y%m%d"),
             ".png")),
    plot = nga_plot,
    width = 10,
    height = 5)

  # USAID only - Map + bar plot
  nga_usaid_plot <- (nga_usaid_map + nga_bar) &
    theme(plot.caption = element_blank())

  si_save(
    filename = file.path(
      dir_graphics,
      paste0(rep_pd, " - ",
             str_to_upper(cntry),
             " - ", rep_agency,
             " - ART Saturation MapUSAID_and_BarsUSAID - ",
             format(Sys.Date(), "%Y%m%d"),
             ".png")),
    plot = nga_usaid_plot,
    width = 10,
    height = 5)


  # USAID only - Map + dots plot
  nga_usaid_plot2 <- (nga_usaid_map + nga_dots) +
    plot_annotation(
      title = glue("{str_to_upper(cntry)} - {rep_pd} ART SATURATION"),
      subtitle = "Increase in ART Saturation between <span style='color:#939598;'>**FY20Q4**</span> and <span style='color:#e07653'>**FY21Q2**</span> in all states<br/>States are sorted by FY21Q2 % ART Saturation"
    ) &
    theme(plot.caption = element_blank(),
          plot.title = element_markdown(hjust = .5),
          plot.subtitle = element_markdown(hjust = .5))

  si_save(
    filename = file.path(
      dir_graphics,
      paste0(rep_pd, " - ",
             str_to_upper(cntry),
             " - ", rep_agency,
             " - ART Saturation MapUSAID_and_DotsChart - ",
             format(Sys.Date(), "%Y%m%d"),
             ".png")),
    plot = nga_usaid_plot2,
    width = 10,
    height = 5)

  # Plot only
  si_save(
    filename = file.path(
      dir_graphics,
      paste0(rep_pd, " - ",
             str_to_upper(cntry),
             " - ", rep_agency,
             " - ART Saturation BarsUSAID - ",
             format(Sys.Date(), "%Y%m%d"),
             ".svg")),
    plot = nga_plot,
    width = 7,
    height = 7)

  si_save(
    filename = file.path(
      dir_graphics,
      paste0(rep_pd, " - ",
             str_to_upper(cntry),
             " - ", rep_agency,
             " - ART Saturation BarsUSAID - ",
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
             " - ART Saturation DotsUSAID - ",
             format(Sys.Date(), "%Y%m%d"),
             ".svg")),
    plot = nga_dots,
    width = 7,
    height = 7)

  si_save(
    filename = file.path(
      dir_graphics,
      paste0(rep_pd, " - ",
             str_to_upper(cntry),
             " - ", rep_agency,
             " - ART Saturation DotsUSAID - ",
             format(Sys.Date(), "%Y%m%d"),
             ".png")),
    plot = nga_dots,
    width = 7,
    height = 7)


  # Map only
  si_save(
    filename = file.path(
      dir_graphics,
      paste0(rep_pd, " - ",
             str_to_upper(cntry),
             " - ART Saturation MapOU - ",
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
             " - ART Saturation MapUSAID - ",
             format(Sys.Date(), "%Y%m%d"),
             ".png")),
    plot = nga_usaid_map,
    width = 7,
    height = 7)
