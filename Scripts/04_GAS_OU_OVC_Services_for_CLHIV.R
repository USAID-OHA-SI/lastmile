##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: PEDS with no access to OVC Services
##  LICENCE: MIT
##  DATE:    2021-04-07


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(glitr)
  library(glamr)
  library(gisr)
  library(ICPIutilities)
  library(sf)
  library(scales)
  library(ggtext)
  library(ggrepel)
  library(tidytext)
  library(ggflags)
  library(patchwork)
  library(glue)
  library(extrafont)
  library(emojifont)

  # Code re-use: placeholder till migration to one of the our packages
  source("./Scripts/00_Geo_Utilities.R")
  source("./Scripts/00_OVC_Utilities.R")

# SETUP ----

  # Project specific folders
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"

  # Ref folders outside of project
  dir_geodata <- si_path("path_vector")
  dir_attrs <- file.path(dir_geodata, "OU-Attributes")
  dir_terr <- si_path("path_raster")
  dir_merdata <- si_path("path_msd")

  # GLOBAL VARS

  # Reporting Filters

  cntry <- "Nigeria"

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

  # msd version
  msd_version <- ifelse(str_detect(file_psnu_im, ".*_\\d{8}_v1_\\d"), "i", "c")
  msd_version <- paste0(rep_pd, msd_version)

  # Shapefile path
  file_shp <- return_latest(
    folderpath = dir_geodata,
    pattern = "VcPepfarPolygons.*.shp",
    recursive = TRUE
  )


# FUNCTIONS ----

  # CLHIV / OVC Map
  map_ovc4clhiv <- function(spdf, terr, df,
                            country = "Nigeria",
                            agency = "",
                            rep_pd = "FY21Q2",
                            save = FALSE) {

    # params
    cntry <- {{country}}

    print(glue("Country = {cntry} & Agency = {agency}"))

    df_tx <- df %>%
      filter(countryname == cntry) %>%
      dplyr::select(-c(operatingunit, countryname)) %>%
      clean_psnu()

    # admin 0 and 1 for basemap
    admin0 <- spdf %>%
      filter(operatingunit == cntry,
             label == "country")

    admin1 <- spdf %>%
      filter(operatingunit == cntry,
             label == "snu1")

    admin_psnu <- spdf %>%
      filter(operatingunit == cntry,
             label == "prioritization")

    # Produce basemap
    basemap <- terrain_map(countries = admin0,
                           adm0 = admin0,
                           adm1 = admin1,
                           mask = TRUE,
                           terr = terr)

    # Add program data to geometries
    spdf_tx <- spdf %>%
      filter(countryname == cntry,
             label == 'prioritization') %>%
      left_join(df_tx, by = c("uid" = "psnuuid")) %>%
      filter(!is.na(tx_curr))

    # Check for valid data
    if (nrow(spdf_tx) == 0) {
      message(glue("No data found for {cntry}"))
      return(NULL)
    }

    spdf_tx <- spdf_tx %>%
      mutate(
        tx_curr_color = if_else(ovc_serv == 0, usaid_red, grey90k),
        tx_curr_label = paste0("<span style = 'color: ",
                               if_else(ovc_serv == 0, usaid_red, grey90k),
                               "; ",
                               "text-decoration: ",
                               if_else(ovc_serv == 0, "underline", "none"),
                               ";'>",
                               comma(tx_curr, accuracy = 1),
                               "</span>")
      )

    # Produce thematic maps

    # CLHIV PSNUs with no OVC Programs
    n_psnu <- nrow(spdf_tx)

    nn_psnu <- spdf_tx %>%
      filter(ovc_serv == 0) %>%
      nrow()

    print(glue("PSNUs with no OVC = {nn_psnu} / {n_psnu}"))

    map_psnus <- basemap +
      geom_sf(data = spdf_tx,
              fill = genoa,
              color = grey10k,
              lwd = .3,
              alpha = .3) +
      geom_sf(data = admin0, fill = NA, size = 1, color = grey10k) +
      geom_sf(data = admin0, fill = NA, size = .5, color = grey90k) +
      geom_sf_text(data = spdf_tx,
                   aes(label = psnu, color = tx_curr_color),
                   size = 5, check_overlap = TRUE) +
      scale_color_identity() +
      labs(title = paste0(
             str_to_upper(cntry),
             " - OVC SERVICES FOR CHILDREN LIVING WITH HIV"
           ),
           subtitle = paste0("<span style = 'color:#ba0c2f;'>", nn_psnu,
                             "</span> out of ", n_psnu,
                             " PSNU(s) with CLHIV dont have OVC Services",
                             ifelse(agency != "",
                                    glue(" ({agency})"),
                                    " (All Agencies)")),
           caption = paste0(glue("Source: MSD {msd_version}"),
                            " - Produced on ",
                            format(Sys.Date(), "%Y-%m-%d"),
                            "\nRed labels indicate no OVC Coverage")
           ) +
      si_style_map() +
      theme(plot.title = element_text(size = 14),
            plot.subtitle = element_markdown(size = 12),
            plot.caption = element_text(size = 10))

    # # of CLHIV Outsite OVC PSNUs

    n_clhiv <- spdf_tx %>%
      st_drop_geometry() %>%
      group_by(countryname) %>%
      summarise(across(tx_curr, sum, na.rm = TRUE)) %>%
      ungroup() %>%
      pull(tx_curr)

    nn_clhiv <- spdf_tx %>%
      st_drop_geometry() %>%
      filter(ovc_serv == 0) %>%
      group_by(countryname) %>%
      summarise(across(tx_curr, sum, na.rm = TRUE)) %>%
      ungroup() %>%
      pull(tx_curr)

    p_clhiv <- percent(nn_clhiv / n_clhiv, 1)

    print(glue("CLHIV have no access to OVC = {nn_clhiv} / {n_clhiv}"))

    map_clhiv <- basemap +
      geom_sf(data = spdf_tx,
              fill = genoa,
              color = grey10k,
              lwd = .3,
              alpha = .3) +
      geom_sf(data = admin0, fill = NA, size = 1, color = grey10k) +
      geom_sf(data = admin0, fill = NA, size = .5, color = grey90k) +
      geom_sf_text(data = spdf_tx,
                   aes(label = comma(tx_curr, accuracy = 1),
                       color = tx_curr_color),
                   size = 5, fontface = "bold",
                   check_overlap = TRUE) +
      scale_color_identity() +
      labs(title = paste0(
             str_to_upper(cntry),
             " - CLHIV WITHOUT ACCESS TO OVC SERVICES"
            ),
           subtitle = paste0(p_clhiv,
                             " of CLHIV (<span style = 'color:#ba0c2f;'>",
                             comma(nn_clhiv),
                             "</span> out of ",
                             comma(n_clhiv),
                             ") do not have access to OVC Services",
                             ifelse(agency != "",
                                    glue(" ({agency})"),
                                    " (All Agencies)")),
           caption = paste0(glue("Source: MSD {rep_pd}{msd_version}"),
                            " - Produced on ",
                            format(Sys.Date(), "%Y-%m-%d"),
                            "\nRed labels indicate no OVC Coverage")
      ) +
      si_style_map() +
      theme(plot.title = element_text(size = 14),
            plot.subtitle = element_markdown(size = 12),
            plot.caption = element_text(size = 10))


    # Save maps as png files
    if (save == TRUE) {

      # Map #1
      si_save(
        filename = file.path(
          dir_graphics,
          paste0(glue("{rep_pd} - "),
                 ifelse(agency != "", glue("{agency} - "), "All Agencies - "),
                 str_to_upper(cntry),
                 " CLHIV PSNUs with no OVC Services - ",
                 format(Sys.Date(), "%Y%m%d"),
                 ".png")),
        plot = map_psnus)

      # Map #2
      si_save(
        filename = file.path(
          dir_graphics,
          paste0(glue("{rep_pd} - "),
                 ifelse(agency != "", glue("{agency} - "), "All Agencies - "),
                 str_to_upper(cntry),
                 " CLHIV with no access to OVC Services - ",
                 format(Sys.Date(), "%Y%m%d"),
                 ".png")),
        plot = map_clhiv)

    } else {
      print(map_psnus)
      print(map_clhiv)
    }

    return(map_clhiv)
  }

# Import Data ----

  # MER Data
  df_psnu <- file_psnu_im %>%
    read_msd()

  df_psnu <- df_psnu %>%
    rename(countryname = countrynamename) %>%
    clean_agency() %>%
    clean_psnu()

  # Geo Data
  terr <- gisr::get_raster(terr_path = dir_terr)

  spdf_pepfar <- file_shp %>% sf::read_sf()

  # Ou / Countries
  ous <- gisr::get_ouuids(add_details = TRUE)

  df_attrs <- ous %>%
    filter(!str_detect(operatingunit, " Region$")) %>%
    pull(operatingunit) %>%
    map_dfr(.x, .f = ~get_attributes(country = .x, folderpath = dir_attrs))

  spdf_pepfar <- spdf_pepfar %>%
    left_join(df_attrs, by = c("uid" = "id"))

# MUNGE ----

  # OVC

  df_ovc <- df_psnu %>%
    filter(fiscal_year == rep_fy,
           indicator == "OVC_SERV",
           standardizeddisaggregate == "Total Numerator",
           str_to_lower(fundingagency) != "dedup") %>%
    mutate(ovc_program = if_else(!is.na(cumulative), TRUE, FALSE)) %>%
    filter(ovc_program == TRUE) %>%
    group_by(fundingagency, operatingunit, countryname,
             primepartner, psnu, psnuuid) %>%
    summarise(ovc_serv = sum(cumulative, na.rm = TRUE)) %>%
    ungroup()

  # TX PEDS

  df_tx <- df_psnu %>%
    filter(fiscal_year == rep_fy,
           indicator == "TX_CURR",
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           str_to_lower(fundingagency) != "dedup",
           trendscoarse == "<15") %>%
    mutate(clhiv = if_else(!is.na(cumulative), TRUE, FALSE)) %>%
    filter(clhiv == TRUE) %>%
    group_by(fundingagency, operatingunit, countryname,
             primepartner, psnu, psnuuid) %>%
    summarise(tx_curr = sum(cumulative, na.rm = TRUE)) %>%
    ungroup()


  # PEDS OVC Coverage
  df_ovc_cov <- df_tx %>%
    full_join(df_ovc,
              by = c("fundingagency", "operatingunit",
                     "countryname", "primepartner", "psnu", "psnuuid"))


  df_ou_ovc_cov <- df_ovc_cov %>%
    group_by(operatingunit, countryname, psnu, psnuuid) %>%
    summarise(across(c(tx_curr, ovc_serv), sum, na.rm = TRUE)) %>%
    ungroup()

  df_usaid_ovc_cov <- df_ovc_cov %>%
    filter(fundingagency == "USAID") %>%
    group_by(operatingunit, countryname, psnu, psnuuid) %>%
    summarise(across(c(tx_curr, ovc_serv), sum, na.rm = TRUE)) %>%
    ungroup()

  spdf_ou_cov <- spdf_pepfar %>%
    left_join(df_ou_ovc_cov %>%
                dplyr::select(-c(operatingunit, countryname)),
              by = c("uid" = "psnuuid"), keep = TRUE) %>%
    filter(label == 'prioritization', !is.na(tx_curr))

# VIZ ----

  # Global Situation ----
  df_ou_ovc_cov %>%
    group_by(operatingunit, countryname) %>%
    summarize_at(vars(tx_curr, ovc_serv), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(ovc_flag = if_else(ovc_serv > 0, burnt_sienna, genoa),
           countryname = case_when(
             countryname == "Democratic Republic of the Congo" ~ "DRC",
             countryname == "Dominican Republic" ~ "DR",
             countryname == "Trinidad and Tobago" ~ "TT",
             TRUE ~ countryname
           ),
           countryname = paste0("<span style = 'color: ",
                                if_else(ovc_serv == 0, burnt_sienna, grey90k),
                                ";'>",
                                countryname,
                                "</span>"),
           countryname = paste0(countryname, " (",
                                comma(ovc_serv, 1),
                                ")")) %>%
    ggplot(data = .,
           aes(x = reorder(countryname, tx_curr),
               y = tx_curr,
               fill = tx_curr)) +
    geom_col(show.legend = F) +
    scale_fill_si(discrete = F) +
    scale_y_continuous(position = "right") +
    coord_flip() +
    labs(x = "", y = "",
         title = "OVC COVERAGE FOR CHILDREN LIVING WITH HIV",
         subtitle = "Countries highlighted in red, have CLHIV with no OVC Programs",
         caption = glue("Source: MSD {rep_pd}{msd_version} - Produced on {format(Sys.Date(), '%Y-%m-%d')}")) +
    si_style_xgrid() +
    theme(axis.text.y = element_markdown())

  # PSNU COVERAGE for All Agencies ----
  df_ou_ovc_cov %>%
    filter(tx_curr > 0) %>%
    group_by(operatingunit, countryname) %>%
    summarise(tx_curr_psnu = sum(tx_curr > 0),
              ovc_serv_psnu = sum(ovc_serv > 0),
              ovc_cov = ovc_serv_psnu / tx_curr_psnu) %>%
    ungroup() %>%
    filter(ovc_cov > 0) %>%
    mutate(ovc_flag = if_else(ovc_serv_psnu == 0, burnt_sienna, genoa),
           tx_curr_color = if_else(ovc_cov > 0.3, grey10k, grey90k),
           countryname = case_when(
             countryname == "Democratic Republic of the Congo" ~ "DRC",
             countryname == "Dominican Republic" ~ "DR",
             countryname == "Trinidad and Tobago" ~ "TT",
             TRUE ~ countryname
           ),
           countryname = paste0("<span style = 'color: ",
                                if_else(ovc_cov == 0, burnt_sienna, grey90k),
                                ";'>",
                                countryname,
                                "</span>"),
           countryname = paste0(countryname, " (",
                                comma(tx_curr_psnu, accuracy = 1),
                                ")")) %>%
    ggplot(data = .,
           aes(x = reorder(countryname, ovc_cov),
               y = ovc_cov,
               fill = ovc_cov)) +
    geom_col(show.legend = F) +
    geom_text(aes(label = percent(ovc_cov, 1), color = tx_curr_color),
              hjust = 1, nudge_y = -.005, size = 4) +
    scale_fill_si(discrete = F) +
    scale_y_continuous(labels = percent, position = "right", expand = c(.001, .001)) +
    scale_color_identity() +
    coord_flip() +
    labs(x = "", y = "",
         title = "% PSNU COVERAGE OF OVC PROGRAMS FOR CHILDREN LIVING WITH HIV",
         #subtitle = "Lesotho and Haiti are the only OUs with OVC Programs in all PSNUs with PEDs (All Agencies)",
         caption = glue("Source: MSD {msd_version} - Produced on {format(Sys.Date(), '%Y-%m-%d')}")) +
    si_style_xgrid() +
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_markdown(size = 15),
          plot.title = element_text(size = 18),
          plot.subtitle = element_markdown(size = 16),
          plot.caption = element_text(size = 13))

  si_save(
    filename = file.path(
      dir_graphics,
      glue("{rep_pd} - All Agencies - OU Proportion of PSNUs with OVC Services - {format(Sys.Date(), '%Y%m%d')}.png")),
    plot = last_plot())


  # PSNU COVERAGE for USAID ONLY ----
  df_usaid_ovc_cov %>%
    filter(tx_curr > 0) %>%
    group_by(operatingunit, countryname) %>%
    summarise(tx_curr_psnu = sum(tx_curr > 0),
              ovc_serv_psnu = sum(ovc_serv > 0),
              ovc_cov = ovc_serv_psnu / tx_curr_psnu) %>%
    ungroup() %>%
    filter(ovc_cov > 0) %>%
    mutate(ovc_flag = if_else(ovc_serv_psnu == 0, burnt_sienna, genoa),
           tx_curr_color = if_else(ovc_cov > 0.3, grey10k, grey90k),
           countryname = case_when(
             countryname == "Democratic Republic of the Congo" ~ "DRC",
             countryname == "Dominican Republic" ~ "DR",
             countryname == "Trinidad and Tobago" ~ "TT",
             TRUE ~ countryname
           ),
           countryname = paste0("<span style = 'color: ",
                                if_else(ovc_cov == 0, burnt_sienna, grey90k),
                                ";'>",
                                countryname,
                                "</span>"),
           countryname = paste0(countryname, " (",
                                comma(tx_curr_psnu, accuracy = 1),
                                ")")) %>%
    ggplot(data = .,
           aes(x = reorder(countryname, ovc_cov),
               y = ovc_cov,
               fill = ovc_cov)) +
    geom_col(show.legend = F) +
    geom_text(aes(label = percent(ovc_cov, 1), color = tx_curr_color),
              hjust = 1, nudge_y = -.005, size = 4) +
    scale_fill_si(discrete = F) +
    scale_y_continuous(labels = percent, position = "right", expand = c(.01, .01)) +
    scale_color_identity() +
    coord_flip(clip = "off") +
    labs(x = "", y = "",
         title = "% PSNU COVERAGE OF OVC PROGRAMS FOR CHILDREN LIVING WITH HIV",
         #subtitle = "Eswatini, Haiti, Kenya & Lesotho are the only OUs with OVC Programs in all PSNUs with PEDs (USAID)",
         caption = glue("Source: MSD {msd_version} - Produced on {format(Sys.Date(), '%Y-%m-%d')}")) +
    si_style_xgrid() +
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_markdown(size = 15),
          plot.title = element_text(size = 18),
          plot.subtitle = element_markdown(size = 16),
          plot.caption = element_text(size = 13))

  si_save(
    filename = file.path(
      dir_graphics,
      glue("{rep_pd} - USAID - OU Proportion of PSNUs with OVC Services - {format(Sys.Date(), '%Y%m%d')}.png")),
    plot = last_plot())

  # Country map ----

  # # all agencies
  # df_ou_ovc_cov %>%
  #   map_ovc4clhiv(spdf = spdf_pepfar,
  #                 terr = terr,
  #                 df = .,
  #                 country = cntry,
  #                 rep_pd = rep_pd)
  #
  # # usaid only
  df_usaid_ovc_cov %>%
    map_ovc4clhiv(spdf = spdf_pepfar,
                  terr = terr,
                  df = .,
                  country = cntry)


  # All Agencies
  df_ou_ovc_cov %>%
    filter(!str_detect(operatingunit, " Region$")) %>%
    distinct(countryname) %>%
    pull() %>%
    map(~map_ovc4clhiv(spdf = spdf_pepfar,
                       terr = terr,
                       df = df_ou_ovc_cov,
                       country = .x,
                       rep_pd = rep_pd,
                       save = T))

  # USAID Only
  df_usaid_ovc_cov %>%
    filter(!str_detect(operatingunit, " Region$")) %>%
    distinct(countryname) %>%
    pull() %>%
    map(~map_ovc4clhiv(spdf = spdf_pepfar,
                       terr = terr,
                       df = df_usaid_ovc_cov,
                       country = .x,
                       agency = 'USAID',
                       rep_pd = rep_pd,
                       save = T))
