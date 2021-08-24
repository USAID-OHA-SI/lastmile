##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: MMD Distribution
##  LICENCE: MIT
##  DATE:    2021-03-02
##  UPDATED: 2021-08-23


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
  library(tidytext)
  library(patchwork)
  library(glue)
  library(ICPIutilities)
  library(rnaturalearth)

  #source("./Scripts/00_Geo_Utilities.R")
  source("./Scripts/00_VL_Utilities.R")

# SETUP ----

  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"

  dir_geodata <- si_path("path_vector")
  dir_terr <- si_path("path_raster")
  dir_merdata <- si_path("path_msd")

  ## GDRIVE
  gdrive_tx_mmd <- "1dlXUE8kapmqCvIyNN5tD5y_lRsnTFaxv"

  ## Reporting Filters
  rep_agency = "USAID"
  rep_agencies <- c("USAID", "HHS/CDC")

  cntry <- "Nigeria"

  # Reporting periods
  source("./Scripts/00_Quarterly_Updates.R")

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

  # MSD File version
  msd_version <- ifelse(str_detect(file_psnu_im, ".*_\\d{8}_v1_\\d"), "i", "c")
  msd_caption <- paste0(rep_pd, msd_version)

  dir_graphics %<>%
    paste0("/", rep_pd, msd_version)

  gdrive_dir <- msd_version



# FUNCTIONS ----

  #' @title MMD Map
  #'
  #' @param df      MMD Processed DataFrame
  #' @param basemap Basemap as ggplot plot
  #' @param len     MMD Duration
  #'
  mmd_map <- function(df, spdf, terr,
                      country = "Nigeria",
                      rep_pds = c("FY20Q4", "FY21Q2"),
                      len = "3+",
                      pal = "genoas",
                      add_name = FALSE) {

    print(glue("Country = {country}, Period = {rep_pd}, MMD = {len}"))

    # ART Sat
    spdf_mmd <- spdf %>%
      left_join(df, by = c("uid" = "psnuuid", "operatingunit", "countryname")) %>%
      mutate(
        countryname = case_when(
          countryname == "Democratic Republic of the Congo" ~ "DRC",
          TRUE ~ countryname
        )
      ) %>%
      filter(operatingunit == country,
             period %in% rep_pds,
             mmd_len == len,
             !is.na(mmd_share))

    if (is.null(spdf_mmd) | nrow(spdf_mmd) == 0) {
      message(glue("No valid data for {country}"))
      return(NULL)
    }

    max <- spdf_mmd %>%
      pull(mmd_share) %>%
      max()

    max <- ifelse(max < 1, 1, max)

    # Extract admin 0 and 1 for basemap
    admin0 <- spdf %>%
      filter(operatingunit == country, label == "country")

    admin1 <- spdf %>%
      filter(operatingunit == country, label == "snu1")

    # Produce basemap
    basemap <- terrain_map(countries = admin0,
                           adm0 = admin0,
                           adm1 = admin1,
                           mask = TRUE,
                           terr = terr)

    # Map
    map <- basemap +
      geom_sf(data = spdf_mmd,
              aes(fill = mmd_share),
              size = .3,
              color = grey10k,
              alpha = 0.5) +
      geom_sf(data = admin0,
              color = grey10k,
              fill = NA,
              size = 1.5) +
      geom_sf(data = admin0,
              color = grey80k,
              fill = NA,
              size = .3)

    # PSNU Labels
    if (add_name) {
      map <- map +
        geom_sf_text(data = spdf_mmd %>%
                       mutate(lbl_color = if_else(mmd_share > .3, grey20k, grey80k)),
                     aes(label = paste0(psnu, "\n", percent(mmd_share, 1)),
                         color = lbl_color),
                     size = 2)
    }
    else {
      map <- map +
        geom_sf_text(data = spdf_mmd %>%
                       mutate(lbl_color = if_else(mmd_share > .3, grey20k, grey80k)),
                     aes(label = percent(mmd_share, 1), color = lbl_color),
                     size = 2.5)
    }

    # Map style
    map <- map +
      scale_fill_si(
        palette = pal,
        discrete = FALSE,
        alpha = 0.7,
        na.value = NA,
        breaks = seq(0, max, .25),
        limits = c(0, 1),
        labels = percent
      ) +
      scale_color_identity() +
      facet_wrap(~period, nrow = 1) +
      labs(
        #title = glue("MMD{len} SCALING UP IN AFRICA",
        #subtitle = "% Treatment by MMD Duration",
        caption = glue("Share of MMD{len} = TX_CURR[{len}] / TX_CURR",
                         "\nSource: {msd_caption} MSD, Produced by OHA/SIEI/SI/Core Analytics on ",
                         {format(Sys.Date(), "%Y-%m-%d")})) +
      si_style_map() +
      theme(
        # plot.title = element_text(family = "Source Sans Pro", color = usaid_red),
        # plot.subtitle = element_text(family = "Source Sans Pro", color = usaid_red),
        # plot.caption = element_text(family = "Source Sans Pro"),
        axis.title = element_blank(),
        legend.position = "bottom"
      )
  }


  #' @title MMD VIZ
  #'
  mmd_viz <- function(cntry, mmd_len) {

    # prep len for output filename
    len <- case_when(
      str_detect(mmd_len, "[+]$") ~ str_replace(mmd_len, "[+]", "plus"),
      str_detect(mmd_len, "^[<]") ~ str_replace(mmd_len, "[<]", "lt"),
      TRUE ~ mmd_len
    )

    # create map: pass global vars thru mmd_viz function
    map <- mmd_map(df = df_mmd_share,
                   spdf = spdf_pepfar,
                   terr = terr,
                   country = cntry,
                   rep_pds = rep_pds,
                   len = mmd_len,
                   pal = "genoas")

    # Export map
    if (!is.null(map)) {

      map <- map +
        plot_annotation(
          title = glue("{str_to_upper(cntry)} - {rep_pd} MMD DISTRIBUTION"),
          subtitle = glue("% MMD{mmd_len} from {rep_ref_pd} to {rep_pd}"),
          theme = theme(plot.title = element_text(hjust = 0),
                        plot.subtitle = element_text(hjust = 0))
        )

      cleaned_country <- cntry %>%
        str_remove("'") %>%
        str_to_upper()

      si_save(
        filename = file.path(
          dir_graphics,
          glue("{rep_pd}_{str_to_upper(cleaned_country)}_MMD{len} Distribution_{format(Sys.Date(), '%Y%m%d')}.png")),
        plot = map,
        width = 9.54,
        height = 5,
        scale = 1.4)
    }

    return(mmd_len)
  }

# DATA ----

  # MSD
  df_psnu <- file_psnu_im %>% read_msd()

  # SPATIAL DATA

  ## Raster
  terr <- gisr::get_raster(terr_path = dir_terr)

  ## PEPFAR Boundaries
  spdf_pepfar <- file_shp %>% sf::read_sf()

  df_attrs <- gisr::get_ouuids() %>%
    filter(!str_detect(operatingunit, " Region$")) %>%
    pull(operatingunit) %>%
    map_dfr(.x, .f = ~get_attributes(country = .x))

  spdf_pepfar <- spdf_pepfar %>%
    left_join(df_attrs, by = c("uid" = "id"))


# DATA MMD Distribution ----

  # Maps
  df_mmd <- df_psnu %>%
    filter(
      fiscal_year %in% rep_fys, # Needed for ref year/qtr
      fundingagency == rep_agency,
      indicator %in% c("TX_CURR"),
      standardizeddisaggregate %in% c("Age/Sex/ARVDispense/HIVStatus",
                                      "Total Numerator")
    ) %>%
    reshape_msd(clean = TRUE) %>%
    filter(period_type == "results") %>%
    mutate(
      otherdisaggregate = if_else(
        is.na(otherdisaggregate),
        NA_character_,
        str_remove(otherdisaggregate, "ARV Dispensing Quantity - ")
      ),
      otherdisaggregate = case_when(
        otherdisaggregate == "Less than 3 months" ~ "<3",
        otherdisaggregate == "3 to 5 months" ~ "3-5",
        otherdisaggregate == "6 or more months" ~ "6+",
        is.na(otherdisaggregate) ~ "tn",
        TRUE ~ otherdisaggregate
      )) %>%
    group_by(period, operatingunit, operatingunituid, countryname,
             psnu, psnuuid, otherdisaggregate) %>%
    summarise_at(vars(value), sum, na.rm = TRUE) %>%
    ungroup() %>%
    filter(period %in% c(rep_ref_pd, rep_pd))

  # Track MMD 3+ for the last 2 Qtrs
  df_mmd_geq3 <- df_mmd %>%
    filter(otherdisaggregate %in% c("3-5", "6+")) %>%
    mutate(
      otherdisaggregate = case_when(
        otherdisaggregate == "3-5" ~ "3+",
        otherdisaggregate == "6+" ~ "3+",
        TRUE ~ otherdisaggregate
      )
    ) %>%
    group_by(period, operatingunit, operatingunituid, countryname,
             psnu, psnuuid, otherdisaggregate) %>%
    summarise_at(vars(value), sum, na.rm = TRUE) %>%
    ungroup()

  # Track MMD Not Reported
  df_mmd_notr <- df_mmd %>%
    mutate(otherdisaggregate = "nr") %>%
    group_by(period, operatingunit, operatingunituid, countryname,
             psnu, psnuuid, otherdisaggregate) %>%
    summarise(value = 0) %>%
    ungroup()

  # Track MMD all for the last 2 Qtrs
  df_mmd_share <- df_mmd %>%
    bind_rows(df_mmd_geq3) %>%
    bind_rows(df_mmd_notr) %>%
    group_by(period, operatingunit, operatingunituid,
             countryname, psnu, psnuuid) %>%
    mutate(
      value = case_when(
        otherdisaggregate == "nr" ~
          (value[otherdisaggregate == 'tn'] -
             sum(value[otherdisaggregate %in% c('<3', '3-5', '6+')])),
        TRUE ~ value
      ),
      value = if_else(value < 0, 0, value),
      mmd_total = value[otherdisaggregate == 'tn'],
      mmd_share = value / value[otherdisaggregate == 'tn']
    ) %>%
    ungroup() %>%
    rename(mmd_len = otherdisaggregate)



# VIZ ----

  # Batch this ----
  mmds <- df_mmd_share %>%
    filter(str_detect(operatingunit, " Region$", negate = TRUE),
           !mmd_len %in% c("tn", "nr"),
           !is.na(mmd_share)) %>%
    distinct(mmd_len) %>%
    pull()

  cntries <- df_mmd_share %>%
    filter(str_detect(operatingunit, " Region$", negate = TRUE),
           !mmd_len %in% c("tn", "nr"),
           !is.na(mmd_share)) %>%
    distinct(operatingunit) %>%
    pull()

  # Create OU x MMD Lenght Table
  df_cntries <- tibble(ou = rep(cntries, length(mmds))) %>%
    group_by(ou) %>%
    mutate(mmd = mmds) %>%
    ungroup()

  # Test purrr::pmap

  # df_cntries %>% pmap(paste)
  #
  # df_cntries %>%
  #   filter(ou == "Zambia") %>%
  #   pmap(~mmd_viz(.x, .y))
  #
  # df_cntries %>%
  #   filter(ou == "Zambia") %>%
  #   mutate(idx = row_number(),
  #          tx_length = "6+") %>%
  #   pmap(~mmd_viz(..1, ..4))

  df_cntries %>%
    pmap(~mmd_viz(.x, .y))

  # Map of MMD
  # TODO:
  # Create a cross3 of OU/MMD Len and use pmap() or pwalk
  # cntries %>%
  #   map2("6+", function(cntry, mmd_len) {
  #   #map2("3+", function(cntry, mmd_len) {
  #   #map2("3-5", function(cntry, mmd_len) {
  #   #map2("<3", function(cntry, mmd_len) {
  #
  #     len <- case_when(
  #       str_detect(mmd_len, "[+]$") ~ str_replace(mmd_len, "[+]", "plus"),
  #       str_detect(mmd_len, "^[<]") ~ str_replace(mmd_len, "[<]", "lt"),
  #       TRUE ~ mmd_len
  #     )
  #
  #     map <- mmd_map(df = df_mmd_share,
  #                    spdf = spdf_pepfar,
  #                    terr = terr,
  #                    country = cntry,
  #                    rep_pds = rep_pds,
  #                    len = mmd_len,
  #                    pal = "genoas")
  #
  #     if (!is.null(map)) {
  #
  #       map <- map +
  #         plot_annotation(
  #           title = glue("{str_to_upper(cntry)} - {rep_pd} MMD DISTRIBUTION"),
  #           subtitle = glue("% MMD{mmd_len} from {rep_ref_pd} to {rep_pd}"),
  #           theme = theme(plot.title = element_text(hjust = 0),
  #                         plot.subtitle = element_text(hjust = 0))
  #         )
  #
  #       cleaned_country <- cntry %>%
  #         str_remove("'") %>%
  #         str_to_upper()
  #
  #       si_save(
  #         filename = file.path(
  #           dir_graphics,
  #           glue("{rep_pd}_{str_to_upper(cleaned_country)}_MMD{len} Distribution_{format(Sys.Date(), '%Y%m%d')}.png")),
  #         plot = map,
  #         width = 9.54,
  #         height = 5,
  #         scale = 1.4)
  #     }
  #
  #     return(mmd_len)
  #   })


# UPLOAD TO GDRIVE ----

  gdrive_dir <- msd_caption %>%
    gdrive_folder(name = .,
                  path = gdrive_tx_mmd,
                  add = TRUE)

  # dir_graphics %>%
  #   list.files(pattern = paste0("^", rep_pd, "_.*_MMD.* Distribution_\\d{8}.png$"),
  #              full.names = TRUE) %>%
  #   export_drivefile(filename = .,
  #                    to_drive = gdrive_tx_mmd,
  #                    to_folder = msd_caption,
  #                    name = basename(.),
  #                    type = "png")

  dir_graphics %>%
    list.files(pattern = paste0("^", rep_pd, "_.*_MMD.* Distribution_\\d{8}.png$"),
               full.names = TRUE) %>%
    map(~drive_upload(.x,
                      path = as_id(gdrive_dir),
                      name = basename(.x),
                      type = "png",
                      overwrite = TRUE))
