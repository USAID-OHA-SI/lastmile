##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: MMD Distribution
##  LICENCE: MIT
##  DATE:    2021-03-02


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

  #' @title MMD Map
  #'
  #' @param df      MMD Processed DataFrame
  #' @param basemap Basemap as ggplot plot
  #' @param len     MMD Duration
  #'
  mmd_map <- function(df, basemap,
                      len = "3+",
                      pal = "genoas") {

    print(len)
    print(pal)

    # Map
    df_mmd <- df %>%
      dplyr::filter(mmd_len == {{len}})

    print(df_mmd %>% nrow())

    basemap +
      geom_sf(data = df_mmd,
              aes(fill = mmd_share),
              size = .3,
              color = grey10k) +
      geom_sf(data = afr_countries,
              color = grey10k,
              fill = NA,
              size = .3) +
      geom_sf_text(data = df_mmd %>%
                     mutate(lbl_color = if_else(mmd_share > .3, grey20k, grey80k)),
                   #aes(label = paste0(countryname, "\n", percent(mmd_share, 1))),
                   aes(label = percent(mmd_share, 1), color = lbl_color),
                   size = 3) +
      scale_fill_si(
        palette = pal, #"genoas", "moody_blues", "scooters",
        discrete = FALSE,
        alpha = 0.85,
        na.value = NA,
        breaks = seq(0, 1, .25),
        limits = c(0, 1),
        labels = percent
      ) +
      scale_color_identity() +
      facet_wrap(~period, nrow = 1) +
      labs(
        #title = "DRAFT DO NOT USE \nMMD3+ SCALING UP IN AFRICA",
        #subtitle = "% Treatment by MMD Duration",
        caption = paste0("MMD3+ = TX_CURR[", len, "] / TX_CURR",
                         "\nNo PEPFAR and/or TX Programs in grayed out countries",
                         "\nSouth Africa's data has been removed for completness reasons.",
                         "\nSource: FY21Q1i PSNU x IM MSDs, Produced on ",
                         format(Sys.Date(), "%Y-%m-%d"))) +
      si_style_map() +
      theme(plot.title = element_text(color = usaid_red),
            plot.subtitle = element_text(color = usaid_red),
            plot.caption = element_text(family = "source sans pro"))
  }

# DATA ----

  # MSD
  df_psnu <- file_psnu_im %>% read_msd()

  # SPATIAL DATA

  ## PEPFAR Boundaries
  terr <- gisr::get_raster(terr_path = dir_terr)

  spdf_pepfar <- file_shp %>% sf::read_sf()

  # working with country boundaries only
  df_ous <- glamr::get_outable(datim_user(), datim_pwd())

  spdf_pepfar <- spdf_pepfar %>%
    left_join(df_ous, by = c("uid" = "countryname_uid"))


  ## NE Admins

  # Get country boundaries
  africa <- ne_countries(continent = "africa", returnclass = "sf") %>%
    st_transform(crs = st_crs(4326)) %>%
    dplyr::select(iso3 = sov_a3, name, admin, sovereignt) %>%
    mutate(
      iso3 = case_when(
        iso3 == "SDS" ~ "SSD",
        TRUE ~ iso3
      )
    )

  # Append PEPFAR OUs
  afr_countries <- africa %>%
    left_join(df_ous, by = c("iso3" = "countryname_iso")) %>%
    filter(!is.na(country_lvl))


# DATA MMD Distribution ----

  df_mmd <- df_psnu %>%
    filter(
      fiscal_year %in% c(2020, 2021), # Needed for Q1 vl
      fundingagency == "USAID",
      indicator %in% c("TX_CURR"),
      standardizeddisaggregate %in% c("Age/Sex/ARVDispense/HIVStatus",
                                      "Total Numerator")
    ) %>%
    reshape_msd(clean = TRUE) %>%
    filter(period_type == "results") %>%
    rename(countryname = countrynamename) %>%
    mutate(
      otherdisaggregate = str_remove(
        otherdisaggregate, "ARV Dispensing Quantity - "),
      otherdisaggregate = case_when(
        otherdisaggregate == "Less than 3 months" ~ "<3",
        otherdisaggregate == "3 to 5 months" ~ "3-5",
        otherdisaggregate == "6 or more months" ~ "6+",
        is.na(otherdisaggregate) ~ "tn",
        TRUE ~ otherdisaggregate
      )) %>%
    group_by(period, operatingunit, operatingunituid, countryname, otherdisaggregate) %>%
    summarise_at(vars(value), sum, na.rm = TRUE) %>%
    ungroup() %>%
    filter(period %in% c("FY20Q4", "FY21Q1"))

  # Track MMD 3+ for the last 2 Qtrs
  df_mmd_geq3 <- df_mmd %>%
    mutate(
      otherdisaggregate = case_when(
        otherdisaggregate == "3-5" ~ "3+",
        otherdisaggregate == "6+" ~ "3+",
        TRUE ~ otherdisaggregate
      )
    ) %>%
    group_by(period, operatingunit, operatingunituid, countryname, otherdisaggregate) %>%
    summarise_at(vars(value), sum, na.rm = TRUE) %>%
    ungroup()

  # Track MMD Not Reported
  df_mmd_notr <- df_mmd %>%
    mutate(otherdisaggregate = "nr") %>%
    group_by(period, operatingunit, operatingunituid, countryname, otherdisaggregate) %>%
    summarise(value = ) %>%
    ungroup()


  # Track MMD all for the last 2 Qtrs
  df_mmd_share <- df_mmd %>%
    filter(otherdisaggregate %in% c("3-5", "6+")) %>%
    bind_rows(df_mmd_geq3) %>%
    bind_rows(df_mmd_notr) %>% #view()
    group_by(period, operatingunit, operatingunituid, countryname) %>%
    mutate(
      value = case_when(
        otherdisaggregate == "nr" ~
          (value[otherdisaggregate == 'tn'] -
             sum(value[otherdisaggregate %in% c('<3', '3-5', '6+')])),
        TRUE ~ value
      ),
      value = if_else(value < 0, 0, value),
      mmd_share = value / value[otherdisaggregate == 'tn']
    ) %>%
    ungroup() %>%
    rename(mmd_len = otherdisaggregate)

  df_mmd_share %>% glimpse()
  df_mmd_share %>% view()

  spdf_mmd <- afr_countries %>%
    left_join(df_mmd_share, by = "countryname") %>%
    filter(countryname != "South Africa", !is.na(mmd_len)) %>%
    mutate(
      countryname = case_when(
        countryname == "Democratic Republic of the Congo" ~ "DRC",
        TRUE ~ countryname
      )
    )



# VIZ ----

  # Bars
  df_mmd_share %>%
    filter(period == "FY21Q1") %>%
    mutate(
      countryname = case_when(
        countryname == "Democratic Republic of the Congo" ~ "DRC",
        TRUE ~ countryname
      ),
      countryname = reorder_within(countryname, mmd_share, mmd_len)
    ) %>%
    ggplot(aes(reorder(countryname, mmd_share), mmd_share)) +
    geom_col(aes(y = 1), fill = grey10k) +
    geom_col(fill = usaid_blue) +
    scale_x_reordered() +
    scale_y_continuous(labels = percent) +
    coord_flip() +
    facet_wrap(~mmd_len, scales = "free_y") +
    si_style_xgrid()

  # countries
  africa %>% gview()

  # Basemap
  basemap <- terrain_map(countries = africa,
                         adm0 = africa,
                         adm1 = africa,
                         terr = terr,
                         mask = TRUE)

  # Batch this ----
  spdf_mmd %>%
    st_drop_geometry() %>%
    filter(!mmd_len %in% c("tn", "nr")) %>%
    distinct(mmd_len) %>%
    pull()

  c("3+", "6+") %>%
    map(function(mmd_len) {

      cols <- case_when(
        mmd_len == "<3" ~ "burnt_siennas",
        mmd_len == "6+" ~ "genoas",
        mmd_len == "3+" ~ "moody_blues",
        mmd_len == "nr" ~ "trolley_greys",
        TRUE ~ "old_roses"
      )

      map <- mmd_map(df = spdf_mmd,
                     basemap = basemap,
                     len = mmd_len,
                     pal = cols)

      si_save(
        filename = file.path(
          dir_graphics,
          paste0("FY21Q1 - AFRICAN Countries - MMD",
                 mmd_len,
                 " Duration - ",
                 format(Sys.Date(), "%Y%m%d"),
                 ".png")),
        plot = map,
        width = 9.54,
        height = 5,
        scale = 1.4)

      return(mmd_len)
    })
