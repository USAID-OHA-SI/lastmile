##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: OHA/PPIR/GSD Branch request - Rwanda DREAMS Districts map
##  NOTE:    AdHoc request from Gender Advisor
##  LICENCE: MIT
##  DATE:    2021-08-27


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(gophr)
  library(glitr)
  library(glamr)
  library(gisr)
  library(sf)
  library(rnaturalearth)
  library(scales)
  library(ggtext)
  library(patchwork)
  library(glue)
  library(extrafont)


# SETUP ----

  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"

  dir_geodata <- si_path("path_vector")
  dir_terr <- si_path("path_raster")
  dir_merdata <- si_path("path_msd")

  cntry <- "Rwanda"

# FUNCTION ----

  get_terrain2 <- function (countries = list("Zambia"), mask = FALSE, buffer = 0.1,
                            terr = NULL)
  {
    cntries <- {
      {
        countries
      }
    }
    aoi <- NULL
    if (base::is.null(cntries)) {
      base::cat(crayon::red(base::paste0("\ncountry(ies) is required",
                                         " to extract Terrain RasterLayer\n")))
      return(NULL)
    }
    if ("sf" %in% base::class(cntries)) {
      aoi <- cntries
    }
    else {
      if (base::is.character(cntries) & cntries %in% glamr::pepfar_country_xwalk$sovereignt) {
        aoi <- get_admin0(countries = cntries)
      }
      else {
        stop("Countries list does not seems to match Natural Earth Countries")
      }
    }
    dem_url <- "https://drive.google.com/drive/u/0/folders/1M02ToX9AnkozOHtooxU7s4tCnOZBTvm_"
    terr_ras <- NULL
    terr_path <- NULL

    if (base::is.null(terr)) {
      terr_path = glamr::si_path("path_raster")
      if (base::is.null(terr_path)) {
        stop("Global Raster path is not set. Please use glamr::set_paths() and add a value for path_raster")
      }
      else if (!base::is.null(terr_path) & !base::dir.exists(terr_path)) {
        stop(base::paste0("Path: ", terr_path, " does not exist"))
      }
    }

    if (!base::is.null(terr) & "RasterLayer" %in% base::class(terr)) {
      terr_ras <- terr
    }

    if (base::is.null(terr_ras)) {
      terr_file <- base::list.files(terr_path, pattern = "SR_LR.tif$",
                                    recursive = TRUE, full.names = TRUE)
      if (length(terr_file) == 0)
        base::stop(base::paste0("Could not locate a TIFF file in: ",
                                terr_path, "\nDownload file from: ", dem_url))

      terr_ras <- raster::raster(terr_file) %>%
        raster::crop(raster::extend(raster::extent(aoi), {{buffer}}))
    }

    terr_ras <- terr_ras %>% raster::crop(raster::extend(raster::extent(aoi),
                                                         {
                                                           {
                                                             buffer
                                                           }
                                                         }))
    if (mask == TRUE)
      terr_ras <- terr_ras %>% raster::mask(aoi)

    spdf <- terr_ras %>% as(., "SpatialPixelsDataFrame") %>%
      base::as.data.frame() %>%
      dplyr::rename(value = SR_LR)

    return(spdf)
  }


  terrain_map2 <-
    function (countries, adm0 = NULL, adm1 = NULL, add_neighbors = FALSE,
              add_labels = FALSE, mask = FALSE, terr = NULL)
  {
    cntries <- {
      {
        countries
      }
    }
    if (!is.null(adm0) | !is.null(adm1)) {
      admin0 <- adm0
      admin1 <- adm1
    }
    else {
      admin0 <- get_admin0(cntries)
      admin1 <- get_admin1(cntries)
    }
    nghbrs <- NULL
    if (TRUE == {
      {
        add_neighbors
      }
    } & is.null(cntries)) {
      base::cat(crayon::red(base::paste0("\ncountry(ies) is required when",
                                         " adding neighbors\n")))
      return(NULL)
    }
    if (TRUE == {
      {
        add_neighbors
      }
    } & !is.null(cntries)) {
      nghbrs <- geo_neighbors(countries = cntries, crop = TRUE)
      cntries <- nghbrs
    }

    spdf <- get_terrain(countries = cntries, mask = {
      {
        mask
      }
    }, terr = {
      {
        terr
      }
    })

    if (base::is.null(spdf)) {
      base::cat(crayon::red(base::paste0("\nCould not extract terrain data.",
                                         " Your AOI may be invalid.\n")))
      return(NULL)
    }
    p <- ggplot2::ggplot() +
      ggplot2::geom_tile(data = spdf,
                         ggplot2::aes(x = x, y = y, alpha = value)) +
      ggplot2::scale_alpha(name = "", range = c(0.6, 0), guide = "none")
    if (!is.null(nghbrs))
      p <- p + ggplot2::geom_sf(data = nghbrs, fill = "#d9d9d9",
                                alpha = 0.35, size = 0.25, colour = "#6d6e71")
    if (!is.null(nghbrs) & add_labels == TRUE)
      p <- p + ggplot2::geom_sf_text(data = nghbrs, ggplot2::aes(label = sovereignt),
                                     family = "Source Sans Pro")
    p <- p + ggplot2::geom_sf(data = admin0, colour = "white",
                              fill = "grey93", size = 2, alpha = 0.25) + ggplot2::geom_sf(data = admin0,
                                                                                          colour = "black", fill = "NA")
    if (!is.null(admin1))
      p <- p + ggplot2::geom_sf(data = admin1, fill = "NA",
                                linetype = "dotted")
    if (base::is.character(cntries)) {
      if ("south africa" %in% stringr::str_to_lower(cntries)) {
        p <- p + ggplot2::xlim(15, 35) + ggplot2::ylim(-38,
                                                       -20)
      }
    }
    p <- p + ggplot2::theme_void()
    return(p)
  }

# DATA ----

  # Dreams data
  df_rwa_dreams <- tibble(
      province = c("Eastern", "Kigali", "Kigali", "Kigali", "Southern", "Southern", "Estern"),
      district = c("Rwamagana", "Gasabo", "Kicukiro", "Nyarugenge", "Nyanza", "Ruhango", "Kayonza"),
      partner = c("FXB/Turengere Abana", "AEE/Ubaka Ejo", "Pact/ACHIEVE (YWCA)", "Pact/ACHIEVE (DUHAMIC ADRI)", "FXB/Turengere Abana", NA, NA)
    ) %>%
    mutate(fiscal_year = case_when(
      is.na(partner) ~ "FY22",
      TRUE ~ "FY21"
    ))

  # Geodata

  terr <- get_raster()
  terr <- get_raster(name = "SR_HR.tif")

  names(terr) <- "SR_LR"


  # NE Geodata
  ne_adm0 <- get_admin0(cntry, scale = "large") %>%
    select(sovereignt, admin, name, iso_a3)

  ne_adm1 <- get_admin1(cntry) %>%
    select(name, type) %>%
    mutate(name = case_when(
      str_detect(name, "City") ~ str_remove(name, " City"),
      TRUE ~ name
    ))


  # GADM Geodata
  rwa_code <- ne_adm0$iso_a3

  gadm_adm0 <- get_adm_boundaries(rwa_code, path = dir_geodata) %>%
    rename(iso3 = gid_0, name = name_0)

  gadm_adm1 <- get_adm_boundaries(rwa_code, adm_level = 1, path = dir_geodata) %>%
    select(iso3 = gid_0, countryname = name_0, name = varname_1,
           localname = name_1, type = type_1, id = cc_1, code = hasc_1) %>%
    separate(name, into = c("name", "name_fr"), sep = "\\|") %>%
    mutate(
      name = case_when(
        str_detect(name, "Province") ~ str_remove(name, " Province"),
        str_detect(name, "City") ~ str_remove(name, " City"),
        TRUE ~ name
      ),
      name = str_to_upper(str_remove(name, "ern")),
      y_nudge = case_when(
        name == 'SOUTH' ~ -.5,
        name == "EAST" ~ +.5,
        TRUE ~ 0
      ))

  gadm_adm2 <- get_adm_boundaries(rwa_code, adm_level = 2, path = dir_geodata) %>%
    select(iso3 = gid_0, countryname = name_0, localprovince = name_1,
           name = name_2, type = type_2, id = cc_2, code = hasc_2) %>%
    mutate(prov_id = str_sub(id, 1, 1),
           prov_code = str_sub(code, 1, 5))

  spdf_dreams <- gadm_adm2 %>%
    left_join(df_rwa_dreams, by = c("name" = "district")) %>%
    filter(!is.na(fiscal_year))

  spdf_dreams %>%
    attributes() %>%
    prinf()

# VIZ ----

  # Assign colors
  spdf_dreams <- spdf_dreams %>%
    mutate(
      fill_color = case_when(
        fiscal_year == "FY21" ~ usaid_blue,
        TRUE ~ usaid_medgrey
      ),
      text_color = case_when(
        fiscal_year == "FY21" ~ grey10k,
        TRUE ~ color_plot_text
      ),
      partner_color = case_when(
        partner == "AEE/Ubaka Ejo" ~ denim,
        partner == "FXB/Turengere Abana" ~ moody_blue,
        partner == "Pact/ACHIEVE (DUHAMIC ADRI)" ~ burnt_sienna,
        partner == "Pact/ACHIEVE (YWCA)" ~ genoa,
        TRUE ~ usaid_medgrey
      ))

  # Basemap
  basemap <- terrain_map2(countries = cntry,
                         adm0 = gadm_adm0,
                         adm1 = gadm_adm1,
                         terr = terr,
                         mask = TRUE)

  # Implementation Areas
  program_map <- ggplot() +
  #basemap +
    geom_sf(data = gadm_adm0,
            fill = grey10k,
            size = 0,
            alpha = .3,
            color = grey10k) +
    geom_sf(data = gadm_adm2,
            fill = NA,
            size = .3,
            color = grey30k) +
    geom_sf(data = spdf_dreams,
            aes(fill = fill_color),
            size = .3,
            color = grey30k) +
    geom_sf(data = gadm_adm1,
            fill = NA,
            size = .8,
            color = grey60k) +
    geom_sf(data = gadm_adm0,
            fill = NA,
            size = 2,
            color = grey20k) +
    geom_sf(data = gadm_adm0,
            fill = NA,
            size = 1,
            color = grey60k) +
    geom_sf_text(data = gadm_adm1 %>% filter(name %in% c('NORTH', 'WEST')),
                 aes(label = name),
                 color = grey40k,
                 size = 8) +
    geom_sf_text(data = gadm_adm1 %>% filter(name == 'KIGALI'),
                 aes(label = name),
                 color = grey40k,
                 size = 8,
                 nudge_y = .3) +
    geom_sf_text(data = gadm_adm1 %>% filter(name == 'EAST'),
                 aes(label = name),
                 color = grey40k,
                 size = 8,
                 nudge_y = .3) +
    geom_sf_text(data = gadm_adm1 %>% filter(name == 'SOUTH'),
                 aes(label = name),
                 color = grey40k,
                 size = 8,
                 nudge_y = -.3) +
    geom_sf_text(data = spdf_dreams,
                 aes(label = name, color = text_color),
                 size = 4) +
    scale_fill_identity() +
    scale_color_identity() +
    labs(title = "RWANDA - USAID IMPLEMENTATION OF DREAMS PROGRAM",
         subtitle = "Program is being implemented in <span style='color:#002a6c'>**5**</span> Districts for <span style='color:#002a6c'>**FY21**</span> and <span style='color:#8C8985'>**2**</span> more will be added in <span style='color:#8C8985'>**FY22**</span>",
         caption = glue("Source: PEPFAR/Rwanda - COP21 Data<br/>OHA/SIEI/SI/Core Analytics, {format(Sys.Date(), '%Y-%m-%d')}")) +
    si_style_map()

  ggsave(paste0(dir_graphics, "/FY21Q3 - RWANDA USAID Implimentation of DREAMS Program - Districts - ",
                format(Sys.Date(), "%Y-%m-%d"),
                ".png"),
         plot = program_map,
         scale = 1.5,
         width = 10, height = 5.625, dpi = 320)

  # Implementation Areas
  partners_map <-
    ggplot() +
    #basemap +
    geom_sf(data = gadm_adm0,
            fill = grey10k,
            size = 0,
            alpha = .3,
            color = grey10k) +
    geom_sf(data = gadm_adm2,
            fill = NA,
            size = .3,
            color = grey30k) +
    geom_sf(data = spdf_dreams %>% filter(!is.na(partner)),
            aes(fill = partner),
            size = .3,
            color = grey30k) +
    geom_sf(data = gadm_adm1,
            fill = NA,
            size = .8,
            color = grey60k) +
    geom_sf(data = gadm_adm0,
            fill = NA,
            size = 2,
            color = grey20k) +
    geom_sf(data = gadm_adm0,
            fill = NA,
            size = 1,
            color = grey60k) +
    geom_sf_text(data = gadm_adm1 %>% filter(name %in% c('NORTH', 'WEST')),
                 aes(label = name),
                 color = grey40k,
                 size = 8) +
    geom_sf_text(data = gadm_adm1 %>% filter(name == 'KIGALI'),
                 aes(label = name),
                 color = grey40k,
                 size = 5) +
    geom_sf_text(data = gadm_adm1 %>% filter(name == 'EAST'),
                 aes(label = name),
                 color = grey40k,
                 size = 8,
                 nudge_y = .3) +
    geom_sf_text(data = gadm_adm1 %>% filter(name == 'SOUTH'),
                 aes(label = name),
                 color = grey40k,
                 size = 8,
                 nudge_y = -.3) +
    geom_sf_text(data = spdf_dreams,
                 aes(label = name, color = text_color),
                 size = 4) +
    scale_fill_si(palette = "siei", discrete = TRUE) +
    #scale_fill_identity() +
    scale_color_identity() +
    labs(title = "RWANDA - USAID IMPLEMENTATION OF DREAMS PROGRAM",
         subtitle = "**AEE** and **Pact** are all in Kigali and **FXB** is in Rwamagana (East) and Nyanza (South)",
         caption = glue("Source: PEPFAR/Rwanda - COP21 Data<br/>OHA/SIEI/SI/Core Analytics, {format(Sys.Date(), '%Y-%m-%d')}")) +
    si_style_map() +
    theme(legend.position = "top", legend.justification = "left")

    ggsave(paste0(dir_graphics, "/FY21Q3 - RWANDA USAID Implimentation of DREAMS Program - Partners - ",
                  format(Sys.Date(), "%Y-%m-%d"),
                  ".png"),
           plot = partners_map,
           scale = 1.5,
           width = 10, height = 5.625, dpi = 320)



    # partner_color = case_when(
    #   partner == "AEE/Ubaka Ejo" ~ denim,
    #   partner == "FXB/Turengere Abana" ~ moody_blue,
    #   partner == "Pact/ACHIEVE (DUHAMIC ADRI)" ~ burnt_sienna,
    #   partner == "Pact/ACHIEVE (YWCA)" ~ genoa,
    #   TRUE ~ usaid_medgrey
    # ))