##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa, G.Sarfaty & T.Essam | USAID
##  EDITED:  T. Essam (2020-10-01)
##  PURPOSE: Geo-utilities
##  LICENCE: MIT
##  DATE:    2020-10-21

#' @title Build PEPFAR Spatial Datasets
#'
#' @param dir_geo Directory of shapefile
#' @param name    Name of the shapefile
#' @param df_psnu MSD PSNU x IM DataFrame
#' @return Spatial DataFrame
#'
build_spdf <-
  function(dir_geo = "../../GEODATA/PEPFAR",
           name = "VcPepfarPolygons.shp",
           df_psnu = NULL) {

    # Variables
    dir_geo <- {{dir_geo}}
    file_name <- {{name}}
    df <- {{df_psnu}}

    # Geo - Idenfity shapefile path
    file_shp <- list.files(
        path = dir_geo,
        pattern = file_name,
        recursive = T,
        full.names = T
      )

    cat("\nSpatial dataset: ",
        crayon::blue(file_shp),
        "\n")

    # Make sure file exists
    if ( !length(file_shp) ) {
      cat("\n",
          Wavelength::paint_red("Shapefile does not seem to exist."),
          "\n")

      return(NULL)
    }

    # Geo - Read shapefile
    spdf <- file_shp %>% read_sf()

    # Check if orgs needs to be identified
    if (is.null(df)) {
      cat("\n",
          Wavelength::paint_blue("\nResult as spatial data with UID only."),
          "\n")

      return(spdf)
    }

    # OUs
    ous <- df %>%
      distinct(operatingunit, operatingunituid) %>%
      arrange(operatingunit)

    spdf <- spdf %>%
      left_join(ous, by = c("uid" = "operatingunituid")) %>%
      dplyr::mutate(
        type = case_when(
          !is.na(operatingunit) ~ 'OU',
          TRUE ~ NA_character_
        )
      )

    # SNU1
    snus <- df %>%
      dplyr::select(operatingunit, countryname, snu1, snu1uid) %>%
      distinct(operatingunit, countryname, snu1, snu1uid) %>%
      filter(!is.na(snu1)) %>%
      arrange(operatingunit, countryname, snu1)

    spdf <- spdf %>%
      left_join(snus, by = c("uid" = "snu1uid")) %>%
      rename(operatingunit = operatingunit.x) %>%
      mutate(
        type = case_when(
          is.na(type) & !is.na(operatingunit.y) ~ "SNU1",
          TRUE ~ type
        )
      ) %>%
      dplyr::select(-operatingunit.y)

    # PSNU
    psnus <- df %>%
      dplyr::select(operatingunit,
                    operatingunituid,
                    countryname,
                    snu1,
                    snu1uid,
                    psnu,
                    psnuuid) %>%
      distinct(operatingunit, countryname, snu1, snu1uid, psnu, psnuuid) %>%
      arrange(operatingunit, countryname, snu1, psnu)

    spdf <- spdf %>%
      left_join(psnus, by = c("uid" = "psnuuid")) %>%
      rename(
        countryname = countryname.x,
        operatingunit = operatingunit.x,
        snu1 = snu1.x
      ) %>%
      mutate(
        type = case_when(
          is.na(type) & !is.na(operatingunit.y) ~ "PSNU",
          TRUE ~ type
        ),
        countryname = case_when(
          is.na(countryname) & type == "PSNU" ~ countryname.y,
          TRUE ~ countryname
        ),
        operatingunit = case_when(
          is.na(operatingunit) & type == "PSNU" ~ operatingunit.y,
          TRUE ~ operatingunit
        ),
        snu1 = case_when(
          is.na(snu1) & type == "PSNU" ~ snu1.y,
          TRUE ~ snu1
        ),
        type = case_when(
          !is.na(snu1uid) & uid == snu1uid & type == "SNU1" ~ "PSNU",
          TRUE ~ type
        )
      ) %>%
      dplyr::select(-ends_with(".y"))

    return(spdf)
  }


#' Append attributes to PEPFAR Geodata
#'
#' @param spdf
#' @param df_ous
#' @param df_snus
#' @param df_psnus
#' @return spdf with lebels (type) and other details
#'
append_attributes <- function(spdf, df_ous, df_snus, df_psnus){

  # Get params
  spdf_pepfar <- {{spdf}}
  ous <- {{df_ous}}
  snus <- {{df_snus}}
  psnus <- {{df_psnus}}

  ## Geo - identify ous
  spdf_pepfar <- spdf_pepfar %>%
    left_join(ous, by = "uid")

  ## Geo - identify snu1
  spdf_pepfar <- spdf_pepfar %>%
    left_join(snus, by = c("uid" = "snu1uid")) %>%
    rename(countryname = countryname.x) %>%
    mutate(
      type = case_when(
        is.na(type) & !is.na(operatingunit) ~ "SNU1",
        TRUE ~ type
      ),
      countryname = case_when(
        is.na(countryname) & type == "SNU1" ~ countryname.y,
        TRUE ~ countryname
      )
    ) %>%
    dplyr::select(-countryname.y)

  ## Geo - identify psnu
  spdf_pepfar <- spdf_pepfar %>%
    left_join(psnus, by = c("uid" = "psnuuid")) %>%
    rename(
      countryname = countryname.x,
      operatingunit = operatingunit.x,
      snu1 = snu1.x
    ) %>%
    mutate(
      type = case_when(
        is.na(type) & !is.na(operatingunit.y) ~ "PSNU",
        TRUE ~ type
      ),
      countryname = case_when(
        is.na(countryname) & type == "PSNU" ~ countryname.y,
        TRUE ~ countryname
      ),
      operatingunit = case_when(
        is.na(operatingunit) & type == "PSNU" ~ operatingunit.y,
        TRUE ~ operatingunit
      ),
      snu1 = case_when(
        is.na(snu1) & type == "PSNU" ~ snu1.y,
        TRUE ~ snu1
      ),
      type = case_when(
        !is.na(snu1uid) & uid == snu1uid & type == "SNU1" ~ "PSNU",
        TRUE ~ type
      )
    ) %>%
    dplyr::select(-ends_with(".y"))

  ## Return geodata

  return(spdf_pepfar)
}


#' Get OrgUnit Attributes
#'
#' @param country ou/country
#' @return df
#'
get_attributes <- function(country) {

  print(country)

  locs <- gisr::extract_locations(country = country, add_geom = FALSE)

  labels <- locs %>%
    distinct(label) %>%
    pull()

  # Use psnu as snu1
  if (!"snu1" %in% labels) {
    df_psnu <- locs %>%
      dplyr::filter(label == "prioritization") %>%
      dplyr::mutate(label = "snu1")

    locs <- locs %>%
      dplyr::bind_rows(df_psnu)
  }

  # Filter out facilities and communities
  locs <- locs %>%
    dplyr::select(-path) %>%
    dplyr::filter(label != "facility")

  return(locs)
}

#' @title Get Terrain Raster dataset
#'
#' @param terr_path path to terrain raster file
#' @return RasterLayer
#'
get_raster <-
  function(terr_path = "../../GEODATA/RASTER",
           name = "SR_LR.tif") {

  # Params
  dir_terr <- {{terr_path}}
  file_name <- {{name}}

  # Check directory
  if (!dir.exists(dir_terr))
    stop(cat("\nInvalid terrain directory: ",
             paint_red(dir_terr),
             "\n"))

  # Identify file path
  terr_file <- list.files(
    path = dir_terr,
    pattern = paste0(file_name, "$"),
    recursive = TRUE,
    full.names = TRUE
  )

  # Check file
  if (!file.exists(terr_file))
    stop(cat("\nFile does not exist: ",
             terr_file,
             "\n"))

  # Read file content
  ras <- raster::raster(terr_file)

  return(ras)
}


#' @title Get basemap
#'
#' @param spdf PEPFAR ORGs Spatial Data
#' @param cntry OU or Country Name
#' @param terr_raster RasterLayer
#' @param add_admins Should the sub-admins be added? Default is false
#' @return ggplot plot of base map
#'
get_basemap <-
  function(spdf,
           cntry = NULL,
           terr_raster = NULL,
           add_admins = FALSE) {

    # Params
    df_geo <- {{spdf}}
    country <- {{cntry}}
    dta_raster <- {{terr_raster}}

    # Filter by OU / Country
    if (!is.null(country)) {
      df_geo <- df_geo %>%
        filter(operatingunit == country)
    }

    # Transform geodata
    df_geo <- df_geo %>%
      sf::st_as_sf() %>%
      sf::st_transform(., crs = sf::st_crs(4326)) %>%
      sf::st_zm()

    # Get country boundaries
    df_geo0 <- df_geo %>%
      filter(type == "OU")

    # Get snu1 or psnu boundaries
    df_geo1 <- df_geo %>%
      filter(type == "SNU1")

    # if (nrow(df_geo1) == 0)
    #   df_geo1 <- df_geo %>%
    #     filter(type == "PSNU")

    # Get psnu boundaries
    # df_geo2 <- df_geo %>% filter(type == "PSNU")

    # Terrain
    if (is.null(dta_raster))
      stop("Terrain raster data is required.")

    # Crop
    terr <- dta_raster %>%
      raster::crop(x = ., y = raster::extend(raster::extent(df_geo0), .2)) %>%
      raster::mask(x = ., mask = df_geo0)

    # Convert raster data into a spatial data frame
    trdf <- terr %>%
      as("SpatialPixelsDataFrame") %>%
      as.data.frame() %>%
      dplyr::rename(value = SR_LR) %>%
      dplyr::filter(value < 210)

    # Basemap
    m <- ggplot() +
      geom_tile(data = trdf, aes(x, y, alpha = value)) +
      scale_alpha(name = "", range = c(0.6, 0), guide = F) +
      geom_sf(
        data = df_geo0,
        colour = "white",
        fill = grey10k,
        size = 2,
        alpha = .25
      )

    # Add sub-admins boundaries
    if (add_admins & nrow(df_geo1) > 0) {
      m <- m +
        geom_sf(
          data = df_geo1,
          fill = "NA",
          linetype = "dotted",
          size = .5
        )
    }

    # Add country boundaries
    m <- m +
      geom_sf(
        data = df_geo0,
        colour = grey90k,
        fill = "NA",
        size = 1
      ) +
      si_style_map()

    # Zoom to South Africa mainland
    if ("south africa" == tolower(country)) {
      m <- m +
        ggplot2::xlim(15, 35) +
        ggplot2::ylim(-38,-20)
    }

    return(m)
  }


#' @title create basemap
#'
#' @param terr_raster RasterLayer
#' @param admin0 Spatial DataFrame of country boundaries
#' @param admin1 Spatial DataFrame of regions boundaries, optional
#' @param limits Spatial extent as a vector: Eg south africa = c(15, 35, -38, -20)
#' @return basemap as ggplot plot
#'
create_basemap <-
  function(terr_raster, admin0,
           admin1 = NULL,
           limits = NULL) {

    # Params
    dta_raster <- {{terr_raster}}
    df_geo0 <- {{admin0}}
    df_geo1 <- {{admin1}}
    ext <- {{limits}}

    # Transform geodata
    df_geo0 <- df_geo0 %>%
      sf::st_as_sf() %>%
      sf::st_transform(., crs = sf::st_crs(4326)) %>%
      sf::st_zm(drop = TRUE)

    # Terrain
    if (is.null(dta_raster))
      stop("Terrain raster data is required.")

    # Crop
    terr <- dta_raster %>%
      raster::crop(x = ., y = raster::extend(raster::extent(df_geo0), .2)) %>%
      raster::mask(x = ., mask = df_geo0)

    # Convert raster data into a spatial data frame
    trdf <- terr %>%
      as("SpatialPixelsDataFrame") %>%
      as.data.frame() %>%
      dplyr::rename(value = SR_LR) %>%
      dplyr::filter(value < 210)

    # Basemap
    m <- ggplot() +
      geom_tile(data = trdf,
                aes(x, y, alpha = value)) +
      scale_alpha(name = "",
                  range = c(0.6, 0),
                  guide = F) +
      geom_sf(
        data = df_geo0,
        colour = "white",
        fill = grey10k,
        size = 2,
        alpha = .25
      )

    # Add sub-admins boundaries
    if (!is.null(df_geo1) & nrow(df_geo1) > 0) {
      m <- m +
        geom_sf(
          data = df_geo1,
          fill = "NA",
          linetype = "dotted",
          size = .5
        )
    }

    # Add country boundaries
    m <- m +
      geom_sf(
        data = df_geo0,
        colour = grey90k,
        fill = "NA",
        size = 1
      ) +
      si_style_map()

    # Zoom to South Africa mainland
    if (!is.null(ext) & length(ext) == 4) {
      m <- m +
        ggplot2::xlim(ext[1:2]) +
        ggplot2::ylim(ext[3:4])
    }

    return(m)
  }




#' @title Map specific orgunits
#'
#' @param spdf PEPFAR ORGs Spatial Data
#' @param org_uid ORG uid
#' @param title Plot title
#' @return ggplot plot of the map
#'
map_org <-
  function(spdf,
           org_uid = NULL,
           title = NULL) {

  df_geo <- {{spdf}}
  uid <- {{org_uid}}

  if (!is.null(uid)) {
    df_geo <- df_geo %>%
      filter(uid == uid)
  }

  map_title <- ifelse(!is.null({{title}}),
                      paste0("MAP - ", {{title}}),
                      paste0("MAP - ", df_geo$uid[1]))

  m <- ggplot() +
    geom_sf(
      data = df_geo,
      fill = NA,
      color = grey30k,
      lwd = .5
    ) +
    labs(title = map_title) +
    si_style_map()

  print(m)

  return(m)
}
