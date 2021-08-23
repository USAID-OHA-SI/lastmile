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


#' Get Orgunit Boundaries Attributes
#'
#' @param country    OU/country
#' @param folderpath Local directory of files
#' @return df
#'
get_attributes <- function(country,
                           folderpath = NULL) {

  file_pattern <- paste0("^orghierarchy - ",
                         str_to_lower(country),
                         " - \\d{8}.csv$")

  # Get attrs from local drive
  if (!is.null(folderpath)) {

    message(paste0("Searching for: ", file_pattern))

    file_attrs <- glamr::return_latest(
      folderpath,
      pattern = file_pattern)

    message(glue("Reading from: {basename(file_attrs)}"))

    df_attrs <- file_attrs %>%
      readr::read_csv(file = ., col_types = c(.default = "c"))
      #vroom::vroom(file = ., col_types = c(.default = "c"))

    return(df_attrs)
  }

  message(country)

  # Get attrs from datim
  df_attrs <- gisr::extract_locations(country = country, add_geom = FALSE)

  labels <- df_attrs %>%
    distinct(label) %>%
    pull()

  # Use psnu as snu1
  if (!"snu1" %in% labels) {
    df_snu <- df_attrs %>%
      dplyr::filter(label == "prioritization") %>%
      dplyr::mutate(label = "snu1")

    df_attrs <- df_attrs %>%
      dplyr::bind_rows(df_snu)
  }

  # Filter out facilities and communities
  df_attrs <- df_attrs %>%
    dplyr::select(-path) %>%
    dplyr::filter(label != "facility") %>%
    dplyr::select(id, level, label, name,
                  operatingunit_iso, operatingunit,
                  countryname_iso, countryname)

  return(df_attrs)
}

#' @title Extract Orgunit Boundaries Attributes
#'
#' @param country ou/country
#' @param
#' @return df
#'
extract_attributes <-
  function(country,
           folderpath = NULL) {

  # local dir & filename
  dir <- folderpath

  if (!is.null(dir) && !dir.exists(dir)) {
    message(glue("Directory does not exist: {folderpath}"))
    stop("Invalid folderpath")
  }

  # Default folder
  path_vector <- glamr::si_path("path_vector")

  if (is.null(dir) && !is.null(path_vector) && dir.exists(path_vector)) {
    message(glue("Data will be extracted to: {path_vector}"))
    dir <- dir_geodata
  }

  # No folder identified
  if (is.null(dir)) {
    message("Destination folder is not set")
    message("Consider glamr::set_path(folderpath_vector = '../<folder>')")

    stop("folderpath is not set")
  }

  filename <- file.path(dir,
                        glue("orghierarchy - ",
                             "{stringr::str_to_lower(country)} - ",
                             "{format(Sys.Date(), '%Y%m%d')}",
                             ".csv"))

  #get and save country attributes
  attrs <- tryCatch(
    get_attributes(country),
    error = function(err) {
      print(err)
      return(NULL)
    },
    warning = function(warn) {
      print(warn)
      message("See warning(s) above")
    }
  )

  #Write to csv file
  if (!is.null(attrs) & nrow(attrs) > 0) {
    message(glue("Writing data to: {basename(filename)}"))
    readr::write_csv(x = attrs, file = filename)
  }
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
