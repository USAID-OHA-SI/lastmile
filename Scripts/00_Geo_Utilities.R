##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa, G.Sarfaty & T.Essam | USAID
##  EDITED:  T. Essam (2020-10-01)
##  PURPOSE: Geo-utilities
##  LICENCE: MIT
##  DATE:    2020-10-21

#' @title Build PEPFAR Spatial Datasets
#'
#' @param terr_path path to terrain raster file
#' @return RasterLayer
#'
build_perfar_spdf <-
  function() {

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
#' @return ggplot plot of base map
#'
get_basemap <-
  function(spdf,
           cntry = NULL,
           terr_raster = NULL) {

    df_geo <- {{spdf}}
    country <- {{cntry}}
    dta_raster <- {{terr_raster}}

    # Filter by OU / Country
    if (!is.null(country)) {
      df_geo <- df_geo %>%
        filter(countryname == country)
    }

    df_geo <- df_geo %>%
      sf::st_as_sf() %>%
      sf::st_transform(., crs = sf::st_crs(4326)) %>%
      sf::st_zm()

    # Get country boundaries
    df_geo0 <- df_geo %>%
      filter(type == "OU")

    # Get snu1 boundaries
    df_geo1 <- df_geo %>%
      filter(type == "SNU1")

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
      ) +
      geom_sf(
        data = df_geo1,
        colour = grey50k,
        fill = "NA",
        linetype = "dotted"
      ) +
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

    # print and return
    print(m)

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
