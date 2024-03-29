##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa & G.Sarfaty | USAID
##  EDITED:  T. Essam (2020-10-01)
##  PURPOSE: Geo-depiction of VL - % not covered
##  LICENCE: MIT
##  DATE:    2020-09-04
##  UPDATED: 2022-01-10

# Libraries ---

  library(tidyverse)
  library(vroom)
  library(sf)
  library(raster)
  library(gisr)
  library(glitr)
  library(glamr)
  library(janitor)
  library(scales)
  library(patchwork)
  library(ggrepel)
  library(here)
  library(gophr)
  library(extrafont)
  library(tidytext)
  library(googledrive)

# GLOBALS ----

  ## Data & Output folders
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"
  dir_geodata <- si_path("path_vector")
  dir_geo <- si_path("path_vector")
  dir_terr <- si_path("path_raster")
  dir_merdata <- si_path("path_msd")

  gdrive_tx_vl <- "1OOBguVY-NTPm7ulF9NF1JHx3tNkVvcpS"
  gdrive_tx_peds <- "1WMxJ1YQzSFKSGyGYVBU-LjsPwsTLVJlZ"

  ## Reporting Filters
  rep_agency = c("USAID", "HHS/CDC")

  ## File path + name
  file_psnu_im <- return_latest(
    folderpath = dir_merdata,
    pattern = "^MER_.*_PSNU_IM_.*_\\d{8}_v\\d{1}_\\d{1}.zip$",
    recursive = TRUE
  )

  rep_pd <- file_psnu_im %>% identify_pd()

  rep_qtr <- rep_pd %>%
    str_sub(-1) %>%
    as.integer()

  rep_fy <- rep_pd %>%
    str_sub(3,4) %>%
    paste0("20", .) %>%
    as.integer()

  # MSD File version
  msd_version <- ifelse(str_detect(file_psnu_im, ".*_\\d{8}_v1_\\d"), "i", "c")

  msd_caption <- paste0(rep_pd, msd_version)

  dir_graphics <- dir_graphics %>%
    paste0("/", rep_pd, msd_version)

  if(!dir.exists(dir_graphics)) {
    dir.create(dir_graphics)
  }

  ## File path + name
  file_shp <- return_latest(
    folderpath = dir_geodata,
    pattern = "VcPepfarPolygons.*.shp",
    recursive = TRUE
  )

# FUNCTIONS ---------------------------------------------------------

## Utility functions
source("./Scripts/00_Utilities.R")
source("./Scripts/00_VL_Utilities.R")
source("./Scripts/00_ML_Utilities.R")


#' @title Generate plot title
#'
#' @param country country name
#' @param var df variable
#' @return plot caption
#' @examples
#'
get_title <-
  function(var, peds = FALSE,
           country = NULL) {

  title <- toupper({{country}})
  var_name <- toupper({{var}})
  var_label <- ""

  # Get label
  if (var_name == "VLS") {
    var_label <- "Viral Load Suppression"

  } else if (var_name == "VLC") {
    var_label <- "Viral Load Coverage"

  } else if (var_name == "VLNC") {
    var_label <- "Viral Load Not-Covered"

  } else {
    var_label <- "Viral Load"
  }

  # PEDS flag
  if (peds == TRUE) {
    var_label <- paste0(var_label, " (Under 15yo)")
  }

  # Title
  if (!is.null({{country}})) {
    title <- paste0(toupper({{country}}), " - ", var_label)

  } else {
    title <- var_label
  }

  return(title)
}


#' @title Generate Caption for Graphic/Output
#'
#' @param country country name
#' @param var df variable
#' @return plot caption
#'
get_caption <-
  function(country,
           rep_pd = msd_caption,
           var = NULL) {

  caption <- paste0("OHA/SIEI - Data Source: ",
                    {{rep_pd}},
                    " MSD, Missing data in grey,
                      VLC = TX_PVLS / TX_CURR (2 periods prior), Not Covered = 1 - VLC\n",
                    toupper({{country}}))

  if (is.null({{var}})) {
    caption <- paste0(caption,
                      " - Variables mapped: VLS, VLC, VLnC")

  } else {
    caption <- paste0(caption,
                      " - ",
                      "Variable mapped: ",
                      toupper({{var}}))
  }

  caption <- paste0(caption,
                    ", Produced on ",
                    format(Sys.Date(), "%Y%m%d"))

  return(caption)
}


#' @title Generate Graphic / Output filename
#'
#' @param country country name
#' @param var df variable
#' @return plot file name
#'
get_output_name <-
  function(country,
           rep_pd = "FY21Q2",
           var = "VLC",
           agency = TRUE) {

  name <- paste0(rep_pd,
                 " - ",
                 str_remove(toupper({{country}}), "'"))

  if (!agency) {
    name <- paste0(name, "_USAID")
  }

  name <- paste0(name,
                 "_ViralLoad_",
                 toupper({{var}}),
                 "_",
                 format(Sys.Date(), "%Y%m%d"),
                 ".png")

  return(name)
}


# DATA --------------------------------------------------------------

  ## MER PSNU Data
  df_psnu <- read_msd(file_psnu_im)

  ## Geodata

  ## Terrain Raster
  terr <- get_raster(path = dir_terr)

  ## GEO - PEPFAR Orgs boundaries
  spdf_pepfar <- file_shp %>% sf::read_sf()

  df_attrs <- glamr::get_ouuids() %>%
    filter(!str_detect(operatingunit, " Region$")) %>%
    pull(operatingunit) %>%
    map_dfr(.x, .f = ~get_attributes(country = .x))

  spdf_pepfar <- spdf_pepfar %>%
    left_join(df_attrs, by = c("uid" = "id"))

# MUNGING ----

  ## Filter & Summarize
  df_vl <- extract_viralload(df_msd = df_psnu,
                             rep_agency = rep_agency,
                             rep_fy = rep_fy,
                             rep_pd = rep_qtr)


  ## VL PEDs
  df_vl_u15 <- extract_viralload(df_msd = df_psnu,
                                 rep_agency = rep_agency,
                                 rep_fy = rep_fy,
                                 rep_pd = rep_qtr,
                                 peds = TRUE)


  #PEDs - EID
  df_eid <- extract_eid_viralload(df_msd = df_psnu,
                                  rep_agency = rep_agency,
                                  rep_fy = rep_fy,
                                  rep_pd = rep_qtr)


  # TX_ML -- aggregate bad events then divide by TX_CURR_lagged_1
  # Numerator: Number of patients not retained on ART (LTFU, Died, Stopped)
  # Denominator: TX_CURR_lag1 + TX_NEW_curr + TX_RTT

  df_tx_ml <- extract_tx_ml(df_msd = df_psnu,
                            rep_agency = rep_agency,
                            rep_fy = rep_fy,
                            rep_pd = rep_qtr)


  # TX_PLP => DO NOT RUN in Qtr1
  df_tx_bad <- extract_tx_plp(
    df_msd = df_psnu,
    rep_agency = c("USAID", "HHS/CDC"),
    rep_fy = rep_fy,
    rep_pd = rep_qtr
  )

  # Test calculations/results
  df_tx_bad %>%
    filter(operatingunit == "Zambia") %>%
    group_by(psnu) %>%
    summarise(sum = (TX_ML_PLP))



# VIZ --------------------------------------

  ## Test Individual VL maps

  # cname <- "Nigeria"
  #
  # map_viralload(
  #   spdf = spdf_pepfar,
  #   df = df_vl,
  #   vl_variable = "VLS",
  #   cntry = cname,
  #   terr_raster = terr,
  #   agency = T,
  #   facet_rows = 2
  # )

# BATCH VIZ ---------------------------------------------------------------


## Batch VL mapping

  ## Batch: ALL by Agency

  map_ous <- df_vl %>%
    filter(!str_detect(operatingunit, " Region$"),
           !is.na(VLC)) %>%
    distinct(operatingunit) %>%
    pull()

  map_ous %>%
    map(.x, .f = ~ map_viralloads(
        spdf = spdf_pepfar,
        df = df_vl,
        cntry = .x,
        terr_raster = terr,
        save = TRUE,
        agency = TRUE,
        facet_rows = 2
      )
    )

  ## Move files to google folder
  gdrive_vlbyagency <- gdrive_folder(name = paste0("VL_by_Agency/", msd_caption),
                             path = gdrive_tx_vl,
                             add = TRUE)

  dir_graphics %>%
    list.files(pattern = paste0("^", rep_pd, " - .*_ViralLoad_VL_\\d{8}.png$"),
               full.names = TRUE) %>%
    map_dfr(~drive_upload(.x,
                      path = as_id(gdrive_vlbyagency),
                      name = basename(.x),
                      type = "png",
                      overwrite = TRUE))


  ## Batch: ALL USAID Only
  map_ous <- df_vl %>%
    filter(fundingagency == "USAID",
           !str_detect(operatingunit, " Region$"),
           !is.na(VLC)) %>%
    distinct(operatingunit) %>%
    pull()

  map_ous %>%
    map(.x, .f = ~ map_viralloads(
      spdf = spdf_pepfar,
      df = df_vl %>% filter(fundingagency == "USAID"),
      cntry = .x,
      terr_raster = terr,
      save = TRUE,
      agency = FALSE
    ))

  ## Move files to google folder
  gdrive_vlusaid <- gdrive_folder(name = msd_caption,
                                  path = gdrive_tx_vl,
                                  add = TRUE)

  dir_graphics %>%
    list.files(pattern = paste0("^", rep_pd, " - .*_USAID_ViralLoad_VL_\\d{8}.png$"),
               full.names = TRUE) %>%
    map_dfr(~drive_upload(.x,
                          path = as_id(gdrive_vlusaid),
                          name = basename(.x),
                          type = "png",
                          overwrite = TRUE))

  ## Batch: PEDS by Agency
  map_ous <- df_vl_u15 %>%
    filter(!str_detect(operatingunit, " Region$"),
           !is.na(VLC)) %>%
    distinct(operatingunit) %>%
    pull()

  map_ous %>%
    map(.x, .f = ~ map_peds_viralloads(
        spdf = spdf_pepfar,
        df = df_vl_u15,
        cntry = .x,
        terr_raster = terr,
        save = TRUE,
        agency = TRUE,
        facet_rows = 2
      )
    )

  ## Move files to google folder
  gdrive_vlpedsagency <- gdrive_folder(name = paste0("VL-by-Agency/", msd_caption),
                                  path = gdrive_tx_peds,
                                  add = TRUE)

  dir_graphics %>%
    list.files(pattern = paste0("^", rep_pd, " - .*_ViralLoad_PEDS_VLC_S_\\d{8}.png$"),
               full.names = TRUE) %>%
    map_dfr(~drive_upload(.x,
                          path = as_id(gdrive_vlpedsagency),
                          name = basename(.x),
                          type = "png",
                          overwrite = TRUE))

  ## Batch: PEDS USAID
  map_ous <- df_vl_u15 %>%
    filter(!str_detect(operatingunit, " Region$"),
           fundingagency == "USAID",
           !is.na(VLC)) %>%
    distinct(operatingunit) %>%
    pull()

  map_ous %>%
    map(.x, .f = ~ map_peds_viralloads(
        spdf = spdf_pepfar,
        df = df_vl_u15 %>% filter(fundingagency == "USAID"),
        cntry = .x,
        terr_raster = terr,
        save = TRUE,
        agency = FALSE
      )
    )

  ## Move files to google folder
  gdrive_vlpedsusaid <- gdrive_folder(name = msd_caption,
                                      path = gdrive_tx_peds,
                                      add = TRUE)

  dir_graphics %>%
    list.files(pattern = paste0("^", rep_pd, " - .*_USAID_ViralLoad_PEDS_VLC_S_\\d{8}.png$"),
               full.names = TRUE) %>%
    map_dfr(~drive_upload(.x,
                          path = as_id(gdrive_vlpedsusaid),
                          name = basename(.x),
                          type = "png",
                          overwrite = TRUE))


  # Batch: VL PEDS  & EID Coverage
  map_ous <- df_eid %>%
    filter(!str_detect(operatingunit, " Region$"),
           !is.na(eid_cov_under2)) %>%
    distinct(operatingunit) %>%
    pull()

  map_ous %>%
    map(.x, .f = ~ map_vlc_eid(
        spdf = spdf_pepfar,
        df = df_vl_u15,
        vl_variable = "VLC",
        cntry = .x,
        terr_raster = terr,
        df2 = df_eid,
        mapvar = eid_cov_under2,
        save_all = T
      )
    )

  ## Move files to google folder
  gdrive_vlpedseid <- gdrive_folder(name = paste0("VLC-EID/", msd_caption),
                                      path = gdrive_tx_peds,
                                      add = TRUE)

  dir_graphics %>%
    list.files(pattern = paste0("^", rep_pd, " - .*_ViralLoad_VL_EID_COVERAGE_\\d{8}.png$"),
               full.names = TRUE) %>%
    map_dfr(~drive_upload(.x,
                          path = as_id(gdrive_vlpedseid),
                          name = basename(.x),
                          type = "png",
                          overwrite = TRUE))


# Batch: TX_ML_PLP map
# df_tx_bad %>%
#   filter(!str_detect(operatingunit, " Region$"),
#          !is.na(TX_ML_PLP)) %>%
#   distinct(operatingunit) %>%
#   pull() %>%
#   nth(11) %>%
#   map(.x, .f = ~ tx_graph(
#     spdf = spdf_pepfar,
#     df_gph = df_tx_bad,
#     mapvar = TX_ML_PLP,
#     cntry = .x
#   ))
#
#
# df_tx_bad %>%
#   filter(!str_detect(operatingunit, " Region$"),
#          !is.na(TX_ML_PLP)) %>%
#   distinct(operatingunit) %>%
#   pull() %>%
#   map(.x, .f = ~ tx_batch(
#     spdf = spdf_pepfar,
#     df_gen = df_tx_bad,
#     mapvar = TX_ML_PLP,
#     cntry = .x,
#     rep_pd = rep_pd,
#     terr_raster = terr,
#     save = TRUE,
#     agency = TRUE,
#     facet_rows = 1,
#     gen_title = "",
#     four_parts = F)
#   )
#
# # re-apply psnu cleanup for SA
# tx_batch(
#     spdf = spdf_pepfar,
#     df_gen = df_tx_bad %>%
#       filter(operatingunit == "South Africa") %>%
#       clean_psnu(),
#     mapvar = TX_ML_PLP,
#     cntry = "South Africa",
#     rep_pd = rep_pd,
#     terr_raster = terr,
#     save = TRUE,
#     agency = TRUE,
#     facet_rows = 1,
#     gen_title = "",
#     four_parts = F)








## END ##
