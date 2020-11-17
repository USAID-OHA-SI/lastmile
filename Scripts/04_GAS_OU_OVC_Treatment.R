##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa, G.Sarfaty & T.Essam | USAID
##  PURPOSE: Geo-depiction of VL - % not covered
##  LICENCE: MIT
##  DATE:    2020-09-04
##  UPDATED: 2020-11-16

# Libraries
library(tidyverse)
library(vroom)
library(sf)
library(sp)
library(raster)
library(gisr)
library(glitr)
library(glamr)
library(janitor)
library(scales)
library(patchwork)
library(here)
library(ICPIutilities)
library(extrafont)

# REQUIRED -------------------------------------------------------------------------------

    ## Get Credentials

    source("../_secrets/credentials.R")
    source("./Scripts/00_Geo_Utilities.R")
    source("./Scripts/00_OVC_Utilities.R")


# GLOBALS -------------------------------------------------------------

## Data & Output folders

dir_data <- "Data"
dir_dataout <- "Dataout"
dir_gis <- "GIS"
dir_graphics <- "Graphics"
dir_geodata <- "../../GEODATA/PEPFAR"
dir_terr <- "../../GEODATA/RASTER"
dir_merdata <- "../../MERDATA"

## Q3 MER Data

psnu_im <- "^MER_.*_PSNU_IM_.*_20200918_.*.zip$"

## Reporting Filters
rep_agency = "USAID"
rep_agencies <- c("USAID", "HHS/CDC")

rep_fy = 2020
rep_qtr = 2 # Data available for Q2 & 4

rep_pd = rep_fy %>%
    as.character() %>%
    str_sub(3,4) %>%
    paste0("FY", ., "Q", rep_qtr)


# FUNCTIONS ---------------------------------------------------------

## NOTE: These functions will end up in gisr package

#' Get Title
#'
#' @param country country name
#' @param var df variable
#' @return plot caption
#'
get_title <- function(country, var = NULL) {

    title <- toupper({{country}})

    var_name <- toupper({{var}})

    # Get label

    # Title
    if (!is.null(var_name)) {
        title <- paste0(toupper({{country}}), " - ", var_name)
    }
    else {
        title <- var_label
    }

    return(title)
}

#' Graphic caption
#'
#' @param country country name
#' @param var df variable
#' @return plot caption
#'
get_caption <- function(country, var = NULL) {

    caption <- paste0(
        "OHA/SIEI - Data Source: FY20Q3c MSD - USAID Only,
            Proxy Coverage = OVC_HIV_STAT_POS / TX_CURR, Age < 20\n",
        toupper({{country}}), ", Produced on ",
        format(Sys.Date(), "%Y%m%d")
    )

    return(caption)
}


#' Graphic file name
#'
#' @param country country name
#' @param var df variable
#' @return plot file name
#'
get_output_name <- function(country,
                            rep_pd = "FY20Q2",
                            var = "Proxy Coverage") {

    name <- paste0(rep_pd, "_",
                   str_replace({{var}}, " ", "_"),
                   "_", toupper({{country}}),
                   "_",
                   format(Sys.Date(), "%Y%m%d"),
                   ".png")

    return(name)
}


# DATA --------------------------------------------------------------

    ## Geodata

    ## Terrain Raster
    terr <- get_raster(terr_path = dir_terr)

    ## ORGs
    spdf_pepfar <- build_spdf(dir_geo = dir_geodata, df_psnu = df_psnu)

    ## MSD

    ## File path + name
    file_psnu_im <- list.files(
            path = dir_merdata,
            pattern = psnu_im,
            recursive = TRUE,
            full.names = TRUE
        ) %>%
        sort() %>%
        last()

    ## MER PSNU Data
    df_psnu <- vroom(file_psnu_im, col_types = c(.default = "c"))


    ## MER Data Munging

    ## Proxy OVC Coverage
    df_ovc_cov <- extract_ovc_coverage(df_msd = df_psnu,
                                       rep_fy = rep_fy,
                                       rep_agency = rep_agency,
                                       rep_pd = rep_pd)


    ## OVC & TX Overlap
    df_ovc_tx <- extract_ovc_tx_overlap(df_msd = df_psnu,
                                        rep_fy = rep_fy + 1,
                                        rep_agency = rep_agencies)


# VIZ --------------------------------------

    ## Test OVC  Proxy Cov maps
    cname <- "Zambia"
    cname <- "Zimbabwe"

    df_cntry <- df_ovc_cov %>% filter(operatingunit == cname)


    map_ovc_coverage(spdf = spdf_pepfar,
                     df_ovc = df_cntry,
                     terr_raster = terr)

    plot_ovc_coverage(df_ovc = df_cntry)

    plot_ovc_coverage(df_ovc = df_ovc_cov, country = cname)


    viz_ovc_coverage(spdf = spdf_pepfar,
                     df_ovc = df_cntry,
                     terr_raster = terr,
                     rep_pd = rep_pd,
                     caption = get_caption(cname))


    # Batch - OVC Proxy Coverage
    df_ovc_cov %>%
        distinct(operatingunit) %>%
        pull() %>%
        map(.x, .f = ~ viz_ovc_coverage(spdf = spdf_pepfar,
                                        df_ovc = df_ovc_cov,
                                        terr_raster = terr,
                                        rep_pd = rep_pd,
                                        country = .x,
                                        caption = get_caption(.x),
                                        filename = get_output_name(.x),
                                        save = T))


    # Test OVC x TX Overlap

    cntries <- df_ovc_tx %>%
        filter(!str_detect(operatingunit, " Region$")) %>%
        distinct(operatingunit) %>%
        pull()

    df_cntry <- df_ovc_tx %>%
        filter(operatingunit == cntries %>% nth(24))

    heatmap_ovc_tx(df = df_cntry)



## END ##

