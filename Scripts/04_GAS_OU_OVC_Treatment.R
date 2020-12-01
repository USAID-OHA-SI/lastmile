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
source("./Scripts/00_Utilities.R")
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

psnu_im <- "^MER_.*_PSNU_IM_.*_20201113_v1_1.zip$"

## Reporting Filters
rep_agency = "USAID"
rep_agencies <- c("USAID", "HHS/CDC")

age_group <- "<20" # options are: "<15", "<20", "All"
age_groups <- c("<15", "<20")

rep_fy = 2020
rep_qtr = 4 # Data available for Q2 & 4

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
get_caption <- function(country,
                        age = "<20",
                        agency = "All Agencies",
                        var = "Proxy Coverage") {
    # Params
    var <- str_replace_all({{var}}, "_", " ")
    agency <- {{agency}}
    agency <- ifelse(!is.null(agency) & toupper(agency) == "USAID",
                     "USAID ", " ")

    # Build caption
    caption <- paste0(
        "*NOTE: Scales are truncated to 100% - Data Source: FY20Q4i MSD\n",
        var, " = OVC_HIV_STAT_POS ", agency, "/ TX_CURR Age ", age, " All Agencies\n",
        "Shown for PSNUs in which USAID is the only agency with OVC Programming\n",
        toupper({{country}}), " - Produced by OHA/SIEI on ", format(Sys.Date(), "%Y%m%d")
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
                            rep_pd = "FY20Q4",
                            var = "Proxy Coverage",
                            age = NULL,
                            agency = NULL) {

    name <- paste0(rep_pd, "_",
                   str_replace({{var}}, " ", "_"))

    # Age
    if (!is.null(age)) {
        name <- paste0(name, "_", str_replace({{age}}, "<", "Under"))
    }

    # Agency
    if (!is.null(agency)) {
        name <- paste0(name, "_", str_replace({{agency}}, " ", "_"))
    }

    # Country & Date

    name <- paste0(name, "_",
                   toupper({{country}}),
                   "_",
                   format(Sys.Date(), "%Y%m%d"),
                   ".png")

    return(name)
}


# DATA --------------------------------------------------------------

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

    df_psnu <- df_psnu %>%
        dplyr::mutate(across(targets:cumulative, as.integer))

    ## Geodata

    ## Terrain Raster
    terr <- get_raster(terr_path = dir_terr)

    ## ORGs
    spdf_pepfar <- build_spdf(
        dir_geo = paste0(dir_geodata, "/VcPepfarPolygons_2020.07.24"),
        df_psnu = df_psnu
    )


    ## MER Data Munging

    ## Proxy OVC Coverage
    df_ovc_cov <- extract_ovc_coverage(df_msd = df_psnu,
                                       rep_fy = rep_fy,
                                       rep_age = "<20", #age_group, #<15 or <20
                                       rep_agency = NULL, #rep_agency,
                                       rep_pd = rep_pd,
                                       sumlevel = "PSNU") # PSNU or SNU1


    ## OVC & TX Overlap
    df_ovc_tx <- extract_ovc_tx_overlap(df_msd = df_psnu,
                                        rep_fy = rep_fy + 1,
                                        rep_agency = rep_agencies)


# VIZ --------------------------------------

    #OVC  Proxy Cov

    ## Test OVC  Proxy Cov maps
    #cname <- "Zambia"
    #cname <- "Zimbabwe"
    #cname <- "Nigeria"
    #cname <- "Ethiopia"
    cname <- "Tanzania"
    #cname <- "South Africa"

    spdf_pepfar %>%
        filter(operatingunit == cname, type == "PSNU") %>%
        ggplot() +
        geom_sf(aes(fill = snu1)) +
        si_style_map()

    df_cntry <- df_ovc_cov %>%
        filter(operatingunit == cname)

    map_ovc_coverage(spdf = spdf_pepfar,
                     df_ovc = df_cntry,
                     terr_raster = terr,
                     agency = F)

    plot_ovc_coverage(df_ovc = df_cntry)

    plot_ovc_coverage(df_ovc = df_ovc_cov, country = cname)


    viz_ovc_coverage(spdf = spdf_pepfar,
                     df_ovc = df_cntry,
                     terr_raster = terr,
                     rep_pd = rep_pd,
                     caption = get_caption(cname))


    # Batch 1: OVC Proxy Coverage <20, All Agencies
    df_ovc_cov <- extract_ovc_coverage(df_msd = df_psnu,
                                       rep_fy = rep_fy,
                                       rep_age = "<20",
                                       rep_agency = NULL,
                                       rep_pd = rep_pd)

    df_ovc_cov %>%
        filter(proxy_coverage > 0, !str_detect(operatingunit, " Region$")) %>%
        distinct(operatingunit) %>%
        pull() %>%
        map(.x, .f = ~ viz_ovc_coverage(
            spdf = spdf_pepfar,
            df_ovc = df_ovc_cov,
            terr_raster = terr,
            rep_pd = rep_pd,
            country = .x,
            age = "<20",
            caption = get_caption(.x, age = "<20", agency = "All Agencies"),
            filename = get_output_name(.x, age = "<20", agency = "All Agencies"),
            save = T))

    # Batch 2: OVC Proxy Coverage <15, All Agencies
    df_ovc_cov <- extract_ovc_coverage(df_msd = df_psnu,
                                       rep_fy = rep_fy,
                                       rep_age = "<15",
                                       rep_agency = NULL,
                                       rep_pd = rep_pd)

    df_ovc_cov %>%
        filter(proxy_coverage > 0, !str_detect(operatingunit, " Region$")) %>%
        distinct(operatingunit) %>%
        pull() %>%
        map(.x, .f = ~ viz_ovc_coverage(
            spdf = spdf_pepfar,
            df_ovc = df_ovc_cov,
            terr_raster = terr,
            rep_pd = rep_pd,
            country = .x,
            age = "<15",
            caption = get_caption(.x, age = "<15", agency = "All Agencies"),
            filename = get_output_name(.x, age = "<15", agency = "All Agencies"),
            save = T))

    # Batch 3: OVC Proxy Coverage <20, USAID Only
    df_ovc_cov <- extract_ovc_coverage(df_msd = df_psnu,
                                       rep_fy = rep_fy,
                                       rep_age = "<20",
                                       rep_agency = rep_agency,
                                       rep_pd = rep_pd)

    df_ovc_cov %>%
        filter(proxy_coverage_usaid > 0,
               !str_detect(operatingunit, " Region$")) %>%
        distinct(operatingunit) %>%
        pull() %>%
        #nth(8) %>%
        map(.x, .f = ~ viz_ovc_coverage(
            spdf = spdf_pepfar,
            df_ovc = df_ovc_cov,
            terr_raster = terr,
            rep_pd = rep_pd,
            country = .x,
            age = "<20",
            caption = get_caption(.x, age = "<20", agency = "USAID Only"),
            filename = get_output_name(.x, age = "<20", agency = "USAID Only"),
            save = T))

    # Batch 4: OVC Proxy Coverage <15, USAID Only
    df_ovc_cov <- extract_ovc_coverage(df_msd = df_psnu,
                                       rep_fy = rep_fy,
                                       rep_age = "<15",
                                       rep_agency = rep_agency,
                                       rep_pd = rep_pd)

    df_ovc_cov %>%
        filter(proxy_coverage_usaid > 0,
               !str_detect(operatingunit, " Region$")) %>%
        distinct(operatingunit) %>%
        pull() %>%
        map(.x, .f = ~ viz_ovc_coverage(
            spdf = spdf_pepfar,
            df_ovc = df_ovc_cov,
            terr_raster = terr,
            rep_pd = rep_pd,
            country = .x,
            age = "<15",
            caption = get_caption(.x, age = "<15", agency = "USAID Only"),
            filename = get_output_name(.x, age = "<15", agency = "USAID Only"),
            save = T))


    ## OVC x TX Overlap

    df_ovc_tx <- extract_ovc_tx_overlap(df_msd = df_psnu,
                                        rep_fy = rep_fy + 1,
                                        rep_agency = rep_agencies)

    ## Test countries viz

    ## Countries
    cntries <- df_ovc_tx %>%
        filter(!str_detect(operatingunit, " Region$"),
               !is.na(ovc_group)) %>%
        group_by(operatingunit) %>%
        summarise(mixed = 'Mixed' %in% ovc_group) %>%
        ungroup() %>% #View()
        filter(mixed == TRUE) %>%
        distinct(operatingunit) %>%
        pull()

    cname <- cntries %>% nth(2)

    df_cntry <- df_ovc_tx %>%
        filter(operatingunit == cname)

    heatmap_ovctx_coverage(df = df_cntry)

    map_ovctx_coverage(spdf = spdf_pepfar,
                       df_ovctx = df_cntry,
                       terr_raster = terr)

    map_mixed_coverage(spdf = spdf_pepfar,
                       df_ovctx = df_cntry,
                       terr_raster = terr)

    ovctx_caption <- paste0(
        "OHA/SIEI - Data Source: FY20Q4i MSD - USAID & CDC,
            OVC_SERV_UNDER_18 & TX_CURR, Age < 20\n",
        toupper(cntries %>% nth(1)), ", Produced on ",
        format(Sys.Date(), "%Y%m%d")
    )

    viz_ovctx_coverage(spdf = spdf_pepfar,
                       df_ovctx = df_cntry,
                       terr_raster = terr,
                       caption = ovctx_caption,
                       #save = FALSE,
                       save = TRUE,
                       filename = paste0(rep_pd, "_OVC_TX_Overlap_",
                                         toupper(cname), "_",
                                         format(Sys.Date(), "%Y%m%d"), ".png"))

    # Batch OVC/TX Coverage
    cntries %>%
        map(.x, .f = ~ viz_ovctx_coverage(
                spdf = spdf_pepfar,
                df_ovctx = df_ovc_tx %>% filter(operatingunit == .x),
                terr_raster = terr,
                caption = paste0(
                    "OHA/SIEI - Data Source: FY20Q4i MSD - USAID & CDC,
            OVC_SERV_UNDER_18 & TX_CURR, Age < 20\n",
                    toupper(.x), ", Produced on ",
                    format(Sys.Date(), "%Y%m%d")
                ),
                save = TRUE,
                filename = paste0(rep_pd, "_OVC_TX_Overlap_",
                                  toupper(.x), "_",
                                  format(Sys.Date(), "%Y%m%d"), ".png")
            ))

## END ##

