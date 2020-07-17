## PROJECT:  PEPFAR Geospatial Analytics
## AUTHOR:   B.Kagniniwa, G.Sarfaty | USAID
## LICENSE:  MIT
## PURPOSE:  MOZ - OVC_SERV FY20 Achievements
## Date:     2020-07-17

# LIBRARIES

library(tidyverse)
library(ggplot2)
library(readxl)
library(tidytext)
library(sf)
library(gisr)
library(glitr)
library(glamr)
library(janitor)
library(scales)
library(patchwork)
library(ggrepel)
library(RColorBrewer)

# REQUIRED -------------------------------------------------------------------------------

source("./Scripts/00_Setup.R")

# GLOBALS --------------------------------------------------------------------------------

country = "Mozambique"

file_psnu_txt <- "MER_Structured_Datasets_PSNU_IM_FY18-20_20200626_v2_1_Mozambique"

file_districts <- "Mozambique_PROD_5_District_DistrictLsib_2020_Feb.shp"

# FUNCTIONS ------------------------------------------------------------------------------

#' Map OVC TX_CURR Achievement by PSNU
#'
#' @param country Operating Unit name
#' @param fy Fiscal Year (4 digit)
#' @param df_psnu PSNU by IM Dataset (data frame)
#' @param geo_psnu PSNU Geodata (sf data frame)
#' @param terr_path Path to terrain raster file
#' @export
#' @examples
#' \dontrun{
#' spdist_ovc_hivstat_pos(country = "saturn", fy = 3030, df_psnu = psnu_by_im, geo_psnu = saturn_districts)
#' spdist_ovc_hivstat_pos(country = "saturn", fy = 3030, df_psnu = psnu_by_im, geo_psnu = saturn_districts, terr_path = path_to_terr)
#' }
#'
spdist_ovc_tx_curr <- function(country, fy, df_psnu, geo_psnu, terr_path = NULL) {

    # Subset data to OU, FY and Indicator
    df <- df_psnu %>%
        dplyr::filter(
            fundingagency == "USAID",
            operatingunit == {{country}},
            fiscal_year == {{fy}},
            indicator == 'TX_CURR',
            disaggregate == 'Age/Sex/HIVStatus'
        ) %>%
        select(snu1, psnu, psnuuid, primepartner, mech_code, mech_name, disaggregate, trendsfine, targets:cumulative) %>%
        mutate(age_group = ifelse(trendsfine %in% c("<01", "01-09", "10-14", "15-19"), "OVC", "Other")) %>%
        relocate(age_group, .after = trendsfine) %>%
        filter(age_group == 'OVC') %>%
        group_by(snu1, psnu, psnuuid, age_group) %>%
        summarise_at(.vars = vars(targets:cumulative), .funs = sum, na.rm = TRUE) %>%
        ungroup() %>%
        mutate(
            ovc_tx_curr_ach = round(cumulative / targets * 100),
            ovc_tx_curr_cat = cut(ovc_tx_curr_ach, breaks = c(-Inf, 49, 89, Inf), labels = c("<50%", "50-89%", "+90%"))
        ) %>%
        arrange(snu1, psnu, desc(ovc_tx_curr_ach)) %>%
        dplyr::mutate(label = paste0(psnu, " (", ovc_tx_curr_ach, ")"))


    # Get Admin1 Spatial Data
    adm1 <- gisr::get_admin1({{country}})

    # Join data to Spatial Data
    df <- geo_psnu %>%
        dplyr::left_join(df, by = c("uid" = "psnuuid")) %>%
        dplyr::filter(!is.na(psnu))

    # Plot a bar chart
    tx_curr_bar <- df %>%
        ggplot2::ggplot(
            aes(x = reorder(label, ovc_tx_curr_ach),
                y = ovc_tx_curr_ach,
                fill = ovc_tx_curr_cat
            )
        ) +
        ggplot2::geom_point(aes(size = targets, color = ovc_tx_curr_cat), alpha = 2/3, show.legend = F) +
        ggplot2::scale_color_manual(values = c("#d7191c", "#dfc27d", "#008837")) +
        ggplot2::scale_y_continuous(position = "right", labels = function(x) {paste0(x, "%")}) +
        ggplot2::labs(x = "", y="") +
        ggplot2::coord_flip() +
        glitr::si_style_nolines() +
        ggplot2::theme(panel.grid.major.x = element_line(size = .2, color = grey10k))

    # Plot the map
    tx_curr_m <- NULL

    if (is.null(terr_path)) {
        tx_curr_m <- gisr::admins_map(countries = {{country}})
    }
    else {
        tx_curr_m <- gisr::terrain_map(countries = {{country}}, terr_path = {{terr_path}}, mask = TRUE)
    }

    tx_curr_m <- tx_curr_m +
        ggplot2::geom_sf(data = df, aes(fill = ovc_tx_curr_cat, alpha = 2/3), lwd = .1, color = grey10k) +
        ggplot2::geom_sf(data = adm1, fill = NA, linetype = "dotted") +
        ggplot2::geom_sf_text(data = adm1, aes(label = name), color = grey80k, size = 3) +
        ggplot2::scale_fill_manual(values = c("#d7191c", "#dfc27d", "#008837")) +
        ggplot2::coord_sf() +
        gisr::si_style_map() +
        guides(fill = guide_legend(override.aes = list(alpha = 2/3))) +
        ggplot2::theme(
            legend.position =  c(.9, .2),
            legend.direction = "vertical",
            legend.key.width = ggplot2::unit(.5, "cm"),
            legend.key.height = ggplot2::unit(1, "cm")
        )

    # Combine map + chart
    viz <- (tx_curr_m + tx_curr_bar) +
        plot_layout(widths = c(1,1)) +
        plot_annotation(
            title = paste0(toupper({{country}}), " - TX_CURR Achievement by PSNU (Age < 20)"),
            subtitle = "The size of the circles represent the targets and the colors the achievements",
            caption = paste0("OHA/SIEI - DATIM ", {{fy}}, " PSNU x IMs Data, procuded on ", Sys.Date())
        )

    # Print and return images
    print(viz)

    return(viz)
}

# DATA -----------------------------------------------------------------------------------

    ## Geo - Moz PSNUs Boundaries
    moz_districts <- list.files(
        path = here(dir_geo, "PEPFAR"),
        pattern = file_districts,
        recursive = TRUE,
        full.names = TRUE
    ) %>%
        unlist() %>%
        first() %>%
        read_sf()

    # Update District Names
    moz_districts <- moz_districts %>%
        select(uid, country = level3name, province = level4name, district = level5name) %>%
        mutate(
            district = case_when(
                district == "Chonguene" ~ "Chongoene",
                district == "Cidade De Xai-Xai" ~ "Xai-Xai",
                district == "Cidade De Chimoio" ~ "Chimoio",
                district == "ManhiÒ«a" ~ "Manhica",
                district == "Cidade Da Matola" ~ "Matola",
                district == "Cidade Da Beira" ~ "Beira",
                district == "Maganja Da Costa" ~ "Maganja da Costa",
                district == "Cidade De Quelimane" ~ "Quelimane",
                TRUE ~ district
            )
        )



    ## MER PSNU x IMs
    df_psnu <- read_rds(here(dir_mer, paste0(file_psnu_txt, ".rds")))

    df_psnu %>% glimpse()

    df_psnu %>%
        distinct(fiscal_year) %>%
        pull()

    df_psnu %>%
        distinct(fundingagency) %>%
        pull()

    df_psnu %>%
        filter(str_detect(indicator, "TX_")) %>%
        distinct(indicator)

    df_psnu %>%
        filter(disaggregate == 'Age/Sex/HIVStatus') %>%
        distinct(trendsfine) %>%
        pull()

    # Filter country & indicator data
    df_ovc_tx_curr <- df_psnu %>%
        filter(
            fiscal_year == "2020",
            fundingagency == "USAID",
            operatingunit == 'Mozambique',
            indicator == 'TX_CURR',
            disaggregate == 'Age/Sex/HIVStatus'
        ) %>%
        select(snu1, psnu, psnuuid, primepartner, mech_code, mech_name, disaggregate, trendsfine, targets:cumulative) %>%
        mutate(age_group = ifelse(trendsfine %in% c("<01", "01-09", "10-14", "15-19"), "OVC", "Other")) %>%
        relocate(age_group, .after = trendsfine) %>%
        filter(age_group == 'OVC') %>%
        group_by(snu1, psnu, psnuuid, age_group) %>%
        summarise_at(.vars = vars(targets:cumulative), .funs = sum, na.rm = TRUE) %>%
        ungroup() %>%
        mutate(
            ovc_tx_curr_ach = round(cumulative / targets * 100),
            ovc_tx_curr_cat = cut(ovc_tx_curr_ach, breaks = c(-Inf, 49, 89, Inf), labels = c("<50%", "50-89%", "+90%"))
        ) %>%
        arrange(snu1, psnu, desc(ovc_tx_curr_ach)) %>%
        dplyr::mutate(label = paste0(psnu, " (", ovc_tx_curr_ach, ")"))


# VIZ -------------------------------------------------------------------------------

    ## Viz - Combine Achievements + Map
    spdist_ovc_tx_curr(country,
                       fy = 2020,
                       df_psnu = df_psnu,
                       geo_psnu = moz_districts,
                       terr_path = dir_terr)


    ## TX_CURR Achievements for OVC
    moz_bar <- df_ovc_tx_curr %>%
        ggplot2::ggplot(
            aes(x = reorder(label, ovc_tx_curr_ach),
                y = ovc_tx_curr_ach,
                fill = ovc_tx_curr_cat
            )
        ) +
        ggplot2::geom_segment(x= "Beira (62)", y = 50, xend = "Beira (62)", yend = 62, size=.2, color = grey30k) +
        ggplot2::geom_point(aes(size = targets, color = ovc_tx_curr_cat), alpha = 2/3, show.legend = F) +
        ggplot2::scale_color_manual(values = c("#d7191c", "#dfc27d", "#008837")) +
        ggplot2::scale_y_continuous(position = "right", labels = function(x) {paste0(x, "%")}) +
        ggplot2::annotate(geom = "text",
                          x = "Beira (62)", y = 40,
                          label = "Beira is at 62% \nwith target set at 5,470") +
        ggplot2::labs(x = "", y="") +
        ggplot2::coord_flip() +
        glitr::si_style_nolines() +
        ggplot2::theme(panel.grid.major.x = element_line(size = .2, color = grey10k))


    # TX_CURR Map
    adm1 <- gisr::get_admin1(country)

    df_ovc_tx_curr_geo <- moz_districts %>%
        left_join(df_ovc_tx_curr, by=c("uid" = "psnuuid")) %>%
        filter(!is.na(psnu))

    moz_m <- gisr::terrain_map(countries = country, terr_path = dir_terr, mask = TRUE) +
        ggplot2::geom_sf(data = df_ovc_tx_curr_geo, aes(fill = ovc_tx_curr_cat, alpha = 2/3), lwd = .1, color = grey10k) +
        ggplot2::geom_sf(data = adm1, fill = NA, linetype = "dotted") +
        ggplot2::geom_sf_text(data = adm1, aes(label = name), color = grey80k, size = 3) +
        ggplot2::scale_fill_manual(values = c("#d7191c", "#dfc27d", "#008837")) +
        ggplot2::coord_sf() +
        gisr::si_style_map() +
        guides(fill = guide_legend(override.aes = list(alpha = 2/3))) +
        ggplot2::theme(
            legend.position =  c(.9, .2),
            legend.direction = "vertical",
            legend.key.width = ggplot2::unit(.5, "cm"),
            legend.key.height = ggplot2::unit(1, "cm")
        )

    (moz_m + moz_bar) +
        plot_layout(widths = c(1,1)) +
        plot_annotation(
            title = "MOZAMBIQUE - TX_CURR Achievement by PSNU (Age < 20)",
            subtitle = "The size of the circles represent the targets and the colors the achievements",
            caption = "OHA/SIEI - DATIM FY20 Q2 Data as of 20200626"
        )

    ggsave(here(dir_graphs, "MOZ-TX_CURR_UNDER_20-Achievement.png"),
           scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")

