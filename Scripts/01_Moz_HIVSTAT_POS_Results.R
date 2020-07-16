## PROJECT:  PEPFAR Geospatial Analytics
## AUTHOR:   B.Kagniniwa, G.Sarfaty | USAID
## LICENSE:  MIT
## PURPOSE:  MOZ - OVC_SERV FY20 Achievements
## Date:     2020-07-13

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

# REQUIRED

source("./Scripts/00_Setup.R")

# GLOBALS -----------------------

country = "Mozambique"

# files
file_ovc <- "OVC DREAMS YCM Districts_Sites for COP20.xlsx"

file_psnu_im1 <- "MER_Structured_Datasets_PSNU_IM_FY18-20_20200605_v1_1.rds"
file_psnu_im2 <- "PSNU_IM_FY18-21_20200605_v1_1.rds"

file_psnu_im3 <- "PSNU_IM_FY18-20_Global_20200626_v1_1.rds"

file_psnu_txt <- "MER_Structured_Datasets_PSNU_IM_FY18-20_20200626_v2_1_Mozambique"

# read_tsv(here(dir_mer, paste0(file_psnu_txt3, ".txt"))) %>%
#     write_rds(path = here(dir_mer, paste0(file_psnu_txt3, ".rds")), compress = "gz")

file_districts <- "Mozambique_PROD_5_District_DistrictLsib_2020_Feb.shp"

# FUNCTIONS ------------------------------------------------------------------------------

    #' Map OVC_HIVSTAT_POS PSNU Results
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
    spdist_ovc_hivstat_pos <- function(country, fy, df_psnu, geo_psnu, terr_path = NULL) {

        # Subset data to OU, FY and Indicator
        df <- df_psnu %>%
            dplyr::filter(
                fundingagency == "USAID",
                operatingunit == {{country}},
                fiscal_year == {{fy}},
                indicator == 'OVC_HIVSTAT_POS'
            ) %>%
            dplyr::select(fiscal_year, snu1, psnu, psnuuid, primepartner, mech_code, mech_name, cumulative)


        # Get Admin1 Spatial Data
        adm1 <- gisr::get_admin1({{country}})

        # Join data to Spatial Data
        df <- geo_psnu %>%
            dplyr::left_join(df, by = c("uid" = "psnuuid")) %>%
            dplyr::filter(!is.na(cumulative))

        # Plot a bar chart
        pos_bar <- df %>%
            dplyr::mutate(label = paste0(psnu, " (", cumulative, ")")) %>%
            ggplot2::ggplot(
                aes(x = reorder(label, cumulative),
                    y = cumulative,
                    fill = cumulative
                )
            ) +
            ggplot2::geom_col(show.legend = F) +
            ggplot2::scale_fill_viridis_c(option = "D", direction = -1) +
            ggplot2::scale_y_continuous(position = "right") +
            ggplot2::labs(x = "", y="") +
            ggplot2::coord_flip() +
            glitr::si_style_nolines() +
            ggplot2::theme(panel.grid.major.x = element_line(size = .2, color = grey10k))

        # Plot the map
        pos_m <- NULL

        if (is.null(terr_path)) {
            pos_m <- gisr::admins_map(countries = {{country}})
        }
        else {
            pos_m <- gisr::terrain_map(countries = {{country}}, terr_path = {{terr_path}}, mask = TRUE)
        }

        pos_m <- pos_m +
            ggplot2::geom_sf(data = df, aes(fill = cumulative, label = psnu), lwd = .1, color = grey10k) +
            ggplot2::geom_sf(data = adm1, fill = NA, linetype = "dotted") +
            ggplot2::geom_sf_text(data = adm1, aes(label = name), color = grey80k, size = 3) +
            ggplot2::scale_fill_viridis_c(direction = -1, na.value = grey30k) +
            ggplot2::coord_sf() +
            gisr::si_style_map() +
            ggplot2::theme(
                legend.position =  c(.9, .2),
                legend.direction = "vertical",
                legend.key.width = ggplot2::unit(.5, "cm"),
                legend.key.height = ggplot2::unit(1, "cm")
            )

        # Combine map + chart
        viz <- (pos_m + pos_bar) +
            patchwork::plot_layout(widths = c(1,1)) +
            patchwork::plot_annotation(
                title = paste0(toupper({{country}}), " - OVC_HIVSTAT_POS Results"),
                subtitle = "Districts hightlighted in grey not part of the PSNUs",
                caption = paste0("OHA/SIEI - DATIM ", {{fy}}, " PSNU x IMs Data, procuded on ", Sys.Date())
            )

        # Print and return images
        print(viz)

        return(viz)
    }

# DATA --------------------------------------

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

    # Explore
    moz_districts %>%
        st_as_sf() %>%
        st_set_geometry(NULL) %>%
        arrange(province, district) %>% glimpse()


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
        filter(str_detect(indicator, "OVC_")) %>%
        distinct(indicator)

    # Filter country & indicator data
    df_ovc_hivstat_pos <- df_psnu %>%
        filter(
            fiscal_year == "2020",
            fundingagency == "USAID",
            operatingunit == 'Mozambique',
            indicator == 'OVC_HIVSTAT_POS'
        ) %>%
        select(snu1, psnu, psnuuid, primepartner, mech_code, mech_name, cumulative)


# Viz Distribution ----------------------------------------------------------------------

    # Map without topo basemap
    moz <- spdist_ovc_hivstat_pos(country = "Mozambique", fy = 2020, df_psnu = df_psnu, geo_psnu = moz_districts)

    # Map with topo basemap
    moz <- spdist_ovc_hivstat_pos(country = "Mozambique", fy = 2020, df_psnu = df_psnu, geo_psnu = moz_districts, terr_path = dir_terr)

    # export map
    ggsave(here(dir_graphs, "MOZ-OVC_HIVSTAT_POS-Results.png"),
       plot = last_plot(), scale = 1.2, dpi = 310,
       width = 10, height = 7, units = "in")

