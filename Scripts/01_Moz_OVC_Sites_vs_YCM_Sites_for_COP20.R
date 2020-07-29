## PROJECT:  PEPFAR Geospatial Analytics
## AUTHOR:   B.Kagniniwa, G.Sarfaty | USAID
## LICENSE:  MIT
## PURPOSE:  MOZ - COP20 OVC Districts by IP
## Date:     2020-07-27

# LIBRARIES

library(tidyverse)
library(readxl)
library(vroom)
library(sf)
library(gisr)
library(glitr)
library(glamr)
library(janitor)
library(scales)
library(RColorBrewer)

# REQUIRED

source("./Scripts/00_Setup.R")

# GLOBALS -----------------------

country = "Mozambique"

# files
file_ovc <- "OVC DREAMS YCM Districts_Sites for COP20.xlsx"

file_psnu_txt <- "MER_Structured_Datasets_PSNU_IM_FY18-20_20200626_v2_1_Mozambique"

file_districts <- "Mozambique_PROD_5_District_DistrictLsib_2020_Feb.shp"

# DATA --------------------------------------------------------------------------

## Geo
amd0 <- get_adm_boundaries("MOZ", adm_level = 0, geo_path = dir_geo) %>%
    st_as_sf()

adm1 <- get_adm_boundaries("MOZ", adm_level = 1, geo_path = dir_geo) %>%
    st_as_sf() %>%
    select(country = name_0, province = name_1) %>%
    mutate(province = ifelse(province == 'Nassa', "Niassa", province))

adm2 <- get_adm_boundaries("MOZ", adm_level = 2, geo_path = dir_geo) %>%
    st_as_sf() %>%
    select(country = name_0, province = name_1, district = name_2)

## PSNUs boundaries
geo_psnu <- list.files(
        path = here(dir_geo, "PEPFAR"),
        pattern = file_districts,
        recursive = TRUE,
        full.names = TRUE
    ) %>%
    read_sf()

geo_psnu <- geo_psnu %>%
    select(uid, country = level3name, province = level4name, district = level5name) %>%
    mutate(
        district = case_when(
            district == "Chonguene" ~ "Chongwene",
            district == "Cidade De Xai-Xai" ~ "Xai-Xai",
            district == "Chimoio" ~ "Cidade De Chimoio",
            district == "ManhiÒ«a" ~ "Manhica",
            district == "Cidade Da Matola" ~ "Matola",
            district == "Cidade Da Beira" ~ "Beira",
            district == "Maganja da Costa" ~ "Maganja Da Costa",
            district == "Cidade De Tete" ~ "Tete",
            district == "Chokwe" ~ "Chókwe",
            district == "Cidade De Quelimane" ~ "Quelimane",
            TRUE ~ district
        )
    )


## PSNU by IM
df_psnu <- list.files(
        path = dir_merdata,
        pattern = paste0(file_psnu_txt, ".zip"),
        full.names = TRUE
    ) %>%
    vroom()

df_psnu %>%
    distinct(fiscal_year)

## MOZ OVC Districts

shts <- excel_sheets(path = here(dir_data, file_ovc))
shts

dfs <- shts[-1] %>%
    set_names(str_replace_all(., " ", "_")) %>%
    map(read_excel, path = here(dir_data, file_ovc))

df_ovc <- dfs$OVC_Districts_COP20 %>%
    select(1:4) %>%
    filter(row_number() > 1)

df_ovc_cols <- dfs$OVC_Districts_COP20[1,] %>%
    unlist() %>%
    unname()

colnames(df_ovc) <- df_ovc_cols[1:4]

df_ovc <- df_ovc %>%
    clean_names() %>%
    rename(province = row_labels) %>%
    mutate(province = ifelse(province == "Zambeze", "Zambezia", province)) %>%
    distinct(implementing_partner, province, district)

df_ovc_districts <- df_ovc %>%
    left_join(geo_psnu, by=c("province", "district")) %>%
    st_as_sf()



## VIZ -----------------------------------------------------------------

    terrain_map(countries = country, terr_path = dir_terr, mask = TRUE) +
        geom_sf(data = df_ovc_districts, aes(fill = implementing_partner), size = .3, color = grey10k, alpha= .6) +
        geom_sf(data = adm1, fill = NA, size = .3, linetype = "dotted") +
        geom_sf_text(data = adm1, aes(label = province), size = 4, color = grey90k) +
        scale_fill_brewer(type = "qual", palette = "Set1") +
        labs(
            title = "MAZAMBIQUE - COP20 OVC Districts by IP",
            subtitle = "District level coverage for each implementing partner",
            caption = paste0(
                "OHA/SIEI/SI - Data from DATIM Q2 & HQ/PEDS-OVC Team, ", Sys.Date(),
                "\nNote: names and boundaries are not necessarily authoritative."
            )
        ) +
        si_style_map() +
        theme(
            legend.direction = "vertical",
            legend.position = c(.8, .2),
            legend.key.width = unit(20, "pt"),
            legend.key.height = unit(15, "pt"),
            legend.text.align = 0
        )

    ggsave(here(dir_graphs, "MOZ - OVC Districts by IP COP20.png"),
       plot = last_plot(), scale = 1.2,
       dpi = 310, width = 7, height = 10, units = "in")
