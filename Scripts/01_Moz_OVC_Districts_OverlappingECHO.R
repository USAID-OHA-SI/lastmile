## PROJECT:  PEPFAR Geospatial Analytics
## AUTHOR:   B.Kagniniwa, G.Sarfaty | USAID
## LICENSE:  MIT
## PURPOSE:  MOZ - OVC_SERV FY20 Achievements
## Date:     2020-07-13

# LIBRARIES

library(tidyverse)
library(readxl)
library(tidytext)
library(sf)
library(gisr)
library(glitr)
library(glamr)
library(vroom)
library(janitor)
library(scales)
library(patchwork)
library(ggrepel)

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
    full.names = TRUE) %>%
    read_sf()

geo_psnu <- geo_psnu %>%
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

geo_psnu %>%
    st_as_sf() %>%
    st_set_geometry(NULL) %>%
    arrange(province, district) %>% glimpse()

## PSNU by IM
df_psnu <- list.files(
        path = dir_merdata,
        pattern = paste0(file_psnu_txt, ".zip"),
        full.names = TRUE
    ) %>%
    vroom()

df_psnu %>% glimpse()

df_psnu %>%
    distinct(mech_code, mech_name, primepartner) %>%
    prinf()

geo_psnu_echo <- df_psnu %>%
    filter(fiscal_year == 2020, fundingagency == "USAID", str_detect(mech_name, "ECHO")) %>%
    select(snu1:psnuuid) %>%
    distinct_all() %>%
    left_join(geo_psnu, by = c("psnuuid" = "uid"))

echo_provinces<- geo_psnu_echo %>%
    distinct(province) %>%
    pull()

## MOZ OVC
df_ovc <- dfs$OVC_Districts_COP20 %>%
    select(1:4) %>%
    filter(row_number() > 1)

df_ovc_cols <- dfs$OVC_Districts_COP20[1,] %>%
    unlist() %>%
    unname()

colnames(df_ovc) <- df_ovc_cols[1:4]

df_ovc <- df_ovc %>%
    clean_names()

df_ovc %>%
    distinct(implementing_partner) %>%
    pull()

df_ovc %>% View()

df_ovc_districts <- df_ovc %>%
    rename(province = row_labels) %>%
    group_by(province, district) %>%
    summarise(ip_count = n_distinct(implementing_partner)) %>%
    ungroup()

df_ovc_districts <- df_ovc_districts %>%
    left_join(geo_psnu, by=c("province", "district"))

df_ovc_districts <- df_ovc_districts %>%
    st_as_sf()

adm1 %>%
    st_set_geometry(NULL) %>%
    select(province) %>% pull()

geo_ocho_prov <- adm1 %>%
    select(province) %>%
    dplyr::filter(province %in% echo_provinces)

## VIZ -----------------------------------------------------------------

    terrain_map(countries = country, terr_path = dir_terr, mask = TRUE) +
        geom_sf(data = geo_ocho_prov, size = .3, fill = USAID_dkred, color = grey10k, alpha= .5) +
        geom_sf(data = df_ovc_districts, size = .2, fill = USAID_ltblue, color = grey10k, alpha= .6) +
        geom_sf(data = adm1, fill = NA, size = .3, linetype = "dotted") +
        geom_sf_text(data = adm1, aes(label = province), size = 4, color = grey90k) +
        labs(
            title = "MAZAMBIQUE - OVC Districts over ECHO Provinces",
            subtitle = "ECHO Provinces are in red and OVC districts in light blue",
            caption = paste0("OHA/SIEI/SI - Data from DATIM Q2 & HQ/PEDS-OVC Team, ", Sys.Date())
        ) +
        si_style_map()

    ggsave(here(dir_graphs, "MOZ - OVC Districts over ECHO Provinces.png"),
       plot = last_plot(), scale = 1.2,
       dpi = 310, width = 7, height = 10, units = "in")
