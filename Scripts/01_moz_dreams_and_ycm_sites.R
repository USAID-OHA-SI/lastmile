## PROJECT:  PEPFAR Geospatial Analytics
## AUTHOR:   B.Kagniniwa, G.Sarfaty | USAID
## LICENSE:  MIT
## PURPOSE:  MOZ - DREAMS Districts & YCM Sites
## Date:     2020-07-27

# LIBRARIES

library(tidyverse)
library(readxl)
library(vroom)
library(foreign)
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

user <- ""
key <- ""

country = "Mozambique"

# files
file_ovc <- "OVC DREAMS YCM Districts_Sites for COP20.xlsx"

file_psnu_txt3 <- "MER_Structured_Datasets_PSNU_IM_FY18-20_20200626_v2_1_Mozambique"

file_districts <- "Mozambique_PROD_5_District_DistrictLsib_2020_Feb.shp"

# Data

## Geo
moz0 <- get_adm_boundaries("MOZ", adm_level = 0, geo_path = dir_geo) %>%
    st_as_sf()

moz1 <- get_adm_boundaries("MOZ", adm_level = 1, geo_path = dir_geo) %>%
    st_as_sf() %>%
    select(country = name_0, province = name_1)

moz2 <- get_adm_boundaries("MOZ", adm_level = 2, geo_path = dir_geo) %>%
    st_as_sf() %>%
    select(country = name_0, province = name_1, district = name_2)


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
            district == "Chonguene" ~ "Chongoene",
            district == "Cidade De Xai-Xai" ~ "Xai-Xai",
            district == "Chimoio" ~ "Cidade De Chimoio",
            district == "ManhiÒ«a" ~ "Manhica",
            district == "Cidade Da Matola" ~ "Matola",
            district == "Cidade Da Beira" ~ "Beira",
            district == "Maganja Da Costa" ~ "Maganja da Costa",
            district == "Cidade De Tete" ~ "Tete",
            district == "Chókwe" ~ "Chokwe",
            district == "Cidade De Quelimane" ~ "Quelimane",
            district == "Cidade De Chimoio" ~ "Chimoio",
            TRUE ~ district
        )
    )

## MOZ DREAMS & YCM Sites

shts <- excel_sheets(path = here(dir_data, file_ovc))

dfs <- shts[-1] %>%
    set_names(str_replace_all(., " ", "_")) %>%
    map(read_excel, path = here(dir_data, file_ovc))

## MOZ DREAMS
df_dreams <- dfs$`DREAMS_COP20_new_+_old_district` %>%
    janitor::clean_names() %>%
    mutate_all(trimws, which = "both") %>%
    mutate(
        new_cop_20 = as.factor(new_cop_20),
        province = gsub(" Prov", "", province)
    ) %>%
    relocate(new_cop_20, .after = last_col()) %>%
    arrange(province, district)

df_dreams_districts <- df_dreams %>%
    left_join(geo_psnu, by = "district") %>%
    dplyr::rename(province = province.x) %>%
    dplyr::select(-province.y) %>%
    st_as_sf()

df_dreams_districts %>%
    ggplot() +
    geom_sf(aes(fill=new_cop_20), lwd = .5, color = grey20k) +
    si_style_map()


## MOZ YCM

df_ycm <- dfs$Youth_Case_Management_Sites_COP

df_ycm_cols <- dfs$Youth_Case_Management_Sites_COP[1,] %>%
    unlist() %>%
    unname()

df_ycm <- df_ycm %>%
    filter(row_number() > 1) %>%
    set_names(df_ycm_cols) %>%
    clean_names()

df_ycm %>% glimpse()

## MOZ Facilities Locations

df_facs <- extract_locations(country = country, username = user, password = glamr::mypwd(key)) %>%
    extract_facilities()

df_ycm_sites <- df_ycm %>%
    left_join(df_facs, by = c("facility_uid" = "id"))


# VIZ -------------------------------------------------------

## DREAMS
terrain_map(countries = country, terr_path = dir_terr, mask = TRUE) +
    geom_sf(data = df_dreams_districts, aes(fill = new_cop_20), size = .3, color = grey10k, alpha= .6, show.legend = F) +
    geom_sf(data = adm1, fill = NA, size = .3, linetype = "dotted") +
    geom_sf_text(data = adm1, aes(label = province), size = 4, color = grey90k) +
    scale_fill_manual(values = c(USAID_medblue, USAID_red)) +
    labs(
        title = "MAZAMBIQUE - COP20 DREAMS Districts",
        subtitle = "Districts in red are new as of COP20",
        caption = paste0(
            "OHA/SIEI/SI - Data from DATIM Q2 & HQ/PEDS-OVC Team, ", Sys.Date(),
            "\nNote: names and boundaries are not necessarily authoritative."
        )
    ) +
    si_style_map()

ggsave(here(dir_graphs, "MOZ - DREAMS Districts for COP20.png"),
       plot = last_plot(), scale = 1.2,
       dpi = 310, width = 7, height = 10, units = "in")


## DREAMS + YCM Sites
terrain_map(countries = country, terr_path = dir_terr, mask = TRUE) +
    geom_sf(data = df_dreams_districts, aes(fill = new_cop_20), size = .3, color = grey10k, alpha= .6, show.legend = F) +
    geom_point(data = df_ycm_sites, aes(longitude, latitude), shape = 21, size = 3, fill = grey80k, colour = "white") +
    geom_sf(data = adm1, fill = NA, size = .3, linetype = "dotted") +
    geom_sf_text(data = adm1, aes(label = province), size = 4, color = grey90k) +
    scale_fill_manual(values = c(USAID_medblue, USAID_red)) +
    labs(
        title = "MAZAMBIQUE - COP20 DREAMS Districts & YCM Sites",
        subtitle = "Districts in red are new and dark dots are YCM Sites",
        caption = paste0(
            "OHA/SIEI/SI - Data from DATIM Q2 & HQ/PEDS-OVC Team, ", Sys.Date(),
            "\nNote: names and boundaries are not necessarily authoritative."
        )
    ) +
    si_style_map()

ggsave(here(dir_graphs, "MOZ - DREAMS Districts and YCM Sites for COP20.png"),
       plot = last_plot(), scale = 1.2,
       dpi = 310, width = 7, height = 10, units = "in")
