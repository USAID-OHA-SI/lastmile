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

# REQUIRED

source("./Scripts/00_Setup.R")

# GLOBALS -----------------------

country = "Mozambique"

# files
file_ovc <- "OVC DREAMS YCM Districts_Sites for COP20.xlsx"
file_psnu_im1 <- "MER_Structured_Datasets_PSNU_IM_FY18-20_20200605_v1_1.rds"
file_psnu_im2 <- "PSNU_IM_FY18-21_20200605_v1_1.rds"

file_psnu_im3 <- "PSNU_IM_FY18-20_Global_20200626_v1_1.rds"

file_psnu_txt3 <- "Genie_PSNU_IM_Global_Frozen_8cdac4af-9175-4859-bf88-a35e76be063c.txt"

# read_tsv(here(dir_mer, file_psnu_txt3))%>%
#     write_rds(path = here(dir_mer, file_psnu_im3), compress = "gz")

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


moz_districts <- (list.files(path = here(dir_geo, "PEPFAR"),
                             pattern = file_districts,
                             recursive = TRUE,
                             full.names = TRUE) %>%
                      map(read_sf))[[1]]

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

moz_districts %>%
    st_as_sf() %>%
    st_set_geometry(NULL) %>%
    arrange(province, district) %>% glimpse()

## MOZ OVC & YCM Sites

shts <- excel_sheets(path = here(dir_data, file_ovc))
shts

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

df_dreams %>% head()

df_dreams <- moz_districts %>%
    left_join(df_dreams, by="district") %>%
    select(-province.y) %>%
    rename(province = province.x) %>%
    filter(!is.na(new_cop_20))

df_dreams %>%
    st_set_geometry(NULL) %>%
    prinf()

df_dreams %>%
    ggplot() +
    geom_sf(aes(fill=new_cop_20), lwd = .5, color = grey20k) +
    si_style_map()

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
    distinct()


## MOZ YCM
