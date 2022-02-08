##  PROJECT: SI Support for Zambia
##  AUTHOR:  B.Kagniniwa & T.ESSAM | USAID
##  PURPOSE: COP22 - ART Coverage for PEDS
##  LICENCE: MIT
##  DATE:    2022-02-08


# PACKAGES ----

  library(tidyverse)
  library(readxl)
  library(googlesheets4)
  library(gophr)
  library(glitr)
  library(glamr)
  library(gisr)
  library(sf)
  library(janitor)
  library(gt)
  library(scales)
  library(cowplot)
  library(extrafont)

  source("./Scripts/00_VL_Utilities.R")

# Access keys

  load_secrets()

# Paths ----

  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"

  dir_geodata <- si_path("path_vector")
  dir_terr <- si_path("path_raster")
  dir_merdata <- si_path("path_msd")

# Files ----

  # MER Data - get the latest MSD PSNU x IM file
  file_psnu <- dir_merdata %>%
    return_latest("PSNU_IM_FY19.*\\d{8}_v.*_\\d{1}.zip")

  # Country data
  gdrive <- "12mo9be0IA3lbYHS7_8RAySlA1xOe6twbKW8U2bquBzo"
  gs_name <- "District Peds"

  # Shapefile path
  file_shp <- dir_geodata %>%
    return_latest(
      pattern = "VcPepfarPolygons.*.shp",
      recursive = TRUE)

# Params ----

  cntry <- "Zambia"
  agency <- "USAID"
  inds <- c("PLHIV", "TX_CURR")


# Data ----

  # Location ----

  ## Terrain Raster
  terr <- gisr::get_raster(path = dir_terr)

  ## GEO - PEPFAR Orgs boundaries
  spdf_pepfar <- file_shp %>% read_sf()

  # Attibutes
  df_attrs <- get_attributes(country = cntry)

  spdf_zambia <- spdf_pepfar %>%
    left_join(df_attrs, by = c("uid" = "id")) %>%
    filter(!is.na(operatingunit))

  spdf_zambia %>% glimpse()
  spdf_zambia %>% pull(label) %>% unique()


  spdf_adm0 <- spdf_zambia %>% filter(label == "country")
  spdf_adm1 <- spdf_zambia %>% filter(label == "snu1")
  spdf_adm2 <- spdf_zambia %>% filter(label == "prioritization")
  spdf_adm3 <- spdf_zambia %>% filter(label == "community")

  spdf_adm1 <- spdf_adm1 %>%
    mutate(name = str_remove(name, " Province"))

  # MER Data
  df_msd <- file_psnu %>%
    read_msd() %>%
    filter(operatingunit == cntry)

  rep_pd <- df_msd %>% identifypd(pd_type = "full")
  rep_fy <- df_msd %>% identifypd(pd_type = "year")

  df_psnus <- df_msd %>%
    filter(fiscal_year == rep_fy) %>%
    distinct(fundingagency, snu1uid, psnuuid) %>%
    clean_agency()

  df_vl <- df_msd %>%
    extract_viralload(df_msd = .,
                      rep_agency = agency,
                      rep_fy = rep_fy,
                      rep_pd = rep_qtr,
                      peds = TRUE)


  # Program
  df_art <- gdrive %>%
    read_sheet(sheet = gs_name) %>%
    clean_names()

  df_art %>% glimpse()

  df_art_prov <- df_art %>%
    group_by(province) %>%
    summarise(across(c(clhiv, tx_curr_15), sum, na.rm = T)) %>%
    mutate(art_cov_2022 = tx_curr_15 / clhiv,
           province = str_remove(province, " Province"))

  spdf_art <- spdf_adm2 %>%
    left_join(df_art, by = c("name" = "psnu")) %>%
    left_join(df_psnus, by = c("uid" = "psnuuid")) %>%
    filter(fundingagency == agency) %>%
    mutate(lbl_color = case_when(
      art_cov_2022 < .6 ~ grey90k,
      TRUE ~ grey10k
    ))

  spdf_art %>% glimpse()

  spdf_art_prov <- spdf_adm1 %>%
    left_join(df_art_prov, by = c("name" = "province")) %>%
    left_join(df_psnus %>% distinct(fundingagency, snu1uid), by = c("uid" = "snu1uid")) %>%
    filter(fundingagency == agency) %>%
    mutate(lbl_color = case_when(
      art_cov_2022 < .8 ~ grey90k,
      TRUE ~ grey10k
    ))

  spdf_art_prov %>% glimpse()

# VIZ

  spdf_adm0 %>% gview()
  spdf_adm1 %>% gview()
  spdf_adm2 %>% gview()

  # Basemap
  basemap <- terrain_map(countries = cntry,
                         adm0 = spdf_adm0,
                         adm1 = spdf_adm1,
                         terr = terr,
                         mask = T)

  # Art Coverage
  art_cov_map <- basemap +
    geom_sf(data = spdf_art, aes(fill = art_cov_2022),
            color = grey10k, size = .5) +
    geom_sf_text(data = spdf_art,
                 aes(label = percent(art_cov_2022, 1),
                     color = lbl_color),
                 check_overlap = T,
                 size = 4, show.legend = F) +
    # geom_sf_text(data = spdf_adm1, aes(label = name),
    #              color = grey70k, size = 8) +
    geom_sf(data = spdf_adm1, fill = NA, size = 1.1, color = grey30k) +
    geom_sf(data = spdf_adm1, fill = NA, size = .3, color = grey70k) +
    geom_sf(data = spdf_adm0, fill = NA, size = 1.5, color = grey10k) +
    geom_sf(data = spdf_adm0, fill = NA, size = .3, color = grey70k) +
    scale_fill_si(palette = "genoas", breaks = seq(0, 2, .5), labels = percent) +
    scale_color_identity() +
    scale_y_continuous(labels = percent) +
    labs(title = "COP22 ART COVERAGE (<15yo)",
         subtitle = "The North Western, Western are the only provinces with at least 80% coverage") +
    si_style_map() +
    theme(axis.title = element_blank(),
          legend.title = element_blank(),
          legend.key.width = unit(50, "pt"))

  art_cov_map

  # Art Coverage
  art_cov_prov_map <- basemap +
    geom_sf(data = spdf_art_prov, aes(fill = art_cov_2022),
            color = grey10k, size = .5) +
    geom_sf_text(data = spdf_art_prov,
                 aes(label = percent(art_cov_2022, 1), color = lbl_color),
                 size = 5, show.legend = F) +
    geom_sf(data = spdf_adm0, fill = NA, size = 1.5, color = grey10k) +
    geom_sf(data = spdf_adm0, fill = NA, size = .3, color = grey70k) +
    scale_fill_si(palette = "genoas", breaks = seq(0, 2, .5), labels = percent) +
    scale_color_identity() +
    scale_y_continuous(labels = percent) +
    # labs(title = "ZAMBIA - COP22 ART COVERAGE (<15yo)",
    #      subtitle = "The North Western, Western are the only provinces with at least 80% coverage") +
    si_style_map() +
    theme(axis.title = element_blank(),
          legend.title = element_blank(),
          legend.key.width = unit(100, "pt"))

  art_cov_prov_map

  # VL Coverage -> ./Scripts/04_GAS_OU_ViralLoad_NotCovered
  map_peds_viralloads(spdf = spdf_adm2,
                        df = df_vl,
                        cntry = cntry,
                        terr_raster = terr,
                        agency = T)
