##  PROJECT: lastmile
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: Health Care Services Coverage
##  REF. ID: eb005b7c
##  LICENCE: MIT
##  DATE:    2023-04-05
##  UPDATE:  2023-04-05

# LIBRARIES ----

  library(tidyverse)
  library(readxl)
  library(vroom)
  library(sf)
  library(rnaturalearth)
  library(glamr)
  library(glitr)
  library(gisr)
  library(gophr)
  library(grabr)
  #library(mindthegap)
  library(janitor)
  #library(here)
  library(scales)
  library(patchwork)
  #library(shadowtext)

  ## Credentials & Utilities
  source("./Scripts/00_Utilities.R")

# GLOBALS ----

  ## Data & Output folders

  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"
  dir_geodata <- si_path("path_vector")
  dir_merdata <- si_path("path_msd")

  open_path(dir_merdata)

  ## PSNUxIM Dataset

  file_nat <- dir_merdata %>%
    return_latest(pattern = "NAT_SUBNAT")

  ## UHC Indicators

  file_uhc <- dir_data %>%
    return_latest(pattern = "GH_scorecard_indicators")

  ## Notes

  ous <- c("Asia Region", "Vietnam")

  curr_fy <- source_info(file_nat, type = "NAT_SUBNAT", return = "fiscal_year")
  curr_pd <- source_info(file_nat, type = "NAT_SUBNAT", return = "period")
  src_msd <- source_info(file_nat, return = "source")

  footnote <- paste0("Source: WHO/The Global Health Observatory - Produced by GH/OHA/SIEI on ", curr_date())

# FUNCTIONS ----



# DATA ----

  ## GeoData

  spdf <- ne_countries(type = "sovereignty",
                       scale = 110,
                       returnclass = "sf") %>%
    dplyr::select(sovereignt, admin, name, adm0_a3) %>%
    filter(admin != "Antarctica") %>% # Remove Antarctica
    glamr::clean_countries(colname = "admin") %>%
    mutate(admin = case_when(
      admin == "Papua New Guinea" ~ "PNG",
      TRUE ~ admin
    ))

  spdf_proj <- spdf %>%
    st_transform(crs = st_crs("+proj=robin"))

  spdf_proj %>%
    st_drop_geometry() %>%
    glimpse()

  ## PEPFAR Support Countries

  #df_pepfar <- pepfar_country_list

  ## UHC

  df_uhc <- file_uhc %>% read_csv(col_types = "c")

  df_uhc %>% glimpse()

  df_uhc <- df_uhc %>%
    filter(year == 2019,
           indicator %in% c("uhc_service_coverage_index",
                            "uhc_subindex1_capacity_access",
                            "uhc_subindex4_id"))

  df_uhc_cov <- df_uhc %>%
    filter(indicator == "uhc_service_coverage_index") %>%
    rename(countryname = country) %>%
    right_join(pepfar_country_list, by = c("iso" = "country_iso")) %>%
    filter(!is.na(country), !is.na(value))%>%
    mutate(label_color = case_when(
      value >= 50 ~ grey10k,
      TRUE ~ grey70k
    ))

  spdf_uhc_cov <- spdf_proj %>%
    left_join(df_uhc_cov, by = c("adm0_a3" = "iso")) %>%
    filter(!is.na(value))


# VIZ ----

  ## Global Map - Showing Asia Countries
  viz_map <- ggplot() +
    geom_sf(data = spdf_proj, fill = NA, color = grey30k, size = .4) +
    geom_sf(data = spdf_uhc_cov, aes(fill = value),
            color = grey10k, size = .2) +
    geom_sf_text(data = spdf_uhc_cov,
                 aes(label = paste0(adm0_a3, "\n", comma(value, 1)),
                     color = label_color),
                 size = 2.5, fontface = "bold", check_overlap = T) +
    scale_fill_si(palette = "genoas",
                  na.value = grey10k,
                  labels = comma,
                  limits = c(30, 100),
                  breaks = seq(30, 100, 10)) +
    scale_color_identity() +
    labs(
      x = "", y = "",
      caption = footnote
    ) +
    si_style_map() +
    theme(
      legend.direction = "horizontal",
      legend.position = "top",
      legend.justification = "center",
      legend.title = element_blank(),
      legend.key.width = unit(4, "cm"),
      legend.key.height = unit(.5, "cm"),
      plot.title = element_text(face = "bold")
    )

  viz_map

  si_save(filename = file.path(dir_graphics,
                              paste0("PEPFAR - MAP uhc_service_coverage_index - ",
                                     curr_date(), ".png")),
         plot = viz_map,
         scale = 1.2,
         dpi = 310,
         width = 10,
         height = 7,
         units = "in")

  ## Plot

  viz_plot <- ggplot() +
    geom_col(data = df_uhc_cov %>% filter(value >= 50),
             aes(x = value, y = reorder(countryname, value), fill = value),
             show.legend = F) +
    #geom_vline(xintercept = seq(0, 100, 20)) +
    geom_text(data = df_uhc_cov %>% filter(value >= 50),
              aes(x = value, y = reorder(countryname, value),
                  label = value, color = label_color),
              size = 5, fontface = "bold", hjust = 1.2) +
    scale_fill_si(palette = "genoas",
                  na.value = grey10k,
                  limits = c(30, 100),
                  breaks = seq(30, 100, 10)) +
    scale_color_identity() +
    scale_x_continuous(position = "top") +
    labs(
      x = "", y = "",
      caption = paste("Note: List of countries with at least 50% Universal Health Coverage\n", footnote)
    ) +
    si_style_xgrid() +
    theme(axis.text.x = element_text())

  viz_plot

  si_save(filename = file.path(dir_graphics,
                               paste0("PEPFAR - Bars uhc_service_coverage_index - ",
                                      curr_date(), ".png")),
          plot = viz_plot,
          scale = 1.2,
          dpi = 310,
          width = 10,
          height = 7,
          units = "in")


