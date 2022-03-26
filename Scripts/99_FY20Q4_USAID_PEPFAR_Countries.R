##  PROJECT: Qtr Data Review analysis
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: USAID PEPFAR Footpring
##  LICENCE: MIT
##  DATE:    2020-08-26
##  UPDATED: 2022-03-26

# Dependancies----------------------------------------------------------

library(tidyverse)
library(readxl)
library(vroom)
library(sf)
library(rnaturalearth)
library(here)
library(glamr)
library(glitr)
library(gisr)
library(gophr)
library(janitor)
library(scales)
library(RColorBrewer)
library(patchwork)

# GLOBALS -------------------------------------------------------------

  ## Credentials & Utilities

    source("./Scripts/00_Utilities.R")

  ## Data & Output folders

    dir_data <- "Data"
    dir_dataout <- "Dataout"
    dir_gis <- "GIS"
    dir_graphics <- "Graphics"
    dir_geodata <- si_path("path_vector")
    dir_merdata <- si_path("path_msd")

  ## PSNUxIM Dataset

    rep_agency <- "USAID"

    rep_fy <- 2020

    ou_im <- "OU_IM_FY20-22"

    file_ou_im <- dir_merdata %>% return_latest(pattern = ou_im)

  ## Notes

    footnote <- paste0("USAID / Office of HIV/AIDS - USAID/PEPFAR Programs Footprint as of ", Sys.Date())

# FUNCTIONS -------------------------------------------------------------



# DATA ------------------------------------------------------------------

  ## GeoData

    spdf <- ne_countries(type = "sovereignty",
                         scale = 110,
                         returnclass = "sf") %>%
      dplyr::select(sovereignt, admin, name, adm0_a3) %>%
      filter(admin != "Antarctica") %>% # Remove Antarctica
      glamr::clean_countries(colname = "admin")


    spdf <- spdf %>%
      st_transform(crs = st_crs("+proj=robin"))

  ## MER OUxIM

    ## Raw data
    df_ou <- read_msd(file_ou_im)

    df_ou %>% glimpse()

    rep_fy = df_ou %>% identifypd(pd_type = "year")
    rep_pd = df_ou %>% identifypd(pd_type = "full")

    # Distinct Countries
    df_cntries <- df_ou %>%
      filter(fundingagency == rep_agency,
             fiscal_year == rep_fy,
             !is.na(targets)) %>%
      distinct(operatingunit, countryname) %>%
      arrange(operatingunit, countryname) %>%
      clean_countries(colname = "countryname")

    # TX_CURR
    df_tx <- df_ou %>%
      filter(fundingagency == rep_agency,
             fiscal_year == rep_fy,
             indicator == "TX_CURR",
             indicatortype %in% c("DSD", "TA"),
             standardizeddisaggregate == "Total Numerator") %>%
      group_by(countryname, indicator, indicatortype) %>%
      summarise(across(cumulative, sum, na.rm = T), .groups = "drop") %>%
      mutate(country = countryname) %>%
      clean_countries(colname = "countryname") %>%
      pivot_wider(names_from = indicatortype,
                  values_from = cumulative) %>%
      rowwise() %>%
      mutate(tx = sum(DSD, TA, na.rm = T)) %>%
      ungroup()

    df_usaid_footprint <- df_cntries %>%
      left_join(df_tx, by = "countryname")

    # Join MSD to spdf
    spdf_ou <- spdf %>%
      left_join(df_cntries, by = c("admin" = "countryname")) %>%
      filter(!is.na(operatingunit))

    # Join TX to SPDF
    spdf_usaid <- spdf %>%
      left_join(df_usaid_footprint, by = c("admin" = "countryname")) %>%
      filter(!is.na(operatingunit)) %>%
      mutate(countryname = admin) %>%
      clean_countries(colname = "countryname") %>%
      mutate(
        tx_services = case_when(
          !is.na(DSD) & is.na(TA) ~ paste0("DSD = ", comma(DSD)),
          is.na(DSD) & !is.na(TA) ~ paste0("TA = ", comma(TA)),
          !is.na(DSD) & !is.na(TA) ~ paste0("DSD = ", comma(DSD), "\n", "TA = ", comma(TA)),
        ),
        label1 = case_when(
          !is.na(tx) ~ paste0(countryname, "\n", comma(tx)),
          TRUE ~ countryname
        ),
        label2 = case_when(
          !is.na(tx) ~ paste0(adm0_a3, "\n", comma(tx)),
          TRUE ~ adm0_a3
        ),
        label3 = case_when(
          !is.na(tx_services) ~ paste0(countryname, "\n", tx_services),
          TRUE ~ countryname
        ),
        color1 = case_when(
          tx > 1000000 | adm0_a3 == "LSO"  ~ "white",
          TRUE ~ usaid_black
        )
      )

    spdf_usaid$tx
    spdf_usaid %>% dview()

    tx_max <- max(spdf_usaid$tx, na.rm = T)
    tx_min <- min(spdf_usaid$tx, na.rm = T)


## VIZ ---------------------------------------------------------

  ## Global Maps
    ggplot() +
      geom_sf(data = spdf, fill = NA, color = grey50k, size = .4) +
      geom_sf(data = spdf_ou,
              fill = usaid_blue,
              color = grey30k,
              size = .2) +
      geom_sf_text(data = spdf_ou, aes(label = adm0_a3),
                   size = 1.8,
                   color = grey30k) +
      labs(
        #title = "USAID - HIV/AIDS Programs",
        #subtitle = "Countries in blue are supported by USAID",
        x = "", y = "",
        caption = footnote
      ) +
      si_style_map() +
      theme(
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.justification = "center",
        legend.title = element_blank(),
        legend.key.width = unit(1.5, "cm"),
        plot.title = element_text(face = "bold")
      )

    ggsave(here(dir_graphics, paste0(rep_pd, "_Global_USAID_PEPFAR_Countries.png")),
           plot = last_plot(),
           scale = 1.2, dpi = 310,
           width = 10, height = 7, units = "in")


    ## Global / USAID Countries
    ggplot() +
      geom_sf(data = spdf, fill = NA, color = grey50k, size = .4) +
      geom_sf(data = spdf_usaid,
              aes(fill = tx), color = grey20k, size = .2) +
      geom_sf_text(data = spdf_usaid,
                   aes(label = label2, color = color1),
                   size = 2,
                   fontface = "bold",
                   #color = usaid_black,
                   check_overlap = TRUE
                   ) +
      scale_fill_si(palette = "genoas",
                    #labels = label_number_si(unit = ""),
                    labels = comma,
                    limits = c(0, tx_max),
                    breaks = c(seq(0, tx_max, 250000))) +
      scale_color_identity() +
      labs(
        #title = "USAID - HIV/AIDS Programs",
        #subtitle = "Countries in blue are supported by USAID",
        x = "", y = "",
        caption = footnote
      ) +
      si_style_map() +
      theme(
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.justification = "center",
        legend.title = element_blank(),
        legend.key.width = unit(3, "cm"),
        legend.key.height = unit(.3, "cm"),
        plot.title = element_text(face = "bold")
      )

    ggsave(here(dir_graphics, paste0(rep_pd, "_Global_USAID_PEPFAR_TX_Programs.png")),
           plot = last_plot(),
           scale = 1.2, dpi = 310,
           width = 10, height = 7, units = "in")



