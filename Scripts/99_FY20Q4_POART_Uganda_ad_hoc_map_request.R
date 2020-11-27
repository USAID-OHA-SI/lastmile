##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: Uganda AOI Map
##  LICENCE: MIT
##  DATE:    2020-11-25

# Libraries
library(tidyverse)
library(readxl)
library(vroom)
library(sp)
library(sf)
library(raster)
library(gisr)
library(glitr)
library(glamr)
library(janitor)
library(scales)
library(patchwork)
library(ggrepel)
library(here)
library(ICPIutilities)
library(extrafont)
library(viridis)
library(tidytext)


# GLOBALS -------------------------------------------------------------

  ## Data & Output folders

  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_graphics <- "Graphics"
  dir_geodata <- NULL          # set your own path from _setup folder
  dir_raster <- NULL           # same as above
  dir_merdata <- NULL          # same as above

  ## UGA PSNU Shp
  psnu_shp <- "Uganda_PROD_5_District.*.shp"

  ## FY20Q4i MER Data
  psnu_im <- "^MER_.*_PSNU_IM_.*_20201113_v1_1.zip$"

  ## EID Data file
  uga_eid <- "EID Device M-Pima.*"

  # Country
  country <- "Uganda"

  # Over default folder
  source("../_setup/00_setup.R")
  source("./Scripts/00_Utilities.R")


# DATA --------------------------------------------------------------

  ## Geodata

  # Country Boundaries
  adm0 <- gisr::get_admin0(countries = country)

  # District shapefiles
  spdf <- list.files(
      path = dir_geodata,
      pattern = psnu_shp,
      recursive = TRUE,
      full.names = TRUE
    ) %>%
    last() %>%  # Just in case you have more than 1 file
    sf::read_sf() %>%
    janitor::clean_names()

  spdf %>%
    st_set_geometry(NULL) %>%
    glimpse()

  spdf %>%
    ggplot() +
    geom_sf(aes(fill = subregion), lwd = .5, color = glitr::grey10k) +
    si_style_map()

  ## MSD
  df_msd <- list.files(
      path = dir_merdata,
      pattern = psnu_im,
      full.names = TRUE
    ) %>%
    read_msd()

  df_msd <- df_msd %>%
    filter(operatingunit == "Uganda")

  ## Mechanisms
  df_ims <- df_msd %>%
    filter(fiscal_year == 2020,
           fundingagency == "USAID",
           mech_name != "Dedup",
           psnuuid != "?",
           indicator == "TX_CURR") %>%
    distinct(fundingagency, snu1, psnu, psnuuid, mech_code, mech_name) %>%
    clean_agency() %>%
    clean_psnu()

  #View(df_ims)

  df_ims <- df_ims %>%
    mutate(
      mech_name = if_else(
        str_detect(mech_name, "\\)"),
        str_extract(mech_name, "(?<=\\().*(?=\\))"),
        mech_name
      ))

  spdf_ims <- spdf %>%
    left_join(df_ims %>% filter(fundingagency == "USAID"),
              by = c("uid" = "psnuuid")) %>%
    filter(!is.na(mech_code))

  ## EID Data
  df_eid_machines <- list.files(
      path = dir_data,
      pattern = uga_eid,
      full.names = TRUE
    ) %>%
    last() %>%
    read_excel(sheet = 1, guess_max = Inf) %>%
    clean_names()

  df_eid_machines <- df_eid_machines %>%
    mutate(no_device_allocated = as.integer(no_device_allocated)) %>%
    group_by(district, datim_district, datim_district_uid) %>%
    summarise_at(vars(starts_with("no_")), sum, na.rm = TRUE)

  #df_eid %>% View()

  spdf_eid_machines <- spdf %>%
    left_join(df_eid_machines, by = c("uid" = "datim_district_uid")) %>%
    filter(no_device_allocated > 0)

  # EID Coverage
  spdf_eid_coverage <- spdf %>%
    left_join(df_eid %>%
                filter(fundingagency == "USAID", operatingunit == country),
              by = c("uid" = "psnuuid")) %>%
    filter(eid_cov_under2 > 0)

  # EID COV & Machines
  spdf_eid_mcov <- spdf_eid_coverage %>%
    left_join(df_eid_machines, by = c("uid" = "datim_district_uid")) %>%
    filter(no_device_allocated > 0)


# VIZ -----------------------------------------------

  ## EID Device locations
  basemap <- terrain_map(countries = country,
                         terr_path = dir_raster,
                         mask = TRUE)

  basemap

  # Colors
  RColorBrewer::display.brewer.all()

  values <- df_eid_machines %>%
    pull(no_device_allocated) %>%
    unique() %>%
    sort()

  cols <- RColorBrewer::brewer.pal(n = max(values),
                           name = "YlGnBu")
  # Machines
  m_machines <- basemap +
    geom_sf(data = spdf_eid_machines,
            aes(fill = no_device_allocated),
            lwd = .3, linetype = "dotted",
            color = grey40k, alpha = .8) +
    geom_sf(data = spdf_eid_machines, fill = NA,
            lwd = .3, linetype = "dotted", color = grey40k) +
    geom_sf(data = adm0, fill = NA) +
    geom_sf_text(data = spdf_eid_machines,
                 aes(label = no_device_allocated),
                 size = 2.5, color = grey60k) +
    scale_fill_stepsn(
      breaks = seq(0, max(values), 1),
      guide = guide_colorsteps(even.steps = FALSE),
      na.value = grey40k,
      limits = c(0, max(values)),
      colors = cols) +
    labs(subtitle = "EID POC Machines availability, All Agencies") +
    si_style_map()

  m_machines

  # EID Coverage
  m_eid_cov <- basemap +
    geom_sf(
      data = spdf_eid_coverage,
      aes(fill = eid_cov_under2),
      na.rm = T,
      lwd = .3,
      linetype = "dotted",
      color = "white",
      alpha = 0.8
    ) +
    geom_sf(data = adm0, fill = NA) +
    geom_sf_label(data = spdf_eid_mcov,
                 aes(label = no_device_allocated),
                 size = 2.5, fill = grey10k, color = grey80k,
                 label.padding = unit(0.2, "lines"),
                 label.size = .1) +
    scale_fill_stepsn(
      colors = c("#D73027","#FC8D59","#FEE08B","#D9EF8B","#91CF60","#1A9850"),
      breaks = seq(0, 1, by = 0.25),
      guide = guide_colorsteps(show.limits = F, even.steps = F),
      na.value = grey40k,
      limits = c(0, 1),
      labels = percent,
      oob = scales::oob_squish,
      values = scales::rescale(seq(0, 1, by = 0.25), c(0, 1))
    ) +
    labs(subtitle = "EID Coverage Under 2yo, USAID Only") +
    si_style_map() +
    theme(
      legend.position =  "bottom",
      legend.direction = "horizontal",
      legend.key.width = ggplot2::unit(1.5, "cm"),
      legend.key.height = ggplot2::unit(.5, "cm")
    )

  m_eid_cov

  m_all <- ((m_machines + m_eid_cov) +
    plot_annotation(
      title = "Uganda - Early Enfant Diagnosis POC Machines & Coverage",
      caption = paste0("Data Source: USAID/Uganda - PEPFAR Programs
           Produced by OHA/SIEI on ", format(Sys.Date(), "%Y-%m%d")),
      theme = theme(
        text = element_text(family = "Gill Sans MT"),
        plot.title = element_text(hjust = .5)
      )
    ))

  m_all

  ggsave(
    here::here(dir_graphics, "FY20Q4_UGANDA_EID_POC_Machine_Availability.png"),
    plot = last_plot(),
    scale = 1.2,
    dpi = 400,
    width = 10,
    height = 7,
    units = "in"
  )




  # Mechs areas

  mechs <- df_ims %>%
    pull(mech_code) %>%
    unique() %>%
    sort()

  mcols <- RColorBrewer::brewer.pal(n = length(mechs), name = "Set3")

  basemap +
    geom_sf(data = spdf_ims,
            aes(fill = mech_name),
            lwd = .3, linetype = "dotted", color = grey40k, alpha = .8) +
    scale_fill_discrete()





