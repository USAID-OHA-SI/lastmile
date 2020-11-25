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
    filter(fiscal_year == 2020, mech_name != "Dedup", psnuuid != "?") %>%
    distinct(fundingagency, snu1, psnu, psnuuid, mech_code, mech_name) %>%
    clean_agency() %>%
    clean_psnu()

  #df_ims %>% View()

  spdf_ims <- spdf %>%
    left_join(df_ims %>% filter(fundingagency == "USAID"),
              by = c("uid" = "psnuuid")) %>%
    filter(!is.na(mech_code))

  ## EID Data
  df_eid <- list.files(
      path = dir_data,
      pattern = uga_eid,
      full.names = TRUE
    ) %>%
    last() %>%
    read_excel(sheet = 1, guess_max = Inf) %>%
    clean_names()

  df_eid <- df_eid %>%
    mutate(no_device_allocated = as.integer(no_device_allocated)) %>%
    group_by(district, datim_district, datim_district_uid) %>%
    summarise_at(vars(starts_with("no_")), sum, na.rm = TRUE)

  #df_eid %>% View()
  #

  spdf_eid <- spdf %>%
    left_join(df_eid, by = c("uid" = "datim_district_uid")) %>%
    filter(no_device_allocated > 0)


# VIZ -----------------------------------------------

  ## EID Device locations
  basemap <- terrain_map(countries = country,
                         terr_path = dir_raster,
                         mask = TRUE)

  basemap

  # Machines
  RColorBrewer::display.brewer.all()

  values <- df_eid %>%
    pull(no_device_allocated) %>%
    unique() %>%
    sort()

  cols <- RColorBrewer::brewer.pal(n = max(values),
                           name = "YlGnBu")

  basemap +
    geom_sf(data = spdf_eid,
            aes(fill = no_device_allocated),
            lwd = .3, linetype = "dotted", color = grey40k, alpha = .8) +
    scale_fill_stepsn(
      breaks = seq(0, max(values), 1),
      guide = guide_colorsteps(even.steps = FALSE),
      na.value = grey40k,
      limits = c(0, max(values)),
      colors = cols) +
    labs(title = "Uganda - EID POC machine availability",
         subtitle = "Grey areas have no machines and machines are aggregated at PSNU level",
         caption = paste0("Data Source: USAID/Uganda - PEPFAR Programs
         Produced by OHA/SIEI on ", format(Sys.Date(), "%Y-%m%d"))) +
    si_style_map()


  ggsave(
    here::here(dir_graphics, "FY20Q4_UGANDA_EID_POC_Machine_Availability.png"),
    plot = last_plot(),
    scale = 1.2,
    dpi = 400,
    width = 7,
    height = 10,
    units = "in"
  )


  # Mechs areas
  mechs <- df_ims %>%
    filter(fundingagency == "USAID") %>%
    pull(mech_code) %>%
    unique() %>%
    sort()

  mcols <- RColorBrewer::brewer.pal(n = length(mechs), name = "Set3")

  basemap +
    geom_sf(data = spdf_ims,
            aes(fill = mech_name),
            lwd = .3, linetype = "dotted", color = grey40k, alpha = .8) +
    scale_fill_discrete()





