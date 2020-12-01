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

  ## Cascasde data file
  uga_cas <- "^FY20Q4 - Cascade Data.*"

  # Country
  country <- "Uganda"

  # Over default folder
  source("../_setup/00_setup.R")
  source("./Scripts/00_Utilities.R")
  source("./Scripts/00_VL_Utilities.R")


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
    clean_psnu() %>%
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

  ## EID Machines Data
  df_eid_machines <- list.files(
      path = dir_data,
      pattern = uga_eid,
      full.names = TRUE
    ) %>%
    last() %>%
    read_excel(sheet = 1, guess_max = Inf) %>%
    clean_names() %>%
    mutate(no_device_allocated = as.integer(no_device_allocated)) %>%
    group_by(district, datim_district, datim_district_uid) %>%
    summarise_at(vars(starts_with("no_")), sum, na.rm = TRUE)

  spdf_eid_machines <- spdf %>%
    left_join(df_eid_machines, by = c("uid" = "datim_district_uid")) %>%
    filter(no_device_allocated > 0)

  # EID Coverage
  df_eid <- extract_eid_viralload(df_msd, rep_pd = 4)

  spdf_eid_coverage <- spdf %>%
    left_join(df_eid, by = c("uid" = "psnuuid")) %>%
    filter(eid_cov_under2 > 0)

  # EID COV & Machines
  spdf_eid_mcov <- spdf_eid_coverage %>%
    left_join(df_eid_machines, by = c("uid" = "datim_district_uid")) %>%
    filter(no_device_allocated > 0)


  # Clinical Cascade

  # File
  file_cascade <- list.files(
      path = dir_data,
      pattern = uga_cas,
      full.names = TRUE
    ) %>%
    last()

  # data
  df_cascade <- file_cascade %>%
    read_excel(sheet = 1, guess_max = 10000) %>%
    clean_names() %>%
    gather(indicator, value, ends_with("q4")) %>%
    separate(indicator, into = c("indicator", "period"), sep = "_f") %>%
    mutate(
      period = str_to_upper(str_replace(period, "y20", "fy20")),
      age_group = case_when(
        age %in% c("<1", "1-4", "5-9","10-14") ~ "<15",
        TRUE ~ "15+"
      )
    ) %>%
    rename(agency = agenncy, psnuuid = psnu_id, mech_name = im) %>%
    relocate(period, .before = psnu) %>%
    relocate(age_group, .after = age) %>%
    group_by(agency, psnuuid, psnu, sex, age_group, indicator) %>%
    summarise(across(value, sum, na.rm = TRUE)) %>%
    ungroup() %>%
    rename(age = age_group)



  # USAID TX_CURR from MSD FY20Q4i
  df_tx_curr <- df_msd %>%
    filter(
      fundingagency == "USAID",
      fiscal_year == 2020,
      indicator == 'TX_CURR',
      standardizeddisaggregate == 'Age/Sex/HIVStatus',
      trendscoarse %in% c("<15", "15+")
    ) %>%
    dplyr::select(psnuuid, psnu, sex, trendscoarse, qtr4) %>%
    group_by(psnuuid, psnu, sex, trendscoarse) %>%
    summarise(across(qtr4, sum, na.rm = TRUE)) %>%
    ungroup() %>%
    rename(age = trendscoarse)

  # Replace DHIS2 TX_CURR with MSD Values
  df_cascade <- df_cascade %>%
    rename(shortname = psnu) %>%
    filter(agency == "USAID") %>%
    pivot_wider(names_from = indicator, values_from = value) %>%
    dplyr::left_join(df_tx_curr, by = c("psnuuid", "sex", "age")) %>%
    mutate(
      tx_curr = case_when(
        is.na(qtr4) ~ tx_curr,
        TRUE ~ qtr4
      )
    ) %>%
    dplyr::select(-qtr4) %>%
    relocate(psnu, .after = psnuuid) %>%
    gather(indicator, value, diagnosed:vl_suppressed)

  # PLHIV
  df_plhiv <- file_cascade %>%
    read_excel(sheet = 3, guess_max = Inf) %>%
    clean_names() %>%
    dplyr::select(psnuuid = psnu_id, sex, age, plhiv_estimate) %>%
    mutate(
      age_group = case_when(
        age %in% c("<1", "1-4", "5-9","10-14") ~ "<15",
        TRUE ~ "15+"
      )
    ) %>%
    group_by(psnuuid, sex, age_group) %>%
    summarise(across(plhiv_estimate, sum, na.rm = TRUE)) %>%
    ungroup() %>%
    rename(age = age_group) %>%
    mutate(plhiv = as.integer(round(plhiv_estimate)))


  # Add PLHIV to the Cascade
  df_cascade_prop <- df_cascade %>%
    mutate(indicator = paste0(indicator, "_results")) %>%
    spread(indicator, value) %>%
    left_join(df_plhiv, by = c("psnuuid", "sex", "age"))


  df_cascade_prop <- df_cascade_prop %>%
    mutate(
      diagnosed_cov = diagnosed_results / plhiv_estimate,
      tx_curr_cov = tx_curr_results / plhiv_estimate,
      vl_suppressed_cov = vl_suppressed_results / plhiv_estimate
    ) %>%
    gather(indicator, value,
           starts_with("plhiv"),
           ends_with(c("results", "cov"))) %>%
    mutate(
      valtype = case_when(
        str_detect(indicator, "_results") ~ "results",
        str_detect(indicator, "_cov") ~ "coverage",
        TRUE ~ NA_character_
      ),
      indicator = str_remove(indicator, "_results$|_cov$"),
      indicator = case_when(
        str_detect(indicator, "vl_") ~ "VL Suppressed",
        str_detect(indicator, "diag") ~ "Diagnosed",
        str_detect(indicator, "_estimate") ~ "PLHIV Estimate",
        TRUE ~ str_to_upper(indicator)
      )
    ) %>%
    relocate(valtype, .after = indicator)

  df_cascade_prop <- df_cascade_prop %>%
    mutate(
      chld_group = case_when(
        valtype == "coverage" & indicator == "Diagnosed" & age == "<15" ~ "% Children <15 diagnosed",
        valtype == "coverage" & indicator == "Diagnosed" & sex == "Female" & age == "15+" ~ "% Female 15+ diagnosed",
        valtype == "coverage" & indicator == "Diagnosed" & sex == "Male" & age == "15+" ~ "% Male 15+ diagnosed",
        TRUE ~ "other"
      )
    )

  spdf_cascade_prop <- spdf %>%
    left_join(df_cascade_prop %>%
                filter(valtype == "coverage", chld_group != "other") %>%
                spread(indicator, value),
              by = c("uid" = "psnuuid")) %>%
    filter(!is.na(Diagnosed))

  spdf_plhiv <- spdf %>%
    left_join(df_cascade_prop %>%
                filter(is.na(valtype)) %>%
                spread(indicator, value),
              by = c("uid" = "psnuuid")) %>%
    filter(!is.na(PLHIV))



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



  # PLHIV
  #plcols <- c("red", "orange", "green")
  plcols <- c(USAID_dkred, "orange", "#4EA36B")

  basemap +
    geom_sf(data = spdf_cascade_prop,
          aes(fill = Diagnosed),
          lwd = .3, linetype = "dotted",
          color = grey10k) +
    geom_sf(data = adm0, fill = NA) +
    scale_fill_stepsn(
      breaks = c(0, .6, .8, 1),
      guide = guide_colorsteps(even.steps = FALSE),
      na.value = grey40k,
      limits = c(0, 1),
      labels = percent,
      colors = plcols) +
    scale_y_continuous(labels = percent) +
    facet_wrap(~chld_group, nrow = 1) +

    si_style_map() +
    theme(
      legend.position =  "bottom",
      legend.direction = "horizontal",
      legend.key.width = ggplot2::unit(1.5, "cm"),
      legend.key.height = ggplot2::unit(.5, "cm")
    )


  ggsave(
    here::here(dir_graphics, "FY20Q4_UGANDA_PLHIV.png"),
    plot = last_plot(),
    scale = 1.2,
    dpi = 400,
    width = 10,
    height = 7,
    units = "in"
  )





