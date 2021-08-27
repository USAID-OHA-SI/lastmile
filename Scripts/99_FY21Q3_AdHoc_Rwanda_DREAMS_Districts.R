##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: OHA/PPIR/GSD Branch request - Rwanda DREAMS Districts map
##  NOTE:    AdHoc request from Gender Advisor
##  LICENCE: MIT
##  DATE:    2021-08-27


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(gophr)
  library(glitr)
  library(glamr)
  library(gisr)
  library(sf)
  library(rnaturalearth)
  library(scales)
  library(ggtext)
  library(patchwork)
  library(glue)
  library(extrafont)


# SETUP ----

  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"

  dir_geodata <- si_path("path_vector")
  dir_terr <- si_path("path_raster")
  dir_merdata <- si_path("path_msd")

  cntry <- "Rwanda"

# FUNCTIONS ----


# DATA ----

  # Dreams data
  df_rwa_dreams <- tibble(
      province = c("Eastern", "Kigali", "Kigali", "Kigali", "Southern", "Southern", "Estern"),
      district = c("Rwamagana", "Gasabo", "Kicukiro", "Nyarugenge", "Nyanza", "Ruhango", "Kayonza"),
      partner = c("FXB/Turengere Abana", "AEE/Ubaka Ejo", "Pact/ACHIEVE (YWCA)", "Pact/ACHIEVE (DUHAMIC ADRI)", "FXB/Turengere Abana", NA, NA)
    ) %>%
    mutate(fiscal_year = case_when(
      is.na(partner) ~ "FY22",
      TRUE ~ "FY21"
    ))

  # Geodata

  terr <- get_raster()
  terr <- get_raster(name = "SR_HR.tif")

  names(terr) <- "SR_LR"


  # NE Geodata
  ne_adm0 <- get_admin0(cntry, scale = "large") %>%
    select(sovereignt, admin, name, iso_a3)

  ne_adm1 <- get_admin1(cntry) %>%
    select(name, type) %>%
    mutate(name = case_when(
      str_detect(name, "City") ~ str_remove(name, " City"),
      TRUE ~ name
    ))


  # GADM Geodata
  rwa_code <- ne_adm0$iso_a3

  gadm_adm0 <- get_adm_boundaries(rwa_code, path = dir_geodata) %>%
    rename(iso3 = gid_0, name = name_0)

  gadm_adm1 <- get_adm_boundaries(rwa_code, adm_level = 1, path = dir_geodata) %>%
    select(iso3 = gid_0, countryname = name_0, name = varname_1,
           localname = name_1, type = type_1, id = cc_1, code = hasc_1) %>%
    separate(name, into = c("name", "name_fr"), sep = "\\|") %>%
    mutate(
      name = case_when(
        str_detect(name, "Province") ~ str_remove(name, " Province"),
        str_detect(name, "City") ~ str_remove(name, " City"),
        TRUE ~ name
      ),
      name = str_to_upper(str_remove(name, "ern")),
      y_nudge = case_when(
        name == 'SOUTH' ~ -.5,
        name == "EAST" ~ +.5,
        TRUE ~ 0
      ))

  gadm_adm2 <- get_adm_boundaries(rwa_code, adm_level = 2, path = dir_geodata) %>%
    select(iso3 = gid_0, countryname = name_0, localprovince = name_1,
           name = name_2, type = type_2, id = cc_2, code = hasc_2) %>%
    mutate(prov_id = str_sub(id, 1, 1),
           prov_code = str_sub(code, 1, 5))

  spdf_dreams <- gadm_adm2 %>%
    left_join(df_rwa_dreams, by = c("name" = "district")) %>%
    filter(!is.na(fiscal_year))

  spdf_dreams %>%
    attributes() %>%
    prinf()

# VIZ ----

  # Assign colors
  spdf_dreams <- spdf_dreams %>%
    mutate(
      fill_color = case_when(
        fiscal_year == "FY21" ~ usaid_blue,
        TRUE ~ usaid_medgrey
      ),
      text_color = case_when(
        fiscal_year == "FY21" ~ grey10k,
        TRUE ~ color_plot_text
      ),
      partner_color = case_when(
        partner == "AEE/Ubaka Ejo" ~ denim,
        partner == "FXB/Turengere Abana" ~ moody_blue,
        partner == "Pact/ACHIEVE (DUHAMIC ADRI)" ~ burnt_sienna,
        partner == "Pact/ACHIEVE (YWCA)" ~ genoa,
        TRUE ~ usaid_medgrey
      ))

  # Basemap
  basemap <- terrain_map2(countries = cntry,
                         adm0 = gadm_adm0,
                         adm1 = gadm_adm1,
                         terr = terr,
                         mask = TRUE)

  # Implementation Areas
  program_map <- ggplot() +
  #basemap +
    geom_sf(data = gadm_adm0,
            fill = grey10k,
            size = 0,
            alpha = .3,
            color = grey10k) +
    geom_sf(data = gadm_adm2,
            fill = NA,
            size = .3,
            color = grey30k) +
    geom_sf(data = spdf_dreams,
            aes(fill = fill_color),
            size = .3,
            color = grey30k) +
    geom_sf(data = gadm_adm1,
            fill = NA,
            size = .8,
            color = grey60k) +
    geom_sf(data = gadm_adm0,
            fill = NA,
            size = 2,
            color = grey20k) +
    geom_sf(data = gadm_adm0,
            fill = NA,
            size = 1,
            color = grey60k) +
    geom_sf_text(data = gadm_adm1 %>% filter(name %in% c('NORTH', 'WEST')),
                 aes(label = name),
                 color = grey40k,
                 size = 8) +
    geom_sf_text(data = gadm_adm1 %>% filter(name == 'KIGALI'),
                 aes(label = name),
                 color = grey40k,
                 size = 8,
                 nudge_y = .3) +
    geom_sf_text(data = gadm_adm1 %>% filter(name == 'EAST'),
                 aes(label = name),
                 color = grey40k,
                 size = 8,
                 nudge_y = .3) +
    geom_sf_text(data = gadm_adm1 %>% filter(name == 'SOUTH'),
                 aes(label = name),
                 color = grey40k,
                 size = 8,
                 nudge_y = -.3) +
    geom_sf_text(data = spdf_dreams,
                 aes(label = name, color = text_color),
                 size = 4) +
    scale_fill_identity() +
    scale_color_identity() +
    labs(title = "RWANDA - USAID IMPLEMENTATION OF DREAMS PROGRAM",
         subtitle = "Program is being implemented in <span style='color:#002a6c'>**5**</span> Districts for <span style='color:#002a6c'>**FY21**</span> and <span style='color:#8C8985'>**2**</span> more will be added in <span style='color:#8C8985'>**FY22**</span>",
         caption = glue("Source: PEPFAR/Rwanda - COP21 Data<br/>OHA/SIEI/SI/Core Analytics, {format(Sys.Date(), '%Y-%m-%d')}")) +
    si_style_map()

  ggsave(paste0(dir_graphics, "/FY21Q3 - RWANDA USAID Implimentation of DREAMS Program - Districts - ",
                format(Sys.Date(), "%Y-%m-%d"),
                ".png"),
         plot = program_map,
         scale = 1.5,
         width = 10, height = 5.625, dpi = 320)

  # Implementation Areas
  partners_map <-
    ggplot() +
    #basemap +
    geom_sf(data = gadm_adm0,
            fill = grey10k,
            size = 0,
            alpha = .3,
            color = grey10k) +
    geom_sf(data = gadm_adm2,
            fill = NA,
            size = .3,
            color = grey30k) +
    geom_sf(data = spdf_dreams %>% filter(!is.na(partner)),
            aes(fill = partner),
            size = .3,
            color = grey30k) +
    geom_sf(data = gadm_adm1,
            fill = NA,
            size = .8,
            color = grey60k) +
    geom_sf(data = gadm_adm0,
            fill = NA,
            size = 2,
            color = grey20k) +
    geom_sf(data = gadm_adm0,
            fill = NA,
            size = 1,
            color = grey60k) +
    geom_sf_text(data = gadm_adm1 %>% filter(name %in% c('NORTH', 'WEST')),
                 aes(label = name),
                 color = grey40k,
                 size = 8) +
    geom_sf_text(data = gadm_adm1 %>% filter(name == 'KIGALI'),
                 aes(label = name),
                 color = grey40k,
                 size = 5) +
    geom_sf_text(data = gadm_adm1 %>% filter(name == 'EAST'),
                 aes(label = name),
                 color = grey40k,
                 size = 8,
                 nudge_y = .3) +
    geom_sf_text(data = gadm_adm1 %>% filter(name == 'SOUTH'),
                 aes(label = name),
                 color = grey40k,
                 size = 8,
                 nudge_y = -.3) +
    geom_sf_text(data = spdf_dreams,
                 aes(label = name, color = text_color),
                 size = 4) +
    scale_fill_si(palette = "siei", discrete = TRUE) +
    #scale_fill_identity() +
    scale_color_identity() +
    labs(title = "RWANDA - USAID IMPLEMENTATION OF DREAMS PROGRAM",
         subtitle = "**AEE** and **Pact** are all in Kigali and **FXB** is in Rwamagana (East) and Nyanza (South)",
         caption = glue("Source: PEPFAR/Rwanda - COP21 Data<br/>OHA/SIEI/SI/Core Analytics, {format(Sys.Date(), '%Y-%m-%d')}")) +
    si_style_map() +
    theme(legend.position = "top", legend.justification = "left")

    ggsave(paste0(dir_graphics, "/FY21Q3 - RWANDA USAID Implimentation of DREAMS Program - Partners - ",
                  format(Sys.Date(), "%Y-%m-%d"),
                  ".png"),
           plot = partners_map,
           scale = 1.5,
           width = 10, height = 5.625, dpi = 320)



    # partner_color = case_when(
    #   partner == "AEE/Ubaka Ejo" ~ denim,
    #   partner == "FXB/Turengere Abana" ~ moody_blue,
    #   partner == "Pact/ACHIEVE (DUHAMIC ADRI)" ~ burnt_sienna,
    #   partner == "Pact/ACHIEVE (YWCA)" ~ genoa,
    #   TRUE ~ usaid_medgrey
    # ))