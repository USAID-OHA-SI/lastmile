##  PROJECT: Qtr Data Review analysis
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: USAID PEPFAR Footpring
##  LICENCE: MIT
##  DATE:    2022-10-14
##  UPDATED: 2022-10-14

# LIBRARIES ----

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
  library(grabr)
  library(mindthegap)
  library(janitor)
  library(scales)
  library(patchwork)
  library(shadowtext)

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

  ## Notes

  ous <- c("Asia Region", "Vietnam")

  curr_fy <- source_info(file_nat, type = "NAT_SUBNAT", return = "fiscal_year")
  curr_pd <- source_info(file_nat, type = "NAT_SUBNAT", return = "period")
  src_msd <- source_info(file_nat, return = "source")

  footnote <- paste0("USAID / Office of HIV/AIDS - UNAID Estimate as 2021 - Produced on ", curr_date())

# FUNCTIONS ----



# DATA ----

  ## GeoData

  spdf <- ne_countries(type = "sovereignty",
                       scale = 110,
                       returnclass = "sf") %>%
    dplyr::select(sovereignt, admin, name, adm0_a3) %>%
    filter(admin != "Antarctica") %>% # Remove Antarctica
    glamr::clean_countries(colname = "admin")


  spdf <- spdf %>%
    st_transform(crs = st_crs("+proj=robin"))

  ## NAT SUBNAT

  df_nat <- read_msd(file_nat)

  # Distinct Countries
  df_cntries <- df_nat %>%
    distinct(operatingunit, country) %>%
    arrange(operatingunit, country) %>%
    clean_countries(colname = "country")

  df_asia <- df_cntries %>%
    filter(operatingunit %in% ous)

  # PLHIV
  df_plhiv <- df_nat %>%
    filter(fiscal_year %in% c(curr_fy, curr_fy +1),
           operatingunit %in% ous,
           indicator == "PLHIV",
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(fiscal_year, operatingunit, country) %>%
    summarise(across(targets, sum, na.rm = T), .groups = "drop") %>%
    mutate(countryname = country) %>%
    clean_countries(colname = "countryname") %>%
    pivot_wider(names_from = fiscal_year,
                values_from = targets)


  ## UNAIDS

  df_hiv_est <- pull_unaids(data_type = "HIV Estimates", pepfar_only = F)

  df_hiv_est %>% glimpse()

  df_hiv_est <- df_hiv_est %>%
    filter(year == curr_fy-1,
           indicator == "Number PLHIV",
           age == "All",
           sex == "All",
           !is.na(estimate))

  df_hiv_asia <- df_hiv_est %>%
    filter(country %in% df_asia$country)

  # Join MSD to spdf
  spdf_asia <- spdf %>%
    left_join(df_hiv_asia, by = c("admin" = "country")) %>%
    filter(!is.na(estimate))

  locs <- spdf_asia %>%
    st_geometry() %>%
    st_make_valid() %>%
    st_centroid() %>%
    st_coordinates()

  spdf_asia <- spdf_asia %>%
    bind_cols(locs)



## VIZ ----

  ## Global Map - Showing Asia Countries
  viz_map <- ggplot() +
    geom_sf(data = spdf, fill = NA, color = grey50k, size = .4) +
    geom_sf(data = spdf_asia,
            aes(fill = estimate),
            #fill = usaid_red,
            color = grey30k,
            size = .2,
            ) +
    geom_shadowtext(data = spdf_asia,
                    aes(X, Y,
                        label = paste0(str_replace(admin, " ", "\n")
                                       #,"\n", comma(estimate, 1)
                                       )
                        ),
                    size = 2.5, fonttype = "bold",
                    color = usaid_darkgrey,
                    bg.color = grey10k) +
    scale_fill_si(palette = "burnt_siennas", labels = comma) +
    labs(
      x = "", y = "",
      caption = footnote
    ) +
    si_style_map() +
    theme(
      legend.direction = "horizontal",
      legend.position = "bottom",
      legend.justification = "center",
      legend.title = element_blank(),
      legend.key.width = unit(4, "cm"),
      legend.key.height = unit(.5, "cm"),
      plot.title = element_text(face = "bold")
    )

  ggsave(here(dir_graphics, paste0(curr_pd, "_ASIA_PEPFAR_ASIA_Countries - MAP - ", curr_date(), ".png")),
         plot = viz_map,
         scale = 1.2, dpi = 310,
         width = 10, height = 7, units = "in")

  ## Bar plot
  viz_bar <-
    df_hiv_asia %>%
      mutate(
        txt_color = case_when(
          estimate > 200000 ~ grey10k,
          TRUE ~ usaid_darkgrey
        ),
        txt_pos = case_when(
          estimate > 50000 ~ 1.2,
          TRUE ~ -0.1
        )) %>%
      ggplot(aes(reorder(country, estimate), estimate, fill = estimate)) +
        geom_col(width = .70) +
        geom_text(aes(y = 0, label = country), hjust = 0, vjust = -2.5,
                  size = 4, color = usaid_black, fontface = "bold") +
        geom_text(aes(label = comma(estimate), color = txt_color, hjust = txt_pos),
                  size = 4, fontface = "bold") +
        scale_fill_si(palette = "burnt_siennas", labels = comma) +
        scale_color_identity() +
        coord_flip() +
        si_style_void() +
        theme(
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill='transparent', color = NA),
          legend.box.background = element_rect(fill='transparent', color = NA)
        )

  ggsave(here(dir_graphics, paste0(curr_pd, "_ASIA_PEPFAR_ASIA_Countries - BARS - ", curr_date(), ".png")),
         plot = viz_bar,
         scale = 1.2, dpi = 310,
         width = 8, height = 7, units = "in")

