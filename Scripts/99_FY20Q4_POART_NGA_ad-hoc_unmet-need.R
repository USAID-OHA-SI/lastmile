##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  Aaron Chafetz | USAID
##  PURPOSE: Geo-depiction of unmet need
##  LICENCE: MIT
##  DATE:    2020-12-07



# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(readxl)
  library(janitor)
  library(glitr)
  library(glamr)
  library(gisr)
  library(extrafont)
  library(scales)
  library(ggtext)
  library(sf)
  library(ggrepel)
  library(patchwork)
  library(glue)


# SETUP -------------------------------------------------------------------

  # folder_setup()

# LOAD DATA ---------------------------------------------------------------

  df <- read_excel("Data/EpiData_2020.xlsx")

  usaid_states <- read_excel("Data/EpiData_2020.xlsx", "USAID sites") %>%
    pull(PSNU)

# SPATIAL DATA ------------------------------------------------------------

  nga_0 <- get_admin0("Nigeria")
  nga_1 <- get_admin1("Nigeria")

  nga_1 <- nga_1 %>%
    mutate(state = recode(name,
                          "Federal Capital Territory" = "FCT",
                          "Nassarawa" = "Nasarawa"))

# MUNGE -------------------------------------------------------------------

  df <- clean_names(df)

  df <- tibble(state = unique(nga_1$state)) %>%
    full_join(df, .)

  df <- df %>%
    pivot_longer(starts_with("fy"),
                 names_to = c("period", "indicator"),
                 names_sep = 5,
                 values_to = "value") %>%
    mutate(period = period %>% toupper %>% str_remove("_")) %>%
    pivot_wider(names_from = indicator,
                values_from = value)

  df <- df %>%
    mutate(usaid_state = state %in% usaid_states)

  df_geo <- df %>%
    full_join(nga_1, by = "state")


# MAP ---------------------------------------------------------------------


  v_plhiv <- df_geo %>%
    filter(period == "FY20") %>%
    ggplot() +
    geom_sf(aes(fill = unmet_need, geometry = geometry),
            color = "gray90") +
    scale_fill_si("moody_blues", discrete = FALSE, label = comma, reverse = TRUE, na.value = "grey90") +
    labs(title = "PLHIV") +
    si_style_map()

  v_cov <- df_geo %>%
    mutate(usaid_art_coverage = case_when(usaid_state == TRUE ~ art_coverage),
           usaid_state = case_when(usaid_state == TRUE & period == "FY20" ~ state),
           period = "FY20") %>%
    ggplot() +
    geom_sf(aes(fill = usaid_art_coverage, geometry = geometry),
            color = "gray90", size = .4) +
    geom_text(aes(longitude, latitude, label = usaid_state), colour = "white", na.rm = TRUE,
              family = "Source Sans Pro", size = 2) +
    # facet_wrap(~period, ncol = 1) +
    scale_fill_si(discrete = FALSE, label = percent, reverse = TRUE, na.value = "grey70") +
    si_style_map() +
    theme(strip.text.x = element_text(size = "12", face = "bold"))

  v_dot <- df %>%
    filter(!is.na(art_coverage),
           usaid_state == TRUE) %>%
    mutate(order = ifelse(period == "FY20", art_coverage, 0),
           start = case_when(period == "FY19" ~ art_coverage),
           end = case_when(period == "FY20" ~ art_coverage),
           pd_lab = case_when(state == "Cross River" ~ period)) %>%
    group_by(state) %>%
    mutate(min = case_when(art_coverage == min(art_coverage) ~ art_coverage),
           max = case_when(art_coverage == max(art_coverage) ~ art_coverage)) %>%
    ungroup() %>%
    ggplot(aes(art_coverage, fct_reorder(state, order, sum), group = state)) +
    geom_blank(aes(art_coverage * 1.05)) +
    geom_path() +
    geom_point(aes(start), shape = 21, fill = "white", na.rm = TRUE) +
    geom_text(aes(min, label = glue("{state}  {percent(art_coverage, 1)}")),
              size = 2, hjust = 1.1, family = "Source Sans Pro", na.rm = TRUE) +
    geom_text(aes(max, label = glue("{percent(art_coverage, 1)}")),
              size = 2, hjust = -.5, family = "Source Sans Pro", na.rm = TRUE) +
    geom_text(aes(label = pd_lab),
              size = 1.5, vjust = -1.2, family = "Source Sans Pro", na.rm = TRUE) +
    geom_point(aes(end), size = 2, shape = 21, fill = "gray30", na.rm = TRUE) +
    expand_limits(x = 0, y = (length(usaid_states) + 1)) +
    scale_x_continuous(labels = percent) +
    labs(x = NULL, y = NULL) +
    si_style_void()



  (v_cov + v_dot) +
    # plot_layout(widths = c(1, 2)) +
    plot_annotation(
      subtitle = 'Nigeria ART Coverage Across USAID Supported States | FY20',
      caption = "Notes: ART Coverage = Current on Treatment / PLHIV\nOnly one point estimate for PLHIV, both year's calculations use the same PLHIV value\nSource: NAIIS estimates from Spectrum Projection, January 2020"
      ) & si_style_map() + theme(panel.grid = element_blank(),
                               plot.title = element_text(hjust = 0),
                               plot.subtitle = element_text(hjust = 0),
                               legend.key.width = unit(.5,"in"),
                               legend.key.height = unit(.1,"in"),
                               plot.caption = element_text(hjust = 1)
                               )



  si_save("Images/NGA_art-coverage_v3.png")


