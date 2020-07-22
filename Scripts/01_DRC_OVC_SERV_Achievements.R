## PROJECT:  PEPFAR Geospatial Analytics
## AUTHOR:   B.Kagniniwa, G.Sarfaty | USAID
## LICENSE:  MIT
## PURPOSE:  MOZ - OVC_SERV FY20 Achievements
## Date:     2020-07-13

# LIBRARIES

library(tidyverse)
library(vroom)
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

country = "Democratic Republic of the Congo"

file_psnu_ims <- "PSNU_IM_FY18-21_20200605_v1_1.rds"

file_psnu_txt <- "MER_Structured_Datasets_PSNU_IM_FY18-20_20200626_v2_1_Democratic Republic of the Congo"


# Data

    ## Geo
    drc0 <- get_adm_boundaries("COD", adm_level = 0, geo_path = dir_geo) %>%
        st_as_sf()

    drc1 <- get_adm_boundaries("COD", adm_level = 1, geo_path = dir_geo) %>%
        st_as_sf() %>%
        select(country = name_0, province = name_1)

    drc2 <- get_adm_boundaries("COD", adm_level = 2, geo_path = dir_geo) %>%
        st_as_sf() %>%
        select(country = name_0, province = name_1, district = name_2)


    geo_psnu <- list.files(path = here(dir_geo, "PEPFAR"),
                               pattern = "^DRC_.*_HZLsib_.*.shp$",
                               recursive = TRUE,
                               full.names = TRUE) %>%
        read_sf()


    ## MER PSNU x IMs
    df_psnu <- read_rds(here(dir_merdata, file_psnu_ims))

    df_psnu %>% glimpse()

    df_psnu %>%
        distinct(fiscal_year) %>%
        pull()

    df_psnu %>%
        filter(str_detect(indicator, "OVC_")) %>%
        distinct(indicator)

    df_psnu <- df_psnu %>%
        filter(
            fiscal_year %in% c("2020", "2021"),
            fundingagency == "USAID",
            operatingunit == country,
            indicator == 'OVC_SERV_UNDER_18'
        )

    ## OVC Achievements

    df_psnu_targets <- df_psnu %>%
        select(fiscal_year, snu1, psnu, psnuuid, mech_code, targets) %>%
        spread(fiscal_year, targets) %>%
        rename(target20 = `2020`, target21 = `2021`)

    df_psnu_targets %>% glimpse()

    colors_ach <- c("#d7191c", "#dfc27d", "#008837")

    df_psnu_ovc <- df_psnu %>%
        select(snu1, psnu, psnuuid, primepartner, mech_code, mech_name, cumulative) %>%
        left_join(df_psnu_targets, by = c("snu1", "psnu", "psnuuid", "mech_code")) %>%
        mutate(
            ovc_serv_20 = round(cumulative / target20 * 100),
            #ovc_serv_20b = cut(ovc_serv_20, breaks = c(-Inf, 80, 90, Inf), labels = c("Below", "Good", "Above")),
            ovc_serv_20b = cut(ovc_serv_20, breaks = c(-Inf, 80, 90, Inf), labels = c("<80%", "80-90%", "+90%")),
            ovc_serv_20c = case_when(
                ovc_serv_20b == "Below" ~ "#d7191c", #brown-ish
                ovc_serv_20b == "Good" ~ "#dfc27d", #yellow-ish
                ovc_serv_20b == "Above" ~ "#008837", #green-ish
                TRUE ~ grey10k
            ),
            ovc_serv_21 = round(cumulative / target21 * 100),
            ovc_serv_21b = cut(ovc_serv_20, breaks = c(-Inf, 80, 90, Inf), labels = c("Below", "Good", "Above")),
        )

    df_psnu_ovc %>%
        arrange(desc(cumulative)) %>% View()

    drc_ovc <- geo_psnu %>%
        left_join(df_psnu_ovc, by = c("uid" = "psnuuid")) %>%
        filter(!is.na(ovc_serv_20b))

    drc_ovc_r <- geo_psnu %>%
        left_join(df_psnu_ovc, by = c("uid" = "psnuuid")) %>%
        filter(!is.na(cumulative))

    drc_ovc_t <- geo_psnu %>%
        left_join(df_psnu_ovc, by = c("uid" = "psnuuid")) %>%
        filter(!is.na(target20))

    # No targets
    drc_ovc_no_t <- geo_psnu %>%
        left_join(df_psnu_ovc, by = c("uid" = "psnuuid")) %>%
        filter(!is.na(cumulative) & is.na(target20))

    # No results
    drc_ovc_no_r <- geo_psnu %>%
        left_join(df_psnu_ovc, by = c("uid" = "psnuuid")) %>%
        filter(is.na(cumulative) & !is.na(target20))

    # No results or targets
    drc_ovc_no_rt <- geo_psnu %>%
        left_join(df_psnu_ovc, by = c("uid" = "psnuuid")) %>%
        filter( (!is.na(cumulative) & is.na(target20)), (is.na(cumulative) & !is.na(target20)))

    # Achievement

    drc_ach <- ggplot(data = drc_ovc, aes(reorder(psnu, ovc_serv_20), ovc_serv_20, fill = ovc_serv_20b)) +
        geom_col(show.legend = F) +
        geom_hline(yintercept = 100, lwd = .2, color = grey10k) +
        geom_hline(yintercept = 200, lwd = .2, color = grey10k) +
        geom_hline(yintercept = 300, lwd = .2, color = grey10k) +
        scale_fill_manual(values = colors_ach) +
        coord_flip() +
        labs(x = "", y="") +
        si_style_nolines()


    drc_ach_m <- terrain_map(countries = country, terr_path = dir_terr, mask = TRUE) +
        geom_sf(data = drc_ovc_no_t, fill = grey30k, lwd = .2, color = grey10k) +
        geom_sf(data = drc_ovc, aes(fill = ovc_serv_20b), lwd = .1, color = grey10k) +
        geom_sf(data = drc1, fill = NA, lwd = .2, color = grey30k) +
        geom_sf_text(data = drc1, aes(label = province), color = grey80k, size = 3) +
        scale_fill_manual(values = colors_ach, na.value = NA) +
        coord_sf() +
        si_style_map()

    (drc_ach_m + drc_ach) +
        plot_layout(widths = c(1,1)) +
        plot_annotation(
            title = "DRC - OVC_SERV_UNDER_18 Achievement",
            caption = "OHA/SIEI - DATIM FY20 Q2 Data as of 20200605"
        )

    ggsave(here(dir_graphs, "DRC-OVC_SERV_UNDER_18-Achievement.png"),
           scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")

    # FY20 Results

    drc_res <- ggplot(data = drc_ovc, aes(reorder(psnu, cumulative), cumulative, fill = cumulative)) +
        geom_col(show.legend = F) +
        scale_fill_viridis_c(direction = -1, na.value = NA, labels = number_format(scale = 1/1000, suffix = "K")) +
        coord_flip() +
        labs(x = "", y="") +
        si_style_nolines() +
        theme(panel.grid.major.x = element_line(size = .2, color = grey10k))

    drc_res_m <- terrain_map(countries = country, terr_path = dir_terr, mask = TRUE) +
        geom_sf(data = drc_ovc_no_r, fill = grey30k, lwd = .2, color = grey10k) +
        geom_sf(data = drc_ovc, aes(fill = cumulative), lwd = .1, color = grey10k) +
        geom_sf(data = drc1, fill = NA, lwd = .2, color = grey10k) +
        geom_sf_text(data = drc1, aes(label = province), color = grey80k, size = 3) +
        scale_fill_viridis_c(direction = -1, na.value = NA, labels = number_format(scale = 1/1000, suffix = "K")) +
        si_style_map() +
        theme(
            legend.position =  c(.2, .2),
            legend.direction = "vertical",
            legend.key.width = ggplot2::unit(.5, "cm"),
            legend.key.height = ggplot2::unit(1, "cm")
        )

    (drc_res_m + drc_res) +
        plot_layout(widths = c(1,1)) +
        plot_annotation(
            title = "DRC - OVC_SERV_UNDER_18 Results",
            caption = "OHA/SIEI - DATIM FY20 Q2 Data as of 20200605"
        )

    ggsave(here(dir_graphs, "DRC-OVC_SERV_UNDER_18-Results.png"),
           scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")

    # Target FY20

    drc_trg <- drc_ovc %>%
        mutate(label = paste0(psnu, " [", round(target20 / 1000, 1), "K]")) %>%
        ggplot(aes(reorder(label, target20), target20, fill = target20)) +
        geom_col(show.legend = F) +
        scale_fill_gradient2(
            low = "yellow",
            high = "brown"
        ) +
        coord_flip() +
        labs(x = "", y="") +
        si_style_nolines() +
        theme(panel.grid.major.x = element_line(size = .2, color = grey10k))

    drc_trg_m <- terrain_map(countries = country, terr_path = dir_terr, mask = TRUE) +
        geom_sf(data = drc_ovc_no_t, fill = grey30k, lwd = .2, color = grey10k) +
        geom_sf(data = drc_ovc, aes(fill = target20), lwd = .1, color = grey10k) +
        geom_sf(data = drc1, fill = NA, lwd = .2, color = grey10k) +
        geom_sf_text(data = drc1, aes(label = province), color = grey80k, size = 3) +
        scale_fill_gradient2(
            low = "yellow",
            high = "brown"
        ) +
        si_style_map() +
        theme(
            legend.position =  c(.2, .2),
            legend.direction = "vertical",
            legend.key.width = ggplot2::unit(.5, "cm"),
            legend.key.height = ggplot2::unit(1, "cm")
        )

    (drc_trg_m + drc_trg) +
        plot_layout(widths = c(1,1)) +
        plot_annotation(
            title = "DRC - OVC_SERV_UNDER_18 Targets",
            subtitle = "Districts hightlighted in grey have no Targets",
            caption = "OHA/SIEI - DATIM FY20 Q2 Data as of 20200605"
        )

    ggsave(here(dir_graphs, "DRC-OVC_SERV_UNDER_18-Targets.png"),
           scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")

    #Target COP20

    drc_cop20 <- geo_psnu %>%
        left_join(df_psnu_targets, by = c("uid" = "psnuuid")) %>%
        filter(!is.na(target21)) %>%
        mutate(label = paste0(psnu, " [", round(target21 / 1000, 1), "K]"))

    drc_trg21 <- drc_cop20 %>%
        ggplot(aes(reorder(label, target21), target21, fill = target21)) +
        geom_col(show.legend = F) +
        scale_fill_gradient2(
            low = "yellow",
            high = "brown"
        ) +
        coord_flip() +
        labs(x = "", y="") +
        si_style_nolines() +
        theme(panel.grid.major.x = element_line(size = .2, color = grey10k))

    drc_trg21_m <- terrain_map(countries = country, terr_path = dir_terr, mask = TRUE) +
        geom_sf(data = drc_cop20, aes(fill = target21), lwd = .1, color = grey10k) +
        geom_sf(data = drc1, fill = NA, lwd = .2, color = grey10k) +
        geom_sf_text(data = drc1, aes(label = province), color = grey80k, size = 3) +
        scale_fill_gradient2(
            low = "yellow",
            high = "brown") +
        si_style_map() +
        theme(
            legend.position =  c(.2, .2),
            legend.direction = "vertical",
            legend.key.width = ggplot2::unit(.5, "cm"),
            legend.key.height = ggplot2::unit(1, "cm")
        )

    (drc_trg21_m + drc_trg21) +
        plot_layout(widths = c(1,1)) +
        plot_annotation(
            title = "DRC - OVC_SERV_UNDER_18 COP20",
            caption = "OHA/SIEI - DATIM FY20 Q2 Data as of 20200605"
        )


    ggsave(here(dir_graphs, "DRC-OVC_SERV_UNDER_18-COP20.png"),
           scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")




