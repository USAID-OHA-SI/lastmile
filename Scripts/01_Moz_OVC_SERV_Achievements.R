## PROJECT:  PEPFAR Geospatial Analytics
## AUTHOR:   B.Kagniniwa, G.Sarfaty | USAID
## LICENSE:  MIT
## PURPOSE:  MOZ - OVC_SERV FY20 Achievements
## Date:     2020-07-13

# LIBRARIES

library(tidyverse)
library(ggplot2)
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

    country = "Mozambique"

    # files
    file_ovc <- "OVC DREAMS YCM Districts_Sites for COP20.xlsx"
    file_psnu_im1 <- "MER_Structured_Datasets_PSNU_IM_FY18-20_20200605_v1_1.rds"
    file_psnu_im2 <- "PSNU_IM_FY18-21_20200605_v1_1.rds"

    file_psnu_im3 <- "PSNU_IM_FY18-20_Global_20200626_v1_1.rds"

    file_psnu_txt3 <- "Genie_PSNU_IM_Global_Frozen_8cdac4af-9175-4859-bf88-a35e76be063c.txt"

    # read_tsv(here(dir_mer, file_psnu_txt3))%>%
    #     write_rds(path = here(dir_mer, file_psnu_im3), compress = "gz")

    file_districts <- "Mozambique_PROD_5_District_DistrictLsib_2020_Feb.shp"

# Data

    ## Geo
    moz0 <- get_adm_boundaries("MOZ", adm_level = 0, geo_path = dir_geo) %>%
        st_as_sf()

    moz1 <- get_adm_boundaries("MOZ", adm_level = 1, geo_path = dir_geo) %>%
        st_as_sf() %>%
        select(country = name_0, province = name_1)

    moz2 <- get_adm_boundaries("MOZ", adm_level = 2, geo_path = dir_geo) %>%
        st_as_sf() %>%
        select(country = name_0, province = name_1, district = name_2)


    moz_districts <- (list.files(path = here(dir_geo, "PEPFAR"),
                               pattern = file_districts,
                               recursive = TRUE,
                               full.names = TRUE) %>%
        map(read_sf))[[1]]

    moz_districts <- moz_districts %>%
        select(uid, country = level3name, province = level4name, district = level5name) %>%
        mutate(
            district = case_when(
                district == "Chonguene" ~ "Chongoene",
                district == "Cidade De Xai-Xai" ~ "Xai-Xai",
                district == "Cidade De Chimoio" ~ "Chimoio",
                district == "ManhiÒ«a" ~ "Manhica",
                district == "Cidade Da Matola" ~ "Matola",
                district == "Cidade Da Beira" ~ "Beira",
                district == "Maganja Da Costa" ~ "Maganja da Costa",
                district == "Cidade De Quelimane" ~ "Quelimane",
                TRUE ~ district
            )
        )

    moz_districts %>%
        st_as_sf() %>%
        st_set_geometry(NULL) %>%
        arrange(province, district) %>% glimpse()


    ## MER PSNU x IMs
    df_psnu <- read_rds(here(dir_mer, file_psnu_im2))

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
            operatingunit == 'Mozambique',
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

    moz_ovc <- moz_districts %>%
        left_join(df_psnu_ovc, by = c("uid" = "psnuuid")) %>%
        filter(!is.na(ovc_serv_20b))

    moz_ovc_r <- moz_districts %>%
        left_join(df_psnu_ovc, by = c("uid" = "psnuuid")) %>%
        filter(!is.na(cumulative))

    moz_ovc_t <- moz_districts %>%
        left_join(df_psnu_ovc, by = c("uid" = "psnuuid")) %>%
        filter(!is.na(target20))

    # No targets
    moz_ovc_no_t <- moz_districts %>%
        left_join(df_psnu_ovc, by = c("uid" = "psnuuid")) %>%
        filter(!is.na(cumulative) & is.na(target20))

    # No results
    moz_ovc_no_r <- moz_districts %>%
        left_join(df_psnu_ovc, by = c("uid" = "psnuuid")) %>%
        filter(is.na(cumulative) & !is.na(target20))

    # No results or targets
    moz_ovc_no_rt <- moz_districts %>%
        left_join(df_psnu_ovc, by = c("uid" = "psnuuid")) %>%
        filter( (!is.na(cumulative) & is.na(target20)), (is.na(cumulative) & !is.na(target20)))

    # Achievement

    moz_ach <- ggplot(data = moz_ovc, aes(reorder(psnu, ovc_serv_20), ovc_serv_20, fill = ovc_serv_20b)) +
        geom_col(show.legend = F) +
        geom_hline(yintercept = 100, lwd = .2, color = grey10k) +
        geom_hline(yintercept = 200, lwd = .2, color = grey10k) +
        geom_hline(yintercept = 300, lwd = .2, color = grey10k) +
        scale_fill_manual(values = colors_ach) +
        coord_flip() +
        labs(x = "", y="") +
        si_style_nolines()


    moz_ach_m <- terrain_map(countries = country, terr_path = dir_terr, mask = TRUE) +
        geom_sf(data = moz_ovc_no_t, fill = grey30k, lwd = .2, color = grey10k) +
        geom_sf(data = moz_ovc, aes(fill = ovc_serv_20b), lwd = .1, color = grey10k) +
        geom_sf(data = moz1, fill = NA, lwd = .2, color = grey30k) +
        geom_sf_text(data = moz1, aes(label = province), color = grey80k, size = 3) +
        scale_fill_manual(values = colors_ach, na.value = NA) +
        coord_sf() +
        si_style_map()

    (moz_ach_m + moz_ach) +
        plot_layout(widths = c(1,1)) +
        plot_annotation(
            title = "MOZAMBIQUE - OVC_SERV_UNDER_18 Achievement",
            subtitle = "Districts hightlighted in grey have Q2 Results but are missing Targets",
            caption = "OHA/SIEI - DATIM FY20 Q2 Data as of 20200605"
        )

    ggsave(here(dir_graphs, "MOZ-OVC_SERV_UNDER_18-Achievement.png"),
           scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")

    # FY20 Results

    moz_res <- ggplot(data = moz_ovc, aes(reorder(psnu, cumulative), cumulative, fill = cumulative)) +
        geom_col(show.legend = F) +
        geom_hline(yintercept = 10000, lwd = .2, color = grey10k) +
        geom_hline(yintercept = 20000, lwd = .2, color = grey10k) +
        geom_hline(yintercept = 30000, lwd = .2, color = grey10k) +
        scale_fill_viridis_c(direction = -1, na.value = NA, labels = number_format(scale = 1/1000, suffix = "K")) +
        coord_flip() +
        labs(x = "", y="") +
        si_style_nolines()

    moz_res_m <- terrain_map(countries = country, terr_path = dir_terr, mask = TRUE) +
        geom_sf(data = moz_ovc_no_r, fill = grey30k, lwd = .2, color = grey10k) +
        geom_sf(data = moz_ovc, aes(fill = cumulative), lwd = .1, color = grey10k) +
        geom_sf(data = moz1, fill = NA, lwd = .2, color = grey10k) +
        geom_sf_text(data = moz1, aes(label = province), color = grey80k, size = 3) +
        scale_fill_viridis_c(direction = -1, na.value = NA, labels = number_format(scale = 1/1000, suffix = "K")) +
        si_style_map() +
        theme(
            legend.position =  c(.9, .2),
            legend.direction = "vertical",
            legend.key.width = ggplot2::unit(.5, "cm"),
            legend.key.height = ggplot2::unit(1, "cm")
        )

    (moz_res_m + moz_res) +
        plot_layout(widths = c(1,1)) +
        plot_annotation(
            title = "MOZAMBIQUE - OVC_SERV_UNDER_18 Results",
            subtitle = "Districts hightlighted in grey have no Q2 Results",
            caption = "OHA/SIEI - DATIM FY20 Q2 Data as of 20200605"
        )

    ggsave(here(dir_graphs, "MOZ-OVC_SERV_UNDER_18-Results.png"),
           scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")

    # Target FY20

    moz_trg <- moz_ovc %>%
        mutate(label = paste0(psnu, " [", round(target20 / 1000, 1), "K]")) %>%
        ggplot(aes(reorder(label, target20), target20, fill = target20)) +
        geom_col(show.legend = F) +
        geom_hline(yintercept = 10000, lwd = .2, color = grey10k) +
        geom_hline(yintercept = 20000, lwd = .2, color = grey10k) +
        geom_hline(yintercept = 30000, lwd = .2, color = grey10k) +
        geom_hline(yintercept = 40000, lwd = .2, color = grey10k) +
        geom_hline(yintercept = 50000, lwd = .2, color = grey10k) +
        scale_fill_gradient2(
            low = "yellow",
            high = "brown"
            #,midpoint = median(moz_ovc$target20), na.value = NA, labels = number_format(scale = 1/1000, suffix = "K")
        ) +
        coord_flip() +
        labs(x = "", y="") +
        si_style_nolines()

    moz_trg_m <- terrain_map(countries = country, terr_path = dir_terr, mask = TRUE) +
        geom_sf(data = moz_ovc_no_t, fill = grey30k, lwd = .2, color = grey10k) +
        geom_sf(data = moz_ovc, aes(fill = target20), lwd = .1, color = grey10k) +
        geom_sf(data = moz1, fill = NA, lwd = .2, color = grey10k) +
        geom_sf_text(data = moz1, aes(label = province), color = grey80k, size = 3) +
        scale_fill_gradient2(
            low = "yellow",
            high = "brown"
            #,midpoint = median(moz_ovc$target20), na.value = NA, labels = number_format(scale = 1/1000, suffix = "K")
        ) +
        si_style_map() +
        theme(
            legend.position =  c(.9, .2),
            legend.direction = "vertical",
            legend.key.width = ggplot2::unit(.5, "cm"),
            legend.key.height = ggplot2::unit(1, "cm")
        )

    (moz_trg_m + moz_trg) +
        plot_layout(widths = c(1,1)) +
        plot_annotation(
            title = "MOZAMBIQUE - OVC_SERV_UNDER_18 Targets",
            subtitle = "Districts hightlighted in grey have no Targets",
            caption = "OHA/SIEI - DATIM FY20 Q2 Data as of 20200605"
        )

    ggsave(here(dir_graphs, "MOZ-OVC_SERV_UNDER_18-Targets.png"),
           scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")

    #Target COP20

    moz_cop20 <- moz_districts %>%
        left_join(df_psnu_targets, by = c("uid" = "psnuuid")) %>%
        filter(!is.na(target21)) %>% #View()
        mutate(label = paste0(psnu, " [", round(target21 / 1000, 1), "K]"))

    moz_trg21 <- moz_cop20 %>%
        ggplot(aes(reorder(label, target21), target21, fill = target21)) +
        geom_col(show.legend = F) +
        geom_hline(yintercept = 10000, lwd = .2, color = grey10k) +
        geom_hline(yintercept = 20000, lwd = .2, color = grey10k) +
        geom_hline(yintercept = 30000, lwd = .2, color = grey10k) +
        scale_fill_gradient2(
            low = "yellow",
            high = "brown"
            #,midpoint = median(moz_ovc$target20), na.value = NA, labels = number_format(scale = 1/1000, suffix = "K")
        ) +
        coord_flip() +
        labs(x = "", y="") +
        si_style_nolines()

    moz_trg21_m <- terrain_map(countries = country, terr_path = dir_terr, mask = TRUE) +
        geom_sf(data = moz_cop20, aes(fill = target21), lwd = .1, color = grey10k) +
        geom_sf(data = moz1, fill = NA, lwd = .2, color = grey10k) +
        geom_sf_text(data = moz1, aes(label = province), color = grey80k, size = 3) +
        scale_fill_gradient2(
            low = "yellow",
            high = "brown"
            #,midpoint = median(moz_ovc$target20), na.value = NA, labels = number_format(scale = 1/1000, suffix = "K")
        ) +
        si_style_map() +
        theme(
            legend.position =  c(.9, .2),
            legend.direction = "vertical",
            legend.key.width = ggplot2::unit(.5, "cm"),
            legend.key.height = ggplot2::unit(1, "cm")
        )

    (moz_trg21_m + moz_trg21) +
        plot_layout(widths = c(1,1)) +
        plot_annotation(
            title = "MOZAMBIQUE - OVC_SERV_UNDER_18 COP20",
            caption = "OHA/SIEI - DATIM FY20 Q2 Data as of 20200605"
        )


    ggsave(here(dir_graphs, "MOZ-OVC_SERV_UNDER_18-COP20.png"),
           scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")




