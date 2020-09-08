##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa & G.Sarfaty | USAID
##  PURPOSE: Geo-depiction of VL - % not covered
##  LICENCE: MIT
##  DATE:    2020-09-04

# Libraries
library(tidyverse)
library(readxl)
library(vroom)
library(sf)
library(gisr)
library(glitr)
library(glamr)
library(janitor)
library(scales)
library(patchwork)
library(ggrepel)
library(here)
library(ICPIutilities)
library(googlesheets4)
library(extrafont)

# REQUIRED -------------------------------------------------------------------------------

    ## Get Credentials

    source("./_secrets/credentials.R")

# GLOBALS -------------------------------------------------------------

    ## Data & Output folders

    dir_data <- "Data"
    dir_dataout <- "Dataout"
    dir_gis <- "GIS"
    dir_graphics <- "Graphics"
    dir_geodata <- "../../GEODATA/PEPFAR"
    dir_geo <- "../../GEODATA/PEPFAR"
    dir_terr <- "../../GEODATA/RASTER"
    dir_merdata <- "../../MERDATA"

    ## Q3 MER Data

    psnu_im <- "^MER_.*_PSNU_IM_.*_20200814_.*.zip$"

    ## Reporting Filters

    rep_agency = "USAID"

    rep_fy = 2020
    rep_qtr = 3

    rep_pd = fy %>%
        as.character() %>%
        str_sub(3,4) %>%
        paste0("FY", ., "Q", qtr)





# DATA --------------------------------------------------------------

    ## File path + name

    file_psnu_im <- list.files(
            path = dir_merdata,
            pattern = psnu_im,
            recursive = TRUE,
            full.names = TRUE
        )

    ## MER PSNU Data

    df_psnu <- vroom(file_psnu_im)

    df_psnu %>% glimpse()


    df_psnu %>%
        distinct(operatingunit) %>%
        arrange(operatingunit) %>%
        prinf()

    df_ous <- df_psnu %>%
        distinct(operatingunit, operatingunituid) %>%
        arrange(operatingunit)

    df_ous %>% prinf()


    df_cntries <- df_psnu %>%
        distinct(operatingunit, countryname) %>%
        arrange(operatingunit, countryname)

    df_cntries %>% prinf()


    df_psnus <- df_psnu %>%
        filter(str_detect(operatingunit, " Region")) %>%
        select(operatingunit, countryname, snu1, snu1uid, psnu, psnuuid) %>%
        distinct(operatingunit, countryname, snu1, snu1uid, psnu, psnuuid) %>%
        arrange(operatingunit, countryname, snu1)

    df_psnus %>% head()


    ## List of Orgs

    lst_psnu <- df_psnus %>%
        filter(!is.na(psnuuid), psnuuid != "?") %>%
        distinct(psnuuid) %>%
        pull()

    lst_snu1 <- df_psnus %>%
        filter(!is.na(snu1uid), psnuuid != "?") %>%
        distinct(snu1uid) %>%
        pull()

    lst_ous <- df_ous %>%
        filter(!str_detect(operatingunit, " Region")) %>%
        pull(operatingunituid)


    ## MER Data Munge

    df_vl <- df_psnu %>%
        filter(fiscal_year == rep_fy,
               fundingagency == rep_agency,
               indicator %in% c("TX_PVLS","TX_CURR"),
               standardizeddisaggregate %in% c("Total Numerator","Total Denominator"),
               operatingunituid %in% lst_ous) %>%
        mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>%
        group_by(fiscal_year, operatingunit, psnuuid, psnu, indicator) %>%
        summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
        ungroup() %>%
        reshape_msd(clean = TRUE) %>%
        select(-period_type) %>%
        spread(indicator, val)



    df_vl <- df_vl %>%
        group_by(operatingunit, psnuuid, psnu) %>%
        mutate(VLC = TX_PVLS_D / lag(TX_CURR, 2, order_by = period)) %>%
        ungroup() %>%
        filter(period == rep_pd) %>%
        mutate(
            VLS = (TX_PVLS / TX_PVLS_D) * VLC,
            VLnC = case_when(
                VLC > 1 ~ 0,
                TRUE ~ 1 - VLC
            ),
            ou_label = paste0(
                operatingunit,
                " (",
                lag(TX_CURR, 2, order_by = period) %>% comma(accuracy = 1),
                ")"
            ),
            psnu_short = case_when(
                str_detect(psnu, " County$") ~  str_remove(psnu, " County$"),
                str_detect(psnu, " District$") ~  str_remove(psnu, " District$"),
                TRUE ~ psnu
            ),
            psnu_label = case_when(
                VLnC > .2 ~ psnu_short,
                TRUE ~ ""
            )
        )



    ## PEPFAR OU/Countries

    ous <- identify_ouuids(username = user, password = mypwd(key)) %>%
        as_tibble() %>%
        filter(type == "OU")

    ous %>% glimpse()

    ous %>% prinf()


    ## Geodata
    spdf_pepfar <- list.files(
            path = dir_geo,
            pattern = "VcPepfarPolygons.shp",
            recursive = T,
            full.names = T
        ) %>%
        read_sf()

    spdf_pepfar %>% plot()

    ## Geo-units

    ## OU
    spdf_pepfar %>%
        filter(uid == "XOivy2uDpMF") %>%
        plot()

    spdf_ou <- spdf_pepfar %>%
        filter(uid %in% lst_ous)

    spdf_ou

    spdf_ou %>% plot()


    ## SNU1
    spdf_snu <- spdf_pepfar %>%
        filter(uid %in% lst_snu1, !uid %in% lst_ous)


    spdf_snu

    spdf_snu %>% plot()


    spdf_psnu <- spdf_pepfar %>%
        filter(uid %in% lst_psnu, !uid %in% lst_ous, !uid %in% lst_snu1)

    spdf_psnu %>% glimpse()

    spdf_psnu %>% plot()


    ## MER GeoData

    spdf_psnu <- spdf_psnu %>%
        left_join(df_vl, by = c("uid" = "psnuuid")) %>%
        filter(!is.na(period) | operatingunit != "South Africa")

    spdf_psnu %>% glimpse()

    spdf_psnu %>%
        st_set_geometry(NULL) %>%
        distinct(operatingunit) %>%
        pull() %>%
        setdiff(df_ous$operatingunit)

# VIZ --------------------------------------


    #' Viz VLC
    #'
    vlc_map <- function(df, country) {

        df_geo <- df %>% filter(operatingunit == {{country}})

        base_map <- terrain_map(countries = {{country}}, terr_path = dir_terr, mask = TRUE)

        print(base_map)

        theme_map <- base_map +
            geom_sf(data = df_geo %>% filter(!is.na(VLnC)), aes(fill = VLnC), lwd = .2, color = grey10k) +
            scale_fill_gradient2(
                low = "yellow",
                high = "brown",
                labels = percent
            )+
            si_style_map() +
            theme(
                legend.position =  c(.9, .2),
                legend.direction = "vertical",
                legend.key.width = ggplot2::unit(.5, "cm"),
                legend.key.height = ggplot2::unit(1, "cm")
            )

        print(theme_map)

        return(them_map)
    }

    vlc_map(spdf_psnu, "Kenya")

###

viz_map <- terrain_map(countries = "Mozambique", terr_path = dir_terr, mask = TRUE) +
    geom_sf(data = mozgeo %>% filter(!is.na(Not_Cov)), aes(fill = Not_Cov), lwd = .2, color = grey10k) +
    geom_sf(data = moz1, fill = NA, lwd = .2, color = grey30k) +
    scale_fill_gradient2(
        low = "yellow",
        high = "brown",
        labels = percent)+
    si_style_map() +
    theme(
        legend.position =  c(.9, .2),
        legend.direction = "vertical",
        legend.key.width = ggplot2::unit(.5, "cm"),
        legend.key.height = ggplot2::unit(1, "cm")
    )

zim_map<-terrain_map(countries = "Zimbabwe", terr_path = dir_terr, mask = TRUE) +
    geom_sf(data = zimgeo %>% filter(!is.na(Not_Cov)), aes(fill = Not_Cov), lwd = .2, color = grey10k) +
    geom_sf(data = zim1, fill = NA, lwd = .2, color = grey30k) +
    scale_fill_gradient2(
        low = "yellow",
        high = "brown",
        labels=percent_format(accuracy = 1))+
    si_style_map() +
    theme(
        legend.position =  "bottom",
        legend.key.width = ggplot2::unit(1, "cm"),
        legend.key.height = ggplot2::unit(.5, "cm")
    )



(moz_map + zim_map) +
    plot_layout(widths = c(1,1)) +
    plot_annotation(
        caption = "Source: FY20Q3i MSD - USAID Only,
VLC = TX_PVLS / TX_CURR (2 periods prior)
Not Covered = 1-VLC")



ggsave(here("Graphics", "FY20Q3_ViralLoad_BottomOUs.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")



