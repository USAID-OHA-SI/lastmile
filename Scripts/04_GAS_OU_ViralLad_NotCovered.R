##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa & G.Sarfaty | USAID
##  PURPOSE: Geo-depiction of VL - % not covered
##  LICENCE: MIT
##  DATE:    2020-09-04

# Libraries
library(tidyverse)
library(ggplot2)
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

    df_psnu %>%
        filter(str_detect(operatingunit, " Region")) %>%
        select(operatingunit, snu1, snu1uid, psnu, psnuuid) %>%
        distinct(operatingunit, snu1, snu1uid, psnu, psnuuid) %>%
        arrange(snu1) %>%
        prinf()

    df_psnu %>%
        filter(!str_detect(operatingunit, " Region")) %>%
        distinct(operatingunit, psnuuid) %>%
        arrange(operatingunit) %>%
        prinf()

    df_cntries <- df_psnu %>%
        distinct(operatingunit, countryname) %>%
        arrange(operatingunit, countryname)

    df_cntries %>% prinf()

    lst_psnu <- df_psnu %>%
        filter(!is.na(psnuuid), psnuuid != "?") %>%
        distinct(psnuuid) %>%
        pull()


    # MER Data Munge
    df_VL<-df %>%
        filter(fiscal_year=="2020",
               fundingagency=="USAID",
               indicator %in% c("TX_PVLS","TX_CURR"),
               standardizeddisaggregate %in% c("Total Numerator","Total Denominator"),
               operatingunit %in% c("Zimbabwe","Mozambique","Lesotho","Kenya")) %>%
        mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>%
        group_by(fiscal_year,operatingunit,psnuuid,psnu,indicator) %>%
        summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
        ungroup() %>%
        reshape_msd(clean = TRUE) %>%
        select(-period_type) %>%
        spread(indicator, val)



    df_VL<-df_VL %>%
        group_by(operatingunit,psnuuid,psnu) %>%
        mutate(VLC = TX_PVLS_D / lag(TX_CURR, 2, order_by = period),
               ou_lab = paste0(operatingunit, " (", lag(TX_CURR, 2, order_by = period) %>% comma(), ")")) %>%
        ungroup() %>%
        mutate(VLS = (TX_PVLS/TX_PVLS_D)*VLC) %>%
        mutate(Not_Cov=case_when(VLC >1 ~ 0,
                                 TRUE ~ 1-VLC)) %>%
        filter(period == "FY20Q3") %>%
        mutate(shortname = case_when(operatingunit=="Kenya" ~ str_remove(psnu, "County"),
                                     TRUE ~ psnu)) %>%
        mutate(lab_psnu = case_when(Not_Cov > .2 ~ shortname))




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

    # Geo OUs
    spdf_ou <- spdf_pepfar %>%
        filter(!uid %in% lst_psnu)

    spdf_ou %>% plot()

    # Geo PSNUs
    spdf_psnu <- spdf_pepfar %>%
        filter(uid %in% lst_psnu)

    spdf_psnu %>% plot()






# VIZ --------------------------------------

moz_map<- terrain_map(countries = "Mozambique", terr_path = dir_terr, mask = TRUE) +
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



