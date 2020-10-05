##  PROJECT: Geospatial Analytics
##  AUTHOR:  B.Kagniniwa, G.Sarfaty, T. Essam | USAID
##  PURPOSE: Proxy OVC Program Coverage of TX_CURR <20, FY21 Targets
##  LICENCE: MIT
##  DATE:    2020-09-21



# Libraries
library(extrafont)
library(tidyverse)
library(sf)
library(glitr)
library(gisr)
library(here)
library(scales)
library(patchwork)
library(ICPIutilities)


# Globals
dir_data <- here("Data")
dir_dataout <- here("Dataout")

dir_geo <- "C:/Users/gsarfaty/Documents/GIS/DATA/Boundaries"
dir_terr <- "C:/Users/gsarfaty/Documents/GIS/DATA/Topography"
dir_merdata <- "C:/Users/gsarfaty/Documents/DATIM"

dir_img <- here("Images")
dir_graphs <- here("Graphics")


# MER Data
file_psnu_im <- (list.files(path =dir_data,
                            pattern = "Genie",
                            recursive = TRUE,
                            full.names = TRUE))

df<-read_msd(file_psnu_im)


# GEO Data
gis_4_sfc <- list.files(dir_geo, pattern = ".*_5_.*.shp$", recursive = T, full.names = T) %>%
    set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
    map(read_sf)

gis_5_sfc <- list.files(dir_geo, pattern = ".*_5_.*.shp$", recursive = T, full.names = T) %>%
  set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
  map(read_sf)

adm1 <- get_adm_boundaries("ZWE", adm_level = 1, geo_path = dir_geo) %>%
  st_as_sf() %>%
  select(country = name_0, province = name_1)



# MER Data Munge ---------------------------------------------------------------------------------

# alignment of OVC_SERV <18 T & TX_CURR <20 T, FY21

ovc_mix<-df %>%
    filter(fiscal_year=="2021",
           indicator =="OVC_SERV_UNDER_18" & standardizeddisaggregate=="Total Numerator" |
               indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus",
           operatingunit %in% c("Zimbabwe")) %>%
    filter(!trendsfine %in% c("20-24","25-29","30-34","35-39","40-49","50+")) %>%
    group_by(fiscal_year,fundingagency,operatingunit,psnuuid,psnu,indicator) %>%
    summarise(across(starts_with("targ"), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    reshape_msd(clean = TRUE) %>%
    select(-period_type) %>%
    group_by(psnuuid) %>%
    count(fundingagency) %>%
    spread(fundingagency,n)

fy21<-df %>%
    filter(fiscal_year=="2021",
           indicator =="OVC_SERV_UNDER_18" & standardizeddisaggregate=="Total Numerator" |
               indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus",
           operatingunit %in% c("Zimbabwe")) %>%
    filter(!trendsfine %in% c("20-24","25-29","30-34","35-39","40-49","50+")) %>%
    group_by(fiscal_year,fundingagency,operatingunit,psnuuid,psnu,indicator) %>%
    summarise(across(starts_with("targ"), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    reshape_msd(clean = TRUE) %>%
    select(-period_type) %>%
    left_join(ovc_mix,by="psnuuid") %>%
    mutate(ovc_group=case_when(
        `HHS/CDC` >0 & USAID >0 ~ "Mixed",
        `HHS/CDC` >1 & is.na(USAID) ~ "CDC Only",
        USAID >1 & is.na(`HHS/CDC`) ~ "USAID Only",
        `HHS/CDC`< 2  & is.na(USAID) ~ "OVC & TX do not overlap in this district",
        USAID < 2  & is.na(`HHS/CDC`) ~ "OVC & TX do not overlap in this district"),
        ovc_group=factor(ovc_group,levels=c("USAID Only", "CDC Only", "Mixed",
                                            "OVC & TX do not overlap in this district")))


# GEO Data Joins
psnu_geo<-st_as_sf(gis_5_sfc$Zimbabwe) %>%
  left_join(fy21, by = c("uid" = "psnuuid"))



# VIZ ------------------------------------------------------------------------------------

# Heatmap
heatmap<-fy21 %>%
    filter(ovc_group=="Mixed") %>%
    mutate(fundingagency = factor(fundingagency,levels = c("USAID", "HHS/CDC"))) %>%
    ggplot(aes(x=fundingagency,y=reorder(psnu,val),fill=fundingagency))+
    geom_tile(colour=grey30k, alpha=0.6)+
    geom_text(aes(label=comma(val)))+
    facet_wrap(~indicator)+
    scale_fill_manual(values=c(USAID_mgrey,USAID_lgrey))+
    xlab(label="")+
    ylab(label="")+
    ggtitle(label="Summary of PSNUs with Mixed Agency Targets")+
    si_style_xline()+
    theme(legend.position = "none",
          plot.title = element_text(family="Gill Sans MT", size=10),
          axis.text = element_text(family="Gill Sans MT"),
          strip.text = element_text(family = "Gill Sans MT", hjust = .5),
          panel.spacing.x=unit(-1, "lines"))



# Map
map<-terrain_map(countries = "Zimbabwe", terr_path = dir_terr, mask = TRUE) +
  geom_sf(data = psnu_geo, aes(fill = ovc_group), lwd = .2, color = grey10k) +
  geom_sf(data = adm1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_manual(values=c("#384F6C","#BA3D56",USAID_dkred,USAID_mgrey,USAID_mgrey))+
  si_style_map()+
  theme(
    legend.direction="vertical",
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm"),
    legend.text.align = 0,
    plot.title = element_text(family="Gill Sans MT", size=10))+
  ggtitle(label = "Agency with OVC_SERV_UNDER_18 \n & TX_CURR <20 Targets in FY21",
          subtitle = "")


mixed_map<-terrain_map(countries = "Zimbabwe", terr_path = dir_terr, mask = TRUE) +
    geom_sf(data = psnu_geo %>% filter(ovc_group=="Mixed"), aes(fill = ovc_group), lwd = .2, color = grey10k) +
    geom_sf(data = adm1, fill = NA, lwd = .2, color = grey30k) +
    scale_fill_manual(values=c(USAID_dkred))+
    si_style_map()+
    theme(
        legend.position =  "bottom",
        legend.key.width = ggplot2::unit(1, "cm"),
        legend.key.height = ggplot2::unit(.5, "cm"),
        plot.title = element_text(family="Gill Sans MT", size=10))+
    ggtitle(label = "Mixed Agency OVC_SERV_UNDER_18 \n & TX_CURR <20 Targets in FY21",
            subtitle = "")

# VIZ COMBINED & SAVED

(map + mixed_map + heatmap)+
  plot_layout(nrow = 1)+
  plot_annotation(
    title="",
    caption = "Source: Genie 2020-09-18")




ggsave(here("Graphics", "Zim_OVC_TX_FY21_overlap_v2.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")


