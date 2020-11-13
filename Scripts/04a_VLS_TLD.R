##  PROJECT: Geospatial Analytics
##  AUTHOR:  B.Kagniniwa, G.Sarfaty, T. Essam | USAID
##  PURPOSE: VLS & TLD/TLE Ratio
##  LICENCE: MIT
##  DATE:    2020-10-05




# Libraries
library(tidyverse)
library(sf)
library(glitr)
library(glamr)
library(gisr)
library(here)
library(scales)
library(patchwork)
library(ICPIutilities)
library(extrafont)



# Globals
dir_data <- here("Data")
dir_dataout <- here("Dataout")

dir_geo <- ".../DATA/Boundaries"
dir_terr <- ".../DATA/Topography"
dir_merdata <- ".../DATIM"

dir_img <- here("Images")
dir_graphs <- here("Graphics")




# MER Data
file_psnu_im <- (list.files(path =dir_merdata,
                            pattern = "PSNU_IM_FY18",
                            recursive = FALSE,
                            full.names = TRUE))

df<-read_msd(file_psnu_im)

# GEO Data
gis_5_sfc <- list.files(dir_geo, pattern = ".*_5_.*.shp$", recursive = T, full.names = T) %>%
  set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
  map(read_sf)

gis_4_sfc <- list.files(dir_geo, pattern = ".*_4_.*.shp$", recursive = T, full.names = T) %>%
  set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
  map(read_sf)

nga1 <- get_adm_boundaries("NGA", adm_level = 1, geo_path = dir_geo) %>%
  st_as_sf() %>%
  select(country = name_0, province = name_1)



# MER Data Munge ---------------------------------------------------------------------------------

#VLS & CURR
df_VL<-df %>%
  filter(fiscal_year %in% c("2019","2020"),
         indicator %in% c("TX_PVLS","SC_ARVDISP","TX_CURR"),
         standardizeddisaggregate %in% c("DispensedARVBottles","Age/Sex/HIVStatus","Age/Sex/Indication/HIVStatus"),
         operatingunit %in% c("Nigeria")) %>%
  mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>%
  group_by(fiscal_year,operatingunit,fundingagency,psnuuid,psnu,indicator,otherdisaggregate) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(indicator=case_when(
    indicator %in% c("TX_PVLS_D", "TX_PVLS") ~ indicator,
    indicator=="TX_CURR" ~ indicator,
    TRUE ~ otherdisaggregate)) %>%
  reshape_msd(clean = TRUE) %>%
  select(-period_type) %>%
  mutate(val=case_when(
    str_detect(indicator,"180-count") ~ val*6,
    str_detect(indicator, "90-count") ~ val*3,
    TRUE ~ val),
    indicator=case_when(
      indicator %in% c("TX_PVLS_D", "TX_PVLS", "TX_CURR") ~ indicator,
      str_detect(indicator, "TLD") ~ "TLD",
      TRUE ~ "other"
    )) %>%
  spread(indicator, val) %>%
  group_by(fundingagency,period,psnuuid,psnu) %>%
  summarise_at(vars(other:TX_PVLS_D),sum,na.rm=TRUE) %>%
  ungroup() %>%
  group_by(fundingagency,psnu,psnuuid) %>%
  mutate(VLC = TX_PVLS_D / lag(TX_CURR, 2, order_by = period),
         VLS_timesVLC = (TX_PVLS/TX_PVLS_D)*VLC,
         VLS=(TX_PVLS/TX_PVLS_D),
         TLD_MOT=ifelse(other >0, (TLD/(TLD+other)), NA),
         VLS_cat=case_when(
           VLS <.8 ~ "Less than 80%",
           VLS >=.8 & VLS <.9 ~ "80-89%",
           VLS >= .9 ~ "Greater than 90%"),
         VLS_TLD_ratio=ifelse(TLD_MOT >0, (VLS/TLD_MOT), NA))



# GEO Data Joins
nga_geo<-st_as_sf(gis_4_sfc$Nigeria) %>%
  left_join(df_VL, by = c("uid" = "psnuuid"))


# VIZ ------------------------------------------------------------------------------------
map_VLS<-terrain_map(countries = "Nigeria", terr_path = dir_terr, mask = TRUE) +
  geom_sf(data = nga_geo %>% filter(period=="FY20Q2" & !is.na(VLS)),
          aes(fill=VLS), lwd=.2, color=grey50k)+
  scale_fill_viridis_c(option="viridis", alpha=0.9, direction = -1,
                       breaks = c(0.5,0.75, 1.00),
                       limits = c(0.50, 1.00),
                       labels=percent)+
  si_style_map()+
  theme(
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm"))+
  ggtitle("Viral Load Suppression")+
  theme(plot.title = element_text(size = 9, family = "Source Sans Pro", face=1))+
  facet_wrap(~fundingagency, nrow = 2)



map_TLD_MOT<-terrain_map(countries = "Nigeria", terr_path = dir_terr, mask = TRUE) +
  geom_sf(data = nga_geo %>% filter(period=="FY20Q2" & !is.na(TLD_MOT)), aes(fill = TLD_MOT), lwd = .2, color = grey10k) +
  geom_sf(data = nga1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_viridis_c(option="magma", alpha=0.9, direction = -1,
                       breaks = c(0.5,0.75, 1.00),
                       limits = c(0.50, 1.00),
                       labels=percent)+
  si_style_map() +
  theme(
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm"))+
  ggtitle("% TLD Months of TX (MOT) \n out of total ARVs Dispensed")+
  theme(plot.title = element_text(size = 9, family = "Source Sans Pro", face=1))+
  facet_wrap(~fundingagency, nrow=2)



scatter<-df_VL %>%
  filter(period=="FY20Q2",
         fundingagency %in% c("HHS/CDC", "USAID")) %>%
  ggplot(aes(
    x=TLD_MOT,
    y=VLS))+
  geom_point(aes(fill=period),
             color="white",shape=21,size=4,alpha=0.7,
             show.legend = F)+
  scale_fill_manual(values=c(grey50k))+
  scale_x_continuous(limits=c(0.5,1), labels = percent)+
  scale_y_continuous(limits=c(0.5,1), labels = percent)+
  geom_abline(intercept=0,slope=1,linetype="dashed")+
  labs(x = "% TLD MOT Dispensed", y="VLS %")+
  si_style_nolines()+
  ggtitle("VLS vs. % TLD Months of TX \n (MOT) Dispensed")+
  theme(plot.title = element_text(size = 9, family = "Source Sans Pro", face=1))+
  facet_wrap(~fundingagency, nrow=2)


# VIZ COMBINED & SAVED ---------------------------------------------------------------------------------

(map_VLS + map_TLD_MOT + scatter) +
  plot_annotation("Nigeria | FY20Q2",
                  caption = "Source: FY20Q3i MSD
    VLS=(TX_PVLS_N/TX_PVLS_D)
    ARV Disp adjusted for Months of Treatments based on pill count")


ggsave(here("Graphics", "Nigeria_VLS_TLD_TLE_Ratio2.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")


