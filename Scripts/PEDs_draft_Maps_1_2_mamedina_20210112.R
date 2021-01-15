#Mamedina
#01122021 - DRAFT PEDs Maps

#MAp 1 a map showing IPs by SNU by country 
#what % of the FY21 peds TX_CURR targets each IP has

#Map 2: a map showing IPs by SNU by country what % of the peds TX_CURR APR20 results to targets 
#each IP achieved (ie what % of targets were achieved) - from 0 - 100%?


library(extrafont)
library(tidyverse)
library(sf)
library(glitr)
library(gisr)
library(here)
library(scales)
library(patchwork)
library(ICPIutilities)
library(glamr)
library(janitor)

# MER Data

load_secrets()

# MER Site level import --------------------------------------------------------------------

peds_psnu <- list.files(path = si_path(type="path_msd"),
                        pattern = "Structured_.*_PSNU_IM.*_20201218_v2_1.txt",
                        full.names = TRUE) %>%
  read_msd()

# GEO DATA ------------------------------------------------------------

gis_vc_sfc <- list.files(si_path(type="path_vector"), pattern = "Vc.*.shp$", recursive = T, full.names = T) %>%
  set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
  map(read_sf)

zam1 <- get_adm_boundaries("ZMB", adm_level = 1, geo_path = si_path(type="path_vector")) %>%
  st_as_sf() %>%
  select(country = name_0, province = name_1)

# MER Data Munge ---------------------------------------------------------------------------------


TX_disaggs<-peds_psnu %>%
  filter(indicator %in% c("TX_CURR")) %>%
  distinct(indicator,standardizeddisaggregate,otherdisaggregate, trendsfine)

#not primepartner
zam_ped<-peds_psnu %>% filter(fiscal_year=="2021",countryname=="Zambia",
           indicator=="TX_CURR", standardizeddisaggregate=="Age/Sex/HIVStatus") %>%
  filter(!trendsfine %in% c("15-19","20-24","25-29","30-34","35-39","40-49","50+")) %>%
  group_by(fiscal_year,operatingunit,snu1, mech_name, snu1uid) %>%
  summarise(across(starts_with("targ"), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  reshape_msd(clean = TRUE) %>%
  select(-period_type) %>%
  group_by(operatingunit, mech_name) %>%
  mutate(mech_val = sum(val, na.rm = TRUE)) %>%
  ungroup() %>%
  #mutate(mech_name = paste0(mech_name, " (", fundingagency, ")")) %>%
  group_by(operatingunit, snu1, snu1uid, mech_name) %>%
  summarise(val = sum(val, na.rm = TRUE),
            mech_val = first(mech_val),
            share = sum(val, na.rm = TRUE) / mech_val)%>%
  ungroup()


 # a map showing IPs by SNU by country what % of the peds TX_CURR APR20 
 #results to targets each IP achieved (ie what % of targets were achieved) - from 0 - 100%?
fy20<-peds_psnu %>% filter(fiscal_year=="2020",countryname=="Zambia",indicator=="TX_CURR", standardizeddisaggregate=="Age/Sex/HIVStatus") %>%
  filter(!trendsfine %in% c("15-19","20-24","25-29","30-34","35-39","40-49","50+")) %>%
  group_by(fiscal_year,operatingunit,snu1, mech_name, snu1uid) %>%
  summarise_at(vars(targets:cumulative),sum,na.rm=TRUE) %>%
  ungroup() %>%
  select(-c(qtr1:qtr4)) %>%
  #reshape_msd(clean = TRUE) %>%
  #select(-period_type)
  group_by(fiscal_year,operatingunit,snu1, mech_name, snu1uid) %>%
  mutate(APR=(cumulative/targets))%>%
  ungroup()
  #reshape_msd(clean = TRUE) %>%
  #select(-period_type,val)

# GEO Data Joins
peds_geo<-st_as_sf(gis_vc_sfc$VcPepfarPolygons.shp) %>%
  left_join(zam_ped, by = c("uid" = "snu1uid"))

map2_geo<-st_as_sf(gis_vc_sfc$VcPepfarPolygons.shp) %>%
  left_join(fy20, by = c("uid" = "snu1uid"))



#MAP 1 VIZ

map<-terrain_map(countries = "Zambia", terr_path = si_path(type = "path_raster"), mask = TRUE) +
  geom_sf(data = peds_geo %>% filter(!is.na(share)), aes(fill = share), lwd = .2, color = grey10k) +
  geom_sf_text(data = peds_geo %>% filter(!is.na(share)), aes(label=percent(share, .1)), color=usaid_red, size = .2)+
  geom_sf(data = zam1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_si(palette = "moody_blues", discrete=FALSE, alpha=0.9, reverse = FALSE,
                breaks = c(0,0.25,0.5,0.75, 1.00),
                limits = c(0, 1.00),
                labels= percent)+
  si_style_map() +
  theme(
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm"))+
  ggtitle("Zambia TX_CURR FY21 Targets | % Share by IP \n Source: SNU Level MSD")+
  # theme(plot.title = element_text(size = 9, family = "Source Sans Pro", face=1))+
  facet_wrap(~mech_name, ncol = 3, labeller = label_wrap_gen(30))
  

print(map)

ggsave(here("Graphics", "Zambia_Map1_labels.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")



map2<-terrain_map(countries = "Zambia", terr_path = si_path(type = "path_raster"), mask = TRUE) +
  geom_sf(data = map2_geo %>% filter(!is.na(APR), !is.infinite(APR)), aes(fill = APR), lwd = .2, color = grey10k) +
  geom_sf(data = zam1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_si(palette = "moody_blues", discrete=FALSE, alpha=0.9, reverse = FALSE,
              breaks = c(0,0.25,0.5,0.75, 1.00,1.25,1.5,2.0),
              limits = c(0, 2.00),
              labels=percent)+
  theme(
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm"))+
  ggtitle("Zambia TX_CURR FY20 % Target Achievements")+
  # theme(plot.title = element_text(size = 9, family = "Source Sans Pro", face=1))+
  facet_wrap(~mech_name, nrow = 3, ncol = 3)

print(map2)

ggsave(here("Graphics", "Zambia_Map2.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
