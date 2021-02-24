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
                        pattern = "Structured_.*_PSNU_IM.*_20201218_v2_1.zip",
                        full.names = TRUE) %>%
  sort() %>%
  last() %>%
  read_msd()

# GEO DATA ------------------------------------------------------------

gis_vc_sfc <- return_latest(
    si_path(type="path_vector"),
    pattern = "Vc.*.shp$",
    recursive = T, full.names = T) %>%
  set_names(basename(.) %>% str_remove(".shp")) %>%
  map(read_sf)

gis_vc_sfc <- list.files(
  si_path(type="path_vector"),
  pattern = "Vc.*.shp$",
  recursive = T, full.names = T) %>%
  set_names(basename(.) %>% str_remove(".shp")) %>%
  map(read_sf)


# cntry_adm1 <- get_adm_boundaries("ZMB", adm_level = 1, geo_path = si_path(type="path_vector")) %>%
#   st_as_sf() %>%
#   select(country = name_0, province = name_1)

# zam1 <- get_adm_boundaries("ZMB", adm_level = 1, geo_path = si_path(type="path_vector")) %>%
#   st_as_sf() %>%
#   select(country = name_0, province = name_1)

# MER Data Munge ---------------------------------------------------------------------------------


# TX_disaggs<-peds_psnu %>%
#   filter(indicator %in% c("TX_CURR")) %>%
#   distinct(indicator,standardizeddisaggregate,otherdisaggregate, trendsfine)

# Global dataset
cntry_peds <- peds_psnu %>%
  filter(fiscal_year == "2021",
         indicator == "TX_CURR",
         standardizeddisaggregate == "Age/Sex/HIVStatus") %>%
  filter(!trendsfine %in% c("15-19","20-24","25-29",
                            "30-34","35-39","40-49","50+")) %>%
  glamr::clean_agency() %>%
  group_by(fiscal_year, operatingunit, snu1, #mech_name, mech_code,
           snu1uid, primepartner, fundingagency) %>%
  summarise(across(starts_with("targ"), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  reshape_msd(clean = TRUE) %>%
  select(-period_type) %>%
  group_by(operatingunit) %>%
  mutate(country_val = sum(val, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(operatingunit, primepartner) %>%
  mutate(primepartner_val = sum(val, na.rm = TRUE)) %>%
  ungroup() %>%
  #mutate(primepartner = paste0(primepartner, " (", fundingagency, ")")) %>%
         #mech_name = paste0(mech_name, " (", fundingagency, ")"),
         #mech_code = paste0(mech_code, " (", fundingagency, ")")) %>%
  group_by(operatingunit, snu1, snu1uid, #mech_name, mech_code,
           primepartner, fundingagency) %>%
  summarise(val = sum(val, na.rm = TRUE),
            share = val / first(country_val),
            primeshare = primepartner_val / first(country_val)) %>%
  ungroup() %>%
  mutate(primepartner = paste0(primepartner, " (", round(primeshare*100, 2), "%",")"))

#how do I stop it from writing over share, and country_val??

cntry_peds %>% distinct(fundingagency)

cntry_peds %>% View()

#counting mechs
cntry_peds %>% filter(operatingunit == "Kenya", fundingagency == "CDC") %>% count(primepartner, mech_code, mech_name) %>% arrange(primepartner) %>% prinf()



map_share <- function(df_peds, ou,
                      agencies = "USAID") {

  print(ou)

  df_cntry <- df_peds %>%
    filter(operatingunit == ou,
           #fundingagency %in% c("USAID", "CDC"),
           fundingagency %in% agencies)

  nmechs <- df_cntry %>%
   # distinct(mech_name) %>%
    distinct(primepartner) %>%
    nrow()

  ncols <- round(nmechs / 3)

  peds_geo <- st_as_sf(gis_vc_sfc$VcPepfarPolygons) %>%
    left_join(df_cntry, by = c("uid" = "snu1uid")) %>%
    dplyr::filter(!is.na(share)) # BK => This will remove rows with no mer data

  basemap <- terrain_map(countries = ou,
                         terr = si_path(type = "path_raster"),
                         mask = TRUE)

 # ou2 <- ifelse(ou == "Cote d'Ivoire", "Ivory Coast", ou)

  ou <- case_when(
    ou == "Cote d'Ivoire" ~ "Ivory Coast",
    ou == "Eswatini" ~ "Swaziland",
    ou == "Democratic Republic of Congo" ~ "<...>",
    ou == "South Sudan" ~ "<...>",
    TRUE ~ ou
  )

  # ou <- ifelse(ou == "Cote d'Ivoire", "Ivory Coast",
  #        ifelse(ou == "Eswatini", "Swaziland",
  #               ifelse(ou == "Democratic Republic of Congo", "<...>",
  #                      ifelse(ou == "South Sudan", "<...>", ou))))


  cntry_adm1 <- gisr::get_admin1(ou)

  map1 <- basemap +
    geom_sf(data = peds_geo,
            aes(fill = share), lwd = .2, color = grey10k) +
    geom_sf_text(data = peds_geo,
                 aes(label = percent(share, .1)),
                 color = usaid_darkgrey, size = 1) +
    geom_sf(data = cntry_adm1, fill = NA, lwd = .2, color = grey30k) +
    scale_fill_si(palette = "moody_blues", discrete = FALSE,
                  alpha = 0.9, reverse = TRUE,
                  breaks = c(0,0.25,0.5,0.75, 1.00),
                  limits = c(0, 1.00),
                  labels = percent) +
    si_style_map() +
    theme(
      legend.position =  "bottom",
      legend.key.width = ggplot2::unit(1, "cm"),
      legend.key.height = ggplot2::unit(.5, "cm")) +
    ggtitle(paste0(ou, " | % of the FY21 PEDS TX_CURR Targets by IP and SNU1")) +
    # theme(plot.title = element_text(size = 9, family = "Source Sans Pro", face=1))+
    #facet_wrap(~mech_code, ncol = ncols, labeller = label_wrap_gen(20))
    facet_wrap(~primepartner, ncol = ncols, labeller = label_wrap_gen(20))


  print(map1)

  return(map1)
}

min(cntry_peds$share)
max(cntry_peds$share)

cntry_peds %>%
  filter(!str_detect(operatingunit, " Region$"),
         !is.na(share)) %>%
  distinct(operatingunit) %>%
  pull() %>%
  nth(11) %>%
  map(.x, .f = ~map_share(df_peds = cntry_peds,
                          ou = .x,
                          agencies = "USAID"))

cntry_peds %>%
  filter(!str_detect(operatingunit, " Region$"),
         !is.na(share)) %>%
  distinct(operatingunit) %>%
  pull() %>%
  nth(11) %>%
  map(.x, .f = ~map_share(df_peds = cntry_peds,
                          ou = .x,
                          agencies = "CDC"))


ggsave(here("~/Github/training/Graphics", "MAP1_edit_kenya TX_CURR by IP.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")



# ---------------- MAP 2 -----------------------------------

#Global Dataset for Map 2

cntry_2_peds <- peds_psnu %>%
  filter(fiscal_year == "2020",
         indicator == "TX_CURR",
         standardizeddisaggregate == "Age/Sex/HIVStatus") %>%
  filter(!trendsfine %in% c("15-19","20-24","25-29",
                            "30-34","35-39","40-49","50+")) %>%
  glamr::clean_agency() %>%
  group_by(fiscal_year, operatingunit, snu1, #mech_name, mech_code,
           snu1uid, primepartner, fundingagency) %>%
  summarise_at(vars(targets:cumulative),sum,na.rm=TRUE) %>%
  ungroup() %>%
  select(-c(qtr1:qtr4)) %>%
  #reshape_msd(clean = TRUE) %>%
  group_by(operatingunit) %>%
  mutate(country_targ = sum(targ, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(primepartner = paste0(primepartner, " (", fundingagency, ")")) %>%
  #mech_name = paste0(mech_name, " (", fundingagency, ")"),
  #mech_code = paste0(mech_code, " (", fundingagency, ")")) %>%
  group_by(operatingunit, snu1, snu1uid, #mech_name, mech_code,
           primepartner, fundingagency) %>%
  #I dont know if I understand what first() does
  summarise(cumulative = sum(cumulative, na.rm = TRUE),
            APR = cumulative / first(country_targ)) %>%
  ungroup()

#mutate(APR = (cumulative/targets)) %>%
#  ungroup()




map_apr <- function(df_apr, ou, agencies = "USAID") {

  print(ou)

  df_cntry_2 <- df_apr %>%
    filter(operatingunit == ou,
           #fundingagency %in% c("USAID", "CDC"),
           fundingagency %in% agencies)

  nmechs <- df_cntry_2 %>%
    # distinct(mech_name) %>%
    distinct(primepartner) %>%
    nrow()

  ncols <- round(nmechs / 3)

  map2_geo <- st_as_sf(gis_vc_sfc$VcPepfarPolygons) %>%
    left_join(df_cntry_2, by = c("uid" = "snu1uid")) %>%
    dplyr::filter(!is.na(share)) # BK => This will remove rows with no mer data

  basemap <- terrain_map(countries = ou,
                         terr = si_path(type = "path_raster"),
                         mask = TRUE)

  ou2 <- ifelse(ou == "Cote d'Ivoire", "Ivory Coast", ou)

  cntry_adm1 <- gisr::get_admin1(ou2)

  map1 <- basemap +
    geom_sf(data = map2_geo,
            aes(fill = APR), lwd = .2, color = grey10k) +
    geom_sf_text(data = map2_geo,
                 aes(label = percent(share, .1)),
                 color = usaid_darkgrey, size = 1) +
    geom_sf(data = cntry_adm1, fill = NA, lwd = .2, color = grey30k) +
    scale_fill_si(palette = "moody_blues", discrete = FALSE,
                  alpha = 0.9, reverse = TRUE,
                  breaks = c(0,0.25,0.5,0.75, 1.00),
                  limits = c(0, 1.00),
                  labels = percent) +
    si_style_map() +
    theme(
      legend.position =  "bottom",
      legend.key.width = ggplot2::unit(1, "cm"),
      legend.key.height = ggplot2::unit(.5, "cm")) +
    ggtitle(paste0(ou, " | % of the FY21 PEDS TX_CURR Targets by IP and SNU1")) +
    # theme(plot.title = element_text(size = 9, family = "Source Sans Pro", face=1))+
    #facet_wrap(~mech_code, ncol = ncols, labeller = label_wrap_gen(20))
    facet_wrap(~primepartner, ncol = ncols, labeller = label_wrap_gen(20))


  print(map1)

  return(map1)
}

min(cntry_peds$share)
max(cntry_peds$share)

cntry_peds %>%
  filter(!str_detect(operatingunit, " Region$"),
         !is.na(share)) %>%
  distinct(operatingunit) %>%
  pull() %>%
  nth(11) %>%
  map(.x, .f = ~map_share(df_peds = cntry_peds,
                          ou = .x,
                          agencies = "USAID"))

cntry_peds %>%
  filter(!str_detect(operatingunit, " Region$"),
         !is.na(share)) %>%
  distinct(operatingunit) %>%
  pull() %>%
  nth(11) %>%
  map(.x, .f = ~map_share(df_peds = cntry_peds,
                          ou = .x,
                          agencies = "CDC"))


ggsave(here("~/Github/training/Graphics", "MAP1_edit_kenya TX_CURR by IP.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")












zam_cdc <- peds_psnu %>% filter(fiscal_year=="2021",countryname=="Zambia",
                              indicator=="TX_CURR", standardizeddisaggregate=="Age/Sex/HIVStatus") %>%
  filter(!trendsfine %in% c("15-19","20-24","25-29","30-34","35-39","40-49","50+")) %>%
  mutate(
    fundingagency = case_when(
      fundingagency == "HHS/CDC" ~ "CDC",
      TRUE ~ fundingagency
    )) %>%
  group_by(fiscal_year,operatingunit,snu1, mech_name, snu1uid, primepartner, fundingagency) %>%
  summarise(across(starts_with("targ"), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  reshape_msd(clean = TRUE) %>%
  select(-period_type) %>%
  group_by(operatingunit) %>%
  mutate(country_val = sum(val, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(primepartner = paste0(primepartner, " (", fundingagency, ")"),
         mech_name = paste0(mech_name, " (", fundingagency, ")")) %>%
  group_by(operatingunit, snu1, snu1uid, mech_name, primepartner, fundingagency) %>%
  summarise(val = sum(val, na.rm = TRUE),
            #mech_val = first(mech_val),
            share = val / first(country_val)) %>%
  ungroup() %>%
  filter(fundingagency=="CDC")

#USAID
zam_USAID<-peds_psnu %>% filter(fiscal_year=="2021",countryname=="Zambia",
                              indicator=="TX_CURR", standardizeddisaggregate=="Age/Sex/HIVStatus") %>%
  filter(!trendsfine %in% c("15-19","20-24","25-29","30-34","35-39","40-49","50+")) %>%
  group_by(fiscal_year,operatingunit,snu1, mech_name, snu1uid, primepartner, fundingagency) %>%
  summarise(across(starts_with("targ"), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  reshape_msd(clean = TRUE) %>%
  select(-period_type) %>%
  group_by(operatingunit) %>%
  mutate(country_val = sum(val, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(primepartner = paste0(primepartner, " (", fundingagency, ")"),
         mech_name = paste0(mech_name, " (", fundingagency, ")")) %>%
  group_by(operatingunit, snu1, snu1uid, mech_name, primepartner, fundingagency) %>%
  summarise(val = sum(val, na.rm = TRUE),
            #mech_val = first(mech_val),
            share = val / first(country_val)) %>%
  ungroup() %>%
  filter(fundingagency=="USAID")


#Merge - should I merge the two data sets here or later on
zam_ped<-rbind(zam_cdc, zam_USAID)


 # a map showing IPs by SNU by country what % of the peds TX_CURR APR20
 #results to targets each IP achieved (ie what % of targets were achieved) - from 0 - 100%?
fy20 <- peds_psnu %>%
  filter(fiscal_year == "2020",
         countryname == "Zambia",
         indicator == "TX_CURR",
         standardizeddisaggregate == "Age/Sex/HIVStatus") %>%
  mutate(
    fundingagency = case_when(
      fundingagency == "HHS/CDC" ~ "CDC",
      TRUE ~ fundingagency
    )) %>%
  filter(!trendsfine %in% c("15-19","20-24","25-29","30-34","35-39","40-49","50+")) %>%
  group_by(fiscal_year, operatingunit,snu1, mech_name,
           snu1uid, primepartner, fundingagency) %>%
  summarise_at(vars(targets:cumulative),sum,na.rm=TRUE) %>%
  ungroup() %>%
  select(-c(qtr1:qtr4)) %>%
  #reshape_msd(clean = TRUE) %>%
  #select(-period_type)
  mutate(primepartner = paste0(primepartner, " (", fundingagency, ")"),
         mech_name = paste0(mech_name, " (", fundingagency, ")")) %>%
  group_by(fiscal_year,operatingunit,snu1, mech_name,
           snu1uid,fundingagency, primepartner) %>%
  mutate(APR = (cumulative/targets)) %>%
  ungroup()
  #reshape_msd(clean = TRUE) %>%
  #select(-period_type,val)

# GEO Data Joins
peds_geo <- st_as_sf(gis_vc_sfc$VcPepfarPolygons.shp) %>%
  left_join(zam_ped, by = c("uid" = "snu1uid"))

map2_geo <- st_as_sf(gis_vc_sfc$VcPepfarPolygons.shp) %>%
  left_join(fy20, by = c("uid" = "snu1uid"))



#MAP 1 VIZ
#I need to see if I should add two different dataframes into one map or if
#I should have joined the two of them earlier on

#BK: create basemap once
basemap <- terrain_map(countries = "Zambia",
                       terr = si_path(type = "path_raster"),
                       mask = TRUE)

#then reuse basemap
map1 <- basemap +
  geom_sf(data = peds_geo %>% filter(!is.na(share)),
              aes(fill = share), lwd = .2, color = grey10k) +
  geom_sf_text(data = peds_geo %>% filter(!is.na(share)),
               aes(label=percent(share, .1)), color=usaid_darkgrey, size = 2) +
  geom_sf(data = zam1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_si(palette = "moody_blues", discrete=FALSE,
                alpha=0.9, reverse = TRUE,
                breaks = c(0,0.25,0.5,0.75, 1.00),
                limits = c(0, 1.00),
                labels = percent) +
  si_style_map() +
  theme(
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm"))+
  ggtitle("Zambia | % of the FY21 PEDS TX_CURR Targets by IP and SNU1")+
  # theme(plot.title = element_text(size = 9, family = "Source Sans Pro", face=1))+
  facet_wrap(~mech_name, ncol = 3, labeller = label_wrap_gen(20))

print(map1)

ggsave(here("Graphics", "Zambia_Map1_edit.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")

#BY AGENCY


map1a <- basemap +
  geom_sf(data = peds_geo %>% filter(!is.na(share), fundingagency == "USAID"),
          aes(fill = share), lwd = .2, color = grey10k) +
  geom_sf_text(data = peds_geo %>% filter(!is.na(share), fundingagency == "USAID"),
               aes(label=percent(share, .1)),
               color=usaid_darkgrey, size = 2)+
  geom_sf(data = zam1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_si(palette = "moody_blues", discrete=FALSE,
                alpha=0.9, reverse = TRUE,
                breaks = seq(0, .2, .1),
                limits = c(0, .2),
                labels = percent) +
  si_style_map() +
  theme(
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm"))+
  #ggtitle("Zambia | % of the FY21 PEDS TX_CURR Targets by IP and SNU1")+
  # theme(plot.title = element_text(size = 9, family = "Source Sans Pro", face=1))+
  facet_wrap(~mech_name, ncol = 1, labeller = label_wrap_gen(20))

print(map1a)

# CDC
map1b <- basemap +
  geom_sf(data = peds_geo %>% filter(!is.na(share), fundingagency == "CDC"),
          aes(fill = share), lwd = .2, color = grey10k) +
  geom_sf_text(data = peds_geo %>% filter(!is.na(share), fundingagency == "CDC"),
               aes(label=percent(share, .1)),
               color=usaid_darkgrey, size = 2)+
  geom_sf(data = zam1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_si(palette = "moody_blues", discrete=FALSE,
                alpha=0.9, reverse = TRUE,
                breaks = seq(0, .3, .1),
                limits = c(0, .3),
                labels = percent) +
  si_style_map() +
  theme(
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm"))+
  #ggtitle("Zambia | % of the FY21 PEDS TX_CURR Targets by IP and SNU1")+
  # theme(plot.title = element_text(size = 9, family = "Source Sans Pro", face=1))+
  facet_wrap(~mech_name, ncol = 2, labeller = label_wrap_gen(20))

print(map1b)

library(patchwork)

map1c <- (map1a + map1b) +
  plot_layout(ncol = 2, widths = c(1, 2)) +
  plot_annotation(
    title = "ZAMBIA | % of the FY21 PEDS TX_CURR Targets by IP and SNU1",
    caption = paste0("OHA/SIEI - MSD", Sys.Date()),
    theme = theme(plot.title = element_text(hjust = .5))
  )


print(map1c)


ggsave(here("Graphics", "MAP1_ZAMBIA_PEDS_TX_CURR Targets FY21.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")


map2<-basemap +
  geom_sf(data = map2_geo %>% filter(!is.na(APR), !is.infinite(APR)),
          aes(fill = APR), lwd = .2, color = grey10k) +
  geom_sf_text(data = map2_geo %>% filter(!is.na(APR), !is.infinite(APR)),
          aes(label=percent(APR, .1)), color=usaid_lightgrey, size = 1) +
  geom_sf(data = zam1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_si(palette = "moody_blues", discrete=FALSE,
                alpha=0.9, reverse = TRUE,
                breaks = c(0,0.25,0.5,0.75, 1.00,1.25,1.5, 1.75, 2.0),
                limits = c(0, 2.00),
                labels = percent) +
  si_style_map() +
  theme(
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(2, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm"))+
  ggtitle("ZAMBIA | % of TX_CURR FY20 Target Achievements by IP and SNU1",
          subtitle ="SIEI/OHA - MSD FY20") +
  facet_wrap(~mech_name, ncol = 4, labeller = label_wrap_gen(20))

print(map2)

ggsave(here("Graphics", "MAP2_ZAMBIA TX_CURR TARGETS by IP.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")



#AGENCY SEPARATION FOR MAP 2

map2a <- basemap +
  geom_sf(data = map2_geo %>% filter(!is.na(APR), !is.infinite(APR), fundingagency == "USAID"),
          aes(fill = APR), lwd = .2, color = grey10k, show.legend = FALSE) +
  geom_sf_text(data = map2_geo %>% filter(!is.na(APR), !is.infinite(APR), fundingagency == "USAID"),
               aes(label=percent(APR, .1)),
               color=usaid_lightgrey, size = 2)+
  geom_sf(data = zam1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_si(palette = "moody_blues", discrete=FALSE,
                alpha=0.9, reverse = TRUE,
                breaks = c(0,0.25,0.5,0.75, 1.00,1.25,1.5, 1.75, 2.0),
                limits = c(0, 2.00),
                labels = percent) +
  si_style_map() +
  theme(
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm"))+
  #ggtitle("Zambia | % of the FY21 PEDS TX_CURR Targets by IP and SNU1")+
  # theme(plot.title = element_text(size = 9, family = "Source Sans Pro", face=1))+
  facet_wrap(~mech_name, ncol = 1, labeller = label_wrap_gen(20))

print(map2a)

# CDC
map2b <- basemap +
  geom_sf(data = map2_geo %>% filter(!is.na(APR), !is.infinite(APR), fundingagency == "CDC"),
          aes(fill = APR), lwd = .2, color = grey10k) +
  geom_sf_text(data = map2_geo %>% filter(!is.na(APR), !is.infinite(APR), fundingagency == "CDC"),
               aes(label=percent(APR, .1)),
               color=usaid_lightgrey, size = 2)+
  geom_sf(data = zam1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_si(palette = "moody_blues", discrete=FALSE,
                alpha=0.9, reverse = TRUE,
                breaks = c(0,0.25,0.5,0.75, 1.00,1.25,1.5, 1.75, 2.0),
                limits = c(0, 2.00),
                labels = percent) +
  si_style_map() +
  theme(
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(1.5, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm"))+
  #ggtitle("Zambia | % of the FY21 PEDS TX_CURR Targets by IP and SNU1")+
  # theme(plot.title = element_text(size = 9, family = "Source Sans Pro", face=1))+
  facet_wrap(~mech_name, ncol = 2, labeller = label_wrap_gen(20))

print(map1b)

library(patchwork)

map2c <- (map2a + map2b) +
  plot_layout(ncol = 2, widths = c(1, 2),
              guides = 'collect') +
  plot_annotation(
    title = "ZAMBIA | % of TX_CURR FY20 Target Achievements by IP and SNU1",
    caption = paste0("OHA/SIEI - MSD", Sys.Date()),
    theme = theme(plot.title = element_text(hjust = .5), legend.position = 'bottom')
)
print(map2c)


ggsave(here("Graphics", "2_MAP2_ZAMBIA TX_CURR TARGETS by IP.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")


