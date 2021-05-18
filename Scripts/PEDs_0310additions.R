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
library(rnaturalearth)
library(rnaturalearthhires)

# MER Data

load_secrets()

# MER Site level import --------------------------------------------------------------------

peds_psnu <- list.files(path = si_path(type="path_msd"),
                        pattern = "Structured_.*_PSNU_IM.*_20210319_v2_1.zip",
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
#double check to make sure there is no duplicate data
#Update with Q1 fy21 data !! may help get rid of TBD
#check panorama to check on like guatemala and other countries that don't have PEDS data
#make sure to de-dups - fundingagency

cntry_peds <- peds_psnu %>%
  filter(fiscal_year == "2021",
         indicator == "TX_CURR",
         standardizeddisaggregate == "Age/Sex/HIVStatus",
        !fundingagency %in% c("Dedup"),
        !primepartner %in% c("TBD")) %>%
  filter(!trendsfine %in% c("15-19","20-24","25-29",
                            "30-34","35-39","40-49","50+")) %>%
  glamr::clean_agency() %>%
  group_by(fiscal_year, operatingunit, snu1, countryname,
           snu1uid, primepartner, fundingagency) %>%
  summarise(across(starts_with("targ"), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  reshape_msd(clean = TRUE) %>%
  select(-period_type) %>%
  group_by(operatingunit) %>%
  mutate(country_val = sum(val, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(operatingunit, primepartner, fundingagency) %>%
  mutate(primepartner_val = sum(val, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(operatingunit, snu1, snu1uid, #countryname,
           primepartner, fundingagency) %>%
  summarise(val = sum(val, na.rm = TRUE),
            share = val / first(country_val),
             primeshare = primepartner_val/first(country_val)) %>%
  ungroup() %>%
  #mutate(primepartner = paste0(primepartner, " (", fundingagency, "(", round(primeshare*100, 2),  "%","))"))
  mutate(primepartner = paste0(fundingagency, "-",
                        primepartner,
                        " (",
                        round(primeshare*100, 1),
                        "%)"))

#how do I stop it from writing over share, and country_val??

write.csv(cntry_peds,"C:\\Users\\STAR\\Documents\\Github\\peds_map1_txcurr.csv", row.names = FALSE)

cntry_2_peds %>% distinct(fundingagency)

#counting mechs
cntry_peds %>% filter(operatingunit == "Kenya", fundingagency == "CDC") %>% count(primepartner, mech_code, mech_name) %>% arrange(primepartner) %>% prinf()


#function 1
map_share <- function(df_peds, ou,
                      agencies = "CDC") {

  print(ou)

  df_cntry <- df_peds %>%
    filter(operatingunit == ou,
           #fundingagency %in% c("USAID", "CDC"),
           fundingagency %in% agencies)

  nmechs <- df_cntry %>%
    distinct(primepartner) %>%
    nrow() %>%
    ncol()

  #ncols <- round(nmechs / 2)

  ou <-  ifelse(ou == "Cote d'Ivoire", "Ivory Coast",
                ifelse(ou == "Eswatini", "Swaziland",
                       ifelse(ou == "Democratic Republic of Congo", "Democratic Republic of the Congo",
                              ifelse(ou == "Tanzania", "United Republic of Tanzania",
                                     ifelse(ou == "South Sudan", "Sudan", ou)))))

  peds_geo <- st_as_sf(gis_vc_sfc$VcPepfarPolygons) %>%
    left_join(df_cntry, by = c("uid" = "snu1uid")) %>%
    dplyr::filter(!is.na(share)) # BK => This will remove rows with no mer data

  basemap <- terrain_map(countries = ou,
                         terr = si_path(type = "path_raster"),
                         mask = TRUE)

  cntry_adm1 <- gisr::get_admin1(ou)

  map1 <- basemap +
    geom_sf(data = peds_geo,
            aes(fill = share), lwd = .2, color = grey10k, stroke = 1.5, alpha = 0.7) +
    geom_sf_text(data = peds_geo,
                 aes(label = percent(share, .01)),
                 color = usaid_black, size = 2.5) +
    geom_sf(data = cntry_adm1, fill = NA, lwd = .2, color = grey30k) +
    scale_fill_si(palette = "moody_blues", discrete = FALSE,
                  alpha = 0.9, reverse = TRUE,
                  breaks = seq(0, .2, 0.1),
                  limits = c(0, 0.2),
                  labels = percent) +
    si_style_map() +
    theme(
      legend.position =  "bottom",
      legend.key.width = ggplot2::unit(1, "cm"),
      legend.key.height = ggplot2::unit(.5, "cm")) +
    ggtitle(paste0(agencies, " - ", ou, " | % of the FY21 PEDS TX_CURR Targets by IP and SNU1")) +
    # theme(plot.title = element_text(size = 9, family = "Source Sans Pro", face=1))+
    #facet_wrap(~mech_code, ncol = ncols, labeller = label_wrap_gen(20))
    facet_wrap(~primepartner, labeller = label_wrap_gen(20))


  print(map1)

  return(map1)
}


#look at 02102021 copy!!!

shp_world <- ne_countries(country=NULL, returnclass="sf")
shp_world %>%sf::st_set_geometry(NULL) %>% view()
get_admin0(countries, scale = "medium", crs = 4326)
gisr::get_admin0()
#look at gisr master utilities geo utilities

min(cntry_peds$share)
max(cntry_peds$share)

ne_countries()
library(rnaturalearthhires)
library(rnaturalearth)


map1a <- cntry_peds %>%
  filter(!str_detect(operatingunit, " Region$"),
         !is.na(share)) %>%
  distinct(operatingunit) %>%
 # mutate(operatingunit = case_when(ou == "Cote d'Ivoire", "Ivory Coast",
                          #  ifelse(ou == "Eswatini", "Swaziland",
                          #  ifelse(ou == "Tanzania", "United Republic of Tanzania",
                          #  ifelse(ou == "Democratic Republic of Congo", "Democratic Republic of the Congo")
                          #  ifelse(ou == "South Sudan", "S Sudan", ou))))) %>%

 # mutate(operatingunit = case_when(
                     #     operatingunit %% ou == "Cote d'Ivoire" ~ "Ivory Coast",
                        #  operatingunit %% ou == "Eswatini" ~ "Swaziland",
                        #  operatingunit %% ou == "Tanzania" ~ "United Republic of Tanzania",
                        #  operatingunit %% ou == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
                        #  operatingunit %% ou == "South Sudan" ~ "Sudan",
                        #   TRUE ~ as.character(operatingunit))) %>%
  pull() %>%
  nth(11) %>%
  map(.x, .f = ~map_share(df_peds = cntry_peds,
                          ou = .x,
                          agencies = "USAID"))




map1b <- cntry_peds %>%
  filter(!str_detect(operatingunit, " Region$"),
         !is.na(share)) %>%
  distinct(operatingunit) %>%
  pull() %>%
  nth(21) %>%
  map(.x, .f = ~map_share(df_peds = cntry_peds,
                          ou = .x,
                          agencies = "CDC"))



ggsave(here("~/Github/training/Graphics/map1_peds", "MAP1_Uganda_CDC_TX_CURR by IP.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")

#write.csv(cntry_peds,"C:\\Users\\STAR\\Documents\\Github\\peds_txcurr.csv", row.names = FALSE)


#Function 2 - USAID and CDC Maps Combined

map_share_2 <- function(df_peds, ou) {
                        #agencies %in% c("USAID", "CDC") {

  print(ou)

  df_cntry <- df_peds %>%
    filter(operatingunit == ou,
           fundingagency %in% c("USAID", "CDC"))
           #fundingagency %in% agencies)

  nmechs <- df_cntry %>%
    distinct(primepartner) %>%
    nrow()

  ncols <- round(nmechs / 3)

  ou <-  ifelse(ou == "Cote d'Ivoire", "Ivory Coast",
                ifelse(ou == "Eswatini", "Swaziland",
                       ifelse(ou == "Democratic Republic of Congo", "Democratic Republic of the Congo",
                              ifelse(ou == "Tanzania", "United Republic of Tanzania",
                                     ifelse(ou == "South Sudan", "Sudan", ou)))))

  peds_geo <- st_as_sf(gis_vc_sfc$VcPepfarPolygons) %>%
    left_join(df_cntry, by = c("uid" = "snu1uid")) %>%
    dplyr::filter(!is.na(share)) # BK => This will remove rows with no mer data

  basemap <- terrain_map(countries = ou,
                         terr = si_path(type = "path_raster"),
                         mask = TRUE)

  cntry_adm1 <- gisr::get_admin1(ou)

  map1a <- basemap +
    geom_sf(data = peds_geo %>% filter(!is.na(share), fundingagency == "USAID"),
            aes(fill = share), lwd = .2, color = grey10k, stroke = 0.5, alpha = 0.7) +
    geom_sf_text(data = peds_geo %>% filter(!is.na(share), fundingagency == "USAID"),
                 aes(label=percent(share, .01)),
                 color=usaid_black, size = 2)+
    geom_sf(data = cntry_adm1, fill = NA, lwd = .2, color = grey30k) +
    scale_fill_si(palette = "old_roses", discrete=FALSE,
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
    facet_wrap(~primepartner, ncol = 1, labeller = label_wrap_gen(20))

  print(map1a)

  # CDC
  map1b <- basemap +
    geom_sf(data = peds_geo %>% filter(!is.na(share), fundingagency == "CDC"),
            aes(fill = share), lwd = .2, color = grey10k, stroke = 0.5, alpha = 0.7) +
    geom_sf_text(data = peds_geo %>% filter(!is.na(share), fundingagency == "CDC"),
                 aes(label=percent(share, .01)),
                 color=usaid_black, size = 3)+
    geom_sf(data = cntry_adm1, fill = NA, lwd = .2, color = grey30k) +
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
    facet_wrap(~primepartner, ncol = 2, labeller = label_wrap_gen(20))

  print(map1b)

  library(patchwork)

  map1c <- (map1a + map1b) +
    plot_layout(ncol = 2, widths = c(1, 2)) +
    plot_annotation(
      title = paste0(ou, " | % of the FY21 PEDS TX_CURR Targets by IP and SNU1"),
      caption = paste0("OHA/SIEI - MSD", Sys.Date()),
      theme = theme(plot.title = element_text(hjust = .5))
    )


  print(map1c)

  return(map1c)
}

??ne_countries

#country priorities:
#Kenya, Tanzania,
#Malawi, Mozambique, Nigeria, South Africa, Uganda, Zambia, and Zimbabwe

cntry_peds %>%
  filter(!str_detect(operatingunit, " Region$"),
         !is.na(share)) %>%
  distinct(operatingunit) %>%
  #mutate(ou <- case_when(
   # ou %% ou == "Cote d'Ivoire" ~ "Ivory Coast",
    #ou %% ou == "Eswatini" ~ "Swaziland",
   # ou %% ou == "Tanzania" ~ "United Republic of Tanzania",
   # ou %% ou == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
   # ou %% ou == "South Sudan" ~ "Sudan",
  #  TRUE ~ as.character(ou))) %>%
  pull() %>%
  nth(25) %>%
  map(.x, .f = ~map_share_2(df_peds = cntry_peds,
                          ou = .x),
                          fundingagency %in% c("CDC" & "USAID"))


ggsave(here("~/Github/training/Graphics/map1_peds", "MAP1_Zimbabwe_TX_CURR by IP.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")


#11, 18, 21 Kenya, SA, Uganda - Still having trouble displaying these: even by primepartner, still 15 IPs for Kenya,
#in general, I want to be able to make the higher percentages and labels more legible - how would I do this?
#I want to make the titles long - how do I adjust this in the wrap?
#For example, Botswana IPs look smushed in some


# ---------------- MAP 2 -----------------------------------

#Global Dataset for Map 2

cntry_2_peds <- peds_psnu %>%
  filter(fiscal_year == "2020",
         indicator == "TX_CURR",
         standardizeddisaggregate == "Age/Sex/HIVStatus",
         !fundingagency %in% c("Dedup"),
         !primepartner %in% c("TBD")) %>%
  filter(!trendsfine %in% c("15-19","20-24","25-29",
                            "30-34","35-39","40-49","50+")) %>%
  glamr::clean_agency() %>%
  group_by(fiscal_year, operatingunit, snu1,
           snu1uid, primepartner, fundingagency) %>%
  summarise_at(vars(targets:cumulative),sum,na.rm=TRUE) %>%
  ungroup() %>%
  select(-c(qtr1:qtr4)) %>%
  #reshape_msd(clean = TRUE) %>%
  group_by(operatingunit, snu1, snu1uid, #countryname,
           primepartner, fundingagency) %>%
  mutate(APR = (cumulative/targets)) %>%
  ungroup() %>%
  mutate(primepartner = paste0(#fundingagency, "-",
                                primepartner))

write.csv(cntry_2_peds,"C:\\Users\\STAR\\Documents\\Github\\peds_map2_txcurr.csv", row.names = FALSE)

#cntry_2_peds %>% distinct(operatingunit=="Tanzania")

###################3
group_by(fiscal_year, operatingunit, snu1,
         snu1uid, primepartner, fundingagency) %>%
  summarise_at(vars(targets:cumulative),sum,na.rm=TRUE) %>%
  ungroup() %>%
  select(-c(qtr1:qtr4)) %>%
  #reshape_msd(clean = TRUE) %>%
  group_by(operatingunit) %>%
  mutate(country_targ = sum(targets, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(operatingunit, snu1, snu1uid, #countryname,
           primepartner, fundingagency) %>%

  summarise(cumulative = sum(cumulative, na.rm = TRUE),
            APR = cumulative / first(country_targ)) %>%
  ungroup() %>%
  mutate(primepartner = paste0(primepartner, " (", fundingagency, ")"))



cntry_peds <- peds_psnu %>%
  filter(fiscal_year == "2021",
         indicator == "TX_CURR",
         standardizeddisaggregate == "Age/Sex/HIVStatus",
         !fundingagency %in% c("Dedup"),
         !primepartner %in% c("TBD")) %>%
  filter(!trendsfine %in% c("15-19","20-24","25-29",
                            "30-34","35-39","40-49","50+")) %>%
  glamr::clean_agency() %>%
  group_by(fiscal_year, operatingunit, snu1, countryname,
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
  group_by(operatingunit, snu1, snu1uid, #countryname,
           primepartner, fundingagency) %>%
  summarise(val = sum(val, na.rm = TRUE),
            share = val / first(country_val),
            primeshare = primepartner_val/first(country_val)) %>%
  ungroup() %>%
  #mutate(primepartner = paste0(primepartner, " (", fundingagency, "(", round(primeshare*100, 2),  "%","))"))
  mutate(primepartner = paste0(fundingagency, "-",
                               primepartner,
                               " (",
                               round(primeshare*100, 1),
                               "%)"))


#LOOK AT THIS

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

#TRY this

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
  mutate(country_targ = sum(targets, na.rm = TRUE)) %>%
  ungroup() %>%
  #mech_name = paste0(mech_name, " (", fundingagency, ")"),
  #mech_code = paste0(mech_code, " (", fundingagency, ")")) %>%
  group_by(operatingunit, snu1, snu1uid, #mech_name, mech_code,
           primepartner, fundingagency) %>%
  #I dont know if I understand what first() does
  summarise(cumulative = sum(cumulative, na.rm = TRUE),
            APR = cumulative / first(country_targ)) %>%
  ungroup() %>%
  mutate(primepartner = paste0(primepartner, " (", fundingagency, ")"))


#################3


map_apr<- function(df_apr, ou,
            agencies = "CDC") {

  print(ou)

  df_cntry_2 <- df_apr %>%
    filter(operatingunit == ou,
           #fundingagency %in% c("USAID", "CDC"))
    fundingagency %in% agencies)

  nmechs <- df_cntry_2 %>%
    distinct(primepartner) %>%
    nrow()# %>%
    #ncol()

 ncols <- round(nmechs / 2)

  ou <-  ifelse(ou == "Cote d'Ivoire", "Ivory Coast",
                ifelse(ou == "Eswatini", "Swaziland",
                       ifelse(ou == "Democratic Republic of Congo", "Democratic Republic of the Congo",
                              ifelse(ou == "Tanzania", "United Republic of Tanzania",
                                     ifelse(ou == "South Sudan", "Sudan", ou)))))

  map2_geo <- st_as_sf(gis_vc_sfc$VcPepfarPolygons) %>%
    left_join(df_cntry_2, by = c("uid" = "snu1uid")) %>%
    dplyr::filter(!is.na(APR)) # BK => This will remove rows with no mer data

  basemap <- terrain_map(countries = ou,
                         terr = si_path(type = "path_raster"),
                         mask = TRUE)

  #ou2 <- ifelse(ou == "Cote d'Ivoire", "Ivory Coast", ou)

  cntry_adm1 <- gisr::get_admin1(ou)

  map2 <- basemap +
    geom_sf(data = map2_geo  %>% filter(!is.na(APR) & !is.infinite(APR)),
            aes(fill = APR), lwd = .2, color = grey10k, stroke = 0.5, alpha = 0.8) +
    geom_sf_text(data = map2_geo,
                 aes(label = percent(APR, .1)),
                 color = usaid_black, size = 1) +
    geom_sf(data = cntry_adm1, fill = NA, lwd = .2, color = grey30k) +
    scale_fill_si(palette = "moody_blues", discrete = FALSE,
                  alpha = 0.95, reverse = TRUE,
                  breaks = c(0,.5,1.00,1.5,2),
                  limits = c(0, 2),
                  labels = percent) +
    si_style_map() +
    theme(
      legend.position =  "bottom",
      legend.key.width = ggplot2::unit(1, "cm"),
      legend.key.height = ggplot2::unit(.5, "cm")) +
    ggtitle(paste0(agencies, " - ", ou, " | % of TX_CURR FY20 Target Achievements by IP and SNU1")) +
    # theme(plot.title = element_text(size = 9, family = "Source Sans Pro", face=1))+
    #facet_wrap(~mech_code, ncol = ncols, labeller = label_wrap_gen(20))
    facet_wrap(~primepartner, ncol=ncols, labeller = label_wrap_gen(20))


  print(map2)
  return(map2)
}


#BY AGENCY
cntry_2_peds %>%
  filter(!str_detect(operatingunit, " Region$"),
         !is.na(APR)) %>%
  distinct(operatingunit) %>%
  pull() %>%
  nth(1) %>%
  map(.x, .f = ~map_apr(df_apr = cntry_2_peds,
                        ou = .x,
                        agencies = "USAID"))

cntry_2_peds %>%
  filter(!str_detect(operatingunit, " Region$"),
         !is.na(APR)) %>%
  distinct(operatingunit) %>%
  pull() %>%
  nth(11) %>%
  map(.x, .f = ~map_apr(df_apr = cntry_2_peds,
                          ou = .x,
                          agencies = "CDC"))

ggsave(here("~/Github/training/Graphics/map2_peds", "MAP2_Kenya_CDC_TX_CURR TARGETS by IP.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")






#-----COMBINED--------------------

map_apr_2 <- function(df_apr, ou) {

  print(ou)

  df_cntry_2 <- df_apr %>%
    filter(operatingunit == ou,
           fundingagency %in% c("USAID", "CDC"))
           #fundingagency %in% agencies)

  nmechs <- df_cntry_2 %>%
    # distinct(mech_name) %>%
    distinct(primepartner) %>%
    nrow() %>%
    ncol()

 # ncols <- round(nmechs / 3)

    ou <-  ifelse(ou == "Cote d'Ivoire", "Ivory Coast",
                  ifelse(ou == "Eswatini", "Swaziland",
                         ifelse(ou == "Democratic Republic of Congo", "Democratic Republic of the Congo",
                                ifelse(ou == "Tanzania", "United Republic of Tanzania",
                                       ifelse(ou == "South Sudan", "Sudan", ou)))))

  map2_geo <- st_as_sf(gis_vc_sfc$VcPepfarPolygons) %>%
    left_join(df_cntry_2, by = c("uid" = "snu1uid")) %>%
    dplyr::filter(!is.na(APR)) # BK => This will remove rows with no mer data

  basemap <- terrain_map(countries = ou,
                         terr = si_path(type = "path_raster"),
                         mask = TRUE)

  ou2 <- ifelse(ou == "Cote d'Ivoire", "Ivory Coast", ou)

  cntry_adm1 <- gisr::get_admin1(ou2)

  map2a <- basemap +
    geom_sf(data = map2_geo,
            aes(fill = APR), lwd = .2, color = grey10k) +
    geom_sf_text(data = map2_geo,
                 aes(label = percent(APR, .1)),
                 color = usaid_darkgrey, size = 1) +
    geom_sf(data = cntry_adm1, fill = NA, lwd = .2, color = grey30k) +
    scale_fill_si(palette = "moody_blues", discrete = FALSE,
                  alpha = 0.9, reverse = TRUE,
                  breaks = c(0,0.25,0.5,0.75, 1.00,1.25,1.5, 1.75, 2.0),
                  limits = c(0, 2.00),
                  labels = percent) +
    si_style_map() +
    theme(
      legend.position =  "bottom",
      legend.key.width = ggplot2::unit(1, "cm"),
      legend.key.height = ggplot2::unit(.5, "cm")) +
    #ggtitle(paste0(ou, " | % of TX_CURR FY20 Target Achievements by IP and SNU1")) +
    # theme(plot.title = element_text(size = 9, family = "Source Sans Pro", face=1))+
    #facet_wrap(~mech_code, ncol = ncols, labeller = label_wrap_gen(20))
    facet_wrap(~primepartner, labeller = label_wrap_gen(30))


  print(map2a)


  map2b <- basemap +
    geom_sf(data = map2_geo %>% filter(!is.na(APR), !is.infinite(APR), fundingagency == "CDC"),
            aes(fill = APR), lwd = .2, color = grey10k) +
    geom_sf_text(data = map2_geo %>% filter(!is.na(APR), !is.infinite(APR), fundingagency == "CDC"),
                 aes(label=percent(APR, .1)),
                 color=usaid_lightgrey, size = 2)+
    geom_sf(data = cntry_adm1, fill = NA, lwd = .2, color = grey30k) +
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
    facet_wrap(~primepartner, ncol = 2, labeller = label_wrap_gen(20))

  print(map2b)


  map2c <- (map2a + map2b) +
    plot_layout(ncol = 2, widths = c(1, 2),
                guides = 'collect') +
    plot_annotation(
      title = (paste0(ou, " | % of TX_CURR FY20 Target Achievements by IP and SNU1")),
      caption = paste0("OHA/SIEI - MSD", Sys.Date()),
      theme = theme(plot.title = element_text(hjust = .5), legend.position = 'bottom')
    )
  print(map2c)

  return(map2c)
}



min(cntry_peds$share)
max(cntry_peds$share)


cntry_2_peds %>%
  filter(!str_detect(operatingunit, " Region$"),
         !is.na(APR)) %>%
  distinct(operatingunit) %>%
  pull() %>%
  nth(16) %>%
  map(.x, .f = ~map_apr(df_apr = cntry_2_peds,
                            ou = .x),
      fundingagency %in% c("CDC" & "USAID"))

ggsave(here("~/Github/training/Graphics/map2_peds", "MAP2_Nigeria_ TX_CURR TARGETS by IP.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")






#BY AGENCY



# ------------------------------





