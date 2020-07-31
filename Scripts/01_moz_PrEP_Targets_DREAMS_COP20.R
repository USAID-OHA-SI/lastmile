## PROJECT:  PEPFAR Geospatial Analytics
## AUTHOR:   B.Kagniniwa, G.Sarfaty | USAID
## LICENSE:  MIT
## PURPOSE:  MOZ - PREP TARGETS COP20
## Date:     2020-07-28

# LIBRARIES

library(tidyverse)
library(readxl)
library(vroom)
library(foreign)
library(sf)
library(gisr)
library(glitr)
library(glamr)
library(janitor)
library(scales)
library(RColorBrewer)
library(here)
library(ICPIutilities)
library(patchwork)
library(extrafont)
library(stringr)

# REQUIRED

source("./Scripts/00_Setup.R")

# GLOBALS -----------------------

user <- ""
key <- ""

country = "Mozambique"

# files
file_prep <- "Genie_SITE_IM_Mozambique_PrEP_FY21T.txt"

file_ovc <- "OVC DREAMS YCM Districts_Sites for COP20.xlsx"

file_psnu_txt3 <- "MER_Structured_Datasets_PSNU_IM_FY18-20_20200626_v2_1.txt"

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


moz_districts <- (list.files(path =dir_geo,
                             pattern = file_districts,
                             recursive = TRUE,
                             full.names = TRUE) %>%
                    map(read_sf))[[1]]


#make shp a spatial feature file
moz_districts<- moz_districts %>%
  select(uid, country = level3name, province = level4name, district = level5name) %>%
  mutate(
    district = case_when(
      district == "Chonguene" ~ "Chongoene",
      district == "Cidade De Xai-Xai" ~ "Xai-Xai",
      district == "Chimoio" ~ "Cidade De Chimoio",
      district == "ManhiÒ«a" ~ "Manhica",
      district == "Cidade Da Matola" ~ "Matola",
      district == "Cidade Da Beira" ~ "Beira",
      district == "Maganja Da Costa" ~ "Maganja da Costa",
      district == "Cidade De Tete" ~ "Tete",
      district == "Chókwe" ~ "Chokwe",
      district == "Cidade De Quelimane" ~ "Quelimane",
      district == "Cidade De Chimoio" ~ "Chimoio",
      TRUE ~ district
    )
  ) 

moz_districts<-moz_districts%>% 
  st_as_sf() 


## MOZ COP20 PREP TARGETS
prep<-read_msd(here("Data",file_prep)) %>% 
  filter(fundingagency=="USAID",
         standardizeddisaggregate=="Total Numerator") %>% 
  group_by(psnu,psnuuid,indicator) %>% 
  summarize_at(vars(targets:cumulative),sum,na.rm=TRUE) %>% 
  ungroup()

 
## DREAMS districts

shts <- excel_sheets(path = here("Data", file_ovc))

dfs <- shts[-1] %>%
  set_names(str_replace_all(., " ", "_")) %>%
  map(read_excel, path = here("Data", file_ovc))

## MOZ DREAMS DISTRICTS
df_dreams <- dfs$`DREAMS_COP20_new_+_old_district` %>%
  janitor::clean_names() %>%
  mutate_all(trimws, which = "both") %>%
  mutate(
    new_cop_20 = as.factor(new_cop_20),
    province = gsub(" Prov", "", province)
  ) %>%
  arrange(province, district)

df_dreams_districts <- df_dreams %>%
  left_join(moz_districts, by = "district") %>%
  dplyr::rename(province = province.x) %>%
  dplyr::select(-province.y) %>%
  st_as_sf()

df_dreams_districts %>%
  ggplot() +
  geom_sf(aes(fill=new_cop_20), lwd = .5, color = grey20k) +
  si_style_map()


#join prepped MER data to shp
moz_prepT <- moz_districts %>%
  left_join(prep, by = c("uid" = "psnuuid")) %>%
  filter(!is.na(targets))



# VIZ -------------------------------------------------------

## PrEP TARGET BAR CHART AND MAP
moz_prep_target_bar <- prep %>% 
  mutate(label = paste0((psnu), " (", targets, ")")) %>% 
  ggplot(aes(reorder(label, targets), targets, fill = targets))+
  geom_col(show.legend = F) +
  geom_hline(yintercept = 1000, lwd = .2, color = grey10k) +
  geom_hline(yintercept = 2000, lwd = .2, color = grey10k) +
  geom_hline(yintercept = 3000, lwd = .2, color = grey10k) +
  scale_fill_gradient2(
    low = "yellow",
    high = "brown")+
  coord_flip() +
  labs(x = "", y="") +
  si_style_nolines()


moz_prep_target_map<- terrain_map(countries = country, terr_path = dir_terr, mask = TRUE) +
  geom_sf(data = moz_prepT, aes(fill = targets), lwd = .2, color = grey10k) +
  geom_sf(data = moz1, fill = NA, lwd = .2, color = grey30k) +
  geom_sf_text(data = moz1, aes(label = province), color = grey80k, size = 3) +
  scale_fill_gradient2(
    low = "yellow",
    high = "brown")+
  si_style_map() +
  theme(
    legend.position =  c(.9, .2),
    legend.direction = "vertical",
    legend.key.width = ggplot2::unit(.5, "cm"),
    legend.key.height = ggplot2::unit(1, "cm")
  )

(moz_prep_target_map + moz_prep_target_bar) +
  plot_layout(widths = c(1,1)) +
  plot_annotation(
    title = "MOZAMBIQUE - USAID PrEP_NEW COP20 Targets",
    caption = "OHA/SIEI")

ggsave(here(dir_graphs, "MOZ - PrEP Targets COP20.png"),
       plot = last_plot(), scale = 1.2,
       dpi = 310, width = 10, height = 7, units = "in")


## PrEP TARGET MAP AND DREAMS DISTRICTS MAP
moz_prep_target_map<- terrain_map(countries = country, terr_path = dir_terr, mask = TRUE) +
  geom_sf(data = moz_prepT, aes(fill = targets), lwd = .2, color = grey10k) +
  geom_sf(data = moz1, fill = NA, lwd = .2, color = grey30k) +
  geom_sf_text(data = moz1, aes(label = province), color = grey80k, size = 3) +
  scale_fill_gradient2(
    low = "yellow",
    high = "brown")+
  ggtitle("USAID COP20 PrEP Targets")+
  si_style_map() +
  theme(
    legend.position =  c(.9, .2),
    legend.direction = "vertical",
    legend.key.width = ggplot2::unit(.5, "cm"),
    legend.key.height = ggplot2::unit(1, "cm")
  )
  

moz_dreams_districts_m<-terrain_map(countries = country, terr_path = dir_terr, mask = TRUE) +
  geom_sf(data = df_dreams_districts, aes(fill = new_cop_20), size = .3, color = grey10k, alpha= .6, show.legend = F) +
  geom_sf(data = moz1, fill = NA, size = .3, linetype = "dotted") +
  geom_sf_text(data = moz1, aes(label = province), size = 3, color = grey90k) +
  scale_fill_manual(values = c(USAID_medblue, USAID_red)) +
  ggtitle("DREAMS Districts \nNew districts as of COP20 are in red")+
  si_style_map()
  


(moz_prep_target_map + moz_dreams_districts_m) +
  plot_layout(widths = c(1,1)) +
  plot_annotation(
    title = "MOZAMBIQUE - USAID PrEP_NEW COP20 Targets & DREAMS DISTRICTS",
    caption = "OHA/SIEI")

ggsave(here(dir_graphs, "MOZ - COP20 PrEP Targets & DREAMS DISTRICTS.png"),
       plot = last_plot(), scale = 1.2,
       dpi = 310, width = 10, height = 7, units = "in")
