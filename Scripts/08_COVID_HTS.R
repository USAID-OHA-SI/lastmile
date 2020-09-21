##  PROJECT: Geospatal Analytics
##  AUTHOR:  B.Kagniniwa, G.Sarfaty, T. Essam | USAID
##  PURPOSE: COVID CONTEXT
##  LICENCE: MIT
##  DATE:    2020-09-17

# Libraries
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
library(here)
library(ICPIutilities)
library(googlesheets4)

# Globals
dir_data <- here("Data")
dir_dataout <- here("Dataout")

dir_geo <- "../../Boundaries"
dir_terr <- "../../Topography"
dir_merdata <- "../../DATIM"
dir_zaf<-"../../MSD_genie"



dir_img <- here("Images")
dir_graphs <- here("Graphics")

# DATA ----------------------------------------------------------------------

# Geo Data
gis_4_sfc <- list.files(dir_geo, pattern = ".*_4_.*.shp$", recursive = T, full.names = T) %>%
  set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
  map(read_sf)



# COVID-Cases
covid_cases<-read_csv(here("Data", "Provincial_COVID_Data.csv")) %>%
    mutate(cumulative_case_per100k=(cumulative_cases/population)*100000,
           cases_7day_per100k=(cases_seven_day_average/population)*100000) %>%
    mutate(province=case_when(
        str_detect(province,"Eastern") ~ "ec Eastern Cape Province",
        str_detect(province,"Free") ~ "fs Free State Province",
        str_detect(province, "Gauteng") ~ "gp Gauteng Province",
        str_detect(province, "KwaZulu") ~ "kz KwaZulu-Natal Province",
        str_detect(province, "Limpopo") ~ "lp Limpopo Province",
        str_detect(province, "Mpumalanga") ~ "mp Mpumalanga Province",
        str_detect(province, "North West") ~  "nw North West Province",
        str_detect(province, "Northern") ~ "nc Northern Cape Province",
        str_detect(province, "Western Cape") ~ "wc Western Cape Province",
        TRUE ~ province
    )) %>%
    filter(date==as.Date("2020-06-30"),
           !province=="Unknown")

cases_geo<-st_as_sf(gis_4_sfc$SouthAfrica) %>%
    left_join(covid_cases, by = c("orgunit_na" = "province")) %>%
    mutate(label=str_remove_all(orgunit_na,"Province"),
           label2=case_when(
               orgunit_na=="wc Western Cape Province" ~ "Western Cape",
               TRUE ~ "NULL"
           ))

# MER Data
file_zaf <- (list.files(path =dir_zaf,
                            pattern = "09-16",
                            recursive = TRUE,
                            full.names = TRUE))

df<-read_msd(file_zaf)


df_hts<-df %>%
  filter(fiscal_year=="2020",
         indicator %in% c("HTS_TST"),
         standardizeddisaggregate %in% c("Total Numerator"),
         operatingunit %in% c("South Africa"),
         priority_district=="YES") %>%
  group_by(fiscal_year,operatingunit,snu1uid,snu1,indicator) %>%
  summarize_at(vars(targets:cumulative),sum,na.rm=TRUE) %>%
  ungroup() %>%
  reshape_msd("long") %>%
  filter(str_detect(period,"q")) %>%
  spread(period,val) %>%
  mutate(q2toq3=((fy2020q3-fy2020q2)/fy2020q2))

zaf_geo<-st_as_sf(gis_4_sfc$SouthAfrica) %>%
  left_join(df_hts, by = c("uid" = "snu1uid"))

# Viz --------------------------------------------------------------------------------------


cases_map <- terrain_map(countries = "South Africa", terr_path = dir_terr, mask = TRUE) +
    geom_sf(data = cases_geo, aes(fill = cumulative_case_per100k), lwd = .2, color = grey10k) +
    # geom_sf_text(data=cases_geo %>% filter(!label2=="NULL"), aes(label=label2), color=grey10k, size = 2.5)+
    scale_fill_viridis_c(option="magma",direction = -1, na.value = NA) +
    ggtitle("Number of Cumulative COVID-19 Cases Per 100K as of 30 June")+
    coord_sf() +
    si_style_map()


hts_map <- terrain_map(countries = "South Africa", terr_path = dir_terr, mask = TRUE) +
  geom_sf(data = zaf_geo, aes(fill = q2toq3), lwd = .2, color = grey30k) +
  scale_fill_viridis_c(option="magma",direction = 1, na.value = NA,
                       labels=percent_format(accuracy = 1))+
  ggtitle("% Change in HTS_TST | Q2 to Q3")+
  coord_sf() +
  si_style_map()


(cases_map + hts_map) +
  plot_layout(nrow=1) +
  plot_annotation(
    title = "South Africa COVID-19 Context",
    subtitle = "",
    caption = "OHA/SIEI"
  )

ggsave(here("Graphics", "SA_COVID_HTS.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
