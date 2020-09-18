##  PROJECT: Geospatial Analytics
##  AUTHOR:  B.Kagniniwa, G.Sarfaty, T. Essam | USAID
##  PURPOSE: Proxy OVC Program Coverage of TX_CURR <20, FY21 Targets
##  LICENCE: MIT
##  DATE:    2020-09-18



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

dir_geo <- "../../Boundaries"
dir_terr <- "../../Topography"
dir_merdata <- "../../DATIM"

dir_img <- here("Images")
dir_graphs <- here("Graphics")



# MER Data
file_psnu_im <- (list.files(path =dir_merdata,
                            pattern = "Genie",
                            recursive = TRUE,
                            full.names = TRUE))

df<-read_msd(file_psnu_im)


# GEO Data
gis_5_sfc <- list.files(dir_geo, pattern = ".*_5_.*.shp$", recursive = T, full.names = T) %>%
  set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
  map(read_sf)

zam1 <- get_adm_boundaries("ZMB", adm_level = 1, geo_path = dir_geo) %>%
  st_as_sf() %>%
  select(country = name_0, province = name_1)



# MER Data Munge ---------------------------------------------------------------------------------

# alignment of OVC_SERV <18 T & TX_CURR <20 T, FY21

proxy_ovc_cov<-df %>%
  filter(fiscal_year=="2021",
         fundingagency=="USAID",
         indicator =="OVC_SERV_UNDER_18" & standardizeddisaggregate=="Total Numerator" |
         indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus",
         operatingunit %in% c("Zambia")) %>%
  filter(!trendsfine %in% c("20-24","25-29","30-34","35-39","40-49","50+")) %>%
  group_by(fiscal_year,operatingunit,psnuuid,psnu,indicator) %>%
  summarise(across(starts_with("targ"), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  reshape_msd(clean = TRUE) %>%
  select(-period_type) %>%
  spread(indicator, val) %>%
  mutate(proxy_coverage=case_when(
    OVC_SERV_UNDER_18 >0 ~ OVC_SERV_UNDER_18/TX_CURR
  ),
  shortname=str_remove(psnu, "District")) %>%
  filter(!is.na(proxy_coverage))



# GEO Data Joins
zam_geo<-st_as_sf(gis_5_sfc$Zambia) %>%
  left_join(proxy_ovc_cov, by = c("uid" = "psnuuid"))



# VIZ ------------------------------------------------------------------------------------

# Map
zam_map_cov21<-terrain_map(countries = "Zambia", terr_path = dir_terr, mask = TRUE) +
  geom_sf(data = zam_geo %>% filter(proxy_coverage >0), aes(fill = proxy_coverage), lwd = .2, color = grey10k) +
  geom_sf(data = zam1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_viridis_c(option="magma",direction=-1, labels=percent)+
  si_style_map()+
  theme(
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm"))+
  ggtitle(label = "",
          subtitle = "")



# Plot
dotplot <- proxy_ovc_cov %>%
  mutate(label = paste0(shortname, " (", OVC_SERV_UNDER_18, "/", TX_CURR, ")")) %>%
  ggplot2::ggplot(
    aes(x = reorder(label, proxy_coverage),
        y = proxy_coverage
    )) +
  geom_point(aes(size = TX_CURR, fill=proxy_coverage),
             color=grey50k,
             shape=21,
             show.legend = F) +
  scale_size_continuous(range=c(3,8))+
  scale_color_viridis_c(option="magma",
                        direction=-1,
                        aesthetics = c("fill"))+
  # geom_hline(aes(yintercept = .9),
  #            color = "gray70",
  #            size = 0.35,
  #            linetype="dashed",
  #            alpha=.8) +
  scale_y_continuous(position = "right", labels=percent) +
  labs(x = "", y="") +
  coord_flip() +
  annotate(
    geom = "curve", x = 6.25, y = 11, xend = 6, yend = 9.8,
    curvature = .3, arrow = arrow(length = unit(4, "mm")),
    color=grey50k
  ) +
  annotate(geom = "text", x = 6.25, y = 11, label = "Circle Sized by TX_CURR",hjust="left",
           size=4, color=grey50k, family="Gill Sans MT")+
  # annotate(geom="text", x=13, y=.88, label="90% threshold", hjust="right",
  #          size=4, color=grey50k, family="Gill Sans MT")+
  si_style_nolines()

print(dotplot)


# VIZ COMBINED & SAVED

(zam_map_cov21 + dotplot)+
  plot_layout(nrow = 1)+
  plot_annotation(
    title="FY21 | % overlap of OVC_SERV_UNDER_18 & TX_CURR <20 targets",
    caption = "Source: Genie 2020-09-18 - USAID Only,
OVC_SERV_UNDER18/TX_CURR <20 targets")




ggsave(here("Graphics", "Zambia_OVC_TX_FY21_overlap_dot.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")


