##  PROJECT: Q3 Review
##  AUTHOR:  B.Kagniniwa, G.Sarfaty, T. Essam | USAID
##  PURPOSE: Proxy OVC Program Coverage of TX_CURR <20
##  LICENCE: MIT
##  DATE:    2020-11-17



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

zam1 <- get_adm_boundaries("ZMB", adm_level = 1, geo_path = dir_geo) %>%
  st_as_sf() %>%
  select(country = name_0, province = name_1)



# MER Data Munge ---------------------------------------------------------------------------------

# proxy coverage of OVC program of pediatric HIV
#OVC_HIVSTAT_POS compared with TX_CURR 0-19 years (in OVC SNUs)<19

proxy_ovc_cov<-df %>%
  filter(fiscal_year=="2020",
         indicator =="OVC_HIVSTAT_POS" & standardizeddisaggregate=="Total Numerator" |
           indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus",
         operatingunit %in% c("Zambia")) %>%
  filter(!trendsfine %in% c("20-24","25-29","30-34","35-39","40-49","50+"),
         !str_detect(psnu,"_Military")) %>%
  # filter(!trendsfine %in% c("15-19", "20-24","25-29","30-34","35-39","40-49","50+")) %>% #under 15
  mutate(fundingagency=case_when(
    fundingagency=="HHS/CDC" ~ "CDC",
    TRUE ~ fundingagency),
    indicator=paste0(indicator,"_",fundingagency)) %>%
  group_by(fiscal_year,operatingunit,psnuuid,psnu,indicator) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  reshape_msd(clean = TRUE) %>%
  select(-period_type) %>%
  spread(indicator, val) %>%
  rowwise() %>%
  mutate(TX_CURR=sum(c_across(TX_CURR_CDC:TX_CURR_USAID),na.rm = TRUE)) %>%
  mutate(flag=case_when(
    OVC_HIVSTAT_POS_USAID >0 & OVC_HIVSTAT_POS_CDC >0 ~ "mixed agency ovc"),
    proxy_coverage_USAID=case_when(
      OVC_HIVSTAT_POS_USAID >0 ~ OVC_HIVSTAT_POS_USAID/TX_CURR),
    proxy_coverage_CDC=case_when(
      OVC_HIVSTAT_POS_CDC >0 ~ OVC_HIVSTAT_POS_CDC/TX_CURR
    ),
    shortname=str_remove(psnu, "District")) %>%
  filter(period=="FY20Q2")



# GEO Data Joins
zam_geo<-st_as_sf(gis_5_sfc$Zambia) %>%
  left_join(proxy_ovc_cov, by = c("uid" = "psnuuid"))



# VIZ ------------------------------------------------------------------------------------

# Map
zam_map_cov20<-terrain_map(countries = "Zambia", terr_path = dir_terr, mask = TRUE) +
  geom_sf(data = zam_geo %>% filter(proxy_coverage_USAID >0), aes(fill = proxy_coverage_USAID), lwd = .2, color = grey10k) +
  geom_sf(data = zam1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_viridis_c(option="magma",direction=-1, labels=percent,
                       limit=c(0,1),oob=scales::squish)+
  si_style_map()+
  theme(
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm"))+
  ggtitle(label = "",
          subtitle = "FY20Q2 | USAID OVC Program Coverage Proxy of TX_CURR <20") #under 20



# Plot
dotplot <- proxy_ovc_cov %>%
  filter(proxy_coverage_USAID >0) %>%
  filter(proxy_coverage_USAID >0, is.na(flag)) %>%
  mutate(label = paste0(shortname, " (", OVC_HIVSTAT_POS_USAID, "/", TX_CURR, ")")) %>%
  ggplot2::ggplot(
    aes(x = reorder(label, proxy_coverage_USAID),
        y = proxy_coverage_USAID
    )) +
  geom_point(aes(size = TX_CURR, fill=proxy_coverage_USAID),
             color=grey50k,
             shape=21,
             show.legend = F) +
  scale_size_continuous(range=c(3,8))+
  scale_color_viridis_c(option="magma",
                        direction=-1,
                        aesthetics = c("fill"),
                        limit=c(0,1),oob=scales::squish)+
  geom_hline(aes(yintercept = .9),
             color = "gray70",
             size = 0.35,
             linetype="dashed",
             alpha=.8) +
  scale_y_continuous(position = "right", labels=percent,
                     limit=c(0,1),oob=scales::squish) +
  labs(x = "", y="") +
  coord_flip() +
  # annotate(geom = "text", x = 1, y = .41, label = "Circle Sized by TX_CURR",hjust="left",
  #          size=4, color=grey50k, family="Source Sans Pro")+
  annotate(geom="text", x=13, y=.88, label="90% threshold", hjust="right",
           size=4, color=grey50k, family="Source Sans Pro")+
  si_style_nolines()


print(dotplot)


# VIZ COMBINED & SAVED

(zam_map_cov20 + dotplot)+
  plot_layout(nrow = 1)+
  plot_annotation(
    title="",
    caption = "*NOTE: Scales are truncated to 100%
    Source: FY20Q3i MSD,
Proxy Coverage=OVC_HIV_STAT_POS USAID/TX_CURR age <20 All Agencies
Shown for PSNUs in which USAID is the only agengy with OVC programming",
    theme=theme(plot.caption = element_text(size=9, color=grey70k, family="Source Sans Pro"))) #under 20



ggsave(here("Graphics", "Zambia_OVC_TX_ProxyCoverage_under20_v2.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")

