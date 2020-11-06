##  PROJECT: Geospatal Analytics
##  AUTHOR:  B.Kagniniwa, G.Sarfaty, T. Essam | USAID
##  PURPOSE: % ompletion of primary package among those in DREAMS 13+ mo
##  LICENCE: MIT
##  DATE:    2020-09-17
##  UPDATED: 2020-11-06

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

# Dependencies
source("./Scripts/00_Geo_Utilities.R")
source("./Scripts/00_DREAMS_Utilities.R")

# Globals
# Globals
dir_data <- here("Data")
dir_dataout <- here("Dataout")

dir_geo <- "C:/Users/gsarfaty/Documents/GIS/DATA/Boundaries"
dir_terr <- "C:/Users/gsarfaty/Documents/GIS/DATA/Topography"
dir_merdata <- "C:/Users/gsarfaty/Documents/DATIM"

dir_img <- here("Images")
dir_graphs <- here("Graphics")

# Update paths
source("../_setup/00_Setup.R")


# MER Data - get the latest MSD PSNU x IM file
# This should return
# Q3 file => MER_Structured_Datasets_PSNU_IM_FY18-21_20200918_v2_1.zip
file_psnu_im <- list.files(
    path = dir_merdata,
    pattern = "Structured_.*_PSNU_IM_.*_\\d{8}_v.*.zip",
    recursive = FALSE,
    full.names = TRUE
  ) %>%
  sort() %>%
  last()

# Read data
df <- read_msd(file_psnu_im)

# GEO Data
gis_5_sfc <- list.files(dir_geo,
                        pattern = ".*_5_.*.shp$",
                        recursive = T,
                        full.names = T) %>%
  set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
  map(read_sf)

zam1 <- get_adm_boundaries("ZMB",
                           adm_level = 1,
                           geo_path = dir_geo) %>%
  st_as_sf() %>%
  select(country = name_0, province = name_1)



# MER Data Munge ---------------------------------------------------------------------------------


AGYW_prev_time <- df %>%
    filter(fiscal_year == "2020",
           indicator == "AGYW_PREV",
           !is.na(otherdisaggregate),
           operatingunit %in% c("Zambia")) %>%
    separate(otherdisaggregate, c("drop","completion"),sep = ", ") %>%
    group_by(fiscal_year,operatingunit,psnuuid,psnu,
             indicator,completion,otherdisaggregate_sub) %>%
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-c(fiscal_year,qtr1,qtr3,qtr4)) %>%
    rename(time_dreams = otherdisaggregate_sub) %>%
    mutate(
      shortname = str_remove(psnu, "District"),
      time_dreams = case_when(
        time_dreams %in% c("<6 Months in DREAMS",
                           "07-12 Months in DREAMS") ~ "Less than 1 year",
        time_dreams %in% c("13-24 Months in DREAMS",
                           "25+ Months in DREAMS") ~ "13+ months"),
      completion = case_when(
        completion %in% c("DREAMS Fully Completed",
                          "DREAMS Fully Completed and Secondary") ~ "completed",
        completion %in% c("DREAMS Not Completed",
                          "DREAMS Only Secondary Completed") ~ "not complete")
    ) %>%
    filter(time_dreams == "13+ months") %>%
    group_by(psnuuid,psnu,shortname,completion) %>%
    summarize_at(vars(qtr2), sum, na.rm = TRUE) %>%
    spread(completion, qtr2) %>%
    rowwise() %>%
    mutate(total = sum(c_across(completed:`not complete`), na.rm = TRUE)) %>%
    mutate(across(completed:`not complete`, ~ . / total)) %>%
    rename(completed_13plus = completed,
           incomplete_13plus = `not complete`,
           total_13plus = total) %>%
    gather(indicator, val, completed_13plus:total_13plus)


prct_total <- df %>%
    filter(fiscal_year == "2020",
           indicator == "AGYW_PREV",
                   !is.na(otherdisaggregate),
                   operatingunit %in% c("Zambia")) %>%
    rename(time_dreams = otherdisaggregate_sub) %>%
    mutate(shortname = str_remove(psnu, "District"),
           time_dreams = case_when(time_dreams %in% c("<6 Months in DREAMS", "07-12 Months in DREAMS") ~ "Less than 1 year",
                                 time_dreams %in% c("13-24 Months in DREAMS", "25+ Months in DREAMS") ~ "13+ months")) %>%
    group_by(fiscal_year,shortname,psnuuid,psnu,indicator,time_dreams) %>%
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-c(fiscal_year,qtr1,qtr3,qtr4)) %>%
    spread(time_dreams,qtr2) %>%
    rowwise() %>%
    mutate(total = sum(c_across(`13+ months`:`Less than 1 year`), na.rm = TRUE)) %>%
    mutate(across(`13+ months`:`Less than 1 year`, ~ . / total)) %>%
    gather(indicator,val,`13+ months`:total)


totals <- AGYW_prev_time %>%
    filter(indicator %in% c("total_13plus", "completed_13plus")) %>%
    spread(indicator, val) %>%
    select(c(psnuuid, total_13plus, completed_13plus))


AGYW <- rbind(AGYW_prev_time, prct_total) %>%
    full_join(totals, by = "psnuuid")



# GEO Data Joins

zam_geo <- st_as_sf(gis_5_sfc$Zambia) %>%
  left_join(AGYW, by = c("uid" = "psnuuid"))

# Labels dataset
zam_labels = zam_geo %>%
  filter(indicator == "completed_13plus" &
         (completed_13plus >= .90 | completed_13plus < .30)) %>%
  mutate(label = paste0(shortname, " (",
                        round(completed_13plus * 100, 1),
                        "%)"))


# VIZ ------------------------------------------------------------------------------------

# Map
zam_map_completion <- terrain_map(countries = "Zambia",
                                  terr_path = dir_terr,
                                  mask = TRUE) +
  geom_sf(data = zam_geo %>%
            filter(indicator == "completed_13plus", !is.na(val)),
          aes(fill = val, label = completed_13plus), lwd = .2, color = grey10k) +
  geom_sf(data = zam1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_viridis_c(option = "magma",
                       direction = -1,
                       labels = percent_format(accuracy = 1)) +
  si_style_map() +
  theme(
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm")) +
  ggtitle("% who completed at least primary package \n after being in DREAMS 13+ months")

print(zam_map_completion)

# Map for comms
zam_map_for_comms <- terrain_map(countries = "Zambia",
                                  terr_path = dir_terr,
                                  mask = TRUE) +
  geom_sf(data = zam_geo %>%
            filter(indicator == "completed_13plus", !is.na(val)),
          aes(fill = val, label = completed_13plus), lwd = .2, color = grey10k) +
  geom_sf(data = zam1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_viridis_c(option = "magma",
                       direction = -1,
                       labels = percent_format(accuracy = 1)) +
  geom_sf_text(data = zam1,
               aes(label = province),
               size = 4, color = grey50k) +
  geom_sf_text(data = zam_labels,
               aes(label = label),
               size = 3, color = grey90k, weight = "bold", nudge_x = .6, nudge_y = .2) +
  si_style_map() +
  labs(caption = "FY20Q2; Source: FY20Q3c MSD") +
  theme(
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(1.5, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm")
  )


print(zam_map_for_comms)


ggsave(here("Graphics", "Zambia_DREAMS_percentcompletion_13plust_months_comms2.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")

# Plot
dotplot <- AGYW %>%
  mutate(label = paste0(shortname, " (", total_13plus, ")")) %>%
  filter(indicator=="completed_13plus") %>%
  ggplot2::ggplot(
    aes(x = reorder(label, val),
        y = val
    )) +
  geom_point(aes(size = total_13plus, fill=val),
             color=grey50k,
             shape=21,
             show.legend = F) +
  scale_size_continuous(range=c(3,12))+
  scale_color_viridis_c(option="magma",
                        direction=-1,
                        aesthetics = c("fill"))+
  geom_hline(aes(yintercept = .9),
             color = "gray70",
             size = 0.35,
             linetype="dashed",
             alpha=.8) +
  scale_y_continuous(position = "right", labels=percent,
                     limits=c(0,1),
                     breaks = seq(0,1,0.5)) +
  labs(x = "", y="") +
  coord_flip() +
  si_style_xgrid()+
  ggtitle("% who completed at least primary package \n after being in DREAMS 13+ months")


print(dotplot)



prct_total_dreams<-AGYW %>%
    filter(indicator %in% c("13+ months", "Less than 1 year")) %>%
    ggplot(aes(x=reorder(shortname,completed_13plus),
               y=val,
               fill=factor(indicator,levels=c("Less than 1 year","13+ months")))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values=c(grey20k,grey40k))+
    geom_text(data=AGYW %>% filter(indicator=="13+ months"), aes(label=percent(val,1)),
              position=position_stack(vjust=0.75), color="white")+
    coord_flip()+
    labs(x = "", y="")+
    si_style_nolines()+
    theme(legend.position="none",
          axis.text.x = element_blank())+
    ggtitle("% in DREAMS 13+ months out of \n total beneficaries")


print(prct_total_dreams)


# VIZ COMBINED & SAVED

(zam_map_completion + dotplot + prct_total_dreams) +
  plot_annotation(
    caption = "FY20Q2; Source: FY20Q3c MSD") &
    theme(plot.title = element_text(size = 9, family = "Gill Sans MT", face=1))



ggsave(here("Graphics", "Zambia_DREAMS_percentcompletion_13+months_v3.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")


