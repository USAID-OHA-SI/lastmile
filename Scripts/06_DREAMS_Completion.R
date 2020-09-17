##  PROJECT: Geospatal Analytics
##  AUTHOR:  B.Kagniniwa, G.Sarfaty, T. Essam | USAID
##  PURPOSE: % ompletion of primary package among those in DREAMS 13+ mo
##  LICENCE: MIT
##  DATE:    2020-09-17



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
                            pattern = "Structured",
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

#DREAMS COMPLETETION BY TIME IN DREAMS

# AGYW_PREV<-df %>%
#   filter(fiscal_year=="2020",
#          indicator =="AGYW_PREV",
#          !is.na(otherdisaggregate),
#          operatingunit %in% c("Zambia")) %>%
#   separate(otherdisaggregate, c("drop","completion"),sep=", ")%>%
#   group_by(fiscal_year,operatingunit,psnuuid,psnu,indicator,completion,otherdisaggregate_sub) %>%
#   summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
#   ungroup() %>%
#   select(-c(fiscal_year,qtr1,qtr3,qtr4)) %>%
#   rename(time_dreams=otherdisaggregate_sub) %>%
#   spread(completion, qtr2) %>%
#   mutate(shortname=str_remove(psnu, "District")) %>%
#   rowwise() %>%
#   mutate(total = sum(c_across(`DREAMS Fully Completed`:`DREAMS Only Secondary Completed`),na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(across(`DREAMS Fully Completed`:`DREAMS Only Secondary Completed`, ~ . / total)) %>%
#   gather(completion,value,`DREAMS Fully Completed`:`DREAMS Only Secondary Completed`)


AGYW_prev_time<-df %>%
    filter(fiscal_year=="2020",
           indicator =="AGYW_PREV",
           !is.na(otherdisaggregate),
           operatingunit %in% c("Zambia")) %>%
    separate(otherdisaggregate, c("drop","completion"),sep=", ")%>%
    group_by(fiscal_year,operatingunit,psnuuid,psnu,indicator,completion,otherdisaggregate_sub) %>%
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-c(fiscal_year,qtr1,qtr3,qtr4)) %>%
    rename(time_dreams=otherdisaggregate_sub) %>%
    mutate(shortname=str_remove(psnu, "District"),
           time_dreams=case_when(time_dreams %in% c("<6 Months in DREAMS", "07-12 Months in DREAMS") ~ "Less than 1 year",
                                 time_dreams %in% c("13-24 Months in DREAMS", "25+ Months in DREAMS") ~ "13+ months"),
           completion=case_when(completion %in% c("DREAMS Fully Completed","DREAMS Fully Completed and Secondary") ~ "completed",
                                completion %in% c("DREAMS Not Completed") ~ "not complete")) %>%
    filter(time_dreams=="13+ months") %>%
    group_by(psnuuid,psnu,shortname,completion) %>%
    summarize_at(vars(qtr2),sum,na.rm=TRUE) %>%
    spread(completion,qtr2) %>%
    rowwise() %>%
    mutate(total = sum(c_across(completed:`not complete`),na.rm = TRUE)) %>%
    mutate(across(completed:`not complete`, ~ . / total))



# GEO Data Joins
# zam_geo<-st_as_sf(gis_5_sfc$Zambia) %>%
#   left_join(AGYW_PREV, by = c("uid" = "psnuuid"))

zam_geo_13plus<-zam_geo<-st_as_sf(gis_5_sfc$Zambia) %>%
  left_join(AGYW_prev_time, by = c("uid" = "psnuuid"))



# VIZ ------------------------------------------------------------------------------------

# Map
zam_map_completion<-terrain_map(countries = "Zambia", terr_path = dir_terr, mask = TRUE) +
  geom_sf(data = zam_geo_13plus %>% filter(!is.na(completed)), aes(fill = completed), lwd = .2, color = grey10k) +
  geom_sf(data = zam1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_viridis_c(option="magma",direction=-1, labels=percent_format(accuracy = 1))+
  si_style_map()+
  theme(
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm"))

print(zam_map_completion)



# Plot
dotplot <- AGYW_prev_time %>%
  mutate(label = paste0(shortname, " (", total, ")")) %>%
  ggplot2::ggplot(
    aes(x = reorder(label, completed),
        y = completed
    )) +
  geom_point(aes(size = total, fill=completed),
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
  scale_y_continuous(position = "right", labels=percent) +
  labs(x = "", y="") +
  coord_flip() +
  si_style_nolines()

print(dotplot)


# VIZ COMBINED & SAVED

(zam_map_completion + dotplot)+
  plot_layout(nrow = 1)+
  plot_annotation(
    title="% who completed at least primary package after being in DREAMS 13+ months",
    caption = "FY20Q2; Source: FY20Q3i MSD") &
    theme(plot.title = element_text(size = 12, family = "Gill Sans MT"))



ggsave(here("Graphics", "Zambia_DREAMS_percentcompletion_13+months.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")


