#Mamedina
#01122021 - DRAFT PEDs Maps

#MAp 1 a map showing IPs by SNU by country
#what % of the FY21 peds TX_CURR targets each IP has

#Map 2: a map showing IPs by SNU by country what % of the peds TX_CURR APR20 results to targets
#each IP achieved (ie what % of targets were achieved) - from 0 - 100%?

#THEY want single map with USAID red and CDC blue and overlapping areas - they want IP and % chart next to the map


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
library(ggtext)
library(ggrepel)
library(tidytext)
library(ggflags)
library(glue)

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



# MER Data Munge ---------------------------------------------------------------------------------


# Global dataset
#double check to make sure there is no duplicate data
#MAY need to update with Q2
#check panorama to check on like guatemala and other countries that don't have PEDS data

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
  mutate(primepartner = paste0(primepartner, " (", round(primeshare*100, 2),  "%",")"))
  #mutate(primepartner = paste0(fundingagency, "-",
                       # primepartner,
                        #" (",
                        #round(primeshare*100, 1),
                        #"%)"))


#write.csv(cntry_peds,"C:\\Users\\STAR\\Documents\\Github\\peds_map1_txcurr.csv", row.names = FALSE)


#counting mechs
cntry_peds %>% filter(operatingunit == "Kenya", fundingagency == "CDC") %>% count(primepartner, mech_code, mech_name) %>% arrange(primepartner) %>% prinf()


#function 1
map_share <- function(df_peds, ou
                                  ) {

  print(ou)

  df_cntry1 <- df_peds %>%
    filter(operatingunit == ou,
           fundingagency %in% c("USAID"))

  df_cntry2 <- df_peds %>%
    filter(operatingunit == ou,
           fundingagency %in% c("CDC"))

  ou <-  ifelse(ou == "Cote d'Ivoire", "Ivory Coast",
                ifelse(ou == "Eswatini", "Swaziland",
                       ifelse(ou == "Democratic Republic of Congo", "Democratic Republic of the Congo",
                              ifelse(ou == "Tanzania", "United Republic of Tanzania",
                                     ifelse(ou == "South Sudan", "Sudan", ou)))))

  peds_geo <- st_as_sf(gis_vc_sfc$VcPepfarPolygons) %>%
    left_join(df_cntry1, by = c("uid" = "snu1uid")) %>%
    dplyr::filter(!is.na(share)) # BK => This will remove rows with no mer data

  peds_geo2 <- st_as_sf(gis_vc_sfc$VcPepfarPolygons) %>%
    left_join(df_cntry2, by = c("uid" = "snu1uid")) %>%
    dplyr::filter(!is.na(share))

  basemap <- terrain_map(countries = ou,
                         terr = si_path(type = "path_raster"),
                         mask = TRUE)

  cntry_adm1 <- gisr::get_admin1(ou)

  map1 <- basemap +
    geom_sf(data = peds_geo,
            fill = usaid_red, lwd = .2, color = grey10k, stroke = 1.5, alpha = 0.5) +
    geom_sf(data = peds_geo2,
            fill = usaid_blue, lwd = .2, color = grey10k, stroke = 1.5, alpha = 0.35) +
    geom_sf(data = cntry_adm1, fill = NA, lwd = .2, color = grey30k) +
    scale_colour_identity()+
    si_style_map() +
    theme(
      legend.position =  "bottom",
      legend.key.width = ggplot2::unit(1, "cm"),
      legend.key.height = ggplot2::unit(.5, "cm")) #+
      #legend.text =HOW DO I ADD A LEGEND HERE
   # ggtitle(paste0(ou, " | % of the FY21 PEDS TX_CURR Targets by IP and SNU1"))
    #theme(plot.title = element_text(size = 14, family = "Source Sans Pro", face=1))


  print(map1)


  bar_usaid <- df_cntry1 %>%
    ggplot(aes(x=reorder(primepartner,desc(-primeshare)),
               y=primeshare, fill=primeshare)) +
    geom_col(show.legend = F)+
    coord_flip()+
    scale_y_continuous(labels = percent)+
    scale_fill_si(palette = "usaid_red", discrete=FALSE, alpha=0.9, reverse = FALSE,
                  breaks = c(0,0.25,0.5),
                  limits = c(0,0.50),
                  labels=percent)+
    si_style_nolines()+
    labs(y="",
         x="",
         title='% Share by Implementing Partner | USAID',
         caption="MSD March 2021")+
    theme(axis.text.y = element_text(size=8, face = "bold"))



  bar_cdc <- df_cntry2 %>%
    ggplot(aes(x=reorder(primepartner,desc(-primeshare)),
               y=primeshare, fill=primeshare)) +
    geom_col(show.legend = F)+
    coord_flip()+
    scale_y_continuous(labels = percent)+
    scale_fill_si(palette = "usaid_blue", discrete=FALSE, alpha=0.9, reverse = FALSE,
                  breaks = c(0,0.25,0.5),
                  limits = c(0,0.50),
                  labels=percent)+
    si_style_nolines()+
    labs(y="",
         x="",
         title='% Share by Implementing Partner | CDC',
         caption="MSD March 2021")+
    theme(axis.text.y = element_text(size=8, face = "bold"))



  test <- (map1 | (bar_usaid / bar_cdc)) +
    plot_layout(ncol = 2, widths = c(1, 1),
                guides = 'collect') +
    plot_annotation(
      title = (paste0(ou, " | % Share of the FY21 PEDS TX_CURR Targets by IP and SNU1")),
      caption = paste0("OHA/SIEI - MSD", Sys.Date()),
      theme = theme(plot.title = element_text(hjust = .5, size = 14, face = "bold"), legend.position = 'bottom')
    )
  print(test)

  return(test)


  }


#shp_world <- ne_countries(country=NULL, returnclass="sf")
#shp_world %>%sf::st_set_geometry(NULL) %>% view()
#get_admin0(countries, scale = "medium", crs = 4326)
#gisr::get_admin0()
#look at gisr master utilities geo utilities

#min(cntry_peds$share)
#max(cntry_peds$share)

#ne_countries()
#library(rnaturalearthhires)
#library(rnaturalearth)


map1 <- cntry_peds %>%
  filter(!str_detect(operatingunit, " Region$"),
         !is.na(share)) %>%
  distinct(operatingunit) %>%
  pull() %>%
  nth(25) %>%
  map(.x, .f = ~map_share(df_peds = cntry_peds,
                          ou = .x),
                          fundingagency %in% c("CDC" & "USAID"))




ggsave(here("~/Github/training/Graphics/map1_peds", "MAP1_Zimbabwe_ex2_TX_CURR by IP.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")

#write.csv(cntry_peds,"C:\\Users\\STAR\\Documents\\Github\\peds_txcurr.csv", row.names = FALSE)


#Function 2 - USAID and CDC Maps Combined



#country priorities:
#Kenya, Tanzania,
#Malawi, Mozambique, Nigeria, South Africa, Uganda, Zambia, and Zimbabwe





#I want to make the titles long - how do I adjust this in the wrap?


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

#write.csv(cntry_2_peds,"C:\\Users\\STAR\\Documents\\Github\\peds_map2_txcurr.csv", row.names = FALSE)

#cntry_2_peds %>% distinct(operatingunit=="Tanzania")




#-----COMBINED--------------------



map_apr <- function(df_apr, ou) {

  print(ou)

  df_cntry_1 <- df_apr %>%
    filter(operatingunit == ou,
           fundingagency %in% c("USAID"))

  df_cntry_2 <- df_apr %>%
    filter(operatingunit == ou,
           fundingagency %in% c("CDC"))


    ou <-  ifelse(ou == "Cote d'Ivoire", "Ivory Coast",
                  ifelse(ou == "Eswatini", "Swaziland",
                         ifelse(ou == "Democratic Republic of Congo", "Democratic Republic of the Congo",
                                ifelse(ou == "Tanzania", "United Republic of Tanzania",
                                       ifelse(ou == "South Sudan", "Sudan", ou)))))

    map2_geo <- st_as_sf(gis_vc_sfc$VcPepfarPolygons) %>%
      left_join(df_cntry_1, by = c("uid" = "snu1uid")) %>%
      dplyr::filter(!is.na(APR))

    map2_geo2 <- st_as_sf(gis_vc_sfc$VcPepfarPolygons) %>%
      left_join(df_cntry_2, by = c("uid" = "snu1uid")) %>%
      dplyr::filter(!is.na(APR))

  basemap <- terrain_map(countries = ou,
                         terr = si_path(type = "path_raster"),
                         mask = TRUE)

  cntry_adm1 <- gisr::get_admin1(ou)


  map2 <- basemap +
    geom_sf(data = map2_geo, fill = usaid_red, lwd = .2, color = grey10k, stroke = 1.5, alpha = 0.5) +
    geom_sf(data = map2_geo2,
            fill = usaid_blue, lwd = .2, color = grey10k, stroke = 1.5, alpha = 0.35) +
    geom_sf(data = cntry_adm1, fill = NA, lwd = .2, color = grey30k) +
    scale_colour_identity() +
    si_style_map() +
    theme(
      legend.position =  "bottom",
      legend.key.width = ggplot2::unit(1, "cm"),
      legend.key.height = ggplot2::unit(.5, "cm"))


    #facet_wrap(~mech_code, ncol = ncols, labeller = label_wrap_gen(20))
    #facet_wrap(~primepartner, labeller = label_wrap_gen(30))


  print(map2)


  bar_usaid_2 <-
    map2_geo %>%
    ggplot(aes(x=reorder(primepartner,desc(-APR)),
               y=APR, fill=APR)) + #+ filter(!is.na(share)) +
    geom_col(show.legend = F)+
    coord_flip()+
    scale_y_continuous(labels = percent) +
    geom_vline(xintercept = 1,
               size = 1.2,
               color = "white") +
    scale_fill_si(palette = "denims",
                  discrete = TRUE,
                  reverse = FALSE,
                  label = percent) +
    scale_x_continuous(labels = percent) +
    labs(y="",
         x="",
         title='% Achievement by Implementing Partner | USAID',
         caption="MSD March 2021") +
    si_style_xline() +
    theme(legend.position = "none")


  print(bar_usaid_2)

  bar_cdc_2 <-
    map2_geo2 %>%
    ggplot(aes(x=reorder(primepartner,desc(-APR)),
               y=APR, fill=APR)) +
    geom_col(show.legend = F)+
    coord_flip()+
    scale_y_continuous(labels = percent) +
    geom_vline(xintercept = 1,
               size = 1.5,
               color = "white") +
    scale_fill_si(palette = "old_roses",
                  discrete = TRUE,
                  label = percent) +
    si_style_nolines() +
    labs(y="",
         x="",
         title='% Achievement by Implementing Partner | USAID',
         caption="MSD March 2021") +
    si_style_xline() +
    theme(legend.position = "none")

  print(bar_cdc_2)


  map <- (map2 | (bar_usaid_2 / bar_cdc_2)) +
    plot_layout(ncol = 2, widths = c(1, 1),
                guides = 'collect') +
    plot_annotation(
      title = (paste0(ou, " | % of TX_CURR FY20 Target Achievements by IP and SNU1")),
      caption = paste0("OHA/SIEI - MSD", Sys.Date()),
      theme = theme(plot.title = element_text(hjust = .5, size = 14, face = "bold"), legend.position = 'bottom')
    )
  print(map)

  return(map)


  }



##min(cntry_peds$share)
#max(cntry_peds$share)


cntry_2_peds %>%
  filter(!str_detect(operatingunit, " Region$"),
         !is.na(APR)) %>%
  distinct(operatingunit) %>%
  pull() %>%
  nth(13) %>%
  map(.x, .f = ~map_apr(df_apr = cntry_2_peds,
                            ou = .x),
      fundingagency %in% c("CDC" & "USAID"))

ggsave(here("~/Github/training/Graphics/map2_peds", "MAP2_Malawi_ex1 TX_CURR TARGETS by IP.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")


#--------------------------


ggplot(aes(y = reorder(primepartner, APR),
           x = APR,
           fill = APR)) +
  geom_col(aes(x = 1),
           fill = grey10k,
           alpha = 0.85) +
  geom_col() +
  geom_vline(xintercept = 1,
             size = 1.5,
             color = "white") +
  scale_fill_si(palette = "old_roses",
                discrete = FALSE,
                label = percent) +
  si_style_nolines() +
  labs(y=NULL,
       x=NULL,
       title='% Achievement by Implementing Partner | USAID',
       caption="MSD March 2021") +
  si_style_xline() +
  theme(legend.position = "none")





bar_cdc <- peds_geo2 %>%
  ggplot(aes(x=reorder(primepartner,desc(-share)),
             y=share, fill=share)) + #+ filter(!is.na(share)) +
  geom_col(show.legend = F)+
  coord_flip()+
  scale_y_continuous(labels = percent)+
  scale_fill_si(palette = "denims", discrete=FALSE, alpha=0.9, reverse = FALSE,
                breaks = c(0,0.25,0.5),
                limits = c(0,0.50),
                labels=percent)+
  si_style_nolines()+
  labs(y="",
       x="",
       title='% Share by Implementing Partner | CDC',
       caption="MSD March 2021")+
  theme(axis.text.y = element_text(size=8, face = "bold"))



#---------------OLD STUFF-------------

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




