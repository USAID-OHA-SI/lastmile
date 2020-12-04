##  PROJECT: Q2 Review target analysis
##  AUTHOR:  B.Kagniniwa & G.Sarfaty | USAID
##  PURPOSE: Geo-depiction of VLC
##  LICENCE: MIT
##  DATE:    2020-08-26

# Dependancies----------------------------------------------------------

library(tidyverse)
library(readxl)
library(vroom)
library(sf)
library(rnaturalearth)
library(here)
library(glamr)
library(glitr)
library(gisr)
library(ICPIutilities)
library(janitor)
library(scales)
library(RColorBrewer)
library(patchwork)

# GLOBALS -------------------------------------------------------------

  ## Credentials & Utilities
    
    source("../_secrets/credentials.R")  
    source("./Scripts/00_Utilities.R")

  ## Data & Output folders
  
    dir_data <- "Data"
    dir_dataout <- "Dataout"
    dir_gis <- "GIS"
    dir_graphics <- "Graphics"
    dir_geodata <- "../../GEODATA/PEPFAR"
    dir_merdata <- "../../MERDATA"
  
  ## PSNUxIM Dataset
  
    rep_agency <- "USAID"
    
    rep_fy <- 2020
    
    ou_im <- "^MER_.*_OU_IM_.*_20201113_v1_1.zip$"
  
    file_ou_im <- list.files(
        path = dir_merdata,
        pattern = ou_im,
        recursive = TRUE,
        full.names = TRUE
      ) %>%
      sort() %>%
      last()
    
  ## Notes
  
    footnote <- paste0("USAID - Office of HIV/AIDS - Programs Overview as of ", Sys.Date())
    
# FUNCTIONS -------------------------------------------------------------

 
    
# DATA ------------------------------------------------------------------
    
  ## GeoData
    
    spdf <- ne_countries(type = "sovereignty", 
                         scale = 110, 
                         returnclass = "sf") %>% 
      dplyr::select(sovereignt, admin, name, adm0_a3) %>% 
      filter(admin != "Antarctica") %>% # Remove Antarctica
      clean_countries(colname = "admin")
    
  ## MER OUxIM
  
    ## Raw data
    df_ou <- vroom(file_ou_im)
    
    df_ou %>% glimpse()
    
    df_ou %>% 
      filter(fundingagency == rep_agency,
             fiscal_year == rep_fy,
             !is.na(targets)) %>% 
      distinct(operatingunit, countryname) %>% 
      arrange(operatingunit, countryname) %>% 
      prinf()
    
    # Join MSD to spdf
    spdf_ou <- spdf %>% 
      left_join(df_ou %>% 
                  filter(fundingagency == rep_agency,
                         fiscal_year == rep_fy,
                         !is.na(targets)) %>% 
                  distinct(operatingunit, countryname), 
                by = c("admin" = "countryname")) %>% 
      filter(!is.na(operatingunit))

    
## VIZ ---------------------------------------------------------

  ## Global Maps
    ggplot() +
      geom_sf(data = spdf, fill = NA, color = grey50k, size = .4) +
      geom_sf(data = spdf_ou,
              fill = USAID_blue,
              #fill = USAID_ltblue, 
              color = grey30k,
              #color = grey10k,
              size = .2) +
      labs(
        #title = "USAID - HIV/AIDS Programs",
        #subtitle = "Countries in blue are supported by USAID",
        caption = footnote
      ) +
      si_style_map() +
      theme(
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(1.5, "cm"),
        plot.title = element_text(face = "bold")
      )
           
    ggsave(here(dir_graphics, "FY20Q4i_Global_PEPFAR_Programs_Maps.png"),
           plot = last_plot(), 
           scale = 1.2, dpi = 310, 
           width = 10, height = 7, units = "in")   
  
    
    
    