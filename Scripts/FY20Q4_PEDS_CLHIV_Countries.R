##  PROJECT: PEDS/Maternal Care /Geospatial Distributions
##  AUTHOR:  Tewodros Hailegeberel | USAID
##  PURPOSE: Geo-depiction of unmet need
##  LICENCE: MIT
##  DATE:    2021-01-12

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glitr)
library(glamr)
library(gisr)
library(extrafont)
library(scales)
library(ggtext)
library(sf)
library(ggrepel)
library(patchwork)
library(glue)
library(ICPIutilities)

# SETUP -------------------------------------------------------------------

# folder_setup()

# LOAD DATA ---------------------------------------------------------------

# import txt data file---------------------------------------------------------
NAT_File <- (list.files(path =si_path(type="path_msd"),
                            pattern = "Structured_.*_NAT_SUBNAT_.*_20201218_.*.txt",
                            recursive = TRUE,
                            full.names = TRUE))

MER_File <- (list.files(path =si_path(type="path_msd"),
                        pattern = "Structured_.*_PSNU_IM_.*_20201218_.*.txt",
                        recursive = TRUE,
                        full.names = TRUE))
#read df-----------------------------------------------------------------------

df1<-read_msd(NAT_File)

df2<-read_msd(MER_File)

#import shapefile -------------------------------------------------------------
gis_vc_poly <- list.files(si_path(type="path_vector"),
                          pattern = "VcPepfar.*.shp$",
                          recursive = T,
                          full.names = T) %>%
  read_sf

# gis_vc_poly %>%
#   filter(uid=='skj3e4YSiJY') %>%
#   glimpse()

# clhiv_lookup<-df2 %>%
#   filter(indicator=="TX_CURR") %>%
#   distinct(indicator,standardizeddisaggregate, numeratordenom, 
#   ageasentered, trendscoarse, fiscal_year)

clhiv <- df1 %>%
  filter(fiscal_year =="2020",
         indicator %in% c("PLHIV"),
         trendscoarse %in% c("<15")) %>%
  mutate(countrynameuid = ifelse(str_detect(operatingunit, " Region$"), snu1uid, operatingunituid)) %>%
  group_by(operatingunit,  operatingunituid, countryname, countrynameuid) %>%
  summarise(targets = sum(targets, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(clhiv = targets)


tx_curr_peds <- df2 %>%
  filter(fiscal_year =="2020",
         indicator %in% c("TX_CURR"),
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus"),
         trendscoarse == "<15")%>%
  mutate(countrynameuid = ifelse(str_detect(operatingunit, " Region$"), snu1uid, operatingunituid)) %>%
  group_by(operatingunit,  operatingunituid, countryname, countrynameuid) %>%
  summarise(cumulative = sum(cumulative, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(tx_curr_peds = cumulative)

#join tables ------------------------------------------------------
total_clhiv_Unmet_ART <- merge(clhiv, tx_curr_peds, by=c("operatingunituid", "countrynameuid")) %>%
  mutate(unmet_art_need = clhiv - tx_curr_peds) %>%
  distinct()


#---Export tables to CSV---------------------------------------------------------
# This will export unmet ART need table
write.csv(total_clhiv_Unmet_ART,"C:\\Users\\teddy\\Documents\\Github\\FY20Q4_PEDS_CLHIV_Countries\\Dataout\\total_clhiv_Unmet_ART.csv", row.names = FALSE)


# This will export # of CLHIV by country
write.csv(clhiv2,"C:\\Users\\teddy\\Documents\\Github\\FY20Q4_PEDS_CLHIV_Countries\\Dataout\\total_clhiv.csv", row.names = FALSE)


#Maps are completed using ArcGIS Pro