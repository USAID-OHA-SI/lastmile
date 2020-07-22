## PROJECT:  PEPFAR Geospatial Analytics
## AUTHOR:   B.Kagniniwa, G.Sarfaty | USAID
## LICENSE:  MIT
## PURPOSE:  DRC - PLHIV Estimates
## Date:     2020-07-22

# LIBRARIES

library(tidyverse)
library(readxl)
library(vroom)
library(sf)
library(gisr)
library(glitr)
library(glamr)
library(ggrepel)


# REQUIRED -------------------------------------------------------------------------------

    source("./Scripts/00_Setup.R")

# GLOBALS --------------------------------------------------------------------------------

    ## Country
    country <- "Democratic Republic of the Congo"

    ## Datim Account details
    user <- "bkagniniwa"
    key <- "datim_myuser"

    ## Country COP20 Data
    dir_drc <- "../../PEPFAR/COUNTRIES/DRC"

    file_cop20 <- "COP 2020 DRC FAST_FINAL_5.11.2020.xlsx"

# DATA ------------------------------------------------------------------------------------

    ## Geodata - Admins & Health Zones
    drc0 <- get_admin0(countries = country) %>%
        dplyr::select(admin)

    drc1 <- get_admin1(countries = country) %>%
        dplyr::select(name)

    geo_psnu <- list.files(path = here(dir_geo, "PEPFAR"),
                           pattern = "^DRC_.*_HZLsib_.*.shp$",
                           recursive = TRUE,
                           full.names = TRUE) %>%
        read_sf() %>%
        janitor::clean_names() %>%
        dplyr::select(province = prov2017, district = hz_nam, uid) %>%
        mutate(pepfar = ifelse(is.na(uid), 'no', 'yes'))

    geo_psnu %>% glimpse()

    terrain_map(countries = country, terr_path = dir_terr, mask = TRUE) +
        geom_sf(data = geo_psnu %>% filter(pepfar == 'yes'), fill = USAID_red, color = grey10k, size = .1, alpha = .3, show.legend = F) +
        geom_sf_text(data = drc1, aes(label = name), color = grey90k, size = 4) +
        si_style_map()


    ## PSNU by IM
    df_psnu <- list.files(path = dir_merdata,
                          pattern = paste0("^MER_.*_PSNU_IM_FY18-20_.*_", country, ".zip$"),
                          full.names = TRUE) %>%
        vroom()

    df_psnu %>% glimpse()


    ## PLHIV Estimates
    file_drc <- list.files(path = paste0(dir_drc, "/Data"),
                           pattern = file_cop20,
                           full.names = TRUE)

    file_shts <- file_drc %>% excel_sheets()

    file_shts


    #PLHIV
    epi<-read_xlsx(here("DP","Zambia_COP20_Datapack_Final.xlsx"), sheet="Epi Cascade I",skip=13, col_names=TRUE)
    epi_clean <-epi %>%
        separate(PSNU, c("psnu","snutype","psnuuid"), sep = "\\[") %>%
        mutate(psnuuid = str_remove(psnuuid, "\\]")) %>%
        select(psnu,psnuuid,Age,Sex,`PLHIV.NA.Age/Sex/HIVStatus.T`,`PLHIV.districtUncertainty`) %>%
        rename(plhiv=`PLHIV.NA.Age/Sex/HIVStatus.T`)%>%
        group_by(psnu,psnuuid,Age,Sex) %>%
        summarise_at(vars(plhiv),sum,na.rm=TRUE)
    View(epi_clean)
