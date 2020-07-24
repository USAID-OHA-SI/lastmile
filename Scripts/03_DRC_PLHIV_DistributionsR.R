## PROJECT:  PEPFAR Geospatial Analytics
## AUTHOR:   B.Kagniniwa, G.Sarfaty | USAID
## LICENSE:  MIT
## PURPOSE:  DRC - PLHIV Estimates
## Date:     2020-07-22
## Update:   2020-07-23

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
    dir_zmb <- "../../PEPFAR/COUNTRIES/Zambia"

    file_drc_cop20 <- "COP 2020 DRC FAST_FINAL_5.11.2020.xlsx"
    file_zmb_cop20 <- "Zambia_COP20_Datapack_Final.xlsx"

    ## MER NAT SubNat
    file_subnat <- "MER_Structured_Datasets_NAT_SUBNAT_FY15-20_20200605_v1_1"
    #file_subnat <- "MER_Structured_Datasets_NAT_SUBNAT_FY15-20_20200626_v2_1"

# DATA ------------------------------------------------------------------------------------

    ## Geodata - Admins & Health Zones
    adm0 <- get_admin0(countries = country) %>%
        dplyr::select(admin)

    adm1 <- get_admin1(countries = country) %>%
        dplyr::select(name)

    geo_psnu <- list.files(
            path = here(dir_geo, "PEPFAR"),
            pattern = "^DRC_.*_HZLsib_.*.shp$",
            recursive = TRUE,
            full.names = TRUE
        ) %>%
        read_sf() %>%
        janitor::clean_names() %>%
        dplyr::select(province = prov2017, district = hz_nam, uid) %>%
        mutate(pepfar = ifelse(is.na(uid), 'no', 'yes'))


    ## PSNU by IM
    df_psnu_results <- list.files(
            path = dir_merdata,
            pattern = paste0("^MER_.*_PSNU_IM_FY18-20_.*_", country, ".zip$"),
            full.names = TRUE
        ) %>%
        vroom()

    df_psnu_results %>% glimpse()


    ## DRC Datapack / FAST
    file_drc <- list.files(path = paste0(dir_drc, "/Data"),
                           pattern = file_drc_cop20,
                           full.names = TRUE)

    file_shts <- file_drc %>% excel_sheets()

    file_shts

    file_drc %>%
        read_excel(., sheet = "Mechs List-R") %>%
        janitor::clean_names() %>%
        filter(funding_agency == "USAID") %>%
        prinf()


    ## MER Nat & SubNat Estimates - FY15-20 v2.1
    df_subnat <- list.files(
            path = dir_merdata,
            pattern = file_subnat,
            full.names = TRUE
        ) %>%
        vroom()

    df_subnat %>% glimpse()

    df_subnat %>%
        distinct(operatingunit, countryname) %>%
        arrange(operatingunit) %>%
        prinf()

    df_subnat %>%
        select(indicatortype, indicator, standardizeddisaggregate) %>%
        distinct_all() %>%
        arrange(indicatortype, indicator) %>%
        prinf()

    df_subnat <- df_subnat %>%
        filter(indicator %in% c("POP_EST", "PLHIV", "TX_CURR_SUBNAT")) %>%
        filter(standardizeddisaggregate == "Age/Sex") %>%
        filter(!str_detect(operatingunit, "Region$")) %>%
        select(operatingunit, countryname:psnuuid, indicator, ageasentered:sex, fiscal_year, targets) #%>% View()

    #df_subnat %>% View(title = "SUBNAT")

    df_subnat %>%
        filter(operatingunit == country) %>%
        #filter(operatingunit == "South Africa") %>%
        #filter(indicator == "PLHIV") %>%
        distinct(operatingunit, indicator, fiscal_year) %>%
        prinf()

    # Only 2018 POP_EST is available for DRC
    # Keep the latest year
    df_pop_est <- df_subnat %>%
        filter(operatingunit == country, indicator == "POP_EST") %>%
        group_by_at(vars(-c(ageasentered, targets))) %>%
        summarise_at(vars(targets), sum, na.rm = TRUE) %>%
        ungroup() %>%
        filter(!is.na(snu1), fiscal_year == max(fiscal_year)) %>%
        select(-fiscal_year) %>%
        arrange(snu1, psnu)

    df_pop_est %>% View(title = "POP_EST")


    df_hiv <- df_subnat %>%
        filter(operatingunit == country, fiscal_year == 2020) %>%
        select(-c(operatingunit, countryname, ageasentered, fiscal_year)) %>%
        arrange(snu1, psnu, indicator, trendscoarse, sex) %>%
        group_by(snu1, psnu, psnuuid, indicator) %>%
        summarise(
            total_all_n = sum(targets, na.rm = TRUE),
            children_all_n = sum(targets[trendscoarse == '<15'], na.rm = TRUE),
            children_all_p = round(children_all_n / total_all_n * 100),
            adult_male_n = sum(targets[trendscoarse == '15+' & sex == 'Male'], na.rm = TRUE),
            adult_male_p = round(adult_male_n / total_all_n * 100),
            adult_female_n = sum(targets[trendscoarse == '15+' & sex == 'Female'], na.rm = TRUE),
            adult_female_p = round(adult_female_n / total_all_n * 100)
        ) %>%
        ungroup() %>%
        gather(key = "age_sex_group", value = "targets", total_all_n:adult_female_p) %>% #view()
        separate(age_sex_group, into = c("age", "sex", "numprop")) %>%
        spread(key = indicator, value = targets) %>%
        mutate(
            unmet_n = ifelse(numprop == 'n', PLHIV - TX_CURR_SUBNAT, NA),
            unmet_p = ifelse(numprop == 'n', round(unmet_n / PLHIV * 100), NA)
        ) %>%
        gather(key = "indicator", value = "targets", PLHIV:TX_CURR_SUBNAT) %>%
        relocate(unmet_n:unmet_p, .after=last_col())



    ## Zambia Datapack
    paste0(dir_zmb, "/", file_zmb_cop20) %>%
        excel_sheets(path = .)

    df_epi <- paste0(dir_zmb, "/", file_zmb_cop20) %>%
        read_xlsx(., sheet="Epi Cascade I",
                  skip=13,
                  col_names=TRUE)

    df_epi %>% glimpse()
    df_epi %>% View()

    df_epi <- df_epi %>%
        separate(PSNU, c("psnu","snutype","psnuuid"), sep = "\\[") %>%
        mutate(psnuuid = str_remove(psnuuid, "\\]")) %>%
        select(psnu, psnuuid, Age, Sex,
               `PLHIV.NA.Age/Sex/HIVStatus.T`,
               `PLHIV.districtUncertainty`) %>%
        rename(plhiv=`PLHIV.NA.Age/Sex/HIVStatus.T`)%>%
        group_by(psnu, psnuuid, Age, Sex) %>%
        summarise_at(vars(plhiv), sum, na.rm=TRUE)

    df_epi %>% glimpse()
    df_epi %>% View()

# VIZ ----------------------------------------------------------

    ## PSNU Map
    terrain_map(countries = country, terr_path = dir_terr, mask = TRUE) +
        geom_sf(data = geo_psnu %>% filter(pepfar == 'yes'), fill = USAID_red, color = grey10k, size = .1, alpha = .3, show.legend = F) +
        geom_sf_text(data = adm1, aes(label = name), color = grey90k, size = 4) +
        si_style_map()

