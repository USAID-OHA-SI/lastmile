## PROJECT:  PEPFAR Geospatial Analytics
## AUTHOR:   B.Kagniniwa, G.Sarfaty | USAID
## LICENSE:  MIT
## PURPOSE:  Facilities Location data assessment
## Date:     2020-07-21
## Updated:  2020-08-05

# LIBRARIES

library(tidyverse)
library(vroom)
library(sf)
library(gisr)
library(glitr)
library(glamr)


# REQUIRED -------------------------------------------------------------------------------

#source("./Scripts/00_Setup.R")

# GLOBALS --------------------------------------------------------------------------------

    ## Datim Account details
    user <- ""
    key <- ""

    # Data & Output folders
    dir_terr <- "../../GEODATA/RASTER"
    dir_sites <- "../../GEODATA/PEPFAR/OU-Sites"

    dir_graphs <- "./Graphics"

    ## MER Sites by IM data: this folder contains all the zip files
    dir_merdata <- "../../MERDATA/OU_Specific_Sites_by_IM"

    files_mer_sites <- list.files(path = dir_merdata,
               pattern = "^MER_.*_Site_IM_FY\\d{2}-\\d{2}_\\d{8}_.*.zip$",
               full.names = TRUE)

    files_mer_sites %>% length()

    files_mer_ous <- files_mer_sites %>%
        map(function(x){
            str_split(x, ".zip") %>%
                unlist() %>%
                first() %>%
                str_split("_") %>%
                unlist() %>%
                last()
        }) %>%
        unlist()

    files_mer_ous


# FUNCTIONS ------------------------------------------------------------------------------

    ## Read country specific MER Sites by IM Data
    get_mersites <- function(mer_folder, country) {

        list.files(path = {{mer_folder}},
                   pattern = paste0("^MER_.*_Site_IM_FY\\d{2}-\\d{2}_\\d{8}_.*_", {{country}}, ".zip$"),
                   full.names = TRUE) %>%
            map_dfr(.x, .f=~vroom(.x, col_select = c("orgunituid", "sitename", "cumulative"))) %>%
            filter(!is.na(cumulative)) %>%
            distinct(orgunituid, sitename)
    }

    ## Export facilities location data
    export_facilities <- function(cntry, user, pass, dir_mersites, out_folder = "./Dataout") {
        sites <- extract_locations(country = {{cntry}},
                          username = {{user}},
                          password = {{pass}}) %>%
            extract_facilities(mer_sites = get_mersites({{dir_mersites}}, {{cntry}})) %>%
            select(operatingunit:id, longitude:latitude)

        sites %>% glimpse()

        readr::write_csv(sites, path = paste0({{out_folder}}, "/", {{cntry}}, " - Facilities_locations_", Sys.Date(), ".csv"), na = "")
    }

# DATA --------------------------------------------------------------------------

    ## Get list of PEPFAR OUs
    ous <- Wavelength::identify_ouuids(username = user, password = glamr::mypwd(key))

    ## Remove out regional OUs
    countries <- ous %>%
        filter(is.na(regional), !str_detect(displayName, "Region")) %>%
        pull(displayName)


    ## Combine map + assessment graph
    countries[1:18, 20:25] %>%
        map(.x, .f = ~generate_facilities_report(cntry = .x,
                                                 mer_sites = get_mersites(dir_merdata, .x),
                                                 user = user,
                                                 pass = glamr::mypwd(key),
                                                 terr_path = dir_terr,
                                                 output_folder = dir_graphs))
    ## SS has mis-matching scales
    countries[19] %>%
        map(.x, .f = ~generate_facilities_report(cntry = .x,
                                             mer_sites = get_mersites(dir_merdata, .x),
                                             user = user,
                                             pass = glamr::mypwd(key)
                                             output_folder = dir_graphs))

    ## Export Facilities location data by OU
    countries %>%
        map(export_facilities, user, glamr::mypwd(key), dir_merdata, dir_sites)




