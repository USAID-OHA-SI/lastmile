## PROJECT:  PEPFAR Geospatial Analytics
## AUTHOR:   B.Kagniniwa, G.Sarfaty | USAID
## LICENSE:  MIT
## PURPOSE:  Facilities Location data assessment
## Date:     2020-07-21

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

    # Data & Outpu folders
    dir_terr <- "../../GEODATA/RASTER"
    dir_graphs <- "./Graphics"

    ## MER Sites by IM data: this folder contains all the zip files
    dir_merdata <- "../../MERDATA/OU_Specific_Sites_by_IM"

    files_mer_sites <- list.files(path = dir_merdata,
               pattern = "^MER_.*_Site_IM_FY\\d{2}-\\d{2}_\\d{8}_.*.zip$",
               full.names = TRUE)

    files_mer_sites %>% length()

    files_mer_sites %>%
        map(function(x){
            str_split(x, ".zip") %>%
                unlist() %>%
                first() %>%
                str_split("_") %>%
                unlist() %>%
                last()
        }) %>%
        unlist()


# FUNCTIONS ------------------------------------------------------------------------------

    ## Read country specific MER Sites by IM Data
    get_mer_sites <- function(mer_folder, country) {

        list.files(path = {{mer_folder}},
                   pattern = paste0("^MER_.*_Site_IM_FY\\d{2}-\\d{2}_\\d{8}_.*_", {{country}}, ".zip$"),
                   full.names = TRUE) %>%
            map_dfr(.x, .f=~vroom(.x, col_select = c("orgunituid", "sitename", "cumulative"))) %>%
            filter(!is.na(cumulative)) %>%
            distinct(orgunituid, sitename)
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
                                                 mer_sites = get_mer_sites(dir_merdata, .x),
                                                 user = user,
                                                 pass = glamr::mypwd(key),
                                                 terr_path = dir_terr,
                                                 output_folder = dir_graphs))
    ## SS has mis-matching scales
    countries[19] %>%
        map(.x, .f = ~generate_facilities_report(cntry = .x,
                                             mer_sites = get_mer_sites(dir_merdata, .x),
                                             user = user,
                                             pass = glamr::mypwd(key)
                                             output_folder = dir_graphs))


