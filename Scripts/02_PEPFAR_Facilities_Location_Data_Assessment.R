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

        readr::write_csv(x = sites,
                         file = paste0({{out_folder}}, "/", {{cntry}}, " - Facilities_locations_", Sys.Date(), ".csv"), na = "")
    }

    ## generate facilities report 2
    generate_facilities_report2 <- function(df_sites, cntry, terr_path = NULL, output_folder = NULL) {

        # Map sites locations
        viz_map <- df_sites %>%
            explore_facilities(cntry = {{cntry}}, terr_path = {{terr_path}})

        # Plot completeness
        viz_bar <- df_sites %>%
            assess_facilities()

        # Combine plots
        viz <- patchwork::wrap_plots(viz_map, viz_bar, nrow = 1, widths = c(2,1)) +
            patchwork::plot_annotation(
                title = toupper({{cntry}}),
                subtitle = "Facilities location data availability",
                theme = ggplot2::theme(
                    plot.title = element_text(hjust = .5),
                    plot.subtitle = element_text(hjust = .5)
                )
            )

        # Export viz as png file
        if ( !is.null({{output_folder}}) ) {
            ggplot2::ggsave(
                filename = paste0({{output_folder}}, "/", {{cntry}}, " - SSSites location data availability.png"),
                plot = last_plot(), scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
        }

        return(viz)
    }

# DATA --------------------------------------------------------------------------

    ## Get list of PEPFAR OUs
    ous <- Wavelength::identify_ouuids(username = datim_user(),
                                       password = datim_pwd())

    ## Remove out regional OUs
    countries <- ous %>%
        filter(is.na(regional), !str_detect(displayName, "Region")) %>%
        pull(displayName)

    ## Combine map + assessment graph - Nigeria
    countries[16] %>%
        map(.x, .f = ~generate_facilities_report(cntry = .x,
                                                 mer_sites = get_mersites(dir_merdata, .x),
                                                 user = datim_user(),
                                                 pass = datim_pwd(),
                                                 terr_path = dir_terr,
                                                 output_folder = dir_graphs))


    ## Combine map + assessment graph
    countries[1:18, 20:25] %>%
        map(.x, .f = ~generate_facilities_report(cntry = .x,
                                                 mer_sites = get_mersites(dir_merdata, .x),
                                                 user = datim_user(),
                                                 pass = datim_pwd(),
                                                 terr_path = dir_terr,
                                                 output_folder = dir_graphs))
    ## SS has mis-matching scales
    countries[19] %>%
        map(.x, .f = ~generate_facilities_report(cntry = .x,
                                             mer_sites = get_mersites(dir_merdata, .x),
                                             user = datim_user(),
                                             pass = datim_pwd(),
                                             output_folder = dir_graphs))

    ## Export Facilities location data by OU
    countries[16] %>%
        map(export_facilities,
            datim_user(),
            datim_pwd(),
            dir_merdata,
            dir_sites)


    ## Batch generate Maps
    list.files(
            path = dir_sites,
            pattern = "^Nigeria.*.csv$",
            full.names = TRUE
        ) %>%
        sort() %>%
        last() %>%
        vroom() %>%
        generate_facilities_report2(cntry = "Nigeria",
                                    output_folder = dir_graphs)

