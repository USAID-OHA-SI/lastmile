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

source("./Scripts/00_Setup.R")

# GLOBALS --------------------------------------------------------------------------------

    ## Datim Account details
    user <- "bkagniniwa"
    key <- "datim_myuser"

    ## Country
    country = "Democratic Republic of the Congo"

    ## MER Sites by IM data: this folder contains all the zip files
    dir_mersites <- "../../MERDATA/OU_Specific_Sites_by_IM"

    files_mer_sites <- list.files(path = dir_mersites,
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

    ## MER PSNU by IM Data: this folder contains all the zip files
    dir_merpsnu <- "../../MERDATA/OU_Specific_PSNU_by_IM"

    files_mer_psnu <- list.files(path = dir_merpsnu,
                                  pattern = "^MER_.*_PSNU_IM_FY\\d{2}-\\d{2}_\\d{8}_.*.zip$",
                                  full.names = TRUE)

    files_mer_psnu %>% length()

    files_mer_psnu %>%
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

    #' Read country specific MER Sites by IM Data
    #'
    #' @param mer_folder
    #' @param country
    #'
    get_mersites <- function(mer_folder, country) {

        list.files(path = {{mer_folder}},
                   pattern = paste0("^MER_.*_Site_IM_FY\\d{2}-\\d{2}_\\d{8}_.*_", {{country}}, ".zip$"),
                   full.names = TRUE) %>%
            map_dfr(.x, .f=~vroom(.x, col_select = c("orgunituid", "sitename", "cumulative"))) %>%
            filter(!is.na(cumulative)) %>%
            distinct(orgunituid, sitename)
    }

    #' Read country specific MER PSNU by IM Data
    #'
    #' @param mer_folder
    #' @param country
    #'
    get_merpsnu <- function(mer_folder, country) {

        list.files(path = {{mer_folder}},
                   pattern = paste0("^MER_.*_PSNU_IM_FY\\d{2}-\\d{2}_\\d{8}_.*_", {{country}}, ".zip$"),
                   full.names = TRUE) %>%
            map_dfr(.x, .f=~vroom(.x, col_select = c("psnuuid", "psnu", "cumulative"))) %>%
            filter(!is.na(cumulative), str_detect(psnuuid, "[a-zA-z0-9]")) %>%
            distinct(psnuuid, psnu)
    }

    #' Extract PSNU (Prioritization)
    #'
    #' @param .data Datim organisation units data frame
    #' @param mer_psnu Distinct PSNUs from PSNU by IM
    #' @export
    #' @examples
    #' \dontrun{
    #' extract_psnu(df_locs)
    #' df_locs %>% extract_psnu()
    #' df_locs %>% extract_psnu(mer_psnu = df)
    #' }
    extract_psnu <- function(.data, mer_psnu = NULL) {

        # Country name
        cntry <- .data %>%
            pull(operatingunit) %>%
            first()

        # Check check missing gemetries
        na_psnu <- .data %>%
            filter(label == 'prioritization', nodes == 0) %>%
            nrow()

        if (na_psnu > 0) {
            cat(paste0("\nWarning: ", cntry, " has PSNUs with missing geometries: ", Wavelength::paint_red(na_psnu), "\n"))
        }

        # Keep only PSNUs and they should be either polygons or multipolygons
        .data <- .data %>%
            filter(label == 'prioritization', geom_type %in% c("Polygon", "MultiPolygon"), nodes > 0)

        # Keep only PSNU with current results
        if (!is.null(mer_psnu)) {
            .data <- .data %>%
                left_join({{mer_psnu}}, by = c("id" = "psnuuid")) %>%
                filter(!is.na(psnu))
        }

        # Check for duplicates
        dup_psnu <- .data %>%
            group_by(id) %>%
            tally() %>%
            filter(n != 1) %>%
            nrow()

        if (dup_psnu > 0) {
            cat(paste0("\nThere are duplicate PSNUs: ", Wavelength::paint_red(dup_psnu), "\n"))
        }

        # Convert coordinates into Simple Features Geometry and Collection: sfc
        .data %>%
            rowwise() %>%
            # mutate(
            #     geometry = ifelse(
            #         tolower(geom_type) == "polygon",
            #         to_polygon(coordinates),
            #         to_multipolygon(coordinates)
            #     )
            # ) %>%
            mutate(
                geometry = ifelse(
                    tolower(geom_type) == "polygon",
                    to_polygon(coordinates),
                    ifelse(
                        tolower(geom_type) == "multipolygon",
                        to_multipolygon(coordinates),
                        NULL
                    )
                )
            ) %>%
            ungroup() %>%
            st_as_sf(wkt = "geometry", crs = 4326)
    }

    #' Explore PSNU (Prioritization)
    #'
    #' @param .data Datim organisation units data frame
    #' @param cntry Country name
    #' @export
    #' @examples
    #' \dontrun{
    #' explore_psnu(df_psnu)
    #' df_psnu %>% resplore_psnu()
    #' }
    explore_psnu <- function(.data, cntry) {

        # Make sure to use rnaturalearth version of the name
        country <- dplyr::case_when(
            {{cntry}} == "Cote d'Ivoire" ~ "Ivory Coast",
            {{cntry}} == "Eswatini" ~ "Swaziland",
            {{cntry}} == "Tanzania" ~"United Republic of Tanzania",
            TRUE ~ {{cntry}}
        )

        # Country admin 0 boundaries
        adm0 <- get_admin0(countries = country)

        # Country admin 1 boundaries
        adm1 <- get_admin1(countries = country)

        # Geo-viz
        viz <- ggplot2::ggplot()

        # Check admin level 0 geodata
        if (!is.null(adm0)) {
            viz <- viz +
                ggplot2::geom_sf(data = adm0, fill = NA, size = 1, color = glitr::grey70k) +
                ggplot2::geom_sf(data = .data, fill = grey10k, size = .2, color = "white")
        }
        else {
            cat(paste0("\nWarning: unable to extract admin 0 level geodata: ", Wavelength::paint_red(country), "\n"))

            viz <- viz +
                ggplot2::geom_sf(data = .data, fill = grey10k, size = .2, color = glitr::grey30k)
        }

        # Check admin level 1 geodata
        if (!is.null(adm1)) {
            viz <- viz +
                ggplot2::geom_sf(data = adm1, fill = NA, size = .5, color = glitr::grey70k, lty = "dotted")
        }
        else {
            cat(paste0("\nWarning: unable to extract admin 1 level data: ", Wavelength::paint_red(country), "\n"))
        }

        # Check admin level 0 geodata
        if (!is.null(adm0)) {
            viz <- viz +
                ggplot2::geom_sf(data = adm0, fill = NA, size = 1, color = glitr::grey70k) +
                ggplot2::geom_sf(data = adm0, fill = NA, size = .5, color = glitr::grey10k)
        }

        # Apply geo-style
        viz <- viz +
            ggplot2::labs(title = paste0(toupper({{cntry}}), " - PEPFAR PSNU Areas")) +
            gisr::si_style_map()

        print(viz)

        return(viz)
    }

    #' Convert list of coordinates to a polygon
    #'
    #' @param coords list of coordinates
    #' @export
    #' @examples
    #' \dontrun{
    #' to_polygon(coordinates)
    #' }
    #'
    to_polygon <- function(coords) {

        geom <- st_as_text(st_polygon(list(matrix(unlist(coords), ncol = 2))))

        return(geom)
    }

    #' Convert list of coordinates to a multipolygon
    #'
    #' @param coords list of coordinates
    #' @export
    #' @examples
    #' \dontrun{
    #' to_polygon(coordinates)
    #' }
    #'
    to_multipolygon <- function(coords) {

        print(length(coords))

        geom <- coords %>%
            map(function(x) {
                list(matrix(x, ncol = 2))
            }) %>%
            st_multipolygon() %>%
            st_as_text()

        return(geom)
    }


    #' Generate psnu maps report
    #'
    #' @param country
    #' @param mer_psnu
    #' @param username
    #' @param password
    #' @param output_folder
    #' @export
    #' \dontrun{
    #' generate_psnu_report("saturn", df_psnu, "<username>", "<password>")
    #' }
    generate_psnu_report <- function(country, mer_psnu, username, password, output_folder = NULL) {

        print(toupper(country))

        # extract location data
        df_locs <- extract_locations(country = {{country}},
                                     username = {{username}},
                                     password = {{password}})

        # Extract, build and vizualize PSNU Geodata
        viz <- df_locs %>%
            extract_psnu(mer_psnu = {{mer_psnu}}) %>%
            explore_psnu(cntry = {{country}})

        # Export Viz
        if (!is.null({{output_folder}})) {
            ggplot2::ggsave(
                filename = paste0({{output_folder}}, "/", {{country}}, " - PSNU Location data availability.png"),
                plot = last_plot(), scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
        }

        print(viz)

        return(viz)
    }

# DATA --------------------------------------------------------------------------

    ## Get list of PEPFAR OUs
    ous <- Wavelength::identify_ouuids(username = user, password = glamr::mypwd(key))

    ## Remove out regional OUs
    countries <- ous %>%
        filter(is.na(regional), !str_detect(displayName, "Region")) %>%
        pull(displayName)

    # Get country org levels
    ou_levels <- Wavelength::identify_levels(ou = countries[6], username = user, password = mypwd(key)) %>%
        dplyr::relocate(dplyr::last_col(), .after = name4) %>%
        tidyr::gather(key = "label", value = "level", -c(1:5))

    ou_levels %>% glimpse()

    ## Country Specific MER PSNU Data
    cntry_mer_psnu <- get_merpsnu(dir_merpsnu, country)

    cntry_mer_psnu %>% glimpse()

    ## Combine map + assessment graph
    df_locs <- extract_locations(country = countries[6], username = user, password = glamr::mypwd(key))

    df_locs %>% glimpse()

    df_locs %>%
        distinct(label) %>%
        pull()

    df_locs %>%
        distinct(geom_type) %>%
        pull()

    df_locs_psnu <- df_locs %>%
        extract_psnu()

    df_locs_psnu %>%
        explore_psnu(cntry = country)

    df_locs %>%
        extract_psnu(mer_psnu = cntry_mer_psnu) %>%
        explore_psnu(cntry = country)

## VIZ ----------------------------------

    countries[1:6] %>%
        map(.x, .f = ~ generate_psnu_report(country = .x,
                                            mer_psnu = get_merpsnu(mer_folder = dir_merpsnu, country = .x),
                                            user, glamr::mypwd(key),
                                            output_folder = dir_graphs))

