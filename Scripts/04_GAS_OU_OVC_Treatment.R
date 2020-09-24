##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa, G.Sarfaty & T.Essam | USAID
##  PURPOSE: Geo-depiction of VL - % not covered
##  LICENCE: MIT
##  DATE:    2020-09-04

# Libraries
library(tidyverse)
library(vroom)
library(sf)
library(sp)
library(raster)
library(gisr)
library(glitr)
library(glamr)
library(janitor)
library(scales)
library(patchwork)
library(here)
library(ICPIutilities)
library(extrafont)

# REQUIRED -------------------------------------------------------------------------------

    ## Get Credentials

    source("./_secrets/credentials.R")

# GLOBALS -------------------------------------------------------------

    ## Data & Output folders

    dir_data <- "Data"
    dir_dataout <- "Dataout"
    dir_gis <- "GIS"
    dir_graphics <- "Graphics"
    dir_geodata <- "../../GEODATA/PEPFAR"
    dir_terr <- "../../GEODATA/RASTER"
    dir_merdata <- "../../MERDATA"

    ## Q3 MER Data

    psnu_im <- "^MER_.*_PSNU_IM_.*_20200814_.*.zip$"

    ## Reporting Filters

    rep_agency = "USAID"

    rep_fy = 2020
    rep_qtr = 2

    rep_pd = rep_fy %>%
        as.character() %>%
        str_sub(3,4) %>%
        paste0("FY", ., "Q", rep_qtr)


# FUNCTIONS ---------------------------------------------------------

    ## NOTE: These functions will end up in gisr package

    #' Get Title
    #'
    #' @param country country name
    #' @param var df variable
    #' @return plot caption
    #'
    get_title <- function(country, var = NULL) {

        title <- toupper({{country}})

        var_name <- toupper({{var}})

        var_label <- ""

        # Get label


        # Title
        if (!is.null({{country}})) {
            title <- paste0(toupper({{country}}), " - ", var_label)
        }
        else {
            title <- var_label
        }

        return(title)
    }

    #' Graphic caption
    #'
    #' @param country country name
    #' @param var df variable
    #' @return plot caption
    #'
    get_caption <- function(country, var = NULL) {

        caption <- paste0(
            "OHA/SIEI - Data Source: FY20Q3i MSD - USAID Only,
            Proxy Coverage = OVC_HIV_STAT_POS / TX_CURR, Age < 20\n",
            toupper({{country}}), ", Produced on ",
            format(Sys.Date(), "%Y%m%d")
        )

        return(caption)
    }


    #' Graphic file name
    #'
    #' @param country country name
    #' @param var df variable
    #' @return plot file name
    #'
    get_output_name <- function(country, var = "Proxy Coverage") {

        name <- paste0("FY20Q2_",
                       str_replace({{var}}, " ", "_"),
                       "_", toupper({{country}}),
                       "_",
                       format(Sys.Date(), "%Y%m%d"),
                       ".png")

        return(name)
    }


    #' Terrain Raster data
    #'
    #' @param terr_path path to terrain raster file
    #' @return RasterLayer
    #'
    get_raster <- function(terr_path = NULL) {

        dir_terr <- {{terr_path}}

        if (is.null(dir_terr))
            dir_terr <- "../../GEODATA/RASTER"

        if (!dir.exists(dir_terr))
            stop(paste0("Invalid terrain path: ", dir_terr))

        terr_file <- list.files(
            path = dir_terr,
            pattern = "SR_LR.tif$",
            recursive = TRUE,
            full.names = TRUE
        )

        ras <- raster::raster(terr_file)

        return(ras)
    }


    #' Get Basemap
    #'
    #' @param spdf PEPFAR ORGs Spatial Data
    #' @param cntry OU Name
    #' @param terr_raster RasterLayer
    #' @return ggplot plot of base map
    #'
    get_basemap <- function(spdf, cntry = NULL, terr_raster = NULL) {

        df_geo <- {{spdf}}

        country <- {{cntry}}

        dta_raster <- {{terr_raster}}

        # Filter by OU / Country
        if ( !is.null(country) ) {

            df_geo <- df_geo %>% filter(countryname == country)
        }

        # Transform spdf
        df_geo <- df_geo %>%
            sf::st_as_sf() %>%
            sf::st_transform(., crs = sf::st_crs(4326)) %>%
            sf::st_zm()

        # Get admins
        df_geo0 <- df_geo %>% filter(type == "OU")

        df_geo1 <- df_geo %>% filter(type == "SNU1")

        df_geo2 <- df_geo %>% filter(type == "PSNU")

        # Terrain
        if(is.null(dta_raster))
            stop("Terrain raster data is required.")

        # Crop
        terr <- dta_raster %>%
            raster::crop(x = ., y = raster::extend(raster::extent(df_geo0), .2)) %>%
            raster::mask(x = ., mask = df_geo0)

        #plot(terr)

        # Convert raster data into a spatial data frame
        trdf <- terr %>%
            as("SpatialPixelsDataFrame") %>%
            as.data.frame() %>%
            dplyr::rename(value = SR_LR) %>%
            dplyr::filter(value < 210)


        # Map
        m <- ggplot() +
            geom_tile(data = trdf, aes(x, y, alpha = value)) +
            scale_alpha(name = "", range = c(0.6, 0), guide = F) +
            geom_sf(data = df_geo0, colour = "white", fill = grey10k, size = 2, alpha = .25) +
            geom_sf(data = df_geo1, colour = grey50k, fill = "NA", linetype = "dotted") +
            geom_sf(data = df_geo0, colour = grey90k, fill = "NA", size = 1) +
            si_style_map()

        # Zoom to South Africa mainland
        if ("south africa" == tolower(country))
            m <- m +
                ggplot2::xlim(15, 35) +
                ggplot2::ylim(-38, -20)

        # print and return
        print(m)

        return(m)
    }


    #' Map org geodata
    #'
    #' @param spdf PEPFAR ORGs Spatial Data
    #' @param org_uid ORG uid
    #' @param title Plot title
    #' @return ggplot plot of the map
    #'
    map_org <- function(spdf, org_uid = NULL, title = NULL) {

        df_geo <- {{spdf}}

        if(!is.null( {{org_uid}} )) {
            df_geo <- df_geo %>% filter(uid == {{org_uid}})
        }

        map_title <- ifelse(!is.null({{title}}), paste0("MAP - ", {{title}}), paste0("MAP - ", df_geo$uid[1]))

        #m <- plot(df_geo, main = map_title)

        m <- ggplot() +
            geom_sf(data = df_geo, fill = NA, color = grey30k, lwd = .5) +
            labs(title = map_title) +
            si_style_map()

        print(m)

        return(m)
    }


    #' Map OVC Proxy Coverage
    #'
    #' @param spdf PEPFAR Spatial Data
    #' @param df OVC Coverage data
    #' @param terr_raster RasterLayer
    #' @param cntry OU Name
    #' @param ovc_variable OVC Variable, default = proxy_coverage
    #' @param caption Add caption to the output?
    #' @param save Save the output to ./Graphics folder
    #' @return ggplot plot of the map
    #'
    map_ovc_coverage <- function(spdf, df, terr_raster, cntry,
                                 ovc_variable = "proxy_coverage",
                                 caption = FALSE, save = FALSE) {

        # Variables
        df_geo <- {{spdf}}

        df_ovc <- {{df}}

        country <- {{cntry}}

        ovc_var <- {{ovc_variable}}

        terr <- {{terr_raster}}

        # Country boundaries
        df_geo0 <- df_geo %>%
            filter(countryname == country, type == "OU")

        # PSNU Geo + OVC Data
        df_geo2 <- df_geo %>%
            filter(countryname == country, type == "PSNU") %>%
            left_join(df_ovc, by = c("uid" = "psnuuid")) %>%
            filter(!is.na(shortname))

        # Basemap
        base_map <- get_basemap(spdf = df_geo,
                                cntry = country,
                                terr_raster = terr)


        # Map specific variable
       if (tolower(ovc_var) == "proxy_coverage") {

            theme_map <- base_map +
                geom_sf(data = df_geo2, aes(fill = proxy_coverage), lwd = .2, color = grey10k) +
                scale_fill_viridis_c(
                    option = "magma",
                    alpha = 0.9,
                    direction = -1,
                    na.value = grey10k,
                    #breaks = c(0, .25, .50, .75, 1.00),
                    #limits = c(0, 1),
                    labels = percent
                )
        }
        else {

            theme_map <- base_map +
                geom_sf(data = df_geo2, aes(fill = TX_CURR), lwd = .2, color = grey10k) +
                scale_fill_gradient2(
                    low = "yellow",
                    high = "brown",
                    na.value = grey10k
                )
        }

        # Add country boundaries and apply map theme
        theme_map <- theme_map +
            geom_sf(data = df_geo0, colour = grey90k, fill = NA, size = 1) +
            si_style_map()


        # Add Caption
        if (caption == TRUE) {

            theme_map <- theme_map +
                labs(
                    title = get_title(var = ovc_var, peds = peds_title),
                    caption = get_caption(country, var = ovc_var)
                )
        }

        # Update legend size and position
        theme_map <- theme_map +
            theme(
                legend.position =  "bottom",
                legend.direction = "horizontal",
                legend.key.width = ggplot2::unit(1.5, "cm"),
                legend.key.height = ggplot2::unit(.5, "cm")
            )

        print(theme_map)

        # Save output
        if(save == TRUE) {
            ggsave(
                here::here("Graphics", get_output_name(country, var = ovc_var)),
                plot = last_plot(), scale = 1.2, dpi = 400,
                width = 10, height = 7, units = "in"
            )
        }

        return(theme_map)
    }


    #' Plot OVC Proxy Coverage
    #'
    #' @param df OVC Coverage data
    #' @param cntry OU Name
    #' @param ovc_variable OVC Variable, default = proxy_coverage
    #' @param caption Add caption to the output?
    #' @param save Save the output to ./Graphics folder
    #' @return ggplot plot of the map
    #'
    plot_ovc_coverage <- function(df, cntry = "Zambia") {

        # Params
        df_ovc <- {{df}}
        country <- {{cntry}}

        # plot
        viz <- df_ovc %>%
            filter(operatingunit == country) %>%
            mutate(
                label = paste0(shortname, " (", OVC_HIVSTAT_POS, "/", TX_CURR, ")")
            ) %>%
            ggplot(aes(x = reorder(label, proxy_coverage), y = proxy_coverage)) +
            geom_point(aes(size = TX_CURR, fill = proxy_coverage),
                       color = grey50k, shape = 21, show.legend = F) +
            geom_text(aes(label = percent(proxy_coverage, accuracy = 1)),
                      position = position_nudge(x = .15, y = .15),
                      size = 3, color = grey50k) +
            scale_size_continuous(range = c(3,8)) +
            scale_color_viridis_c(option = "magma",
                                  direction = -1,
                                  aesthetics = c("fill")) +
            geom_hline(aes(yintercept = .9),
                       color = "gray70",
                       size = 0.35,
                       linetype = "dashed",
                       alpha = .8) +
            scale_y_continuous(position = "right", labels = percent) +
            labs(x = "", y = "") +
            coord_flip() +
            # annotate(geom = "curve", x = 1.25, y = .65, xend = 1, yend = .33, curvature = .3,
            #          arrow = arrow(length = unit(4, "mm")), color = grey50k) +
            # annotate(geom = "text", x = 1, y = .68, label = "Circle Sized by TX_CURR", hjust="left",
            #          size = 4, color = grey50k, family = "Gill Sans MT") +
            annotate(geom = "text", x = 13, y = .88, label = "90% threshold", hjust="right",
                     size = 4, color = grey50k, family = "Gill Sans MT") +
            si_style_nolines() +
            theme(
                axis.text.x = element_blank()
            )

        print(viz)

        return(viz)
    }


    #' Viz OVC Proxy Coverage
    #'
    #' @param spdf PEPFAR Spatial Data
    #' @param df OVC Coverage data
    #' @param terr_raster RasterLayer
    #' @param cntry OU Name
    #'
    viz_ovc_coverage <- function(spdf, df, terr_raster, cntry, save = FALSE) {

        # Params
        df_geo <- {{spdf}}
        df_ovc <- {{df}}
        terr <- {{terr_raster}}
        country <- {{cntry}}
        sgraph <- {{save}}

        # Map
        map <- map_ovc_coverage(df_geo, df_ovc, terr, country)

        # graph
        viz <- plot_ovc_coverage(df_ovc, country)

        # VIZ COMBINED & SAVED
        graph <- (map + viz) +
            plot_layout(nrow = 1) +
            plot_annotation(
                title = "FY20Q2 | OVC Program Coverage Proxy of TX_CURR (Under 20yo)",
                subtitle = "The size of circles represents the volume of TX_CURR",
                caption = get_caption(country)
            )

        print(graph)

        if (sgraph == TRUE) {
            ggsave(here("Graphics", get_output_name(country)),
               scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
        }

        return(graph)
    }


    #' Append attributes to PEPFAR Geodata
    #'
    #' @param spdf
    #' @param df_ous
    #' @param df_snus
    #' @param df_psnus
    #' @return spdf with lebels (type) and other details
    #'
    append_attributes <- function(spdf, df_ous, df_snus, df_psnus){

        # Get params
        spdf_pepfar <- {{spdf}}
        ous <- {{df_ous}}
        snus <- {{df_snus}}
        psnus <- {{df_psnus}}

        ## Geo - identify ous
        spdf_pepfar <- spdf_pepfar %>%
            left_join(ous, by = "uid")

        ## Geo - identify snu1
        spdf_pepfar <- spdf_pepfar %>%
            left_join(snus, by = c("uid" = "snu1uid")) %>%
            rename(countryname = countryname.x) %>%
            mutate(
                type = case_when(
                    is.na(type) & !is.na(operatingunit) ~ "SNU1",
                    TRUE ~ type
                ),
                countryname = case_when(
                    is.na(countryname) & type == "SNU1" ~ countryname.y,
                    TRUE ~ countryname
                )
            ) %>%
            dplyr::select(-countryname.y)

        ## Geo - identify psnu
        spdf_pepfar <- spdf_pepfar %>%
            left_join(psnus, by = c("uid" = "psnuuid")) %>%
            rename(
                countryname = countryname.x,
                operatingunit = operatingunit.x,
                snu1 = snu1.x
            ) %>%
            mutate(
                type = case_when(
                    is.na(type) & !is.na(operatingunit.y) ~ "PSNU",
                    TRUE ~ type
                ),
                countryname = case_when(
                    is.na(countryname) & type == "PSNU" ~ countryname.y,
                    TRUE ~ countryname
                ),
                operatingunit = case_when(
                    is.na(operatingunit) & type == "PSNU" ~ operatingunit.y,
                    TRUE ~ operatingunit
                ),
                snu1 = case_when(
                    is.na(snu1) & type == "PSNU" ~ snu1.y,
                    TRUE ~ snu1
                ),
                type = case_when(
                    !is.na(snu1uid) & uid == snu1uid & type == "SNU1" ~ "PSNU",
                    TRUE ~ type
                )
            ) %>%
            dplyr::select(-ends_with(".y"))

        ## Return geodata

        return(spdf_pepfar)
    }


# DATA --------------------------------------------------------------

    ## File path + name
    file_psnu_im <- list.files(
            path = dir_merdata,
            pattern = psnu_im,
            recursive = TRUE,
            full.names = TRUE
        ) %>%
        sort() %>%
        last()

    ## MER PSNU Data
    df_psnu <- vroom(file_psnu_im)

    df_psnu %>% glimpse()

    df_ous <- df_psnu %>%
        distinct(operatingunit, operatingunituid)

    ## SNU1
    df_snus <- df_psnu %>%
        dplyr::select(operatingunit, countryname, snu1, snu1uid) %>%
        distinct(operatingunit, countryname, snu1, snu1uid) %>%
        filter(!is.na(snu1)) %>%
        arrange(operatingunit, countryname, snu1)

    ## PSNU
    df_psnus <- df_psnu %>%
        dplyr::select(operatingunit, operatingunituid, countryname, snu1, snu1uid, psnu, psnuuid) %>%
        distinct(operatingunit, countryname, snu1, snu1uid, psnu, psnuuid) %>%
        arrange(operatingunit, countryname, snu1, psnu)


    ## List of Distinct Orgs

    ## 2619
    lst_psnuuid <- df_psnus %>%
        filter(!is.na(psnuuid), psnuuid != "?") %>%
        distinct(psnuuid) %>%
        pull()

    ## 416
    lst_snu1uid <- df_snus %>%
        filter(!is.na(snu1uid), snu1uid != "?") %>%
        distinct(snu1uid) %>%
        pull()

    ## 28
    lst_ouuid <- df_ous %>%
        pull(operatingunituid)

    ## MER Data Munging

    ## Proxy OVC Coverage
    df_ovc_cov <- df_psnu %>%
        filter(
            fiscal_year == rep_fy,
            fundingagency == rep_agency,
            indicator == "OVC_HIVSTAT_POS" & standardizeddisaggregate == "Total Numerator" |
            indicator == "TX_CURR" & standardizeddisaggregate == "Age/Sex/HIVStatus",
            !trendsfine %in% c("20-24","25-29","30-34","35-39","40-49","50+")
        ) %>%
        group_by(fiscal_year, operatingunit, psnuuid, psnu, indicator) %>%
        summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
        ungroup() %>%
        reshape_msd(clean = TRUE) %>%
        dplyr::select(-period_type) %>%
        spread(indicator, val) %>%
        mutate(
            proxy_coverage = case_when(
                OVC_HIVSTAT_POS > 0 ~ OVC_HIVSTAT_POS/TX_CURR
            ),
            shortname = str_remove(psnu, " District$| County$")
        ) %>%
        filter(
            period == rep_pd,
            !is.na(proxy_coverage)
        )



    ## PEPFAR OU/Countries

    ous <- identify_ouuids(username = user, password = mypwd(key)) %>%
        as_tibble() %>%
        rename(countryname = country)

    ## Geodata

    ## Terrain Raster
    terr <- get_raster(terr_path = dir_terr)

    ## ORGs
    spdf_pepfar <- list.files(
            path = dir_geodata,
            pattern = "VcPepfarPolygons.shp",
            recursive = T,
            full.names = T
        ) %>%
        sort() %>%
        last() %>%
        read_sf()


    ## Geo - identify features
    spdf_pepfar <- append_attributes(spdf = spdf_pepfar,
                                     df_ous = ous,
                                     df_snus = df_snus,
                                     df_psnus = df_psnus)

    spdf_pepfar %>%
        st_set_geometry(NULL) %>%
        filter(!is.na(type)) %>%
        head(n = 20) %>%
        prinf()


# VIZ --------------------------------------

    ## Test Individual OVC maps
    cname <- "Zambia"


    map_ovc_coverage(spdf = spdf_pepfar,
                  df = df_ovc_cov,
                  terr_raster = terr,
                  cntry = cname,
                  ovc_variable = "proxy_coverage")

    plot_ovc_coverage(df = df_ovc_cov,
                      cntry = cname)


    viz_ovc_coverage(spdf = spdf_pepfar,
                     df = df_ovc_cov,
                     terr_raster = terr,
                     cntry = cname,
                     save = TRUE)


    # Batch mapping
    df_ovc_cov %>%
        distinct(operatingunit) %>%
        pull() %>%
        map(.x, .f = ~ viz_ovc_coverage(spdf = spdf_pepfar,
                                        df = df_ovc_cov,
                                        terr_raster = terr,
                                        cntry = .x,
                                        save = TRUE))






## END ##

