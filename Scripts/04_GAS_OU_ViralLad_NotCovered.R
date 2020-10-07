##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa & G.Sarfaty | USAID
#   EDITED:  T. Essam (2020-10-01)
##  PURPOSE: Geo-depiction of VL - % not covered
##  LICENCE: MIT
##  DATE:    2020-09-04

# Libraries
library(tidyverse)
library(vroom)
library(sp)
library(sf)
library(raster)
library(gisr)
library(glitr)
library(glamr)
library(janitor)
library(scales)
library(patchwork)
library(ggrepel)
library(here)
library(ICPIutilities)
library(extrafont)
library(viridis)
library(tidytext)

# REQUIRED -------------------------------------------------------------------------------

    ## Get Credentials

    source("../../surprises/surprise.R")

# GLOBALS -------------------------------------------------------------

    ## Data & Output folders

    dir_data <- "Data"
    dir_dataout <- "Dataout"
    dir_gis <- "GIS"
    dir_graphics <- "Graphics"
    dir_geodata <- "../../GEODATA/PEPFAR"
    dir_geo <- "../../GEODATA/PEPFAR"
    dir_terr <- "../../GEODATA/RASTER"
    dir_merdata <- "../../MERDATA"

    ## Q3 MER Data

    psnu_im <- "^MER_.*_PSNU_IM_.*_20200918_.*.zip$"

    ## Reporting Filters

    rep_agency = c("USAID", "HHS/CDC")

    rep_fy = 2020
    rep_qtr = 3

    rep_pd = rep_fy %>%
        as.character() %>%
        str_sub(3,4) %>%
        paste0("FY", ., "Q", rep_qtr)


# FUNCTIONS ---------------------------------------------------------

    #' get_title(country, var = vl_var)
    #'
    #' @param country country name
    #' @param var df variable
    #' @return plot caption
    #'
    get_title <- function(var, peds = FALSE, country = NULL) {

        title <- toupper({{country}})

        var_name <- toupper({{var}})

        var_label <- ""

        # Get label
        if ( var_name == "VLS" ) {
            var_label <- "Viral Load Suppression"
        }
        else if (var_name == "VLC") {
            var_label <- "Viral Load Coverage"
        }
        else if (var_name == "VLNC") {
            var_label <- "Viral Load Not-Covered"
        }
        else {
            var_label <- "Viral Load"
        }

        # PEDS flag
        if (peds == TRUE) {
            var_label <- paste0(var_label, " (Under 15yo)")
        }

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
            "OHA/SIEI - Data Source: FY20Q3i MSD, Missing data in grey,
            VLC = TX_PVLS / TX_CURR (2 periods prior), Not Covered = 1 - VLC\n",
            toupper({{country}})
        )

        if (is.null({{var}})) {

            caption <- paste0(
                caption,
                " - Variable mapped: VLS, VLC, VLnC"
            )
        }
        else {

            caption <- paste0(
                caption,
                " - ",
                "Variable mapped: ",
                toupper({{var}})
            )
        }

        caption <- paste0(
            caption,
            ", Produced on ",
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
    get_output_name <- function(country, var = "VLC") {

        name <- paste0("FY20Q3_ViralLoad_",
                       toupper({{var}}),
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


    #' Map VL S/C/nC
    #'
    #' @param spdf PEPFAR Spatial Data
    #' @param vl_variable Viral Load Variable
    #' @param cntry OU Name
    #' @param terr_raster RasterLayer
    #' @param peds VL for <15
    #' @param caption Add caption to the output?
    #' @param save Save the output to ./Graphics folder
    #' @param agency Facets indicator by fundingagencies in data frame
    #' @facet_rows Number of facet rows to use; default is 1
    #' @return ggplot plot of the map
    #'
    map_viralload <- function(spdf, df, vl_variable, cntry,
                              terr_raster, peds = FALSE,
                              caption = TRUE, save = FALSE,
                              agency = TRUE, facet_rows = 1) {

        # Variables
        df_geo <- {{spdf}}

        df_vl <- {{df}}

        country <- {{cntry}}

        vl_var <- {{vl_variable}}

        terr <- {{terr_raster}}

        peds_title <- {{peds}}

        # Country boundaries
        df_geo0 <- df_geo %>% filter(countryname == country, type == "OU")

        # PSNU Geo + VL data
        df_geo2 <- df_geo %>%
            filter(countryname == country, type == "PSNU") %>%
            left_join(df_vl, by = c("uid" = "psnuuid")) %>%
            filter(!is.na(VLnC))

        # Basemap
        base_map <- get_basemap(spdf = df_geo, cntry = country, terr_raster = terr)

        # Map specific variable
        if (tolower(vl_var) == "vls") {

            theme_map <- base_map +
                geom_sf(data = df_geo2, aes(fill = VLS), lwd = .2, color = grey10k, alpha = 0.8) +
                scale_fill_stepsn(
                    breaks = c(0, .8, .9, 1),
                    guide = guide_colorsteps(even.steps = FALSE),
                    na.value = grey40k,
                    limits = c(0,1),
                    labels = percent,
                    colors = RColorBrewer::brewer.pal(n = 11, name = "RdYlGn")
                )

            if (agency == TRUE) {

                 theme_map <- theme_map + facet_wrap(~fundingagency, nrow = facet_rows)

                 }

        }
        else if (tolower(vl_var) == "vlc") {

            theme_map <- base_map +
                geom_sf(data = df_geo2, aes(fill = VLC), lwd = .2, color = grey10k) +
                scale_fill_viridis_c(
                    option = "magma",
                    alpha = 0.9,
                    direction = -1,
                    na.value = grey40k,
                    breaks = c(0, .25, .50, .75, 1.00),
                    limits = c(0, 1),
                    labels = percent
                )

            if (agency == TRUE) {

                theme_map <- theme_map + facet_wrap(~fundingagency, nrow = facet_rows)

            }
        }


        else {

            theme_map <- base_map +
                geom_sf(data = df_geo2, aes(fill = VLnC), lwd = .2, color = grey10k) +
                # scale_fill_gradient2(
                #     low = "yellow",
                #     high = "brown",
                #     na.value = grey10k,
                #     breaks = c(0, .25, .50, .75, 1.00),
                #     limits = c(0, 1),
                #     labels = percent
                # )
                #
                scale_fill_viridis_c(
                    option = "viridis",
                    alpha = 0.9,
                    direction = -1,
                    na.value = grey40k,
                    breaks = c(0, .25, .50, .75, 1.00),
                    limits = c(0, 1),
                    labels = percent
                )

            if (agency == TRUE) {

                theme_map <- theme_map + facet_wrap(~fundingagency, nrow = facet_rows)

            }
        }

        # Add country boundaries and apply map theme
        theme_map <- theme_map +
            geom_sf(data = df_geo0, colour = grey90k, fill = NA, size = 1) +
            si_style_map()


        # Add Caption
        if (caption == TRUE) {

            theme_map <- theme_map +
                labs(
                    title = get_title(var = vl_var, peds = peds_title),
                    caption = get_caption(country, var = vl_var)
                )
        }
        else {

            theme_map <- theme_map +
                labs(title = get_title(var = vl_var, peds = peds_title))
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


        if(save == TRUE) {
            ggsave(
                here::here("Graphics", get_output_name(country, var = vl_var)),
                plot = last_plot(), scale = 1.2, dpi = 400,
                width = 10, height = 7, units = "in"
            )
        }

        return(theme_map)
    }


    #' Map all VL Variables
    #'
    #' @param spdf PEPFAR Spatial Data
    #' @param df wrangled VL data frame
    #' @param cntry OU Name
    #' @param terr_raster RasterLayer
    #' @param save Save the output to ./Graphics folder
    #' @param facet_rows Number of facets to include for map sequence
    #' @return ggplot plot of the map
    #'
    map_viralloads <- function(spdf, df, cntry, terr_raster, save = FALSE, agency = TRUE, facet_rows = 1) {

        df_geo <- {{spdf}}
        df_vl <- {{df}}
        country <- {{cntry}}
        terr <- {{terr_raster}}
        facets <- facet_rows

        # VLS
        m_vls <- map_viralload(spdf = df_geo,
                               df = df_vl,
                               vl_variable = "VLS",
                               cntry = country,
                               terr_raster = terr,
                               caption = FALSE,
                               agency = TRUE,
                               facet_rows = facets)

        # VLC
        m_vlc <- map_viralload(spdf = spdf_pepfar,
                               df = df_vl,
                               vl_variable = "VLC",
                               cntry = country,
                               terr_raster = terr,
                               caption = FALSE,
                               agency = TRUE,
                               facet_rows = facets)

        # VLnC
        m_vlnc <- map_viralload(spdf = spdf_pepfar,
                                df = df_vl,
                                vl_variable = "VLnC",
                                cntry = country,
                                terr_raster = terr,
                                caption = FALSE,
                                agency = TRUE,
                                facet_rows = facets)

        # ALL
        m_all <- (m_vlc + m_vlnc + m_vls) +
            plot_layout(widths = c(1,1,1)) +
            plot_annotation(
                caption = get_caption(country)
            )

        print(m_all)

        # Save output
        if (save == TRUE) {
            ggsave(here("Graphics", get_output_name(country, var = "VL")),
               plot = last_plot(), scale = 1.2, dpi = 400, width = 10, height = 7, units = "in")
        }

        return(m_all)
    }


    #' Map generic Variables
    #'
    #' @param spdf PEPFAR Spatial Data
    #' @param df wrangled VL data frame
    #' @param cntry OU Name
    #' @param terr_raster RasterLayer
    #' @param save Save the output to ./Graphics folder
    #' @return ggplot plot of the map
    #'
    map_peds_viralloads <- function(spdf, df, cntry, terr_raster, save = FALSE, agency = TRUE, facet_rows = 1) {

        df_geo <- {{spdf}}
        df_vl <- {{df}}
        country <- {{cntry}}
        terr <- {{terr_raster}}
        facets <- facet_rows

        # VLS
        m_vls <- map_viralload(spdf = df_geo,
                               df = df_vl,
                               vl_variable = "VLS",
                               cntry = country,
                               terr_raster = terr,
                               peds = TRUE,
                               caption = FALSE,
                               agency = TRUE,
                               facet_rows = facets)

        # VLC
        m_vlc <- map_viralload(spdf = spdf_pepfar,
                               df = df_vl,
                               vl_variable = "VLC",
                               cntry = country,
                               terr_raster = terr,
                               peds = TRUE,
                               caption = FALSE,
                               agency = TRUE,
                               facet_rows = facets)

        # ALL
        m_all <- (m_vlc + m_vls) +
            plot_layout(widths = c(1,1)) +
            plot_annotation(
                caption = get_caption(country)
            )

        print(m_all)

        # Save output
        if (save == TRUE) {
            ggsave(here("Graphics", str_replace(get_output_name(country, var = "VLC_S"), "_ViralLoad_", "_ViralLoad_PEDS_")),
                   plot = last_plot(), scale = 1.2, dpi = 400, width = 10, height = 7, units = "in")
        }

        return(m_all)
    }



    #' Map generic variables based on data frame
    #'
    #' @param spdf PEPFAR Spatial Data
    #' @param df wrangled VL data frame
    #' @param mapvar variable of interest to map
    #' @param cntry OU Name
    #' @param terr_raster RasterLayer
    #' @param save Save the output to ./Graphics folder
    #' @param facet_rows sets the number of facets for multiple agencies
    #' @param gen_title returns a generic title directly passed to map
    #' @param agency if true adds facet wrap to map / owise data are combined
    #' @return ggplot plot of the map
    # Map Generic

    map_generic <- function(spdf, df_gen, mapvar, cntry, terr_raster, save = FALSE,
                            agency = TRUE, facet_rows = 1, gen_title = "", four_parts = TRUE) {

        df_geo <- {{spdf}}
        df_oth <- {{df_gen}}
        varname <- df_oth %>% dplyr::select({{mapvar}}) %>% names()
        country <- {{cntry}}
        terr <- {{terr_raster}}
        facets <- facet_rows


        # Country boundaries
        df_geo0 <- df_geo %>% filter(countryname == country, type == "OU")

        # PSNU Geo + VL data
        df_geo2 <- df_geo %>%
            filter(countryname == country, type == "PSNU") %>%
            left_join(df_oth, by = c("uid" = "psnuuid")) %>%
            filter(!is.na({{mapvar}}))

        # Basemap
        base_map <- get_basemap(spdf = df_geo, cntry = country, terr_raster = terr)


        theme_map <- base_map +
            geom_sf(data = df_geo2, aes(fill = {{mapvar}}), lwd = .2, color = grey10k, alpha = 0.8)

            if (four_parts == TRUE) {

                 theme_map <-  theme_map +
                     scale_fill_stepsn(
                        colors = c("#D73027", "#FC8D59", "#FEE08B",
                                  "#D9EF8B", "#91CF60", "#1A9850"),
                        breaks = seq(0, 1, by = 0.25),
                        guide = guide_colorsteps(show.limits = F,
                                                 even.steps = F),
                        na.value = grey40k,
                        limits = c(0, 1),
                        labels = percent,
                        oob = scales::oob_squish,
                        values = scales::rescale(seq(0, 1, by = 0.25), c(0,1))
                        #colors = RColorBrewer::brewer.pal(n = 11, name = "RdYlGn")
                        )
            }

        else {
            theme_map <-  theme_map  +
                scale_fill_viridis_c(
                    na.value = grey40k,
                    direction = -1,
                    option = "viridis",
                    labels = percent_format(accuracy = 1)
                    )

        }


            # scale_fill_viridis_c(
            #     option = "viridis",
            #     #guide = guide_coloursteps(show.limits = TRUE),
            #     direction = -1,
            #     na.value = grey40k,
            #     breaks = c(0, .25, .50, .75, 1.00),
            #     limits = c(0, 1),
            #     labels = percent,
            #     oob = squish
            # )

        if (agency == TRUE) {

            theme_map <- theme_map + facet_wrap(~fundingagency, nrow = facet_rows)
        }

        else {

            theme_map <- theme_map
        }

        # Update legend size and position
        theme_map <- theme_map +
            theme(
                legend.position =  "bottom",
                legend.direction = "horizontal",
                legend.key.width = ggplot2::unit(1.5, "cm"),
                legend.key.height = ggplot2::unit(.5, "cm")
            ) +
        labs(title = gen_title)


        print(theme_map)
        return(theme_map)


        # Save output
        if (save == TRUE) {
            ggsave(here("Graphics", paste0(rep_pd, "_", varname, "_", cntry, "_", Sys.Date(), ".png")),
                   plot = last_plot(), scale = 1.2, dpi = 400, width = 10, height = 7, units = "in")
        }

    }


    # Map VL + EID Coverage combined

    #' @param spdf PEPFAR Spatial Data
    #' @param df wrangled VL data frame
    #' @param cntry OU Name
    #' @param vl_variable which viral load variable to use
    #' @param terr_raster RasterLayer
    #' @param save_all saves all outputs to a new file w/ OU name in title
    #' @param facet_rows how many facets to use
    #' @param df2 wrangled generic data frame to be combined with VL data
    #' @param mapvar name of variable from wrangled dataframe to use in maps/titles
    #' @param save Save the output to ./Graphics folder
    #' @param facet_rows Number of facets to include for map sequence


    # step 1 - retrieve viral load map
    map_vlc_eid <- function(spdf, df, cntry, vl_variable, terr_raster,
                            save_all = T,
                            facet_rows = 2, df2, mapvar) {

        df_geo <- {{spdf}}
        df_vl <- {{df}}
        vl_var <- {{vl_variable}}
        country <- {{cntry}}
        df_oth <- {{df2}}
        terr <- {{terr_raster}}
        varname <- df_oth %>% dplyr::select({{mapvar}}) %>% names()
        facets <- facet_rows

        vlc <- map_viralload(spdf = spdf_pepfar,
                             df = df_vl,
                             peds = TRUE,
                             vl_variable = vl_var,
                             cntry = country,
                             terr_raster = terr,
                             agency = T,
                             facet_rows = facets,
                             caption = FALSE)

        # step 2 - retrieve EID coverage map
        eid <- map_generic(spdf = spdf_pepfar,
                           df_gen = df_oth,
                           mapvar = {{mapvar}},
                           cntry = country,
                           terr_raster = terr,
                           agency = T,
                           facet_rows = facets,
                           gen_title = "Early Infant Diagnosis Under Two")

        m_all <- (vlc + eid) +
            plot_layout(widths = c(1,1)) +
            plot_annotation(
                caption = paste0("OHA/SIEI - Data Source: MSD ", rep_pd, "_", cntry, "_VLC + EID \n",
                                 "Produced on ", Sys.Date(), ", Missing data shown in gray."),
                title = paste0(str_to_upper(country), " VLC & EID PROXY COVERAGE SUMMARY")
            )


        print(m_all)

        if (save_all == TRUE) {
            ggsave(here("Graphics", get_output_name(cntry, var = "VL_EID_Coverage")),
                   plot = last_plot(), scale = 1.2, dpi = 400, width = 10, height = 7, units = "in")
        }

    }


    # Create a bar graph for TX_ML_PLP
    tx_graph <- function(spdf, df_gph, mapvar, cntry) {

        df_geo <- {{spdf}}
        df <- {{df_gph}}
        country <- {{cntry}}


        df_geo2 <- df_geo %>%
            filter(countryname == country, type == "PSNU") %>%
            left_join(df, by = c("uid" = "psnuuid")) %>%
            filter(!is.na({{mapvar}})) %>%
            mutate(psnu_short = case_when(
                str_detect(psnu.y, " County$") ~  str_replace_all(psnu.y, " County$", ""),
                str_detect(psnu.y, " District$") ~  str_replace_all(psnu.y, " District$", ""),
                TRUE ~ psnu.y)
            )


      p <-   df_geo2 %>%
            mutate(yorder = tidytext::reorder_within(psnu_short, {{mapvar}}, fundingagency),
                   rank = percent_rank({{mapvar}})) %>%
            ggplot(aes(y = {{mapvar}}, x = yorder)) +
            geom_col(aes(fill = {{mapvar}})) +#aes(fill = if_else(rank > 0.75, grey60k, grey30k))) +
            facet_wrap(~paste0(fundingagency, "\n"), nrow = 2, scales = "free_y") +
            coord_flip() +
            scale_x_reordered() +
            #scale_fill_identity() +
            scale_fill_viridis_c(option = "viridis", direction = -1) +
            scale_y_continuous(labels = percent_format(accuracy = 1)) +
            si_style_xgrid() +
            labs(x = NULL, y = NULL) +
            theme(legend.position = "none",
                  axis.title.y = element_text(margin = margin(t = 0, r = -20)))

      return(p)
    }





# DATA --------------------------------------------------------------

    ## File path + name
    file_psnu_im <- list.files(
            path = dir_merdata,
            pattern = psnu_im,
            recursive = TRUE,
            full.names = TRUE
        ) %>%
        last()

    ## MER PSNU Data
    df_psnu <- vroom(file_psnu_im)

    df_psnu %>% glimpse()

    ## OUs
    df_ous <- df_psnu %>%
        distinct(operatingunit, operatingunituid) %>%
        arrange(operatingunit)

    df_cntries <- df_psnu %>%
        distinct(operatingunit, countryname) %>%
        arrange(operatingunit, countryname)

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

    df_psnus %>% View()



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

    ## Filter & Summarize
    df_vl <- df_psnu %>%
        filter(fiscal_year == rep_fy,
               fundingagency %in% rep_agency,
               indicator %in% c("TX_PVLS","TX_CURR"),
               standardizeddisaggregate %in% c("Total Numerator","Total Denominator"),
               operatingunituid %in% lst_ouuid) %>%
        mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator),
               fundingagency = if_else(fundingagency == "HHS/CDC", "CDC", fundingagency)) %>%
        group_by(fiscal_year, operatingunit, psnuuid, psnu, indicator, fundingagency) %>%
        summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
        ungroup() %>%
        reshape_msd(clean = TRUE) %>%
        dplyr::select(-period_type) %>%
        spread(indicator, val)


    ## Calculate VL Stats
    df_vl <- df_vl %>%
        group_by(operatingunit, psnuuid, psnu, fundingagency) %>%
        mutate(VLC = TX_PVLS_D / lag(TX_CURR, 2, order_by = period)) %>%
        ungroup() %>%
        filter(period == rep_pd) %>%
        mutate(
            VLS = (TX_PVLS / TX_PVLS_D) * VLC,
            VLnC = case_when(
                VLC > 1 ~ 0,
                TRUE ~ 1 - VLC
            ),
            ou_label = paste0(
                operatingunit,
                " (",
                lag(TX_CURR, 2, order_by = period) %>% comma(accuracy = 1),
                ")"
            ),
            psnu_short = case_when(
                str_detect(psnu, " County$") ~  str_remove(psnu, " County$"),
                str_detect(psnu, " District$") ~  str_remove(psnu, " District$"),
                TRUE ~ psnu
            ),
            psnu_label = case_when(
                VLnC > .7 ~ psnu_short,
                TRUE ~ ""
            )
        )


    ## VL PEDs
    df_vl_u15 <- df_psnu %>%
        filter(fiscal_year == rep_fy,
               fundingagency %in% rep_agency,
               indicator %in% c("TX_PVLS","TX_CURR"),
               standardizeddisaggregate %in% c("Age/Sex/HIVStatus","Age/Sex/Indication/HIVStatus"),
               operatingunituid %in% lst_ouuid) %>%
        mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator),
               fundingagency = if_else(fundingagency == "HHS/CDC", "CDC", fundingagency)) %>%
        group_by(fiscal_year, operatingunit, psnuuid, psnu, trendscoarse, indicator, fundingagency) %>%
        summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
        ungroup() %>%
        reshape_msd(clean = TRUE) %>%
        dplyr::select(-period_type) %>%
        spread(indicator, val) %>%
        group_by(operatingunit, psnuuid, psnu, trendscoarse, fundingagency) %>%
        mutate(VLC = TX_PVLS_D / lag(TX_CURR, 2, order_by = period)) %>%
        ungroup() %>%
        filter(period == rep_pd, trendscoarse == "<15") %>%
        mutate(
            VLS = (TX_PVLS / TX_PVLS_D) * VLC,
            VLnC = case_when(
                VLC > 1 ~ 0,
                TRUE ~ 1 - VLC
            ),
            ou_label = paste0(
                operatingunit,
                " (",
                lag(TX_CURR, 2, order_by = period) %>% comma(accuracy = 1),
                ")"
            ),
            psnu_short = case_when(
                str_detect(psnu, " County$") ~  str_remove(psnu, " County$"),
                str_detect(psnu, " District$") ~  str_remove(psnu, " District$"),
                TRUE ~ psnu
            ),
            psnu_label = case_when(
                VLnC > .7 ~ psnu_short,
                TRUE ~ ""
            )
        )


    #PEDs - EID
    df_eid <- df_psnu %>%
        filter(fiscal_year == rep_fy,
               fundingagency %in% rep_agency,
               indicator %in% c("PMTCT_EID_Less_Equal_Two_Months", "PMTCT_EID"),
               operatingunituid %in% lst_ouuid) %>%
        mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator),
               fundingagency = if_else(fundingagency == "HHS/CDC", "CDC", fundingagency)) %>%
        filter(indicator != "PMTCT_EID") %>%
        group_by(fiscal_year, operatingunit, psnuuid, psnu, indicator, fundingagency) %>%
        summarise(across(starts_with("cumulative"), sum, na.rm = TRUE)) %>%
        ungroup() %>%
        reshape_msd(clean = TRUE) %>%
        dplyr::select(-period_type) %>%
        spread(indicator, val) %>%
        mutate(eid_cov_under2 = (PMTCT_EID_Less_Equal_Two_Months / PMTCT_EID_D))


    # TX_ML -- aggregate bad events then divide by TX_CURR_lagged_1
    # Numerator: Number of patients not retained on ART (LTFU, Died, Stopped)
    # Denominator: TX_CURR_lag1 + TX_NEW_curr + TX_RTT

       df_tx_ml <-
           df_psnu %>%
           filter(
               fiscal_year == rep_fy,
               fundingagency %in% c("USAID", "HHS/CDC"),
               indicator %in% c("TX_ML"),
               standardizeddisaggregate %in% c("Age/Sex/ARTNoContactReason/HIVStatus"),
               operatingunituid %in% lst_ouuid,
               typemilitary == 'N',
           ) %>%
       mutate(
           fundingagency = if_else(fundingagency == "HHS/CDC", "CDC", fundingagency),
           otherdisaggregate = str_remove(otherdisaggregate, "No Contact Outcome - "),
           tx_badevents = case_when(
               otherdisaggregate %in% c("Died", "Lost to Follow-Up <3 Months Treatment",
                     "Lost to Follow-Up 3+ Months Treatment",
                     "Refused Stopped Treatment") ~ "Bad events",
               TRUE ~ NA_character_)
           ) %>%
           filter(!is.na(tx_badevents) & indicator == "TX_ML") %>%
      group_by(operatingunit, snu1, snu1uid, psnu, psnuuid, indicator, tx_badevents, fundingagency) %>%
           summarize_at(vars(targets:cumulative), sum, na.rm = TRUE) %>%
           ungroup() %>%
           dplyr::select(-(targets:qtr4), TX_ML_bad = cumulative, -indicator, -tx_badevents)


     df_tx <-
         df_psnu %>%
         filter(
             fiscal_year == rep_fy,
             fundingagency %in% c("USAID", "HHS/CDC"),
             indicator %in% c("TX_CURR", "TX_NEW", "TX_RTT"),
             standardizeddisaggregate %in% c("Total Numerator")
         ) %>%
         mutate(
             fundingagency = if_else(fundingagency == "HHS/CDC", "CDC", fundingagency),
             operatingunituid %in% lst_ouuid
         ) %>%
         group_by(fiscal_year, operatingunit, psnuuid, psnu,indicator, fundingagency) %>%
         summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
         ungroup() %>%
        reshape_msd() %>%
         filter(str_detect(period, "q2|q3")) %>%
         filter(period == "fy2020q3" | (period == "fy2020q2" & indicator == "TX_CURR")) %>%
         mutate(period = case_when(
             period == "fy2020q2" ~ "Q2",
             period == "fy2020q3" ~ "Q3"
         )) %>%
         pivot_wider(names_from = c(indicator, period), values_from = val) %>%
         rowwise() %>%
         mutate(TX_BADEVENTS_DENOM = sum(TX_CURR_Q2, TX_NEW_Q3, TX_RTT_Q3, na.rm = T))

     df_tx_bad <-
         df_tx %>%
          left_join(df_tx_ml, by = c("operatingunit", "psnuuid", "psnu", "fundingagency")) %>%
         mutate(TX_ML_PLP = round((TX_ML_bad / TX_BADEVENTS_DENOM), digits = 3))

      # Test calculations/results
      df_tx_bad %>%
          filter(operatingunit == "Zambia") %>%
          group_by(psnu) %>%
          summarise(sum = (TX_ML_PLP))

    ## PEPFAR OU/Countries

    ous <- identify_ouuids(username = user, password = mypwd(key)) %>%
        as_tibble() %>%
        rename(countryname = country)

    ## Geodata

    ## Terrain Raster
    terr <- get_raster(terr_path = dir_terr)

    ## ORGs
    spdf_pepfar <- list.files(
            path = dir_geo,
            pattern = "VcPepfarPolygons.shp",
            recursive = T,
            full.names = T
        ) %>%
        read_sf()


    ## Geo - identify ous
    spdf_pepfar <- spdf_pepfar %>%
        left_join(ous, by = "uid")

    ## Geo - identify snu1
    spdf_pepfar <- spdf_pepfar %>%
        left_join(df_snus, by = c("uid" = "snu1uid")) %>%
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
        left_join(df_psnus, by = c("uid" = "psnuuid")) %>%
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


# VIZ --------------------------------------

    ## What are these orgs?

    spdf_pepfar %>%
        dplyr::filter(is.na(type)) %>%
        dplyr::select(uid) %>%
        plot()

    ## Geo-units

    ## OUs [Regional + bilateral] => 28
    spdf_pepfar %>%
        dplyr::filter(!is.na(countryname), type == "OU") %>%
        ggplot() +
        geom_sf(aes(fill = countryname), colour = "white", size = .5, show.legend = F) +
        si_style_map()

    ## Countries [Members of Regional OUs, also used as SNU1] => 44
    spdf_pepfar %>%
        filter(!is.na(countryname), type == "Country") %>%
        ggplot() +
        geom_sf(aes(fill = operatingunit), colour = "white", size = .5, show.legend = F) +
        si_style_map()


    ## Individual country maps
    spdf_pepfar %>%
        st_set_geometry(NULL) %>%
        filter(type == "OU") %>%
        pull(uid) %>%
        first() %>%
        map(.x, .f = ~map_org(spdf_pepfar, org_uid = .x, title = .x))

    spdf_pepfar %>%
        st_set_geometry(NULL) %>%
        filter(type == "OU") %>%
        pull(uid) %>%
        last() %>%
        map(.x, .f = ~map_org(spdf_pepfar,
                              org_uid = .x,
                              title = spdf_pepfar %>%
                                  filter(uid == .x) %>% pull(countryname)))


    ## Individual country basemaps => Ressource Intensive
    spdf_pepfar %>%
        st_set_geometry(NULL) %>%
        filter(type == "OU") %>%
        pull(countryname) %>%
        nth(8) %>%                  # This is test for Cote d'Ivoire
        map(.x, .f = ~get_basemap(spdf = spdf_pepfar,
                                  cntry = .x,
                                  terr_raster = terr))


    ## Test Individual VL maps

    cname <- "Nigeria"
    cname <- "Kenya"
    cname <- "Ukraine" # Nah
    cname <- "Lesotho"
    cname <- "Zambia"
    cname <- "Botswana"

    map_viralload(spdf = spdf_pepfar,
                  df = df_vl,
                  vl_variable = "VLS",
                  cntry = cname,
                  terr_raster = terr,
                  agency = TRUE,
                  facet_rows = 2)

    map_viralload(spdf = spdf_pepfar,
                  df = df_vl,
                  vl_variable = "VLC",
                  cntry = cname,
                  terr_raster = terr,
                  agency = T)

    map_viralload(spdf = spdf_pepfar,
                  df = df_vl,
                  vl_variable = "VLnC",
                  cntry = cname,
                  terr_raster = terr)

    map_viralloads(spdf = spdf_pepfar,
                   df = df_vl,
                   cntry = cname,
                   terr_raster = terr,
                   facet_rows = 2)

    # PEDS Test
    map_peds_viralloads(spdf = spdf_pepfar,
                   df = df_vl_u15,
                   cntry = cname,
                   terr_raster = terr,
                   agency = T,
                   facet_rows = 2,
                   save = TRUE)

    # Generic Test

    map_generic(spdf = spdf_pepfar,
                df = df_eid,
                mapvar = eid_cov_under2,
                cntry = cname,
                terr_raster = terr,
                agency = T,
                facet_rows = 2,
                save = TRUE,
                four_parts = F)


    # Test VLc + EID combined
    map_vlc_eid(spdf = spdf_pepfar,
                df = df_vl_u15,
                vl_variable = "VLC",
                cntry = cname,
                terr_raster = terr,
                df2 = df_eid,
                mapvar = eid_cov_under2,
                save_all = T)

    # Test TX_ML_PLP map
    map_generic(spdf = spdf_pepfar,
                df = df_tx_bad,
                mapvar = TX_ML_PLP,
                cntry = cname,
                terr_raster = terr,
                agency = T,
                facet_rows = 2,
                save = TRUE,
                four_parts = F)








    # Dot plot / bar graph to go with map
    tx_graph(spdf_pepfar, df_tx_bad, TX_ML_PLP, "Kenya")




# BATCH VIZ ---------------------------------------------------------------


    ## Batch mapping
    #df_vl_usaid <- df_vl %>% filter(fundingagency == "USAID")

    map_ous <- df_vl %>%
        filter(!str_detect(operatingunit, " Region$"), !operatingunit %in% c("none")) %>%
        distinct(operatingunit) %>%
        pull()



    ## VLS
    map_ous %>%
        nth(2) %>%
        map(.x, .f = ~map_viralload(spdf = spdf_pepfar,
                                    df = df_vl_usaid ,
                                    vl_variable = "VLS",
                                    cntry = .x,
                                    terr_raster = terr,
                                    save = TRUE))

    ## VLC
    map_ous %>%
        map(.x, .f = ~map_viralload(spdf = spdf_pepfar,
                                    df = df_vl_usaid,
                                    vl_variable = "VLC",
                                    cntry = .x,
                                    terr_raster = terr,
                                    save = TRUE))

    ## VLnC
    map_ous %>%
        nth(11) %>%
        map(.x, .f = ~map_viralload(spdf = spdf_pepfar,
                                    df = df_vl_usaid,
                                    vl_variable = "VLnC",
                                    cntry = .x,
                                    terr_raster = terr,
                                    save = TRUE))


    ## ALL
    map_ous %>%
        map(.x, .f = ~map_viralloads(spdf = spdf_pepfar,
                                     df = df_vl,
                                     cntry = .x,
                                     terr_raster = terr,
                                     save = TRUE,
                                     agency = TRUE,
                                     facet_rows = 2))

    ## PEDS ALL
    map_ous %>%
        map(.x, .f = ~map_peds_viralloads(spdf = spdf_pepfar,
                                     df = df_vl_u15,
                                     cntry = .x,
                                     terr_raster = terr,
                                     save = TRUE))


    ## VL + EID Coverage
    map_ous <- df_eid %>%
        filter(!str_detect(operatingunit, " Region$"), !operatingunit %in% c("none")) %>%
        distinct(operatingunit) %>%
        pull()


   # Batch
   map_ous %>%
       nth(13) %>%
       map(.x, .f = ~map_vlc_eid(spdf = spdf_pepfar,
                                 df = df_vl_u15,
                                 vl_variable = "VLC",
                                 cntry = .x,
                                 terr_raster = terr,
                                 df2 = df_eid,
                                 mapvar = eid_cov_under2,
                                 save_all = T))



   # Batch Patient Loss Proxy Maps + graphs


   tx_batch <- function(cname, save = F) {

    country <- {{cname}}
     p1 <-   map_generic(spdf = spdf_pepfar,
                   df = df_tx_bad,
                   mapvar = TX_ML_PLP,
                   cntry = country,
                   terr_raster = terr,
                   agency = T,
                   facet_rows = 2,
                   save = TRUE,
                   four_parts = F)

    p2 <- tx_graph(spdf = spdf_pepfar, df_gph = df_tx_bad, mapvar = TX_ML_PLP, cntry = country )

    (p1 + p2) +
        plot_layout(widths = c(1,1)) +
        plot_annotation(
            caption = paste0("OHA/SIEI - Data Source: MSD ", rep_pd, "_", country, "TX_ML Patient Loss Proxy \n",
                             "TX_ML_PLP = Number not retained on ART (LTFU, Died, Stopped) / (TX_CURR_prev + TX_NEW + TX_RTT) \n",
                             "Produced on ", Sys.Date(), ", Missing data shown in gray on map."),
            title = paste0(str_to_upper(country), " TX_ML Patient Loss Proxy")
        )

    if (save == TRUE) {
      ggsave(here("Graphics", paste0(rep_pd, "_", "TX_ML_PLP_", country, "_", Sys.Date(), ".png")),
             plot = last_plot(), scale = 1.2, dpi = 400, width = 10, height = 7, units = "in")
    }

   }

    tx_batch(cname = "Nigeria", save = TRUE)








## END ##




