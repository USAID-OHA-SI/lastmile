##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa & G.Sarfaty | USAID
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
    dir_geo <- "../../GEODATA/PEPFAR"
    dir_terr <- "../../GEODATA/RASTER"
    dir_merdata <- "../../MERDATA"

    ## Q3 MER Data

    psnu_im <- "^MER_.*_PSNU_IM_.*_20200814_.*.zip$"

    ## Reporting Filters

    rep_agency = "USAID"

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
            "OHA/SIEI - Data Source: FY20Q3i MSD - USAID Only,
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
    #' @return ggplot plot of the map
    #'
    map_viralload <- function(spdf, df, vl_variable, cntry,
                              terr_raster, peds = FALSE,
                              caption = TRUE, save = FALSE) {

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
                geom_sf(data = df_geo2, aes(fill = VLS), lwd = .2, color = grey10k) +
                scale_fill_viridis_c(
                    option = "viridis",
                    alpha = 0.9,
                    direction = -1,
                    breaks = c(0, .25, .50, .75, 1.00),
                    limits = c(0, 1),
                    labels = percent
                )
        }
        else if (tolower(vl_var) == "vlc") {

            theme_map <- base_map +
                geom_sf(data = df_geo2, aes(fill = VLC), lwd = .2, color = grey10k) +
                scale_fill_viridis_c(
                    option = "magma",
                    alpha = 0.9,
                    direction = -1,
                    breaks = c(0, .25, .50, .75, 1.00),
                    limits = c(0, 1),
                    labels = percent
                )
        }
        else {

            theme_map <- base_map +
                geom_sf(data = df_geo2, aes(fill = VLnC), lwd = .2, color = grey10k) +
                scale_fill_gradient2(
                    low = "yellow",
                    high = "brown",
                    breaks = c(0, .25, .50, .75, 1.00),
                    limits = c(0, 1),
                    labels = percent
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
    #' @return ggplot plot of the map
    #'
    map_viralloads <- function(spdf, df, cntry, terr_raster, save = FALSE) {

        df_geo <- {{spdf}}
        df_vl <- {{df}}
        country <- {{cntry}}
        terr <- {{terr_raster}}

        # VLS
        m_vls <- map_viralload(spdf = df_geo,
                               df = df_vl,
                               vl_variable = "VLS",
                               cntry = country,
                               terr_raster = terr,
                               caption = FALSE)

        # VLC
        m_vlc <- map_viralload(spdf = spdf_pepfar,
                               df = df_vl,
                               vl_variable = "VLC",
                               cntry = country,
                               terr_raster = terr,
                               caption = FALSE)

        # VLnC
        m_vlnc <- map_viralload(spdf = spdf_pepfar,
                                df = df_vl,
                                vl_variable = "VLnC",
                                cntry = country,
                                terr_raster = terr,
                                caption = FALSE)

        # ALL
        m_all <- (m_vls + m_vlc + m_vlnc) +
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


    #' Map PEDS VL Variables
    #'
    #' @param spdf PEPFAR Spatial Data
    #' @param df wrangled VL data frame
    #' @param cntry OU Name
    #' @param terr_raster RasterLayer
    #' @param save Save the output to ./Graphics folder
    #' @return ggplot plot of the map
    #'
    map_peds_viralloads <- function(spdf, df, cntry, terr_raster, save = FALSE) {

        df_geo <- {{spdf}}
        df_vl <- {{df}}
        country <- {{cntry}}
        terr <- {{terr_raster}}

        # VLS
        m_vls <- map_viralload(spdf = df_geo,
                               df = df_vl,
                               vl_variable = "VLS",
                               cntry = country,
                               terr_raster = terr,
                               peds = TRUE,
                               caption = FALSE)

        # VLC
        m_vlc <- map_viralload(spdf = spdf_pepfar,
                               df = df_vl,
                               vl_variable = "VLC",
                               cntry = country,
                               terr_raster = terr,
                               peds = TRUE,
                               caption = FALSE)

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


# DATA --------------------------------------------------------------

    ## File path + name
    file_psnu_im <- list.files(
            path = dir_merdata,
            pattern = psnu_im,
            recursive = TRUE,
            full.names = TRUE
        )

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
        distinct(operatingunit, countryname, snu1, snu1uid, psnu, psnuuid) %>% View()
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

    ## Filter & Summarize
    df_vl <- df_psnu %>%
        filter(fiscal_year == rep_fy,
               fundingagency == rep_agency,
               indicator %in% c("TX_PVLS","TX_CURR"),
               standardizeddisaggregate %in% c("Total Numerator","Total Denominator"),
               operatingunituid %in% lst_ouuid) %>%
        mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>%
        group_by(fiscal_year, operatingunit, psnuuid, psnu, indicator) %>%
        summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
        ungroup() %>%
        reshape_msd(clean = TRUE) %>%
        dplyr::select(-period_type) %>%
        spread(indicator, val)


    ## Calculate VL Stats
    df_vl <- df_vl %>%
        group_by(operatingunit, psnuuid, psnu) %>%
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
               fundingagency == rep_agency,
               indicator %in% c("TX_PVLS","TX_CURR"),
               standardizeddisaggregate %in% c("Age/Sex/HIVStatus","Age/Sex/Indication/HIVStatus"),
               operatingunituid %in% lst_ouuid) %>%
        mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>%
        group_by(fiscal_year, operatingunit, psnuuid, psnu, trendscoarse, indicator) %>%
        summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
        ungroup() %>%
        reshape_msd(clean = TRUE) %>%
        dplyr::select(-period_type) %>%
        spread(indicator, val) %>%
        group_by(operatingunit, psnuuid, psnu, trendscoarse) %>%
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

    map_viralload(spdf = spdf_pepfar,
                  df = df_vl,
                  vl_variable = "VLS",
                  cntry = cname,
                  terr_raster = terr)

    map_viralload(spdf = spdf_pepfar,
                  df = df_vl,
                  vl_variable = "VLC",
                  cntry = cname,
                  terr_raster = terr)

    map_viralload(spdf = spdf_pepfar,
                  df = df_vl,
                  vl_variable = "VLnC",
                  cntry = cname,
                  terr_raster = terr)

    map_viralloads(spdf = spdf_pepfar,
                   df = df_vl,
                   cntry = cname,
                   terr_raster = terr)

    # PEDS Test
    map_peds_viralloads(spdf = spdf_pepfar,
                   df = df_vl_u15,
                   cntry = cname,
                   terr_raster = terr)


    ## Batch mapping

    map_ous <- df_vl %>%
        filter(!str_detect(operatingunit, " Region$"), operatingunit != "South Africa") %>%
        distinct(operatingunit) %>%
        pull()

    ## VLS
    map_ous %>%
        map(.x, .f = ~map_viralload(spdf = spdf_pepfar,
                                    df = df_vl,
                                    vl_variable = "VLS",
                                    cntry = .x,
                                    terr_raster = terr,
                                    save = TRUE))

    ## VLC
    map_ous %>%
        map(.x, .f = ~map_viralload(spdf = spdf_pepfar,
                                    df = df_vl,
                                    vl_variable = "VLC",
                                    cntry = .x,
                                    terr_raster = terr,
                                    save = TRUE))

    ## VLnC
    map_ous %>%
        map(.x, .f = ~map_viralload(spdf = spdf_pepfar,
                                    df = df_vl,
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
                                     save = TRUE))

    ## PEDS ALL
    map_ous %>%
        map(.x, .f = ~map_peds_viralloads(spdf = spdf_pepfar,
                                     df = df_vl_u15,
                                     cntry = .x,
                                     terr_raster = terr,
                                     save = TRUE))


## END ##

