##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa & G.Sarfaty | USAID
##  PURPOSE: Geo-depiction of VL - % not covered
##  LICENCE: MIT
##  DATE:    2020-09-04

# Libraries
library(tidyverse)
library(readxl)
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
library(googlesheets4)
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

    # Graphic caption
    caption <- paste0("Source: FY20Q3i MSD - USAID Only,
            VLC = TX_PVLS / TX_CURR (2 periods prior)
            Not Covered = 1 - VLC
            Produced on ", format(Sys.Date(), "%Y%m%d"))


    get_caption <- function(var) {

        caption <- paste0(
            "Source: FY20Q3i MSD - USAID Only,
            VLC = TX_PVLS / TX_CURR (2 periods prior), Not Covered = 1 - VLC
            Variable mapped: ", {{var}},
            ", Produced on ",
            format(Sys.Date(), "%Y%m%d")
        )

        return(caption)
    }

    # Graphic file name
    get_output_name <- function(country, var = "VLC") {

        name <- paste0("FY20Q3_ViralLoad_",
                       toupper({{var}}),
                       "_", toupper({{country}}),
                       "_",
                       format(Sys.Date(), "%Y%m%d"),
                       ".png")

        return(name)
    }



# FUNCTIONS ---------------------------------------------------------

    #' Terrain data
    #'
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


    #' Basemap
    #'
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

    #terr <- get_raster()
    #get_basemap(spdf_pepfar, cntry = cntry, terr_raster = terr)

    #' Viz geodata
    #'
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

    #map_org(spdf_ou, org_uid = "XOivy2uDpMF")
    #map_org(spdf_ou, org_uid = "XOivy2uDpMF", title = "Testing")

    #' Map VL S/C/nC
    #'
    #'
    map_viralload <- function(spdf, df, vl_variable, cntry, terr_raster, save = FALSE) {

        df_geo <- {{spdf}}

        country <- {{cntry}}

        vl_var <- {{vl_variable}}

        terr <- {{terr_raster}}

        # Country boundaries
        df_geo0 <- df_geo %>% filter(countryname == country, type == "OU")

        # PSNU Geo + VL data
        df_geo2 <- df_geo %>%
            filter(countryname == country, type == "PSNU") %>%
            left_join(df, by = c("uid" = "psnuuid")) %>%
            filter(!is.na(VLnC))

        # Basemap
        base_map <- get_basemap(spdf = df_geo, cntry = country, terr_raster = terr)

        # Map specific variable
        # theme_map <- case_when(
        #     tolower(vl_var) == "vls" ~ (base_map + geom_sf(data = df_geo2, aes(fill = VLS), lwd = .2, color = grey10k)),
        #     tolower(vl_var) == "vlc" ~ (base_map + geom_sf(data = df_geo2, aes(fill = VLC), lwd = .2, color = grey10k)),
        #     TRUE ~ (base_map + geom_sf(data = df_geo2, aes(fill = VLnC), lwd = .2, color = grey10k))
        # )

        if (tolower(vl_var) == "vls") {
            theme_map <- base_map +
                geom_sf(data = df_geo2, aes(fill = VLS), lwd = .2, color = grey10k)
        }
        else if (tolower(vl_var) == "vlc") {
            theme_map <- base_map +
                geom_sf(data = df_geo2, aes(fill = VLc), lwd = .2, color = grey10k)
        }
        else {
            theme_map <- base_map +
                geom_sf(data = df_geo2, aes(fill = VLnC), lwd = .2, color = grey10k)
        }


        theme_map <- theme_map +
            scale_fill_gradient2(
                low = "yellow",
                high = "brown",,
                limits = c(0, 1),
                labels = percent
            ) +
            geom_sf(data = df_geo0, colour = grey90k, fill = NA, size = 1) +
            si_style_map() +
            labs(caption = get_caption(var = vl_var)) +
            theme(
                legend.position =  "bottom",
                legend.direction = "horizontal",
                legend.key.width = ggplot2::unit(1.5, "cm"),
                legend.key.height = ggplot2::unit(.5, "cm")
            )

        print(theme_map)

        if(save == TRUE) {
            ggsave(here::here("Graphics", get_output_name(country, var = vl_var)),
                   plot = last_plot(), scale = 1.2, dpi = 400, width = 10, height = 7, units = "in")
        }

        return(theme_map)
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


    df_ous <- df_psnu %>%
        distinct(operatingunit, operatingunituid) %>%
        arrange(operatingunit)

    df_ous %>% prinf()


    df_cntries <- df_psnu %>%
        distinct(operatingunit, countryname) %>%
        arrange(operatingunit, countryname)

    df_cntries %>% prinf()


    df_snus <- df_psnu %>%
        select(operatingunit, countryname, snu1, snu1uid) %>%
        distinct(operatingunit, countryname, snu1, snu1uid) %>%
        filter(!is.na(snu1)) %>%
        arrange(operatingunit, countryname, snu1)

    df_snus %>% head()


    df_psnus <- df_psnu %>%
        select(operatingunit, operatingunituid, countryname, snu1, snu1uid, psnu, psnuuid) %>%
        distinct(operatingunit, countryname, snu1, snu1uid, psnu, psnuuid) %>%
        arrange(operatingunit, countryname, snu1, psnu)

    df_psnus %>% head()


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
        select(-period_type) %>%
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
                VLnC > .2 ~ psnu_short,
                TRUE ~ ""
            )
        )

    df_vl %>% glimpse()



    ## PEPFAR OU/Countries

    ous <- identify_ouuids(username = user, password = mypwd(key)) %>%
        as_tibble() %>%
        rename(countryname = country)

    ous %>% glimpse()

    ous %>% prinf()


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
        select(-countryname.y)

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
            )
        ) %>%
        select(-ends_with(".y"))


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
        geom_sf(aes(fill = countryname), colour = "white", size = .5, show.legend = F) +
        si_style_map()


    ## Individual country maps
    spdf_ou %>%
        st_set_geometry(NULL) %>%
        pull(uid) %>%
        first() %>%
        map(.x, .f = ~map_org(spdf_ou, org_uid = .x, title = .x))

    spdf_ou %>%
        st_set_geometry(NULL) %>%
        pull(uid) %>%
        last() %>%
        map(.x, .f = ~map_org(spdf_ou, org_uid = .x, title = .x))


    ## Individual country basemaps => Ressource Intensive
    spdf_ou %>%
        st_set_geometry(NULL) %>%
        pull(countryname) %>%
        nth(8) %>%                  # This is test for Cote d'Ivoire
        map(.x, .f = ~get_basemap(spdf = spdf_pepfar,
                                  cntry = .x,
                                  terr_raster = terr))


    ## Individual VLS maps
    map_viralload(spdf = spdf_pepfar,
                  df = df_vl,
                  vl_variable = "VLS",
                  cntry = "Zambia",
                  terr_raster = terr)

    map_viralload(spdf = spdf_pepfar,
                  df = df_vl,
                  vl_variable = "VLC",
                  cntry = "Zambia",
                  terr_raster = terr,
                  save = TRUE)


    df_vl %>%
        filter(!str_detect(operatingunit, " Region$"), operatingunit != "South Africa") %>%
        distinct(operatingunit) %>%
        pull() %>%
        #nth(3) %>%                  # This is a test for DRC
        map(.x, .f = ~map_viralload(spdf = spdf_pepfar,
                                    df = df_vl,
                                    vl_variable = "VLS",
                                    cntry = .x,
                                    terr_raster = terr,
                                    save = TRUE))




###

viz_map <- terrain_map(countries = "Mozambique", terr_path = dir_terr, mask = TRUE) +
    geom_sf(data = mozgeo %>% filter(!is.na(Not_Cov)), aes(fill = Not_Cov), lwd = .2, color = grey10k) +
    geom_sf(data = moz1, fill = NA, lwd = .2, color = grey30k) +
    scale_fill_gradient2(
        low = "yellow",
        high = "brown",
        labels = percent)+
    si_style_map() +
    theme(
        legend.position =  c(.9, .2),
        legend.direction = "vertical",
        legend.key.width = ggplot2::unit(.5, "cm"),
        legend.key.height = ggplot2::unit(1, "cm")
    )

zim_map<-terrain_map(countries = "Zimbabwe", terr_path = dir_terr, mask = TRUE) +
    geom_sf(data = zimgeo %>% filter(!is.na(Not_Cov)), aes(fill = Not_Cov), lwd = .2, color = grey10k) +
    geom_sf(data = zim1, fill = NA, lwd = .2, color = grey30k) +
    scale_fill_gradient2(
        low = "yellow",
        high = "brown",
        labels=percent_format(accuracy = 1))+
    si_style_map() +
    theme(
        legend.position =  "bottom",
        legend.key.width = ggplot2::unit(1, "cm"),
        legend.key.height = ggplot2::unit(.5, "cm")
    )



(moz_map + zim_map) +
    plot_layout(widths = c(1,1)) +
    plot_annotation(
        caption = "Source: FY20Q3i MSD - USAID Only,
VLC = TX_PVLS / TX_CURR (2 periods prior)
Not Covered = 1-VLC")



ggsave(here("Graphics", "FY20Q3_ViralLoad_BottomOUs.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")



