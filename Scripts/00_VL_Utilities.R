##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa, G.Sarfaty & T.Essam | USAID
##  EDITED:  T. Essam (2020-10-01)
##  PURPOSE: VL-Utilities
##  LICENCE: MIT
##  DATE:    2020-10-21
##  UPDATED: 2020-11-12


#' @title Viral Load Datasets
#'
#' @param df_msd
#' @param rep_agency
#' @param rep_fy
#' @param rep_pd
#' @param peds
#' @param lst_ous
#' @return VL Datasets
#'
extract_viralload <-
  function(df_msd,
           rep_agency = "USAID",
           rep_fy = 2020,
           rep_pd = 3,
           peds = FALSE,
           lst_ous = NULL) {

    # Variables
    df <- {{df_msd}}
    agencies <- {{rep_agency}}
    fy <- {{rep_fy}}

    pd <- {{rep_pd}}
    pd <- paste0("FY", str_sub(as.character(fy), 3, 4), "Q", pd)

    peds <- {{peds}}
    ous <- {{lst_ous}}


    # Filter Indicators and
    df_vl <- df %>%
      filter(
        fiscal_year == fy,
        fundingagency %in% agencies,
        indicator %in% c("TX_PVLS", "TX_CURR")
    )

    # Target ous
    if (!is.null(ous)) {
      df_vl <- df_vl %>%
        filter(operatingunituid %in% ous)
    }

    # Peds option
    if (isTRUE(peds)) {

      df_peds <- extract_peds_viralload(df_msd = df_vl, rep_qtr = pd)

      return(df_peds)
    }

    # Proceed with the filter, only if peds = FALSE
    df_vl <- df_vl %>%
      filter(
        standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")
      ) %>%
      mutate(
        indicator = if_else(
          numeratordenom == "D",
          paste0(indicator, "_D"),
          indicator
        ),
        fundingagency = if_else(
          fundingagency == "HHS/CDC",
          "CDC",
          fundingagency
        )
      ) %>%
      group_by(fiscal_year,
               operatingunit,
               snu1,
               psnuuid,
               psnu,
               indicator,
               fundingagency) %>%
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
      ungroup() %>%
      reshape_msd(clean = TRUE) %>%
      dplyr::select(-period_type) %>%
      spread(indicator, val)


    # Calculate VL Stats
    df_vl <- df_vl %>%
      group_by(operatingunit, snu1, psnuuid, psnu, fundingagency) %>%
      mutate(VLC = TX_PVLS_D / lag(TX_CURR, 2, order_by = period)) %>%
      ungroup() %>%
      filter(period == pd) %>%
      clean_psnu() %>%
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
        psnu_short = psnu
      )

    return(df_vl)
  }


#' @title PEDS Viral Load Datasets
#' @description This is a subset of extract_viralload
#' @param df_msd
#' @param rep_qtr
#' @return VL Datasets
#'
extract_peds_viralload <-
  function(df_msd, rep_qtr = "FY20Q3") {

    # Variables
    df <- {{df_msd}}
    pd <- {{rep_qtr}}

    # Proceed with filters
    df_vl <- df %>%
      filter(
        standardizeddisaggregate %in%
               c("Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus")
      ) %>%
      mutate(
        indicator = if_else(
          numeratordenom == "D",
          paste0(indicator, "_D"),
          indicator),
        fundingagency = if_else(
          fundingagency == "HHS/CDC",
          "CDC",
          fundingagency)
      ) %>%
      group_by(fiscal_year,
               operatingunit,
               psnuuid,
               psnu,
               trendscoarse,
               indicator,
               fundingagency) %>%
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
      ungroup() %>%
      reshape_msd(clean = TRUE) %>%
      dplyr::select(-period_type) %>%
      spread(indicator, val)

    # Calculate VL Stats
    df_vl <- df_vl %>%
      group_by(operatingunit, psnuuid, psnu, trendscoarse, fundingagency) %>%
      mutate(VLC = TX_PVLS_D / lag(TX_CURR, 2, order_by = period)) %>%
      ungroup() %>%
      filter(period == pd, trendscoarse == "<15") %>%
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

    return(df_vl)
  }


#' EID PEDS Datasets
#'
#' @param df_msd     PEPFAR MSD datasets
#' @param rep_agency Agency (ies)
#' @param rep_fy     Fiscal Year
#' @param rep_pd     Quarter, eg: 2 for Q2 data, NUll for cumulative
#' @param lst_ou     List of OU UIDs
#' @return EID VL Datasets
#'
extract_eid_viralload <-
  function(df_msd,
           rep_agency = "USAID",
           rep_fy = 2020,
           rep_pd = NULL,
           lst_ous = NULL) {

    # Variables
    df <- {{df_msd}}
    agencies <- as.vector({{rep_agency}})
    fy <- {{rep_fy}}
    pd <- {{rep_pd}}
    pd <- ifelse(is.null(pd),
                 paste0("FY", str_sub(fy, 3, 4)),
                 paste0("FY", str_sub(fy, 3, 4), "Q", pd))

    ous <- {{lst_ous}}

    # Filter and summarise
    df_eid <- df %>%
      filter(
        fiscal_year == fy,
        fundingagency %in% agencies,
        indicator %in% c("PMTCT_EID_Less_Equal_Two_Months", "PMTCT_EID")
      )

    # Target ous
    if (!is.null(ous)) {
      df_eid <- df_eid %>%
        filter(operatingunituid %in% ous)
    }

    df_eid <- df_eid %>%
      mutate(
        indicator = if_else(
          numeratordenom == "D",
          paste0(indicator, "_D"),
          indicator
        ),
        fundingagency = if_else(
          fundingagency == "HHS/CDC",
          "CDC",
          fundingagency
        )
      ) %>%
      filter(indicator != "PMTCT_EID") %>%
      group_by(fiscal_year,
               operatingunit,
               snu1,
               psnuuid,
               psnu,
               indicator,
               fundingagency) %>%
      #summarise(across(starts_with("cumulative"), sum, na.rm = TRUE)) %>%
      summarise(across(qtr1:cumulative, sum, na.rm = TRUE)) %>%
      ungroup() %>%
      reshape_msd(clean = TRUE) %>%
      dplyr::select(-period_type) %>%
      spread(indicator, val) %>%
      mutate(eid_cov_under2 = (PMTCT_EID_Less_Equal_Two_Months / PMTCT_EID_D)) %>%
      filter(period == pd)

    return(df_eid)
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
map_viralload <-
  function(spdf,
           df,
           vl_variable,
           cntry,
           terr_raster,
           peds = FALSE,
           caption = TRUE,
           save = FALSE,
           agency = TRUE,
           facet_rows = 1) {

    # Variables
    df_geo <- {{spdf}}
    df_vl <- {{df}}
    country <- {{cntry}}
    vl_var <- {{vl_variable}}
    terr <- {{terr_raster}}
    peds_title <- {{peds}}

    # Country boundaries
    df_geo0 <- df_geo %>%
      filter(countryname == country, type == "OU")

    # PSNU Geo + VL data
    df_geo2 <- df_geo %>%
      #filter(countryname == country, type == "PSNU") %>%
      filter(countryname == country) %>%
      left_join(df_vl, by = c("uid" = "psnuuid")) %>%
      dplyr::filter(!is.na(VLnC))

    # Basemap
    base_map <- get_basemap(spdf = df_geo,
                            cntry = country,
                            terr_raster = terr)

    # Map specific variable
    if (tolower(vl_var) == "vls") {
      theme_map <- base_map +
        geom_sf(
          data = df_geo2,
          aes(fill = VLS),
          lwd = .2,
          color = grey10k,
          alpha = 0.8
        ) +
        scale_fill_stepsn(
          breaks = c(0, .8, .9, 1),
          guide = guide_colorsteps(even.steps = FALSE),
          na.value = grey40k,
          limits = c(0, 1),
          labels = percent,
          colors = RColorBrewer::brewer.pal(n = 11, name = "RdYlGn")
        )

      if (agency == TRUE) {
        theme_map <-
          theme_map + facet_wrap( ~ fundingagency, nrow = facet_rows)
      }

    }
    else if (tolower(vl_var) == "vlc") {
      theme_map <- base_map +
        geom_sf(
          data = df_geo2,
          aes(fill = VLC),
          lwd = .2,
          color = grey10k
        ) +
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
        theme_map <-
          theme_map + facet_wrap( ~ fundingagency, nrow = facet_rows)
      }
    }
    else {
      theme_map <- base_map +
        geom_sf(
          data = df_geo2,
          aes(fill = VLnC),
          lwd = .2,
          color = grey10k
        ) +
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
        theme_map <-
          theme_map + facet_wrap( ~ fundingagency, nrow = facet_rows)
      }
    }

    # Add country boundaries and apply map theme
    theme_map <- theme_map +
      geom_sf(
        data = df_geo0,
        colour = grey90k,
        fill = NA,
        size = 1
      ) +
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


    #print(theme_map)


    if (save == TRUE) {
      ggsave(
        here::here("Graphics", get_output_name(country, var = vl_var)),
        plot = last_plot(),
        scale = 1.2,
        dpi = 400,
        width = 10,
        height = 7,
        units = "in"
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
map_viralloads <-
  function(spdf,
           df,
           cntry,
           terr_raster,
           save = FALSE,
           agency = TRUE,
           facet_rows = 1) {

    # Variables
    df_geo <- {{spdf}}
    df_vl <- {{df}}
    country <- {{cntry}}
    terr <- {{terr_raster}}
    facets <- {{facet_rows}}

    # Notification
    print(country)

    # Check for valid data
    n <- df_vl %>%
      filter(operatingunit == country) %>%
      nrow()

    if (n == 0) {
      return(NULL)
    }

    # VLS
    m_vls <- map_viralload(
      spdf = df_geo,
      df = df_vl,
      vl_variable = "VLS",
      cntry = country,
      terr_raster = terr,
      caption = FALSE,
      agency = agency,
      facet_rows = facets
    )

    # VLC
    m_vlc <- map_viralload(
      spdf = spdf_pepfar,
      df = df_vl,
      vl_variable = "VLC",
      cntry = country,
      terr_raster = terr,
      caption = FALSE,
      agency = agency,
      facet_rows = facets
    )

    # VLnC
    m_vlnc <- map_viralload(
      spdf = spdf_pepfar,
      df = df_vl,
      vl_variable = "VLnC",
      cntry = country,
      terr_raster = terr,
      caption = FALSE,
      agency = agency,
      facet_rows = facets
    )

    # ALL
    m_all <- (m_vlc + m_vlnc + m_vls) +
      plot_layout(widths = c(1, 1, 1)) +
      plot_annotation(caption = get_caption(country))

    #print(m_all)

    # Save output
    if (save == TRUE) {
      ggsave(
        here("Graphics", get_output_name(country,
                                         var = "VL",
                                         agency = agency)),
        plot = last_plot(),
        scale = 1.2,
        dpi = 400,
        width = 10,
        height = 7,
        units = "in"
      )
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
map_peds_viralloads <-
  function(spdf,
           df,
           cntry,
           terr_raster,
           save = FALSE,
           agency = TRUE,
           facet_rows = 1) {

    # Variables
    df_geo <- {{spdf}}
    df_vl <- {{df}}
    country <- {{cntry}}
    terr <- {{terr_raster}}
    facets <- {{facet_rows}}

    # Check for valid data
    n <- df_vl %>%
      filter(operatingunit == country) %>%
      nrow()

    if (n == 0) {
      return(NULL)
    }

    # VLS
    m_vls <- map_viralload(
      spdf = df_geo,
      df = df_vl,
      vl_variable = "VLS",
      cntry = country,
      terr_raster = terr,
      peds = TRUE,
      caption = FALSE,
      agency = agency,
      facet_rows = facets
    )

    # VLC
    m_vlc <- map_viralload(
      spdf = spdf_pepfar,
      df = df_vl,
      vl_variable = "VLC",
      cntry = country,
      terr_raster = terr,
      peds = agency,
      caption = FALSE,
      agency = agency,
      facet_rows = facets
    )

    # ALL
    m_all <- (m_vlc + m_vls) +
      plot_layout(widths = c(1, 1)) +
      plot_annotation(caption = get_caption(country))

    print(m_all)

    # Save output
    if (save == TRUE) {
      ggsave(
        here(
          "Graphics",
          str_replace(
            get_output_name(country, var = "VLC_S", agency = agency),
            "_ViralLoad_",
            "_ViralLoad_PEDS_")),
        plot = last_plot(),
        scale = 1.2,
        dpi = 400,
        width = 10,
        height = 7,
        units = "in")
    }

    return(m_all)
  }



#' Map generic variables based on data frame
#'
#' @param spdf    PEPFAR Spatial Data
#' @param df      wrangled VL data frame
#' @param mapvar  variable of interest to map
#' @param cntry   OU Name
#' @param terr_raster RasterLayer
#' @param save    Save the output to ./Graphics folder
#' @param facet_rows sets the number of facets for multiple agencies
#' @param gen_title returns a generic title directly passed to map
#' @param agency    if true adds facet wrap to map / owise data are combined
#' @param four_parts Use 4 parts classification: [0-25, 25-50, 50-75, 75-100]
#' @return ggplot plot of the map
#

map_generic <-
  function(spdf,
           df_gen,
           mapvar,
           cntry,
           terr_raster,
           save = FALSE,
           agency = TRUE,
           facet_rows = 1,
           gen_title = "",
           four_parts = TRUE) {

    df_geo <- {{spdf}}
    df_oth <- {{df_gen}}

    varname <- df_oth %>%
      dplyr::select({{mapvar}}) %>%
      names()

    country <- {{cntry}}
    terr <- {{terr_raster}}
    facets <- {{facet_rows}}

    # Country boundaries
    df_geo0 <- df_geo %>%
      filter(countryname == country, type == "OU")

    # PSNU Geo + VL data
    df_geo2 <- df_geo %>%
      filter(countryname == country, type == "PSNU") %>%
      left_join(df_oth, by = c("uid" = "psnuuid")) %>%
      filter(!is.na({{mapvar}}))

    # Basemap
    base_map <-
      get_basemap(spdf = df_geo,
                  cntry = country,
                  terr_raster = terr)

    # Thematic map
    theme_map <-
      base_map +
      geom_sf(
        data = df_geo2,
        aes(fill = {{mapvar}}),
        lwd = .2,
        color = grey10k,
        alpha = 0.8
      )

    if (four_parts == TRUE) {
      theme_map <-  theme_map +
        scale_fill_stepsn(
          colors = c("#D73027","#FC8D59","#FEE08B","#D9EF8B","#91CF60","#1A9850"),
          breaks = seq(0, 1, by = 0.25),
          guide = guide_colorsteps(show.limits = F, even.steps = F),
          na.value = grey40k,
          limits = c(0, 1),
          labels = percent,
          oob = scales::oob_squish,
          values = scales::rescale(seq(0, 1, by = 0.25), c(0, 1))
        )

    } else {
      theme_map <-  theme_map  +
        scale_fill_viridis_c(
          na.value = grey40k,
          direction = -1,
          option = "viridis",
          labels = percent_format(accuracy = 1)
        )
    }

    if (agency == TRUE) {
      theme_map <- theme_map +
        facet_wrap( ~ fundingagency, nrow = facet_rows)

    } else {
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

    # Save output
    if (save == TRUE) {
      ggsave(
        here("./Graphics",
             paste0(rep_pd, "_",
                    toupper(varname), "_",
                    toupper(cntry), "_",
                    Sys.Date(),
                    ".png")),
        plot = last_plot(),
        scale = 1.2,
        dpi = 400,
        width = 10,
        height = 7,
        units = "in")
    }

    return(theme_map)
  }


#' Map VL + EID Coverage combined
#'
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
#'
# step 1 - retrieve viral load map
map_vlc_eid <-
  function(spdf,
           df,
           cntry,
           vl_variable,
           terr_raster,
           save_all = T,
           facet_rows = 2,
           df2,
           mapvar) {

    df_geo <- {{spdf}}
    df_vl <- {{df}}
    vl_var <- {{vl_variable}}
    country <- {{cntry}}
    df_oth <- {{df2}}
    terr <- {{terr_raster}}
    varname <- df_oth %>% dplyr::select({{mapvar}}) %>% names()
    facets <- {{facet_rows}}

    vlc <- map_viralload(
      spdf = df_geo,
      df = df_vl,
      peds = TRUE,
      vl_variable = vl_var,
      cntry = country,
      terr_raster = terr,
      agency = T,
      facet_rows = facets,
      caption = FALSE
    )

    # step 2 - retrieve EID coverage map
    eid <- map_generic(
      spdf = df_geo,
      df_gen = df_oth,
      mapvar = {{mapvar}},
      cntry = country,
      terr_raster = terr,
      agency = T,
      facet_rows = facets,
      gen_title = "Early Infant Diagnosis (Under 2yo)"
    )

    m_all <- (vlc + eid) +
      plot_layout(widths = c(1, 1)) +
      plot_annotation(
        caption = paste0(
          "OHA/SIEI - Data Source: MSD ",
          rep_pd,
          "i_",
          cntry,
          "_VLC + EID \n",
          "Produced on ",
          Sys.Date(),
          ", Missing data shown in gray."
        ),
        title = paste0(str_to_upper(country),
                       " VLC & EID PROXY COVERAGE SUMMARY")
      )

    # Print and save
    if (save_all == TRUE) {
      print(m_all)

      ggsave(
        here(
          "Graphics",
          get_output_name(cntry, var = "VL_EID_Coverage")
        ),
        plot = last_plot(),
        scale = 1.2,
        dpi = 400,
        width = 10,
        height = 7,
        units = "in"
      )
    }
  }
