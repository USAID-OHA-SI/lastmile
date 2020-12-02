##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa, G.Sarfaty & T.Essam | USAID
##  EDITED:  T. Essam (2020-10-01)
##  PURPOSE: OVC Data Utilities
##  LICENCE: MIT
##  DATE:    2020-11-06


#' @title Extract OVC Proxy Coverage data
#'
#' @param df_msd      Dataframe of MSD PSNU IM
#' @param rep_fy      Reporting Fiscal Year
#' @param rep_age     Filter <15 or <20
#' @param rep_agency  Reporting / Funding Agency, as char or vector
#' @param rep_pd      Reporting Period (eg: FY20Q2)
#' @param sumlevel    Summary level
#' @return df
#'
extract_ovc_coverage <-
  function(df_msd,
           rep_fy = 2020,
           rep_age = "<20",
           rep_agency = NULL,
           rep_pd = "FY20Q2",
           sumlevel = "PSNU") {

    # All Params
    fy <- {{rep_fy}}
    age <- {{rep_age}}
    agency <- {{rep_agency}}
    pd <- {{rep_pd}}
    sumlevel <- {{sumlevel}}

    # Dynamic group columns: PSNU
    grp_cols <- c("fiscal_year", "operatingunit", "snu1uid", "snu1",
                  "psnuuid", "psnu", "indicator")

    # SNU1
    if (sumlevel == "SNU1") {

      grp_cols <- c("fiscal_year",
                    "operatingunit",
                    "snu1uid",
                    "snu1",
                    "indicator")
    }


    # Filter and Calculate Proxy Coverage
    df_proxy_ovc_cov <- df_msd %>%
      filter(fiscal_year == fy)

    # continue with filter
    df_proxy_ovc_cov <- df_proxy_ovc_cov %>%
      filter(indicator == "OVC_HIVSTAT_POS" &
               standardizeddisaggregate == "Total Numerator" |
               indicator == "TX_CURR" &
               standardizeddisaggregate == "Age/Sex/HIVStatus",
             !str_detect(psnu, "_Military"))

    # filter out age bands
    if ( age == "<15") {
      df_proxy_ovc_cov <- df_proxy_ovc_cov %>%
        filter(!trendsfine %in%
                 c("15-19", "20-24","25-29","30-34","35-39","40-49","50+"))
    }
    else if (age == "<20") {
      df_proxy_ovc_cov <- df_proxy_ovc_cov %>%
        filter(!trendsfine %in%
                 c("20-24","25-29","30-34","35-39","40-49","50+"))
    }
    else {
      # message to be printed
      msg <- paste0("ERROR - Unknown OVC Age Band: ", age)

      cat("\n", Wavelength::paint_red(msg), "\n")

      return(NULL)
    }

    # continue with calculations
    df_proxy_ovc_cov <- df_proxy_ovc_cov %>%
      clean_agency() %>%      # Change HHS/CDC to CDC
      clean_psnu()            # Remove Districts, Country, etc from the end


    # Disagg ovc by agency if needed
    if (is.null(agency)) {

      # OVC Cov for all agency
      df_proxy_ovc_cov <- df_proxy_ovc_cov %>%
        #group_by(fiscal_year, operatingunit, snu1uid, snu1, psnuuid, psnu, indicator) %>%
        group_by_at(all_of(grp_cols)) %>%
        summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
        ungroup() %>%
        reshape_msd(clean = TRUE) %>%
        dplyr::select(-period_type) %>%
        spread(indicator, val)

      # Calcualte Proxy Coverage
      df_proxy_ovc_cov <- df_proxy_ovc_cov %>%
        rowwise() %>%
        mutate(
          proxy_coverage = case_when(
            OVC_HIVSTAT_POS > 0 ~ OVC_HIVSTAT_POS/TX_CURR
          ),
          proxy_coverage_max = case_when(
            proxy_coverage > 1 ~ 1,
            TRUE ~ proxy_coverage
          )
        )
    }
    else {
      # OVC Cov for specific agency
      df_proxy_ovc_cov <- df_proxy_ovc_cov %>%
        mutate(
          indicator = paste0(indicator, "_", fundingagency) # Keep track of agency
        ) %>%
        #group_by(fiscal_year, operatingunit, snu1uid, snu1, psnuuid, psnu, indicator) %>%
        group_by_at(all_of(grp_cols)) %>%
        summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
        ungroup() %>%
        reshape_msd(clean = TRUE) %>%
        dplyr::select(-period_type) %>%
        spread(indicator, val)

      df_proxy_ovc_cov <- df_proxy_ovc_cov %>%
        rowwise()  %>%
        mutate(
          TX_CURR = sum(c_across(TX_CURR_CDC:TX_CURR_USAID), na.rm = TRUE),
          flag = case_when(
            OVC_HIVSTAT_POS_USAID > 0 & OVC_HIVSTAT_POS_CDC > 0 ~ "mixed agency ovc"
          ),
          proxy_coverage_usaid = case_when(
            OVC_HIVSTAT_POS_USAID > 0 ~ OVC_HIVSTAT_POS_USAID/TX_CURR
          ),
          proxy_coverage_usaid_max = case_when(
            proxy_coverage_usaid > 1 ~ 1,
            TRUE ~ proxy_coverage_usaid
          ),
          proxy_coverage_cdc = case_when(
            OVC_HIVSTAT_POS_CDC > 0 ~ OVC_HIVSTAT_POS_CDC/TX_CURR
          ),
          proxy_coverage_cdc_max = case_when(
            proxy_coverage_cdc > 1 ~ 1,
            TRUE ~ proxy_coverage_cdc
          )
        )
    }

    # Ungroup
    df_proxy_ovc_cov <- df_proxy_ovc_cov %>%
      ungroup()

    # if sumlevel = SNU1, rename snu1 to psnu
    if (sumlevel == "SNU1") {
      df_proxy_ovc_cov <- df_proxy_ovc_cov %>%
        clean_column(colname = "snu1") %>%
        dplyr::rename_at(vars(starts_with("snu1")), str_replace,
                              pattern = "snu1",
                              replacement = "psnu")
    }

    # Filter by reporting period
    df_proxy_ovc_cov <- df_proxy_ovc_cov %>%
      mutate(shortname = psnu) %>% # Short names for plots
      filter(period == pd)

    # Check valid rows
    if (nrow(df_proxy_ovc_cov) == 0) {
      cat(
        "\n",
        Wavelength::paint_red(
          paste0("No OVC Proxy Coverage data available for ", pd)), "\n")

      return(NULL)
    }

    return(df_proxy_ovc_cov)
  }


#' @title Extract OVC Proxy Coverage data
#'
#' @param df_msd Dataframe of MSD PSNU IM
#' @param rep_fy Reporting Fiscal Year
#' @param rep_agency Reporting / Funding Agency
#' @param rep_pd Reporting Period (eg: FY20Q2)
#' @return df
#'
extract_ovc_tx_overlap <-
  function(df_msd,
           rep_fy = 2020,
           rep_agency = c("USAID","HHS/CDC")) {

    # All Params
    fy <- {{rep_fy}}
    agencies <- {{rep_agency}}

    # Summarise data by psnu / indicator
    df <- df_msd %>%
      filter(
        fiscal_year == fy,
        indicator == "OVC_SERV_UNDER_18" &
          standardizeddisaggregate == "Total Numerator" |
        indicator == "TX_CURR" &
          standardizeddisaggregate == "Age/Sex/HIVStatus",
        fundingagency %in% agencies,
        !trendsfine %in% c("20-24","25-29","30-34","35-39","40-49","50+")
      ) %>%
      mutate(
        fundingagency = case_when(
          fundingagency == "HHS/CDC" ~ "CDC",
          TRUE ~ fundingagency
        )
      ) %>%
      group_by(fiscal_year, fundingagency, operatingunit,
               psnuuid, psnu, indicator) %>%
      summarise(targets = sum(as.integer(targets), na.rm = TRUE)) %>%
      ungroup() %>%
      reshape_msd(clean = TRUE) %>%
      dplyr::select(-period_type)

    # Calculate agency's presence by psnu
    # # of inidicator x agency x psnu
    df_ovc_mix <- df %>%
      group_by(psnuuid) %>%
      count(fundingagency) %>%
      spread(fundingagency, n)

    #
    df <- df %>%
      rename(targets = val) %>%
      left_join(df_ovc_mix, by = "psnuuid") %>%
      mutate(
        ovc_group = case_when(
          CDC > 0 & USAID > 0 ~ "Mixed",
          CDC > 1 & is.na(USAID) ~ "CDC Only",
          USAID > 1 & is.na(CDC) ~ "USAID Only",
          CDC < 2 & is.na(USAID) ~ "OVC & TX do not overlap in this district",
          USAID < 2 & is.na(CDC) ~ "OVC & TX do not overlap in this district"
        ),
        ovc_group = factor(
          ovc_group,
          levels = c("USAID Only", "CDC Only", "Mixed",
                     "OVC & TX do not overlap in this district"))
      ) %>%
      clean_psnu()


    return(df)
  }


#' @title Map OVC Proxy Coverage Data
#'
#' @param spdf        Spatial Dataframe of MSD PSNU IM
#' @param df_ovc      Proxy OVC Coverage data
#' @param terr_raster Terrain Raster
#' @param orglevel    Org Hierarchy Level
#' @param agency      agency specific?
#' @return ggplot plot
#'
map_ovc_coverage <-
  function(spdf, df_ovc, terr_raster,
           orglevel = "PSNU",
           agency = FALSE,
           facet = FALSE) {

    # Params
    orglevel <- {{orglevel}}
    agency <- {{agency}}

    # Identify Country/OU
    country <- df_ovc %>%
      dplyr::distinct(operatingunit) %>%
      pull()

    # Geodata
    spdf_adm0 <- spdf %>%
      filter(operatingunit %in% country, type == "OU")

    # reformat data
    if (agency == TRUE) {
      df_ovc <- df_ovc %>%
        dplyr::mutate(proxy_coverage = proxy_coverage_usaid,
                      proxy_coverage_max = proxy_coverage_usaid_max) %>%
        filter(proxy_coverage > 0, is.na(flag))
    }

    # Append Program data to geo
    spdf_ovc <- spdf %>%
      left_join(df_ovc, by = c("uid" = "psnuuid")) %>%
      filter(proxy_coverage > 0)

    # Get basemap
    basemap <- get_basemap(spdf = spdf,
                           cntry = country,
                           terr_raster = terr,
                           add_admins = TRUE)

    # Overlay program data
    ovc_map <- basemap +
      geom_sf(
        data = spdf_ovc,
        aes(fill = proxy_coverage_max),
        lwd = .2,
        color = grey10k
      ) +
      geom_sf(
        data = spdf_adm0,
        colour = grey90k,
        fill = NA,
        size = 1
      ) +
      scale_fill_viridis_c(
        option = "magma",
        direction = -1,
        labels = percent,
        limits = c(0, 1)
      )
      si_style_map() +
      theme(
        legend.position = "bottom",
        legend.key.width = ggplot2::unit(1, "cm"),
        legend.key.height = ggplot2::unit(.5, "cm")
      )

    return(ovc_map)
  }


#' @title Plot OVC Proxy Coverage
#'
#' @param df_ovc OVC Datasets
#' @param country country name
#' @param agency agency specific?
#' @return ggplot plot
#'
plot_ovc_coverage <-
  function(df_ovc,
           country = NULL,
           agency = FALSE) {

    # Params
    cntry <- {{country}}
    agency <- {{agency}}

    # reformat data
    if (agency == TRUE) {
      df_ovc <- df_ovc %>%
        dplyr::mutate(proxy_coverage = proxy_coverage_usaid,
                      proxy_coverage_max = proxy_coverage_usaid_max,
                      OVC_HIVSTAT_POS = OVC_HIVSTAT_POS_USAID) %>%
        filter(proxy_coverage > 0, is.na(flag))
    }

    # Generate labels
    df <- df_ovc %>%
      filter(!is.na(proxy_coverage)) %>%
      mutate(
        label = paste0(shortname, " (", comma(OVC_HIVSTAT_POS, 1), "/", comma(TX_CURR, 1), ")")
      )

    # Filter by country
    if (!is.null(cntry)) {
      df <- df %>%
        filter(operatingunit %in% {{cntry}})
    }

    # Identify lowest record
    low <- df %>%
      filter(proxy_coverage == min(proxy_coverage))

    # Plot
    dotplot <- df %>%
      ggplot(aes(x = reorder(label, proxy_coverage_max), y = proxy_coverage_max)) +
      geom_point(aes(size = TX_CURR, fill = proxy_coverage_max),
                 color = grey50k,
                 shape = 21,
                 show.legend = F) +
      scale_size_continuous(range = c(3,8)) +
      scale_color_viridis_c(option = "magma",
                            direction = -1,
                            aesthetics = c("fill"),
                            limits = c(0, 1)) +
      geom_hline(aes(yintercept = .9),
                 color = "gray70",
                 size = .7,
                 linetype = "dashed",
                 alpha = .8) +
      scale_y_continuous(position = "right",
                         labels = percent,
                         limits = c(0, 1), #
                         breaks = c(0, .25, .5, .75, 1)) +
      labs(x = "",y = "") +
      coord_flip() +
      annotate(
        geom = "curve",
        x = low$label, y = (low$proxy_coverage_max + .05),
        xend = low$label, yend = (low$proxy_coverage_max + .01),
        curvature = -.3, arrow = arrow(length = unit(4, "mm")),
        color = grey50k
      ) +
      annotate(geom = "text",
               x = low$label, y = (low$proxy_coverage_max + .06),
               label = "Circle Sized by TX_CURR", hjust = "left",
               size = 4, color = grey50k, family = "Gill Sans MT") +
      annotate(geom = "text",
               x = low$label, y = .905,
               label = "90%\nthreshold", hjust = "left",
               size = 4, color = grey50k, family = "Gill Sans MT") +
      si_style_xgrid()

    return(dotplot)
  }


#' Viz OVC Proxy Coverage
#'
#' @param spdf PEPFAR Spatial Data
#' @param df_ovc OVC Coverage data
#' @param terr_raster RasterLayer
#' @param cntry OU Name
#'
viz_ovc_coverage <-
  function(spdf, df_ovc, terr_raster, rep_pd,
           country = NULL,
           age = "<20",
           caption = "",
           filename = "",
           save = FALSE) {

    # Params
    df_geo <- {{spdf}}
    df <- {{df_ovc}}
    terr <- {{terr_raster}}
    pd <- {{rep_pd}}
    cntry <- {{country}}
    age_group <- paste0(str_replace({{age}}, "<", "Under "), "yo")
    caption <- {{caption}}
    fname <- {{filename}}
    sgraph <- {{save}}

    # flag agency data
    agency <- "proxy_coverage_usaid" %in% names(df)

    # Monitor progress
    print(cntry)

    if (!is.null(cntry)) {
      df <- df %>%
        filter(operatingunit == cntry)
    }

    # Filter data by country / OU
    if (agency == FALSE) {
      df <- df %>%
        filter(proxy_coverage > 0)
    }
    else {
      df <- df %>%
        filter(proxy_coverage_usaid > 0)
    }

    # Map
    map <- map_ovc_coverage(df_geo, df, terr, agency = agency)

    # graph
    viz <- plot_ovc_coverage(df, agency = agency)

    # VIZ COMBINED & SAVED
    graph <- (map + viz) +
      plot_layout(nrow = 1) +
      plot_annotation(
        title = paste0(pd, " | OVC Program Coverage Proxy of TX_CURR (",
                       age_group, ")"),
        subtitle = "The size of circles represents the volume of TX_CURR and the color represents the % coverage",
        caption = caption
      ) +
      theme(text = element_text(family = "Gill Sans MT"))

    # Save plot
    if (sgraph == TRUE & endsWith(tolower(fname), ".png")) {

      print(graph)

      ggsave(here("Graphics", fname),
             plot = last_plot(), scale = 1.2, dpi = 310,
             width = 10, height = 7, units = "in")

      # Save individual plot for larger dataset
      if (nrow(df) > 30) {

        # Map only
        graph_map <- map +
          plot_layout(nrow = 1) +
          plot_annotation(
            title = paste0(pd, " | OVC Program Coverage Proxy of TX_CURR (",
                           age_group, ")"),
            #subtitle = "The size of circles represents the volume of TX_CURR and the color represents the % coverage",
            caption = caption
          ) +
          theme(text = element_text(family = "Gill Sans MT"))

        print(graph_map)

        ggsave(here("Graphics",
                    str_replace(fname, "Proxy_Coverage", "Proxy_Coverage_Map")),
               plot = last_plot(), scale = 1.2, dpi = 310,
               width = 10, height = 7, units = "in")

        # dotplot
        graph_dotplot <- viz +
          plot_layout(nrow = 1) +
          plot_annotation(
            title = paste0(pd, " | OVC Program Coverage Proxy of TX_CURR (",
                           age_group, ")"),
            subtitle = "The size of circles represents the volume of TX_CURR and the color represents the % coverage",
            caption = caption
          ) +
          theme(text = element_text(family = "Gill Sans MT"))

        print(graph_dotplot)

        ggsave(here("Graphics",
                    str_replace(fname, "Proxy_Coverage", "Proxy_Coverage_DotPlot")),
               plot = last_plot(), scale = 1.2, dpi = 310,
               width = 10, height = 7, units = "in")
      }
    }


    return(graph)
  }



#' Plot OVC & TX Heatmap
#'
#' @param df OVC vs TX DataFrame
#' @return ggplot plot
#'
heatmap_ovctx_coverage <-
  function(df) {

    # Heatmap
    df <- df %>%
      filter(ovc_group == "Mixed")

    print(df %>% distinct(operatingunit) %>% pull())

    if (nrow(df) == 0) return(NULL)

    heatmap <- df %>%
      mutate(fundingagency = factor(fundingagency, levels = c("USAID", "CDC")),
             targets = as.integer(targets)) %>%
      ggplot(aes(x = fundingagency,
                 y = reorder(psnu, targets),
                 fill = fundingagency)) +
      geom_tile(color = grey20k, alpha = .6) +
      geom_text(aes(label = comma(targets, accuracy = 1))) +
      scale_fill_manual(values = c(USAID_mgrey, USAID_lgrey),
                        na.translate = TRUE, na.value = "red") +
      xlab(label = "") +
      ylab(label = "") +
      facet_wrap(~indicator) +
      #labs(subtitle = "Summary of PSNUs with Mixed Agency Targets") +
      labs(subtitle = "Summary by PSNUs & Agency") +
      si_style_xline() +
      theme(legend.position = "none",
            plot.title = element_text(family = "Gill Sans MT"),
            axis.text = element_text(family = "Gill Sans MT"),
            strip.text = element_text(family = "Gill Sans MT", hjust = .5),
            panel.spacing.x = unit(-1, "lines"))

    return(heatmap)
  }



#' Map OVC & TX Coverage
#'
#' @param spdf
#' @param df_ovctx
#' @param terr_raster
#' @return
#'
map_ovctx_coverage <-
  function(spdf, df_ovctx, terr_raster) {

  country <- df_ovctx %>%
    distinct(operatingunit) %>%
    pull()

  # Geodata
  spdf_adm0 <- spdf %>%
    filter(operatingunit %in% country, type == "OU")

  # Append Program data to geo
  spdf_ovctx <- spdf %>%
    #filter(operatingunit %in% country, type == "PSNU") %>%
    left_join(df_ovctx, by = c("uid" = "psnuuid")) %>%
    filter(!is.na(ovc_group)) %>%
    mutate(ovc_colour = case_when(
      ovc_group == "USAID Only" ~ USAID_blue,
      ovc_group == "CDC Only" ~ USAID_ltblue,
      ovc_group == "Mixed" ~ USAID_dkred,
      ovc_group == "OVC & TX do not overlap in this district" ~ USAID_mgrey,
      TRUE ~ USAID_mgrey
    ))

  # ovc_labels <- spdf_ovctx %>% distinct(ovc_group) %>% pull()
  # ovc_colors <- spdf_ovctx %>% distinct(ovc_colour) %>% pull()

  ovc_items <- c("USAID Only" = USAID_blue,
                 "CDC Only"   = USAID_ltblue,
                 "Mixed"      = USAID_dkred,
                 "OVC & TX do not overlap in this district" = USAID_mgrey)

  # Get basemap
  basemap <- get_basemap(spdf = spdf,
                         cntry = country,
                         terr_raster = terr_raster,
                         add_admins = TRUE)

  map <- basemap +
    #geom_sf(data = spdf_adm0, colour = grey90k, fill = USAID_mgrey, size = .2) +
    geom_sf(data = spdf_ovctx, aes(fill = ovc_group),
            lwd = .2, color = grey10k) +
    geom_sf(data = spdf_adm0, colour = grey90k, fill = NA, size = 1) +
    # scale_fill_manual(
    #   #values = c("#384F6C","#BA3D56", USAID_dkred, USAID_mgrey, USAID_mgrey),
    #   values = c(USAID_blue, USAID_ltblue, USAID_dkred, USAID_mgrey, USAID_mgrey),
    #   labels = c("USAID Only", "CDC Only", "Mixed",
    #              "OVC & TX do not overlap in this district", "NA")
    # ) +
    scale_fill_manual(values = ovc_items) +
    # scale_fill_identity(labels = ovc_labels, guide = "legend") +
    #labs(label = "Agency with OVC_SERV_UNDER_18 \n & TX_CURR <20 Targets in FY21") +
    labs(subtitle = "Targets by Agency") +
    si_style_map() +
    theme(
      legend.direction = "vertical",
      legend.position =  "bottom",
      legend.key.width = ggplot2::unit(1, "cm"),
      legend.key.height = ggplot2::unit(.5, "cm"),
      legend.text.align = 0,
      plot.title = element_text(family = "Gill Sans MT")
    )

  return(map)
}


#' Map Mixed Coverage
#'
#' @param spdf
#' @param df_ovctx
#' @param terr_raster
#' @return
#'
map_mixed_coverage <-
  function(spdf, df_ovctx, terr_raster) {

    country <- df_ovctx %>%
      distinct(operatingunit) %>%
      pull()

    # Geodata
    spdf_adm0 <- spdf %>%
      filter(operatingunit %in% country, type == "OU")

    # Append Program data to geo
    spdf_ovctx <- spdf %>%
      #filter(operatingunit %in% country, type == "PSNU") %>%
      left_join(df_ovctx, by = c("uid" = "psnuuid")) %>%
      filter(ovc_group == "Mixed")

    # Get basemap
    basemap <- get_basemap(spdf = spdf,
                           cntry = country,
                           terr_raster = terr_raster,
                           add_admins = TRUE)

    mixed_map <- basemap +
      geom_sf(data = spdf_ovctx, aes(fill = ovc_group),
              lwd = .2, fill = USAID_dkred,
              color = grey10k, show.legend = F) +
      #scale_fill_manual(values = c(USAID_dkred)) +
      geom_sf(data = spdf_adm0, colour = grey90k, fill = NA, size = 1) +
      #labs(subtitle = "Mixed Agency OVC_SERV_UNDER_18 \n & TX_CURR <20 Targets in FY21") +
      labs(subtitle = "Mixed Targets") +
      si_style_map() +
      theme(
        plot.title = element_text(family = "Gill Sans MT")
      )

    return(mixed_map)
  }

#' Viz OVC & TX Coverage
#'
#' @param spdf
#' @param df_ovctx
#' @param terr_raster
#' @param caption
#' @return
#'
viz_ovctx_coverage <-
  function(spdf, df_ovctx, terr_raster,
           caption = "",
           save = FALSE,
           filename = "") {

    # Heat
    heatmap <- heatmap_ovctx_coverage(df = df_ovctx)

    # Overlap
    ovctx <- map_ovctx_coverage(spdf = spdf_pepfar,
                                df_ovctx = df_ovctx,
                                terr_raster = terr)

    # Mixed
    mixed <- map_mixed_coverage(spdf = spdf_pepfar,
                                df_ovctx = df_ovctx,
                                terr_raster = terr)

    viz_title <- "FY21 Targets - PSNUs with Mixed Agency OVC_SERV_UNDER_18 & TX_CURR <20"

    # Combine graphs
    viz <- (ovctx + mixed + heatmap) +
      #plot_layout(nrow = 1) +
      plot_annotation(
        title = viz_title,
        caption = caption
      )

    # Save plot
    if (save == TRUE & endsWith(tolower(filename), ".png")) {

      print(viz)

      ggsave(here("./Graphics", filename),
             plot = last_plot(), scale = 1.2, dpi = 310,
             width = 10, height = 7, units = "in")

      if (nrow(df_ovctx) > 30) {

        # Maps only
        (ovctx + mixed) +
          plot_annotation(
            title = viz_title,
            caption = caption
          )

        # Save maps
        ggsave(here("./Graphics",
                    str_replace(filename,
                                "OVC_TX_Overlap",
                                "OVC_TX_Overlap_Maps")),
               plot = last_plot(), scale = 1.2, dpi = 310,
               width = 10, height = 7, units = "in")

        # Heatmap only
        heatmap +
          plot_annotation(
            title = viz_title,
            caption = caption
          )

        # Save heatmap
        ggsave(here("./Graphics",
                    str_replace(filename,
                                "OVC_TX_Overlap",
                                "OVC_TX_Overlap_Heatmap")),
               plot = last_plot(), scale = 1.2, dpi = 310,
               width = 7, height = 10, units = "in")

      }
    }

    return(viz)
  }
