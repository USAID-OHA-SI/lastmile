##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa, G.Sarfaty & T.Essam | USAID
##  EDITED:  T. Essam (2020-10-01)
##  PURPOSE: OVC Data Utilities
##  LICENCE: MIT
##  DATE:    2020-11-06


#' @title Extract OVC Proxy Coverage data
#'
#' @param df_msd Dataframe of MSD PSNU IM
#' @param rep_fy Reporting Fiscal Year
#' @param rep_agency Reporting / Funding Agency
#' @param rep_pd Reporting Period (eg: FY20Q2)
#' @return df
#'
extract_ovc_coverage <-
  function(df_msd,
           rep_fy = 2020,
           rep_agency = "USAID",
           rep_pd = "FY20Q2") {

    # All Params
    fy <- {{rep_fy}}
    agency <- {{rep_agency}}
    pd <- {{rep_pd}}

    # Filter and Calculate Proxy Coverage
    df_proxy_ovc_cov <- df_msd %>%
      filter(fiscal_year == fy,
             fundingagency %in% agency,
             indicator == "OVC_HIVSTAT_POS" &
               standardizeddisaggregate == "Total Numerator" |
               indicator == "TX_CURR" &
               standardizeddisaggregate == "Age/Sex/HIVStatus") %>%
      filter(!trendsfine %in% c("20-24","25-29","30-34","35-39","40-49","50+")) %>%
      group_by(fiscal_year,operatingunit,psnuuid,psnu,indicator) %>%
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
      ungroup() %>%
      reshape_msd(clean = TRUE) %>%
      dplyr::select(-period_type) %>%
      spread(indicator, val) %>%
      mutate(
        proxy_coverage = case_when(
          OVC_HIVSTAT_POS > 0 ~ OVC_HIVSTAT_POS/TX_CURR
        ),
        proxy_coverage_max = case_when(
          proxy_coverage > 1 ~ 1.01,
          TRUE ~ proxy_coverage
        ),
        shortname = str_remove(psnu," District$| County$"),
        shortname = str_remove(shortname, "District Municipality$"),
        shortname = str_remove(shortname, "Metropolitan Municipality$"),
        shortname = str_remove(shortname, "Municipality$")
      ) %>%
      filter(period == pd, !is.na(proxy_coverage))

    if (nrow(df_proxy_ovc_cov) == 0) {
      cat(
        "\n",
        Wavelength::paint_red(
          paste0("No OVC Proxy Coverage data available for ", pd)), "\n")
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
    df <- df_psnu %>%
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
      #summarise(across(starts_with("targ"), sum, na.rm = TRUE)) %>%
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
          CDC < 2  & is.na(USAID) ~ "OVC & TX do not overlap in this district",
          USAID < 2  & is.na(CDC) ~ "OVC & TX do not overlap in this district"
        ),
        ovc_group = factor(
          ovc_group,
          levels = c("USAID Only", "CDC Only", "Mixed",
                     "OVC & TX do not overlap in this district"))
      )

    glimpse(df)

    return(df)
  }


#' @title Map OVC Proxy Coverage Data
#'
#' @param spdf Spatial Dataframe of MSD PSNU IM
#' @param terr Terrain Raster
#' @param df_ovc Proxy OVC Coverage data
#' @param country Operatingunit(s)
#' @param orglevel Org Hierarchy Level
#' @return ggplot plot
#'
map_ovc_coverage <-
  function(spdf, df_ovc, terr_raster,
           orglevel = "PSNU",
           facet = FALSE) {

    # Params
    orglevel <- {{orglevel}}
    rep_pd <- {{rep_pd}}

    # Identify Country/OU
    country <- df_ovc %>%
      dplyr::distinct(operatingunit) %>%
      pull()

    # Geodata
    spdf_adm0 <- spdf %>%
      filter(operatingunit %in% country, type == "OU")

    # spdf_adm1 <- spdf %>%
    #   filter(operatingunit %in% country, type == "SNU1")

    # Append Program data to geo
    spdf_ovc <- spdf %>%
      filter(operatingunit %in% country, type == orglevel) %>%
      left_join(df_ovc, by = c("uid" = "psnuuid")) %>%
      filter(proxy_coverage > 0)

    # Get basemap
    basemap <- get_basemap(spdf = spdf,
                           cntry = country,
                           terr_raster = terr)

    # Overlay program data
    ovc_map <- basemap +
      geom_sf(
        data = spdf_ovc,
        aes(fill = proxy_coverage),
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
        labels = percent
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
#' @return ggplot plot
#'
plot_ovc_coverage <-
  function(df_ovc,
           country = NULL) {

    # Params
    cntry <- {{country}}

    # Generate labels
    df <- df_ovc %>%
    mutate(
      label = paste0(shortname, " (", OVC_HIVSTAT_POS, "/", TX_CURR, ")")
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
      ggplot(aes(x = reorder(label, proxy_coverage), y = proxy_coverage_max)) +
      geom_point(aes(size = TX_CURR, fill = proxy_coverage),
                 color = grey50k,
                 shape = 21,
                 show.legend = F) +
      scale_size_continuous(range = c(3,8)) +
      scale_color_viridis_c(option = "magma",
                            direction = -1,
                            aesthetics = c("fill")) +
      geom_hline(aes(yintercept = .9),
                 color = "gray70",
                 size = .7,
                 linetype = "dashed",
                 alpha = .8) +
      scale_y_continuous(position = "right",
                         labels = percent,
                         #limits = c(0, 1), #
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
               x = low$label, y = .89,
               label = "90% threshold", hjust = "right",
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
           caption = "",
           filename = "",
           save = FALSE) {

  # Params
  df_geo <- {{spdf}}
  df <- {{df_ovc}}
  terr <- {{terr_raster}}
  pd <- {{rep_pd}}
  cntry <- {{country}}
  caption <- {{caption}}
  fname <- {{filename}}
  sgraph <- {{save}}

  # Filter data by country / OU
  if (!is.null(cntry)) {
    df <- df %>%
      filter(operatingunit == cntry)
  }

  # Map
  map <- map_ovc_coverage(df_geo, df, terr)

  # graph
  viz <- plot_ovc_coverage(df)

  # VIZ COMBINED & SAVED
  graph <- (map + viz) +
    plot_layout(nrow = 1) +
    plot_annotation(
      title = paste0(pd, " | OVC Program Coverage Proxy of TX_CURR (Under 20yo)"),
      subtitle = "The size of circles represents the volume of TX_CURR and the color represents the % coverage",
      caption = caption
    ) +
    theme(text = element_text(family = "Gill Sans MT"))

  # Save plot
  if (sgraph == TRUE & endsWith(tolower(fname), ".png")) {
    print(graph)

    ggsave(here("Graphics", fname),
           scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
  }



  return(graph)
  }


#' Plot OVC & TX Heatmap
#'
heatmap_ovc_tx <-
  function(df) {

    # Heatmap
    heatmap <- df %>%
      filter(ovc_group == "Mixed") %>%
      mutate(
        fundingagency = factor(fundingagency, levels = c("USAID", "CDC"))
      ) %>%
      ggplot(aes(x = fundingagency,
                 y = reorder(psnu, targets),
                 fill = fundingagency)) +
      geom_tile(color = grey20k, alpha = .6) +
      geom_text(aes(label = comma(targets))) +
      scale_fill_manual(values = c(USAID_mgrey, USAID_lgrey),
                        na.translate = TRUE, na.value = "red") +
      xlab(label = "") +
      ylab(label = "") +
      facet_wrap(~indicator) +
      ggtitle(label = "Summary of PSNUs with Mixed Agency Targets") +
      si_style_xline() +
      theme(legend.position = "none",
            plot.title = element_text(family = "Gill Sans MT", size = 10),
            axis.text = element_text(family = "Gill Sans MT"),
            strip.text = element_text(family = "Gill Sans MT", hjust = .5),
            panel.spacing.x = unit(-1, "lines"))

    return(heatmap)
  }
