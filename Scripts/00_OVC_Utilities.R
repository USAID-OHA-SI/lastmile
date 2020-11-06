##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa, G.Sarfaty & T.Essam | USAID
##  EDITED:  T. Essam (2020-10-01)
##  PURPOSE: OVC Data Utilities
##  LICENCE: MIT
##  DATE:    2020-11-06


#' @title Extract OVC Proxy Coverage data
#'
#' @param df_msd Dataframe of MSD PSNU IM
#' @param country Operatingunit name (Single OU or list)
#' @param rep_fy Reporting Fiscal Year
#' @param rep_agency Reporting / Funding Agency
#' @param rep_pd Reporting Period (eg: FY20Q2)
#' @return
#'
extract_ovc_coverage <-
  function(df_msd,
           country = "Zambia",
           rep_fy = 2020,
           rep_agency = "USAID",
           rep_pd = "FY20Q2") {

    # All Params
    cntry <- {{country}}
    fy <- {{rep_fy}}
    agency <- {{rep_agency}}
    pd <- {{rep_pd}}

    # Filter and Calculate Proxy Coverage
    df_proxy_ovc_cov <- df %>%
      filter(fiscal_year == fy,
             fundingagency %in% agency,
             indicator == "OVC_HIVSTAT_POS" &
               standardizeddisaggregate == "Total Numerator" |
               indicator == "TX_CURR" &
               standardizeddisaggregate == "Age/Sex/HIVStatus",
             operatingunit %in% cntry) %>%
      filter(!trendsfine %in% c("20-24","25-29","30-34","35-39","40-49","50+")) %>%
      group_by(fiscal_year,operatingunit,psnuuid,psnu,indicator) %>%
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
      ungroup() %>%
      reshape_msd(clean = TRUE) %>%
      select(-period_type) %>%
      spread(indicator, val) %>%
      mutate(proxy_coverage = case_when(
        OVC_HIVSTAT_POS > 0 ~ OVC_HIVSTAT_POS/TX_CURR
      ),
      shortname = str_remove(psnu, "District")) %>%
      filter(period == pd, !is.na(proxy_coverage))

    return(df_proxy_ovc_cov)
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
  function(spdf, terr, df_ovc,
           orglevel = "PSNU",
           rep_pd = "FY20Q2",
           facet = FALSE) {

    # Identify Country/OU
    country <- df_ovc %>%
      dplyr::distinct(operatingunit) %>%
      pull()

    # Geodata
    spdf_adm0 <- spdf %>%
      filter(operatingunit %in% country, type == "OU")

    spdf_adm1 <- spdf %>%
      filter(operatingunit %in% country, type == "SNU1")

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
      ) +
      ggtitle(
        label = "",
        subtitle = paste0(rep_pd,
                          " | OVC Program Coverage Proxy of TX_CURR <20")
      ) +
      si_style_map() +
      theme(
        legend.position = "bottom",
        legend.key.width = ggplot2::unit(1, "cm"),
        legend.key.height = ggplot2::unit(.5, "cm")
      )

    return(ovc_map)
  }


plot_ovc_coverage <-
  function(df_ovc) {

    dotplot <- df_ovc %>%
      mutate(
        label = paste0(shortname, " (", OVC_HIVSTAT_POS, "/", TX_CURR, ")")
      ) %>%
      ggplot(aes(x = reorder(label, proxy_coverage), y = proxy_coverage)) +
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
                 size = 0.35,
                 linetype = "dashed",
                 alpha = .8) +
      scale_y_continuous(position = "right", labels = percent) +
      labs(x = "",y = "") +
      coord_flip() +
      annotate(
        geom = "curve", x = 1.25, y = .65, xend = 1, yend = .33,
        curvature = .3, arrow = arrow(length = unit(4, "mm")),
        color = grey50k
      ) +
      annotate(geom = "text", x = 1, y = .68,
               label = "Circle Sized by TX_CURR", hjust = "left",
               size = 4, color = grey50k, family = "Gill Sans MT") +
      annotate(geom = "text", x = 13, y = .88,
               label = "90% threshold", hjust = "right",
               size = 4, color = grey50k, family = "Gill Sans MT") +
      si_style_nolines()

    print(dotplot)
  }
