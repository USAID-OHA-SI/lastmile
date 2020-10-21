##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa, G.Sarfaty & T.Essam | USAID
##  EDITED:  T. Essam (2020-10-01)
##  PURPOSE: VL-Utilities
##  LICENCE: MIT
##  DATE:    2020-10-21


#' VL Datasets
#'
#' @param df_msd
#' @param lst_ou
#' @return VL Datasets
#'
extract_viralload <-
  function(df_msd,
           lst_ou = NULL) {

  }


#' VL PEDS Datasets
#'
#' @param df_msd
#' @param lst_ou
#' @return VL Datasets
#'
extract_peds_viralload <-
  function(df_msd,
           lst_ou = NULL) {

  }


#' EID PEDS Datasets
#'
#' @param df_msd
#' @param lst_ou
#' @return VL Datasets
#'
extract_eid_viralload <-
  function(df_msd,
           lst_ou = NULL) {

  }


#' Treatment ML Datasets
#'
#' @param df_msd
#' @param lst_ou
#' @return VL Datasets
#'
extract_tx_ml <-
  function(df_msd,
           lst_ou = NULL) {

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
      filter(countryname == country, type == "PSNU") %>%
      left_join(df_vl, by = c("uid" = "psnuuid")) %>%
      filter(!is.na(VLnC))

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


    print(theme_map)


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

    # VLS
    m_vls <- map_viralload(
      spdf = df_geo,
      df = df_vl,
      vl_variable = "VLS",
      cntry = country,
      terr_raster = terr,
      caption = FALSE,
      agency = TRUE,
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
      agency = TRUE,
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
      agency = TRUE,
      facet_rows = facets
    )

    # ALL
    m_all <- (m_vlc + m_vlnc + m_vls) +
      plot_layout(widths = c(1, 1, 1)) +
      plot_annotation(caption = get_caption(country))

    print(m_all)

    # Save output
    if (save == TRUE) {
      ggsave(
        here("Graphics", get_output_name(country, var = "VL")),
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

    # VLS
    m_vls <- map_viralload(
      spdf = df_geo,
      df = df_vl,
      vl_variable = "VLS",
      cntry = country,
      terr_raster = terr,
      peds = TRUE,
      caption = FALSE,
      agency = TRUE,
      facet_rows = facets
    )

    # VLC
    m_vlc <- map_viralload(
      spdf = spdf_pepfar,
      df = df_vl,
      vl_variable = "VLC",
      cntry = country,
      terr_raster = terr,
      peds = TRUE,
      caption = FALSE,
      agency = TRUE,
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
            get_output_name(country, var = "VLC_S"),
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
    return(theme_map)


    # Save output
    if (save == TRUE) {
      ggsave(
        here("Graphics",
             paste0(rep_pd, "_", varname, "_", cntry, "_", Sys.Date(), ".png")),
        plot = last_plot(),
        scale = 1.2,
        dpi = 400,
        width = 10,
        height = 7,
        units = "in")
    }
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
      spdf = spdf_pepfar,
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
      spdf = spdf_pepfar,
      df_gen = df_oth,
      mapvar = {{mapvar}},
      cntry = country,
      terr_raster = terr,
      agency = T,
      facet_rows = facets,
      gen_title = "Early Infant Diagnosis Under Two"
    )

    m_all <- (vlc + eid) +
      plot_layout(widths = c(1, 1)) +
      plot_annotation(
        caption = paste0(
          "OHA/SIEI - Data Source: MSD ",
          rep_pd,
          "_",
          cntry,
          "_VLC + EID \n",
          "Produced on ",
          Sys.Date(),
          ", Missing data shown in gray."
        ),
        title = paste0(str_to_upper(country),
                       " VLC & EID PROXY COVERAGE SUMMARY")
      )


    print(m_all)

    if (save_all == TRUE) {
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


# Create a bar graph for TX_ML_PLP
tx_graph <-
  function(spdf,
           df_gph,
           mapvar,
           cntry) {

    df_geo <- {{spdf}}
    df <- {{df_gph}}
    country <- {{cntry}}

    df_geo2 <- df_geo %>%
      filter(countryname == country, type == "PSNU") %>%
      left_join(df, by = c("uid" = "psnuuid")) %>%
      filter(!is.na({{mapvar}})) %>%
      mutate(
        psnu_short = case_when(
          str_detect(psnu.y, " County$") ~  str_replace_all(psnu.y,
                                                            " County$",
                                                            ""),
          str_detect(psnu.y, " District$") ~  str_replace_all(psnu.y,
                                                              " District$",
                                                              ""),
          TRUE ~ psnu.y
        )
      )

    p <-  df_geo2 %>%
      mutate(
        yorder = tidytext::reorder_within(psnu_short, {{mapvar}}, fundingagency),
        rank = percent_rank({{mapvar}})
      ) %>%
      ggplot(aes(y = {{mapvar}}, x = yorder)) +
      geom_col(aes(fill = {{mapvar}})) +
      facet_wrap(~ paste0(fundingagency, "\n"), nrow = 2,scales = "free_y") +
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
