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
        fiscal_year %in% c(fy - 1, fy), # Needed for Q1 vls
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
      spread(indicator, value)

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
      ) %>%
      filter(str_detect(period, paste0("^FY", str_sub(fy, 3, 4))))

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
      spread(indicator, value)

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
      spread(indicator, value) %>%
      mutate(eid_cov_under2 = (PMTCT_EID_Less_Equal_Two_Months / PMTCT_EID_D)) %>%
      filter(period == pd)

    return(df_eid)
  }


#' @title VLS & TLD Datasets
#'
#' @param df_msd      MSD PNSU x IM
#' @param rep_agency  Funding Agency(ies)
#' @param rep_fy      Fiscal Year(s)
#' @param rep_pd      Full reporting period (eg: FY20Q2)
#' @return VL Datasets
#'
extract_vls_tld <-
  function(df_msd,
           rep_agency = c("USAID","HHS/CDC"),
           rep_fy = c("2020","2021"),
           rep_pd = "FY21Q2",
           peds = FALSE,
           lst_ous = NULL) {

    # Variables
    df <- {{df_msd}}
    agencies <- {{rep_agency}}
    fy <- {{rep_fy}}

    # Calculate VLS & TLD MOT
    df_vls_tld <- df %>%
      filter(
        fundingagency %in% agencies,
        fiscal_year %in% fy,
        indicator %in% c("TX_PVLS","SC_ARVDISP","TX_CURR"),
        standardizeddisaggregate %in% c("DispensedARVBottles",
                                        "Age/Sex/HIVStatus",
                                        "Age/Sex/Indication/HIVStatus")
      ) %>%
      mutate(
        indicator = if_else(numeratordenom == "D",
                            paste0(indicator, "_D"),
                            indicator)
      ) %>%
      group_by(fiscal_year, operatingunit, fundingagency,
               psnuuid, psnu, indicator, otherdisaggregate) %>%
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(
        indicator = case_when(
          indicator == "SC_ARVDISP" ~ otherdisaggregate,
          TRUE ~ indicator)) %>%
      reshape_msd(clean = TRUE) %>%
      dplyr::select(-period_type) %>%
      mutate(
        value = case_when(
          str_detect(indicator,"180-count") ~ value*6,
          str_detect(indicator, "90-count") ~ value*3,
          TRUE ~ value
        ),
        indicator = case_when(
          indicator %in% c("TX_PVLS_D", "TX_PVLS", "TX_CURR") ~ indicator,
          str_detect(indicator, "TLD") ~ "TLD",
          TRUE ~ "other"
        )) %>%
      spread(indicator, value) %>%
      group_by(period, fundingagency, operatingunit, psnuuid, psnu) %>%
      summarise_at(vars(other:TX_PVLS_D), sum, na.rm = TRUE) %>%
      ungroup() %>%
      group_by(period, fundingagency, operatingunit, psnuuid, psnu) %>%
      mutate(
        VLC = TX_PVLS_D / lag(TX_CURR, 2, order_by = period),
        VLS_timesVLC = (TX_PVLS / TX_PVLS_D) * VLC,
        VLS = (TX_PVLS / TX_PVLS_D),
        #TLD_MOT = ifelse(other > 0, (TLD / (TLD + other)), NA),
        TLD_MOT = case_when(
         other > 0 ~ (TLD / (TLD + other)),
         TRUE ~ NA_real_),
        VLS_cat = case_when(
         VLS <.8 ~ "Less than 80%",
         VLS >=.8 & VLS <.9 ~ "80-89%",
         VLS >= .9 ~ "Greater than 90%"),
        VLS_TLD_ratio = case_when(
         TLD_MOT > 0 ~ (VLS / TLD_MOT),
         TRUE ~ NA_real_
        )) %>%
      ungroup() %>%
      filter(period == rep_pd)

    return(df_vls_tld)
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
    pd <- df_vl %>% pull(period) %>% first()
    country <- {{cntry}}
    vl_var <- {{vl_variable}}
    terr <- {{terr_raster}}
    peds_title <- {{peds}}

    # if no data exit
    if (nrow(df_vl) == 0) {
      return(NULL)
    }

    # Si breaks and colors
    breaks <- c(0, .8, .9, 1)
    labels <- c("0 - 80%", "80 - 90%", "90 - 100%", "100%+")
    colors <- c(old_rose_light, burnt_sienna_light, scooter, trolley_grey_light)

    # Check the number of agencies
    agencies <- df_vl %>%
      distinct(fundingagency) %>%
      pull() %>%
      length()

    # Country boundaries
    df_geo0 <- df_geo %>%
      filter(operatingunit == country, label == "country")

    df_geo1 <- df_geo %>%
      filter(operatingunit == country, label == "snu1")

    # PSNU Geo + VL data
    df_geo2 <- df_geo %>%
      filter(countryname == country) %>%
      left_join(df_vl, by = c("uid" = "psnuuid")) %>%
      dplyr::filter(!is.na(VLnC))

    # Basemap
    base_map <- terrain_map(countries = lookup_country(cntry),
                            adm0 = df_geo0,
                            adm1 = df_geo1,
                            mask = TRUE,
                            terr = terr)

    # Map specific variable
    if (tolower(vl_var) == "vls") {

      df_geo2 <- df_geo2 %>%
        mutate(
          VLS_color = case_when(
            VLS <= 0.8 ~ old_rose_light,
            VLS > 0.8 & VLS <= 0.9 ~ burnt_sienna_light,
            VLS > 0.9 & VLS <= 1.0 ~ scooter,
            VLS > 1 ~ trolley_grey_light),
          VLS_color = factor(VLS_color, levels = colors)
          )

      # Map
      theme_map <- base_map +
        geom_sf(
          data = df_geo2,
          aes(fill = VLS_color),
          lwd = .2,
          color = grey10k,
          alpha = 0.8
        ) +
        scale_fill_identity(
          labels = labels,
          guide = guide_legend(label.position = "bottom")
        )

    }
    else if (tolower(vl_var) == "vlc") {
      theme_map <- base_map +
        geom_sf(
          data = df_geo2,
          aes(fill = VLC),
          lwd = .2,
          color = grey10k
        ) +
        scale_fill_si(
          palette = "genoas",
          discrete = FALSE,
          alpha = 0.9,
          na.value = grey40k,
          breaks = c(0, .25, .50, .75, 1.00),
          limits = c(0, 1),
          labels = percent
        )
    }
    else {
      theme_map <- base_map +
        geom_sf(
          data = df_geo2,
          aes(fill = VLnC),
          lwd = .2,
          color = grey10k
        ) +
        scale_fill_si(
          palette = "burnt_siennas",
          discrete = FALSE,
          alpha = 0.9,
          na.value = grey40k,
          breaks = c(0, .25, .50, .75, 1.00),
          limits = c(0, 1),
          labels = percent
        )
    }

    # Check
    if (agency == TRUE & agencies > 1) {
      theme_map <-
        theme_map +
        facet_wrap( ~ fundingagency, nrow = facet_rows)
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
        plot.title = element_text(family = "Source Sans Pro", size = 14),
        plot.subtitle = element_text(family = "Source Sans Pro", size = 12),
        plot.caption = element_text(family = "Source Sans Pro", size = 8),
        legend.position =  "bottom",
        legend.direction = "horizontal",
        legend.key.width = ggplot2::unit(1.5, "cm"),
        legend.key.height = ggplot2::unit(.5, "cm")
      )

    # Save map
    if (save == TRUE) {

      #print(theme_map)

      si_save(
        here::here("Graphics", get_output_name(country, rep_pd = pd, var = vl_var)),
        plot = theme_map
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
    pd <- df_vl %>% pull(period) %>% first()
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
      print("No data ...")
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
      print(m_all)

      si_save(
        here("Graphics",
             get_output_name(country, rep_pd = pd, var = "VL", agency = agency)),
        plot = m_all)
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

    #print(m_all)

    # Save output
    if (save == TRUE) {
      ggsave(
        here(
          "Graphics",
          str_replace(
            get_output_name(country, var = "VLC_S", agency = agency),
            "_ViralLoad_",
            "_ViralLoad_PEDS_")),
        plot = m_all,
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
      filter(operatingunit == country, label == "country")

    df_geo1 <- df_geo %>%
      filter(operatingunit == country, label == "snu1")

    # PSNU Geo + VL data
    df_geo2 <- df_geo %>%
      filter(countryname == country) %>%
      left_join(df_oth, by = c("uid" = "psnuuid")) %>%
      filter(!is.na({{mapvar}}))

    # Basemap
    base_map <- terrain_map(countries = lookup_country(country),
                            adm0 = df_geo0,
                            adm1 = df_geo1,
                            mask = TRUE,
                            terr = terr)

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

    theme_map <- theme_map  +
      geom_sf(data = df_geo0, colour = grey90k, fill = "NA", size = 1)

    if (agency == TRUE) {
      theme_map <- theme_map +
        facet_wrap( ~ fundingagency, nrow = facet_rows)

    } else {
      theme_map <- theme_map
    }

    # Update legend size and position
    theme_map <- theme_map +
      labs(title = gen_title) +
      theme(
        plot.title = element_text(family = "Source Sans Pro", size = 14, hjust = .5),
        plot.subtitle = element_text(family = "Source Sans Pro", size = 12),
        plot.caption = element_text(family = "Source Sans Pro", size = 8),
        legend.title = element_blank(),
        legend.position =  "bottom",
        legend.direction = "horizontal",
        legend.key.width = ggplot2::unit(1.5, "cm"),
        legend.key.height = ggplot2::unit(.5, "cm")
      )


    #print(theme_map)

    # Save output
    if (save == TRUE) {
      ggsave(
        here("./Graphics",
             paste0(rep_pd, "_",
                    toupper(varname), "_",
                    toupper(cntry), "_",
                    Sys.Date(),
                    ".png")),
        plot = theme_map,
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

    # Notification
    print(country)

    # Check valid data
    n_vl <- df_vl %>%
      filter(operatingunit == country,
             is.na(VLC)) %>%
      nrow()

    n_eid <- df_oth %>%
      filter(operatingunit == country,
             !is.na(eid_cov_under2)) %>%
      nrow()

    if (n_vl == 0 | n_eid == 0) {
      return(NULL)
    }

    # Map
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
                       " VLC & EID PROXY COVERAGE SUMMARY"),
        theme = theme(plot.title = element_text(hjust = .5))
      )

    # Print and save
    if (save_all == TRUE) {
      #print(m_all)

      ggsave(
        here(
          "Graphics",
          get_output_name(cntry, var = "VL_EID_Coverage")
        ),
        plot = m_all,
        scale = 1.2,
        dpi = 400,
        width = 10,
        height = 7,
        units = "in"
      )
    }
  }



#' @title VLS/TLD Maps + Scatter plot
#'
#' @param df_vl       DataFrame containing PSNUUID, VLS &TLD_MOT
#' @param spdf        Spatial DataFrame
#' @param terr        Raster dataset
#' @param country     Operationgunit
#' @param rep_agency  Plot only 1 agency (default is NULL for both agencies)
#' @param caption     Plot caption
#' @param save        Export plot
#'
viz_vls_tld <-
  function(df_vl, spdf, terr,
           country = NULL,
           rep_agency = NULL,
           caption = "",
           save = FALSE) {

    # params
    cntry <- {{country}}

    # filter country dataset
    if (!is.null(country)) {
      df_vl <- df_vl %>%
        filter(operatingunit == country)
    }

    # Get VLS / TLD Min
    vls_min <- df_vl %>%
      filter(!is.na(VLS)) %>%
      pull(VLS) %>%
      min()

    tld_min <- df_vl %>%
      filter(!is.na(TLD_MOT)) %>%
      pull(TLD_MOT) %>%
      min()

    lmin <- floor(min(vls_min, tld_min) * 10) / 10

    cat("\n", str_to_upper(country), " Min VLS/TLD = ", lmin, "\n")

    # Country boundaries
    spdf_adm0 <- spdf %>%
      filter(label == "country", operatingunit == country)

    # SNU1 boundaries
    spdf_adm1 <- spdf %>%
      filter(label == "snu1", operatingunit == country)

    # Country specific data joined to spatial data
    spdf_cntry <- spdf %>%
      filter(operatingunit == country) %>%
      left_join(df_vl, by = c("uid" = "psnuuid"))

    # Get basemap
    basemap <- terrain_map(countries = lookup_country(cntry),
                           adm0 = spdf_adm0,
                           adm1 = spdf_adm1,
                           mask = TRUE,
                           terr = terr)

    # VLS Map
    map_vls <- basemap

    # VLS Data
    spdf_vls = spdf_cntry %>% filter(!is.na(VLS))

    if (nrow(spdf_vls) > 0) {

      map_vls <- map_vls +
        geom_sf(data = spdf_vls,
                aes(fill = VLS), lwd = .2, color = grey10k) +
        geom_sf(data = spdf_adm0, fill = NA, lwd = .2, color = grey30k) +
        scale_fill_si(palette = "genoas",
                      discrete = FALSE,
                      alpha = 0.7,
                      breaks = rev(seq(1, lmin, -.25)),
                      limits = c(lmin, 1),
                      labels = percent) +
        facet_wrap(~fundingagency, nrow = 2) +
        ggtitle("Viral Load Suppression") +
        si_style_map() +
        theme(
          legend.position =  "bottom",
          legend.key.width = ggplot2::unit(1, "cm"),
          legend.key.height = ggplot2::unit(.5, "cm"),
          plot.title = element_text(size = 9,
                                    family = "Source Sans Pro",
                                    face = 1)
        )
    }

    # TLD Map
    map_tld_mot <- basemap

    # TLD Data
    spdf_tld <- spdf_cntry %>% filter(!is.na(TLD_MOT))

    if (nrow(spdf_tld) > 0) {

      map_tld_mot <- map_tld_mot +
        geom_sf(data = spdf_tld,
                aes(fill = TLD_MOT), lwd = .2, color = grey10k) +
        geom_sf(data = spdf_adm0, fill = NA, lwd = .2, color = grey30k) +
        scale_fill_si(palette = "burnt_siennas",
                      discrete = FALSE,
                      alpha = 0.7,
                      breaks = rev(seq(1, lmin, -.25)),
                      limits = c(lmin, 1),
                      labels = percent) +
        facet_wrap(~fundingagency, nrow = 2) +
        ggtitle("% TLD Months of TX (MOT) \n out of total ARVs Dispensed") +
        si_style_map() +
        theme(
          legend.position =  "bottom",
          legend.key.width = ggplot2::unit(1, "cm"),
          legend.key.height = ggplot2::unit(.5, "cm"),
          plot.title = element_text(size = 9,
                                    family = "Source Sans Pro",
                                    face = 1))
    }

    # Scatter plot
    scatter <- df_vl %>%
      ggplot(aes(x = TLD_MOT, y = VLS)) +
      geom_point(fill = scooter_light,
                 color = "white",
                 shape = 21,
                 size = 6,
                 alpha = 0.5,
                 show.legend = F) +
      scale_x_continuous(limits = c(lmin, 1), labels = percent) +
      #scale_y_continuous(limits = c(lmin, 1), labels = percent) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = grey70k) +
      facet_wrap(~fundingagency, nrow = 2) +
      labs(x = "% TLD MOT Dispensed", y = "VLS %") +
      ggtitle("VLS vs. % TLD Months of TX \n (MOT) Dispensed") +
      si_style_nolines() +
      theme(plot.title = element_text(size = 9,
                                      family = "Source Sans Pro",
                                      face = 1))

    # VIZ
    viz <- (map_vls + map_tld_mot + scatter) +
      plot_annotation(
        #title = paste0(str_to_upper(country), " | ", rep_pd),
        caption = paste0(str_to_upper(country),
                         caption,
                         format(Sys.Date(), "%Y%m%d")),
        theme = theme(plot.title = element_text(hjust = .5))
      )

    if (save == TRUE) {

      print(viz)

      ggsave(here("Graphics",
                  paste0(rep_pd,
                         "_VLS_TLD_TLE_Ratio_",
                         str_to_upper(country),
                         "_",
                         format(Sys.Date(), "%Y%m%d"),
                         ".png")),
             plot = last_plot(), scale = 1.2, dpi = 310,
             width = 10, height = 7, units = "in")
    }

    return(viz)
}
