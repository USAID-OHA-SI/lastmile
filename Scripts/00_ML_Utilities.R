##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa, G.Sarfaty & T.Essam | USAID
##  EDITED:  T. Essam (2020-10-01)
##  PURPOSE: ML-Utilities
##  LICENCE: MIT
##  DATE:    2020-11-02
##

#' @title TX_ML Dataset
#' @description Aggregate bad events then divide by TX_CURR_lagged_1
#' Numerator: Number of patients not retained on ART (LTFU, Died, Stopped)
#' Denominator: TX_CURR_lag1 + TX_NEW_curr + TX_RTT
#' @param df_msd     MER PSNU x IM dataset
#' @param rep_agency Funding Agency
#' @param rep_fy     Reporting Fiscal Year
#' @param rep_pd     PEPFAR Reporting Period (Qtr)
#' @param lst_ous    Targeted OU [List of UIDs]
extract_tx_ml <- function(df_msd,
                          rep_agency = "USAID",
                          rep_fy = 2020,
                          rep_pd = 3,
                          lst_ous = NULL) {

  # Variables
  df <- {{df_msd}}
  agencies <- as.vector({{rep_agency}})
  fy <- {{rep_fy}}

  pd <- {{rep_pd}}
  pd <- paste0("FY", str_sub(as.character(fy), 3, 4), "Q", pd)

  ous <- {{lst_ous}}

  df <- df %>%
    filter(
      fiscal_year == fy,
      fundingagency %in% agencies,
      indicator %in% c("TX_ML"),
      standardizeddisaggregate %in% c("Age/Sex/ARTNoContactReason/HIVStatus"),
      typemilitary == 'N'
    )

  if (!is.null(ous)) {
    df <- df %>%
      filter(operatingunituid %in% ous)
  }

  df <- df %>%
    mutate(
      fundingagency = if_else(
        fundingagency == "HHS/CDC",
        "CDC",
        fundingagency),
      otherdisaggregate = str_remove(
        otherdisaggregate,
        "No Contact Outcome - "),
      tx_badevents = case_when(
        otherdisaggregate %in% c(
          "Died",
          "Lost to Follow-Up <3 Months Treatment",
          "Lost to Follow-Up 3+ Months Treatment",
          "Refused Stopped Treatment"
        ) ~ "Bad events",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(tx_badevents) & indicator == "TX_ML") %>%
    group_by(operatingunit,
             snu1,
             snu1uid,
             psnu,
             psnuuid,
             indicator,
             tx_badevents,
             fundingagency) %>%
    summarize_at(vars(targets:cumulative), sum, na.rm = TRUE) %>%
    ungroup() %>%
    dplyr::select(-(targets:qtr4),
                  TX_ML_bad = cumulative,
                  -indicator,
                  -tx_badevents)

  return(df)
}


#' Treatment PLP (Patient Lost Proxy)
#'
#' @param df_msd     MER PSNU x IM dataset
#' @param rep_agency Funding Agency
#' @param rep_fy     Reporting Fiscal Year
#' @param rep_pd     PEPFAR Reporting Period (Qtr)
#' @param lst_ou     Targeted OU [List of UIDs]
#' @return PLP Datasets
#'
extract_tx_plp <-
  function(df_msd,
           rep_agency = "USAID",
           rep_fy = 2020,
           rep_pd = 3,
           lst_ous = NULL) {

    # Variables
    df <- {{df_msd}}
    agencies <- as.vector({{rep_agency}})
    fy <- {{rep_fy}}
    pd <- {{rep_pd}}
    ous <- {{lst_ous}}

    if (pd == 1) {
      cat(Wavelength::paint_red("\nThis may not be appropriate for QTR1\n"))
      stop("Error - Invalid Reporting Period")
    }

    # Filter options
    pds <- paste0("q", c((pd - 1), pd))
    pds <- paste0(pds, collapse = "|")

    curr_pd <- paste0("fy", fy, "q", pd)
    prev_pd <- paste0("fy", fy, "q", pd - 1)

    #Indicators => to be used as dplyr column names
    tx_curr_prev_pd <- rlang::sym(paste0("TX_CURR_Q", (pd - 1)))
    tx_new_curr_pd <- rlang::sym(paste0("TX_NEW_Q", pd))
    tx_rtt_curr_pd <- rlang::sym(paste0("TX_RTT_Q", pd))

    # Get TX_ML Data => to be joined to df_tx
    df_tx_ml <- extract_tx_ml(df_msd = df,
                              rep_agency = agencies,
                              rep_fy = fy,
                              rep_pd = pd,
                              lst_ous = ous)
    # Munging
    df_tx <- df %>%
      filter(
        fiscal_year == fy,
        fundingagency %in% agencies,
        indicator %in% c("TX_CURR", "TX_NEW", "TX_RTT"),
        standardizeddisaggregate %in% c("Total Numerator")
      ) %>%
      glamr::clean_agency()

    # Filter out OUs
    if (!is.null(ous)) {
      df_tx <- df_tx %>%
        filter(operatingunituid %in% ous)
    }

    # Continue data munging
    df_tx <- df_tx %>%
      group_by(fiscal_year,
               operatingunit,
               psnuuid,
               psnu,
               indicator,
               fundingagency) %>%
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
      ungroup() %>%
      reshape_msd() %>% #dplyr::gather(period, val, qtr1:qtr4)
      filter(str_detect(period, pds)) %>%
      filter(
        period == curr_pd | (period == prev_pd & indicator == "TX_CURR")
      ) %>%
      mutate(period = toupper(str_sub(period, 7, 8))) %>%
      pivot_wider(names_from = c(indicator, period),
                  values_from = val) %>%
      rowwise() %>%
      mutate(
        TX_BADEVENTS_DENOM = sum({{tx_curr_prev_pd}},
                                 {{tx_new_curr_pd}},
                                 {{tx_rtt_curr_pd}},
                                 na.rm = T))

    # Join df_tx_ml to df_tx
    df_tx_bad <- df_tx %>%
      left_join(
        df_tx_ml,
        by = c("operatingunit", "psnuuid", "psnu", "fundingagency")
      ) %>%
      mutate(
        TX_ML_PLP = round((TX_ML_bad / TX_BADEVENTS_DENOM), digits = 3)
      ) %>%
      clean_psnu()


    return(df_tx_bad)
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

    p <- df_geo2 %>%
      mutate(
        yorder = tidytext::reorder_within(psnu_short, {{mapvar}}, fundingagency),
        rank = percent_rank({{mapvar}})
      ) %>%
      ggplot(aes(y = {{mapvar}}, x = yorder)) +
      geom_col(aes(fill = {{mapvar}})) +
      facet_wrap(~ paste0(fundingagency, "\n"), nrow = 2, scales = "free_y") +
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


#' @title Map TX
#'
#' @param cname Country name
#' @param save Save output of not
#' return ggplot plot
#'
#tx_batch <- function(cname, save = F) {
tx_batch <- function(spdf,
                     df_gen,
                     mapvar,
                     cntry,
                     rep_pd,
                     terr_raster,
                     save = FALSE,
                     agency = TRUE,
                     facet_rows = 1,
                     gen_title = "",
                     four_parts = TRUE) {

  country <- {{cntry}}

  p1 <- map_generic(
    spdf = spdf,
    df_gen = df_gen,
    mapvar = TX_ML_PLP,
    cntry = country,
    terr_raster = terr_raster,
    agency = agency,
    facet_rows = 2,
    save = FALSE,
    four_parts = four_parts
  )

  p2 <- tx_graph(
    spdf = spdf_pepfar,
    df_gph = df_gen,
    mapvar = TX_ML_PLP,
    cntry = country
  )

  p <- (p1 + p2) +
    plot_layout(widths = c(1, 1)) +
    plot_annotation(
      caption = paste0(
        "OHA/SIEI - Data Source: MSD ",
        rep_pd,
        "_",
        toupper(country),
        "_TX_ML Patient Loss Proxy \n",
        "TX_ML_PLP = Number not retained on ART (LTFU, Died, Stopped) /
        (TX_CURR_prev + TX_NEW + TX_RTT) \n",
        "Produced on ",
        Sys.Date(),
        ", Missing data shown in gray on map."
      ),
      title = paste0(str_to_upper(country), " - TX_ML Patient Loss Proxy"),
      theme = theme(plot.title = element_text(hjust = .5))
    )

  if (save == TRUE) {

    print(p)

    ggsave(
      here("./Graphics",
           paste0(
             toupper(rep_pd),
             "_",
             "TX_ML_PLP_",
             toupper(country),
             "_",
             format(Sys.Date(), "%Y%m%d"),
             ".png"
           )
      ),
      plot = last_plot(),
      scale = 1.2,
      dpi = 400,
      width = 10,
      height = 7,
      units = "in"
    )
  }

  return(p)
}


#' Extract TX_ML from MER PSNU Dataset
#'
#' @param fy fiscal year
#' @param snu_prio snuprioritization
#'
extract_tx_ltfu <- function(.data,
                          rep_fy = "2020",
                          rep_pd = 4,
                          rep_agency = "USAID",
                          snu_prio = NULL,
                          mech_code = NULL) {

  # Params
  fy <- {{rep_fy}}
  pd <- {{rep_pd}}
  agency <- {{rep_agency}}
  prio <- {{snu_prio}}
  mechs <- {{mech_code}}

  ## For ZAF Only
  if (!is.null(snu_prio)) {
    .data %>%
      filter(snuprioritization %in% prio)
  }

  if (!is.null(mechs)) {
    .data %>%
      filter(mech_code %in% mechs)
  }

  ## Common Munging
  .data %>%
    filter(
      fiscal_year == fy,
      indicator == "TX_ML",
      standardizeddisaggregate == "Age/Sex/ARTNoContactReason/HIVStatus",
      typemilitary == 'N',
      fundingagency %in% agency
    ) %>%
    mutate(
      otherdisaggregate = str_remove(otherdisaggregate, "No Contact Outcome - "),
      otherdisagg = ifelse(
        str_detect(otherdisaggregate, "Lost to Follow-Up"),
        "Lost to Follow-Up",
        otherdisaggregate),
      otherdisagg = ifelse(
        str_detect(otherdisagg, "Refused"),
        "Refused or Stopped",
        otherdisagg),
      otherdisagg = factor(
        otherdisagg,
        levels = c("Transferred Out", "Lost to Follow-Up",
                   "Refused or Stopped", "Died"),
        labels = c("TO", "LTFU", "Refused or Stopped", "Died"))
    ) %>%
    group_by(operatingunit, snu1, snu1uid, psnu, psnuuid,
             indicator, otherdisagg) %>%
    summarize_at(vars(targets:cumulative), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(
      prct_ch = round(((qtr2 - qtr1) - qtr1) / qtr1 * 100, 2)
    ) %>%
    dplyr::select(operatingunit, snu1, snu1uid, psnuuid, psnu,
                  otherdisagg, qtr1:qtr4, prct_ch, cumulative) %>%
    group_by(operatingunit, snu1uid, snu1, psnuuid, psnu) %>%
    dplyr::mutate(
      ml_ttl = sum(cumulative, na.rm = T),
      to_ttl = first(cumulative),
      to_cum = round(first(cumulative) / sum(cumulative, na.rm = T) * 100, 2),
      prct = round( cumulative / sum(cumulative, na.rm = T) * 100, 2)
    ) %>%
    ungroup() %>%
    clean_psnu()
}

#' Create a bar graph of % TO
#'
#' @param df Summarized country level TX_ML Data
#' @param org_level snu1 or psnu
#'
plot_tx_ltfu <-
  function(df_msd,
           org_level = "psnu",
           fcolor = NULL) {
  # Params
  org <- {{org_level}}
  fcolor <- {{fcolor}}

  # Plot
  viz <- df_msd %>%
    mutate(label = paste0(!!sym(org_level), " (", to_ttl, "/", ml_ttl, ")")) %>%
    ggplot(aes(reorder(label, to_cum), prct, fill = otherdisagg)) +
    geom_col(position = position_fill(reverse = TRUE)) +
    geom_hline(yintercept = .25, color = grey10k, lwd = .3) +
    geom_hline(yintercept = .50, color = grey10k, lwd = .3) +
    geom_hline(yintercept = .75, color = grey10k, lwd = .3)

  if (is.null(fcolor)) {
    viz <- viz +
      scale_fill_brewer(palette = "Set3", direction = -1)
  } else {
    viz <- viz +
      scale_fill_manual(values = fcolor)
  }

  viz <- viz  +
    scale_y_continuous(position = "right", labels = percent) +
    coord_flip() +
    labs(x = "", y = "",
         subtitle = paste0(toupper(org), " (TO / ML)")) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.major.y = element_blank()
    )

  print(viz)

  return(viz)
}


#' Map % TO
#'
#' @param df country dataset
#' @param df_shp country geodata
#' @param label_name colname to be used for labels
#' @param uid_name colname foreign key from df
#'
map_tx_ltfu <-
  function(df, sf_shp,
                      label_name = "level5name",
                      uid_name = "psnuuid") {

  gviz <- sf_shp %>%
    left_join(df, by = c("uid" = {{uid_name}})) %>%
    dplyr::filter(!is.na(otherdisagg)) %>%
    ggplot(data = ., aes(fill = prct, label = {{label_name}})) +
    geom_sf(color = grey40k) +
    geom_sf(data = sf_shp, fill = NA, color = grey40k) +
    geom_sf_text(size = 1.5, color = grey60k) +
    scale_fill_viridis_c(direction = -1, na.value = NA) +
    facet_wrap(~otherdisagg, ncol = 2) +
    theme_void() +
    theme(
      legend.position = 'bottom',
      legend.box.just = "right",
      legend.title = element_blank(),
      legend.key.width = unit(2, "cm"),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", color = grey80k),
      plot.subtitle = element_text(face = "italic", margin = unit(c(1,1,10,1), 'pt')),
      plot.caption = element_text(face = "italic")
    )

  print(gviz)

  return(gviz)
}


#' Combine Map + Graph
#'
#' @param cntry_plot bar chart
#' @param cntry_map map
#' @param title graphic title
#' @param caption graphic footnote
#'
viz_tx_ml <- function(cntry_plot, cntry_map,
                      title = "<COUNTRY XYZ - Descriptive Title>",
                      #subtitle = "<Key takeway for audience>",
                      caption = "QAC Product") {

  viz_output <- cntry_map + cntry_plot +
    plot_layout(ncol = 2, widths = c(2, 1)) +
    plot_annotation(
      title = title,
      #subtitle = subtitle,
      caption = paste0("OHA/SIEI - ", caption, ", ", Sys.Date())
    ) +
    theme(title = element_text(face = "bold"))

  print(viz_output)

  return(viz_output)
}