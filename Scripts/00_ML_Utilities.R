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
      cat(Wavelength::paint_red("This may not be appropriate for QTR1"))
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
      mutate(
        fundingagency = if_else(
          fundingagency == "HHS/CDC",
          "CDC",
          fundingagency))

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
      mutate(
        # period = case_when(
        #   period == "fy2020q2" ~ "Q2",
        #   period == "fy2020q3" ~ "Q3")
        period = toupper(str_sub(period, 7, 8))
      ) %>%
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
      mutate(TX_ML_PLP = round((TX_ML_bad / TX_BADEVENTS_DENOM), digits = 3))

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
        # psnu_short = case_when(
        #   str_detect(psnu, " County$") ~ str_replace_all(psnu, " County$", ""),
        #   str_detect(psnu, " District$") ~ str_replace_all(psnu, " District$",""),
        #   str_detect(psnu, " Municipality$") ~ str_replace_all(psnu, " Municipality$",""),
        #   str_detect(psnu, " Metropolitan$") ~ str_replace_all(psnu, " Metropolitan$",""),
        #   str_detect(psnu, " District$") ~ str_replace_all(psnu, " District$",""),
        #   TRUE ~ psnu
        # ),
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
      title = paste0(str_to_upper(country), " - TX_ML Patient Loss Proxy")
    )

  print(p)

  if (save == TRUE) {
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
