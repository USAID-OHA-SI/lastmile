##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa, G.Sarfaty & T.Essam | USAID
##  EDITED:  T. Essam (2020-10-01)
##  PURPOSE: VL-Utilities
##  LICENCE: MIT
##  DATE:    2020-11-02

#' @title TX_ML Dataset
#' @description Aggregate bad events then divide by TX_CURR_lagged_1
#' Numerator: Number of patients not retained on ART (LTFU, Died, Stopped)
#' Denominator: TX_CURR_lag1 + TX_NEW_curr + TX_RTT
#' @param df_msd
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

  df <- df_psnu %>%
    filter(
      fiscal_year == fy,
      fundingagency %in% agencies,
      indicator %in% c("TX_ML"),
      standardizeddisaggregate %in% c("Age/Sex/ARTNoContactReason/HIVStatus"),
      typemilitary == 'N'
    )

  if(!is.null(ous)){
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
