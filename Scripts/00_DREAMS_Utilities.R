##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa, G.Sarfaty & T.Essam | USAID
##  EDITED:  T. Essam (2020-10-01)
##  PURPOSE: OVC Data Utilities
##  LICENCE: MIT
##  DATE:    2020-11-06
##  UPDATED: 2021-05-27


#' @title Extract DREAMS data
#'
#' @description AGYW_PREV Semi-annual indicator, data available for Q2 & Q4
#' @param df_msd Dataframe of MSD PSNU IM
#' @param country Operatingunit name (Single OU or list)
#' @param rep_fy Reporting Fiscal Year
#' @param rep_qtr Reporting Period (eg: 2, 4)
#' @return agyw_prev data
#'
extract_agyw_prevalence <-
  function(df_msd,
           country = NULL,
           rep_fy = 2021,
           rep_qtr = 2){

    # Params
    cntry <- {{country}}
    fy <- {{rep_fy}}
    pd <- {{rep_qtr}}

    # targeted period
    curr_pd <- paste0("qtr", pd)
    sym_curr_pd <- rlang::sym(curr_pd) # To be used with dplyr summarise

    # Exclude non-targeted period
    cols <- paste0("qtr", 1:4)
    remv_cols <- cols[!cols == curr_pd]

    # Filter and calculate
    df_dreams <- df_msd %>%
      filter(fiscal_year == fy,
             indicator == "AGYW_PREV",
             !is.na(otherdisaggregate))

    # Filter country data
    if (!is.null(cntry)) {
      df_dreams <- df_dreams %>%
        filter(operatingunit %in% cntry)
    }

    # Continue with filters
    df_dreams <- df_dreams %>%
      separate(otherdisaggregate, c("drop", "completion"), sep = ", ") %>%
      rename(time_dreams = otherdisaggregate_sub) %>%
      clean_psnu() %>%
      mutate(
        shortname = psnu,
        time_dreams = case_when(
          time_dreams %in% c("<6 Months in DREAMS",
                             "07-12 Months in DREAMS") ~ "leq12m",
          time_dreams %in% c("13-24 Months in DREAMS",
                             "25+ Months in DREAMS") ~ "gt12m"),
        completion = case_when(
          completion %in% c("Primary Package Completed",
                            "Primary Package Completed and Secondary") ~ "completed",
          completion %in% c("Primary Package Incomplete") ~ "incomplete")
      ) %>%
      group_by(fiscal_year, operatingunit, snu1, psnuuid, psnu, shortname,
               indicator, completion, time_dreams) %>%
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
      ungroup() %>%
      dplyr::select(-indicator, -{{remv_cols}}) %>%
      group_by(fiscal_year, operatingunit, snu1, psnuuid, psnu, shortname) %>%
      summarise(
        total = sum({{sym_curr_pd}}, na.rm = TRUE),
        ttl_leq12m = sum({{sym_curr_pd}}[time_dreams == "leq12m"], na.rm = TRUE),
        prp_leq12m = ttl_leq12m / total,
        ttl_gt12m = sum({{sym_curr_pd}}[time_dreams == "gt12m"], na.rm = TRUE),
        prp_gt12m = ttl_gt12m / total,
        ttl_completed = sum(
          {{sym_curr_pd}}[completion == "completed"],
          na.rm = TRUE),
        prp_completed = ttl_completed / total,
        ttl_completed_gt12m = sum(
          {{sym_curr_pd}}[time_dreams == "gt12m" & completion == "completed"],
          na.rm = TRUE),
        prp_completed_gt12m = ttl_completed_gt12m / ttl_gt12m,
      ) %>%
      ungroup() %>%
      mutate(across(.cols = starts_with("prp"),
                    .fns = ~ case_when(
                      is.nan(.) ~ NA_real_,
                      TRUE ~ .))) %>%
      gather(indicator, value, starts_with(c("tot", "ttl", "prp")))

    return(df_dreams)
  }


#' @title Map AGYW_PREV
#' @description
#'
#' @param spdf
#' @param terr
#' @param df_agyw
#' @return
#'
map_agyw_prevalence <-
  function(spdf, terr, df_agyw,
           orglevel = "PSNU",
           facet = FALSE) {

    # Identify Country/OU
    country <- df_agyw %>%
      dplyr::distinct(operatingunit) %>%
      pull()

    # Geodata
    spdf_adm0 <- spdf %>%
      filter(operatingunit %in% country, label == "country")

    spdf_adm1 <- spdf %>%
      filter(operatingunit %in% country, label == "snu1")

    # Basemap
    basemap <- terrain_map(countries = lookup_country(country),
                           adm0 = spdf_adm0,
                           adm1 = spdf_adm1,
                           mask = TRUE,
                           terr = terr)
    # spdf agyw
    df_agyw <- spdf %>%
      left_join(df_agyw, by = c("uid" = "psnuuid")) %>%
      filter(indicator == "prp_completed_gt12m", !is.na(value))

    # Map
    agyw_map <- basemap +
      geom_sf(
        data = df_agyw,
        aes(fill = value),
        lwd = .2,
        color = grey10k) +
      geom_sf(
        data = spdf_adm0,
        fill = NA,
        lwd = 2,
        color = grey10k) +
      geom_sf(
        data = spdf_adm0,
        fill = NA,
        lwd = .75,
        color = grey90k) +
      scale_fill_si(palette = "scooters",
                    discrete = FALSE,
                    labels = percent_format(accuracy = 1),
                    limits = c(0, 1)) +
      labs(subtitle = "% who completed at least primary package") +
      si_style_map() +
      theme(
        legend.position =  "bottom",
        legend.key.width = ggplot2::unit(1.5, "cm"),
        legend.key.height = ggplot2::unit(.5, "cm")
      )

    return(agyw_map)
  }


#' @title Plot AGYW 13+ Months on Dreams
#'
#' @param df_dreams OVC Datasets
#' @param type Plot type (bars or dots)
#' @return ggplot plot
#'
plot_agyw_prevalence <-
  function(df_agyw, type = "bars") {

    if (type == "dots") {
      # dot-plot
      plot <- df_agyw %>%
        filter(indicator %in% c("ttl_gt12m", "prp_completed_gt12m")) %>%
        spread(indicator, value) %>%
        mutate(label = paste0(shortname, " (", comma(ttl_gt12m, 1), ")")) %>%
        ggplot(aes(x = reorder(label, prp_completed_gt12m),
                   y = prp_completed_gt12m)) +
        geom_point(aes(size = prp_completed_gt12m,
                       fill = prp_completed_gt12m),
                   color = grey50k,
                   shape = 21,
                   show.legend = F) +
        scale_size_continuous(range = c(3, 12)) +
        scale_fill_si(palette = "scooters",
                      discrete = FALSE,
                      labels = percent_format(accuracy = 1),
                      limits = c(0, 1)) +
        scale_y_continuous(position = "right",
                           labels = percent,
                           limits = c(0, 1),
                           #breaks = seq(0, 1, 0.25)) +
                           breaks = c(0, .25, .5, .75, .9, 1)) +
        geom_hline(aes(yintercept = .9),
                   color = grey70k,
                   size = .7,
                   linetype = "dashed",
                   alpha = .8) +
        labs(subtitle = "% who completed at least primary package",
             x = "", y = "") +
        coord_flip() +
        si_style_xgrid() +
        theme(
          axis.text.x = element_text(size = 6)
        )

      return(plot)

    } else if (type == "bars") {
      # Bar charts

      # Reshape data
      df_agyw <- df_agyw %>%
        filter(indicator %in% c("total", "prp_gt12m", "prp_completed_gt12m")) %>%
        spread(indicator, value) %>%
        mutate(label = paste0(shortname, " (", scales::comma(total, 1), ")"))

      # Plot
      plot <-  df_agyw %>%
        ggplot(aes(x = reorder(label, prp_completed_gt12m))) +
        geom_bar(stat = "identity", aes(y = 1), fill = grey10k) +
        geom_bar(stat = "identity", aes(y = prp_gt12m), fill = scooter) +
        scale_fill_manual(values = c(grey20k, grey40k)) +
        geom_text(aes(y = prp_gt12m, label = paste0(" ", percent(prp_gt12m, 1))),
                  position = position_stack(vjust = 1),
                  hjust = "inward",
                  color = glitr::grey80k,
                  size = 4) +
        coord_flip() +
        labs(subtitle = "% in DREAMS 13+ months",
             x = "", y = "") +
        si_style_nolines() +
        theme(legend.position = "none", axis.text.x = element_blank())

      return(plot)

    } else {
      stop(glue("Invalid plot type: {type}"))
    }
  }
