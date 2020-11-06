##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa, G.Sarfaty & T.Essam | USAID
##  EDITED:  T. Essam (2020-10-01)
##  PURPOSE: OVC Data Utilities
##  LICENCE: MIT
##  DATE:    2020-11-06


#' @title Extract DREAMS data
#'
#' @description AGYW_PREV Semi-annual indicator, data available for Q2 & Q4
#' @param df_msd Dataframe of MSD PSNU IM
#' @param country Operatingunit name (Single OU or list)
#' @param rep_fy Reporting Fiscal Year
#' @param rep_pd Reporting Period (eg: 2, 4)
#' @return agyw_prev data
#'
extract_agyw_prevalence <-
  function(df_msd, country,
           rep_fy = 2020,
           rep_pd = 2){

    # Params
    cntry <- {{country}}
    fy <- {{rep_fy}}
    pd <- {{rep_pd}}

    # targeted period
    pd <- paste0("qtr", pd)

    # Exclude targeted period
    cols <- c("fiscal_year", paste0("qtr", 1:4))
    cols <- cols[!cols == "qtr2"]


    # Filter and calculate
    agyw_prev_time <- df_msd %>%
      filter(fiscal_year == fy,
             indicator == "AGYW_PREV",
             !is.na(otherdisaggregate),
             operatingunit %in% cntry) %>%
      separate(otherdisaggregate, c("drop", "completion"), sep = ", ") %>%
      group_by(fiscal_year, operatingunit, psnuuid, psnu,
               indicator, completion, otherdisaggregate_sub) %>%
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
      ungroup() %>%
      select(-{{cols}}) %>%
      rename(time_dreams = otherdisaggregate_sub) %>%
      mutate(
        shortname = str_remove(psnu, "District"),
        time_dreams = case_when(
          time_dreams %in% c("<6 Months in DREAMS",
                             "07-12 Months in DREAMS") ~ "Less than 1 year",
          time_dreams %in% c("13-24 Months in DREAMS",
                             "25+ Months in DREAMS") ~ "13+ months"),
        completion = case_when(
          completion %in% c("DREAMS Fully Completed",
                            "DREAMS Fully Completed and Secondary") ~ "completed",
          completion %in% c("DREAMS Not Completed",
                            "DREAMS Only Secondary Completed") ~ "not complete")
      ) %>%
      filter(time_dreams == "13+ months") %>%
      group_by(operatingunit, psnuuid, psnu, shortname, completion) %>%
      summarize_at(vars({{pd}}), sum, na.rm = TRUE) %>%
      spread(completion, {{pd}}) %>%
      rowwise() %>%
      mutate(total = sum(c_across(completed:`not complete`), na.rm = TRUE)) %>%
      mutate(across(completed:`not complete`, ~ . / total)) %>%
      rename(completed_13plus = completed,
             incomplete_13plus = `not complete`,
             total_13plus = total) %>%
      gather(indicator, val, completed_13plus:total_13plus)

    # Percentage
    prct_total <- df %>%
      filter(fiscal_year == fy,
             indicator == "AGYW_PREV",
             !is.na(otherdisaggregate),
             operatingunit %in% cntry) %>%
      rename(time_dreams = otherdisaggregate_sub) %>%
      mutate(
        shortname = str_remove(psnu, "District"),
        time_dreams = case_when(
          time_dreams %in% c("<6 Months in DREAMS",
                             "07-12 Months in DREAMS") ~ "Less than 1 year",
          time_dreams %in% c("13-24 Months in DREAMS",
                             "25+ Months in DREAMS") ~ "13+ months")) %>%
      group_by(fiscal_year, operatingunit, shortname,
               psnuuid, psnu, indicator, time_dreams) %>%
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
      ungroup() %>%
      select(-{{cols}}) %>%
      spread(time_dreams, {{pd}}) %>%
      rowwise() %>%
      mutate(total = sum(c_across(`13+ months`:`Less than 1 year`), na.rm = TRUE)) %>%
      mutate(across(`13+ months`:`Less than 1 year`, ~ . / total)) %>%
      gather(indicator,val,`13+ months`:total)

    # Totals
    totals <- agyw_prev_time %>%
      filter(indicator %in% c("total_13plus", "completed_13plus")) %>%
      spread(indicator, val) %>%
      select(c(psnuuid, total_13plus, completed_13plus))

    # bind all
    agyw <- rbind(agyw_prev_time, prct_total) %>%
      full_join(totals, by = "psnuuid")

    return(agyw)
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
      filter(operatingunit %in% country, type == "OU")

    df_agyw <- spdf %>%
      filter(operatingunit %in% country, type == orglevel) %>%
      left_join(df_agyw, by = c("uid" = "psnuuid")) %>%
      filter(indicator == "completed_13plus", !is.na(val))

    # Map
    agyw_map <- get_basemap(
        spdf = spdf,
        cntry = country,
        terr_raster = terr,
        add_admins = TRUE
      ) +
      geom_sf(
        data = df_agyw,
        aes(fill = val, label = completed_13plus),
        lwd = .2,
        color = grey10k) +
      geom_sf(
        data = spdf_adm0,
        fill = NA,
        lwd = .2,
        color = grey30k) +
      scale_fill_viridis_c(option = "magma",
                           direction = -1,
                           labels = percent_format(accuracy = 1)) +
      ggtitle("% who completed at least primary package\nafter being in DREAMS 13+ months") +
      si_style_map() +
      theme(
        legend.position =  "bottom",
        legend.key.width = ggplot2::unit(1, "cm"),
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

    # dot-plot
    plot <- df_agyw %>%
      mutate(label = paste0(shortname, " (", total_13plus, ")")) %>%
      filter(indicator == "completed_13plus") %>%
      ggplot(aes(x = reorder(label, val), y = val)) +
      geom_point(aes(size = total_13plus, fill = val),
                 color = grey50k,
                 shape = 21,
                 show.legend = F) +
      scale_size_continuous(range = c(3, 12)) +
      scale_color_viridis_c(option = "magma",
                            direction = -1,
                            aesthetics = c("fill")) +
      geom_hline(aes(yintercept = .9),
                 color = "gray70",
                 size = 0.35,
                 linetype = "dashed",
                 alpha = .8) +
      scale_y_continuous(position = "right", labels = percent,
                         limits = c(0, 1),
                         breaks = seq(0, 1, 0.5)) +
      labs(x = "", y = "") +
      coord_flip() +
      si_style_xgrid() +
      ggtitle("% who completed at least primary package\nafter being in DREAMS 13+ months")

    # Bar charts (overwrite previous)
    if (type == "bars") {

      plot <- df_agyw %>%
        filter(indicator %in% c("13+ months", "Less than 1 year")) %>%
        ggplot(
          aes(x = reorder(shortname, completed_13plus), y = val,
          fill = factor(indicator,
                        levels = c("Less than 1 year","13+ months")))) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c(grey20k, grey40k)) +
        geom_text(data = df_agyw %>% filter(indicator == "13+ months"),
                  aes(label = percent(val, 1)),
                  position = position_stack(vjust = 0.75), color = "white") +
        coord_flip() +
        labs(title = "% in DREAMS 13+ months out of \n total beneficaries",
             x = "", y = "") +
        si_style_nolines() +
        theme(legend.position = "none", axis.text.x = element_blank())
    }

    return(plot)
  }
