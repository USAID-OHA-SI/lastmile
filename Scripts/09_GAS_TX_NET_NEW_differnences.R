##  PROJECT: TX_NET_NEW Changes across agencies / partners / age
##  AUTHOR:  B.Kagniniwa, G.Sarfaty & T.Essam | USAID
##  PURPOSE: Geo-depiction of TX_NET_NET changes
##  LICENCE: MIT
##  DATE:    2020-09-04

# Libraries
library(tidyverse)
library(vroom)
library(sf)
library(sp)
library(raster)
library(gisr)
library(glitr)
library(glamr)
library(janitor)
library(scales)
library(patchwork)
library(here)
library(ICPIutilities)
library(extrafont)
library(gggenes)
library(tidytext)

# REQUIRED -------------------------------------------------------------------------------

    ## Get Credentials

    # source("./_secrets/credentials.R")
    #
    # ## Source lines of function from 04_GAS_OU_OVC
    # source2 <- function(file, start, end, ...) {
    #     file.lines <- scan(file, what = character(),
    #                        skip = start - 1,
    #                        nlines = end + 1,
    #                        sep = '\n')
    #     file.lines.collapsed <- paste(file.lines, collapse = '\n')
    #
    #     source(textConnection(file.lines.collapsed), ...)
    # }
    #
    # source2("Scripts/04_GAS_OU_OVC_Treatment.R", start = 58, end = 541)




# GLOBALS -------------------------------------------------------------

    ## Data & Output folders

    dir_scripts <- "Scripts"
    dir_data <- "Data"
    dir_dataout <- "Dataout"
    dir_gis <- "GIS"
    dir_graphics <- "Graphics"
    dir_geodata <- "../../GEODATA/PEPFAR"
    dir_terr <- "../../GEODATA/RASTER"
    dir_merdata <- "../../MERDATA"

    ## Q3 MER Data

    psnu_im <- "^MER_.*_PSNU_IM_.*_20200918_.*.zip$"

    ## Reporting Filters

    rep_agency = "USAID"

    rep_fy = 2020
    rep_qtr = 3

    rep_pd = rep_fy %>%
        as.character() %>%
        str_sub(3,4) %>%
        paste0("FY", ., "Q", rep_qtr)


    usaid_clr = "#2887a1"
    cdc_clr   = "#a16928"

    psnu_baggage <- "(District$|County$|District Municipality|Oblast'$|MC$|DC$|Woreda|Metropolitan $|Metropolitan Municipality$)"

    ratio_calc <- function(x, y) {
            ifelse(y > 0.000, x / y, 0)
        }



# LOAD and MUNGE ----------------------------------------------------------


    ## File path + name
    file_psnu_im <- list.files(
        path = dir_merdata,
        pattern = psnu_im,
        recursive = TRUE,
        full.names = TRUE
    ) %>%
        sort() %>%
        last()

    ## MER PSNU Data
    df_psnu <- vroom(file_psnu_im) %>% filter(indicator %in% c("TX_NET_NEW", "TX_CURR"))

    df_psnu %>% glimpse()

    df_ous <- df_psnu %>%
        distinct(operatingunit, operatingunituid)

    ## SNU1
    df_snus <- df_psnu %>%
        dplyr::select(operatingunit, countryname, snu1, snu1uid) %>%
        distinct(operatingunit, countryname, snu1, snu1uid) %>%
        filter(!is.na(snu1)) %>%
        arrange(operatingunit, countryname, snu1)

    ## PSNU
    df_psnus <- df_psnu %>%
        dplyr::select(operatingunit, operatingunituid, countryname, snu1, snu1uid, psnu, psnuuid) %>%
        distinct(operatingunit, countryname, snu1, snu1uid, psnu, psnuuid) %>%
        arrange(operatingunit, countryname, snu1, psnu)


    ## List of Distinct Orgs

    ## 2619
    lst_psnuuid <- df_psnus %>%
        filter(!is.na(psnuuid), psnuuid != "?") %>%
        distinct(psnuuid) %>%
        pull()

    ## 416
    lst_snu1uid <- df_snus %>%
        filter(!is.na(snu1uid), snu1uid != "?") %>%
        distinct(snu1uid) %>%
        pull()

    ## 28
    lst_ouuid <- df_ous %>%
        pull(operatingunituid)


    ## TX_NET_NEW Changes across agency and partner
    df_tx_nn_delta <- df_psnu %>%
        filter(
            fiscal_year == rep_fy,
            standardizeddisaggregate == "Total Numerator"
        ) %>%
        reshape_msd(clean = TRUE) %>%
        filter(period_type == "results")



    # Show a list by OUs of where CDC and USAID overlap in PSNUs for Q3

    tmp <- df_tx_nn_delta %>%
        filter(
            period %in% c("FY20Q3", "FY20Q2"),
            fundingagency %in% c("HHS/CDC", "USAID"),
            indicator %in% c("TX_NET_NEW", "TX_CURR")
        ) %>%
        mutate(fundingagency = if_else(fundingagency == "HHS/CDC", "CDC", fundingagency)) %>%
        group_by(fundingagency, period, psnu, psnuuid, indicator, operatingunit, operatingunituid,
                 countryname) %>%
        summarise(val = sum(val, na.rm = TRUE)) %>%
        ungroup() %>%
        arrange(psnu, fundingagency) %>%

        # Pivot wide to easily calculate TX_NN_CURR ratio
        pivot_wider(names_from = c(indicator, period),
                    values_from = val) %>%
        mutate(TX_NN_CURR_ratio = TX_NET_NEW_FY20Q3 / TX_CURR_FY20Q2) %>%
        select_at(vars(!contains("FY20Q2"))) %>%
        dplyr::select(-TX_CURR_FY20Q3) %>%

        # Pivot again to get agencies on same line to check for USAID or CDC
        pivot_wider(
            names_from = fundingagency,
            values_from = c(TX_NET_NEW_FY20Q3, TX_NN_CURR_ratio)
        ) %>%
        rename(USAID = TX_NET_NEW_FY20Q3_USAID,
               CDC = TX_NET_NEW_FY20Q3_CDC,
               USAID_ratio = TX_NN_CURR_ratio_USAID,
               CDC_ratio = TX_NN_CURR_ratio_CDC) %>%
                mutate(both = if_else(!is.na(CDC) & !is.na(USAID), "Both", "Only CDC or USAID"),
               diff = USAID - CDC,
               diff_bin = case_when(
                   diff > 0 ~ "USAID higher \n",
                   diff < 0 ~ "CDC Higher \n",
                   is.na(diff) ~ "One Agency in PSNU \n"),
               diff_bin = fct_relevel(diff_bin, "USAID higher \n", "CDC Higher \n", "One Agency in PSNU \n"),
               #psnu_order = trim(psnu, side = c("both")),
               psnu_order = str_remove_all(psnu, psnu_baggage))

    tmp <- tmp %>%
            group_by(operatingunit) %>%
            mutate(both_count = sum(both == "Both")) %>%
        ungroup()
        # rowwise() %>%
        # mutate(order_var = max(USAID, CDC, na.rm = T),
        #        order_var = if_else(!is.na(diff), diff, order_var))


#
#     # Check South Africa
#         tmp %>% filter(operatingunit == "South Africa") %>%
#             mutate(trim = trim(psnu)) %>%
#             distinct(psnu_order
#                      ) %>% prinf



 return_dotplot <- function(df, ou){


   p <-  df %>%
       filter(operatingunit %in% {{ou}}) %>%
       mutate(
           psnu_order = reorder_within(psnu_order, abs(diff), diff_bin)
              ) %>%
       ggplot(aes(x = psnu_order)) +
       coord_flip() +
       geom_segment(aes(y = CDC, yend = USAID, xend = psnu_order),
                    size = 3, alpha = 0.5, color = grey20k) +
       geom_point(aes(y = CDC), shape = 21, size = 4, fill = cdc_clr, color = grey80k) +
       geom_point(aes(y = USAID), shape = 21, size = 4, fill = usaid_clr, color = grey90k) +
       si_style_xgrid() +
       scale_x_reordered() +

       facet_wrap(~diff_bin, scales = "free_y", drop = T) +

          labs(x = NULL, y = NULL,
               title = paste({{ou}}, "TX_NET_NEW"))
       #scale_size_binned(name = "TX_NET_NEW / TX_CURR_lag", guide = "bins")


     name <- paste0("FY20Q3_",
                    str_replace("TX_NET_NEW", " ", "_"),
                    "_", toupper({{ou}}),
                    "_",
                    format(Sys.Date(), "%Y%m%d"),
                    ".png")


     ggsave(file.path(dir_graphics, name),
            plot = p,
            scale = 1.2, dpi = 400,
            width = 10, height = 7, units = "in")

     return(p)
 }



   tmp %>% filter(both_count > 1) %>%
       distinct(operatingunit) %>%
       pull() %>%
       map(.x, .f = ~return_dotplot(tmp, .x))








   tmp_pp <- df_tx_nn_delta %>%
       filter(
           period %in% c("FY20Q3"),
           fundingagency %in% c("USAID"),
           indicator == "TX_NET_NEW",
           countryname == "Malawi"
       ) %>%
       mutate(fundingagency = if_else(fundingagency == "HHS/CDC", "CDC", fundingagency)) %>%
       group_by(period, psnu, psnuuid, indicator, operatingunit, operatingunituid,
                countryname, mech_code, mech_name) %>%
       summarise(val = sum(val, na.rm = TRUE)) %>%
       ungroup() %>%
       arrange(psnu, mech_code) %>%
       spread(mech_code, val) %>%
       rowwise() %>%
       mutate(both = sum(!is.na(c_across(where(is.double)))))

   %>%
       mutate(both = if_else(!is.na(CDC) & !is.na(USAID), "Both", "Only CDC or USAID"),
              diff = USAID - CDC,
              diff_bin = case_when(
                  diff > 0 ~ "USAID higher",
                  diff < 0 ~ "CDC Higher",
                  is.na(diff) ~ "One Agency in PSNU"),
              diff_bin = fct_relevel(diff_bin, "USAID higher", "CDC Higher", "One Agency in PSNU"))









