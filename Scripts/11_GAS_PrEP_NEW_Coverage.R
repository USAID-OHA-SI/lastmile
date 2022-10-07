## PROJECT:  PEPFAR Geospatial Analytics
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  Lesotho - PrEP NEW vs HTS POS
## Date:     2022-09-21

# LIBRARIES ----

  library(tidyverse)
  library(gophr)
  library(grabr)
  library(sf)
  library(gisr)
  library(glitr)
  library(glamr)
  library(janitor)
  library(scales)
  library(patchwork)
  library(extrafont)
  library(shadowtext)

# SETUP ----
  ## Directories ----
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"

  dir_geodata <- si_path("path_vector")
  dir_terr <- si_path("path_raster")
  dir_merdata <- si_path("path_msd")

  ## Reporting Filters ----

  rep_agency = "USAID"

  rep_agencies <- c("USAID", "CDC")

  cntries <- c("Nigeria", "Lesotho", "Eswatini", "Namibia", "South Africa")

  ## Indicators
  inds <- c("HTS_TST", "HTS_TST_POS", "PrEP_CURR", "PrEP_NEW", "PrEP_CT")

  ## Tech areas
  tech_areas <- inds %>%
    map(word, sep = "_") %>%
    unlist() %>%
    unique()

  ## Disaggs
  disaggs <- list(
    tn = "Total Numerator",
    td = "Total Denominator",
    as = "Age/Sex",
    kp = "KeyPop"
  )

# Files ----

  ## Global OUxIM
  file_ou_im <- glamr::return_latest(
    folderpath = dir_merdata,
    pattern = "OU_IM_FY20-23_.*.zip$"
  )

  ## Global PSNUxIM
  file_psnu_im <- glamr::return_latest(
    folderpath = dir_merdata,
    pattern = "PSNU_IM_FY20-23_.*_\\d{1}.zip$"
  )

  ## Extract OU - SitexIm
  cntries %>%
    map(function(.c) {
      f <- list.files(
        path = dir_merdata,
        pattern = paste0("Site_IM_FY20-23_.*_", .c)
      )

      if(length(f) == 0) {
        return(.c)
      }

      return(NA)
    }) %>%
    unlist() %>%
    magrittr::extract(!is.na(.)) %>%
    walk(function(.c){
      pano_extract_msd(operatingunit = .c,
                       version = "Clean",
                       fiscal_year = 2022,
                       quarter = 3,
                       level = "site",
                       dest_path = dir_merdata)
    })

  ## Get list of files
  files_site_im <- list.files(
    path = dir_merdata,
    full.names = TRUE,
    pattern = paste0(paste0("Site_IM_FY20.*_", cntries, ".zip$"), collapse = "|")
  )

  files_site_im

  ## Data Sources
  msd_source <- file_ou_im %>% source_info(return = "source")

  ## Reporting Periods
  curr_fy <- file_ou_im %>% source_info(return = "fiscal_year")
  curr_qtr <- file_ou_im %>% source_info(return = "quarter")
  curr_pd <- file_ou_im %>% source_info(return = "period")


# FUNCTIONS ----

  cntry_polygon <- function(spdf, cntry, level = "country") {
    filter(
      spdf,
      countryname == {{cntry}},
      (label == {{level}} | level == {{level}}))
  }

  cntry_map <- function(.spdf, .data, cntry,
                        ind = NULL,
                        rep_pd = "FY22",
                        rep_value = "cumulative",
                        rep_group = "Adult Male") {

    # Geodata
    spdf_cntry <- cntry_polygon(spdf = .spdf, cntry = cntry, level = "country")
    spdf_psnu <- cntry_polygon(spdf = .spdf, cntry = cntry, level = "prioritization")

    # Program Data
    df_cntry <- .data %>%
      filter(country == cntry,
             period == rep_pd,
             period_type == rep_value)

    if (!is.null(rep_group)) {
      df_cntry <- df_cntry %>%
        filter(pop_group == rep_group)
    }

    spdf <- spdf_psnu %>%
      left_join(df_cntry, by = c("uid" = "psnuuid")) %>%
      filter(!is.na(value))

    locs <- spdf %>%
      st_geometry() %>%
      st_make_valid() %>%
      st_centroid() %>%
      st_coordinates()

    spdf <- spdf %>%
      bind_cols(locs)

    # Base map
    bmap <- terrain_map(countries = cntry,
                        adm0 = spdf_cntry,
                        adm1 = spdf_psnu,
                        mask = T)

    #
    map <- bmap +
      geom_sf(data = spdf, aes(fill = value), size = .6, color = grey10k, linetype = "dashed", show.legend = TRUE) +
      geom_sf(data = spdf_cntry, fill = NA, color = grey10k, size = 1.5) +
      geom_sf(data = spdf_cntry, fill = NA, color = grey90k, size = .5) +
      geom_shadowtext(data = spdf,
                    aes(X, Y, label = paste0(str_replace(psnu, " ", "\n"), "\n", comma(value, 1))),
                    size = 2.5, color = usaid_darkgrey, bg.color = grey10k) #+
      # geom_sf_text(data = spdf,
      #              aes(label = paste0(name, "\n", comma(value, 1))),
      #              color = grey90k)

    # Add indicator facet
    if (is.null(ind)) {
      map <- map +
        facet_grid(pop_group ~ indicator)
    }

    map <- map +
      scale_fill_si(palette = "burnt_siennas") +
      si_style_map() +
      theme(legend.position = "top",
            legend.title = element_blank(),
            legend.key.width = ggplot2::unit(2, "cm"),
            legend.key.height = ggplot2::unit(.3, "cm"))

    return(map)
  }

  ##

# LOAD DATA ----

  ## Geodata

  ras <- get_raster()

  spdf_pepfar <- get_vcpolygons()

  df_attrs <- cntries %>%
    map_dfr(get_attributes)

  df_attrs

  spdf_cntries <- spdf_pepfar %>%
    left_join(df_attrs, by = c("uid" = "id")) %>%
    filter(!is.na(name))

  spdf_cntries %>%
    st_drop_geometry() %>%
    distinct(countryname)

  # Country sub-units
  spdf_nga_cntry <- cntry_polygon(spdf_cntries, cntry = "Nigeria", level = "country")
  spdf_nga_psnu <- cntry_polygon(spdf_cntries, cntry = "Nigeria", level = "prioritization")
  spdf_nga_lga <- cntry_polygon(spdf_cntries, cntry = "Nigeria", level = "community")

  spdf_nga_psnu %>%
    st_geometry() %>%
    st_make_valid() %>%
    st_centroid() %>%
    st_coordinates() %>%
    bind_cols(spdf_nga_psnu, .)

  # Global PSNUxIM Dataset
  df_psnu <- file_psnu_im %>%
    read_msd() %>%
    filter(str_detect(indicator, paste0(tech_areas, collapse = "|"))) %>%
    clean_agency()

  df_psnu %>% distinct(indicator) %>% prinf()

  df_psnu %>%
    distinct(indicator, standardizeddisaggregate) %>%
    arrange(indicator, standardizeddisaggregate) %>% prinf()

# MUNGING ----

  ## PSNUxIM
  # df_psnu %>%
  #   filter(fiscal_year == curr_fy,
  #          funding_agency == rep_agency,
  #          operatingunit %in% cntries,
  #          indicator %in% inds,
  #          standardizeddisaggregate == "Total Numerator") %>%
  #   group_by(fiscal_year, funding_agency, operatingunit, country,
  #            psnuuid, psnu, indicator) %>%
  #   summarise(across(c(starts_with("qtr"), cumulative, targets),
  #                    sum, na.rm = T), .groups = "drop") %>%
  #   reshape_msd()

  # PSNUxInd Dataset
  df_psnu_cov <- df_psnu %>%
    filter(funding_agency == rep_agency,
           fiscal_year == curr_fy,
           country %in% cntries,
           (indicator == "HTS_TST_POS" &
             standardizeddisaggregate == "Modality/Age/Sex/Result" |
           indicator == "PrEP_NEW" &
             standardizeddisaggregate == "Age/Sex"))

  df_psnu_cov <- df_psnu_cov %>%
    group_by(fiscal_year, country, psnuuid, psnu,
             indicator, trendscoarse, sex) %>%
    summarise(across(c(starts_with("qtr"), cumulative, targets),sum, na.rm = T),
              .groups = "drop") %>%
    reshape_msd() %>%
    group_by(period, country, psnuuid, psnu, indicator, period_type) %>%
    mutate(
      pop_group = case_when(
        trendscoarse == "<15" ~ "Children",
        trendscoarse == "15+" & sex == "Male" ~ "Adult Male",
        trendscoarse == "15+" & sex == "Female" ~ "Adult Female",
        TRUE ~ "Other"
      )
    ) %>%
    ungroup()

  df_psnu_cov <- df_psnu_cov %>%
    group_by(period, country, psnuuid, psnu,
             indicator, period_type) %>%
    summarise(across(value, sum, na.rm = T), .groups = "drop") %>%
    mutate(trendscoarse = NA,
           sex = NA,
           pop_group = "All") %>%
    bind_rows(df_psnu_cov, .) %>%
    mutate(pop_group = factor(pop_group,
                              levels = c("Children", "Adult Female",
                                         "Adult Male", "Other", "All"),
                              ordered = T)) %>%
    arrange(country, psnuuid, psnu, indicator, pop_group)

  df_psnu_cov <- df_psnu_cov %>%
    clean_column(colname = "psnu")

  # Country Summary

  df_ou_cov <- df_psnu_cov %>%
    group_by(period, period_type, pop_group, country, indicator) %>%
    summarise(across(value, sum, na.rm=T), .groups = "drop")


# VIZ ----

  df_psnu_cov %>%
    filter(country == cntries[1],
           period == "FY22",
           period_type == "cumulative",
           pop_group == "Adult Male")

  cntry_map(.spdf = spdf_cntries,
            .data = df_psnu_cov,
            cntry = cntries[1])

  cntry_map(.spdf = spdf_cntries,
            .data = df_psnu_cov,
            cntry = cntries[1],
            rep_group = "All")

  cntry_map(.spdf = spdf_cntries,
            .data = df_psnu_cov,
            cntry = cntries[2],
            rep_group = "All")

  cntry_map(.spdf = spdf_cntries,
            .data = df_psnu_cov,
            cntry = cntries[3],
            rep_group = "All")

  cntry_map(.spdf = spdf_cntries,
            .data = df_psnu_cov,
            cntry = cntries[4],
            rep_group = "All")

  cntry_map(.spdf = spdf_cntries,
            .data = df_psnu_cov,
            cntry = cntries[5],
            rep_group = "All")

  cntry_map(.spdf = spdf_cntries,
            .data = df_psnu_cov,
            cntry = cntries[1],
            rep_group = NULL)

  df_ou_cov$value[df_ou_cov$country == cntries[1] & df_ou_cov$indicator == "HTS_TST_POS"]

  pds <- df_psnu_cov %>%
    distinct(period) %>%
    pull()

  rsts <- df_psnu_cov %>%
    distinct(period_type) %>%
    pull()

  pop_grp <- "All"



  cntries %>%
    nth(5) %>%
    map(function(.cntry){

      logger::log_info(.cntry)

      pds %>%
        #nth(1) %>%
        map(function(.pd) {

          logger::log_info(.pd)

          pd_ty <- ifelse(str_detect(.pd, "Q", negate=T), "cumulative", "results")

          val_hts <- df_ou_cov %>%
            filter(
               country == .cntry,
               indicator == "HTS_TST_POS",
               period == .pd,
               period_type == pd_ty,
               pop_group == pop_grp) %>%
            pull(value)

          val_prep <- df_ou_cov %>%
            filter(
              country == .cntry,
              indicator == "PrEP_NEW",
              period == .pd,
              period_type == pd_ty,
              pop_group == pop_grp) %>%
            pull(value)

          viz <- cntry_map(.spdf = spdf_cntries,
                    .data = df_psnu_cov,
                    cntry = .cntry,
                    rep_pd = .pd,
                    rep_value = pd_ty,
                    rep_group = pop_grp) +
            labs(title = paste0(.pd, " - ", rep_agency, "/", .cntry,
                                " HTS_TST_POS & PrEP_NEW Results"),
                 subtitle = paste0(
                   "OU Level Results are HTS_TST_POS = ", comma(val_hts, 1),
                   " & PrEP_NEW = ", comma(val_prep, 1)
                 ))

          si_save(filename = file.path(dir_graphics,
                                       paste0(.cntry, " - ", .pd,
                                              " HTS_TST_POS and PrEP_NEW results - ",
                                              curr_date(), ".png")),
                  plot = viz)

          return(viz)
        })
    })

