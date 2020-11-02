##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa & G.Sarfaty | USAID
##  EDITED:  T. Essam (2020-10-01)
##  PURPOSE: Geo-depiction of VL - % not covered
##  LICENCE: MIT
##  DATE:    2020-09-04

# Libraries
library(tidyverse)
library(vroom)
library(sp)
library(sf)
library(raster)
library(gisr)
library(glitr)
library(glamr)
library(janitor)
library(scales)
library(patchwork)
library(ggrepel)
library(here)
library(ICPIutilities)
library(extrafont)
library(viridis)
library(tidytext)

# REQUIRED -------------------------------------------------------------------------------

## Get Credentials

source("../../surprises/surprise.R")

# GLOBALS -------------------------------------------------------------

## Data & Output folders

dir_data <- "Data"
dir_dataout <- "Dataout"
dir_gis <- "GIS"
dir_graphics <- "Graphics"
dir_geodata <- "../../GEODATA/PEPFAR"
dir_geo <- "../../GEODATA/PEPFAR"
dir_terr <- "../../GEODATA/RASTER"
dir_merdata <- "../../MERDATA"

## Q3 MER Data

psnu_im <- "^MER_.*_PSNU_IM_.*_20200918_.*.zip$"

## Reporting Filters

rep_agency = c("USAID", "HHS/CDC")

rep_fy = 2020
rep_qtr = 3

rep_pd = rep_fy %>%
  as.character() %>%
  str_sub(3, 4) %>%
  paste0("FY", ., "Q", rep_qtr)


# FUNCTIONS ---------------------------------------------------------

## Utility functions
source("./Scripts/00_Geo_Utilities.R")
source("./Scripts/00_VL_Utilities.R")


#' @title Generate plot title
#'
#' @param country country name
#' @param var df variable
#' @return plot caption
#' @examples
#'
get_title <-
  function(var, peds = FALSE,
           country = NULL) {

  title <- toupper({{country}})

  var_name <- toupper({{var}})

  var_label <- ""

  # Get label
  if (var_name == "VLS") {
    var_label <- "Viral Load Suppression"

  } else if (var_name == "VLC") {
    var_label <- "Viral Load Coverage"

  } else if (var_name == "VLNC") {
    var_label <- "Viral Load Not-Covered"

  } else {
    var_label <- "Viral Load"
  }

  # PEDS flag
  if (peds == TRUE) {
    var_label <- paste0(var_label, " (Under 15yo)")
  }

  # Title
  if (!is.null({{country}})) {
    title <- paste0(toupper({{country}}), " - ", var_label)

  } else {
    title <- var_label
  }

  return(title)
}


#' @title Generate Caption for Graphic/Output
#'
#' @param country country name
#' @param var df variable
#' @return plot caption
#'
get_caption <-
  function(country,
           rep_pd = "FY20Q3i",
           var = NULL) {

  caption <- paste0("OHA/SIEI - Data Source: ",
                    {{rep_pd}},
                    " MSD, Missing data in grey,
                      VLC = TX_PVLS / TX_CURR (2 periods prior), Not Covered = 1 - VLC\n",
                    toupper({{country}}))

  if (is.null({{var}})) {
    caption <- paste0(caption,
                      " - Variables mapped: VLS, VLC, VLnC")

  } else {
    caption <- paste0(caption,
                      " - ",
                      "Variable mapped: ",
                      toupper({{var}}))
  }

  caption <- paste0(caption,
                    ", Produced on ",
                    format(Sys.Date(), "%Y%m%d"))

  return(caption)
}


#' @title Generate Graphic / Output filename
#'
#' @param country country name
#' @param var df variable
#' @return plot file name
#'
get_output_name <-
  function(country,
           var = "VLC") {

  name <- paste0("FY20Q3_ViralLoad_",
                 toupper({{var}}),
                 "_",
                 toupper({{country}}),
                 "_",
                 format(Sys.Date(), "%Y%m%d"),
                 ".png"
  )

  return(name)
}


#' @title Map TX
#'
#' @param cname Country name
#' @param save Save output of not
#' return ggplot plot
#'
tx_batch <- function(cname, save = F) {
  country <- {{cname}}

  p1 <- map_generic(
    spdf = spdf_pepfar,
    df = df_tx_bad,
    mapvar = TX_ML_PLP,
    cntry = country,
    terr_raster = terr,
    agency = T,
    facet_rows = 2,
    save = TRUE,
    four_parts = F
  )

  p2 <- tx_graph(
    spdf = spdf_pepfar,
    df_gph = df_tx_bad,
    mapvar = TX_ML_PLP,
    cntry = country
  )

  (p1 + p2) +
    plot_layout(widths = c(1, 1)) +
    plot_annotation(
      caption = paste0(
        "OHA/SIEI - Data Source: MSD ",
        rep_pd,
        "_",
        country,
        "TX_ML Patient Loss Proxy \n",
        "TX_ML_PLP = Number not retained on ART (LTFU, Died, Stopped) /
        (TX_CURR_prev + TX_NEW + TX_RTT) \n",
        "Produced on ",
        Sys.Date(),
        ", Missing data shown in gray on map."
      ),
      title = paste0(str_to_upper(country), " TX_ML Patient Loss Proxy")
    )

  if (save == TRUE) {
    ggsave(
      here("Graphics",
           paste0(
             rep_pd,
             "_",
             "TX_ML_PLP_",
             country,
             "_",
             Sys.Date(),
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
}


# DATA --------------------------------------------------------------

## File path + name
file_psnu_im <- list.files(
    path = dir_merdata,
    pattern = psnu_im,
    recursive = TRUE,
    full.names = TRUE
  ) %>%
  last()

## MER PSNU Data
df_psnu <- vroom(file_psnu_im)

df_psnu %>% glimpse()


## OUs
df_ous <- df_psnu %>%
  distinct(operatingunit, operatingunituid) %>%
  arrange(operatingunit)

df_cntries <- df_psnu %>%
  distinct(operatingunit, countryname) %>%
  arrange(operatingunit, countryname)

## SNU1
df_snus <- df_psnu %>%
  dplyr::select(operatingunit, countryname, snu1, snu1uid) %>%
  distinct(operatingunit, countryname, snu1, snu1uid) %>%
  filter(!is.na(snu1)) %>%
  arrange(operatingunit, countryname, snu1)

## PSNU
df_psnus <- df_psnu %>%
  dplyr::select(operatingunit,
                operatingunituid,
                countryname,
                snu1,
                snu1uid,
                psnu,
                psnuuid) %>%
  distinct(operatingunit, countryname, snu1, snu1uid, psnu, psnuuid) %>%
  arrange(operatingunit, countryname, snu1, psnu)

df_psnus %>% View()



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


## MER Data Munging

## Filter & Summarize
df_vl <- extract_viralload(df_msd = df_psnu,
                           rep_agency = rep_agency,
                           rep_fy = rep_fy,
                           lst_ous = lst_ouuid)


## VL PEDs
df_vl_u15 <- extract_viralload(df_msd = df_psnu,
                               rep_agency = rep_agency,
                               rep_fy = rep_fy,
                               peds = TRUE,
                               lst_ous = lst_ouuid)



#PEDs - EID
df_eid <- extract_eid_viralload(df_msd = df_psnu,
                                rep_agency = rep_agency,
                                rep_fy = rep_fy)


# TX_ML -- aggregate bad events then divide by TX_CURR_lagged_1
# Numerator: Number of patients not retained on ART (LTFU, Died, Stopped)
# Denominator: TX_CURR_lag1 + TX_NEW_curr + TX_RTT

df_tx_ml <- df_psnu %>%
  filter(
    fiscal_year == rep_fy,
    fundingagency %in% c("USAID", "HHS/CDC"),
    indicator %in% c("TX_ML"),
    standardizeddisaggregate %in% c("Age/Sex/ARTNoContactReason/HIVStatus"),
    operatingunituid %in% lst_ouuid,
    typemilitary == 'N'
  ) %>%
  mutate(
    fundingagency = if_else(fundingagency == "HHS/CDC", "CDC", fundingagency),
    otherdisaggregate = str_remove(otherdisaggregate, "No Contact Outcome - "),
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
  dplyr::select(-(targets:qtr4), TX_ML_bad = cumulative,-indicator,-tx_badevents)


df_tx <- df_psnu %>%
  filter(
    fiscal_year == rep_fy,
    fundingagency %in% c("USAID", "HHS/CDC"),
    indicator %in% c("TX_CURR", "TX_NEW", "TX_RTT"),
    standardizeddisaggregate %in% c("Total Numerator")
  ) %>%
  mutate(
    fundingagency = if_else(fundingagency == "HHS/CDC", "CDC", fundingagency),
    operatingunituid %in% lst_ouuid
  ) %>%
  group_by(fiscal_year,
           operatingunit,
           psnuuid,
           psnu,
           indicator,
           fundingagency) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  reshape_msd() %>%
  filter(str_detect(period, "q2|q3")) %>%
  filter(period == "fy2020q3" |
           (period == "fy2020q2" & indicator == "TX_CURR")) %>%
  mutate(period = case_when(period == "fy2020q2" ~ "Q2",
                            period == "fy2020q3" ~ "Q3")) %>%
  pivot_wider(names_from = c(indicator, period),
              values_from = val) %>%
  rowwise() %>%
  mutate(TX_BADEVENTS_DENOM = sum(TX_CURR_Q2, TX_NEW_Q3, TX_RTT_Q3, na.rm = T))


df_tx_bad <-
  df_tx %>%
  left_join(df_tx_ml,
            by = c("operatingunit", "psnuuid", "psnu", "fundingagency")) %>%
  mutate(TX_ML_PLP = round((TX_ML_bad / TX_BADEVENTS_DENOM), digits = 3))

# Test calculations/results
df_tx_bad %>%
  filter(operatingunit == "Zambia") %>%
  group_by(psnu) %>%
  summarise(sum = (TX_ML_PLP))

## PEPFAR OU/Countries

ous <-
  identify_ouuids(username = user, password = mypwd(key)) %>%
  as_tibble() %>%
  rename(countryname = country)


## Geodata

## Terrain Raster
terr <- get_raster(terr_path = dir_terr)

## GEO

spdf_pepfar <- build_spdf(df_psnu = df_psnu)

spdf_pepfar %>%
  st_set_geometry(NULL) %>%
  prinf()


# VIZ --------------------------------------

## What are these orgs?

spdf_pepfar %>%
  dplyr::filter(is.na(type)) %>%
  dplyr::select(uid) %>%
  plot()

## Geo-units

## OUs [Regional + bilateral] => 28
spdf_pepfar %>%
  dplyr::filter(!is.na(countryname), type == "OU") %>%
  ggplot() +
  geom_sf(
    aes(fill = countryname),
    colour = "white",
    size = .5,
    show.legend = F
  ) +
  si_style_map()


## Countries [Members of Regional OUs, also used as SNU1] => 44
spdf_pepfar %>%
  filter(!is.na(countryname), type == "Country") %>%
  ggplot() +
  geom_sf(
    aes(fill = operatingunit),
    colour = "white",
    size = .5,
    show.legend = F
  ) +
  si_style_map()


## Individual country maps
spdf_pepfar %>%
  st_set_geometry(NULL) %>%
  filter(type == "OU") %>%
  pull(uid) %>%
  first() %>%
  map(.x, .f = ~ map_org(spdf_pepfar, org_uid = .x, title = .x))


## Individual country basemaps => Ressource Intensive
spdf_pepfar %>%
  st_set_geometry(NULL) %>%
  filter(type == "OU") %>%
  pull(operatingunit) %>%
  nth(8) %>%                  # This is a test for Cote d'Ivoire
  map(.x, .f = ~ get_basemap(
        spdf = spdf_pepfar,
        cntry = .x,
        terr_raster = terr
      ))


## Test Individual VL maps

cname <- "Nigeria"
# cname <- "Kenya"
# cname <- "Ukraine" # Nah
# cname <- "Lesotho"
# cname <- "Zambia"
# cname <- "Botswana"

map_viralload(
  spdf = spdf_pepfar,
  df = df_vl,
  vl_variable = "VLS",
  cntry = cname,
  terr_raster = terr,
  agency = TRUE,
  facet_rows = 2
)

map_viralload(
  spdf = spdf_pepfar,
  df = df_vl,
  vl_variable = "VLC",
  cntry = cname,
  terr_raster = terr,
  agency = T
)

map_viralload(
  spdf = spdf_pepfar,
  df = df_vl,
  vl_variable = "VLnC",
  cntry = cname,
  terr_raster = terr
)

map_viralloads(
  spdf = spdf_pepfar,
  df = df_vl,
  cntry = cname,
  terr_raster = terr,
  facet_rows = 2
)

# PEDS Test
map_peds_viralloads(
  spdf = spdf_pepfar,
  df = df_vl_u15,
  cntry = cname,
  terr_raster = terr,
  agency = T,
  facet_rows = 2,
  save = TRUE
)

# Generic Test

map_generic(
  spdf = spdf_pepfar,
  df = df_eid,
  mapvar = eid_cov_under2,
  cntry = cname,
  terr_raster = terr,
  agency = T,
  facet_rows = 2,
  save = TRUE,
  four_parts = F
)


# Test VLc + EID combined
map_vlc_eid(
  spdf = spdf_pepfar,
  df = df_vl_u15,
  vl_variable = "VLC",
  cntry = cname,
  terr_raster = terr,
  df2 = df_eid,
  mapvar = eid_cov_under2,
  save_all = T
)

# Test TX_ML_PLP map

map_generic(
  spdf = spdf_pepfar,
  df = df_tx_bad,
  mapvar = TX_ML_PLP,
  cntry = cname,
  terr_raster = terr,
  agency = T,
  facet_rows = 2,
  save = TRUE,
  four_parts = F
)



# BATCH VIZ ---------------------------------------------------------------


## Batch mapping

map_ous <- df_vl %>%
  filter(!str_detect(operatingunit, " Region$"),
         !operatingunit %in% c("none")) %>%
  distinct(operatingunit) %>%
  pull()


## VLS
map_ous %>%
  nth(2) %>%
  map(.x, .f = ~ map_viralload(
      spdf = spdf_pepfar,
      df = df_vl_usaid ,
      vl_variable = "VLS",
      cntry = .x,
      terr_raster = terr,
      save = TRUE
    )
  )

## VLC
map_ous %>%
  map(.x, .f = ~ map_viralload(
      spdf = spdf_pepfar,
      df = df_vl_usaid,
      vl_variable = "VLC",
      cntry = .x,
      terr_raster = terr,
      save = TRUE
    )
  )

## VLnC
map_ous %>%
  nth(11) %>%
  map(.x, .f = ~ map_viralload(
      spdf = spdf_pepfar,
      df = df_vl_usaid,
      vl_variable = "VLnC",
      cntry = .x,
      terr_raster = terr,
      save = TRUE
    )
  )


## ALL
map_ous %>%
  map(.x, .f = ~ map_viralloads(
      spdf = spdf_pepfar,
      df = df_vl,
      cntry = .x,
      terr_raster = terr,
      save = TRUE,
      agency = TRUE,
      facet_rows = 2
    )
  )

## PEDS ALL
map_ous %>%
  map(.x, .f = ~ map_peds_viralloads(
      spdf = spdf_pepfar,
      df = df_vl_u15,
      cntry = .x,
      terr_raster = terr,
      save = TRUE
    )
  )


## VL + EID Coverage
map_ous <- df_eid %>%
  filter(!str_detect(operatingunit, " Region$"),
         !operatingunit %in% c("none")) %>%
  distinct(operatingunit) %>%
  pull()


# Batch
map_ous %>%
  nth(13) %>%
  map(.x, .f = ~ map_vlc_eid(
      spdf = spdf_pepfar,
      df = df_vl_u15,
      vl_variable = "VLC",
      cntry = .x,
      terr_raster = terr,
      df2 = df_eid,
      mapvar = eid_cov_under2,
      save_all = T
    )
  )


# Dot plot / bar graph to go with map
tx_graph(spdf_pepfar, df_tx_bad, TX_ML_PLP, "Kenya")

# Batch Patient Loss Proxy Maps + graphs
tx_batch(cname = "Nigeria", save = TRUE)








## END ##
