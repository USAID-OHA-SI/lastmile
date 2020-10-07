# PURPOSE: Ad Hoc maps to show TX_NN for select countries
# Author: Tim Essam | SI
# Date: 2020-10-01
# Notes: Built on top of BKs code for 04_GAS_OU_ViralLoad_NotCovered.R
# Uses his


# DF net-new request for map
df_nn <- df_psnu %>%
    filter(fiscal_year == rep_fy,
           fundingagency == "USAID",
           operatingunit %in% c("South Africa", "Nigeria", "Mozambique"),
           indicator == "TX_NET_NEW",
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(fiscal_year, operatingunit, psnuuid, psnu, trendscoarse, indicator, fundingagency) %>%
    summarise(across(starts_with("qtr3"), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    reshape_msd(clean = TRUE) %>%
    rename(VLS = val) %>%
    mutate(VLnC = 0,
           TX_nn_cat = if_else(VLS >= 0, "TX_NET_NEW Gains", "TX_NET_NEW Loses"))



caption = ("Source: MSD Q3 2020-09-18,
         Indicator: TX_NET_NEW_results")

max_nn <- max(df_nn$VLS)
min_nn <- min(df_nn$VLS)

nn_map  <- function(df, country) {

    cntry <- {{country}}

    max <- df %>% filter(operatingunit == {{country}}) %>% dplyr::select(VLS) %>% max(abs(.))
    min <- max * -1

    map_viralload(spdf = spdf_pepfar,
                  df = df_nn,
                  vl_variable = "VLS",
                  cntry = cntry,
                  terr_raster = terr,
                  agency = F) +
        #facet_wrap(~TX_nn_cat, nrow = 2) +
        scale_fill_stepsn(
            colours = RColorBrewer::brewer.pal(11, 'Spectral')
            #limits = c(min, max)
        ) +

        labs(caption = caption,
             title = paste0(cntry))

    }


 a <- nn_map(df_nn, "South Africa")
#     ggsave(file.path(dir_graphics, "SOUTH_AFRICA_FY20Q3_TX_NN_map.png"),
#         plot = last_plot(), scale = 1.2, dpi = 400,
#         width = 10, height = 7, units = "in")



 b <- nn_map(df_nn, "Mozambique")
#     ggsave(file.path(dir_graphics, "Mozambique_FY20Q3_TX_NN_map.png"),
#            plot = last_plot(), scale = 1.2, dpi = 400,
#            width = 10, height = 7, units = "in")


 c <- nn_map(df_nn, "Nigeria")
#     ggsave(file.path(dir_graphics, "Nigeria_FY20Q3_TX_NN_map.png"),
#            plot = last_plot(), scale = 1.2, dpi = 400,
#            width = 10, height = 7, units = "in")

(a + b + c) +
     ggsave(file.path(dir_graphics, "ZAF_MZB_NGA_FY20Q3_TX_NN_map.png"),
            plot = last_plot(), scale = 1.2, dpi = 400,
            width = 10, height = 7, units = "in")


map_viralload(spdf = spdf_pepfar,
              df = df_nn,
              vl_variable = "VLS",
              cntry = "Mozambique",
              terr_raster = terr,
              agency = F) +
    scale_fill_viridis_c() +
    labs(caption = caption,
         title = "Mozambique")

map_viralload(spdf = spdf_pepfar,
              df = df_nn,
              vl_variable = "VLS",
              cntry = "Nigeria",
              terr_raster = terr,
              agency = F) +
    scale_fill_viridis_c() +
    labs(caption = caption,
         title = "Nigeria")

































map_viralload(spdf = spdf_pepfar,
              df = df_vl,
              vl_variable = "VLS",
              cntry = cname,
              terr_raster = terr)


colors = c("#A85168", "#F5ED81", "#5D96A8")



## Test Individual VL maps

cname <- "Nigeria"
cname <- "Kenya"
cname <- "Ukraine" # Nah
cname <- "Lesotho"
cname <- "Zimbabwe"
cname <- "Mozambique"



map_viralload(spdf = spdf_pepfar,
              df = df_nn,
              vl_variable = "VLS",
              cntry = cname,
              terr_raster = terr,
              agency = FALSE)





map_viralload(spdf = spdf_pepfar,
              df = df_vl,
              vl_variable = "VLC",
              cntry = cname,
              terr_raster = terr,
              agency = TRUE)

map_viralload(spdf = spdf_pepfar,
              df = df_vl,
              vl_variable = "VLnC",
              cntry = cname,
              terr_raster = terr,
              agency = FALSE)

map_viralloads(spdf = spdf_pepfar,
               df = df_vl,
               cntry = cname,
               terr_raster = terr,
               agency = FALSE)

# PEDS Test
map_peds_viralloads(spdf = spdf_pepfar,
                    df = df_vl_u15,
                    cntry = cname,
                    terr_raster = terr)