# PURPOSE: Ad Hoc maps to show TX_NN for select countries
# Author: Tim Essam | SI
# Date: 2020-10-01
# Update: 2020-11-24
# Notes: Built on top of BKs code for 04_GAS_OU_ViralLoad_NotCovered.R
# Uses his
# BK: Output updated with Q4 Data and Re-used for:
# "2020.11.23 SIEI Contributions to November POB:


# DF net-new request for map
df_nn <- df_psnu %>%
    filter(fiscal_year == 2020,
           fundingagency == "USAID",
           operatingunit %in% c("South Africa", "Nigeria", "Mozambique"),
           indicator == "TX_NET_NEW",
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(fiscal_year, operatingunit, psnuuid, psnu,
             trendscoarse, indicator, fundingagency) %>%
    summarise(across(starts_with("qtr4"), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    reshape_msd(clean = TRUE) %>%
    rename(VLS = val) %>%
    mutate(VLnC = 0,
           TX_nn_cat = if_else(VLS >= 0, "TX_NET_NEW Gains", "TX_NET_NEW Loses"))


# df_nn %>%
#     filter(operatingunit == "Nigeria") %>%
#     dplyr::select(-VLnC) %>%
#     rename(q3_results = VLS) %>%
#     write_csv(path = "./Dataout/Nigeria_FY20Q3_TX_NET_NEW.csv")
#
# df_nn %>%
#     filter(operatingunit == "Nigeria", VLS < 0) %>%
#     dplyr::select(-VLnC) %>%
#     rename(q3_results = VLS) %>%
#     write_csv(path = "./Dataout/Nigeria_FY20Q3_TX_NET_NEW_lt_zero.csv")

caption = paste0("Data Source: FY20Q4i MSD, ",
                 "Indicator: TX_NET_NEW_results\n",
                 "OHA/SIEI - Produced on ", format(Sys.Date(), "%Y%m%d"),
                 " - South Africa, Mozambique, Nigeria")

max_nn <- max(df_nn$VLS)
min_nn <- min(df_nn$VLS)



nn_map  <- function(df, country) {

    cntry <- {{country}}

    max <- df %>%
        filter(operatingunit == {{country}}) %>%
        dplyr::select(VLS) %>%
        max(abs(.))

    min <- max * -1

    # Map
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


nn_map2 <- function(spdf, terr, df, country,
                    breaks, labels, cols = NULL) {

    cntry <- {{country}}
    cols <- {{cols}}

    # Colors
    if (is.null(cols)) {
        cols = c("#9E0142", "#F46D43", "#FEE08B", "#ABDDA4", "#66C2A5", "#3288BD")
    }

    df <- df %>%
        mutate(nn = cut(VLS,
                        breaks = {{breaks}}
                        ,labels = {{labels}}
                        ))

    # Country boundaries
    df_geo0 <- spdf %>%
        filter(countryname == cntry, type == "OU")

    # PSNU Geo + VL data
    df_geo2 <- spdf %>%
        filter(countryname == cntry, type == "PSNU") %>%
        left_join(df, by = c("uid" = "psnuuid")) %>%
        filter(!is.na(nn))

    # Map
    map <- get_basemap(
            spdf = spdf,
            cntry = cntry,
            terr_raster = terr
        ) +
        geom_sf(
            data = df_geo2,
            aes(fill = nn),
            lwd = .2,
            color = "White",#grey90k,
            alpha = 0.9
        ) +
        geom_sf(
            data = df_geo0,
            colour = grey90k,
            fill = NA,
            size = 1
        ) +
        scale_fill_manual(
            values = cols,
            na.translate = F,
            guide = guide_legend(reverse = T)) +
        labs(title = cntry) +
        si_style_map() +
        theme(
            legend.position = c(.9, .2),
            legend.direction = "vertical",
            legend.text = element_text(hjust = 0),
            legend.key.width = unit(1, "cm")
        )

    return(map)
}

# df_nn %>%
#     filter(operatingunit == "Nigeria") %>%
#     pull(VLS) %>%
#     sort()

RColorBrewer::brewer.pal(11, 'Spectral')
RColorBrewer::brewer.pal(6, 'Spectral')
RColorBrewer::display.brewer.all()

# "#9E0142" "#D53E4F" "#F46D43" "#FDAE61" "#FEE08B"
# "#FFFFBF"
# "#E6F598" "#ABDDA4" "#66C2A5" "#3288BD" "#5E4FA2"

RColorBrewer::brewer.pal(6, 'GnBu')


a <- nn_map2(spdf_pepfar, terr, df_nn, "South Africa",
            breaks = c(-10000, -5000, -1000, 0, 1000, 5000, 10000),
            labels = c("-10,000 to -5,000", "-4,999 to -1,000", "-999 to 0",
                        "1 to 1,000", "1,001 to 5,000", "5,001 to 10,000"),
            cols = RColorBrewer::brewer.pal(6, 'Spectral'))
a

ggsave(file.path(dir_graphics, "SOUTH_AFRICA_FY20Q4_TX_NN_map.png"),
    plot = last_plot(), scale = 1.2, dpi = 400,
    width = 10, height = 7, units = "in")


b <- nn_map2(spdf_pepfar, terr, df_nn, "Mozambique",
             breaks = c(0, 100, 200, 500, 1000, 1500, 2000),
             labels = c("0 to 100", "101 to 200", "201 to 500",
                        "501 to 1,000", "1,001 to 1,500", "1,500 to 2,000"),
             cols = RColorBrewer::brewer.pal(6, 'GnBu')
             )
b

ggsave(file.path(dir_graphics, "Mozambique_FY20Q4_TX_NN_map.png"),
       plot = last_plot(), scale = 1.2, dpi = 400,
       width = 10, height = 7, units = "in")


c <- nn_map2(spdf_pepfar, terr, df_nn, "Nigeria",
            breaks = c(0, 1000, 3000, 5000, 10000, 30000),
            labels = c("0 to 1,000", "1,001 to 3,000", "3,001 to 5,000",
                       "5,001 to 10,000", "10,001 to 23,000"),
            cols = RColorBrewer::brewer.pal(5, 'GnBu'))
c

ggsave(file.path(dir_graphics, "Nigeria_FY20Q4_TX_NN_map.png"),
       plot = last_plot(), scale = 1.2, dpi = 400,
       width = 10, height = 7, units = "in")

(a + b + c) +
    #plot_layout(widths = c(1,1,1)) +
    plot_annotation(caption = caption)


ggsave(file.path(dir_graphics, "ZAF_MZB_NGA_FY20Q4_TX_NN_map.png"),
    plot = last_plot(), scale = 1.2, dpi = 400,
    width = 10, height = 7, units = "in")
































# Other function
map_viralload(spdf = spdf_pepfar,
              df = df_nn,
              vl_variable = "VLS",
              cntry = "Mozambique",
              terr_raster = terr,
              agency = F) +
    scale_fill_viridis_c() +
    labs(
        #caption = caption,
         title = "Mozambique")

map_viralload(spdf = spdf_pepfar,
              df = df_nn,
              vl_variable = "VLS",
              cntry = "Nigeria",
              terr_raster = terr,
              agency = F) +
    scale_fill_viridis_c() +
    labs(
        #caption = caption,
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