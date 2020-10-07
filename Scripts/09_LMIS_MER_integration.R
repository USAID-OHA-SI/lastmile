# Purpose: Create a map showing where LMIS data matches to MER
# Author : TE, BK, JD
# Date: 2020-09-24


# GLOBALS -----------------------------------------------------------------

    library(gisr)
    library(glitr)
    library(tidyverse)
    library(readxl)
    library(sf)
    library(raster)
    library(extrafont)


# LOAD AND PLOT -----------------------------------------------------------

    df <- read_csv(file.path(dir_data, "LMIS_txcurr_share_coords.csv")) %>%
        mutate(lmis = if_else(no_lmis_match == "lmis site", si_orange, grey40k))
    glimpse(df)



    lmis_map <- function(ou) {

        terrain <- terrain_map({{ou}}, terr_path = dir_terr)

        admin1 <-

        map <- terrain +
            geom_point(data = df %>% filter(operatingunit == {{ou}}),
                       aes(y = latitude, x = longitude, fill = lmis),
                       shape = 21, color = "white", stroke = 0.25, size = 3) +
            scale_fill_identity() +
            facet_wrap(~lmis) +
            si_style_map() +
            labs(title = "Distribution of LMIS sites with matching MER data (in orange)") +
            theme(legend.position = "none")
        return(map)
    }

    list <- list("Nigeria", "Namibia", "Haiti")

    lmis_map("Haiti") +
        geom_sf_text(aes(label = psnu1))








