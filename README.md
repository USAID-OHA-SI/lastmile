# lastmile

Last mile analytics and Geo-anything
Where are areas (pockets) with excellent and/or poor results?

## Use case - Mozambique Spatial Distribution of OVC_SERV, HIVSTAT_POS and TX_CURR 

1. Achievements rates for OVC_TX_CURR (Age < 20)

```{r}

library(here)
library(tidyverse)
library(readxl)
library(sf)
library(gisr)
library(glitr)
library(patchwork)

# Glabal Variables

country = "Mozambique"
file_psnu_txt <- "MER_Structured_Datasets_PSNU_IM_FY18-20_20200626_v2_1_Mozambique"
file_districts <- "Mozambique_PROD_5_District_DistrictLsib_2020_Feb.shp"

# Data

## Geo - Moz PSNUs Boundaries
moz_districts <- list.files(
        path = here("../GEODATA", "PEPFAR"),
        pattern = file_districts,
        recursive = TRUE,
        full.names = TRUE
    ) %>%
    unlist() %>%
    first() %>%
    read_sf()
    
## MER PSNU x IMs
moz_psnu <- read_rds(here("../MERDATA", paste0(file_psnu_txt, ".rds")))

# VIZ - Produce a Spatial Distribution map

##Combine Achievements + Map
spdist_ovc_tx_curr(country = country,
                   fy = 2020,
                   df_psnu = moz_psnu,
                   geo_psnu = moz_districts,
                   terr_path = "../GEODATA/RASTER")
                   
```


2. Results of OVC_HIVSTAT_POS


```{r}

library(here)
library(tidyverse)
library(readxl)
library(sf)
library(gisr)
library(glitr)
library(patchwork)

# Glabal Variables

country = "Mozambique"
file_psnu_txt <- "MER_Structured_Datasets_PSNU_IM_FY18-20_20200626_v2_1_Mozambique"
file_districts <- "Mozambique_PROD_5_District_DistrictLsib_2020_Feb.shp"

# Data

## Geo - Moz PSNUs Boundaries
moz_districts <- list.files(
        path = here("../GEODATA", "PEPFAR"),
        pattern = file_districts,
        recursive = TRUE,
        full.names = TRUE
    ) %>%
    unlist() %>%
    first() %>%
    read_sf()
    
## MER PSNU x IMs
moz_psnu <- read_rds(here("../MERDATA", paste0(file_psnu_txt, ".rds")))

# VIZ - Produce a Spatial Distribution map

## Combine Results + Map
spdist_ovc_hivstat_pos(country = country, 
                        fy = 2020, 
                        df_psnu = moz_psnu, 
                        geo_psnu = moz_districts, 
                        terr_path = "../GEODATA/RASTER")
                   
```
