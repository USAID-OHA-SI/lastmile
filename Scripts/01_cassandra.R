## PROJECT:  playing with plhiv
## AUTHOR:   jdavis | USAID
## LICENSE:  MIT
## PURPOSE:  Developing metrics from plhiv size estimates
## Date:     2020-07-22

# LIBRARIES------------------------------------------------

library(tidyverse)
library(readxl)
library(glitr)
library(glamr)
library(vroom)
library(ICPIutilities)

#globals---------------------------------------------------

data_in <- "Data"
data_out <- "Dataout"

#read in/subset--------------------------------------------
## most recent DATIM IMPATT

#   This has PLHIV by fine age/sex/PSNU for "fiscal_year" == 2021
#   This works for the purposes of this exercise as the process
#   for Spectrum meetings in COP20 pushed the esimates forward to
#   Sept 30 2020

raw_plhiv <- vroom(file.path(data_in, "MER_Structured_Datasets_NAT_SUBNAT_FY15-20_20200605_v1_1.txt"))

plhiv <- raw_plhiv %>%
    filter(indicator == "PLHIV",
           standardizeddisaggregate == "Age/Sex/HIVStatus") %>%
    select(operatingunit, snu1, psnu, psnuuid, ageasentered, sex, targets) %>%
    rename(plhiv = targets)

#mer psnu (genie or msd)

mer_raw <- read_msd(file.path(data_in, "Genie-PSNUByIMs-Global-Frozen-2020-07-22.zip"), save_rds = TRUE)

mer <- mer_raw %>%
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Age/Sex/HIVStatus") %>%
    group_by(operatingunit, snu1, psnu, psnuuid, ageasentered, sex) %>%
    summarise_at(vars(cumulative, targets), sum, na.rm = TRUE) %>%
    rename(curr_targets = targets,
           curr_results = cumulative)

# tmp <- mer <- mer_raw %>%
#     filter(indicator == "TX_CURR",
#            standardizeddisaggregate == "Age/Sex/HIVStatus")
#
# tmp <- tmp %>%
#     filter(psnu == "Makoni", ageasentered == "25-29", sex == "Female")

#join/create shares-------------------------------------------

df <- bind_rows(plhiv, mer)

test <- full_join(plhiv, mer)

rm(mer, mer_raw, plhiv, raw_plhiv)

df_share <- df %>%
    group_by(operatingunit, indicator) %>%
    mutate(ou_tot = sum(val, na.rm = TRUE),
           ou_share = round(val / ou_tot, 6)) %>%
    ungroup() %>%
    group_by(psnu, indicator) %>%
    mutate(psnu_tot = sum(val, na.rm = TRUE),
           psnu_share = round(val / psnu_tot, 4))










