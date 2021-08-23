##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: Notes for quarterly updates
##  LICENCE: MIT
##  DATE:    2021-08-23

# Reporting periods ----

  rep_fy <- 2021

  rep_qtr <- 3

  rep_fy2 <- rep_fy %>%
    as.character() %>%
    str_sub(3,4) %>%
    paste0("FY", .)

  rep_fys <- c(rep_fy - 1, rep_fy)

  rep_fys2 <- rep_fys %>%
    as.character() %>%
    str_sub(3,4) %>%
    paste0("FY", .)

  rep_pd <- rep_fy %>%
    as.character() %>%
    str_sub(3,4) %>%
    paste0("FY", ., "Q", rep_qtr)

  rep_ref_pd <- rep_fys %>%
    first() %>%
    str_sub(3,4) %>%
    paste0("FY", ., "Q4")

  rep_init_pd <- rep_fys %>%
    first() %>%
    str_sub(3,4) %>%
    paste0("FY", ., "Q1")

  rep_pds <- c(rep_ref_pd, rep_pd)
  rep_pds2 <- c(rep_init_pd, rep_pd)

# Source files by Tech Area