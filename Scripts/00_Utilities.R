##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa, G.Sarfaty & T.Essam | USAID
##  PURPOSE: Utilities
##  LICENCE: MIT
##  DATE:    2020-11-19
##


#' @title Clean column data
#'
#' @param .data   MSD Datasets
#' @param colname Name of the column
#' @return  Cleaned MSD DataFrame
#'
clean_column <-
  function(.data, colname = "psnu") {

    # Check params
    name <- {{colname}}

    # Check for valid column name
    if (length(setdiff(name,  names(.data))) > 0) {
      cat("\nERROR - One of the column names is unknown: ",
          Wavelength::paint_red({{colname}}), "\n")

      return(NULL)
    }

    # Remove characters
    rmv_tail <- c(
        "District",
        "County",
        "District Municipality",
        "Metropolitan Municipality",
        "Municipality"
      ) %>%
      paste0(" ", ., "$", collapse = "|")

    # Remove characters at the end
    .data <- .data %>%
      mutate_at(.vars = all_of(name), str_remove, pattern = rmv_tail)

    # Remove first 2 leading chars
    rmv_lead <- "^[A-Za-z]{2}[[:space:]]"

    # Remove characters
    .data <- .data %>%
      mutate_at(.vars = all_of(name), str_remove, pattern = rmv_lead)
  }


#' @title Clean PSNU column data
#'
#' @param .data   MSD Datasets
#' @return  Cleaned MSD DataFrame
#'
clean_psnu <-
  function(.data) {

    # Check for valid column name
    if (!"psnu" %in% names(.data)) {
      cat("\nERROR - psnu column is not available as a column.\n")
      return(NULL)
    }

    # Remove extract characters
    .data <- .data %>%
      clean_column(colname = "psnu")
  }


#' @title Clean fundingagency data
#'
#' @param .data   MSD Datasets
#' @return  Cleaned MSD DataFrame
#'
clean_agency <-
  function(.data) {

    # Check for valid column name
    if (!"fundingagency" %in% names(.data)) {
      cat("\nERROR - fundingagency column is not available as a column.\n")
      return(NULL)
    }

    # Remove extract characters
    .data <- .data %>%
      mutate(
        fundingagency = toupper(fundingagency),
        fundingagency = case_when(
          toupper(fundingagency) == "HHS/CDC" ~ "CDC",
          TRUE ~ fundingagency
        )
      )
  }


#' TODO - Extract text within parenthesis
#'
extract_parenthesis <-
  function(.data, colname = "mech_name") {

    #name <- {{colname}}
    name <- dplyr::sym(colname)

    #
    .data %>%
      mutate(
        name = if_else(str_detect(name, "\\)"),
                       str_extract(name, "(?<=\\().*(?=\\))"),
                       name))
  }


