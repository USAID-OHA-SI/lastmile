##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  B.Kagniniwa, G.Sarfaty & T.Essam | USAID
##  PURPOSE: Utilities
##  LICENCE: MIT
##  DATE:    2020-11-19
##


#' @title Clean column data
#'
#' @param .data   MSD Datasets
#' @param colname Name of the column(s)
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


#' @title Clean data from fundingagency column
#'
#' @param .data MSD Datasets
#' @return  Cleaned MSD DataFrame
#'
clean_agency <-
  function(.data) {

    # Check for valid column name
    if (!"fundingagency" %in% names(.data)) {
      cat("\nERROR - fundingagency column is not available as a column.\n")
      return(NULL)
    }

    # clean column data
    .data <- .data %>%
      dplyr::mutate(
        # Convert everyting to upper case
        fundingagency = stringr::str_to_upper(fundingagency),
        # Remove any leading char up to /
        fundingagency = stringr::str_remove(fundingagency, pattern = "^.*\\/")
      )
  }

#' Update Country name
#'
#' @param country country name
#' @return cleaned country name
#'
lookup_country <- function(country) {

  # Symbol [Just for dplyr to deal]
  country <- {{country}}

  # Retrieve the correct name
  country = dplyr::case_when(
    country == "Myanmar" ~ "Burma",
    country == "Czech Republic" ~ "Czechia",
    country == "Ivory Coast" ~ "Cote d'Ivoire",
    country == "Czechia" ~ "Czech Republic",
    country == "Myanmar" ~ "Burma",
    country == "Northern Cyprus" ~ "Cyprus",
    country == "Republic of Serbia" ~ "Serbia",
    country == "Taiwan*" ~ "Taiwan",
    country == "Korea, South" ~ "South Korea",
    country == "Guinea-Bissau" ~ "Guinea Bissau",
    country == "Congo (Kinshasa)" ~ "Democratic Republic of the Congo",
    country == "DRC" ~ "Democratic Republic of the Congo",
    country == "Congo Democratic Republic" ~ "Democratic Republic of the Congo",
    country == "Congo (Brazzaville)" ~ "Republic of the Congo",
    country == "Republic of Congo" ~ "Republic of the Congo",
    country == "East Timor" ~ "Timor-Leste",
    country == "The Bahamas" ~ "Bahamas",
    country == "United Republic of Tanzania" ~ "Tanzania",
    country == "United States of America" ~ "United States",
    country == "Swaziland" ~ "Eswatini",
    country == "S. Sudan" ~ "South Sudan",
    country == "Saint Kitts and Nevis" ~ "Saint Kitts & Nevis",
    country == "Antigua and Barbuda" ~ "Antigua & Barbuda",
    country == "Trinidad and Tobago" ~ "Trinidad & Tobago",
    country == "Saint Vincent and The Grenadines" ~ "Saint Vincent & the Grenadines",
    TRUE ~ country
  )

  return(country)
}

#' @title Clean OU/Country names to match PEPFAR Data
#'
#' @param .data MSD Datasets
#' @param colname Column name to be updated
#' @return  Cleaned DataFrame
#'
clean_countries <-
  function(.data, colname = "admin") {

    # Check for valid column name
    if (!colname %in% names(.data)) {
      cat("\nERROR - ",
          Wavelength::paint_red(colname),
          " is not available as a column.\n")

      return(NULL)
    }

    cat("\nUpdating", Wavelength::paint_green(colname), "column ...\n")

    name <- {{colname}}

    #clean column data
    .data %>%
      dplyr::mutate(across(.cols = dplyr::all_of(name),
                           .fns = lookup_country))
  }

spdf %>%
  st_set_geometry(NULL) %>%
  as_tibble() %>%
  clean_countries(colname = "name") %>% View()

#' Extract text
#' @param txt text containing parenthesis
#' @param limits area to extract text from, c("()", "{}", "[]")
#'
extract_text <- function(txt, limits = "()") {

  # Validate limi
  options <- c("()", "[]", "{}")

  if (!limits %in% options) {
    cat("\nERROR - invalid limits option: ",
        Wavelength::paint_red({{limits}}), "\n")

    return(NULL)
  }

  # filter limits
  if ( limits == "()" ) {
    limits <- "\\(.*\\)"
    content <- "(?<=\\().*(?=\\))"
  }
  else if ( limits == "[]" ) {
    limits <- "\\[.*\\]"
    content <- "(?<=\\[).*(?=\\])"
  }
  else if ( limits == "{}" ) {
    limits <- "\\{.*\\}"
    content <- "(?<=\\{).*(?=\\})"
  }

  # Extract text with limits
  txt <- case_when(
    str_detect(txt, limits) ~ str_extract(txt, content),
    TRUE ~ txt
  )

  return(txt)
}

# extract_text("Hello (World)")
# extract_text("Hello [Worldddd]", limits = "[]")
# extract_text("Hello {Worldzzz}", limits = "{}")
# extract_text(c("Hello (World)", "Hello (Space)"))
# extract_text(c("Hello {World)", "Hello (Space}"))

#' TODO - Extract text within parenthesis
#'
unpack_parenthesis <-
  function(.data, colname) {

    name <- {{colname}}

    # Extract text within parenthesis
    .data %>%
      mutate(across(all_of(colname), extract_text(name)))
  }





