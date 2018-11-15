#' Convert known SDMX frequency identifiers to tif codes for tis
#'
#' @usage lookup_tif(freq)
#'
#' @description
#'
#' SDMX uses freq codes to describe the frequency of a time series.
#' This function translates freq codes into the tif codes used by
#' the tis package for the same purpose. This information is neccessary
#' to create a valid tis object
#'
#' @param freq A character vector of frequency identifier codes for an SDMX time
#'        series.
#'
#' @return A character vector of tif codes
#'
#' @examples
#'
#' lookup_tif(8)            # daily
#' lookup_tif(c("8", "9")   # daily, business
#' lookup_tif(c(9, -999))   # business, NA
#' lookup_tif()             # return all freq codes in a named vector

lookup_tif <- function(freq = NULL) {

    lookup <- c(
        "8"   = "daily",         # daily
        "9"   = "business",      # business daily (m-f)
        "17"  = "wmonday",       # weekly Monday
        "19"  = "wwednesday",    # weekly Wednesday
        "20"  = "wthursday",     # weekly Thursday
        "21"  = "wfriday",       # weekly friday
        "67"  = "bw1wednesday",  # biweekly (every other) wednesday
        "129" = "monthly",
        "162" = "quarterly",
        "203" = "annual"
    )

    if ( is.null(freq) ) {
        result <- lookup
    } else {
        freq <- as.character(freq)
        result <- ifelse(freq %in% names(lookup), unname(lookup[freq]), NA_character_)
    }

    result
}



#' Lookup descriptions for valid SDMX obs_status codes
#'
#' @usage lookup_obs_status(code, standard = FALSE)
#'
#' @description
#'
#' Map SDMX obs_status to descriptive text labels. Unrecognized codes
#' will be mapped to NA.
#'
#'
#' @param code A character vector of obs_status codes for observations in an
#'        SDMX time series
#' @param standard A logical value. If TRUE, nonstandard DDP codes will be filtered out
#'        (mapped to \code{NA}).
#'
#' @return A character vector of descriptions for recognized obs_status codes
#'
#' @details
#'
#' Valid codes are documented in the SDMX statistical standards:
#' \url{https://sdmx.org/?page_id=3215}. DDP data include additional nonstandard
#' codes. Set \code{standard = TRUE} to exclude these values.
#'
#' @export

lookup_obs_status <- function(code = NULL, standard = FALSE) {

    lookup <- c(
        "A" = "Normal",
        "B" = "Break",
        "E" = "Estimated value",
        "F" = "Forecast value",
        "I" = "Imputed value (CCSA definition)",
        "M" = "Missing value",
        "P" = "Provisional value",
       "ND" = "No Data"
    )

    nonstandard <- c("ND")

    if ( standard == TRUE ) lookup[names(lookup) %in% nonstandard] <- NA_character_

    if ( is.null(code) ) {
        result <- lookup
    } else {
        status <- as.character(code)
        result <- ifelse(code %in% names(lookup), unname(lookup[code]), NA_character_)
    }

    result
}



#' Map DDP release keys to FRB release names
#'
#' @usage lookup_release_name(key)
#'
#' @description
#'
#' Map DDP release keys to release titles. Returns a complete list of known
#' release keys if no input argument is given
#'
#' @param key A character vector of DDP release keys
#'
#' @return A character vector
#'
#' @examples
#'
#' lookup_release("G19")  # Consumer Credit (G.19)
#' lookup_release("H15")  # Selected Interest Rates (H.15)
#'
#' lookup_release(c("PRATES", "H41" ))
#'
#' lookup_release()       # returns a named vector with all releases
#'
#' @export

lookup_release_name <- function(key = NULL) {

    lookup <- c(
        "G19"    = "Consumer Credit (G.19)",
        "G17"    = "Industrial Production and Capacity Utilization (G.17)",
        "H3"     = "Aggregate Reserves of Depository Institution and the Monetary Base (H.3)",
        "H8"     = "Assets and Liabilities of Commercial Banks in the U.S. (H.8)",
        "CHGDEL" = "Charge-off and Delinquency Rates",
        "SLOOS"  = "Senior Loan Officer Opinion Survey on Bank Lending Practices",
        "E2"     = "Survey of Terms of Business Lending (E.2) ",
        "CP"     = "Commercial Paper",
        "G20"    = "Finance Companies (G.20)",
        "SCOOS"  = "Senior Credit Officer Opinion Survey on Dealer Financing Terms",
        "H10"    = "Foreign Exchange Rates (G.5 / H.10)",
        "Z1"     = "Financial Accounts of the United States (Z.1)",
        "G20"    = "Finance Companies (G.20)",
        "FOR"    = "Household Debt Service and Financial Obligations Ratios (FOR)",
        "H15"    = "Selected Interest Rates (H.15)",
        "PRATES" = "Policy Rates",
        "H41"    = "Factors Affecting Reserve Balances (H.4.1)",
        "H6"     = "Money Stock Measures (H.6)"
    )

    if ( is.null(key) ) {
        result <- lookup
    } else {
        key <- as.character(key)
        result <- ifelse(key %in% names(lookup), unname(lookup[key]), NA_character_)
    }

    result
}
