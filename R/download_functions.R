#' Generate a download url based on the DDP release key
#'
#' @usage ddp_url(key)
#'
#' @description
#'
#' A helper function to map DDP release keys to download urls
#'
#' @param key A DDP release key (character value)
#'
#' @return A url string (character value)
#'
#' @examples
#'
#' ddp_url("G19")
#' ddp_url("H15")

ddp_url  <- function(key) {
    base_url <- "https://www.federalreserve.gov/datadownload/Output.aspx"
    paste0(base_url, "?rel=", key, "&filetype=zip")
}


#' Generate a DDP zip filename based on the DDP release key
#'
#' @usage ddp_zipfile(key, dir = ".")
#'
#' @description
#'
#' Map DDP release keys to download zip file names
#'
#' @param key A DDP release key (character value)
#' @param dir an optional directory name to include in the path
#'
#' @return A zipfile pathname (character value)
#'
#' @details
#'
#' Since DDP download urls are dynamic and do not include the filename, it is
#' neccessary to specify the filename when downloading from R. This function
#' replicates the file naming convention used by the DDP site.
#'
#' @examples
#'
#' ddp_zipfile("G19")  # FRB_G19.zip
#' ddp_zipfile("H15")  # FRB_H15.zip)

ddp_zipfile <- function(key, dir = ".") {
    zipfile <- paste0("FRB_", key, ".zip")
    if ( dir != "." ) zipfile <- file.path(dir, zipfile)
    zipfile
}

#' Download data from the DDP website
#'
#' @usage
#'
#' download_zip(key, dir = ".", url = NULL, destfile = NULL, ...)
#'
#' @description
#'
#' Given a release key, this function will download the zip file for the
#' corresponding statistical release from the DDP website. Alternatively,
#' provide a url and/or destination file.
#'
#' @param key A valid DDP release key. If not specified, a valid download
#'        \code{url} must be provided
#' @param dir A directory for the output file. The current directory
#'        is the default
#' @param url A url string. If not specified the function will
#'        determine the default download url based on the release \code{key}
#' @param destfile An output file path. If not specified the function
#'        will determine the default filename based on the release \code{key}
#'        and the \code{dir} option
#' @param ... Any additional arguments are passed on to the \code{download.file}
#'        function, which handles the download
#'
#' @return name of the local file that was downloaded
#'
#' @details
#'
#' Data Download Program (DDP) website: \url{https://www.federalreserve.gov/datadownload/}
#'
#' @examples
#'
#' # save FRB_H15.zip in the current directory
#' download_zip(key = "H15")
#'
#' # save FRB_H15.zip in the ./data-raw/ directory
#' download_zip(key = "H15", dir = "data-raw")
#' download_zip(key = "H15", destfile = "data-raw/FRB_H15.zip")
#' myurl <- "https://www.federalreserve.gov/datadownload/Output.aspx?rel=H15&filetype=zip"
#' download_zip(url = myurl, destfile = "data-raw/FRB_H15.zip")
#'
#' @export

download_zip <- function(key, dir = ".", url = NULL, destfile = NULL, ...) {
    if ( is.null(url) ) url <- ddp_url(key)
    if ( is.null(destfile) ) destfile <- ddp_zipfile(key, dir = dir)
    download.file(url = url, destfile = destfile, ...)
    destfile
}

#' Extract SDMX data from DDP zip file
#'
#' @usage
#'
#' extract_xml(zipfile, ...)
#'
#' @description
#'
#' DDP zip files contain multiple xml files. This function will selectively
#' extract the SDMX data file that contains the time series data. By default
#' this file follows the naming convention KEY_data.xml, where KEY is a DDP
#' release key.
#'
#' @param zipfile A file path for a DDP zip file
#' @param ... Additional options for the \code{unzip} function
#'
#' @return name of the file that was extracted
#'
#' @examples
#'
#' # Extract H15_data.xml in current directory
#' extract_xml("FRB_H15.zip")
#'
#' # Extract H15_data.xml in data-raw directory
#' extract_xml("FRB_H15.zip", exdir = "data-raw")
#' extract_xml("FRB_H15.zip", files = "H15_data.xml", exdir = "data-raw")
#'
#' @export

extract_xml <- function(zipfile, ...) {
    args <- list(...)
    if ( "files" %in% names(args) ) {
        unzip(zipfile, ...)
    } else {
        files <- grep("_data\\.xml$", unzip(zipfile, list = TRUE)$Name, value = TRUE)
        unzip(zipfile, files = files, ...)
    }
    files
}
