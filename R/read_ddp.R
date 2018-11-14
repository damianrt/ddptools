#' @title Read an SDMX data file downloaded from the FRB DDP page
#'
#' @usage
#'
#' \code{read_ddp(file, pattern = NULL, skip = 0, n = Inf)}
#'
#' @description
#'
#' Read time series data and metadata stored in an SDMX formatted XML file, and
#' return a list of \code{tis} objects. Series metadata are stored as attributes
#' within each \code{tis} object.
#'
#'
#' @param file A filename or a connection to an SDMX formatted file.
#'
#' @param pattern A charcter vector of regular expressions used to filter input
#'        by the \emph{series_name} attribute. Only matching series will be imported.
#'
#' @param skip Number of series to skip starting from the beginning of the file.
#'        If any \code{pattern} filters are provided, only matching series are
#'        counted.
#'
#' @param n Maximum number of series to import from the file. If any \code{pattern}
#'        filters are provided, only matching series are counted.
#'
#' @return A list of \code{tis} objects
#'
#' @details
#'
#' Input files are read sequentially, line-by-line, to avoid storing the
#' entire XML file in memory. With the exception of the tis package, this
#' function is implemented using only built-in R functions.
#'
#'
#' @section Note:
#' This function is intended for use with the SDMX 1.0 formatted data files
#' provided by the Federal Reserve Board (refer to
#' \url{https://www.federalreserve.gov/datadownload/}). SDMX files from other
#' sources may not be processed correctly.
#'
#' @examples
#'
#' library(tis)
#' library(ddptools)
#'
#' # read only the first 5 series
#' db <- read_ddp("G19_data.xml", n = 5)
#'
#' # skip the first 5 series, then read the next 10
#' db <- read_ddp("G19_data.xml", skip = 5, n = 10)
#'
#' # include only matching series_name
#' db <- read_ddp("H15_data.xml", pattern = c("^RIF", "^RIM.*"))
#'
#' # read all data
#' db <- read_ddp("PRATES_data.xml")
#'
#' names(db)
#' attributes(db$RESBMS_N.D)
#' tail(db$RESBMS_N.D)
#'
#' lm(RESBMS_N.D ~ lag(RESBME_N.D), db)
#'
#' arima(db$RESBMS_N.D, c(0, 0, 1))
#'
#' plot(db$RESBMS_N.D)
#'
#' plot(
#'     x    = as.Date(ti(db$RESBMS_N.D)),
#'     y    = db$RESBMS_N.D,
#'     type = "s",
#'     main = attr(db$RESBMS_N.D, "long_description"),
#'     xlab = "Date",
#'     ylab = attr(db$RESBMS_N.D, "unit")
#' )
#'
#' @seealso
#'
#' \link[tis]{tis}
#'
#' @importFrom tis tis
#' @importFrom tis tif
#' @export


read_ddp <- function(file, pattern = NULL, skip = 0, n = Inf) {

    # Read FRB XML Data file
    # Return a list of tis objects

    # Input file connection
    if ( "character" %in% class(file) & file.exists(file) ) {
        con <- file(file, "r")
    } else if ( "connection" %in% class(file) ) {
        con <- file
    } else {
        con <- NULL
    }

    # Filter series by names/patterns
    if ( !is.null(pattern) ) pattern <- as.character(pattern)

    # Number of series to skip
    if ( !is.numeric(skip) ) skip <- abs(as.numeric(skip))

    # Maximum number of series to read
    if ( !is.numeric(n) ) n <- abs(as.numeric(n))


    # lookup to translate frequency identifiers from sdmx freq to tis tif
    freq2tif <- c(
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

    # reference info from statistical standards: https://sdmx.org/?page_id=3215
    valid_obs_status <- c(
        "A" = "Normal",
        "B" = "Break",
        "E" = "Estimated value",
        "F" = "Forecast value",
        "I" = "Imputed value (CCSA definition)",
        "M" = "Missing value",
        "P" = "Provisional value"
    )


    # initialize output object
    db <- list()

    # initialize temp objects
    metadata <- c()
    annotation_type <- NULL
    obsdata <- list()
    matched <- TRUE
    skipped <- 0

    while ( TRUE ) {

        # limit number of series read
        if ( n != Inf & length(db) == n ) break

        # Read one line at a time
        line <- readLines(con, n = 1, warn = FALSE)

        # Last line in file
        if ( length(line) == 0 ) break

        # Start of a new series
        if ( grepl("<kf:Series", line) ) {

            #Example line: <kf:Series CURRENCY="NA" DEBT="TOTAL" FREQ="162" SERIES_NAME="DTFD%YPD.Q" TYPE="DSR" UNIT="Percent" UNIT_MULT="1"  >
            raw <- unlist(strsplit(line, "\\s"))
            raw <- grep('\\S+="[^"]+"', raw, value = TRUE)
            metadata <- gsub('\\S+="([^"]+)"', '\\1', raw)
            names(metadata) <- tolower(gsub('(\\S+)="[^"]+"', '\\1', raw))


            # Selectively process series. Check if names match patterns
            if ( !is.null(pattern) ) {
                matched <- FALSE
                for ( p in pattern ) {
                    matched <- grepl(p, metadata["series_name"])
                    if ( matched ) break
                }
                if ( !matched ) metadata <- c()
            } else {
                matched <- TRUE
            }

            # Discard first n matching series in file, where n = skip argument
            if ( skip > 0 ) {
                if ( matched & skipped <= skip ) skipped <- skipped + 1
            }

        }


        if (  matched & ( skip == 0 | skipped > skip ) ) {

            # Series Annotation
            if ( grepl("<common:AnnotationType", line) ) {
                annotation_type <- tolower(gsub(" +", "_", sub(".*>([^<]+)</.*", "\\1", line)))
            }

            if ( grepl("<common:AnnotationText", line) ) {
                metadata[annotation_type] <- sub(".*>([^<]+)</.*", "\\1", line)
            }

            # Series data
            if ( grepl("<frb:Obs", line) ) {

                #Example line: <frb:Obs OBS_STATUS="A" OBS_VALUE="6.82" TIME_PERIOD="1997-06-30" />
                raw <- unlist(strsplit(line, "\\s"))
                raw <- grep('\\S+="[^"]+"', raw, value = TRUE)
                obs <- gsub('\\S+="([^"]+)"', '\\1', raw)
                names(obs) <- tolower(gsub('(\\S+)="[^"]+"', '\\1', raw))

                for ( key in names(obs) ) {
                    if ( key %in% names(obsdata) ) {
                        obsdata[[key]] <- c(obsdata[[key]], unname(obs[key]))
                    } else {
                        obsdata[[key]] <- unname(obs[key])
                    }
                }
            }

            # End of series
            if ( grepl("</kf:Series", line) ) {

                obsdata$obs_value[ obsdata$obs_status == "ND" ] <- NA
                #obsdata$is_valid <- obsdata$obs_status %in% c("ND", names(valid_obs_status))
                obsdata$is_valid <- !is.na(lookup_obs_status(obsdata$obs_status))

                series <- tis::tis(
                    data  = as.numeric(obsdata$obs_value[ obsdata$is_valid ]),
                    start = as.POSIXct(head(obsdata$time_period, 1)),
                    #tif   = tis::tif(freq2tif[metadata["freq"]])
                    tif   = lookup_tif(metadata["freq"])
                )

                # Add other series attributes to the tis object
                for ( w in names(metadata) ) {
                    if ( w == "series_name" ) next
                    attr(series, which = w) <- unname(metadata[w])
                }

                db[[ metadata["series_name"] ]] <- series

                obsdata <- list()
                metadata <- c()

            }
        }
    }

    close(con)

    db
}
