#' Calculate features of a `tsibble` object in conjunction with [features()]
#'
#' You can calculate a series of summary statistics (features) of a given
#'   variable for a dataset. For example, a three number summary, the minimum,
#'   median, and maximum, can be calculated for a given variable. This is
#'   designed to work with the [features()] function shown in the examples.
#'   Other available features in `loadflux` include:
#'
#'
#' @param x A vector to extract features from.
#' @param ... Further arguments passed to other functions.
#' @name loadflux-features
#' @examples
#'
#' # You can use any of the features `feat_*` in conjunction with `features`
#' # like so:
#' library(dplyr)
#' library(fabletools)
#' library(tsibble)
#'
#' djants <- djan %>%
#'        hydro_events(q = discharge,
#'                     datetime = time,
#'                     window = 21) %>%
#'        as_tsibble(key = he,
#'                   index = time)
#'
#' djants %>%
#'   features(time, # variable you want to explore
#'            feat_event) # the feature summarisation you want to perform
#' @rdname loadflux-features
#' @export
#'
#' @importFrom dplyr "%>%" first last tibble
#' @importFrom rlang abort
#' @importFrom lubridate is.POSIXct

feat_event <- function(x){

  if (!lubridate::is.POSIXct(x)) {
    rlang::abort(paste0(
      "`datetime` must be a vector of date-time objects; not ",
      typeof(x), "."
    ))
  }

  start <- dplyr::first(x)
  end <- dplyr::last(x)
  length <- difftime(end, start, units = "h")

  output <- tibble(
    start = unname(start),
    end = unname(end),
    length = unname(length)
  )

  output
}
