#' @title Splitting to hydrological events
#'
#' @description The main aim of this function is to split your dataset
#' into hydrological events based on the local minimum method
#'
#' @param dataframe A data set object
#' @param q Water discharge variable (or water stage)
#' @param datetime Datetime variable (column in POSIXct format)
#' @param window Indicate time period to search for a local minimum (in hours)
#'
#' @return A data frame object with a hydrological events column \code{he}
#' added
#' @examples
#' data(djan)
#' output_table <- hydro_events(dataframe = djan, q = discharge, datetime = time, window = 21)
#' @export
#' @importFrom dplyr "%>%"
#' @importFrom zoo "na.locf"

hydro_events <- function(dataframe,
                         q,
                         datetime,
                         window = 1) {

  q <- deparse(substitute(q))
  datetime <- deparse(substitute(datetime))

  locmin <- function(x, y, window = 1) {

    timestep <- as.double(signif(difftime(head(y)[5],
                                          head(y)[4],
                                          units = "hours"), 4))

    N2star <- round(window / timestep)
    N2star <- ifelse(N2star %% 2 == 0, N2star  + 1, N2star)
    Nobs <- length(x)
    Ngrp <- ceiling(Nobs / N2star)
    Nfil <- (N2star - 1L) / 2L
    Mid <- as.integer((N2star) / 2)
    LocMin <- sapply(seq(N2star, Nobs), function(i)
      min(x[seq(i - N2star + 1L, i)]) == x[i - Mid]
    )
    LocMin <- c(rep(FALSE, Nfil), LocMin, rep(FALSE, Nfil))
    return(LocMin)

  }

  dataframe$q_int <- zoo::na.approx(dataframe[[q]], rule = 2)
  dataframe$LocMin <- locmin(x = dataframe$q_int,
                             y = dataframe[[datetime]],
                             window = window)
  dataframe$test <- ifelse(dataframe$LocMin == FALSE, NA_integer_, 1)

  # Remove multiple local minimums
  dataframe$test <- lapply(seq(1, length(dataframe$test)), function(i)
    dataframe$test[i - 1L] == dataframe$test[i])

  dataframe$LocMin[dataframe$test == T] <- FALSE

  # Name the hydrological events
  dataframe$he <- NA
  dataframe$he[dataframe$LocMin == T] <- 2:(length(dataframe$he[dataframe$LocMin == T])+1)
  dataframe$he[1] <- 1

  # Fill the gaps of he's
  dataframe$he <- zoo::na.locf(dataframe$he)
  # dataframe$he <- as.factor(dataframe$he)

  dataframe <- dplyr::select(dataframe,
                             he,
                             dplyr::everything(),
                             -LocMin, -test, -q_int)
  dataframe <- dplyr::as_tibble(dataframe)

  return(dataframe)
}
