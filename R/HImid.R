#' Calculate Lawler's Mid Hysteresis Index
#'
#' @description This function calculates Hysteresis Index
#' proposed by \emph{Lawler et al.} (2006)
#'
#' @param dataframe data.frame object, containing water discharge
#' (`q`) and suspended sediment concentration (`ssc`) columns
#' @param q numeric, water discharge variable.
#' @param ssc numeric, suspended sediment concentration variable.
#' @param .warn logical, indicating if the warning message
#' should be displayed.
#'
#' @return a numeric value either NA
#'
#' @references Lawler DM, Petts GE, Foster IDL, Harper S. 2006.
#' Turbidity dynamics during spring storm events in an urban
#' headwater river system: The Upper Tame, West Midlands, UK.
#' Science of The Total Environment 360 (1): 109–126
#' DOI: 10.1016/j.scitotenv.2005.08.032
#'
#' @export
#'
#' @importFrom dplyr "%>%" enquo select pull filter
#' @importFrom tidyr drop_na
#' @importFrom stats approx
#'
#' @example man/examples/HImid_example.R
#'
HImid <- function(dataframe,
                  q,
                  ssc,
                  .warn = TRUE) {

  # Some check
  stopifnot(
    "Input must be data frame" =
      is.data.frame(dataframe)
  )

  if (any(is.na(dataframe)) & .warn) {
    warning("NAs dropped",
            call. = FALSE
    )
  }

  q <- dplyr::enquo(q)
  ssc <- dplyr::enquo(ssc)

  df <- dataframe %>%
    tidyr::drop_na(!!q, !!ssc) %>%
    dplyr::select(q = !!q, ssc = !!ssc)

  # Additional checks
  stopifnot(
    "Discharge (q) must be numeric" =
      is.numeric(df$q)
  )
  stopifnot(
    "Suspended sediment concentration (ssc) must be numeric" =
      is.numeric(df$ssc)
  )

  mid <- function(x)  0.5*(max(x, na.rm = T) - min(x, na.rm = T)) + min(x, na.rm = T)

  target <- mid(df$q)
  idx <- which(abs(diff(sign(df$q-target)))>0)

  f <- function(i, target) stats::approx(c(df$q[i], df$q[i+1]),
                                         c(df$ssc[i], df$ssc[i+1]),
                                         xout=target)

  yp <- sapply(idx, f, target=target)
  yp <- as.data.frame(yp[-1,])

  if (dim(yp)[2] == 1){

    return(NA_real_)

  } else {

    colnames(yp) <- c("s1", "s2")

    if (yp$s1 < yp$s2){

      h <- -1/(yp$s1/yp$s2) + 1

    } else {
      h <- (yp$s1/yp$s2) - 1
    }

    return(h)

  }

}

