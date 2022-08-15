#' Calculate Aich's Hysteresis Index
#'
#' @description This function calculates Hysteresis Index proposed by
#' \emph{Aich et al.} (2014)
#'
#' @param dataframe data.frame object.
#' @param q numeric, water discharge variable.
#' @param ssc numeric, suspended sediment concentration variable.
#' @param .warn logical, indicating if the warning message should be displayed.
#'
#' @return a numeric value either NA
#'
#' @references Aich V, Zimmermann A, Elsenbeer H. 2014. Quantification and
#' interpretation of suspended-sediment discharge hysteresis patterns: How much
#' data do we need? CATENA 122: 120–129 DOI: 10.1016/j.catena.2014.06.020
#'
#' @export
#'
#' @importFrom dplyr "%>%" enquo select pull filter
#' @importFrom tidyr drop_na
#' @importFrom stats lm coef
#'
#' @example man/examples/AHI_example.R
#'

AHI <- function(dataframe, q, ssc, .warn = TRUE) {

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

  # Some tidyeval
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

  # Normalize dataframe
  tt <- df %>%
    dplyr::mutate(
      x = q / max(q),
      y = ssc / max(ssc)
    )

  # Connect max Q and last sediment sample
  min_max <- tt %>%
    dplyr::filter(x == max(x) | y == last(y))

  mm_lm <- lm(y ~ x, data = min_max)
  slope <- coef(mm_lm)[2]
  inter <- coef(mm_lm)[1]
  line <- dplyr::tibble(
    x = seq(
      from = -inter / slope,
      to = 1,
      length.out = 10^3 * nrow(tt)
    ),
    y = seq(
      from = 0,
      to = slope + inter,
      length.out = 10^3 * nrow(tt)
    )
  )

  # Find the maximum distance to the rising and
  # falling limbs
  euclid_min_d <- function(line, pts) {
    d <- vector(
      mode = "integer",
      length = nrow(pts)
    )
    for (i in seq_len(nrow(pts))) {
      d[i] <- min((abs(abs(pts$x[i]) - abs(line$x))^2 + abs(abs(pts$y[i]) - abs(line$y))^2)^(.5))
    }
    return(d)
  }

  res <- euclid_min_d(line, tt)
  r <- max(res[1:which.max(tt$x)])
  f <- max(res[c((which.max(tt$x) + 1):length(tt$x))])

  # Check if sign should be negative or not
  rising_df <- tt[1:which.max(tt$x), ]
  rising_res <- res[1:which.max(tt$x)]

  rising_x <- rising_df[which.max(rising_res), ]

  line_y <- line %>%
    dplyr::filter(round(x, 3) == round(rising_x$x, 3)) %>%
    dplyr::pull(y) %>%
    mean()

  # falling limb
  falling_df <- tt[c((which.max(tt$x) + 1):length(tt$x)), ]
  falling_res <- res[c((which.max(tt$x) + 1):length(tt$x))]

  falling_x <- falling_df[which.max(falling_res), ]

  falling_line_y <- line %>%
    dplyr::filter(round(x, 3) == round(falling_x$x, 3)) %>%
    dplyr::pull(y) %>%
    mean()


  if (!is.nan(line_y)) {
    if (rising_x$y > line_y) {
      r + f
    } else {
      f - r
    }
  } else {
    if (falling_x$y < falling_line_y) {
      r + f
    } else {
      r - f
    }
  }
}
