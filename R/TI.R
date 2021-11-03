#' @title Calculate Turbidity Index (TI)
#'
#' @description This function calculates Turbidity Index (TI)
#' following the description from \emph{Chalov & Tsyplenkov} (2020)
#'
#' @param dataframe A data set object
#' @param ssc Turbidity variable
#' @param datetime a vector of date-time objects
#' @param round_time unit a character string specifying a time unit or a multiple of a unit
#' to be rounded to. Valid base units are `second`, `minute`, `hour`, `day`,
#' `week`, `month`, `bimonth`, `quarter`, `season`, `halfyear` and
#' `year`. See \code{lubridate::round_date}
#'
#' @return A numeric variable
#'
#' @author Anatoly Tsyplenkov and Sergey Chalov
#'
#' @references Chalov SR, Tsyplenkov AS. 2020. Influence of macroturbulence on the dynamics of river water turbidity. Vestnik Moskovskogo universiteta. Seriya 5, Geografiya 0 (3): 34–46 (In Russ.)
#'
#' @examples
#' library(dplyr)
#' data(djanturb)
#' output_table <- hydro_events(
#'   dataframe = djanturb,
#'   q = discharge,
#'   datetime = time,
#'   window = 21
#' )
#'
#' output_table %>%
#'   filter(he == 2) %>%
#'   TI(
#'     ssc = discharge,
#'     datetime = time
#'   )
#' @export
#' @importFrom dplyr "%>%" enquo select
#' @importFrom tidyr drop_na
#' @importFrom rlang abort
#' @importFrom lubridate is.POSIXct round_date

TI <- function(dataframe,
               ssc,
               datetime, # a vector of date-time objects
               round_time = "hour") {
  if (!is.data.frame(dataframe)) {
    rlang::abort(paste0(
      "`dataframe` must be a data.frame; not ",
      typeof(dataframe), "."
    ))
  }

  datetime <- dplyr::enquo(datetime)
  ssc <- dplyr::enquo(ssc)

  df <- dataframe %>%
    tidyr::drop_na(!!datetime, !!ssc) %>%
    dplyr::select(datetime = !!datetime, ssc = !!ssc)

  if (!lubridate::is.POSIXct(df$datetime)) {
    rlang::abort(paste0(
      "`datetime` must be a vector of date-time objects; not ",
      typeof(df$datetime), "."
    ))
  }

  if (!is.numeric(df$ssc)) {
    rlang::abort(paste0(
      "`ssc` must be be a numeric vector; not ",
      typeof(df$ssc), "."
    ))
  }

  df %>%
    dplyr::mutate(hour = lubridate::round_date(datetime,
      unit = round_time
    )) %>%
    dplyr::mutate(ntu_dif = max(ssc) - min(ssc)) %>%
    dplyr::group_by(hour) %>%
    dplyr::summarise(
      delta = max(ssc) - min(ssc),
      ntu_dif = mean(ntu_dif)
    ) %>%
    dplyr::mutate(TI = delta / ntu_dif) %>%
    dplyr::ungroup() %>%
    dplyr::pull(TI) %>%
    mean()
}
