#' @title Plot Q-SSC hysteresis plot
#'
#' @description This function plots a water discharge (\code{Q}) and suspended
#' sediment concentration (\code{SSC}) relationship.
#'
#' @param dataframe A data set object
#' @param datetime Datetime variable (column in POSIXct format), optional
#' @param q Water discharge variable (or water stage)
#' @param ssc Suspended sediment concentration variable
#' @param base_font_size Font size (numeric, 12 by default)
#' @param legend the position of legends ("none", "left", "right", "bottom",
#' "top", or two-element numeric vector)
#' @param ... Other arguments passed to \code{ggpubr::theme_pubclean}
#'
#' @return A \code{ggplot2} object
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' data(djan)
#' output_table <- hydro_events(
#'   dataframe = djan,
#'   q = discharge,
#'   datetime = time,
#'   window = 21
#' )
#'
#' output_table %>%
#'   filter(he == 2) %>%
#'   hysteresis_plot(q = discharge, ssc = SS)
#' @export
#' @importFrom dplyr "%>%" enquo select pull filter row_number
#' @importFrom tidyr drop_na
#' @importFrom ggplot2 aes ggplot labs theme geom_point geom_line
#' @importFrom ggplot2 arrow geom_path element_blank unit

hysteresis_plot <- function(dataframe,
                            datetime,
                            q,
                            ssc,
                            base_font_size = 12,
                            legend = "bottom", ...) {

  stopifnot("Table must be of class 'data.frame'" = "data.frame" %in% class(dataframe))

  if (missing(datetime)) {
    q <- dplyr::enquo(q)
    ssc <- dplyr::enquo(ssc)

    dataframe %>%
      tidyr::drop_na(!!q, !!ssc) %>%
      dplyr::mutate(Limb = ifelse(dplyr::row_number() %in% c(1:which.max(!!q)),
        "Rising limb",
        "Falling limb"
      )) %>%
      ggplot2::ggplot(ggplot2::aes(x = !!q, y = !!ssc)) +
      ggplot2::geom_path(arrow = ggplot2::arrow(
        length = ggplot2::unit(3, "mm"),
        ends = "last"
      )) +
      ggplot2::geom_point(aes(color = Limb),
        size = 2
      ) +
      ggplot2::labs(
        x = expression(italic("Q") * "," ~ m^3 %.% s^-1),
        y = expression(italic("SSC") * "," ~ g %.% m^-3),
        color = ""
      ) +
      ggplot2::theme(
        legend.background = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        legend.position = legend,
        strip.background = ggplot2::element_blank()
      )
  } else {
    q <- dplyr::enquo(q)
    datetime <- dplyr::enquo(datetime)
    ssc <- dplyr::enquo(ssc)

    dataframe %>%
      dplyr::arrange(!!datetime) %>%
      tidyr::drop_na(!!q, !!ssc) %>%
      dplyr::mutate(Limb = ifelse(dplyr::row_number() %in% c(1:which.max(!!q)),
        "Rising limb", "Falling limb"
      )) %>%
      ggplot2::ggplot(ggplot2::aes(x = !!q, y = !!ssc)) +
      ggplot2::geom_path(arrow = ggplot2::arrow(
        length = unit(3, "mm"),
        ends = "last"
      )) +
      ggplot2::geom_point(aes(color = Limb),
        size = 2
      ) +
      ggplot2::labs(
        x = expression(italic("Q") * "," ~ m^3 %.% s^-1),
        y = expression(italic("SSC") * "," ~ g %.% m^-3),
        color = ""
      ) +
      ggplot2::theme(
        legend.background = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        legend.position = legend,
        strip.background = ggplot2::element_blank()
      )
  }
}
