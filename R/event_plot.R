#' @title Visualize hydrological events interactively
#'
#' @description This function creates an interactive plot using \code{dygraphs}
#' package
#'
#' @param dataframe A data set object
#' @param q Water discharge variable (or water stage)
#' @param datetime Datetime variable (column in POSIXct format)
#' @param he Hydrological event variable (or other day column)
#' @param ssc Suspended sediment concentration variable (to plot on a
#' second axis)
#' @param ylabel Y-axis label
#' @param y2label Second Y-axis label
#'
#'
#' @return The `event_plot` function returns object of class "dygraphs",
#' "htmlwidget"
#'
#' @examples
#' library(dplyr)
#' data(djan)
#' djan %>%
#'   hydro_events(
#'     q = discharge,
#'     datetime = time,
#'     window = 21
#'   ) %>%
#'   event_plot(q = SS, datetime = time, he = he)
#' @export
#' @importFrom dplyr "%>%" enquo select pull filter
#' @importFrom xts xts

event_plot <- function(dataframe,
                       q,
                       datetime,
                       he,
                       ssc,
                       ylabel = "Water discharge",
                       y2label = "Suspended Sediment Concentration") {
  . <- NULL

  stopifnot("Table must be of class 'data.frame'" = "data.frame" %in% class(dataframe))

  if (missing(he)) {
    if (missing(ssc)) {
      q <- dplyr::enquo(q)
      datetime <- dplyr::enquo(datetime)

      plot <-
        dataframe %>%
        dplyr::arrange(!!datetime) %>%
        dplyr::select(!!q, !!datetime) %>%
        dplyr::mutate(q_int = zoo::na.approx(!!q, rule = 2)) %>%
        xts::xts(
          x = data.frame("Q" = dplyr::pull(., q_int)),
          order.by = dplyr::pull(., !!datetime)
        ) %>%
        dygraphs::dygraph() %>%
        dygraphs::dySeries("Q", label = rlang::as_name(q)) %>%
        dygraphs::dyAxis("y", label = ylabel) # %>%
      # dygraphs::dyOptions(useDataTimezone = TRUE)

      plot
    } else {
      q <- dplyr::enquo(q)
      datetime <- dplyr::enquo(datetime)
      ssc <- dplyr::enquo(ssc)

      plot <-
        dataframe %>%
        dplyr::arrange(!!datetime) %>%
        dplyr::select(!!q, !!datetime, !!ssc) %>%
        dplyr::mutate(q_int = zoo::na.approx(!!q, rule = 2)) %>%
        dplyr::mutate(ssc_int = zoo::na.approx(!!ssc, rule = 2)) %>%
        xts::xts(
          x = data.frame(
            "Q" = dplyr::pull(., q_int),
            "SSC" = dplyr::pull(., ssc_int)
          ),
          order.by = dplyr::pull(., !!datetime)
        ) %>%
        dygraphs::dygraph() %>%
        dygraphs::dySeries("Q",
          label = rlang::as_name(q)
        ) %>%
        dygraphs::dySeries("SSC",
          label = rlang::as_name(ssc),
          axis = "y2"
        ) %>%
        dygraphs::dyAxis("y",
          label = ylabel
        ) %>%
        dygraphs::dyAxis("y2",
          label = y2label,
          independentTicks = TRUE
        ) # %>%
      # dygraphs::dyOptions(useDataTimezone = TRUE)

      plot
    }
  } else {
    if (missing(ssc)) {
      q <- dplyr::enquo(q)
      datetime <- dplyr::enquo(datetime)
      he <- dplyr::enquo(he)

      db_he <-
        dataframe %>%
        dplyr::group_by(!!he) %>%
        dplyr::summarise(
          start = dplyr::first(!!datetime),
          end = dplyr::last(!!datetime),
          .groups = "drop"
        )

      plot <-
        dataframe %>%
        dplyr::arrange(!!datetime) %>%
        dplyr::select(!!q, !!datetime) %>%
        dplyr::mutate(q_int = zoo::na.approx(!!q, rule = 2)) %>%
        xts::xts(
          x = data.frame("Q" = dplyr::pull(., q_int)),
          order.by = dplyr::pull(., !!datetime)
        ) %>%
        dygraphs::dygraph() %>%
        dygraphs::dySeries("Q", label = rlang::as_name(q)) %>%
        dygraphs::dyAxis("y", label = ylabel) # %>%
      # dygraphs::dyOptions(useDataTimezone = TRUE)

      rows_to_plot <- (seq_len(nrow(db_he)))[seq(1, nrow(db_he), 2)]

      for (i in rows_to_plot) {
        plot <- plot %>% dygraphs::dyShading(
          from = db_he$start[i],
          to = db_he$end[i],
          color = "#FFE6E6"
        )
      }

      plot
    } else {
      q <- dplyr::enquo(q)
      datetime <- dplyr::enquo(datetime)
      he <- dplyr::enquo(he)
      ssc <- dplyr::enquo(ssc)

      db_he <-
        dataframe %>%
        dplyr::group_by(!!he) %>%
        dplyr::summarise(
          start = dplyr::first(!!datetime),
          end = dplyr::last(!!datetime),
          .groups = "drop"
        )

      plot <-
        dataframe %>%
        dplyr::arrange(!!datetime) %>%
        dplyr::select(!!q, !!datetime, !!ssc) %>%
        dplyr::mutate(q_int = zoo::na.approx(!!q, rule = 2)) %>%
        dplyr::mutate(ssc_int = zoo::na.approx(!!ssc, rule = 2)) %>%
        xts::xts(
          x = data.frame(
            "Q" = dplyr::pull(., q_int),
            "SSC" = dplyr::pull(., ssc_int)
          ),
          order.by = dplyr::pull(., !!datetime)
        ) %>%
        dygraphs::dygraph() %>%
        dygraphs::dySeries("Q",
          label = rlang::as_name(q)
        ) %>%
        dygraphs::dySeries("SSC",
          label = rlang::as_name(ssc),
          axis = "y2"
        ) %>%
        dygraphs::dyAxis("y",
          label = ylabel
        ) %>%
        dygraphs::dyAxis("y2",
          label = y2label,
          independentTicks = TRUE
        ) # %>%
      # dygraphs::dyOptions(useDataTimezone = TRUE)

      rows_to_plot <- (seq_len(nrow(db_he)))[seq(1, nrow(db_he), 2)]

      for (i in rows_to_plot) {
        plot <- plot %>% dygraphs::dyShading(
          from = db_he$start[i],
          to = db_he$end[i],
          color = "#FFE6E6"
        )
      }

      plot
    }
  }
}
