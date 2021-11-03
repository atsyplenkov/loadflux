library(testthat)
library(loadflux)
library(dplyr)

test_that("check output classes", {
  output_table <- hydro_events(
    dataframe = djan,
    q = discharge,
    datetime = time,
    window = 21
  )

  output_plot <- output_table %>%
    event_plot(
      q = discharge,
      datetime = time,
      he = he
    )

  output_ti <- djanturb %>%
    hydro_events(
      q = discharge,
      datetime = time,
      window = 21
    ) %>%
    dplyr::filter(he == 2) %>%
    TI(
      ssc = discharge,
      datetime = time
    )

  output_ahi <- djan %>%
    hydro_events(
      q = discharge,
      datetime = time,
      window = 21
    ) %>%
    dplyr::filter(he == 2) %>%
    AHI(
      q = discharge,
      ssc = SS
    )

  output_shi <- djan %>%
    hydro_events(
      q = discharge,
      datetime = time,
      window = 21
    ) %>%
    dplyr::filter(he == 2) %>%
    SHI(
      q = discharge,
      ssc = SS
    )

  expect_error(hydro_events("cat"))
  expect_error(SHI("cat"))
  expect_error(AHI("cat"))
  expect_error(TI("cat"))
  expect_error(event_plot("cat"))

  expect_s3_class(output_plot, c("dygraphs", "htmlwidget"))
  expect_type(output_ti, "double")
  expect_type(output_shi, "double")
  expect_type(output_ahi, "double")
})
