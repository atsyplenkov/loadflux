library(testthat)
library(loadflux)
library(dplyr)
library(tsibble)
library(fabletools)

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

  output_feat <- djan %>%
    hydro_events(
      q = discharge,
      datetime = time,
      window = 21
    ) %>%
    as_tsibble(
      key = he,
      index = time
    ) %>%
    features(
      time,
      feat_event
    )

  expect_s3_class(output_plot,
                  c("dygraphs", "htmlwidget"))
  expect_s3_class(output_feat,
                  c("tbl_df", "tbl", "data.frame"))
  expect_type(output_ti, "double")
  expect_type(output_shi, "double")
  expect_type(output_ahi, "double")
})


test_that("Non-numeric or missing inputs should error", {
  expect_error(hydro_events("cat"))
  expect_error(hysteresis_plot("cat"))
  expect_error(SHI("cat"))
  expect_error(AHI("cat"))
  expect_error(TI("cat"))
  expect_error(SHI(NA))
  expect_error(AHI(NA))
  expect_error(TI(NA))
  expect_error(event_plot(dataframe = "cat",
                          q = "cat",
                          datetime = "cat"))

  expect_error(SHI(NA))
})
