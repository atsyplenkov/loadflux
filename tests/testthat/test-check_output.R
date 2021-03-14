context("check-output")
library(testthat)
library(loadflux)

test_that("hydro_events() returns a data frame", {
  data(djan17)
  output_table <- hydro_events(dataframe = djan17, q = discharge, datetime = time, window = 21)
  expect_is(output_table, "data.frame")
})
