context("check-output")
library(testthat)
library(loadflux)

test_that("hydro_events() returns a data frame", {
  data(djan)
  output_table <- hydro_events(dataframe = djan, q = discharge, datetime = time, window = 21)
  expect_is(output_table, "data.frame")
})
