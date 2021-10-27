library(testthat)
library(loadflux)

test_that("check output classes", {

  output_table <- hydro_events(dataframe = djan,
                               q = discharge,
                               datetime = time,
                               window = 21)

  output_plot <- output_table %>%
    event_plot(q = discharge,
               datetime = time,
               he = he)

  output_ti <- djanturb %>%
    hydro_events(q = discharge,
                 datetime = time,
                 window = 21) %>%
    dplyr::filter(he == 2) %>%
    TI(ssc = discharge,
       datetime = time)

  expect_s3_class(output_plot, c("dygraphs", "htmlwidget"))
  expect_s3_class(output_plot, c("dygraphs", "htmlwidget"))
  expect_type(output_ti, "double")
})
