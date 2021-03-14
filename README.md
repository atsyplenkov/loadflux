
<!-- README.md is generated from README.Rmd. Please edit that file -->

# loadflux <img src='man/figures/logo.svg' align="right" height="139" />

<!-- badges: start -->

[![codecov](https://codecov.io/gh/atsyplenkov/loadflux/branch/master/graph/badge.svg?token=DI1DCJV15D)](https://codecov.io/gh/atsyplenkov/loadflux)
<!-- badges: end -->

The goal of `loadflux` is aimed at comprehensive analysis of the
intra-event suspended sediment dynamics.

## Installation

<!-- CRAN -->

You can install the released version of loadflux from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("loadflux")
```

<!-- CRAN -->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("atsyplenkov/loadflux")
```

## Example

This is a basic example which shows you how to split your series into
hydrological events:

``` r
library(dplyr)
library(loadflux)
## basic example code
data(djan)

djan %>% 
  hydro_events(q = discharge,
              datetime = time,
              window = 21)
#> # A tibble: 2,950 x 4
#>       he time                discharge    SS
#>    <dbl> <dttm>                  <dbl> <dbl>
#>  1     1 2017-06-06 12:00:00     0.778  227.
#>  2     1 2017-06-06 13:00:00     0.778   NA 
#>  3     1 2017-06-06 14:00:00     0.778  224.
#>  4     1 2017-06-06 15:00:00     0.778   NA 
#>  5     1 2017-06-06 16:00:00     0.778  271.
#>  6     1 2017-06-06 17:00:00     0.925   NA 
#>  7     1 2017-06-06 18:00:00     1.07    NA 
#>  8     1 2017-06-06 19:00:00     1.22   388.
#>  9     1 2017-06-06 20:00:00     1.25    NA 
#> 10     1 2017-06-06 21:00:00     0.933   NA 
#> # ... with 2,940 more rows
```

Then you can explore created hydrological events via `event_plot`
function

``` r
plot <- djan %>% 
  hydro_events(q = discharge,
              datetime = time,
              window = 21) %>%
  event_plot(q = SS, datetime = time, he = he)
```
