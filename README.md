
<!-- README.md is generated from README.Rmd. Please edit that file -->

# loadflux <img src='man/figures/logo.svg' align="right" height="139" />

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/loadflux)](https://cran.r-project.org/package=loadflux)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6992087.svg)](https://doi.org/10.5281/zenodo.6992087)
[![](http://cranlogs.r-pkg.org/badges/grand-total/loadflux)](https://cran.r-project.org/package=loadflux)
[![codecov](https://codecov.io/gh/atsyplenkov/loadflux/branch/master/graph/badge.svg?token=DI1DCJV15D)](https://app.codecov.io/gh/atsyplenkov/loadflux/)
[![R-CMD-check](https://github.com/atsyplenkov/loadflux/workflows/R-CMD-check/badge.svg)](https://github.com/atsyplenkov/loadflux/actions/)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active/)
<!-- badges: end -->

The `loadflux` package is build for the comprehensive analysis of the
intra-event suspended sediment dynamics.

## Installation

<!-- CRAN
You can install the released version of loadflux from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("loadflux")
```
 -->

You can install the development version from
[GitHub](https://github.com/atsyplenkov/loadflux/) with:

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
data(djan)

df <- djan %>% 
  hydro_events(q = discharge,
               datetime = time,
               window = 21)

head(df)
#> # A tibble: 6 × 4
#>      he time                discharge    SS
#>   <dbl> <dttm>                  <dbl> <dbl>
#> 1     1 2017-06-06 12:00:00     0.778  227.
#> 2     1 2017-06-06 13:00:00     0.778   NA 
#> 3     1 2017-06-06 14:00:00     0.778  224.
#> 4     1 2017-06-06 15:00:00     0.778   NA 
#> 5     1 2017-06-06 16:00:00     0.778  271.
#> 6     1 2017-06-06 17:00:00     0.925   NA
```

### Plots

Then you can plot the hysteresis loop by running `hysteresis_plot`

``` r
library(ggplot2)

df %>% 
  filter(he == 2) %>%
  hysteresis_plot(q = discharge,
                  ssc = SS,
                  base_font_size = 14)
```

<img src="man/figures/README-hysteresis_plot-1.png" width="50%" style="display: block; margin: auto;" />

### Hyseteresis indexes

This package also contains several function for hysteresis index
calculation:

**Added**

-   `SHI` - Simple Hystersis Index from [*Tsyplenkov et al.,
    2020*](https://link.springer.com/article/10.1007/s11368-020-02633-z/)
-   `AHI` - Aich’s Hysteresis Index from [*Aich et al.,
    2014*](https://www.sciencedirect.com/science/article/abs/pii/S0341816214001969/)

**Pending**

-   `HImid` - Hysteresis Index from [*Lawler et al.,
    2006*](https://www.sciencedirect.com/science/article/abs/pii/S0048969705005711/)

To calculate a hysteresis index for your event run as follows:

``` r

df %>% 
  filter(he == 2) %>% 
  SHI(q = discharge,
      ssc = SS)
#> [1] 0.2473604
```

### ACKNOWLEDGEMENTS

*This package was developed in accordance to the Development program of
the Interdisciplinary Scientific and Educational School of M.V.
Lomonosov Moscow State University “Future Planet and Global
Environmental Change”*
