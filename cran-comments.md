## Resubmission (un-archive)

This is an update of loadflux, previously published as 0.0.2 and archived on
2023-05-19 because issues were not corrected in time.

CRAN Linux checks ERROR'd while rebuilding vignettes: turbidity.Rmd called
library(brolgar) after brolgar had left CRAN. A check without Suggests also
ERROR'd on unconditional library(fabletools) in examples/tests and
library(feasts) in the same vignette.

See:
https://cran-archive.R-project.org/web/checks/2023/2023-05-19_check_results_loadflux.html

Version 0.1.0 removes feat_event and the brolgar / fabletools / feasts / tsibble
stack. The turbidity vignette now uses only loadflux and its Imports. Suggests
are no longer loaded unconditionally.

## Test environments
- local Linux, R 4.6.1

## R CMD check results

0 errors | 0 warnings | 1 note

Maintainer: 'Anatoly Tsyplenkov <atsyplenkov@gmail.com>'

New submission

Package was archived on CRAN

This incoming note is expected for an un-archive.
