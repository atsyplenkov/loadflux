# What `R CMD check --as-cran` reports on loadflux 0.0.3

Primary sources: the check logs in this directory, `DESCRIPTION`, `R/features.R`, `man/loadflux-features.Rd`, `vignettes/turbidity.Rmd`, `tests/testthat/test-check_output.R`. No CRAN fixes were applied. Suggests that were missing locally were left missing.

## Environment

| Item | Value | Source |
|------|--------|--------|
| Date (UTC) | 2026-09-12T16:21:11Z (env snapshot); checks at 2026-09-12 16:21:50 UTC | [env.txt](env.txt); both `00check.log` headers |
| R | 4.6.1 (2026-06-24) | [env.txt](env.txt); `00check.log` |
| Platform | x86_64-pc-linux-gnu | same |
| OS | Arch Linux | same |
| Package version | 0.0.3 | [DESCRIPTION](../../DESCRIPTION) line 4 |

Local library at check time ([env.txt](env.txt)):

- **Suggests missing (not installed):** `brolgar`, `fabletools`, `feasts`
- **Suggests present:** `covr` 3.6.5, `knitr` 1.51, `lifecycle` 1.0.5, `purrr` 1.2.2, `rmarkdown` 2.31, `testthat` 3.3.2
- **Imports present:** `dplyr` 1.2.1, `ggplot2` 4.0.3, `lubridate` 1.9.5, `rlang` 1.3.0, `tidyr` 1.3.2, `xts` 0.14.2, `zoo` 1.8-15
- **Imports installed for this run only:** `tsibble` 1.2.0 and `dygraphs` 1.1.1.6 were absent from the library at the start of the session (`DESCRIPTION` lists both under Imports, lines 18–26). They were installed from CRAN so the package could load. Dependency `anytime` 0.3.13 came in with `tsibble`. **`brolgar` / `fabletools` / `feasts` were not installed.**

Those three Suggests *are* on CRAN as of this run (`available.packages()` from `https://cloud.r-project.org`: `brolgar` 1.0.2, `fabletools` 0.8.0, `feasts` 0.5.0). They were missing from this machine, not from the repository. That is the same shape of failure CRAN used in 2023 (`--no-suggests` / missing Suggests), even though `brolgar` is back on CRAN.

## Commands

Working tree: unpublished 0.0.3 at this worktree. `^\.scratch$` was added to `.Rbuildignore` so these notes are not in a tarball.

```sh
# 1. Default source build (vignettes rebuilt)
R CMD build .
# -> ERROR, see r-cmd-build.log

# 2. Tarball without prebuilt vignettes, so check could still run
R CMD build --no-build-vignettes .
# -> loadflux_0.0.3.tar.gz

# 3. Default --as-cran (FORCE_SUGGESTS true)
R CMD check --as-cran -o .scratch/cran-resubmit/as-cran loadflux_0.0.3.tar.gz

# 4. CRAN --no-suggests equivalent
_R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran -o .scratch/cran-resubmit/no-suggests loadflux_0.0.3.tar.gz
```

The huge `*.Rcheck` trees were discarded after copying logs. Kept:

- [as-cran-00check.log](as-cran-00check.log)
- [no-suggests-00check.log](no-suggests-00check.log)
- [r-cmd-build.log](r-cmd-build.log)
- [no-suggests-testthat.Rout](no-suggests-testthat.Rout)
- [no-suggests-Ex.timings](no-suggests-Ex.timings)

## Status lines

| Run | Status |
|-----|--------|
| `R CMD build .` | ERROR (vignette `turbidity.Rmd`) |
| `R CMD check --as-cran` | **1 ERROR, 1 NOTE** |
| `_R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran` | **2 ERRORs, 2 WARNINGs, 1 NOTE** |

Quoted from the logs:

```
Status: 1 ERROR, 1 NOTE
```

([as-cran-00check.log](as-cran-00check.log) line 50)

```
Status: 2 ERRORs, 2 WARNINGs, 1 NOTE
```

([no-suggests-00check.log](no-suggests-00check.log) line 161)

## `R CMD build .`

`loadflux.Rmd` rebuilt. `turbidity.Rmd` died in chunk `feat_ev` at lines 77–84 ([vignettes/turbidity.Rmd](../../vignettes/turbidity.Rmd)):

```
Quitting from turbidity.Rmd:77-84 [feat_ev]
Error in `library()`:
! there is no package called 'feasts'
...
Error: processing vignette 'turbidity.Rmd' failed with diagnostics:
there is no package called 'feasts'
```

([r-cmd-build.log](r-cmd-build.log) lines 10–31)

The next chunk in that vignette (`stats`, lines 88–98) calls `library(brolgar)` and `library(feasts)`. It was never reached.

## `--as-cran` (Suggests forced)

Check stopped after package dependencies. No examples, tests, or vignette rebuild.

### ERROR — package dependencies

```
* checking package dependencies ... ERROR
Packages suggested but not available: 'brolgar', 'fabletools', 'feasts'

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.
```

([as-cran-00check.log](as-cran-00check.log) lines 40–45; Suggests listed in [DESCRIPTION](../../DESCRIPTION) lines 28–38)

### NOTE — CRAN incoming feasibility

```
* checking CRAN incoming feasibility ... [5s/23s] NOTE
Maintainer: ‘Anatoly Tsyplenkov <atsyplenkov@gmail.com>’

New submission

Package was archived on CRAN

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2023-05-19 as issues were not corrected
    in time.

Package has a VignetteBuilder field but no prebuilt vignette index.

Found the following (possibly) invalid URLs:
  URL: https://atsyplenkov.github.io/loadflux/ (moved to https://anatolii.nz/loadflux/)
    From: DESCRIPTION
    Status: 301
    Message: Moved Permanently
  URL: https://link.springer.com/article/10.1007/s11368-020-02633-z/ (moved to https://link.springer.com/article/10.1007/s11368-020-02633-z)
    From: README.md
    Status: 301
    Message: Moved Permanently
```

([as-cran-00check.log](as-cran-00check.log) lines 15–37)

- Maintainer matches [DESCRIPTION](../../DESCRIPTION) line 8.
- GitHub Pages URL is [DESCRIPTION](../../DESCRIPTION) line 13.
- Springer URL with trailing slash is [README.md](../../README.md) line 90 (from [README.Rmd](../../README.Rmd) line 83).
- “No prebuilt vignette index” is from the `--no-build-vignettes` tarball after the `feasts` build failure.

## `_R_CHECK_FORCE_SUGGESTS_=false` `--as-cran`

Dependencies become INFO, not ERROR. Check continues.

```
* checking package dependencies ... INFO
Packages suggested but not available for checking:
  'brolgar', 'fabletools', 'feasts'
```

([no-suggests-00check.log](no-suggests-00check.log) lines 40–42)

Install, load, Rd, data, and PDF/HTML manuals were OK ([no-suggests-00check.log](no-suggests-00check.log) lines 48–84, 156–157). `loadflux.Rmd` rebuilt ([no-suggests-00check.log](no-suggests-00check.log) lines 129–131). That vignette uses `dplyr`, `loadflux`, and `dygraphs` only ([vignettes/loadflux.Rmd](../../vignettes/loadflux.Rmd)).

### ERROR — examples (`feat_event` / `fabletools`)

```
* checking examples ... ERROR
Running examples in ‘loadflux-Ex.R’ failed
The error most likely occurred in:
...
> ### Name: loadflux-features
> ### Title: Calculate features of a 'tsibble' object in conjunction with
> ###   [features()]
> ### Aliases: loadflux-features feat_event
...
> library(fabletools)
Error in library(fabletools) : there is no package called ‘fabletools’
Execution halted
```

([no-suggests-00check.log](no-suggests-00check.log) lines 88–117)

The example is [R/features.R](../../R/features.R) lines 16–35 / [man/loadflux-features.Rd](../../man/loadflux-features.Rd): `library(fabletools)`, `library(tsibble)`, `as_tsibble()`, `features(..., feat_event)`.

Examples that finished before that failure (Rd order; `feat_event` has no timing row because it aborted):

```
AHI, HImid, SHI, TI, djan, djanturb, event_plot, hydro_events, hysteresis_plot
```

([no-suggests-Ex.timings](no-suggests-Ex.timings))

### ERROR — vignette rebuild (`feasts` / `feat_event`)

```
* checking re-building of vignette outputs ... ERROR
...
--- re-building ‘turbidity.Rmd’ using rmarkdown

Quitting from turbidity.Rmd:77-84 [feat_ev]
Error in `library()`:
! there is no package called 'feasts'
...
Error: processing vignette 'turbidity.Rmd' failed with diagnostics:
there is no package called 'feasts'
```

([no-suggests-00check.log](no-suggests-00check.log) lines 127–154)

Same chunk as the build failure: `library(feasts)` then `features(time, feat_event)` ([vignettes/turbidity.Rmd](../../vignettes/turbidity.Rmd) lines 77–82). `brolgar` is only in the later `stats` chunk (lines 88–90) and was not executed.

Pandoc printed `Deprecated: --mathjax. Use --math-method=mathjax[:URL] instead.` while rebuilding `loadflux.Rmd`. That is not a check ERROR/WARNING/NOTE.

### WARNING — vignette files / `inst/doc`

```
* checking files in ‘vignettes’ ... WARNING
Files in the 'vignettes' directory but no files in 'inst/doc':
  ‘loadflux.Rmd’ ‘turbidity.Rmd’
```

```
* checking package vignettes ... WARNING
Directory 'inst/doc' does not exist.
Package vignettes without corresponding single PDF/HTML:
  ‘loadflux.Rmd’
  ‘turbidity.Rmd’
```

([no-suggests-00check.log](no-suggests-00check.log) lines 85–87, 122–126)

These WARNINGs are from checking a `--no-build-vignettes` tarball. Default `R CMD build` never produced `inst/doc` because `feasts` failed first.

### NOTE — same incoming NOTE as the forced-Suggests run

Identical CRAN incoming NOTE ([no-suggests-00check.log](no-suggests-00check.log) lines 15–37).

### Tests: OK, but the stack file was skipped

```
* checking tests ... OK
  Running ‘testthat.R’
```

([no-suggests-00check.log](no-suggests-00check.log) lines 119–120)

```
[ FAIL 0 | WARN 0 | SKIP 1 | PASS 0 ]
• {fabletools} is not installed. (1): 'test-check_output.R:5:1'
```

([no-suggests-testthat.Rout](no-suggests-testthat.Rout) lines 40–43)

Line 5 of [tests/testthat/test-check_output.R](../../tests/testthat/test-check_output.R) is `library(fabletools)`. That file also `library(tsibble)` and calls `as_tsibble()` / `features(..., feat_event)` (lines 4, 64–70). testthat 3.3.2 skipped the whole file; it did not fail the check, and it did not exercise `hydro_events` / `AHI` / `SHI` / `TI` / `event_plot` either, because those assertions share the file.

## Split: tidyverts / `feat_event` stack vs remaining package

### Caused by `brolgar` / `fabletools` / `feasts` / `tsibble` / `feat_event`

| Result | Why |
|--------|-----|
| `--as-cran` ERROR: Suggests `brolgar`, `fabletools`, `feasts` missing | [DESCRIPTION](../../DESCRIPTION) Suggests; forced-Suggests check |
| no-suggests INFO: same three packages | same, with `_R_CHECK_FORCE_SUGGESTS_=false` |
| no-suggests examples ERROR | `library(fabletools)` in `feat_event` examples |
| `R CMD build` ERROR and no-suggests vignette ERROR | `library(feasts)` then `features(..., feat_event)` in `turbidity.Rmd` |
| `brolgar` not seen as a runtime error here | later chunk; `feasts` fails first |
| Tests skip `test-check_output.R` | `library(fabletools)` at line 5 |
| WARNINGs: no `inst/doc` / no HTML vignettes | `--no-build-vignettes` after the `feasts` build failure |
| Incoming NOTE: “VignetteBuilder … no prebuilt vignette index” | same tarball |

`tsibble` is an Import (`DESCRIPTION` line 24; `NAMESPACE` `importFrom(tsibble, as_tsibble)` / `tsibble`). With `tsibble` installed, it did not produce its own check ERROR. It is only used with `feat_event` and the turbidity `features()` demo (`R/features.R`, `vignettes/turbidity.Rmd` `to_tsibble` / `feat_ev` chunks, tests). Removing that stack is what would drop `tsibble` from Imports; this check does not implement that.

### Remaining package (not that stack)

| Result | Why |
|--------|-----|
| Incoming NOTE: New submission | unpublished / archived package |
| Incoming NOTE: archived 2023-05-19, issues not corrected in time | CRAN db override in both `00check.log` files |
| Incoming NOTE: `https://atsyplenkov.github.io/loadflux/` → 301 to `https://anatolii.nz/loadflux/` | [DESCRIPTION](../../DESCRIPTION) URL field |
| Incoming NOTE: Springer article URL 301 (trailing slash) | [README.md](../../README.md) |
| Examples for AHI, HImid, SHI, TI, data, `event_plot`, `hydro_events`, `hysteresis_plot` | completed ([no-suggests-Ex.timings](no-suggests-Ex.timings)) |
| `vignettes/loadflux.Rmd` rebuild | OK; dygraphs HTML vignette, not tidyverts |
| Install, load, Rd, data, PDF/HTML manuals | OK in the no-suggests run |

This log does not prove the remaining examples and `loadflux.Rmd` would be clean on a CRAN machine after the stack is deleted; it only shows they did not fail in this no-suggests run once Imports were present.
