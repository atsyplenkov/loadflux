# What CRAN requires to resubmit an archived package

Research for [issue 3](https://github.com/atsyplenkov/loadflux/issues/3). Sources are CRAN policy pages, R manuals, and R's own incoming-check code. No blog posts.

**Package facts used here.** `loadflux` last published as 0.0.2 on 2021-11-05. CRAN archived it on 2023-05-19 "as issues were not corrected in time." The archival snapshot shows Linux `ERROR`s rebuilding `turbidity.Rmd` (`there is no package called 'brolgar'`), a `NOTE` "Package suggested but not available for checking: ‘brolgar’", and an additional issue `noSuggests`. Destination already chosen: version **0.1.0**, drop the `brolgar` / `fabletools` / `feasts` / `tsibble` / `feat_event` stack, local `R CMD check --as-cran`, human upload.

## 1. How the resubmission is described (`cran-comments.md`)

CRAN does **not** document a file named `cran-comments.md`. That filename is a local draft (this repo already lists it in `.Rbuildignore`). What CRAN reads is the **‘Optional comment’** field on the web form.

CRAN Repository Policy, [Re-submission](https://cran.r-project.org/web/packages/policies.html#Re_002dsubmission):

> Re-submission is done in the same way as submission, using the ‘Optional comment’ field on the web form (and not a separate email) to explain how the feedback on previous submission(s) has been addressed.

CRAN Repository Policy, [Submission](https://cran.r-project.org/web/packages/policies.html#Submission):

> In principle, packages must pass `R CMD check` without warnings or significant notes to be admitted to the main CRAN package area. If there are warnings or notes you cannot eliminate (for example because you believe them to be spurious) send an explanatory note as part of your covering email, or as a comment on the submission form.

Same page: uploads go through <https://CRAN.R-project.org/submit.html>; the tarball must come from `R CMD build`; `R CMD check --as-cran` must have been run on **that tarball**, preferably with current R-devel.

For a package archived since February 2018, the same Re-submission section adds:

> For packages which have been archived since February 2018, a snapshot of the CRAN results page at the time of archival will be available under https://cran-archive.r-project.org/web/checks/.

`loadflux`'s snapshot is <https://cran-archive.R-project.org/web/checks/2023/2023-05-19_check_results_loadflux.html>. The landing page wording is: "Archived on 2023-05-19 as issues were not corrected in time." (<https://cran.r-project.org/package=loadflux>)

Incoming `R CMD check --as-cran` itself will label this upload. R's `.check_package_CRAN_incoming()` (current R-devel `tools`, [QC.R](https://svn.r-project.org/R/trunk/src/library/tools/R/QC.R)):

- if the name is not on current CRAN: `out$new_submission <- TRUE` → formatted as **"New submission"**
- if the name is in the CRAN archive: `out$CRAN_archive <- TRUE` → formatted as **"Package was archived on CRAN"**
- always prints **Maintainer: …**

So a local `cran-comments.md` that is pasted into Optional comment should, in CRAN's own terms:

1. Explain how the **archival check issues** were fixed (the "feedback" / uncorrected ERRORs), pointing at the archive snapshot.
2. Explain any remaining **warnings or notes you cannot eliminate**.
3. Not treat this as a brand-new package name: the name is persistent ("Package names on CRAN are persistent and in general it is not permitted to change a package’s name." — [Source packages](https://cran.r-project.org/web/packages/policies.html#Source-packages)).

CRAN does not prescribe a sentence such as "This is a resubmission of an archived package." The incoming check already emits `Package was archived on CRAN`. The Optional comment still needs to say **what was wrong in 2023 and what changed**, because Re-submission is defined as explaining how previous problems were addressed.

Draft content that matches that wording (not a CRAN template; CRAN has none for this filename):

```text
This is an update of loadflux after archival on 2023-05-19
("issues were not corrected in time").
Archive snapshot:
https://cran-archive.R-project.org/web/checks/2023/2023-05-19_check_results_loadflux.html

The Linux ERROR was re-building vignette turbidity.Rmd
(there is no package called 'brolgar'), with NOTE
"Package suggested but not available for checking: ‘brolgar’"
and additional issue noSuggests.

This 0.1.0 tarball removes that Suggests stack and the code
that used it. Remaining NOTES: <list and justify each>.
```

Also from Re-submission: if macOS-arm64 / M1mac issues appear, check with [macbuilder](https://mac.r-project.org/macbuilder/submit.html) first. `loadflux` was OK on macOS at archival; Linux was the failure.

## 2. Version numbering (last published 0.0.2 → 0.1.0)

CRAN Repository Policy, Re-submission:

> Updates to previously-published packages must have an increased version. Increasing the version number at each submission reduces confusion so is preferred even when a previous submission was not accepted.

[Writing R Extensions §1.1.1](https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#The-DESCRIPTION-file):

> The mandatory ‘Version’ field gives the version of the package. This is a sequence of at least two (and usually three) non-negative integers separated by single ‘.’ or ‘-’ characters. […] It is not a decimal number, so for example 0.9 < 0.75 since 9 < 75.

Incoming check for an **archived** name (same `QC.R`): take the max version among archived tarballs; if submitted `v_m <= v_a` then **WARNING** "Insufficient package version (submitted: …, existing: …)".

For `loadflux`, archive contains only `loadflux_0.0.2.tar.gz`. **0.1.0 > 0.0.2**, so it satisfies both the policy ("increased version") and the incoming comparator.

Other incoming version NOTES that do **not** apply to 0.1.0:

- leading zeroes in a component (`0.01` style)
- a "jump in minor" of 10 or more (`v_m$minor >= v_d$minor + 10`) — 0 → 1 is not that
- components ≥ 1234 (except the current year)

The unpublished tree currently says 0.0.3. Policy prefers bumping **each submission**, including failed ones. Shipping 0.1.0 rather than recycling 0.0.3 matches that sentence. CRAN does not require a major/minor/patch *meaning*; only that the version is larger than the last published (and any previous unaccepted upload of the same number).

Recency/frequency NOTES (`Days since last update: < 7`, `Number of updates in past 6 months: > 6`) apply to packages **currently on CRAN**. An archived package takes the early-return path in `QC.R` after the archive version check, so those counters are not attached.

## 3. Vignettes and Suggests (`--no-suggests`, `requireNamespace`, skipping)

### What failed in 2023

Archival snapshot: vignette rebuild `ERROR` because `turbidity.Rmd` used `brolgar` unconditionally; Linux flavors also `NOTE` that `brolgar` was not available for checking; additional issue [noSuggests](https://cran.r-project.org/web/checks/check_issue_kinds.html).

BDR's noSuggests README (<https://www.stats.ox.ac.uk/pub/bdr/noSuggests/README.txt>):

> Tests on x86_64 Linux with R-devel using `_R_CHECK_DEPENDS_ONLY_=true`, so packages in Suggests (and Enhances) are not available (other than recommended packages). […] Packages get reported here when they have used a suggested/enhances package which is unavailable.

### Conditional use is required

CRAN Repository Policy, Source packages:

> A package listed in ‘Suggests’ or ‘Enhances’ should be used conditionally in examples or tests if it cannot straightforwardly be installed on the major R platforms. (‘Writing R Extensions’ recommends that they are *always* used conditionally.)

[Writing R Extensions §1.1.3.1 Suggested packages](https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Suggested-packages):

> Note that someone wanting to run the examples/tests/vignettes may not have a suggested package available (and it may not even be possible to install it for that platform). […] using `if(requireNamespace("pkgname"))` is preferred, if possible.

> However, using `require` for conditioning in package code is not good practice […]. It is better practice to use code like

```r
if (requireNamespace("rgl", quietly = TRUE)) {
   rgl::plot3d(...)
} else {
   ## do something else not involving rgl.
}
```

> On most systems, `R CMD check` can be run with only those packages declared in ‘Depends’ and ‘Imports’ by setting environment variable `_R_CHECK_DEPENDS_ONLY_=true` […]. It is recommended that a package is checked with each of these set, as well as with neither.

[R Internals, Tools](https://cran.r-project.org/doc/manuals/r-devel/R-ints.html#Tools):

- `_R_CHECK_SUGGESTS_ONLY_`: default false, **true for CRAN submission checks**
- `_R_CHECK_DEPENDS_ONLY_`: examples/tests/vignettes see only Depends/Imports (plus test-suite managers in Suggests, plus VignetteBuilder)
- Incoming `--as-cran` also sets `_R_CHECK_FORCE_SUGGESTS_=FALSE` "since some packages do suggest other packages not available on CRAN"

There is no CRAN flag spelled `--no-suggests`. The equivalent is `_R_CHECK_DEPENDS_ONLY_=true` (the noSuggests additional issue) plus `_R_CHECK_FORCE_SUGGESTS_=FALSE` on incoming.

### Vignettes still have to rebuild

[Writing R Extensions §1.4](https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Writing-package-vignettes):

> Package vignettes are tested by `R CMD check` by executing all R code chunks they contain (except those marked for non-evaluation, e.g., with option `eval=FALSE` for Sweave).

> `R CMD build` will automatically create the (PDF or HTML versions of the) vignettes in `inst/doc` for distribution with the package sources. By including the vignette outputs in the package sources it is not necessary that these can be re-built at install time […].

HTML or PDF is the recommended installed form. Sources live in `vignettes/`. `BuildVignettes: false` stops rebuild, but incoming then NOTES **"FOSS licence with BuildVignettes: false"** — not a usable escape for this MIT package.

If a vignette engine is `knitr::rmarkdown` (both current vignettes declare `%\\VignetteEngine{knitr::rmarkdown}`), R-exts §1.1.1 requires **both** `knitr` and `rmarkdown` in `VignetteBuilder` and at least in Suggests. The 2022 DESCRIPTION only lists `VignetteBuilder: knitr`.

Chunks that must not run when a Suggests package is missing should use `eval=` conditioned on `requireNamespace()`, or `eval=FALSE`. Unconditional `library(purrr)` / `library(tsibble)` in `turbidity.Rmd` is the same class of bug as `library(brolgar)` once those packages are only Suggests or gone.

### Skipping examples and tests

[Writing R Extensions, documenting functions](https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Documenting-functions):

- `\dontrun{}` — shown, not run
- `\dontshow{}` — run, not shown
- `\donttest` — run by `example()`, **not** by `R CMD check` by default; `--as-cran` **does** run `\donttest` unless `_R_CHECK_DONTTEST_EXAMPLES_=false` (R Internals). "This should be needed only occasionally […]. Note that code included in `\donttest` must be correct R code, and any packages used should be declared in the DESCRIPTION file."

Policy, Source packages: "Long-running tests and vignette code can be made optional for checking, but do ensure that the checks that are left do exercise all the features of the package." Examples "should run for no more than a few seconds each."

R-exts checking: slow tests may live under e.g. `inst/slowTests` and be invoked with `R CMD check --test-dir=…`; they are not part of the default CRAN check.

`testthat::skip_on_cran()` is **not** described in CRAN policy or R-exts. If tests stay, any Suggests they need (including `testthat` itself) must still be declared; R Internals notes test-suite managers in Suggests remain available even under `_R_CHECK_DEPENDS_ONLY_`.

## 4. NOTES that CRAN will still accept on a resubmission

CRAN does **not** publish a whitelist of harmless NOTES. The rule is negative:

- Policy: "must pass `R CMD check` without warnings or **significant notes**" unless you "cannot eliminate" them and you explain them on the form.
- [Checklist for CRAN submissions](https://cran.r-project.org/web/packages/submission_checklist.html): "you will be asked to fix and resubmit your package if it gives warnings or significant notes."
- Policy also: maintainers "will be asked to update packages which show any warnings or significant notes […]. Packages which are not updated are liable to be archived."

What **will** appear on this resubmission even if the package is clean, because incoming feasibility always reports them (formatted in `format.check_package_CRAN_incoming`):

| Incoming text | Why it appears |
| --- | --- |
| `Maintainer: 'Anatoly Tsyplenkov <atsyplenkov@gmail.com>'` | Always printed |
| `New submission` | Name is not on current CRAN |
| `Package was archived on CRAN` | Name is in the archive |

Those three are NOTES, not ERRORs. They are expected for an un-archive. Explain them in Optional comment as incoming labels, not as defects.

Anything else (URLs, HTML5 Rd, LazyData compression, spelling, Title/Description style, missing vignette index, undeclared Suggests, large installed size, …) is judged case by case. URL checks: <https://cran.r-project.org/web/packages/URL_checks.html> — if a site fails HEAD but works in a browser, "explain your findings in the submission."

Do **not** leave the 2023 `brolgar` NOTE: "Package suggested but not available for checking" is a significant note and was part of archival.

## 5. DESCRIPTION fields expected now that a 2022 file may lack

Mandatory fields remain Package, Version, License, Description, Title, and Author/Maintainer **or** Authors@R ([R-exts §1.1.1](https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#The-DESCRIPTION-file)). Current wording that was not always this strict:

> Note that for CRAN submissions, providing ‘Authors@R’ is required, and providing ORCID or ROR identifiers (see https://orcid.org/ and https://ror.org/) where possible is strongly encouraged.

Incoming check: missing Authors@R is a NOTE that prints a generated `Authors@R:` block to copy. This package already has Authors@R, `cre`+`aut`, email, and ORCID `0000-0003-4144-8402`. ROR applies to organizations, not this maintainer.

Checklist (same Authors@R / ORCID / ROR examples): <https://cran.r-project.org/web/packages/submission_checklist.html>

**Already present and still valid**

| Field | Current value | Status |
| --- | --- | --- |
| Authors@R | person with aut, cre, ORCID | Required for CRAN; OK |
| Maintainer | Anatoly Tsyplenkov `<atsyplenkov@gmail.com>` | Still required as a person, not a list ([Policy](https://cran.r-project.org/web/packages/policies.html#Source-packages)); may be auto-generated from Authors@R |
| Encoding | UTF-8 | Required only if DESCRIPTION is not pure ASCII; UTF-8 and latin1 are the portable names |
| Language | en-US | Optional IETF tag; first tag is the HTML manual language |
| LazyData | true | Correct while `data/` exists. Incoming/check sanity (since R 4.2): LazyData without `data/` is stripped/noted; large lazy DBs without `LazyDataCompression` get a pointer to R-exts §1.1.6 |
| License | MIT + file LICENSE | Matches R-exts "standard short specifications" plus `+ file LICENSE` for the MIT template (YEAR / COPYRIGHT HOLDER) |
| Depends | R (>= 4.1) | Trailing zeros may be dropped; patch-level Depends on R is discouraged |
| URL, BugReports | present | Optional; URLs must survive `--as-cran` URL checks |
| RoxygenNote | 7.2.3 | Not a CRAN field; ignore for policy |

**Not required, not missing**

- `Date` — optional; ISO `yyyy-mm-dd` if used.
- `NeedsCompilation` — `R CMD build` sets it (`yes` iff `src/` exists).
- `ByteCompile` — default is to byte-compile.
- `Config/testthat/edition` — allowed (`Config/` is a reserved prefix in incoming field-name checks); not required.
- `Repository`, `Date/Publication`, `Packaged`, `Built` — CRAN/tools add these; "There should be no ‘Built’ or ‘Packaged’ fields" in the sources.

**Gaps relative to current manuals (not new named fields)**

1. **`VignetteBuilder` should list `rmarkdown` as well as `knitr`** for `%\\VignetteEngine{knitr::rmarkdown}` (R-exts §1.1.1). Current file only names `knitr`.
2. **`LazyDataCompression`** — only if `R CMD check` NOTES a large lazy-load DB; R-exts §1.1.6 gives `CheckLazyDataCompression()` (`gzip` / `bzip2` / `xz`).
3. **HTML5 Rd / tidy** — `--as-cran` builds HTML help and checks HTML5 (R-exts §1.3.1). Not a DESCRIPTION field.
4. **DOI/URL markup in Description** — citations as author-year plus `<doi:…>` or `<https://…>` (Policy and checklist). Current Description has no citation; none required unless you add papers.
5. **No Authors@R is not optional anymore.** Already satisfied.

Nothing in current R-exts or CRAN policy requires a 2026-only field this DESCRIPTION lacks (`Date`, `ROR`, `Config/Needs/website`, `Copyright`, …). The live requirements that bite an un-archive are Authors@R (present), conditional Suggests/vignettes, an increased version, Optional-comment explanation of the 2023 ERROR, and `--as-cran` on the uploaded tarball.

## Practical sequence (from the same sources)

1. Fix the archival ERROR/NOTE/noSuggests so vignettes, examples, and tests do not load undeclared or Suggests-only packages unconditionally.
2. Set Version **greater than 0.0.2** (0.1.0 is valid).
3. `R CMD build` with current R-patched or release; `R CMD check --as-cran` on **that tarball** with current R-devel (Policy Submission; checklist). Also run with `_R_CHECK_DEPENDS_ONLY_=true`.
4. Upload the tarball at <https://CRAN.R-project.org/submit.html>. Put the archival explanation and leftover-NOTE justification in Optional comment. Accept the confirmation email. Do not upload again while the submission is pending.
5. Correspondence: `CRAN-submissions@R-project.org`, plain text ASCII (Policy Preamble).

## Sources

- CRAN Repository Policy (Revision 6875, fetched 2026-09-12): <https://cran.r-project.org/web/packages/policies.html>
- Checklist for CRAN submissions: <https://cran.r-project.org/web/packages/submission_checklist.html>
- CRAN URL checks: <https://cran.r-project.org/web/packages/URL_checks.html>
- CRAN submit form: <https://cran.r-project.org/submit.html>
- Writing R Extensions (R-devel): <https://cran.r-project.org/doc/manuals/r-devel/R-exts.html>
- R Internals, Tools / `_R_CHECK_*`: <https://cran.r-project.org/doc/manuals/r-devel/R-ints.html#Tools>
- CRAN check issue kinds (`noSuggests`): <https://cran.r-project.org/web/checks/check_issue_kinds.html>
- noSuggests README: <https://www.stats.ox.ac.uk/pub/bdr/noSuggests/README.txt>
- Incoming feasibility formatter and archive version check: <https://svn.r-project.org/R/trunk/src/library/tools/R/QC.R>
- loadflux CRAN page: <https://cran.r-project.org/package=loadflux>
- Archival check snapshot (2023-05-19): <https://cran-archive.R-project.org/web/checks/2023/2023-05-19_check_results_loadflux.html>
- Archive tarball listing: <https://cran.r-project.org/src/contrib/Archive/loadflux/>
