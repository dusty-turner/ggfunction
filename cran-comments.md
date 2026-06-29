## Test Environments

* local macOS Tahoe 26.5.1, R 4.6.0

## R CMD Check Results

Updated local checks after the final figure and confidence-display revisions:

* `devtools::document()`: completed cleanly
* `devtools::test(reporter = "summary")`: 959 passing expectations across
  443 tests, 0 failures, 0 warnings
* `spelling::spell_check_package()`: no spelling errors found
* `rcmdcheck::rcmdcheck(args = c("--as-cran", "--no-manual"), env = c("_R_CHECK_CRAN_INCOMING_REMOTE_" = "false", "_R_CHECK_SYSTEM_CLOCK_" = "false"), error_on = "never")`:
  0 errors, 0 warnings, 0 notes

## Notes

The adjusted as-CRAN check above disables remote incoming and system-clock
checks for this network-restricted local environment. During package dependency
checking, R printed repository-index access warnings for CRAN/Bioconductor, but
these did not become R CMD check warnings or notes.

The local LaTeX installation could not previously build the manual PDF because
the `tctt0900` font is unavailable. The package passes the adjusted no-manual
as-CRAN check above.

No reverse dependencies are currently known for this development version.
