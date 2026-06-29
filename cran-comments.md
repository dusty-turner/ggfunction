## Test Environments

* local macOS Tahoe 26.5.1, R 4.6.0

## R CMD Check Results

Baseline local check before these revisions:

* `rcmdcheck::rcmdcheck(args = c("--no-manual"), error_on = "never")`
* 0 errors, 0 warnings, 0 notes

Updated local checks after these revisions:

* `devtools::document()`: completed cleanly
* `devtools::test()`: 911 passing tests, 0 failures, 0 warnings
* `devtools::check(args = c("--no-manual"), error_on = "never")`:
  0 errors, 0 warnings, 1 note
* `rcmdcheck::rcmdcheck(args = c("--as-cran", "--no-manual"), env = c("_R_CHECK_CRAN_INCOMING_REMOTE_" = "false", "_R_CHECK_SYSTEM_CLOCK_" = "false"), error_on = "never")`:
  0 errors, 0 warnings, 0 notes
* `urlchecker::url_check()`: all URLs correct
* `spelling::spell_check_package()`: no spelling errors found

## Notes

The single note from the local `devtools::check()` run was:

* "Unable to verify current time." This is caused by the network-restricted
  local environment used for the check. Re-running with
  `_R_CHECK_SYSTEM_CLOCK_=false` removes the note.

A raw `rcmdcheck::rcmdcheck(args = c("--as-cran"), error_on = "never")`
attempt could not complete CRAN incoming checks without network access, and
the local LaTeX installation could not build the manual PDF because the
`tctt0900` font is unavailable. The package passes the adjusted no-manual
as-CRAN check above.

No reverse dependencies are currently known for this development version.
