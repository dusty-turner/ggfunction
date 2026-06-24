# What We Fixed — Plain-Language Summary

This document explains the fixes from the 2026-06 audit. For each item:
**what was wrong before**, **what we did to fix it**, and **the correct
behavior now**. The fixes were organized into five batches.

---

## Batch 1 — Packaging / CRAN readiness

**What was wrong before:** The package used several R libraries (grid, scales,
stats) without formally declaring that it depended on them, and its
description/metadata didn't meet CRAN's submission rules. It would have been
rejected by R's official quality check.

**What we did:** Declared all required libraries as formal dependencies, moved
`scales` from optional to required, rewrote the package Title and Description to
CRAN's format, and cleaned up the build configuration.

**Correct behavior now:** The package passes R's official check
(`R CMD check`) with **0 errors and 0 warnings**, so it's ready to submit to
CRAN.

---

## Batch 2 — Correctness (math & plotting bugs)

These are the substantive bugs — cases where the plots could draw the *wrong*
picture.

**Shaded regions under discrete distributions**
- *Before:* When shading the area under a discrete probability mass function
  (e.g. a binomial), the shading could attach to the wrong group and could omit
  the top value of the range (the upper endpoint was excluded).
- *Fix:* Rewrote the shading logic to compute the shaded bars per group and to
  include the upper endpoint.
- *Now:* The correct bars are shaded, for the correct group, including both
  ends of the requested range.

**Curves that should close / include their endpoints**
- *Before:* Some curves (and closed shapes) dropped their final point, leaving a
  visible gap.
- *Fix:* Made the endpoint inclusive so the curve reaches its true end.
- *Now:* Curves render complete, and closed shapes actually close.

**Discrete quantile function boundary**
- *Before:* The last step of a discrete quantile function didn't reach
  probability 1.0.
- *Fix:* Forced the final boundary to 1.0.
- *Now:* The quantile function spans the full 0-to-1 probability range.

**Silent bad input**
- *Before:* Supplying a function that isn't a valid cumulative distribution
  (e.g. it doesn't reach 1) was accepted silently and produced a misleading
  plot.
- *Fix:* Added a soft validation check that warns the user instead of staying
  silent, plus guards against missing values in survival curves.
- *Now:* Invalid input produces a clear warning rather than a wrong plot drawn
  without comment.

---

## Batch 3 — Documentation

**What was wrong before:** Several help pages described functions inaccurately —
e.g. one geom was titled "Norm of a Vector Field" when it actually draws a
scalar field z = f(x, y); some `@param` descriptions pointed at the wrong
argument; and a vignette had an example whose curve was invisible.

**What we did:** Corrected the titles, parameter descriptions, and the example
so the docs match what the code actually does.

**Correct behavior now:** A user reading the help pages or vignettes gets an
accurate description and working examples.

---

## Batch 4 — The R Journal paper

**What was wrong before:** The paper contained factual errors — a wrong
simulation count and confidence level in the prose, the abstract named the wrong
statistical band (KS instead of DKW), an equation label was misnamed, and a
boundary condition was stated incorrectly. The rendered PDF and figures had also
drifted out of sync with the source.

**What we did:** Corrected the numbers, the abstract, the equation label, and
the boundary condition, then pruned stale cached figures and re-rendered the
paper cleanly.

**Correct behavior now:** The paper's text, PDF, and all figures are
internally consistent with no missing references. (The author ORCID is still a
placeholder — intentionally deferred.)

---

## Batch 5 — Test hardening

**What was wrong before:** Many tests only confirmed that a plot *ran* without
error — they didn't check that it produced the *right numbers*. A test helper
was also quietly adding stray columns to the data being tested.

**What we did:** Added value-level assertions (checking the actual computed
coordinates for trajectories, scalar fields, discrete CDFs, and survival
curves) and removed the leaky helper.

**Correct behavior now:** The test suite verifies correct *values*, not just
that code executes, so future changes that break the math will be caught.

---

*Result: the full test suite passes and the package is CRAN-clean.*
