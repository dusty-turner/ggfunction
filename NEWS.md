# ggfunction (development version)

* `geom_qf()` and `geom_survival()` gained an `hf_fun` source (with
  `hf_lower`), so quantile and survival curves can now be derived from a hazard
  function like the other continuous distribution geoms.
* `geom_qf()` gained `xlim` (the drawn probability range) and `check`/
  `check_tol` validity diagnostics.
* `geom_survival()` gained probability shading (`p`, `lower.tail`,
  `p_lower`/`p_upper`, and a `fill` argument), mirroring `geom_cdf()`, plus
  `check`/`check_tol` diagnostics that replace the previous unconditional
  monotonicity warning.
* `geom_hf()` and `geom_chf()` gained `check`/`check_tol` validity diagnostics
  (non-negativity, and monotonicity for cumulative hazards).
* The discrete step geoms `geom_cdf_discrete()`, `geom_survival_discrete()`,
  and `geom_qf_discrete()` gained tail/interval shading (`p`, `lower.tail`,
  `p_lower`/`p_upper`, `shade_outside`) consistent with `geom_pmf()`:
  highlighted atoms draw at full opacity while the rest are dimmed.
* Fixed a bug where `geom_pdf()`, `geom_cdf()`, `geom_survival()`, and
  `geom_hf()` silently overrode a user-supplied `colour` argument (or a mapped
  `colour`/`fill` aesthetic) with their fixed `color` default, drawing the
  layer in black and emitting a duplicated-aesthetics warning.

# ggfunction 0.1.0

* Initial release of mathematical-function taxonomy layers:
  `geom_function_1d_1d()`, `geom_function_1d_2d()`,
  `geom_function_2d_1d()`, and `geom_function_2d_2d()`.
* Added centralized internal conversion helpers for continuous distribution
  routes among PDFs, CDFs, survival functions, quantile functions, hazards, and
  cumulative hazards.
* Added theoretical probability layers for PDF/PMF, CDF, survival, quantile,
  hazard, and cumulative-hazard displays, including support-aware tail,
  interval, and HDR shading where supported.
* `geom_pmf()` gained a `type` argument to render a probability mass function as
  bars (`type = "bar"`) in addition to the default lollipop display, with the
  same cumulative/HDR shading in both modes.
* Added bivariate probability layers for theoretical PDF and PMF displays,
  including HDR/raster behavior for PDFs and point/tile behavior for PMFs.
* Added empirical complete-data layers: `geom_ecdf()` and `geom_eqf()` with
  DKW/Massart bands, `geom_epmf()` as an empirical mass/lollipop display, and
  `geom_echf()` as a complete-data empirical cumulative-hazard display with a
  transformed DKW band.
* Added diagnostic layers `geom_ppplot()`, `geom_qqplot()`, and
  `geom_spplot()`, including support for fully specified null references and a
  clear fitted-null caveat for inferred parameters.
* Added censored-data layers: `geom_ecdf_km()` for Kaplan-Meier survival curves
  and `geom_echf_na()` for Nelson-Aalen cumulative hazards, with distinct
  confidence-display conventions for simultaneous bands versus pointwise
  intervals.
* Added `support` arguments to continuous theoretical distribution geoms so
  numerical integration, inversion, endpoint checks, and probability shading
  are separated from the visible `xlim` display window.
* Updated `geom_pdf()` probability shading so `p`, `p_lower`, and `p_upper`
  refer to distributional probabilities over `support` instead of mass
  renormalized over `xlim`.
* Added `hdr_xlim` for univariate PDF HDR shading and `hdr_xlim`/`hdr_ylim`
  controls for bivariate HDR delegation.
* `geom_echf_na()` draws pointwise Nelson-Aalen confidence intervals as gray
  error bars at event times by default (`conf_geom = "errorbar"`),
  visually distinguishing them from the simultaneous Kaplan-Meier ribbon band;
  `conf_geom = "ribbon"` and `conf_geom = "none"` remain available when users
  want a ribbon or no confidence display.
* Added manual benchmark and accuracy scripts under `inst/benchmarks/` for
  future manuscript evidence.
