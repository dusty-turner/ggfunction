# ggfunction (development version)

## Correctness overhaul

This release implements the July 2026 correctness review: a package-wide
coordinate contract, exact probability computations, structural input
validation, and repaired survival/diagnostic statistics. Externally visible
changes are listed below.

### Transformed scales and the coordinate contract

* All public `xlim`, `ylim`, `support`, `support_x`, and `support_y`
  arguments are now interpreted in **data coordinates**. Under a transformed
  position scale (log, log10, reverse, ...), evaluation grids are built
  evenly in panel (transformed) space, functions are evaluated at the
  inverse-transformed data-space values, and output positions are
  transformed exactly once. This intentionally differs from
  `ggplot2::stat_function()`, which treats its `xlim` as transformed
  scale-space: `geom_function_1d_1d(fun = identity, xlim = c(1, 100)) +
  scale_x_log10()` now evaluates at data values 1..100 (panel positions
  0..2), where `stat_function()` would evaluate at 10^1..10^100.
* Generated-position stats retain raw evaluation values alongside panel
  positions as computed variables: `x_eval`/`y_eval` (evaluation points),
  `y_raw`, `density`, `mass`, `cdf`, `survival`, `hazard`, `cumhazard`,
  `q`, and `sample`, accessible via `after_stat()`.
* 2D grids (rasters, contours, PMF lattices) are panel-uniform under
  transformed scales; vector- and stream-field types
  (`geom_function_2d_2d()`), which delegate to ggvfields in raw
  coordinates, now reject transformed position scales with a clear error
  instead of drawing an incorrect plot.
* Mathematical baselines and probability endpoints (density/mass/hazard
  zero, probability 0 and 1) train scales only when finite under the active
  transformation; otherwise they are retained as metadata and
  baseline-anchored geometry is clipped to the visible panel boundary with
  one documented warning (e.g. under `scale_y_log10()`).
* `dt` in `geom_function_1d_2d()` is now an exact positive step magnitude:
  evaluations are exactly `dt` apart, the terminal endpoint is appended when
  the span is not an exact multiple, reversed `tlim` steps backwards, and
  invalid values error at construction. Previously `dt` was converted to an
  approximate point count.

### Probability computation fixes

* CDF/PDF/survival shading boundaries are now exact distributional
  quantiles computed once per group in the Stat from raw probabilities —
  independent of grid resolution and y scale. Raw boundaries are exposed as
  `shade_x_lower_raw`/`shade_x_upper_raw` (with `shade_lower`/`shade_upper`
  for `geom_pdf()`); a boundary outside `xlim` keeps its exact metadata and
  clips visually instead of being clamped to the window edge (the previous
  "not reached" warning is gone because nothing is misdrawn anymore).
* Function-only `geom_cdf()` layers now have a finite default evaluation
  range with documented precedence (explicit `xlim`, then a trained scale
  window, then finite `support`, then the panel fallback `c(0, 1)`); they
  previously warned and drew nothing.
* Hazard conversions are numerically stable deep in the tail: the
  survival route uses the logarithmic derivative, PDF routes divide by an
  upper-tail integral, and a CDF-only route warns when `F(x)` has rounded
  to 1 (`as_hf_1d(survival_fun = function(x) exp(-x))` is now exactly 1 at
  x = 40 instead of 0). Hazard-derived CHF/CDF/survival/PDF values are exact
  at support endpoints (`H = Inf`, `F = 1`, `S = 0`, density 0) without
  spurious integration warnings, and `hf_lower` is validated against the
  upper support endpoint.
* The direct discrete quantile-function route recovers exact cumulative
  boundaries by monotone bisection, so rare atoms (e.g.
  `qbinom(size = 1, prob = 0.99995)`) are no longer dropped; bounded integer
  support is inferred from `Q(0)`/`Q(1)` up to an internal 10,000-point cap
  (supply `support`, `pmf_fun`, or `cdf_fun` beyond it); zero-mass support
  points are dropped from QF geometry; and an unverified observed maximum is
  no longer pinned to probability 1.
* Narrowed discrete windows (`xlim` inside `support`) preserve true
  predecessor values: the first visible CDF step starts at F of the
  preceding support point rather than 0, survival at S rather than 1, and
  the QF at its true left boundary.

### Validation (structural checks always abort)

* PMFs are evaluated exactly once per Stat computation and structurally
  validated: numeric output, one value per support point, finite,
  non-negative, positive total. PMF-derived cumulative routes (discrete
  CDF/survival/QF) additionally require the declared support to carry total
  mass 1 within 1e-8 — supply the full computational `support` and use
  `xlim` only as the display window. These structural checks cannot be
  disabled with `options(ggfunction.check = FALSE)`.
* Direct discrete survival and CDF sources are strictly validated (finite,
  within [0, 1] with roundoff clamping, monotone); violations abort instead
  of drawing silently.
* Probability shading arguments share one strict validator across geoms:
  scalar `p` in (0, 1), `p_lower`/`p_upper` supplied together with
  `p_lower < p_upper`, no `p` alongside the pair, and
  `shade_outside = TRUE` only with a complete pair.
* Plotting-position offsets (`a`) are validated after `ppoints()`;
  `band_max` must be `NULL`, `Inf`, or one finite non-negative number.

### Survival and empirical statistics

* Logical and integer 0/1 `status` now produce identical Kaplan-Meier and
  Nelson-Aalen results: status is normalized before ggplot2's implicit
  grouping, so a logical status no longer splits the risk set into
  event-only and censor-only curves. Factor and character status vectors
  are rejected with a clear error instead of being interpreted through
  their integer codes.
* Constructor-local mappings (e.g. `geom_ecdf(aes(x = z))`,
  `geom_ecdf_km(aes(x = time, status = status))`) now reach every auxiliary
  layer (bands, censor marks, intervals) and produce the same statistics as
  plot-global mappings.
* The equal-precision KM confidence band no longer draws a false zero-width
  interval at a terminal event where the Greenwood variance is singular.
  `geom_ecdf_km()` gained `ep_range = c(a_L, a_U)` to prespecify the
  equal-precision domain; the default remains a data-adaptive plug-in band,
  now documented as approximate. Invalid domains omit the band with a
  warning or error directly — never a pointwise-normal fallback.
* All-censored data now draw S(t) = 1 (KM) and H(t) = 0 (Nelson-Aalen)
  over the observed follow-up instead of nothing; curves and bands extend
  horizontally through trailing censoring; each grouped curve stops at its
  own maximum follow-up; and censor marks no longer change the trained
  x domain.
* `band_max = Inf` in `geom_echf()` now genuinely disables the upper cap:
  where `F_n + eps >= 1` the upper band is exactly infinite, retained in
  the built data, and rendered at the visible panel edge.
* Probability-valued geoms (CDF/ECDF/survival/KM, and the QF/PP/SP
  probability axes) train their scales on the transform-valid mathematical
  endpoints 0 and 1, including in narrowed windows; cumulative hazards
  train on raw zero when the transformation allows it.

### Diagnostic (PP/SP/QQ) plots

* The canonical input aesthetic is now the non-positional
  `aes(sample = )` (as in `ggplot2::stat_qq()`), so the null CDF/quantile
  function always receives raw observations even under transformed output
  scales. Legacy `aes(x = )` still works on identity x scales with a
  deprecation warning and aborts under a transformed x scale.
* PP/SP confidence bands are continuous-null procedures and now require an
  explicit declaration: `geom_ppplot()`/`geom_spplot()` gained
  `null_type` (`"continuous"`/`"discrete"`); a band request without it, or
  with a discrete null, errors at construction. The point diagnostic with
  `conf_int = FALSE` needs no declaration. QQ plots are unaffected.

### Other user-visible changes

* `geom_pdf_2d()` now requires a finite function domain for a
  function-only layer (`xlim`/`ylim`, or `hdr_xlim`/`hdr_ylim` for HDR
  types); the raster type previously defaulted silently to `[-1, 1]^2`
  while HDR types warned and drew nothing.
* Contour scalar fields (`geom_function_2d_1d()`) populate every facet
  with an equivalent grid, accept precomputed `x`/`y`/`z` data with
  `fun = NULL`, and preserve auxiliary mapped aesthetics; raster mode maps
  `fill = after_stat(z)` by default in both `geom_function_2d_1d()` and
  `stat_function_2d_1d()`, retaining it under auxiliary mappings.
* `geom_function_1d_1d()` honors `colour`/`color` and mapped colour/fill
  (fixed defaults moved into the geom); shading inserts exact
  `shade_from`/`shade_to` evaluation rows and trains the zero baseline;
  fill that varies within one shaded group errors clearly.
* Unshaded discrete pieces dim multiplicatively (0.3 x the resolved alpha),
  so a low user alpha can never render dimmed pieces more opaque than
  highlighted ones; HDR labels use an adaptive-precision formatter so close
  coverages (e.g. 0.5001 and 0.5004) get distinct labels.
* Overriding a package-added default mapping with a static aesthetic no
  longer triggers a duplicated-aesthetic warning; genuinely user-duplicated
  aesthetics keep ggplot2's native diagnostics.
* Attaching ggfunction no longer touches the RNG (`.Random.seed`); the
  citation reminder is deterministic and can be silenced with
  `options(ggfunction.quiet = TRUE)`.
* ggfunction now requires ggplot2 >= 4.0.0 (the package already relied on
  scale-transformation accessors introduced after the previously declared
  minimum).

## Earlier development changes

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
