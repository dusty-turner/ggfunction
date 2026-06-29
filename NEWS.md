# ggfunction 0.1.0

* Added centralized internal conversion helpers for continuous distribution
  routes among PDFs, CDFs, survival functions, quantile functions, hazards, and
  cumulative hazards.
* Added `support` arguments to continuous theoretical distribution geoms so
  numerical integration, inversion, endpoint checks, and probability shading
  are separated from the visible `xlim` display window.
* Updated `geom_pdf()` probability shading so `p`, `p_lower`, and `p_upper`
  refer to distributional probabilities over `support` instead of mass
  renormalized over `xlim`.
* Added `hdr_xlim` for univariate PDF HDR shading and `hdr_xlim`/`hdr_ylim`
  controls for bivariate HDR delegation.
* Added manual benchmark and accuracy scripts under `inst/benchmarks/` for
  future manuscript evidence.

