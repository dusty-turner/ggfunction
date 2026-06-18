# Plot a Cumulative Distribution Function

`geom_cdf()` creates a ggplot2 layer that plots a cumulative
distribution function (CDF) as a line. You can optionally shade a region
by specifying a cumulative probability threshold via `p`, or a two-sided
interval via `p_lower` and `p_upper`.

## Usage

``` r
geom_cdf(
  mapping = NULL,
  data = NULL,
  stat = StatCDF,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE,
  fun = NULL,
  pdf_fun = NULL,
  survival_fun = NULL,
  qf_fun = NULL,
  hf_fun = NULL,
  hf_lower = -Inf,
  xlim = NULL,
  n = 101,
  args = list(),
  fill = "grey20",
  color = "black",
  p = NULL,
  lower.tail = TRUE,
  p_lower = NULL,
  p_upper = NULL,
  check = TRUE,
  check_tol = 0.01
)

StatCDF

GeomCDF
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  Ignored by
  [`stat_function()`](https://ggplot2.tidyverse.org/reference/geom_function.html),
  do not use.

- stat:

  The statistical transformation to use on the data for this layer. When
  using a `geom_*()` function to construct a layer, the `stat` argument
  can be used to override the default coupling between geoms and stats.
  The `stat` argument accepts the following:

  - A `Stat` ggproto subclass, for example `StatCount`.

  - A string naming the stat. To give the stat as a string, strip the
    function name of the `stat_` prefix. For example, to use
    [`stat_count()`](https://ggplot2.tidyverse.org/reference/geom_bar.html),
    give the stat as `"count"`.

  - For more information and other ways to specify the stat, see the
    [layer
    stat](https://ggplot2.tidyverse.org/reference/layer_stats.html)
    documentation.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- ...:

  Other parameters passed on to
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html).

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

- fun:

  A function to compute the CDF (e.g.
  [pnorm](https://rdrr.io/r/stats/Normal.html)). The function must
  accept a numeric vector as its first argument and return values
  between 0 and 1. Exactly one of `fun`, `pdf_fun`, `survival_fun`,
  `qf_fun`, or `hf_fun` must be provided.

- pdf_fun:

  A PDF function (e.g. [dnorm](https://rdrr.io/r/stats/Normal.html)).
  When supplied, the CDF is derived numerically via integration. Exactly
  one of `fun`, `pdf_fun`, `survival_fun`, `qf_fun`, or `hf_fun` must be
  provided.

- survival_fun:

  A survival function (e.g. `function(x) 1 - pnorm(x)`). When supplied,
  the CDF is computed as \\F(x) = 1 - S(x)\\. Exactly one of `fun`,
  `pdf_fun`, `survival_fun`, `qf_fun`, or `hf_fun` must be provided.

- qf_fun:

  A quantile function (e.g.
  [qnorm](https://rdrr.io/r/stats/Normal.html)). When supplied, the CDF
  is derived via interpolation on a dense grid. Exactly one of `fun`,
  `pdf_fun`, `survival_fun`, `qf_fun`, or `hf_fun` must be provided.

- hf_fun:

  A hazard function (e.g. a Weibull hazard). When supplied, the CDF is
  derived via numerical integration of the cumulative hazard as \\F(x) =
  1 - \exp(-H(x))\\. Exactly one of `fun`, `pdf_fun`, `survival_fun`,
  `qf_fun`, or `hf_fun` must be provided.

- hf_lower:

  Lower limit for integrating `hf_fun`. Defaults to `-Inf`. For
  finite-support hazards, set this to the lower support point (for
  example, `0` for Weibull or exponential hazards); values below
  `hf_lower` return CDF `0`.

- xlim:

  A numeric vector of length 2 specifying the x-range over which to
  evaluate the CDF.

- n:

  Number of points at which to evaluate `fun`.

- args:

  A named list of additional arguments passed on to `fun`.

- fill:

  Fill color for the shaded area.

- color:

  Line color for the CDF curve.

- p:

  (Optional) A numeric value between 0 and 1 specifying the threshold
  value of the CDF. The area will be shaded up until (if
  `lower.tail = TRUE`) or from (if `lower.tail = FALSE`) the point where
  the CDF reaches this value.

- lower.tail:

  Logical; if `TRUE` (the default) shading is applied from the left end
  of the curve up to the threshold; if `FALSE`, shading is applied from
  the threshold to the right end.

- p_lower:

  (Optional) A numeric value between 0 and 1 specifying the lower CDF
  threshold for two-sided shading. Used with `p_upper`.

- p_upper:

  (Optional) A numeric value between 0 and 1 specifying the upper CDF
  threshold for two-sided shading. Used with `p_lower`.

- check:

  Logical; if `TRUE`, issue a diagnostic when the CDF is not near 0 and
  1 at the lower and upper ends of the drawn x-range. Use `FALSE` to
  suppress this check.

- check_tol:

  Numeric tolerance used by the CDF endpoint check.

## Value

A ggplot2 layer.

## Computed variables

These are calculated by the `stat` part of the layer and can be accessed
with
[`ggplot2::after_stat()`](https://ggplot2.tidyverse.org/reference/aes_eval.html).

- `after_stat(x)`:

  Points at which the CDF is evaluated.

- `after_stat(y)`:

  CDF values.

- `after_stat(p)`:

  CDF values; the default y aesthetic maps to this variable.

## Aesthetics

`geom_cdf()` does not require any input aesthetics when a function
source is supplied. It understands the following aesthetics:

- Computed position aesthetics:

  `x` and `y`, mapped by default to `after_stat(x)` and `after_stat(p)`.

- Drawing aesthetics:

  `alpha`, `colour`/`color`, `fill`, `group`, `linetype`, and
  `linewidth` for the area and outline.

## See also

[`geom_pdf()`](/reference/geom_pdf.md),
[`geom_qf()`](/reference/geom_qf.md),
[`geom_survival()`](/reference/geom_survival.md),
[`geom_hf()`](/reference/geom_hf.md),
[`geom_chf()`](/reference/geom_chf.md), and
[`geom_cdf_discrete()`](/reference/geom_cdf_discrete.md) for related
distribution-function layers.

## Examples

``` r
  # Plot the standard normal CDF, shading up to the 97.5th percentile.
  ggplot() +
    geom_cdf(fun = pnorm, xlim = c(-3, 3), p = 0.975, lower.tail = TRUE)


  # Parameterized via `args`
  ggplot() +
    geom_cdf(fun = pexp, xlim = c(0, 10), args = list(rate = 0.5))

```
