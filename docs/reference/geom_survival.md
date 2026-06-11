# Plot a Survival Function S(x) = 1 - F(x)

`geom_survival()` creates a ggplot2 layer that plots a survival
function. By default only the line is drawn (no fill).

## Usage

``` r
geom_survival(
  mapping = NULL,
  data = NULL,
  stat = StatSurvival,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE,
  fun = NULL,
  cdf_fun = NULL,
  pdf_fun = NULL,
  qf_fun = NULL,
  xlim = NULL,
  n = 101,
  args = list(),
  color = "black"
)

StatSurvival

GeomSurvival
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  Ignored by `stat_function()`, do not use.

- stat:

  The statistical transformation to use on the data for this layer. When
  using a `geom_*()` function to construct a layer, the `stat` argument
  can be used to override the default coupling between geoms and stats.
  The `stat` argument accepts the following:

  - A `Stat` ggproto subclass, for example `StatCount`.

  - A string naming the stat. To give the stat as a string, strip the
    function name of the `stat_` prefix. For example, to use
    `stat_count()`, give the stat as `"count"`.

  - For more information and other ways to specify the stat, see the
    [layer
    stat](https://ggplot2.tidyverse.org/reference/layer_stats.html)
    documentation.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    `position_jitter()`. This method allows for passing extra arguments
    to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use `position_jitter()`, give the position as
    `"jitter"`.

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

  A survival function \\S(x)\\ returning values between 0 and 1 (e.g.
  `function(x) 1 - pnorm(x)`). Evaluated directly. Exactly one of `fun`,
  `cdf_fun`, `pdf_fun`, or `qf_fun` must be provided.

- cdf_fun:

  A CDF function (e.g. [pnorm](https://rdrr.io/r/stats/Normal.html)).
  The survival function is computed as `1 - cdf_fun(x)`. Exactly one of
  `fun`, `cdf_fun`, `pdf_fun`, or `qf_fun` must be provided.

- pdf_fun:

  A PDF function (e.g. [dnorm](https://rdrr.io/r/stats/Normal.html)).
  The CDF is derived by numerical integration and the survival function
  is computed as `1 - F(x)`. Exactly one of `fun`, `cdf_fun`, `pdf_fun`,
  or `qf_fun` must be provided.

- qf_fun:

  A quantile function (e.g.
  [qnorm](https://rdrr.io/r/stats/Normal.html)). The CDF is derived via
  interpolation and the survival function is computed as `1 - F(x)`.
  Exactly one of `fun`, `cdf_fun`, `pdf_fun`, or `qf_fun` must be
  provided.

- xlim:

  A numeric vector of length 2 giving the x-range.

- n:

  Number of points at which to evaluate. Defaults to 101.

- args:

  A named list of additional arguments to pass to `fun`, `cdf_fun`, or
  `pdf_fun`.

- color:

  Line color for the survival curve.

## Value

A ggplot2 layer.

## Details

Supply exactly one of `fun` (a survival function), `cdf_fun` (a CDF),
`pdf_fun` (a PDF), or `qf_fun` (a quantile function). When `cdf_fun` is
supplied, \\S(x) = 1 - F(x)\\. When `pdf_fun` is supplied, the CDF is
first derived by numerical integration and then \\S(x) = 1 - F(x)\\.
When `qf_fun` is supplied, the CDF is derived via interpolation and then
\\S(x) = 1 - F(x)\\.

## Computed variables

These are calculated by the `stat` part of the layer and can be accessed
with
[`ggplot2::after_stat()`](https://ggplot2.tidyverse.org/reference/aes_eval.html).

- `after_stat(x)`:

  Points at which the survival function is evaluated.

- `after_stat(y)`:

  Survival probabilities.

## Aesthetics

`geom_survival()` does not require any input aesthetics when a function
source is supplied. It understands the following aesthetics:

- Computed position aesthetics:

  `x` and `y`, mapped by default to `after_stat(x)` and `after_stat(y)`.

- Drawing aesthetics:

  `alpha`, `colour`/`color`, `group`, `linetype`, and `linewidth` for
  the line.

## See also

[`geom_cdf()`](/reference/geom_cdf.md),
[`geom_chf()`](/reference/geom_chf.md),
[`geom_hf()`](/reference/geom_hf.md),
[`geom_survival_discrete()`](/reference/geom_survival_discrete.md), and
[`geom_ecdf_km()`](/reference/geom_ecdf_km.md) for related survival and
distribution-function layers.

## Examples

``` r
  # Direct survival function
  ggplot() +
    geom_survival(fun = function(x) 1 - pnorm(x), xlim = c(-3, 3))


  # Via CDF
  ggplot() +
    geom_survival(cdf_fun = pnorm, xlim = c(-3, 3))


  ggplot() +
    geom_survival(cdf_fun = pexp, args = list(rate = 0.5), xlim = c(0, 10))

```
