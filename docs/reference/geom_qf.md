# Plot a Quantile Function

`geom_qf()` creates a ggplot2 layer that plots a quantile function
(inverse CDF) as a line. It computes quantile values for a sequence of
probabilities (from 0 to 1) and connects them with a line.

## Usage

``` r
geom_qf(
  mapping = NULL,
  data = NULL,
  stat = StatQF,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  fun = NULL,
  cdf_fun = NULL,
  pdf_fun = NULL,
  survival_fun = NULL,
  n = 101,
  args = list()
)

StatQF
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

  A function to compute the quantile function (e.g.
  [qnorm](https://rdrr.io/r/stats/Normal.html)). The function must
  accept a numeric vector of probabilities (values in `[0,1]`) as its
  first argument. Exactly one of `fun`, `cdf_fun`, `pdf_fun`, or
  `survival_fun` must be provided.

- cdf_fun:

  A CDF function (e.g. [pnorm](https://rdrr.io/r/stats/Normal.html)).
  The quantile function is derived numerically via root-finding. Exactly
  one of `fun`, `cdf_fun`, `pdf_fun`, or `survival_fun` must be
  provided.

- pdf_fun:

  A PDF function (e.g. [dnorm](https://rdrr.io/r/stats/Normal.html)).
  The CDF is first derived by numerical integration, then the quantile
  function by root-finding. Exactly one of `fun`, `cdf_fun`, `pdf_fun`,
  or `survival_fun` must be provided.

- survival_fun:

  A survival function (e.g. `function(x) 1 - pnorm(x)`). The CDF is
  computed as \\F(x) = 1 - S(x)\\ and then the quantile function is
  derived by root-finding. Exactly one of `fun`, `cdf_fun`, `pdf_fun`,
  or `survival_fun` must be provided.

- n:

  Number of probability points at which to evaluate `fun`. Defaults
  to 101. Points are placed at [Chebyshev
  nodes](https://en.wikipedia.org/wiki/Chebyshev_nodes) of the first
  kind on \$(0, 1)\$, which cluster near 0 and 1 where quantile
  functions are typically most curved, and never include the exact
  endpoints (avoiding \\\pm\infty\\ for unbounded distributions).

- args:

  A named list of additional arguments to pass to `fun`, `cdf_fun`, or
  `pdf_fun`.

## Value

A ggplot2 layer.

## Details

Supply exactly one of `fun` (a quantile function), `cdf_fun` (a CDF),
`pdf_fun` (a PDF), or `survival_fun` (a survival function). When
`cdf_fun` is supplied, the quantile function is derived by numerical
root-finding. When `pdf_fun` is supplied, the CDF is first derived by
numerical integration and then inverted. When `survival_fun` is
supplied, the CDF is computed as \\F(x) = 1 - S(x)\\ and then inverted.

## Computed variables

These are calculated by the `stat` part of the layer and can be accessed
with
[`ggplot2::after_stat()`](https://ggplot2.tidyverse.org/reference/aes_eval.html).

- `after_stat(p)`:

  Probability values at which the quantile function is evaluated.

- `after_stat(x)`:

  Quantile values; the default y aesthetic maps to this variable.

- `after_stat(q)`:

  Quantile values.

## Aesthetics

`geom_qf()` does not require any input aesthetics when a function source
is supplied. It understands the following aesthetics:

- Computed position aesthetics:

  `x` and `y`, mapped by default to `after_stat(p)` and `after_stat(x)`.

- Drawing aesthetics:

  `alpha`, `colour`/`color`, `group`, `linetype`, and `linewidth` for
  the line.

## See also

[`geom_cdf()`](/reference/geom_cdf.md),
[`geom_pdf()`](/reference/geom_pdf.md), and
[`geom_qf_discrete()`](/reference/geom_qf_discrete.md) for related
quantile and distribution-function layers.

## Examples

``` r
  ggplot() +
    geom_qf(fun = qnorm, args = list(mean = 3, sd = 2))


  ggplot() +
    geom_qf(fun = qbeta, args = list(shape1 = 3, shape2 = 4))

```
