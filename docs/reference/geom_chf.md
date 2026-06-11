# Plot a Cumulative Hazard Function H(x)

`geom_chf()` creates a ggplot2 layer that plots a cumulative hazard
function. By default only the line is drawn (no fill).

## Usage

``` r
geom_chf(
  mapping = NULL,
  data = NULL,
  stat = StatCHF,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE,
  fun = NULL,
  hf_fun = NULL,
  cdf_fun = NULL,
  pdf_fun = NULL,
  survival_fun = NULL,
  qf_fun = NULL,
  hf_lower = -Inf,
  xlim = NULL,
  n = 101,
  args = list()
)

StatCHF

GeomCHF
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

  A cumulative hazard function \\H(x)\\ (e.g.
  `function(x) -log(1 - pnorm(x))`). Evaluated directly. Exactly one of
  `fun`, `hf_fun`, `cdf_fun`, `pdf_fun`, `survival_fun`, or `qf_fun`
  must be provided.

- hf_fun:

  A hazard function (e.g. a Weibull hazard). The cumulative hazard is
  derived by numerical integration. Exactly one of `fun`, `hf_fun`,
  `cdf_fun`, `pdf_fun`, `survival_fun`, or `qf_fun` must be provided.

- cdf_fun:

  A CDF function (e.g. [pnorm](https://rdrr.io/r/stats/Normal.html)).
  The cumulative hazard is computed as \\H(x) = -\log(1 - F(x))\\.
  Exactly one of `fun`, `hf_fun`, `cdf_fun`, `pdf_fun`, `survival_fun`,
  or `qf_fun` must be provided.

- pdf_fun:

  A PDF function (e.g. [dnorm](https://rdrr.io/r/stats/Normal.html)).
  The CDF is derived by numerical integration and then \\H(x) =
  -\log(1 - F(x))\\. Exactly one of `fun`, `hf_fun`, `cdf_fun`,
  `pdf_fun`, `survival_fun`, or `qf_fun` must be provided.

- survival_fun:

  A survival function (e.g. `function(x) 1 - pnorm(x)`). The cumulative
  hazard is computed as \\H(x) = -\log(S(x))\\. Exactly one of `fun`,
  `hf_fun`, `cdf_fun`, `pdf_fun`, `survival_fun`, or `qf_fun` must be
  provided.

- qf_fun:

  A quantile function (e.g.
  [qnorm](https://rdrr.io/r/stats/Normal.html)). The CDF is derived via
  interpolation and then \\H(x) = -\log(1 - F(x))\\. Exactly one of
  `fun`, `hf_fun`, `cdf_fun`, `pdf_fun`, `survival_fun`, or `qf_fun`
  must be provided.

- hf_lower:

  Lower limit for integrating `hf_fun`. Defaults to `-Inf`. For
  finite-support hazards, set this to the lower support point (for
  example, `0` for Weibull or exponential hazards); values below
  `hf_lower` return cumulative hazard `0`.

- xlim:

  A numeric vector of length 2 giving the x-range.

- n:

  Number of points at which to evaluate. Defaults to 101.

- args:

  A named list of additional arguments to pass to `fun`, `hf_fun`,
  `cdf_fun`, `pdf_fun`, `survival_fun`, or `qf_fun`.

## Value

A ggplot2 layer.

## Details

Supply exactly one of `fun` (a cumulative hazard function, evaluated
directly), `hf_fun` (a hazard function, integrated numerically),
`cdf_fun` (a CDF), `pdf_fun` (a PDF), `survival_fun` (a survival
function), or `qf_fun` (a quantile function).

## Computed variables

These are calculated by the `stat` part of the layer and can be accessed
with
[`ggplot2::after_stat()`](https://ggplot2.tidyverse.org/reference/aes_eval.html).

- `after_stat(x)`:

  Points at which the cumulative hazard is evaluated.

- `after_stat(y)`:

  Cumulative hazard values.

## Aesthetics

`geom_chf()` does not require any input aesthetics when a function
source is supplied. It understands the following aesthetics:

- Computed position aesthetics:

  `x` and `y`, mapped by default to `after_stat(x)` and `after_stat(y)`.

- Drawing aesthetics:

  `alpha`, `colour`/`color`, `group`, `linetype`, and `linewidth` for
  the line.

## See also

[`geom_hf()`](/reference/geom_hf.md),
[`geom_survival()`](/reference/geom_survival.md),
[`geom_cdf()`](/reference/geom_cdf.md), and
[`geom_echf()`](/reference/geom_echf.md) for related hazard and
survival-function layers.

## Examples

``` r
  # Via CDF
  ggplot() +
    geom_chf(cdf_fun = pexp, args = list(rate = 0.5), xlim = c(0, 10))


  # Via hazard function (constant hazard = exponential)
  h_const <- function(x, rate) ifelse(x >= 0, rate, 0)
  ggplot() +
    geom_chf(hf_fun = h_const, args = list(rate = 0.5), xlim = c(0, 10))


  # Via survival function
  ggplot() +
    geom_chf(survival_fun = function(x) 1 - pnorm(x), xlim = c(-3, 3))

```
