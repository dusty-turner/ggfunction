# Plot a Hazard Function h(x) = f(x) / S(x)

`geom_hf()` creates a ggplot2 layer that plots a hazard function. Two
interfaces are supported:

## Usage

``` r
geom_hf(
  mapping = NULL,
  data = NULL,
  stat = StatHF,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE,
  fun = NULL,
  pdf_fun = NULL,
  cdf_fun = NULL,
  xlim = NULL,
  n = 101,
  args = list(),
  pdf_args = NULL,
  cdf_args = NULL,
  color = "black"
)

StatHF

GeomHF
```

## Format

An object of class `StatHF` (inherits from `Stat`, `ggproto`, `gg`) of
length 3.

An object of class `GeomHF` (inherits from `GeomPath`, `Geom`,
`ggproto`, `gg`) of length 2.

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

  Logical. Should this layer be included in the legends? `NA`, the
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

  A hazard function \\h(x)\\ (optional). When supplied, `pdf_fun` and
  `cdf_fun` must not be provided.

- pdf_fun:

  A PDF function (e.g. [dnorm](https://rdrr.io/r/stats/Normal.html)).
  Required when `fun` is not supplied.

- cdf_fun:

  A CDF function (e.g. [pnorm](https://rdrr.io/r/stats/Normal.html)).
  Required when `fun` is not supplied.

- xlim:

  A numeric vector of length 2 giving the x-range.

- n:

  Number of points at which to evaluate. Defaults to 101.

- args:

  A named list of arguments passed to `fun`, or shared by both `pdf_fun`
  and `cdf_fun`.

- pdf_args:

  A named list of additional arguments specific to `pdf_fun` (overrides
  `args`). Ignored when using the direct hazard interface.

- cdf_args:

  A named list of additional arguments specific to `cdf_fun` (overrides
  `args`). Ignored when using the direct hazard interface.

- color:

  Line color for the hazard curve.

## Value

A ggplot2 layer.

## Details

- **PDF + CDF interface**: supply `pdf_fun` and `cdf_fun`; the hazard is
  computed internally as \\h(x) = f(x) / (1 - F(x))\\.

- **Direct hazard interface**: supply `fun`, a function that returns
  \\h(x)\\ directly (e.g. a closed-form expression).

Exactly one of these two interfaces must be used. By default only the
line is drawn (no fill).

## Examples

``` r
  # PDF + CDF interface
  ggplot() +
    geom_hf(pdf_fun = dnorm, cdf_fun = pnorm, xlim = c(-3, 3))


  ggplot() +
    geom_hf(pdf_fun = dexp, cdf_fun = pexp,
      args = list(rate = 0.5), xlim = c(0, 10))


  # Direct hazard interface (Weibull closed-form hazard)
  h_weibull <- function(x, shape, scale) (shape / scale) * (x / scale)^(shape - 1)
  ggplot() +
    geom_hf(fun = h_weibull, xlim = c(0.01, 5),
      args = list(shape = 0.5, scale = 2))

```
