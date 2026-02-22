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
  fun,
  n = 101,
  args = list()
)

StatQF
```

## Format

An object of class `StatQF` (inherits from `Stat`, `ggproto`, `gg`) of
length 2.

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

  A function to compute the quantile function (e.g.
  [qnorm](https://rdrr.io/r/stats/Normal.html)). The function must
  accept a numeric vector of probabilities (values in `[0,1]`) as its
  first argument.

- n:

  Number of probability points at which to evaluate `fun`. Defaults
  to 101. Points are placed at [Chebyshev
  nodes](https://en.wikipedia.org/wiki/Chebyshev_nodes) of the first
  kind on \$(0, 1)\$, which cluster near 0 and 1 where quantile
  functions are typically most curved, and never include the exact
  endpoints (avoiding \\\pm\infty\\ for unbounded distributions).

- args:

  A named list of additional arguments to pass to `fun`.

## Value

A ggplot2 layer.

## Examples

``` r
  ggplot() +
    geom_qf(fun = qnorm, args = list(mean = 3, sd = 2))


  ggplot() +
    geom_qf(fun = qbeta, args = list(shape1 = 3, shape2 = 4))

```
