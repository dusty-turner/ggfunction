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
  fun,
  xlim = NULL,
  n = 101,
  args = list(),
  fill = "grey20",
  color = "black",
  p = NULL,
  lower.tail = TRUE,
  p_lower = NULL,
  p_upper = NULL
)

StatCDF

GeomCDF
```

## Format

An object of class `StatCDF` (inherits from `Stat`, `ggproto`, `gg`) of
length 3.

An object of class `GeomCDF` (inherits from `GeomArea`, `GeomRibbon`,
`Geom`, `ggproto`, `gg`) of length 2.

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

  A function to compute the CDF (e.g.
  [pnorm](https://rdrr.io/r/stats/Normal.html)). The function must
  accept a numeric vector as its first argument and return values
  between 0 and 1.

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

## Value

A ggplot2 layer.

## Examples

``` r
  # Plot the standard normal CDF, shading up to the 97.5th percentile.
  ggplot() +
    geom_cdf(fun = pnorm, xlim = c(-3, 3), p = 0.975, lower.tail = TRUE)

```
