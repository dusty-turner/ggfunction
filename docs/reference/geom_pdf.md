# Plot a Probability Density Function with a Filled Area

`geom_pdf()` computes a probability density function and plots it as a
filled area. This function is similar to
[`ggplot2::geom_function()`](https://ggplot2.tidyverse.org/reference/geom_function.html),
but it shades the area corresponding to a given proportion of the total
density.

## Usage

``` r
geom_pdf(
  mapping = NULL,
  data = NULL,
  stat = StatPDF,
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
  linewidth = NULL,
  alpha = 0.35,
  p = NULL,
  lower.tail = TRUE,
  p_lower = NULL,
  p_upper = NULL,
  shade_outside = FALSE,
  shade_hdr = NULL
)

StatPDF

GeomPDF
```

## Format

An object of class `StatPDF` (inherits from `Stat`, `ggproto`, `gg`) of
length 3.

An object of class `GeomPDF` (inherits from `GeomArea`, `GeomRibbon`,
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

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html)'s
  `params` argument. These arguments broadly fall into one of 4
  categories below. Notably, further arguments to the `position`
  argument, or aesthetics that are required can *not* be passed through
  `...`. Unknown arguments that are not part of the 4 categories below
  are ignored.

  - Static aesthetics that are not mapped to a scale, but are at a fixed
    value and apply to the layer as a whole. For example,
    `colour = "red"` or `linewidth = 3`. The geom's documentation has an
    **Aesthetics** section that lists the available options. The
    'required' aesthetics cannot be passed on to the `params`. Please
    note that while passing unmapped aesthetics as vectors is
    technically possible, the order and required length is not
    guaranteed to be parallel to the input data.

  - When constructing a layer using a `stat_*()` function, the `...`
    argument can be used to pass on parameters to the `geom` part of the
    layer. An example of this is
    `stat_density(geom = "area", outline.type = "both")`. The geom's
    documentation lists which parameters it can accept.

  - Inversely, when constructing a layer using a `geom_*()` function,
    the `...` argument can be used to pass on parameters to the `stat`
    part of the layer. An example of this is
    `geom_area(stat = "density", adjust = 0.5)`. The stat's
    documentation lists which parameters it can accept.

  - The `key_glyph` argument of
    [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html) may
    also be passed on through `...`. This can be one of the functions
    described as [key
    glyphs](https://ggplot2.tidyverse.org/reference/draw_key.html), to
    change the display of the layer in the legend.

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

  A function to compute the density (e.g.
  [dnorm](https://rdrr.io/r/stats/Normal.html)). The function must
  accept a numeric vector as its first argument and return density
  values that integrate (approximately) to 1.

- xlim:

  A numeric vector of length 2 giving the x-range over which to evaluate
  the PDF.

- n:

  (defaults to 101)Number of points at which to evaluate `fun`.

- args:

  A named list of additional arguments to pass to `fun`.

- fill:

  Fill color for the shaded area.

- color:

  Line color for the outline of the density curve.

- linewidth:

  Line width for the outline of the density curve.

- alpha:

  Alpha transparency for the shaded area.

- p:

  (Optional) A numeric value between 0 and 1 specifying the cumulative
  probability threshold. The area will be shaded up until the point
  where the cumulative density reaches this value.

- lower.tail:

  Logical; if `TRUE` (the default) the shaded area extends from the left
  end of the density up to the threshold. If `FALSE`, the shading
  extends from the threshold to the right end.

- p_lower:

  (Optional) A numeric value between 0 and 1 specifying the lower
  cumulative probability bound. Used with `p_upper` for two-sided
  shading.

- p_upper:

  (Optional) A numeric value between 0 and 1 specifying the upper
  cumulative probability bound. Used with `p_lower` for two-sided
  shading.

- shade_outside:

  Logical; if `FALSE` (the default) shading is applied between `p_lower`
  and `p_upper`. If `TRUE`, shading is applied to the tails outside that
  range.

- shade_hdr:

  (Optional) A numeric value between 0 and 1 specifying the coverage of
  the [highest density
  region](https://en.wikipedia.org/wiki/Credible_interval#Highest_density_interval)
  (HDR) to shade. The HDR is the smallest region of the domain
  containing the specified probability mass; for multimodal densities it
  may be disconnected, producing multiple shaded intervals. Computed
  following the approach of
  [doi:10.32614/RJ-2023-048](https://doi.org/10.32614/RJ-2023-048) :
  density values are evaluated on the grid, normalized to sum to 1,
  sorted in descending order, and cumulated until the target coverage is
  reached; the density at that threshold determines which regions are
  shaded. Takes precedence over `p`, `p_lower`, and `p_upper` if
  specified.

## Value

A ggplot2 layer.

## Examples

``` r
ggplot() +
  geom_pdf(fun = dnorm, xlim = c(-3, 3), p = .975, lower.tail = TRUE)


# Highest density region of a bimodal density
f_bim <- function(x) 0.5 * dnorm(x, -2, 0.5) + 0.5 * dnorm(x, 2, 0.5)
ggplot() +
  geom_pdf(fun = f_bim, xlim = c(-4, 4), shade_hdr = 0.9)

```
