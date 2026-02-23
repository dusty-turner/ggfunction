# Plot an Empirical Quantile Function

`geom_eqf()` computes the empirical quantile function of a sample and
renders it as a left-continuous step function on \\\[0, 1\]\\, using the
same visual conventions as
[`geom_qf_discrete()`](/reference/geom_qf_discrete.md): horizontal
segments, dashed vertical jumps, closed circles at the bottom of each
jump (value achieved), and open circles at the top (next value not yet
reached). An optional simultaneous confidence band is drawn using the
Kolmogorov-Smirnov construction.

## Usage

``` r
geom_eqf(
  mapping = NULL,
  data = NULL,
  stat = StatEQF,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  open_fill = NULL,
  vert_type = "dashed",
  show_points = NULL,
  show_vert = NULL,
  conf_int = TRUE,
  level = 0.95,
  conf_alpha = 0.4
)

StatEQF

StatEQFBand
```

## Format

An object of class `StatEQF` (inherits from `Stat`, `ggproto`, `gg`) of
length 3.

An object of class `StatEQFBand` (inherits from `Stat`, `ggproto`, `gg`)
of length 3.

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  - `NULL` (default): the data is inherited from the plot data as
    specified in the call to
    [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  - A `data.frame`, or other object, will override the plot data. All
    objects will be fortified to produce a data frame. See
    [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
    for which variables will be created.

  - A `function` will be called with a single argument, the plot data.
    The return value must be a `data.frame`, and will be used as the
    layer data. A `function` can be created from a `formula` (e.g.
    `~ head(.x, 10)`).

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

  If `TRUE`, silently remove missing values. Defaults to `FALSE`.

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

- open_fill:

  Fill color for the open (hollow) endpoint circles. Defaults to `NULL`,
  which uses the active theme's panel background color.

- vert_type:

  Line type for the vertical jump segments. Defaults to `"dashed"`.

- show_points:

  Logical. If `FALSE`, suppresses all endpoint circles. If `NULL` (the
  default), circles are shown when there are 50 or fewer points and
  hidden otherwise.

- show_vert:

  Logical. If `FALSE`, suppresses the vertical jump segments. If `NULL`
  (the default), segments are shown when there are 50 or fewer points
  and hidden otherwise.

- conf_int:

  Logical. If `TRUE` (the default), draws a simultaneous KS confidence
  band around the ECDF.

- level:

  Confidence level for the band. Defaults to `0.95`.

- conf_alpha:

  Alpha (transparency) of the confidence ribbon. Defaults to `0.4`.

## Value

A ggplot2 layer, or a list of two layers when `conf_int = TRUE`.

## Details

The empirical quantile function is the left-continuous inverse of the
empirical CDF: \\Q(p) = \inf\\x : F_n(x) \geq p\\\\.

The confidence band at probability level \\p\\ is \\\[Q_n(p -
\varepsilon),\\ Q_n(p + \varepsilon)\]\\, where \\\varepsilon =
\sqrt{\log(2/\alpha) / (2n)}\\ is the KS half-width (\\\alpha = 1 -
\texttt{level}\\). This follows directly from inverting the simultaneous
ECDF confidence band.

## Examples

``` r
set.seed(1)
df <- data.frame(x = rnorm(50))

ggplot(df, aes(x = x)) + geom_eqf()


# Compare two groups
df2 <- data.frame(
  x     = c(rnorm(40), rnorm(40, mean = 2)),
  group = rep(c("A", "B"), each = 40)
)
ggplot(df2, aes(x = x, colour = group)) + geom_eqf()

```
