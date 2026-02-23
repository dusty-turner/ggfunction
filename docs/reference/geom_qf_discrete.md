# Plot a Discrete Quantile Function as a Step Function

`geom_qf_discrete()` renders a discrete quantile function as a
left-continuous step function with horizontal segments, dashed vertical
jumps, closed circles at the lower limit of each jump, and open circles
at the upper limit.

## Usage

``` r
geom_qf_discrete(
  mapping = NULL,
  data = NULL,
  stat = StatQFDiscrete,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE,
  fun = NULL,
  pmf_fun = NULL,
  xlim = NULL,
  support = NULL,
  args = list(),
  open_fill = NULL,
  vert_type = "dashed",
  show_points = NULL,
  show_vert = NULL
)

StatQFDiscrete

GeomQFDiscrete
```

## Format

An object of class `StatQFDiscrete` (inherits from `Stat`, `ggproto`,
`gg`) of length 3.

An object of class `GeomQFDiscrete` (inherits from `Geom`, `ggproto`,
`gg`) of length 5.

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

  A discrete quantile function (e.g.
  [qbinom](https://rdrr.io/r/stats/Binomial.html)). Evaluated on a dense
  grid of probabilities in \\(0, 1)\\. Use `xlim` to restrict the range
  of support values shown. Exactly one of `fun` or `pmf_fun` must be
  provided.

- pmf_fun:

  A PMF function (e.g. [dbinom](https://rdrr.io/r/stats/Binomial.html)).
  The quantile function is derived internally by inverting the
  cumulative sum. Exactly one of `fun` or `pmf_fun` must be provided.

- xlim:

  A numeric vector of length 2 specifying the range of support values to
  display (y-axis of the quantile function). For the `pmf_fun` path this
  also defines the integer support to evaluate.

- support:

  An optional integer or numeric vector giving the exact support points.
  When supplied, `xlim` is ignored.

- args:

  A named list of additional arguments to pass to `fun` or `pmf_fun`.

- open_fill:

  Fill color for the open (hollow) endpoint circles. Defaults to `NULL`,
  which uses the active theme's panel background color.

- vert_type:

  Line type for the vertical jump segments. Defaults to `"dashed"`.

- show_points:

  Logical. If `FALSE`, suppresses all endpoint circles (open and
  closed). If `NULL` (the default), circles are shown when there are 50
  or fewer points and hidden otherwise.

- show_vert:

  Logical. If `FALSE`, suppresses the vertical jump segments. If `NULL`
  (the default), segments are shown when there are 50 or fewer points
  and hidden otherwise.

## Value

A ggplot2 layer.

## Details

Supply **either** `pmf_fun` (a PMF such as
[dbinom](https://rdrr.io/r/stats/Binomial.html), from which the CDF is
computed via cumulative summation and then inverted) **or** `fun` (a
quantile function such as
[qbinom](https://rdrr.io/r/stats/Binomial.html), evaluated directly on a
dense probability grid).

## Examples

``` r
  # via PMF
  ggplot() +
    geom_qf_discrete(pmf_fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5))


  # via quantile function directly
  ggplot() +
    geom_qf_discrete(fun = qbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5))


  ggplot() +
    geom_qf_discrete(pmf_fun = dpois, xlim = c(0, 15), args = list(lambda = 5))

```
