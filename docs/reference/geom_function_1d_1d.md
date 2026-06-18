# Plot a General R to R Function with Optional Shading

`geom_function_1d_1d()` computes a function \\f: \mathbb{R} \to
\mathbb{R}\\ and plots it as a line (like
[`ggplot2::geom_function()`](https://ggplot2.tidyverse.org/reference/geom_function.html))
with an optional shaded region between two x-values.

## Usage

``` r
geom_function_1d_1d(
  mapping = NULL,
  data = NULL,
  stat = StatFunction1d,
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
  shade_from = NULL,
  shade_to = NULL
)

StatFunction1d

GeomFunction1d
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

  A function to compute. The function must accept a numeric vector as
  its first argument.

- xlim:

  A numeric vector of length 2 giving the x-range over which to evaluate
  the function.

- n:

  Number of points at which to evaluate `fun`. Defaults to 101.

- args:

  A named list of additional arguments to pass to `fun`.

- fill:

  Fill color for the shaded area (only used when `shade_from`/`shade_to`
  are specified).

- color:

  Line color for the curve.

- shade_from:

  (Optional) Numeric. Left boundary of the x-interval to shade.

- shade_to:

  (Optional) Numeric. Right boundary of the x-interval to shade.

## Value

A ggplot2 layer.

## Computed variables

These are calculated by the `stat` part of the layer and can be accessed
with
[`ggplot2::after_stat()`](https://ggplot2.tidyverse.org/reference/aes_eval.html).

- `after_stat(x)`:

  Points at which `fun` is evaluated.

- `after_stat(y)`:

  Function values.

## Aesthetics

`geom_function_1d_1d()` does not require any input aesthetics when `fun`
is supplied. It understands the following aesthetics:

- Computed position aesthetics:

  `x` and `y`, mapped by default to `after_stat(x)` and `after_stat(y)`.

- Drawing aesthetics:

  `alpha`, `colour`/`color`, `fill`, `group`, `linetype`, and
  `linewidth` for the line and optional shaded region.

## See also

[`ggplot2::geom_function()`](https://ggplot2.tidyverse.org/reference/geom_function.html)
for ggplot2's built-in one-dimensional function layer.

## Examples

``` r
  ggplot() +
    geom_function_1d_1d(fun = sin, xlim = c(0, 2 * pi))


  ggplot() +
    geom_function_1d_1d(fun = dnorm, xlim = c(-3, 3),
      shade_from = -1, shade_to = 1)


  # Parameterized via `args`
  ggplot() +
    geom_function_1d_1d(fun = dnorm, xlim = c(-3, 9),
      args = list(mean = 3, sd = 2))

```
