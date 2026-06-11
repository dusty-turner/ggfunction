# Plot a Nelson-Aalen Cumulative Hazard Estimate for Censored Data

`geom_echf_na()` computes the Nelson-Aalen cumulative hazard estimator
from right-censored data and renders it as an increasing step function
starting at 0, using the same visual conventions as
[`geom_cdf_discrete()`](/reference/geom_cdf_discrete.md). An optional
pointwise normal confidence band (defaulting to 95%) is drawn around the
curve using the Nelson variance estimator.

## Usage

``` r
geom_echf_na(
  mapping = NULL,
  data = NULL,
  stat = StatECHFNA,
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

StatECHFNA

StatECHFNABand
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

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

  Logical. If `TRUE` (the default), draws a pointwise normal confidence
  band around the Nelson-Aalen estimate.

- level:

  Confidence level for the band. Defaults to `0.95`.

- conf_alpha:

  Alpha (transparency) of the confidence ribbon. Defaults to `0.4`.

## Value

A ggplot2 layer, or a list of two layers when `conf_int = TRUE`.

## Details

The Nelson-Aalen estimator at event time \\t_j\\ is \$\$\hat{H}(t) =
\sum\_{t_j \le t} \frac{d_j}{n_j},\$\$ where \\d_j\\ is the number of
events and \\n_j\\ is the number at risk just before \\t_j\\.

The pointwise confidence band uses the Nelson variance estimator
\$\$\widehat{\mathrm{Var}}\[\hat{H}(t)\] = \sum\_{t_j \le t}
\frac{d_j}{n_j^2}\$\$ with the normal critical value, giving pointwise
bounds \\\hat{H}(t) \pm z\_{1-\alpha/2}\\\mathrm{se}(t)\\ with lower
bound clipped to 0.

## Computed variables

These are calculated by the `stat` part of the layer and can be accessed
with
[`ggplot2::after_stat()`](https://ggplot2.tidyverse.org/reference/aes_eval.html).

- `after_stat(x)`:

  Event times.

- `after_stat(y)`:

  Nelson-Aalen cumulative hazard estimates.

- `after_stat(ymin)` and `after_stat(ymax)`:

  Lower and upper confidence band limits when `conf_int = TRUE`.

## Dropped variables

`status` is used to compute event times and risk sets, but is not
available after statistical transformation.

## Aesthetics

`geom_echf_na()` requires the following aesthetics:

- `x`:

  Observed time (event or censoring time).

- `status`:

  Event indicator: 1 = event occurred, 0 = censored.

It also understands `alpha`, `colour`/`color`, `fill`, `group`,
`linetype`, `linewidth`, `shape`, `size`, and `stroke`.

## See also

[`geom_echf()`](/reference/geom_echf.md) for complete (uncensored) data,
[`geom_chf()`](/reference/geom_chf.md) for theoretical cumulative hazard
functions, [`geom_ecdf_km()`](/reference/geom_ecdf_km.md) for the
Kaplan-Meier survival curve.

## Examples

``` r
set.seed(42)
n <- 50
true_time <- rexp(n, rate = 0.5)
cens_time <- rexp(n, rate = 0.2)
df <- data.frame(
  time   = pmin(true_time, cens_time),
  status = as.integer(true_time <= cens_time)
)

ggplot(df, aes(x = time, status = status)) +
  geom_echf_na()


# Without confidence band
ggplot(df, aes(x = time, status = status)) +
  geom_echf_na(conf_int = FALSE)


# Grouped data
df2 <- data.frame(
  time   = c(rexp(40, 0.5), rexp(40, 1)),
  status = sample(0:1, 80, replace = TRUE, prob = c(0.2, 0.8)),
  group  = rep(c("A", "B"), each = 40)
)
ggplot(df2, aes(x = time, status = status, colour = group)) +
  geom_echf_na()

```
