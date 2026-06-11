# Plot a Probability Mass Function as Lollipops

`geom_pmf()` creates a ggplot2 layer that plots a probability mass
function (PMF) using a lollipop representation. Vertical segments extend
from zero up to the probability value at each integer support value and
a point is drawn at the top. Shading modes mirror those of
[`geom_pdf()`](/reference/geom_pdf.md): a cumulative threshold (`p`), a
two-sided interval (`p_lower`/`p_upper`), or highest density regions
(`shade_hdr`). Lollipops outside a `p`-based region are rendered at
reduced opacity; `shade_hdr` maps each support point's smallest
containing HDR to `alpha` as an ordered factor with a legend, mirroring
[`geom_pmf_2d()`](/reference/geom_pmf_2d.md).

## Usage

``` r
geom_pmf(
  mapping = NULL,
  data = NULL,
  stat = StatPMF,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  fun,
  xlim = NULL,
  support = NULL,
  point_size = 2.5,
  stick_linewidth = 0.25,
  stick_linetype = "dashed",
  color = "black",
  args = list(),
  p = NULL,
  lower.tail = TRUE,
  p_lower = NULL,
  p_upper = NULL,
  shade_outside = FALSE,
  shade_hdr = NULL
)

StatPMF

GeomPMF
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

  A function to compute the PMF (e.g.
  [dbinom](https://rdrr.io/r/stats/Binomial.html) or
  [dpois](https://rdrr.io/r/stats/Poisson.html)). The function must
  accept a numeric vector as its first argument and return non-negative
  probability values. (Ideally, the probabilities sum to 1 over the
  support.)

- xlim:

  A numeric vector of length 2 specifying the range (of x values) over
  which to evaluate the PMF. If not provided, a default range of 0 to 10
  is used.

- support:

  An optional integer or numeric vector giving the exact support points
  to evaluate. When supplied, `xlim` is ignored.

- point_size:

  Size of the points at the top of each lollipop (defaults to 2.5).

- stick_linewidth:

  Linewidth of the vertical sticks (defaults to 0.25).

- stick_linetype:

  Linetype of the vertical sticks (defaults to `"dashed"`).

- color:

  (Optional) Fixed color for the lollipop points and segments. When
  omitted, lollipops render with the geom's default color (black) unless
  a `colour` aesthetic is mapped (e.g.
  `aes(colour = after_stat(probs))`); when supplied, it overrides any
  colour mapping.

- args:

  A named list of additional arguments to pass to `fun`.

- p:

  (Optional) A numeric value between 0 and 1 specifying a cumulative
  probability threshold. When `lower.tail = TRUE` (the default),
  lollipops up to the corresponding quantile are shaded; when `FALSE`,
  the upper tail is shaded.

- lower.tail:

  Logical; controls the direction of `p`-based shading. Defaults to
  `TRUE`.

- p_lower:

  (Optional) Lower cumulative probability bound for two-sided shading.
  Used with `p_upper`.

- p_upper:

  (Optional) Upper cumulative probability bound for two-sided shading.
  Used with `p_lower`.

- shade_outside:

  Logical; if `TRUE`, shading is applied to the tails outside the
  `p_lower`/`p_upper` interval rather than inside. Defaults to `FALSE`.

- shade_hdr:

  (Optional) A numeric vector of target coverages for the highest
  density regions (HDRs) to shade, each strictly between 0 and 1, e.g.
  `shade_hdr = c(0.5, 0.8, 0.95)`: the smallest sets of support points
  containing at least the specified probability masses. Each support
  point is assigned the smallest requested HDR containing it; the
  assignment is exposed as the ordered factor `after_stat(probs)` and
  mapped to `alpha` by default, so points outside all requested regions
  render nearly transparent. Because a discrete distribution may not
  achieve the exact coverages, the smallest HDR with coverage \>= each
  target is used; HDRs are threshold-based, so all support points tied
  at the cutoff mass are included and the actual coverage can exceed the
  target. A message is issued via
  [`cli::cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html)
  reporting the actual coverages whenever they differ.

## Value

A ggplot2 layer.

## Computed variables

These are calculated by the `stat` part of the layer and can be accessed
with
[`ggplot2::after_stat()`](https://ggplot2.tidyverse.org/reference/aes_eval.html).

- `after_stat(x)`:

  Support points at which the PMF is evaluated.

- `after_stat(y)`:

  Probability mass at each support point.

- `after_stat(probs)`:

  Only when `shade_hdr` is supplied: the smallest requested HDR
  containing each support point, as an ordered factor whose outermost
  level (e.g. `">95%"`) collects points outside all requested regions.

## Aesthetics

`geom_pmf()` does not require any input aesthetics when `fun` is
supplied. It understands the following aesthetics:

- Computed position aesthetics:

  `x` and `y`, mapped by default to `after_stat(x)` and `after_stat(y)`.

- Drawing aesthetics:

  `alpha`, `colour`/`color`, `fill`, `group`, `linetype`, `linewidth`,
  `shape`, `size`, and `stroke` for the lollipop display.

## See also

[`geom_pdf()`](/reference/geom_pdf.md),
[`geom_cdf_discrete()`](/reference/geom_cdf_discrete.md),
[`geom_qf_discrete()`](/reference/geom_qf_discrete.md), and
[`geom_survival_discrete()`](/reference/geom_survival_discrete.md) for
related discrete distribution-function layers.

## Examples

``` r
# Basic PMF
ggplot() +
  geom_pmf(fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.25))


# Shade the lower tail up to the 80th percentile
ggplot() +
  geom_pmf(fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5),
    p = 0.8)


# Shade the 50/80/95% HDRs
ggplot() +
  geom_pmf(fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5),
    shade_hdr = c(0.5, 0.8, 0.95))
#> ! shade_hdr: exact coverage is not achievable for this discrete distribution.
#> ℹ Using smallest HDRs with coverage >= each target: 50% -> 65.6%, 80% -> 89.1%,
#>   95% -> 97.9%.

```
