# Introduction to ggfunction

## Overview

**ggfunction** extends ggplot2 to visualize mathematical functions
directly. It provides a unified interface organized around two families:

1.  **Dimensional taxonomy** – functions classified by input/output
    dimension:
    - [`geom_function_1d_1d()`](/reference/geom_function_1d_1d.md):
      $\left. {\mathbb{R}}\rightarrow{\mathbb{R}} \right.$ (scalar to
      scalar)
    - [`geom_function_1d_2d()`](/reference/geom_function_1d_2d.md):
      $\left. {\mathbb{R}}\rightarrow{\mathbb{R}}^{2} \right.$
      (parametric curves)
    - [`geom_function_2d_1d()`](/reference/geom_function_2d_1d.md):
      $\left. {\mathbb{R}}^{2}\rightarrow{\mathbb{R}} \right.$ (scalar
      fields, raster/contour)
    - [`geom_function_2d_2d()`](/reference/geom_function_2d_2d.md):
      $\left. {\mathbb{R}}^{2}\rightarrow{\mathbb{R}}^{2} \right.$
      (vector fields)
2.  **Probability distributions** – specialized geoms for distribution
    functions:
    - [`geom_pdf()`](/reference/geom_pdf.md): probability density
      function
    - [`geom_cdf()`](/reference/geom_cdf.md): cumulative distribution
      function
    - [`geom_pmf()`](/reference/geom_pmf.md): probability mass function
      (lollipop)
    - [`geom_qf()`](/reference/geom_qf.md): quantile function
    - [`geom_cdf_discrete()`](/reference/geom_cdf_discrete.md): discrete
      CDF (right-continuous step function)
    - [`geom_qf_discrete()`](/reference/geom_qf_discrete.md): discrete
      quantile function (left-continuous step function)
    - [`geom_survival_discrete()`](/reference/geom_survival_discrete.md):
      discrete survival function $S(x) = 1 - F(x)$ (right-continuous
      step function)
    - [`geom_survival()`](/reference/geom_survival.md): survival
      function $S(x) = 1 - F(x)$
    - [`geom_hf()`](/reference/geom_hf.md): hazard function
      $h(x) = f(x)/S(x)$

## Dimensional Taxonomy

### 1D to 1D: Scalar Functions

Plot any $\left. f:{\mathbb{R}}\rightarrow{\mathbb{R}} \right.$ with
optional interval shading:

``` r
ggplot() +
  geom_function_1d_1d(fun = sin, xlim = c(0, 2 * pi))
```

![](ggfunction_files/figure-html/unnamed-chunk-1-1.png)

Shade a specific interval:

``` r
ggplot() +
  geom_function_1d_1d(
    fun = dnorm, xlim = c(-3, 3),
    shade_from = -1, shade_to = 1, fill = "steelblue"
  )
```

![](ggfunction_files/figure-html/unnamed-chunk-2-1.png)

### 1D to 2D: Parametric Curves

Map a scalar parameter to a 2D path using `tlim` to set the parameter
range:

``` r
f <- function(t) c(sin(t), cos(t))
ggplot() +
  geom_function_1d_2d(fun = f, tlim = c(0, 2 * pi))
```

![](ggfunction_files/figure-html/unnamed-chunk-3-1.png)

### 2D to 1D: Scalar Fields

Visualize $\left. f:{\mathbb{R}}^{2}\rightarrow{\mathbb{R}} \right.$ as
a raster:

``` r
f_gaussian <- function(v) exp(-(v[1]^2 + v[2]^2) / 2)

ggplot() +
  geom_function_2d_1d(fun = f_gaussian, xlim = c(-3, 3), ylim = c(-3, 3))
```

![](ggfunction_files/figure-html/unnamed-chunk-4-1.png)

Or as contour lines:

``` r
ggplot() +
  geom_function_2d_1d(
    fun = f_gaussian, xlim = c(-3, 3), ylim = c(-3, 3),
    type = "contour"
  )
```

![](ggfunction_files/figure-html/unnamed-chunk-5-1.png)

Or as filled contours:

``` r
ggplot() +
  geom_function_2d_1d(
    fun = f_gaussian, xlim = c(-3, 3), ylim = c(-3, 3),
    type = "contour_filled"
  )
```

![](ggfunction_files/figure-html/unnamed-chunk-6-1.png)

### 2D to 2D: Vector Fields

Visualize vector fields as short arrows at each grid point (the default,
`type = "vector"`):

``` r
f_rotation <- function(u) c(-u[2], u[1])
ggplot() +
  geom_function_2d_2d(fun = f_rotation, xlim = c(-1, 1), ylim = c(-1, 1))
```

![](ggfunction_files/figure-html/unnamed-chunk-7-1.png)

Or as integral-curve streamlines with `type = "stream"`:

``` r
ggplot() +
  geom_function_2d_2d(fun = f_rotation, xlim = c(-1, 1), ylim = c(-1, 1),
    type = "stream")
```

![](ggfunction_files/figure-html/unnamed-chunk-8-1.png)

## Probability Distribution Family

### PDF with Shading

``` r
ggplot() +
  geom_pdf(fun = dnorm, xlim = c(-3, 3), p = 0.975, fill = "tomato")
```

![](ggfunction_files/figure-html/unnamed-chunk-9-1.png)

Two-sided shading (e.g., middle 95%):

``` r
ggplot() +
  geom_pdf(
    fun = dnorm, xlim = c(-3, 3),
    p_lower = 0.025, p_upper = 0.975, fill = "steelblue"
  )
```

![](ggfunction_files/figure-html/unnamed-chunk-10-1.png)

Shade the tails instead (outside):

``` r
ggplot() +
  geom_pdf(
    fun = dnorm, xlim = c(-3, 3),
    p_lower = 0.025, p_upper = 0.975, shade_outside = TRUE, fill = "tomato"
  )
```

![](ggfunction_files/figure-html/unnamed-chunk-11-1.png)

### CDF

``` r
ggplot() +
  geom_cdf(fun = pnorm, xlim = c(-3, 3))
```

![](ggfunction_files/figure-html/unnamed-chunk-12-1.png)

With shading below a quantile:

``` r
ggplot() +
  geom_cdf(fun = pnorm, xlim = c(-3, 3), p = 0.975, fill = "darkgreen")
```

![](ggfunction_files/figure-html/unnamed-chunk-13-1.png)

### PMF (Lollipop)

``` r
ggplot() +
  geom_pmf(fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.3))
```

![](ggfunction_files/figure-html/unnamed-chunk-14-1.png)

Tail shading with `p` and HDR shading with `shade_hdr` work the same way
as in [`geom_pdf()`](/reference/geom_pdf.md):

``` r
ggplot() +
  geom_pmf(fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5), p = 0.8)
```

![](ggfunction_files/figure-html/unnamed-chunk-15-1.png)

``` r
ggplot() +
  geom_pmf(fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.3),
    shade_hdr = 0.7)
```

![](ggfunction_files/figure-html/unnamed-chunk-16-1.png)

### Quantile Function

``` r
ggplot() +
  geom_qf(fun = qnorm, args = list(mean = 0, sd = 1))
```

![](ggfunction_files/figure-html/unnamed-chunk-17-1.png)

### Discrete CDF (Step Function)

[`geom_cdf_discrete()`](/reference/geom_cdf_discrete.md) renders a
discrete CDF as a right-continuous step function with dashed vertical
jumps and open/closed endpoint circles:

``` r
ggplot() +
  geom_cdf_discrete(
    pmf_fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5)
  )
```

![](ggfunction_files/figure-html/unnamed-chunk-18-1.png)

Use `show_points = FALSE` or `show_vert = FALSE` to suppress circles or
jump lines:

``` r
ggplot() +
  geom_cdf_discrete(
    pmf_fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5),
    show_points = FALSE
  )
```

![](ggfunction_files/figure-html/unnamed-chunk-19-1.png)

### Discrete Quantile Function (Step Function)

[`geom_qf_discrete()`](/reference/geom_qf_discrete.md) renders the
inverse of the discrete CDF as a left-continuous step function on
$\lbrack 0,1\rbrack$, with closed circles at the bottom of each jump and
open circles at the top:

``` r
ggplot() +
  geom_qf_discrete(
    pmf_fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5)
  )
```

![](ggfunction_files/figure-html/unnamed-chunk-20-1.png)

### Discrete Survival Function (Step Function)

[`geom_survival_discrete()`](/reference/geom_survival_discrete.md)
renders the discrete survival function $S(x) = 1 - F(x) = P(X > x)$ as a
right-continuous step function with the same visual conventions as
[`geom_cdf_discrete()`](/reference/geom_cdf_discrete.md):

``` r
ggplot() +
  geom_survival_discrete(
    pmf_fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5)
  )
```

![](ggfunction_files/figure-html/unnamed-chunk-21-1.png)

### Survival Function

``` r
ggplot() +
  geom_survival(fun = pexp, xlim = c(0, 10), args = list(rate = 0.5))
```

![](ggfunction_files/figure-html/unnamed-chunk-22-1.png)

### Hazard Function

The exponential distribution has a constant hazard rate:

``` r
ggplot() +
  geom_hf(
    pdf_fun = dexp, cdf_fun = pexp,
    xlim = c(0.01, 10), args = list(rate = 0.5)
  )
```

![](ggfunction_files/figure-html/unnamed-chunk-23-1.png)
