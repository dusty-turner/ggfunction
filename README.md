ggfunction
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

`ggfunction` extends `ggplot2` with layers for plotting mathematical
functions and probability functions.

## Installation

``` r
# install.packages("pak")
pak::pak("dusty-turner/ggfunction")
```

## What This Package Does

### 1D to 2D Parametric Streams

`geom_function_1d_2d()` maps a 1D input (usually time) into a 2D path.

``` r
library(ggplot2)
library(ggfunction)

f <- function(t) c(sin(t), t * cos(t))

ggplot() +
  geom_function_1d_2d(fun = f, T = 20, tail_point = TRUE)
```

<figure>
<img src="docs/reference/geom_function_1d_2d-1.png"
alt="1D to 2D stream example" />
<figcaption aria-hidden="true">1D to 2D stream example</figcaption>
</figure>

### 2D to 1D Scalar Fields

`geom_function_2d_1d()` evaluates a scalar function on a grid and
renders it as a raster layer.

``` r
f <- function(v) {
  x <- v[1]
  y <- v[2]
  sin(sqrt(x^2 + y^2))
}

ggplot() +
  geom_function_2d_1d(fun = f, xlim = c(-50, 50), ylim = c(-50, 50), n = 200)
```

<figure>
<img src="docs/reference/geom_function_2d_1d-4.png"
alt="2D to 1D scalar field example" />
<figcaption aria-hidden="true">2D to 1D scalar field
example</figcaption>
</figure>

### 2D to 2D Vector Fields

`geom_function_2d_2d()` draws streamlines from a vector field function.

``` r
f <- function(u) c(-u[2], u[1])

ggplot() +
  geom_function_2d_2d(fun = f, xlim = c(-1, 1), ylim = c(-1, 1))
```

<figure>
<img src="docs/reference/geom_function_2d_2d-1.png"
alt="2D to 2D vector field example" />
<figcaption aria-hidden="true">2D to 2D vector field
example</figcaption>
</figure>

### Probability Distribution Layers

The package includes helper layers for common statistical functions.

#### Probability Density Function (`geom_pdf`)

``` r
ggplot() +
  geom_pdf(fun = dnorm, xlim = c(-3, 3), p = 0.975, lower.tail = TRUE)
```

<figure>
<img src="man/figures/readme-geom-pdf.png" alt="PDF example" />
<figcaption aria-hidden="true">PDF example</figcaption>
</figure>

#### Cumulative Distribution Function (`geom_cdf`)

``` r
ggplot() +
  geom_cdf(fun = pnorm, xlim = c(-3, 3), p = 0.975, lower.tail = TRUE)
```

<figure>
<img src="man/figures/readme-geom-cdf.png" alt="CDF example" />
<figcaption aria-hidden="true">CDF example</figcaption>
</figure>

#### Probability Mass Function (`geom_pmf`)

``` r
ggplot() +
  geom_pmf(fun = dbinom, args = list(size = 10, prob = 0.25), xlim = c(0, 10))
```

<figure>
<img src="man/figures/readme-geom-pmf.png" alt="PMF example" />
<figcaption aria-hidden="true">PMF example</figcaption>
</figure>

#### Quantile Function (`geom_qf`)

``` r
ggplot() +
  geom_qf(fun = qnorm, args = list(mean = 3, sd = 2))
```

<figure>
<img src="man/figures/readme-geom-qf.png" alt="QF example" />
<figcaption aria-hidden="true">QF example</figcaption>
</figure>

### New in v0.1.0

#### General R to R Function (`geom_function_1d_1d`)

``` r
ggplot() +
  geom_function_1d_1d(fun = dnorm, xlim = c(-3, 3),
    shade_from = -1, shade_to = 1, fill = "steelblue")
```

#### Discrete CDF (`geom_discrete_cdf`)

``` r
ggplot() +
  geom_discrete_cdf(fun = dbinom, args = list(size = 10, prob = 0.5), xlim = c(0, 10))
```

#### Survival Function (`geom_survival`)

``` r
ggplot() +
  geom_survival(fun = pexp, args = list(rate = 0.5), xlim = c(0, 10))
```

#### Hazard Function (`geom_hf`)

``` r
ggplot() +
  geom_hf(pdf_fun = dexp, cdf_fun = pexp, args = list(rate = 0.5), xlim = c(0.01, 10))
```

#### Two-sided Shading for PDF/CDF

``` r
ggplot() +
  geom_pdf(fun = dnorm, xlim = c(-3, 3), p_lower = 0.025, p_upper = 0.975, fill = "steelblue")
```

#### Contour Support for 2D to 1D

``` r
f <- function(v) exp(-(v[1]^2 + v[2]^2) / 2)
ggplot() +
  geom_function_2d_1d(fun = f, xlim = c(-3, 3), ylim = c(-3, 3), type = "contour_filled")
```

## Learn More

- Reference: `docs/reference/index.html`
- Source: <https://github.com/dusty-turner/ggfunction>
- Issues: <https://github.com/dusty-turner/ggfunction/issues>
