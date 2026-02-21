## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)
library(ggfunction)


## -----------------------------------------------------------------------------
ggplot() +
  geom_function_1d_1d(fun = sin, xlim = c(0, 2 * pi))


## -----------------------------------------------------------------------------
ggplot() +
  geom_function_1d_1d(
    fun = dnorm, xlim = c(-3, 3),
    shade_from = -1, shade_to = 1, fill = "steelblue"
  )


## -----------------------------------------------------------------------------
f <- function(t) c(sin(t), cos(t))
ggplot() +
  geom_function_1d_2d(fun = f, tlim = c(0, 2 * pi))


## -----------------------------------------------------------------------------
f_gaussian <- function(v) exp(-(v[1]^2 + v[2]^2) / 2)

ggplot() +
  geom_function_2d_1d(fun = f_gaussian, xlim = c(-3, 3), ylim = c(-3, 3))


## -----------------------------------------------------------------------------
ggplot() +
  geom_function_2d_1d(
    fun = f_gaussian, xlim = c(-3, 3), ylim = c(-3, 3),
    type = "contour"
  )


## -----------------------------------------------------------------------------
ggplot() +
  geom_function_2d_1d(
    fun = f_gaussian, xlim = c(-3, 3), ylim = c(-3, 3),
    type = "contour_filled"
  )


## -----------------------------------------------------------------------------
f_rotation <- function(u) c(-u[2], u[1])
ggplot() +
  geom_function_2d_2d(fun = f_rotation, xlim = c(-1, 1), ylim = c(-1, 1))


## -----------------------------------------------------------------------------
ggplot() +
  geom_function_2d_2d(fun = f_rotation, xlim = c(-1, 1), ylim = c(-1, 1),
    type = "stream")


## -----------------------------------------------------------------------------
ggplot() +
  geom_pdf(fun = dnorm, xlim = c(-3, 3), p = 0.975, fill = "tomato")


## -----------------------------------------------------------------------------
ggplot() +
  geom_pdf(
    fun = dnorm, xlim = c(-3, 3),
    p_lower = 0.025, p_upper = 0.975, fill = "steelblue"
  )


## -----------------------------------------------------------------------------
ggplot() +
  geom_pdf(
    fun = dnorm, xlim = c(-3, 3),
    p_lower = 0.025, p_upper = 0.975, shade_outside = TRUE, fill = "tomato"
  )


## -----------------------------------------------------------------------------
ggplot() +
  geom_cdf(fun = pnorm, xlim = c(-3, 3))


## -----------------------------------------------------------------------------
ggplot() +
  geom_cdf(fun = pnorm, xlim = c(-3, 3), p = 0.975, fill = "darkgreen")


## -----------------------------------------------------------------------------
ggplot() +
  geom_pmf(fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.3))


## -----------------------------------------------------------------------------
ggplot() +
  geom_pmf(fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5), p = 0.8)


## -----------------------------------------------------------------------------
ggplot() +
  geom_pmf(fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.3),
    shade_hdr = 0.7)


## -----------------------------------------------------------------------------
ggplot() +
  geom_qf(fun = qnorm, args = list(mean = 0, sd = 1))


## -----------------------------------------------------------------------------
ggplot() +
  geom_cdf_discrete(
    fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5)
  )


## -----------------------------------------------------------------------------
ggplot() +
  geom_cdf_discrete(
    fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5),
    show_points = FALSE
  )


## -----------------------------------------------------------------------------
ggplot() +
  geom_qf_discrete(
    fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5)
  )


## -----------------------------------------------------------------------------
ggplot() +
  geom_survival_discrete(
    fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5)
  )


## -----------------------------------------------------------------------------
ggplot() +
  geom_survival(fun = pexp, xlim = c(0, 10), args = list(rate = 0.5))


## -----------------------------------------------------------------------------
ggplot() +
  geom_hf(
    pdf_fun = dexp, cdf_fun = pexp,
    xlim = c(0.01, 10), args = list(rate = 0.5)
  )

