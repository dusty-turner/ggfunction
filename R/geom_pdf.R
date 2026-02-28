#' Plot a Probability Density Function with a Filled Area
#'
#' `geom_pdf()` computes a probability density function and plots it as a filled area.
#' This function is similar to [geom_function()], but it shades the area corresponding to
#' a given proportion of the total density.
#'
#' @inheritParams ggplot2::geom_function
#' @importFrom stats integrate
#' @importFrom cli cli_alert
#' @param fun A function to compute the density (e.g. [dnorm]). The function must accept a
#'   numeric vector as its first argument and return density values that integrate (approximately)
#'   to 1. Exactly one of `fun` or `cdf_fun` must be provided.
#' @param cdf_fun A CDF function (e.g. [pnorm]). When supplied, the PDF is derived
#'   numerically via central finite differences. Exactly one of `fun` or `cdf_fun` must
#'   be provided.
#' @param n (defaults to 101)Number of points at which to evaluate `fun`.
#' @param args A named list of additional arguments to pass to `fun`.
#' @param xlim A numeric vector of length 2 giving the x-range over which to evaluate the PDF.
#' @param fill Fill color for the shaded area.
#' @param color Line color for the outline of the density curve.
#' @param linewidth Line width for the outline of the density curve.
#' @param alpha Alpha transparency for the shaded area.
#' @param p (Optional) A numeric value between 0 and 1 specifying the cumulative probability
#'   threshold. The area will be shaded up until the point where the cumulative density reaches
#'   this value.
#' @param lower.tail Logical; if `TRUE` (the default) the shaded area extends from the left end
#'   of the density up to the threshold. If `FALSE`, the shading extends from the threshold to the
#'   right end.
#' @param p_lower (Optional) A numeric value between 0 and 1 specifying the lower cumulative
#'   probability bound. Used with `p_upper` for two-sided shading.
#' @param p_upper (Optional) A numeric value between 0 and 1 specifying the upper cumulative
#'   probability bound. Used with `p_lower` for two-sided shading.
#' @param shade_outside Logical; if `FALSE` (the default) shading is applied between `p_lower`
#'   and `p_upper`. If `TRUE`, shading is applied to the tails outside that range.
#' @param shade_hdr (Optional) A numeric value between 0 and 1 specifying the coverage of the
#'   [highest density region](https://en.wikipedia.org/wiki/Credible_interval#Highest_density_interval)
#'   (HDR) to shade. The HDR is the smallest region of the domain containing the specified
#'   probability mass; for multimodal densities it may be disconnected, producing multiple
#'   shaded intervals. Computed following the approach of \doi{10.32614/RJ-2023-048}: density
#'   values are evaluated on the grid, normalized to sum to 1, sorted in descending order, and
#'   cumulated until the target coverage is reached; the density at that threshold determines
#'   which regions are shaded. Takes precedence over `p`, `p_lower`, and `p_upper` if specified.
#'
#' @return A ggplot2 layer.
#'
#' @examples
#' ggplot() +
#'   geom_pdf(fun = dnorm, xlim = c(-3, 3), p = .975, lower.tail = TRUE)
#'
#' # Highest density region of a bimodal density
#' f_bim <- function(x) 0.5 * dnorm(x, -2, 0.5) + 0.5 * dnorm(x, 2, 0.5)
#' ggplot() +
#'   geom_pdf(fun = f_bim, xlim = c(-4, 4), shade_hdr = 0.9)
#'
#' @name geom_pdf
#' @aliases StatPDF GeomPDF
#' @export
geom_pdf <- function(
    mapping = NULL,
    data = NULL,
    stat = StatPDF,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = FALSE,
    fun = NULL,
    cdf_fun = NULL,
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
    ) {

  if (is.null(data)) data <- ensure_nonempty_data(data)

  default_mapping <- aes(x = after_stat(x), y = after_stat(y))

  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

  linewidth_params <- if (is.null(linewidth)) {
    list()
  } else if (utils::packageVersion("ggplot2") < "3.4.0") {
    list(size = linewidth)
  } else {
    list(linewidth = linewidth)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPDF,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = c(
      list(
        fun = fun,
        cdf_fun = cdf_fun,
        n = n,
        xlim = xlim,
        args = args,
        na.rm = na.rm,
        fill = fill,
        color = color,
        alpha = alpha,
        p = p,
        lower.tail = lower.tail,
        p_lower = p_lower,
        p_upper = p_upper,
        shade_outside = shade_outside,
        shade_hdr = shade_hdr
      ),
      linewidth_params,
      list(...)
    )
  )
}

#' @rdname geom_pdf
#' @export
StatPDF <- ggproto("StatPDF", Stat,
  default_aes = aes(x = NULL, y = after_stat(y)),

  compute_group = function(data, scales, fun = NULL, cdf_fun = NULL,
                           xlim = NULL, n = 101, args = NULL) {

    # Validate: exactly one source
    n_provided <- (!is.null(fun)) + (!is.null(cdf_fun))
    if (n_provided == 0L) {
      cli::cli_abort("One of {.arg fun} or {.arg cdf_fun} must be provided.")
    }
    if (n_provided > 1L) {
      cli::cli_abort("Supply only one of {.arg fun} or {.arg cdf_fun}, not both.")
    }

    range <- if (is.null(scales$x)) {
      xlim %||% c(0, 1)
    } else {
      xlim %||% scales$x$dimension()
    }

    if (!is.null(cdf_fun)) {
      cdf_injected <- function(x) rlang::inject(cdf_fun(x, !!!args))
      fun_injected <- cdf_to_pdf(cdf_injected)
    } else {
      fun_injected <- function(x) rlang::inject(fun(x, !!!args))
    }

    # Check that the injected function integrates to 1 over the specified range.
    invisible(check_pdf_normalization(fun_injected, lower = -Inf, upper = range[2], tol = 1e-2))

    xseq <- seq(range[1], range[2], length.out = n)
    y_out <- fun_injected(xseq)

    data.frame(x = xseq, y = y_out)
  }
)

#' @rdname geom_pdf
#' @export
GeomPDF <- ggproto("GeomPDF", GeomArea,
  draw_panel = function(self, data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE, p = NULL, lower.tail = TRUE,
                        p_lower = NULL, p_upper = NULL, shade_outside = FALSE,
                        shade_hdr = NULL
                        ) {

    x_vals <- data$x
    y_vals <- data$y

    # Compute cumulative area using the trapezoidal rule.
    dx <- diff(x_vals)
    seg_area <- (y_vals[-length(y_vals)] + y_vals[-1]) / 2 * dx
    cum_area <- c(0, cumsum(seg_area))

    total_area <- max(cum_area)
    norm_cum <- cum_area / total_area  # normalized cumulative area

    # Helper to build a closed polygon from clipped data
    build_poly <- function(clip_data, clip_range) {
      pd <- rbind(
        transform(clip_data[1, , drop = FALSE], x = clip_range[1], y = 0),
        clip_data,
        transform(clip_data[nrow(clip_data), , drop = FALSE], x = clip_range[2], y = 0)
      )
      pd$colour <- NA
      pd
    }

    area_grobs <- list()

    # Determine the clipping range based on p_lower/p_upper or p (if provided)
    if (!is.null(p_lower) && !is.null(p_upper)) {
      idx_lower <- which(norm_cum >= p_lower)[1]
      if (is.na(idx_lower)) idx_lower <- length(norm_cum)
      idx_upper <- which(norm_cum >= p_upper)[1]
      if (is.na(idx_upper)) idx_upper <- length(norm_cum)
      threshold_lower <- x_vals[idx_lower]
      threshold_upper <- x_vals[idx_upper]

      if (shade_outside) {
        # Shade both tails: left of p_lower and right of p_upper
        left_data <- data[data$x <= threshold_lower, , drop = FALSE]
        if (nrow(left_data) > 0) {
          left_range <- c(min(x_vals), threshold_lower)
          area_grobs <- c(area_grobs, list(
            ggproto_parent(GeomArea, self)$draw_panel(
              build_poly(left_data, left_range), panel_params, coord, na.rm = na.rm
            )
          ))
        }
        right_data <- data[data$x >= threshold_upper, , drop = FALSE]
        if (nrow(right_data) > 0) {
          right_range <- c(threshold_upper, max(x_vals))
          area_grobs <- c(area_grobs, list(
            ggproto_parent(GeomArea, self)$draw_panel(
              build_poly(right_data, right_range), panel_params, coord, na.rm = na.rm
            )
          ))
        }
      } else {
        # Shade between p_lower and p_upper
        clip_data <- data[data$x >= threshold_lower & data$x <= threshold_upper, , drop = FALSE]
        clip_range <- c(threshold_lower, threshold_upper)
        area_grobs <- c(area_grobs, list(
          ggproto_parent(GeomArea, self)$draw_panel(
            build_poly(clip_data, clip_range), panel_params, coord, na.rm = na.rm
          )
        ))
      }
    } else if (!is.null(p)) {
      if (lower.tail) {
        idx <- which(norm_cum >= p)[1]
        if (is.na(idx)) idx <- length(norm_cum)
        threshold_x <- x_vals[idx]
        clip_data <- data[data$x <= threshold_x, , drop = FALSE]
        clip_range <- c(min(x_vals), threshold_x)
      } else {
        idx <- which(norm_cum >= (1 - p))[1]
        if (is.na(idx)) idx <- 1
        threshold_x <- x_vals[idx]
        clip_data <- data[data$x >= threshold_x, , drop = FALSE]
        clip_range <- c(threshold_x, max(x_vals))
      }
      area_grobs <- c(area_grobs, list(
        ggproto_parent(GeomArea, self)$draw_panel(
          build_poly(clip_data, clip_range), panel_params, coord, na.rm = na.rm
        )
      ))
    } else if (!is.null(shade_hdr)) {
      # Highest density region (HDR) shading, following ggdensity's approach:
      # normalize f(x) values to sum to 1, sort descending, cumsum until
      # coverage is reached, shade all connected intervals above the cutoff.
      fhat_discretized <- y_vals / sum(y_vals)
      ord <- order(y_vals, decreasing = TRUE)
      cumprob <- cumsum(fhat_discretized[ord])
      cutoff_idx <- which(cumprob >= shade_hdr)[1]
      if (is.na(cutoff_idx)) cutoff_idx <- length(y_vals)
      cutoff <- y_vals[ord[cutoff_idx]]

      # Identify connected runs of grid points at or above the cutoff
      above <- y_vals >= cutoff
      runs <- rle(above)
      idx_end <- cumsum(runs$lengths)
      idx_start <- c(1L, head(idx_end, -1L) + 1L)

      for (i in seq_along(runs$values)) {
        if (runs$values[i]) {
          clip_data  <- data[idx_start[i]:idx_end[i], , drop = FALSE]
          clip_range <- c(x_vals[idx_start[i]], x_vals[idx_end[i]])
          area_grobs <- c(area_grobs, list(
            ggproto_parent(GeomArea, self)$draw_panel(
              build_poly(clip_data, clip_range), panel_params, coord, na.rm = na.rm
            )
          ))
        }
      }
    } else {
      clip_range <- range(x_vals, na.rm = TRUE)
      clip_data <- data[data$x >= clip_range[1] & data$x <= clip_range[2], , drop = FALSE]
      area_grobs <- c(area_grobs, list(
        ggproto_parent(GeomArea, self)$draw_panel(
          build_poly(clip_data, clip_range), panel_params, coord, na.rm = na.rm
        )
      ))
    }

    # Create the line grob for the entire function using GeomPath’s draw_panel.
    line_grob <- ggproto_parent(GeomPath, self)$draw_panel(
      data, panel_params, coord,
      arrow = arrow,
      lineend = lineend, linejoin = linejoin, linemitre = linemitre,
      na.rm = na.rm
    )

    do.call(grid::grobTree, c(area_grobs, list(line_grob)))
  }
)
