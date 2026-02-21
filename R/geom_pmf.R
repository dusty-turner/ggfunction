#' Plot a Probability Mass Function as Lollipops
#'
#' `geom_pmf()` creates a ggplot2 layer that plots a probability mass function
#' (PMF) using a lollipop representation. Vertical segments extend from
#' zero up to the probability value at each integer support value and a point is
#' drawn at the top. Shading modes mirror those of [geom_pdf()]: a cumulative
#' threshold (`p`), a two-sided interval (`p_lower`/`p_upper`), or a highest
#' density region (`shade_hdr`). Non-shaded lollipops are rendered in grey with
#' dashed segments.
#'
#' @inheritParams ggplot2::geom_point
#' @param fun A function to compute the PMF (e.g. [dbinom] or [dpois]). The
#'   function must accept a numeric vector as its first argument and return
#'   non-negative probability values. (Ideally, the probabilities sum to 1 over
#'   the support.)
#' @param xlim A numeric vector of length 2 specifying the range (of x values)
#'   over which to evaluate the PMF. If not provided, a default range of 0 to 10
#'   is used.
#' @param support An optional integer or numeric vector giving the exact support
#'   points to evaluate. When supplied, `xlim` is ignored.
#' @param point_size Size of the points at the top of each lollipop (defaults to
#'   2.5).
#' @param stick_linewidth Linewidth of the vertical sticks (defaults to 0.25).
#' @param stick_linetype Linetype of the shaded vertical sticks (defaults to
#'   `"solid"`). Non-shaded sticks always use `"dashed"`.
#' @param color Color for the shaded points and segments (defaults to
#'   `"black"`).
#' @param args A named list of additional arguments to pass to `fun`.
#' @param p (Optional) A numeric value between 0 and 1 specifying a cumulative
#'   probability threshold. When `lower.tail = TRUE` (the default), lollipops
#'   up to the corresponding quantile are shaded; when `FALSE`, the upper tail
#'   is shaded.
#' @param lower.tail Logical; controls the direction of `p`-based shading.
#'   Defaults to `TRUE`.
#' @param p_lower (Optional) Lower cumulative probability bound for two-sided
#'   shading. Used with `p_upper`.
#' @param p_upper (Optional) Upper cumulative probability bound for two-sided
#'   shading. Used with `p_lower`.
#' @param shade_outside Logical; if `TRUE`, shading is applied to the tails
#'   outside the `p_lower`/`p_upper` interval rather than inside. Defaults to
#'   `FALSE`.
#' @param shade_hdr (Optional) A numeric value between 0 and 1 specifying the
#'   target coverage of the highest density region (HDR) to shade -- the
#'   smallest set of support points containing at least the specified probability
#'   mass. Because a discrete distribution may not achieve the exact coverage,
#'   the smallest HDR with coverage >= `shade_hdr` is used and a message is
#'   issued via [cli::cli_inform()] reporting both the specified and actual
#'   coverage whenever they differ.
#' @param ... Other parameters passed on to [ggplot2::layer()].
#'
#' @return A ggplot2 layer.
#'
#' @examples
#' # Basic PMF
#' ggplot() +
#'   geom_pmf(fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.25))
#'
#' # Shade the lower tail up to the 80th percentile
#' ggplot() +
#'   geom_pmf(fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5),
#'     p = 0.8)
#'
#' # Shade the 80% HDR
#' ggplot() +
#'   geom_pmf(fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5),
#'     shade_hdr = 0.8)
#'
#' @name geom_pmf
#' @aliases StatPMF GeomPMF
#' @export
geom_pmf <- function(mapping = NULL,
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
                     stick_linetype = "solid",
                     color = "black",
                     args = list(),
                     p = NULL,
                     lower.tail = TRUE,
                     p_lower = NULL,
                     p_upper = NULL,
                     shade_outside = FALSE,
                     shade_hdr = NULL) {

  if (is.null(data)) data <- ensure_nonempty_data(data)

  default_mapping <- aes(x = after_stat(x), y = after_stat(y))
  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPMF,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      xlim = xlim,
      support = support,
      point_size = point_size,
      stick_linewidth = stick_linewidth,
      stick_linetype = stick_linetype,
      color = color,
      args = args,
      na.rm = na.rm,
      p = p,
      lower.tail = lower.tail,
      p_lower = p_lower,
      p_upper = p_upper,
      shade_outside = shade_outside,
      shade_hdr = shade_hdr,
      ...
    )
  )
}

#' @rdname geom_pmf
#' @export
StatPMF <- ggproto("StatPMF", Stat,

  default_aes = aes(x = NULL, y = after_stat(y)),

  compute_group = function(data, scales, fun, xlim = NULL, support = NULL, args = NULL, ...) {

    if (!is.null(support)) {
      x_vals <- sort(support)
    } else if (is.null(xlim)) {
      x_vals <- 0:10
    } else {
      x_vals <- seq(ceiling(xlim[1]), floor(xlim[2]))
    }

    fun_injected <- function(x) {
      rlang::inject(fun(x, !!!args))
    }

    # check to make sure pmf is a real pmf
    invisible(check_pmf_normalization(fun_injected, support = x_vals, tol = 1e-2))

    y_vals <- fun_injected(x_vals)
    data.frame(x = x_vals, y = y_vals)
  }
)

#' @rdname geom_pmf
#' @export
GeomPMF <- ggproto("GeomPMF", GeomPoint,

  draw_panel = function(self, data, panel_params, coord, na.rm = FALSE,
                        point_size = 2.5, stick_linewidth = 0.25,
                        stick_linetype = "solid",
                        p = NULL, lower.tail = TRUE,
                        p_lower = NULL, p_upper = NULL,
                        shade_outside = FALSE, shade_hdr = NULL) {

    n <- nrow(data)
    pmf_vals <- data$y
    cum_vals  <- cumsum(pmf_vals)

    # Determine which lollipops fall inside the shaded region
    if (!is.null(shade_hdr)) {
      fhat_d   <- pmf_vals / sum(pmf_vals)
      ord      <- order(pmf_vals, decreasing = TRUE)
      cumprob  <- cumsum(fhat_d[ord])
      k        <- which(cumprob >= shade_hdr)[1L]
      if (is.na(k)) k <- n
      actual   <- cumprob[k]
      cutoff   <- pmf_vals[ord[k]]
      in_shade <- pmf_vals >= cutoff

      if (abs(actual - shade_hdr) > 0.005) {
        fmt <- function(x) paste0(round(x * 100, 1), "%")
        cli::cli_inform(c(
          "!" = "shade_hdr: {fmt(shade_hdr)} is not exactly achievable for this discrete distribution.",
          "i" = "Using smallest HDR with coverage >= {fmt(shade_hdr)}: actual coverage = {fmt(actual)}."
        ))
      }

    } else if (!is.null(p_lower) && !is.null(p_upper)) {
      idx_lo <- which(cum_vals >= p_lower)[1L]
      if (is.na(idx_lo)) idx_lo <- n
      idx_hi <- which(cum_vals >= p_upper)[1L]
      if (is.na(idx_hi)) idx_hi <- n
      if (shade_outside) {
        in_shade <- seq_len(n) < idx_lo | seq_len(n) > idx_hi
      } else {
        in_shade <- seq_len(n) >= idx_lo & seq_len(n) <= idx_hi
      }

    } else if (!is.null(p)) {
      if (lower.tail) {
        idx <- which(cum_vals >= p)[1L]
        if (is.na(idx)) idx <- n
        in_shade <- seq_len(n) <= idx
      } else {
        idx <- which(cum_vals >= (1 - p))[1L]
        if (is.na(idx)) idx <- 0L
        in_shade <- seq_len(n) > idx
      }

    } else {
      in_shade <- rep(TRUE, n)
    }

    # Build segment data: unshaded segments are grey + dashed
    seg_data          <- transform(data, yend = y, y = 0)
    seg_data$linewidth <- stick_linewidth
    seg_data$linetype  <- ifelse(in_shade, stick_linetype, "dashed")
    seg_data$colour    <- ifelse(in_shade, seg_data$colour, "grey70")
    seg_data$size      <- NULL

    seg_grob <- ggproto_parent(GeomSegment, self)$draw_panel(
      seg_data, panel_params, coord, na.rm = na.rm
    )

    # Build point data: unshaded points are grey
    pt_data         <- data
    pt_data$size    <- point_size
    pt_data$colour  <- ifelse(in_shade, pt_data$colour, "grey70")

    pt_grob <- ggproto_parent(GeomPoint, self)$draw_panel(
      pt_data, panel_params, coord, na.rm = na.rm
    )

    grid::grobTree(seg_grob, pt_grob)
  }
)
