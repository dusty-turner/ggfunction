#' Plot a Probability Mass Function
#'
#' `geom_pmf()` creates a ggplot2 layer that plots a probability mass function
#' (PMF) using a lollipop representation by default, or bars with
#' `type = "bar"`. Lollipops use vertical segments extending from zero up to
#' the probability value at each integer support value, capped by a point.
#' Shading modes mirror those of [geom_pdf()]: a cumulative threshold (`p`), a
#' two-sided interval (`p_lower`/`p_upper`), or highest density regions
#' (`shade_hdr`). Marks outside a `p`-based region are rendered at reduced
#' opacity; `shade_hdr` maps each support point's smallest containing HDR to
#' `alpha` as an ordered factor with a legend, mirroring [geom_pmf_2d()].
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
#' @param type Rendering type. `"lollipop"` (the default) draws vertical sticks
#'   capped with points. `"bar"` draws PMF values as bars.
#' @param point_size Size of the points at the top of each lollipop (defaults to
#'   2.5).
#' @param stick_linewidth Linewidth of the vertical sticks (defaults to 0.25).
#' @param stick_linetype Linetype of the vertical sticks (defaults to
#'   `"dashed"`).
#' @param color (Optional) Fixed color for the lollipop points and segments.
#'   When omitted, lollipops render with the geom's default color (black)
#'   unless a `colour` aesthetic is mapped (e.g.
#'   `aes(colour = after_stat(probs))`); when supplied, it overrides any
#'   colour mapping.
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
#' @param shade_hdr (Optional) A numeric vector of target coverages for the
#'   highest density regions (HDRs) to shade, each strictly between 0 and 1,
#'   e.g. `shade_hdr = c(0.5, 0.8, 0.95)`: the smallest sets of support points
#'   containing at least the specified probability masses. Each support point
#'   is assigned the smallest requested HDR containing it; the assignment is
#'   exposed as the ordered factor `after_stat(probs)` and mapped to `alpha`
#'   by default, so points outside all requested regions render nearly
#'   transparent. Because a discrete distribution may not achieve the exact
#'   coverages, the smallest HDR with coverage >= each target is used; HDRs
#'   are threshold-based, so all support points tied at the cutoff mass are
#'   included and the actual coverage can exceed the target. A message is
#'   issued via [cli::cli_inform()] reporting the actual coverages whenever
#'   they differ.
#' @param ... Other parameters passed on to [ggplot2::layer()].
#'
#' @section Computed variables:
#' These are calculated by the `stat` part of the layer and can be accessed
#' with [ggplot2::after_stat()].
#' \describe{
#'   \item{`after_stat(x)`}{Support points at which the PMF is evaluated.}
#'   \item{`after_stat(y)`}{Probability mass at each support point.}
#'   \item{`after_stat(probs)`}{Only when `shade_hdr` is supplied: the
#'   smallest requested HDR containing each support point, as an ordered
#'   factor whose outermost level (e.g. `">95%"`) collects points outside all
#'   requested regions.}
#' }
#'
#' @section Aesthetics:
#' `geom_pmf()` does not require any input aesthetics when `fun` is supplied.
#' It understands the following aesthetics:
#' \describe{
#'   \item{Computed position aesthetics}{`x` and `y`, mapped by default to
#'   `after_stat(x)` and `after_stat(y)`. For lollipops, `yend` is also mapped
#'   internally so the y scale includes zero.}
#'   \item{Drawing aesthetics}{`alpha`, `colour`/`color`, `fill`, `group`,
#'   `linetype`, `linewidth`, `shape`, `size`, and `stroke` for the lollipop
#'   display; bar displays use the usual rectangle aesthetics from
#'   [ggplot2::geom_col()].}
#' }
#' The points use the fillable shape 21 by default, with `fill` following
#' `colour` when unset, so default lollipops render solid. Mapping `fill`
#' (e.g. `fill = after_stat(probs)`) colors the point interiors while the
#' sticks and outlines keep the `colour` aesthetic.
#'
#' @return A ggplot2 layer.
#'
#' @seealso [geom_pdf()], [geom_cdf_discrete()], [geom_qf_discrete()], and
#'   [geom_survival_discrete()] for related discrete distribution-function
#'   layers.
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
#' # Shade the 50/80/95% HDRs
#' ggplot() +
#'   geom_pmf(fun = dbinom, xlim = c(0, 10), args = list(size = 10, prob = 0.5),
#'     shade_hdr = c(0.5, 0.8, 0.95))
#'
#' @name geom_pmf
#' @aliases StatPMF GeomPMF GeomPMFBar
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
                     type = c("lollipop", "bar"),
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
                     shade_hdr = NULL) {
  type <- match.arg(type)

  if (is.null(data)) data <- ensure_nonempty_data(data)

  if (identical(type, "lollipop")) {
    geom <- GeomPMF
    default_mapping <- aes(x = after_stat(x), y = after_stat(y))
  } else {
    geom <- GeomPMFBar
    default_mapping <- aes(x = after_stat(x), y = after_stat(y))
  }

  # The HDR alpha default is added only when the user supplied neither a
  # static alpha nor an alpha mapping, so a user override never triggers a
  # duplicated-aesthetic warning.
  has_user_alpha <- "alpha" %in% names(list(...)) ||
    (!is.null(mapping) && "alpha" %in% names(mapping))
  if (!is.null(shade_hdr) && !has_user_alpha) {
    default_mapping <- modifyList(default_mapping, aes(alpha = after_stat(probs)))
  }

  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

  params <- list(
    fun = fun,
    xlim = xlim,
    support = support,
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

  if (identical(type, "lollipop")) {
    params$point_size <- point_size
    params$stick_linewidth <- stick_linewidth
    params$stick_linetype <- stick_linetype
  }

  # Forward color as a fixed aesthetic only when explicitly supplied, so
  # mapped colour aesthetics are not silently overridden by the default.
  if (!missing(color)) {
    params$color <- color
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' @rdname geom_pmf
#' @export
StatPMF <- ggproto("StatPMF", Stat,

  default_aes = aes(x = NULL, y = after_stat(y)),

  compute_group = function(data, scales, fun, xlim = NULL, support = NULL, args = NULL,
                           shade_hdr = NULL, p = NULL, lower.tail = TRUE,
                           p_lower = NULL, p_upper = NULL, shade_outside = FALSE,
                           ...) {

    x_vals <- discrete_support(xlim = xlim, support = support)

    if (length(x_vals) == 0L) {
      out <- data.frame(x = numeric(0), y = numeric(0))
      if (!is.null(shade_hdr)) out$probs <- factor(character(0), ordered = TRUE)
      out$in_shade <- logical(0)
      return(out)
    }

    # Evaluated and structurally validated exactly once: the same
    # mass vector drives checks, plotting, HDRs, and shading.
    y_vals <- evaluate_pmf(
      fun, x_vals, args = args, arg = "fun",
      normalization = "warn", tol = 1e-2
    )
    out <- data.frame(
      x = scale_forward(scales$x, x_vals),
      x_eval = x_vals,
      y = scale_forward(scales$y, y_vals),
      mass = y_vals
    )

    if (!is.null(shade_hdr)) {
      out$probs <- discrete_hdr_probs(y_vals, shade_hdr)
    }

    # Mass baseline: raw zero, transformed once when finite in the
    # transformation domain; metadata otherwise.
    out$baseline_panel <- resolve_stat_baseline(scales$y, 0)$panel

    # Resolve p-based shading here, per group, so the cumulative probability
    # never crosses group boundaries (a panel-level cumsum would mis-shade the
    # second and later groups).
    out$in_shade <- pmf_shade_index(
      y_vals, p = p, lower.tail = lower.tail,
      p_lower = p_lower, p_upper = p_upper, shade_outside = shade_outside
    )

    out
  }
)

#' @rdname geom_pmf
#' @export
GeomPMF <- ggproto("GeomPMF", GeomPoint,

  optional_aes = c("yend"),

  default_aes = modifyList(GeomPoint$default_aes, aes(shape = 21)),

  setup_data = function(data, params) {
    # The mass baseline trains the y scale when it is finite under the
    # active transformation; the lollipop sticks then drop
    # to that baseline at draw time.
    if ("baseline_panel" %in% names(data)) {
      data$ymin <- ifelse(is.finite(data$baseline_panel),
                          data$baseline_panel, NA_real_)
    }
    data
  },

  draw_key = function(data, params, size) {
    data$fill <- ifelse(is.na(data$fill), data$colour, data$fill)
    ggplot2::draw_key_point(data, params, size)
  },

  draw_panel = function(self, data, panel_params, coord, na.rm = FALSE,
                        point_size = 2.5, stick_linewidth = 0.25,
                        stick_linetype = "dashed",
                        p = NULL, lower.tail = TRUE,
                        p_lower = NULL, p_upper = NULL,
                        shade_outside = FALSE) {

    n <- nrow(data)

    # Shading membership is resolved per group in StatPMF$compute_group (so the
    # cumulative probability never crosses group boundaries); shade_hdr is
    # handled separately via the alpha-mapped probs factor.
    in_shade <- if (!is.null(data$in_shade)) data$in_shade else rep(TRUE, n)

    # Lollipop sticks drop to the transformed raw-zero baseline, clipped to
    # the visible panel floor when the transformation excludes zero.
    baseline_panel <- if ("baseline_panel" %in% names(data)) {
      data$baseline_panel[1]
    } else {
      0
    }
    base_y <- baseline_draw_value(baseline_panel, panel_params)

    # Build segment data: unshaded segments are dimmed multiplicatively
    seg_data          <- transform(data, yend = y, y = base_y)
    seg_data$linewidth <- stick_linewidth
    seg_data$linetype  <- stick_linetype
    seg_data$alpha     <- dim_alpha(seg_data$alpha, in_shade)
    seg_data$size      <- NULL

    seg_grob <- ggproto_parent(GeomSegment, self)$draw_panel(
      seg_data, panel_params, coord, na.rm = na.rm
    )

    # Build point data: unshaded points are dimmed, and the fillable point
    # shape follows colour when fill is unset so default lollipops are solid
    pt_data         <- data
    pt_data$size    <- point_size
    pt_data$alpha   <- dim_alpha(pt_data$alpha, in_shade)
    pt_data$fill    <- ifelse(is.na(pt_data$fill), pt_data$colour, pt_data$fill)

    pt_grob <- ggproto_parent(GeomPoint, self)$draw_panel(
      pt_data, panel_params, coord, na.rm = na.rm
    )

    grid::grobTree(seg_grob, pt_grob)
  }
)

#' @rdname geom_pmf
#' @export
GeomPMFBar <- ggproto("GeomPMFBar", ggplot2::GeomCol,

  setup_data = function(self, data, params) {
    data <- ggproto_parent(ggplot2::GeomCol, self)$setup_data(data, params)
    # Bars rest on the transformed raw-zero baseline, not panel zero; a
    # transform-excluded baseline emits no training value and is clipped to
    # the panel floor at draw time.
    if ("baseline_panel" %in% names(data)) {
      base <- data$baseline_panel
      data$ymin <- ifelse(is.finite(base), pmin(data$y, base), NA_real_)
      data$ymax <- ifelse(is.finite(base), pmax(data$y, base), data$y)
    }
    data
  },

  draw_panel = function(self, data, panel_params, coord, na.rm = FALSE,
                        lineend = "butt", linejoin = "mitre") {
    in_shade <- if (!is.null(data$in_shade)) {
      data$in_shade
    } else {
      rep(TRUE, nrow(data))
    }
    data$alpha <- dim_alpha(data$alpha, in_shade)

    if (any(!is.finite(data$ymin))) {
      floor_y <- baseline_draw_value(NA_real_, panel_params)
      data$ymin[!is.finite(data$ymin)] <- floor_y
    }

    ggproto_parent(ggplot2::GeomCol, self)$draw_panel(
      data, panel_params, coord, lineend = lineend, linejoin = linejoin
    )
  }
)
