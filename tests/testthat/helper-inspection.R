# Shared inspection helpers for value-level and render-level assertions:
# trained panel ranges, grob-tree searches, and layer lookup
# by stat class, so tests need not hard-code deeply nested grob indices.

plot_x_range <- function(p) {
  ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x.range
}

plot_y_range <- function(p) {
  ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y.range
}

# Index of the layer(s) whose stat inherits from `stat_class` in a built plot.
layers_with_stat <- function(built, stat_class) {
  which(vapply(
    built$plot$layers,
    function(layer) inherits(layer$stat, stat_class),
    logical(1)
  ))
}

# Depth-first collection of grobs inheriting from any of `classes`.
# Descends into gtables, gTrees, and gLists.
find_grobs <- function(grob, classes) {
  found <- list()
  walk <- function(g) {
    if (inherits(g, classes)) found[[length(found) + 1L]] <<- g
    if (inherits(g, "gtable")) {
      for (child in g$grobs) walk(child)
    } else if (inherits(g, "gTree")) {
      for (child in g$children) walk(child)
    } else if (inherits(g, "gList")) {
      for (child in g) walk(child)
    }
  }
  walk(grob)
  found
}

# All grobs of the given classes drawn by layer `i` of plot `p`.
layer_grobs <- function(p, i = 1, classes = NULL) {
  grobs <- ggplot2::layer_grob(p, i)
  if (is.null(classes)) return(grobs)
  out <- list()
  for (g in grobs) out <- c(out, find_grobs(g, classes))
  out
}

# Numeric values of a grid unit, in its native units.
unit_values <- function(u) {
  as.numeric(u)
}
