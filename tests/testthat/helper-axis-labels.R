plot_axis_titles <- function(plot) {
  grob <- ggplotGrob(plot)

  extract_label <- function(name) {
    idx <- match(name, grob$layout$name)
    if (is.na(idx)) {
      return(NA_character_)
    }

    label_grob <- grob$grobs[[idx]]
    if (!is.null(label_grob$children) && length(label_grob$children) > 0) {
      label_grob <- label_grob$children[[1]]
    }

    if (is.null(label_grob$label)) NA_character_ else label_grob$label
  }

  c(
    x = extract_label("xlab-b"),
    y = extract_label("ylab-l")
  )
}

plot_x_range <- function(plot) {
  ggplot_build(plot)$layout$panel_params[[1]]$x.range
}
