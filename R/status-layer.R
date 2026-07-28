# R/status-layer.R
#
# Censoring-status normalization (spec A-03).
#
# `status` is documented as logical or 0/1 numeric. Logical (and any other
# discrete) status must not create statistical groups: ggplot2 derives
# implicit groups from discrete aesthetics before compute_group(), which
# would split the risk set into event-only and censor-only curves. The
# status-aware Layer below normalizes the mapped status to integer right
# after aesthetic computation — before implicit grouping is consumed — and
# re-derives implicit groups from the remaining discrete aesthetics. An
# explicitly mapped `group` is preserved unchanged.

#' Normalize a censoring status vector to integer 0/1 (spec A-03).
#'
#' Accepts logical and exact-0/1 numeric input; preserves `NA` so the
#' documented `na.rm` policy can operate downstream; rejects factors and
#' character vectors, whose codes must never be interpreted silently.
#' @noRd
normalize_status <- function(status) {
  if (is.factor(status) || is.character(status)) {
    cli::cli_abort(c(
      "{.field status} must be a logical or 0/1 numeric vector, not a factor or character vector.",
      "i" = 'Recode explicitly, e.g. {.code status = as.integer(status == "1")}.'
    ))
  }
  if (is.logical(status)) {
    return(as.integer(status))
  }
  if (is.numeric(status)) {
    bad <- !is.na(status) & !(status %in% c(0, 1))
    if (any(bad)) {
      cli::cli_abort(
        "{.field status} must contain only 0/1 or FALSE/TRUE values (missing values follow the {.arg na.rm} policy)."
      )
    }
    return(as.integer(status))
  }
  cli::cli_abort("{.field status} must be a logical or 0/1 numeric vector.")
}

#' Re-derive implicit group ids from the discrete aesthetics of a layer's
#' computed data (used after status normalization removes `status` from the
#' discrete set). Mirrors ggplot2's implicit grouping: the interaction of
#' all discrete, non-positional columns; -1 when there are none.
#' @noRd
rederive_layer_groups <- function(data) {
  discrete <- vapply(
    data,
    function(col) is.factor(col) || is.character(col) || is.logical(col),
    logical(1)
  )
  discrete[names(data) %in% c("label", "PANEL", "group")] <- FALSE
  if (any(discrete)) {
    ids <- interaction(data[discrete], drop = TRUE, lex.order = TRUE)
    data$group <- as.integer(ids)
  } else {
    data$group <- -1L
  }
  data
}

# A template layer instance whose ggproto parent chain carries the standard
# Layer methods; subclassing it avoids reaching for the unexported
# ggplot2 Layer class. layer(layer_class = ) overrides every instance field.
.layer_template <- ggplot2::layer(
  geom = "blank", stat = "identity", position = "identity"
)

#' @noRd
StatusAwareLayer <- ggproto(
  "StatusAwareLayer", .layer_template,
  compute_aesthetics = function(self, data, plot) {
    out <- ggproto_parent(.layer_template, self)$compute_aesthetics(data, plot)
    if ("status" %in% names(out)) {
      out$status <- normalize_status(out$status)
      mapping <- self$computed_mapping %||% self$mapping
      if (!("group" %in% names(mapping))) {
        out <- rederive_layer_groups(out)
      }
    }
    out
  }
)

#' A layer() wrapper that installs the status-aware Layer class (A-03).
#' @noRd
status_layer <- function(...) {
  layer(..., layer_class = StatusAwareLayer)
}
