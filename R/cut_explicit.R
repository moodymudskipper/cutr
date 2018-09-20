
cut_explicit <- function(
  x, cuts , labels, simplify, closed, squeeze, open_end,
  brackets, sep, format_fun, ...) {
  #browser()
  bins <- .bincode(as.numeric(x), breaks = cuts, right = closed == "right", include.lowest = !open_end)
  # cuts <- cuts[min(bins):(length(unique(bins)) + 1)]
  # bins <- bins - min(bins) + 1

  # warn if incorrect number of labels, and proceed with auto labels
  if (!is.null(labels) && !is.function(labels)) {
    if (length(labels) != length(cuts) - 1) {
      warning("incorrect number of labels proceeding with default labels")
      labels <- NULL
    }
  }

  if (is.null(labels)) {
    # format
    if (is.factor(x)) cuts <- setNames(cuts, levels(x)[cuts])
    if (squeeze) {
      labels <- format_interval_squeezed(
        x = x, cuts = cuts, closed = closed, open_end = open_end,
        brackets = brackets, sep = sep, format_fun = format_fun, ...)
    } else {
      labels <- format_interval(
        cuts = cuts, closed = closed, open_end = open_end,
        brackets = brackets, sep = sep, format_fun = format_fun,...)
    }

    # simplify
    if (simplify) {
      # ind is NA or unique(x) value for each bin
      ind <- tapply(x,bins,FUN = function(xi) if (length(unique(xi)) == 1) xi[1] else NA)
      # remove nas, ind is named with bins
      ind <- ind[!is.na(ind)]
      if (length(ind))
        labels[as.numeric(names(ind))] <-
        if (is.factor(x)) levels(x)[ind] else ind
    }
  } else if (is.function(labels)) {
    # apply center_fun
    vals <- tapply(x, bins, function(w) labels(w))
    labels <- format_fun(vals, ...)
  }

  bins <- factor(bins,levels = seq_along(labels), labels = labels,ordered = TRUE)
  bins
}


#' Middle of range
#'
#' For easy use in labels argument of cut3
#'
#' @param x a numeric vector
#'
#' @return a double
#' @export
#'
#' @examples
#' middle(c(10,12,20))
middle <- function(x) {
  mean(range(x))
}
