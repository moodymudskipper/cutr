

#' Middle of range
#'
#' For easy use in center_fun argument of cut3
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

cut_explicit <- function(x, cuts , labels, simplify, closed, squeeze, open_end, brackets, sep, center_fun, format_fun, ...) {
  #browser()
  cuts <- unique(cuts)
  bins <- .bincode(as.numeric(x), breaks = cuts, right = closed == "right", include.lowest = !open_end)

  if (is.null(labels)) {
    if (is.null(center_fun)) {
      # format
      if (is.factor(x)) cuts <- setNames(cuts, levels(x)[cuts])
      if (squeeze) {
        labels <- format_interval_squeezed(x, cuts, closed, open_end, brackets, sep, format_fun,...)
      } else {
      labels <- format_interval(cuts, closed, open_end, brackets, sep, format_fun,...)
      }

      # simplify
      if (simplify) {
        ind <- tapply(x,bins,FUN = function(xi) if (length(unique(xi)) == 1) xi[1] else NA)
        ind <- ind[!is.na(ind)]
        if (length(ind))
          labels[as.numeric(names(ind))] <-
          if (is.factor(x)) levels(x)[ind] else ind
      }
    } else {
      # apply center_fun
      vals <- tapply(x, bins, function(w) center_fun(w, na.rm = TRUE))
      labels <- format_fun(vals, ...)
    }
  }

  bins <- factor(bins,labels = labels,ordered = TRUE)
  bins
}
