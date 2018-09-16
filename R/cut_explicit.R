cut_explicit <- function(x, cuts , labels, simplify, closed, open_end, brackets, sep, center_fun, format_fun, ...) {
  #browser()
  cuts <- unique(cuts)
  bins <- .bincode(as.numeric(x), breaks = cuts, right = closed == "right", include.lowest = !open_end)

  if (is.null(labels)) {
    if (is.null(center_fun)) {
      # format
      if (is.factor(x)) cuts <- setNames(cuts,levels(x)[cuts])
      labels <- format_interval(cuts, closed, open_end, brackets, sep, format_fun,...)

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
