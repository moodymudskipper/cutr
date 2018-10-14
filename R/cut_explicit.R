
cut_explicit <- function(
  bins, x, cuts , labels, simplify, closed, squeeze, open_end,
  brackets, sep, format_fun, output, ...) {

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
      ind <- tapply(x,bins,FUN = function(xi)
        if (length(unique(xi)) == 1) xi[1] else NA)
      # remove nas, ind is named with bins
      ind <- ind[!is.na(ind)]
      if (length(ind))
        labels[as.numeric(names(ind))] <-
        if (is.factor(x)) levels(x)[ind] else ind
    }
  } else if (is.function(labels)) {
    # u is values in bins, v is left cut point, w is right cut point
    is_factor_lgl <- is.factor(x)
    labels   <- mapply(
      function(u,v,w) {
        ifelse(is_factor_lgl,
               labels(u,factor(levels(x)[c(v,w)], levels = levels(x))),
               labels(u,c(v,w)))
        },
      split(x,bins),
      cuts[-length(cuts)],
      cuts[-1])
    if (is.numeric(labels)) labels <- format_fun(labels, ...)
  }

  bins <- factor(
    bins,levels = seq_along(labels), labels = labels,ordered = TRUE)

  if (squeeze) {
    # to squeeze empty intervals let cuts at original pos but use open brackets
    levels_ <- levels(bins)
    notin_  <- !levels(bins) %in% bins
    levels_notin_ <- levels_[notin_]
    b2 <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", brackets[2])
    b4 <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", brackets[4])
    levels_notin_ <- sub(paste0("^",b2),brackets[1],levels_notin_)
    levels_notin_ <- sub(paste0(b4,"$"),brackets[3],levels_notin_)
    levels(bins)[notin_] <- levels_notin_
  }

  # coerce to appropriate class (ordered factor by default)
  if (output == "character") {
    bins <- as.character(bins)
  } else if (output == "factor") {
    bins <- factor(bins,ordered = FALSE)
  } else if (output == "labels") {
    bins <- levels(bins)
  }

  bins
}
