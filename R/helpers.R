get_cuts <- function(x, i, what, expand, crop, closed = "left", optim_fun = NULL){
  xrange <- range(x, na.rm = TRUE)
  xmin <- xrange[1]
  xmax <- xrange[2]

  cuts <- switch(
    what,
    groups      = {
      if (is.null(optim_fun)) {
        cuts <- quantile(x, seq(0, 1, length.out = i + 1), na.rm = TRUE,names = FALSE,type = 3)
      } else {
        cuts <- get_optimal_cutpoints(x, i, optim_fun, closed)
        #cuts <- c(xmin,cuts[cuts > xmin & cuts < xmax],xmax)
      }
      cuts
    },
    breaks      = {
      cuts <- sort(i)
      if (expand) {
        if (xmin < cuts[1])            cuts <- c(xmin, cuts)
        if (xmax > cuts[length(cuts)]) cuts <- c(cuts, xmax)
      } else if (crop) {
        xmin <- min(x[x >= cuts[1]])
        xmax <- max(x[x <= cuts[length(cuts)]])
      }
      if (crop) c(xmin,cuts[cuts > xmin & cuts < xmax],xmax) else cuts},
    n_intervals = seq(xmin,xmax, len = i + 1),
    width       = {
      adj_range <- i*(round(xrange/i) + c(-0.5,0.5));
      cuts <- seq(adj_range[1],adj_range[2], i)
      if (crop) c(xmin,cuts[cuts > xmin & cuts < xmax],xmax) else cuts},
    width_0     = seq(i*floor(xmin/i),i*ceiling(xmax/i),i),
    width_min   = {
      cuts <- union(seq(xmin, xmax, i),xmax)
      if (!crop) cuts[length(cuts)] <- cuts[length(cuts) - 1] + i
      cuts},
    width_max    = {
      cuts <- union(xmin, rev(seq(xmax, xmin, -i)))
      if (!crop) cuts[1] <- cuts[2] - i
      cuts}
  )
  cuts
}

format_interval <- function(cuts, closed, open_end, brackets, sep, format_fun,...){
  `.(` <- brackets[1]
  `.[` <- brackets[2]
  `.)` <- brackets[3]
  `.]` <- brackets[4]
  l <- length(cuts)
  if (closed == "left") {
  left     <- `.[`
  right    <- c(rep(`.)`, l - 2),
                if (open_end) `.)` else `.]`)
  } else if (closed == "right") {
    right     <- `.]`
    left    <- c(if (open_end) `.(` else `.[`,
                 rep(`.(`, l - 2))
  }
  cuts_chr <- format_fun(cuts, ...)
  labels   <- paste(left, cuts_chr[-l], sep, cuts_chr[-1], right, sep = "")
  labels
}

cut_explicit <- function(x, cuts , labels, simplify, closed, open_end, brackets, sep, center_fun, format_fun, ...) {
  #browser()
  cuts <- unique(cuts)
  bins <- .bincode(x, breaks = cuts, right = closed == "right", include.lowest = !open_end)


  if (is.null(labels)) {
    if (is.null(center_fun)) {
      labels <- format_interval(cuts, closed, open_end, brackets, sep, format_fun,...)

      if (simplify) {
        ind <- tapply(x,bins,FUN = function(xi) if (length(unique(xi)) == 1) xi[1] else NA)
        ind <- ind[!is.na(ind)]
        if (length(ind))
          labels[as.numeric(names(ind))] <- ind
      }
    } else {
      vals <- tapply(x, bins, function(w) center_fun(w, na.rm = TRUE))
      labels <- format_fun(vals, ...)
    }
  }

  bins <- factor(bins,labels = labels,ordered = TRUE)
  bins
}

