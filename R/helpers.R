get_cuts <- function(x, i, what, expand, crop){
  xrange <- range(x, na.rm = TRUE)
  xmin <- xrange[1]
  xmax <- xrange[2]

  cuts <- switch(
    what,
    groups      = quantile(x, seq(0, 1, length.out = i + 1), na.rm = TRUE,names = FALSE),
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


# simplify could work only on cutpoints or work for all points, and then it could crop neighbours or not
# 4 options : any_and_crop_neighbours, cutpoints, any, NULL

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
    left    <- c(rep(`.(`, l - 2),
                  if (open_end) `.(` else `.[`)
  }
  cuts_chr <- format_fun(cuts, ...)
  labels   <- paste(left, cuts_chr[-l], sep, cuts_chr[-1], right, sep = "")
  labels
}

cut_explicit <- function(x, cuts , labels, simplify, closed, open_end, brackets, sep, center_fun, format_fun, ...) {

  cuts <- unique(cuts)
  y <- .bincode(x,cuts, closed == "right", !open_end)

  if (!is.null(labels)) {
    if (length(labels) != length(cuts) - 1) warning("incorrect number of labels")
    labels <- NULL
  }

  if (is.null(labels)) {
    if (is.null(center_fun)) {
      labels <- format_interval(cuts, closed, open_end, brackets, sep, format_fun,...)
      if (simplify) {
        ind <- which(!duplicated(y) & !duplicated(y,fromLast = TRUE))
        if (length(ind))
          labels[y[ind]] <- format_fun(c(x[ind],cuts),...)[1:length(ind)]
      }
    } else {
      vals <- tapply(x, y, function(w) center_fun(w, na.rm = TRUE))
      labels <- format_fun(vals, ...)
    }
  }
  # message("using traditional cut with right = FALSE and include.lowest = FALSE")
  # print(table(cut(x,cuts,right = FALSE, include.lowest = TRUE)))
  levels(y) <- labels
  list(x,y)
}
