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
