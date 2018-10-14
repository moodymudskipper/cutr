#' get cut points
#'
#' mainly used by `smart_cut` but can be used to compute cut point without assessing
#' intervals to data points
#'
#' @inheritParams smart_cut
#' @param optim_fun optimization function (used if `what = "group"`)
#' @param width_fun left boundary function (used if `what = "width"`)
#'
#' @seealso ?smart_cut
#' @return a vector of cut points
get_cuts <- function(x, i, what, expand, crop, closed = "left", open_end, optim_fun = NULL, width_fun = NULL){
  xrange <- range(x, na.rm = TRUE)
  xmin <- xrange[1]
  xmax <- xrange[2]

  if (what == "width") {
    what <- "breaks"
    if (is.null(width_fun))            width_fun <- "left"
    if (is.numeric(width_fun))         cut1 <- width_fun
    else if (is.function(width_fun))   cut1 <- width_fun(x,i)
    else if (width_fun == "left")      cut1 <- xmin
    else if (width_fun == "right")     cut1 <- xmax # xmin + (xmax - xmin + i) %% i - i
    else if (width_fun == "centered")  cut1 <- (xmin + xmax) / 2 #xmin + ((xmax-xmin + i) %% i - i)/2
    else if (width_fun == "centered0") cut1 <- i/2 # 0 - i/2 + i * (xmin %/% i)
    cut1 <- xmin - (i - cut1 + xmin) %% i
    i <- seq(cut1, by = i, length.out = ceiling((xmax - cut1)/i) + 1)
  }

  cuts <- switch(
    what,
    groups      = {
      if (is.null(optim_fun)) {
        cuts <- unique(quantile(x, seq(0, 1, length.out = i + 1), na.rm = TRUE, names = FALSE,type = 3))
      } else {
        cuts <- get_optimal_cutpoints(x, i, optim_fun, closed)
        #cuts <- c(xmin,cuts[cuts > xmin & cuts < xmax],xmax)
      }
      cuts
    },
    breaks      = {
      cuts <- sort(i)
      if (expand) {
        # doubling the boundary in case of cut on open ended boundary
        if (xmax == cuts[length(cuts)] & closed == "left" & open_end)
          cuts <- c(cuts, xmax)
        if (xmin == cuts[1] & closed == "right" & open_end)
          cuts <- c(xmin, cuts)
        if (xmin < cuts[1]) cuts <- c(xmin, cuts)
        if (xmax > cuts[length(cuts)]) cuts <- c(cuts, xmax)

      } else if (crop) {
        # we limit x to what's inside the cuts, "inside" depends on `closed`
        if (closed == "left" || !open_end)
          xmin <- min(x[x >= cuts[1]]) else
          xmin <- min(x[x > cuts[1]])
        if (closed == "right" || !open_end)
          xmax <- max(x[x <= cuts[length(cuts)]]) else
          xmax <- max(x[x < cuts[length(cuts)]])
      }
      if (crop) {
        # if xmin is on a closed right border, cropping should not be done
        # or it will be passed to next interval
        if (closed == "left"  || !xmin %in% cuts) cuts <- c(head(cuts[cuts < xmin],-1), xmin, cuts[cuts > xmin])
        if (closed == "right" || !xmax %in% cuts) cuts <- c(cuts[cuts < xmax], xmax, cuts[cuts > xmax][-1])
      }
      cuts
      },
    n_intervals = seq(xmin,xmax, len = i + 1),
    cluster      = {
      sortx <- sort(x)
      bins <- kmeans(sortx,i)$cluster
      cuts <- c(sortx[1], approx(seq_along(x),x,cumsum(rle(bins[-length(bins)])$lengths) + 0.5)$y)
    }
  )
  cuts
}
