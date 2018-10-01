#' get cut points
#'
#' mainly used by `cutf3` but can be used to compute cut point without assessing
#' intervals to data points
#'
#' @inheritParams cutf3
#' @param optim_fun optimization function (used if `what = "group"`)
#' @param width_fun left boundary function (used if `what = "width"`)
#'
#' @seealso cutf3
#' @return a vector of cut points
#' @export
get_cuts <- function(x, i, what, expand, crop, closed = "left", open_end, optim_fun = NULL, width_fun = NULL){
  xrange <- range(x, na.rm = TRUE)
  xmin <- xrange[1]
  xmax <- xrange[2]

  if (what == "width") {
    what <- "breaks"
    if (is.null(width_fun)) width_fun <- "min"
    if (is.numeric(width_fun))       cut1 <- width_fun
    else if (is.function(width_fun)) cut1 <- width_fun(x,i)
    else if (width_fun == "min")     cut1 <- xmin
    #else if (width_fun == "default") cut1 <- xmin - i/2

    if (identical(width_fun, "max"))
      i <- rev(seq(xmax,by = -i, length.out = ceiling((xmax - xmin)/i) + 1))
    else
      i <- seq(cut1, by = i, length.out = ceiling((xmax - cut1)/i) + 1)
  }

  cuts <- switch(
    what,
    groups      = {
      if (is.null(optim_fun)) {
        cuts <- quantile(x, seq(0, 1, length.out = i + 1), na.rm = TRUE, names = FALSE,type = 3)
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
        # or it will be passed tonext interval
        if (closed == "left"  || !xmin %in% cuts) cuts <- c(head(cuts[cuts < xmin],-1), xmin, cuts[cuts > xmin])
        if (closed == "right" || !xmax %in% cuts) cuts <- c(cuts[cuts < xmax], xmax, cuts[cuts > xmax][-1])
      }
      cuts
      },
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
