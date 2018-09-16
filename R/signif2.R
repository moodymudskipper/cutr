#' Minimum preferred significant digits
#'
#' @details
#' Facilitate reducing numbers to their least *distinguishable*
#' significant digits, where "distinguishable" means
#' "between neighbors". This means that if reducing more digits would
#' cause two neighbors to reduce to the same number, then the
#' reduction cannot take place.
#'
#' References:
#'
#' - [Original question on StackOverflow](https://stackoverflow.com/q/51616332/3358272)
#'
#' @param x numeric, length 2 or more
#' @param digits integer, number of preferred remaining significant digits
#' @return numeric vector
#' @export
#' @md
#' @examples
#' \dontrun{
#' set.seed(1)
#' x  <- cumsum(abs(rnorm(10,100,100)))
#' # [1]   37.35462  155.71895  172.15609  431.68417  564.63495  582.58811  731.33101  905.16348 1062.74162 1132.20278
#' signif2(x, 1)
#' #  [1]   40  160  170  400  560  580  700  900 1060 1130
#' signif2(x, 2)
#' #  [1]   37  160  170  430  560  580  730  910 1060 1130
#' signif2(x, 3)
#' #  [1]   37.4  156.0  172.0  432.0  565.0  583.0  731.0  905.0 1060.0 1130.0
#' }
signif2 <- function(x, digits=1L) {
  stopifnot(length(x) > 1L)
  logscale <- ceiling(log10(abs(x)))
  logdiff <- log10(diff(x))
  keepoom <- floor(pmin(c(Inf, logdiff), c(logdiff, Inf)))
  signif(x, pmax(digits, logscale - keepoom))
}
