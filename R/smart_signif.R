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
#' @param ... unused, kept for compatibility reasons
#' @return numeric vector
#' @export
#' @md
#' @examples
#' \dontrun{
#' set.seed(1)
#' x  <- cumsum(abs(rnorm(10,100,100)))
#' smart_signif(x, 1)
#' smart_signif(x, 2)
#' smart_signif(x, 3)
#' }
smart_signif <- function(x, digits=3L, ...) {
  stopifnot(length(x) > 1L)
  logscale <- ceiling(log10(abs(x)))
  logdiff <- log10(diff(x))
  keepoom <- floor(pmin(c(Inf, logdiff), c(logdiff, Inf)))
  signif(x, pmax(digits, logscale - keepoom))
}
