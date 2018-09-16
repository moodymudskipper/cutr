#' Cut a Numeric Variable into Intervals
#'
#' This behaves by default the same as Frank Harrel's \code{Hmisc::cut2},
#' and the code is essentially taken straight from the original.
#' The difference is that the \code{digits} argument was renamed to \code{levels.digits}
#' and that there are 2 additonal arguments : \code{format_fun} and \code{...} .
#' \code{format_fun} supports choosing another formating function than \code{format}, and \code{...}
#' is passed to the formatting function.
#'
#' The motivation that led to modify the function was to be able to use \code{format_metric}
#' to have more readable cuts in some cases.
#'
#'
#' @param x numeric vector to classify into intervals
#' @param format_fun formatting function
#' @param ... additional arguments passed to \code{format_fun}
#'
#' @return a factor variable with levels of the form \code{"[a,b]"} or formatted means (character strings) unless \code{onlycuts} is \code{TRUE} in which case a numeric vector is returned
#' @export
#'
#' @examples
#' set.seed(1)
#' numbers  <- cumsum(abs(rnorm(100,100,100)))
#' numbers  <- cumsum(abs(rnorm(10,10,10)))
#' cut3(numbers, 10,"groups",format_fun = format_metric, digits=2)
cut3 <- function(
  x,
  i,
  what = c("breaks","groups","n_by_group","n_intervals","width",
           "width_0", "width_min", "width_max"),
  labels     = NULL,
  closed     = c("left","right"),
  expand     = TRUE,
  crop       = FALSE,
  simplify   = TRUE,
  squeeze    = FALSE,
  open_end   = FALSE,
  brackets   = c("(","[",")","]"),
  sep        = ",",
  output     = c("ordered","factor","character"),
  center_fun = NULL,
  optim_fun  = NULL,
  format_fun = formatC, ...){

  # checks
  what   <- match.arg(what)
  closed <- match.arg(closed)
  output <- match.arg(output)
  i >= 1 || what == "breaks" || stop("i must be positive")

  # handle factors
  if (is.factor(x) && what == "breaks" && is.character(i)) i <- match(i,levels(x))

  # convert "n_by_group" case into a "groups" case
  if (what == "n_by_group") {
    i <- max(1, floor(sum(!is.na(x))/i))
    what <- "groups"}

  # get breaks
  cuts <- get_cuts(as.numeric(x), i, what, expand, crop, closed, optim_fun)
  if (length(cuts) == 1 && !expand)
    stop("Can't cut data if only one break is provided and `expand` is FALSE")

  # warn if incorrect number of labels, and proceed with auto labels
  if (!is.null(labels)) {
    if (length(labels) != length(cuts) - 1) warning("incorrect number of labels")
    labels <- NULL
  }

  # get raw output
  bins <- cut_explicit(x, cuts , labels, simplify, closed, squeeze,
                     open_end, brackets, sep, center_fun, format_fun, ...)

  # coerce to appropriate class (ordered by default)
  if (output == "character") {
    bins <- as.character(bins)
  } else if (output == "factor") {
    bins <- factor(bins,ordered = FALSE)
  }

  bins
}
