#' Cut a Numeric Variable into Intervals
#'
#' Enhanced cut that supports among other things factor inputs, optimal grouping,
#' and flexible formatting.
#'
#' @param x numeric vector to classify into intervals
#' @param i numeric or character, main parameter depending on `what`
#' @param what character, choices can be abreviated
#' @param labels character of the same length as the resulting bins
#' @param closed character, which side of the intervals should be closed
#' @param expand logical, if TRUE cuts are added if necessary to cover min and max values
#' @param crop logical, if TRUE intervals which go past the min or max values will be cropped
#' @param simplify logical, if TRUE categories containing only one distinct
#' value will be named by it
#' @param squeeze logical, if TRUE all bins are cropped so they are closed on
#'  both sides on their min and max values, useful for sparse data and factors
#' @param open_end keep the open side open at the extremities
#' @param sep,brackets character, used to build the default labels
#' @param output character, class of output
#' @param center_fun function or formula to apply on bin content to build a label
#' @param optim_fun character or 2 argument function
#' @param format_fun formatting function
#' @param ... additional arguments passed to \code{format_fun}
#'
#' @section what:
#'
#' Depending on the value of `what`, i is:
#'
#' \describe{
#'   \item{breaks}{the actual cut points}
#'   \item{groups}{the number of desired groups, by default cuts are
#'   calculated as quantiles, which might not always give i groups for some
#'   distributions, see help on optim_fun below to handle these cases }
#'   \item{n_by_group}{the number of desired items by group, with the
#'   same caveat as above}
#'   \item{n_intervals}{the number of desired intervals}
#'   \item{width}{the interval width, which will be centered on 0}
#'   \item{width_0}{same but intervals will start on 0}
#'   \item{width_min}{same but intervals will start on min value}
#'   \item{width_max}{same but intervals will end on max value}
#' }
#'
#' @section optim_fun:
#'
#' only for what = groups. optim_fun will be applied on all possible
#' combinations. It will be fed the size of bins as its first argument and the
#' cuts as its second. From the results the combination that gives the minimum
#' result will be kept.
#'
#' Alternatively the parameter can be any of the following strings:
#'
#' \describe{
#'   \item{balanced}{Returns a combination with the minimal group size variance}
#'   \item{biggest_small_bin}{Returns a combination that has the biggest smallest
#'   bin, to avoid narrow intervals}
#'   \item{smallest_big_bin}{Returns a combination that has the biggest smallest
#'   bin, to avoid wide intervals}
#' }
#'
#' In practice the results should be quite similar and balanced should be enough
#' most of the time
#'
#' @section format_fun:
#'
#' The original base::cut uses formatC in its code to format the labels while
#' the commonly used Hmisc::cut2 uses format. cut3 allows one to choose and to
#' pass additional parameters to ... .
#'
#' Any formating function can be used as long as it takes as a first argument a
#' vector of characters and returns one.
#'
#' The function format_metric including in cutr permits additional formatting
#' especially well suited for cut3
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
  if (is.factor(x) && what == "breaks" && (is.character(i) || is.factor(i))) i <- match(as.character(i),levels(x))

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

  # coerce to appropriate class (ordered factor by default)
  if (output == "character") {
    bins <- as.character(bins)
  } else if (output == "factor") {
    bins <- factor(bins,ordered = FALSE)
  }

  bins
}
