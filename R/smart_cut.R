#' Cut a Numeric Variable into Intervals
#'
#' Enhanced cut that supports among other things factor inputs, optimal grouping,
#' and flexible formatting.
#'
#' @param x numeric vector to classify into intervals
#' @param i numeric, character, or list, main parameter depending on `what`
#' @param what character, choices can be abreviated
#' @param labels character of the same length as the resulting bins or function
#' (or formula) to apply on the relevant bin's values.
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
#'   \item{width}{the interval width, which will be centered on 0 by default or
#'   at a different value (see dedicated section)}
#'   \item{cluster}{the number of clusters}
#' }
#'
#' If `what` is `"group"` or `"width"` i can be a list in which the second
#' element is a' function, see following sections.
#'
#' @section optimize groups:
#'
#' If `what = "groups"` then `i` can be a list in which the second element is a
#' function that will be applied on all possible combinations.
#' It will be fed the size of bins as its first argument and the
#' cuts as its second. From the results the combination that gives the minimum
#' result will be kept.
#'
#' Alternatively the parameter can be any of the following strings:
#'
#' \describe{
#'   \item{"balanced"}{Returns a combination with the minimal group size variance}
#'   \item{"biggest_small_bin"}{Returns a combination that has the biggest smallest
#'   bin, to avoid narrow intervals}
#'   \item{"smallest_big_bin"}{Returns a combination that has the biggest smallest
#'   bin, to avoid wide intervals}
#' }
#'
#' In practice the results should be quite similar and balanced should be enough
#' most of the time, for continuous data of a decent size without singular points,
#' optimization of groups is not necessary and will be ressource expensive.
#'
#' @section custom left boundary for `what = "width"`:
#'
#' If `what = "width"` then `i` can be either single numeric value setting the
#' width of the interval or a list in which the second element is a
#' function that will be applied on x (as a first parameter) and the cut points
#' (as a second parameter). The output of this function will determine where the
#' leftmost interval starts. Formula notation is supported.
#'
#' Alternatively the parameter can be a numeric value or any of the following
#' strings:
#'
#' \describe{
#'   \item{"left"}{First interval starts at the data point}
#'   \item{"right"}{Last interval stops at the last data point}
#'   \item{"centered"}{Margins are balanced on both sides}
#'   \item{"centered0"}{Interval containing zero is centered on zero}
#' }
#'
#' @section cluster:
#'
#' Uses function `stats::kmean` to
#' cluster `x` into `i` groups
#'
#' @section format_fun:
#'
#' The original base::cut uses formatC in its code to format the labels while
#' the commonly used Hmisc::cut2 uses format. smart_cut allows one to choose and to
#' pass additional parameters to ... .
#'
#' Any formating function can be used as long as it takes as a first argument a
#' vector of characters and returns one.
#'
#' The function format_metric including in cutr permits additional formatting
#' especially well suited for smart_cut
#'
#' @seealso
#' `?cut`
#' `?Hmisc::cut2`
#' `?format`
#' `?formatC`
#' `?format_metric`
#' `?kmeans
#'
#' @return a factor variable with levels of the form \code{"[a,b]"} or formatted means (character strings) unless \code{onlycuts} is \code{TRUE} in which case a numeric vector is returned
#' @export
#'
#' @examples
#' set.seed(1)
#' numbers  <- cumsum(abs(rnorm(100,100,100)))
#' numbers  <- cumsum(abs(rnorm(10,10,10)))
#' smart_cut(numbers, 10,"groups",format_fun = format_metric, digits=2)
smart_cut <- function(
  x,
  i,
  what = c("breaks", "groups", "n_by_group", "n_intervals", "width", "cluster"),
  labels     = NULL,
  closed     = c("left", "right"),
  expand     = TRUE,
  crop       = FALSE,
  simplify   = TRUE,
  squeeze    = FALSE,
  open_end   = FALSE,
  brackets   = c("(", "[", ")", "]"),
  sep        = ",",
  output     = c("ordered", "factor", "character"),
  format_fun = formatC, ...){

  # checks
  what   <- match.arg(what)
  closed <- match.arg(closed)
  output <- match.arg(output)
  if(is.null(brackets)) brackets <- rep("",4)

  # extract relevant functions from i arg
  if (what == "groups" && length(i) > 1) {
    optim_fun <- i[[2]]
    i <- i[[1]]
  } else optim_fun <- NULL

  if (what == "width" && length(i) > 1) {
    width_fun <- i[[2]]

    i <- i[[1]]
  } else width_fun <- NULL
  i >= 1 || what == "breaks" || stop("i must be positive")

  # set mappers (handle formula notation if relevant)
  set_mappers(labels, optim_fun, format_fun, width_fun, only_formulas = TRUE)

  # handle factors
  if (is.factor(x) && what == "breaks" && (is.character(i) || is.factor(i))) i <- match(as.character(i),levels(x))

  # convert "n_by_group" case into a "groups" case
  if (what == "n_by_group") {
    i <- max(1, floor(sum(!is.na(x))/i))
    what <- "groups"}

  # get breaks
  cuts <- get_cuts(x = as.numeric(x), i = i, what = what, expand = expand,
                   crop = crop, closed = closed, open_end = open_end,
                   optim_fun = optim_fun, width_fun = width_fun)
  # after the cropping is done, ends are closed by definition
  if (crop || expand) open_end <- FALSE

  if (length(cuts) == 1 && !expand)
    stop("Can't cut data if only one break is provided and `expand` is FALSE")


  # get raw output
  bins <- cut_explicit(x = x, cuts = cuts , labels = labels, simplify = simplify,
                       closed = closed, squeeze = squeeze, open_end = open_end,
                       brackets = brackets, sep = sep,
                       format_fun = format_fun, ...)

  # coerce to appropriate class (ordered factor by default)
  if (output == "character") {
    bins <- as.character(bins)
  } else if (output == "factor") {
    bins <- factor(bins,ordered = FALSE)
  }

  bins
}