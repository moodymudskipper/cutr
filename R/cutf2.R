#' Cut a Numeric Variable into Intervals
#'
#' This is a copy of Frank Harell's `Hmisc::cut2` function which uses the
#' additional parameters `format_fun` and `...` to apply custom formatting to
#' the input. By default it behaves exactly like `Hmisc::cut2`
#'
#' @param x numeric vector to classify into intervals
#' @param cuts cut points
#' @param m desired minimum number of observations in a group. The algorithm
#' does not guarantee that all groups will have at least m observations.
#' @param g number of quantile groups
#' @param levels.mean set to TRUE to make the new categorical vector have levels
#'  attribute that is the group means of x instead of interval endpoint labels
#' @param digits number of significant digits to use in constructing levels.
#' Default is 3 (5 if `levels.mean=TRUE`)
#' @param minmax if cuts is specified but `min(x)<min(cuts)` or
#' `max(x)>max(cuts)`, augments cuts to include min and max x
#' @param oneval if an interval contains only one unique value, the interval
#' will be labeled with the formatted version of that value instead of the
#' interval endpoints, `unless oneval=FALSE`
#' @param onlycuts set to `TRUE` to only return the vector of computed cuts. This consists of the interior values plus outer ranges.
#' @param format_fun
#' @param ...
#'
#' @return a factor variable with levels of the form `[a,b)` or formatted means
#' (character strings) unless `onlycuts` is `TRUE` in which case a numeric
#' vector is returned
#' @export
#'
#' @examples
#' set.seed(1)
#' Z <- 1000*stats::rnorm(10000)
#' table(cutf2(Z, c(-10005, -5000, 100, 0, 50, 10000)))
#' table(cutf2(Z, c(-10005, -5000, 100, 0, 50, 10000), format_fun = formatC))
#' table(cutf2(Z, c(-10005, -5000, 100, 0, 50, 10000), format_fun = signif2)) # investigate NaN issue!
#' table(cutf2(Z, c(-10005, -5000, 100, 0, 50, 10000), format_fun = format_metric))
#' table(cutf2(Z, g=10))      # quantile groups
#' table(cutf2(Z, g=10, format_fun = formatC))
#' table(cutf2(Z, g=10, format_fun = signif2))
#' table(cutf2(Z, g=10, format_fun = format_metric))
#' table(cutf2(Z, m=500))      # group x into intevals with at least 50 obs.
#' table(cutf2(Z, m=500, format_fun = formatC))
#' table(cutf2(Z, m=500, format_fun = signif2))
#' table(cutf2(Z, m=500, format_fun = format_metric))
cutf2 <- function (x, cuts, m = 150, g, levels.mean = FALSE, digits, minmax = TRUE,
                   oneval = TRUE, onlycuts = FALSE, format_fun = format, ...)
{
  method <- 1
  x.unique <- sort(unique(c(x[!is.na(x)], if (!missing(cuts)) cuts)))
  min.dif <- min(diff(x.unique))/2
  min.dif.factor <- 1
  if (missing(digits))
    digits <- if (levels.mean)
      5
  else 3
  oldopt <- options("digits")
  options(digits = digits)
  on.exit(options(oldopt))
  xlab <- attr(x, "label")
  if (missing(cuts)) {
    nnm <- sum(!is.na(x))
    if (missing(g))
      g <- max(1, floor(nnm/m))
    if (g < 1)
      stop("g must be >=1, m must be positive")
    options(digits = 15)
    n <- table(x)
    xx <- as.double(names(n))
    options(digits = digits)
    cum <- cumsum(n)
    m <- length(xx)
    y <- as.integer(ifelse(is.na(x), NA, 1))
    labs <- character(g)
    cuts <- approx(cum, xx, xout = (1:g) * nnm/g, method = "constant",
                   rule = 2, f = 1)$y
    cuts[length(cuts)] <- max(xx)
    lower <- xx[1]
    upper <- 1e+45
    up <- low <- double(g)
    i <- 0
    for (j in 1:g) {
      cj <- if (method == 1 || j == 1)
        cuts[j]
      else {
        if (i == 0)
          stop("program logic error")
        s <- if (is.na(lower))
          FALSE
        else xx >= lower
        cum.used <- if (all(s))
          0
        else max(cum[!s])
        if (j == m)
          max(xx)
        else if (sum(s) < 2)
          max(xx)
        else approx(cum[s] - cum.used, xx[s], xout = (nnm -
                                                        cum.used)/(g - j + 1), method = "constant",
                    rule = 2, f = 1)$y
      }
      if (cj == upper)
        next
      i <- i + 1
      upper <- cj
      y[x >= (lower - min.dif.factor * min.dif)] <- i
      low[i] <- lower
      lower <- if (j == g)
        upper
      else min(xx[xx > upper])
      if (is.na(lower))
        lower <- upper
      up[i] <- lower
    }
    low <- low[1:i]
    up <- up[1:i]
    variation <- logical(i)
    for (ii in 1:i) {
      r <- range(x[y == ii], na.rm = TRUE)
      variation[ii] <- diff(r) > 0
    }
    if (onlycuts)
      return(unique(c(low, max(xx))))
    flow <- format_fun(low,...)
    fup <- format_fun(up,...)
    bb <- c(rep(")", i - 1), "]")
    labs <- ifelse(low == up | (oneval & !variation), flow,
                   paste("[", flow, ",", fup, bb, sep = ""))
    ss <- y == 0 & !is.na(y)
    if (any(ss))
      stop(paste("categorization error in cut2.  Values of x not appearing in any interval:\n",
                 paste(format(x[ss], digits = 12), collapse = " "),
                 "\nLower endpoints:", paste(format(low, digits = 12),
                                             collapse = " "), "\nUpper endpoints:", paste(format(up,
                                                                                                 digits = 12), collapse = " ")))
    y <- structure(y, class = "factor", levels = labs)
  }
  else {
    if (minmax) {
      r <- range(x, na.rm = TRUE)
      if (r[1] < cuts[1])
        cuts <- c(r[1], cuts)
      if (r[2] > max(cuts))
        cuts <- c(cuts, r[2])
    }
    l <- length(cuts)
    k2 <- cuts - min.dif
    k2[l] <- cuts[l]
    y <- cut(x, k2)
    if (!levels.mean) {
      brack <- rep(")", l - 1)
      brack[l - 1] <- "]"
      fmt <- format_fun(cuts,...)
      labs <- paste("[", fmt[1:(l - 1)], ",", fmt[2:l],
                    brack, sep = "")
      if (oneval) {
        nu <- table(cut(x.unique, k2))
        if (length(nu) != length(levels(y)))
          stop("program logic error")
        levels(y) <- ifelse(nu == 1, c(fmt[1:(l - 2)],
                                       fmt[l]), labs)
      }
      else levels(y) <- labs
    }
  }
  if (levels.mean) {
    means <- tapply(x, y, function(w) mean(w, na.rm = TRUE))
    levels(y) <- format_fun(means, ...)
  }
  attr(y, "class") <- "factor"
  if (length(xlab))
    label(y) <- xlab
  y
}
