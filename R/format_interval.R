
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
    left    <- c(if (open_end) `.(` else `.[`,
                 rep(`.(`, l - 2))
  }
  # handle factors, or format numerics
  if (!is.null(names(cuts))) cuts_chr <- names(cuts) else
    cuts_chr <- format_fun(cuts, ...)
  labels   <- paste(left, cuts_chr[-l], sep, cuts_chr[-1], right, sep = "")
  labels
}



