format_interval_squeezed <- function(x, cuts, closed, open_end, brackets, sep, format_fun,...){
  ind <- length(cuts) - 1
  cut_list <-  vector("list", length = ind)

  if (is.factor(x)) {
    is_factor <- TRUE
    x_levels <- levels(x)
    x <- as.numeric(x)
  } else {is_factor <- FALSE}

  if (closed == "right") {
    if (!open_end) {
      cut_list[[1]] <-
        c(x[which(x >= cuts[1])[1]], x[max(which(x <= cuts[2]))])
      start <- 2
    } else start <- 1

    for (i in start:ind) {
      if (all(x < cuts[i] || x > cuts[i + 1]))
        cut_list[[i]] <- c(cuts[i],cuts[i + 1]) else
      cut_list[[i]] <-
        c(x[which(x > cuts[i])[1]], x[max(-Inf,which(x <= cuts[i + 1]))])
        if (anyNA(cut_list[[i]])) cut_list[[i]] <- c(cuts[i+1],cuts[i+1])
    }
  } else if (closed == "left") {
    if (!open_end) {
      # prepare case for empty intervals!
      #if (all(x < cuts[ind] || x > cuts[ind+1]))
      if (all(x < cuts[ind]))
        cut_list[[ind]] <- c(cuts[ind],cuts[ind + 1]) else
          cut_list[[ind]] <- c(x[which(x >= cuts[ind])[1]], x[max(which(x <= cuts[ind + 1]))])
        end <- ind - 1
    } else end <- ind - 2

    for (i in 1:end) {
      if (all(x < cuts[i] || x > cuts[i + 1]))
        cut_list[[i]] <- c(cuts[i],cuts[i + 1]) else
          cut_list[[i]] <- c(x[which(x >= cuts[i])[1]], x[max(-Inf,which(x < cuts[i + 1]))])
        if (anyNA(cut_list[[i]])) cut_list[[i]] <- c(cuts[i],cuts[i])
    }
  }
  `.[` <- brackets[2]
  `.]` <- brackets[4]

  if (is_factor) cuts_chr <- x_levels[unlist(cut_list)] else
    cuts_chr <- format_fun(unlist(cut_list),...)
  labels <- apply(matrix(cuts_chr,2),2,function(x) paste0(`.[`,x[1],sep,x[2],`.]`))
  labels
}



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



