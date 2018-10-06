get_optimal_cutpoints <- function(x, n, f, closed = c("left","right")){
  closed <- match.arg(closed)
  # character inputs are mapped to unexported functions of the same name
  if (is.character(f)) f <- getFromNamespace(f,"cutr")
  else if (inherits(f,"formula")) {
    if (requireNamespace("purrr")) f <- purrr::as_mapper(f) else
      stop("Make sure package 'purrr' is installed to use formula notation")
  }

  # candidate breaks, depends on which side of the interval we close
  br <- sort(unique(x))
  xmax <- max(br)
  xmin <- min(br)
  br <- if (closed == "right") br[-length(br)] else br[-1]

  # bin with all combinations of cutpoints
  # need trick with Inf to be able to have univalue bins on both sides
  n_poss <- prod(length(br):(length(br) - n + 2))/factorial(n - 1)
  if (n_poss > 1e6) stop(n_poss, " possibilities would have to be tested, limit is set to 1e6")
  combn_     <- combn(br, n - 1, function(brks){
    if (closed == "right") brks <- c(-Inf, brks, xmax)
    if (closed == "left") brks  <- c(xmin, brks, Inf)
    bins <- .bincode(x,brks,right = closed == "right")
    list(bin_sizes = tabulate(bins), breaks = brks)
    }, simplify = FALSE)
  #browser()

  # extract variables to loop on
  sizes   <- lapply(combn_,`[[`,1)
  breaks  <- lapply(combn_,`[[`,2)

  # apply f on all combinations
  chosen <- breaks[[which.min(mapply(f,sizes,breaks))]]

  # "repair" sides where infinity is not needed
  if (chosen[2] != xmin) chosen[1] <- xmin
  if (chosen[length(chosen - 1)] != xmax) chosen[length(chosen)] <- xmax
  chosen
}

biggest_small_bin <- function(sizes,cuts){-min(sizes)}
smallest_big_bin  <- function(sizes,cuts){ max(sizes)}
balanced          <- function(sizes,cuts){ sum((sizes - mean(sizes))^2)}
