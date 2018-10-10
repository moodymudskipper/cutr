set_mappers <- function(..., only_formulas = FALSE){
  funs     <- list(...)
  funs_chr <- as.character(match.call())[-1]
  keep_lgl <- sapply(funs,Negate(is.null))
  funs     <- funs[keep_lgl]
  funs_chr <- funs_chr[keep_lgl]
  # type_checker check if there's a non formula / non function input
  type_checker <-
    if (only_formulas)
      function(x) inherits(x, "formula") else
        Negate(is.function)

  # use it on all functions and stop if we found some AND purr is not installed
  mappable_lgl <- sapply(funs, type_checker)
  if (any(mappable_lgl)) {
    if (!requireNamespace("purrr")) stop(
      "Package 'purrr' is required for this function to work with ",
      if (only_formulas) "formula inputs." else "non function inputs.")

    funs     <- funs[mappable_lgl]
    funs_chr <- funs_chr[mappable_lgl]

    # purrr is installed, so we can use as_mapper
    env <- parent.frame()
    for (i in seq_along(funs)) {
     assign(funs_chr[i], purrr::as_mapper(funs[[i]]),envir = parent.frame())
      }
  }
}
