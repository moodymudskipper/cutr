#' cutr: Enhanced cut And Useful Related Functions
#'
#' Expands formatting options of `base::cut.default` and `Hmisc::cut2`,
#' supports factors, adhoc formating, group optimization and much more.
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom Hmisc label<-
#' @importFrom stats approx
#' @importFrom stats kmeans
#' @importFrom stats quantile
#' @importFrom stats setNames
#' @importFrom utils combn
#' @importFrom utils getFromNamespace
#' @importFrom utils head
## usethis namespace: end
NULL

globalVariables(c(
  "prefix",
  "n"
))
