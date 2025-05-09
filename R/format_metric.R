#' format a numeric vector using metric system
#'
#' @param x a numeric vector
#' @param prefixes a vector or prefixes ('','k', 'M', ...), lower case symbols ('','kilo', 'mega' ...) or powers of tens (0, 3, 6 ...).
#' @param digits integer used by signif or smart_signif
#' @param smart_signif logical, if TRUE smart_signif will be used instead of signif
#' @param symbol logical, if TRUE, full symbol will be used, else prefix will be used
#' @param base_unit character, e.g. 'g'
#' @param ... additional arguments passed to formatC
#'
#' @return formated character
#' @export
#'
#' @examples
#' format_metric(c(-2500,0.15,3,5e6),c(0,3))
#' format_metric(c(-2500,0.15,3,5e6),"kilo")
#' format_metric(c(-2500,0.15,3,5e6),base_unit="\U{20AC}")
format_metric <- function(x, prefixes = c("","k","M","G"), digits = 3, smart_signif = FALSE, symbol=TRUE, base_unit="", ...){
  all_units <- data.frame(
    prefix = c("yocto", "zepto", "atto", "femto", "pico", "nano", "micro",
               "milli", "centi", "deci", "", "deca", "hecto", "kilo", "mega",
               "giga", "tera", "peta", "exa", "zetta", "yotta"),
    symbol = c("y", "z", "a", "f", "p", "n", "\U{B5}", "m", "c", "d", "", "da",
               "h", "k", "M", "G", "T", "P", "E", "Z", "Y"),
    n = c(-24, -21, -18, -15, -12, -9, -6, -3, -2, -1, 0, 1, 2, 3, 6,
          9, 12, 15, 18, 21, 24),
    stringsAsFactors = FALSE)

  lkp <- subset(all_units, prefix %in% prefixes | symbol %in% prefixes | n %in% prefixes)
  lkp$units <- if (symbol) lkp$symbol else lkp$prefix
  lkp$units <- paste0(lkp$units,base_unit)

  cuts <- as.numeric(cut(abs(x),c(-Inf,10^lkp$n[-1],Inf),right = FALSE))
  ns <- lkp$n[cuts]
  units <- lkp$units[cuts]
  out <- if (smart_signif) smart_signif(x,digits) else signif(x,digits)
  out <- ifelse(is.finite(x), trimws(paste(formatC(out / 10^ns,...),units)), as.character(x))
  out
}
