#' Determine relative entropy (hrel)
#'
#' @description
#' Function determines relative entropy (hrel) over a numeric vector
#'
#' @export
#' @param x Numeric vector.
#' @returns Numeric vector of length 1 with hrel of the input vector.
#' @examples
#' \dontrun{
#' hrel(c(0.1, 0.1, 0.1, 0.3, 0.4, 0.8))
#'}

hrel <- function(x) {

  logw0 <- function(x) { ifelse (x == 0, 0, log(x)) }

  #if (!is(x, "vector"))
  #  stop("Argument not a vector.")

  #if (!is(x, "numeric"))
  #  stop("Vector not numeric.")

  perc <- x/sum(x)
  hrel <- -sum(perc*logw0(perc))/logw0(length(perc))
  hrel

}
