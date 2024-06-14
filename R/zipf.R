#' Determine Zipf
#'
#' @description
#' Function determines a Zipf converted value of corpus frequency
#'
#' @export
#' @param x Numeric vector.
#' @param corpsize Size of the corpus where the frequency is from.
#' Default is number of tokens in COCA excl. punctuation
#' @param rounding Desired decimal rounding
#' @returns Numeric vector of length 1 with hrel of the input vector.
#' @examples
#' \dontrun{
#'
#'}
zipf <- function(x, corpsize = 471255965, rounding = 1) {

  x <- log(normalizer(x, 471255965, 1000000000), 10)
  x <- round(x, rounding)
  x

}
