#' Normalize corpus frequencyies
#'
#' @description
#' Function to normalize corpus frequencies
#'
#' @export
#' @param x Numeric vector of corpus frequencies
#' @param corpsize Size of the corpus where the frequency is from.
#' Default is number of tokens in COCA excl. punctuation
#' @param base Base of per x ('million') frequency
#' @returns Returns something (update required)
normalizer <- function(x, corpsize, base = "million") {

  if (base == "million") {
    base = 1000000
  } else if (base == "thousand") {
    base = 1000
  } else if (is.numeric(base)) {
    base = base
  } else {
    stop("Enter 'million', 'thousand' or a number you want to normalize by.")
  }

  x * (base / corpsize)

}
