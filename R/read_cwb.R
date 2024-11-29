#' Read raw CWB output
#'
#' @description
#' Reads raw CWB output. For now, it can read cpos, left, match, right.
#' Other output settings TODO.
#'
#' @export
#' @import tidyverse
#' @param path A filepath to the raw input
#' @param skip Number of lines to skip (11 default for cwb header).
#' @param kwicize If TRUE, function will split input into left, match, right (default: FALSE)
#' @returns A tibble with tab-separated left, match, right.
#' @examples
#' \dontrun{
#' detokenize(x)
#'}
read_cwb <- function(path, skip = 10, kwicize = FALSE) {

  res <- readr::read_lines(path, skip = skip) |>
    as_tibble()

  if (kwicize) {

    res <- res |>
      # recreate the traditional KWIC view:
      mutate(cpos = gsub("^ *(\\d+):.*$", "\\1", value),
             left = gsub("^ *\\d+: *(.*?) *\\[\\[\\[.*$", "\\1", value),
             match = gsub("^.*\\[\\[\\[ *(.*?) *\\]\\]\\].*$", "\\1", value),
             right = gsub("^.+\\]\\]\\] ?(.*?)$", "\\1", value))
  } else {

    res <- res |>
      mutate(cpos = gsub("^ *(\\d+):.*$", "\\1", value),
             conc = gsub("^ *\\d+: *(.+)$", "\\1", value))

  }

  # cleanup
  res <- res |>
    select(-value) |>
    mutate(cpos = as.numeric(cpos))

  # return value
  res

}
