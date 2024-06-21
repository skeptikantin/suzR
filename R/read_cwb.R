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
#' @returns A tibble with tab-separated left, match, right.
#' @examples
#' \dontrun{
#' detokenize(x)
#'}
read_cwb <- function(path, skip = 11) {
  res <- readr::read_lines(path, skip = skip) |>
    as_tibble() |>
    # recreate the traditional KWIC view:
    mutate(cpos = gsub("^ *(\\d+):.*$", "\\1", value),
           left = gsub("^ *\\d+: *(.*?) *\\[\\[\\[.*$", "\\1", value),
           match = gsub("^.*\\[\\[\\[ *(.*?) *\\]\\]\\].*$", "\\1", value),
           right = gsub("^.+\\]\\]\\] (.*?)$", "\\1", value)) |>
    select(-value) |>
    mutate(cpos = as.numeric(cpos))
  res
}
