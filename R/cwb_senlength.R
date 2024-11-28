#' Count sentence length in cwb context sentences
#'
#' @description
#' Function to split cwb context
#'
#' @export
#' @import tidyverse
#' @param x A dataframe with a cpos column and context sentences
#' @param required_length Minimum length of sentences to keep?
#' @param filter_short Should the target concs with short context sentences be removed?
#' @param remove_cols Should the ancillary columns be removed?
#' @examples
#' \dontrun{
#' count_sentence_length(x)
#'}
cwb_senlength <- function(x, required_length = 3, filter_short = TRUE, remove_cols = TRUE) {

  res <- x |>
    mutate(
      across(starts_with('context'),
             ~ str_count(., ' ') + 1,
      .names = '{col}_len'
    )
  ) |>
    mutate(
      length_req = +(if_all(ends_with('_len'), ~. > required_length))
    )

  if (filter_short){
    res <- res |>
      filter(length_req == 1)
  }

  if (remove_cols) {
    res <- res |> select(cpos, starts_with('context') & (!ends_with('_len')))
  }

  res

}
