#' Process cwb context sentences
#'
#' @description
#' Function to split cwb context
#'
#' @export
#' @import tidyverse
#' @param x A dataframe with two columns: cpos and context with the sentences to be detokenized
#' @param cols The name of the column with the concordance (usually "conc")
#' @param delim The delimiter at which the sentences shall be split.
#' @param col_names A vector with the column names (default works for 3 context sentences)
#' @param no_of_contexts An integer with the number of LEFT context sentences
#' @filter_incomplete Should sentences with "@ @ @" be excluded?
#' @debug separate_wider_delim() tends to break with messy data, so a debugging round to identify the issue might be needed (in which case filter_incomplete should be set to FALSE)
#' @returns A data frame with detokenized text.
#' @examples
#' \dontrun{
#' process_cwb_context(x)
#'}
cwb_context <- function(x, cols = conc, delim = "</s>",
                        #col_names = c("context3", "context2", "context1", "context", "end"),
                        no_of_contexts = 3,
                        filter_incomplete = FALSE, debug = FALSE) {

  # create column names for after the split:
  col_names <- c(paste(rep('context', no_of_contexts:no_of_contexts-1), no_of_contexts:1, sep = ""), "context", "end")


  # Difficult to control the cwb output, so if the separation at </s> fails, first run a debug:
  if (debug) {

    res <- x |>
      separate_wider_delim(cols = conc, delim = delim, names = col_names, too_few = "debug", too_many = 'debug')

  } else {

    res <- x |>
      separate_wider_delim(cols = conc, delim = delim, names = col_names) |>
      select(-end) |>
      # remove all sentence tags:
      mutate_at(vars(starts_with("context")), (~ sub("<s>", "", .))) |>
      # remove the match tags:
      mutate(context = gsub('(\\[|\\]){3}', '', context))
  }

  if (filter_incomplete) {
    res <- res |>
      # determine if all sentences are complete:
      mutate(contexts_complete = if_else(if_any(.cols = everything(), ~ grepl("@ @ @", .)), "no", "yes")) |>
      filter(contexts_complete == "yes") |>
      select(-contexts_complete)
  } else {
    res <- res |>
      mutate(contexts_complete = if_else(if_any(.cols = everything(), ~ grepl("@ @ @", .)), "no", "yes"))
  }

  # return the df:
  res
}
