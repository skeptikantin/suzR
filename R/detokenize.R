#' Add together two numbers
#'
#' @export
#' @import tidyverse
#' @param x A dataframe with two columns: cpos and context with the sentences to be detokenized
#' @param context A the name of the column with the sentences to be tokenized.
#' @returns A data frame with detokenized text.
#' @examples
#' \dontrun{
#' detokenize(x)
#'}
detokenize <- function(x, context = context) {
  x |>
    mutate(
      # remove double quotations
      context = str_replace_all(context, '"', " "),
      # remove single quation marks with space in between
      context = str_replace_all(context, " ' *' ", " "),
      # clitics
      context = str_replace_all(context, " ('s|'re|'ve|'m|'d|n't|'ll)", "\\1"),
      # contractions (ignore case with (?i))
      context = str_replace_all(context, "(?i)(gon|wan|can) (na|not)", "\\1\\2"),
      # special case: don't and do not (above would yield donot):
      context = str_replace_all(context, "(?i)(do) (n't)", "\\1\\2"),
      # punctuation
      context = str_replace_all(context, " ([,.;:!?])", "\\1"),
      # remove space after opening brackets
      context = str_replace_all(context, "(\\() *", "\\1"),
      # remove space before closing brackets
      context = str_replace_all(context, " *(\\))", "\\1"),
      # fix double hyphens
      context = str_replace_all(context, " -- ", " - "),
      # erroneous tokenization for plural possessive marker:
      context = str_replace_all(context, "s ' ", "s' "),
      # remove single quotation marks after the contractions etc. have been fixed:
      context = str_replace_all(context, " ' ", " "),

      # final: remove double+ spaces:
      context = str_replace_all(context, "  +", " ")
    )
}
