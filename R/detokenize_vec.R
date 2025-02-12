#' Detokenize corpus output
#'
#' @description
#' Output from corpus files in CQP mode are often already tokenized. This function
#' untokenizes the most basic tokenization issues (and removes some punctuation)
#' as a quick fix for now. Experimental. --> this function is to run over
#' every column of a dataframe
#'
#' @export
#' @import tidyverse
#' @param x A dataframe with one cpos and n context columns
#' @param context A vector with the names of the cols of sentences to be detokenized.
#' @returns A data frame with detokenized text.
#' @examples
#' \dontrun{
#' detokenize_vec(x)
#'}
detokenize_vec <- function(x, context = context) {

  cont_cols = context

  x |>
    mutate(

      # if run over raw CWB output, remove the brackets
      across(cont_cols, ~str_replace_all(., " (\\[{3}|\\]{3}) ", " ")),
      across(cont_cols, ~str_replace_all(., "^(\\[{3}) ", "")),
      across(cont_cols, ~str_replace_all(., " (\\]{3})$", "")),

      ## remove boilerplate tokens:
      # TOOLONG:
      across(cont_cols, ~str_replace_all(., "\\(?\\*{2}\\S+TOOLONG\\)?", " ")),

      # remove double quotations
      across(cont_cols, ~str_replace_all(., '"', " ")),

      # remove single quation marks with space in between
      across(cont_cols, ~str_replace_all(., " ' *' ", " ")),

      # clitics
      across(cont_cols, ~str_replace_all(., " ('s|'re|'ve|'m|'d|n't|'ll)", "\\1")),

      # contractions (ignore case with (?i))
      across(cont_cols, ~str_replace_all(., "(?i)(gon|wan|can) (na|not)", "\\1\\2")),

      # special case: don't and do not (above would yield donot):
      across(cont_cols, ~str_replace_all(., "(?i)(do) (n't)", "\\1\\2")),

      # punctuation
      across(cont_cols, ~str_replace_all(., " ([,.;:!?])", "\\1")),

      # remove space before punctuation
      across(cont_cols, ~str_replace_all(., " ([,.;:!?])", "\\1")),

      # remove space before closing brackets
      across(cont_cols, ~str_replace_all(., " *(\\))", "\\1")),

      # remove space after opening brackets
      across(cont_cols, ~str_replace_all(., "(\\() ", "\\1")),

      # fix double hyphens
      across(cont_cols, ~str_replace_all(., " *-- ", " - ")),

      # erroneous tokenization for plural possessive marker:
      across(cont_cols, ~str_replace_all(., "s ' ", "s' ")),

      # remove single quotation marks after the contractions etc. have been fixed:
      across(cont_cols, ~str_replace_all(., " ' ", " ")),

      # remove the 'off-screen' tokens (-> '@!...:'"):
      across(cont_cols, ~str_replace_all(., "@!?\\S+? ", "")),

      # remove bracketed stuff at the beginning: TODO
      across(cont_cols, ~str_replace_all(., "^\\(\\S+?\\) *", "")),

      # remove numbers at the beginning
      across(cont_cols, ~str_replace_all(., "^ *\\d+ ", "")),

      # remove hashes:
      across(cont_cols, ~str_replace_all(., " *[#//+]+ ", " ")),

      # remove spaces after dollar sings etc.:
      across(cont_cols, ~str_replace_all(., "$ ", "$")),

      # remove sequences of trailing hyphens & co:
      across(cont_cols, ~str_replace_all(., "^ *[-,;:'\". *)+&/]+ *", "")),

      # final: remove double+ spaces:
      across(cont_cols, ~str_replace_all(., "  +", " ")),

      # remove leading and trailing whitespace:
      across(cont_cols, ~str_replace_all(., "(^ *| *$)", ""))

    )
}

#detokenize_vec(raw_clean, context = cont_cols)

