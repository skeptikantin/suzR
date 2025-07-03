#' Create files to determine surprisal of A-maze alternatives
#'
#' @description
#' Runs over a verticalized df with target and distractor stimuli.
#' For each alternative, prints a file with a sentence of target stimuli
#' and the distractor as the last word.
#' Filename will be sentence id and position of the distractor.
#'
#' @export
#' @import tidyverse
#' @param x the df this function runs over.
#' @param targets The column with the target stimuli.
#' @param distractors The column with the distractor stimuli.
#' @param position Surp files are created from Nth position.
#' @param grouping The column with the grouping variable (usually sentence ID)
#' @param file File path that the files should be written to (if NULL, files are written to the pwd)
#' @param fileinfo Additional info to be written to filename (currently not used).
#' @param write_to_file Should output be written to file?
#' @param print_to_console Should output be written to the console? (Useful for testing).
#' @returns Writes separate files for each distractor (from #2).
#' @examples
#' \dontrun{
#' #'
#' stims <- tribble(
#'   ~Id, ~Target, ~Alternative,
#'   1, "The", "---",
#'   1, "comment", "watched",
#'   1, "about", "china",
#'   1, "his", "cent",
#'   1, "brother", "imagine",
#'   1, "made", "push",
#'   1, "him", "jack",
#'   1, "feel", "inch",
#'   1, "uncomfortable.", "qualification.",
#'   2, "They", "---",
#'   2, "forced", "senate",
#'   2, "him", "belt",
#'   2, "into", "anti",
#'   2, "going,", "occur,",
#'   2, "so", "hour",
#'   2, "he", "jack",
#'   2, "went.", "lake."
#' )
#' amaze_create_surpfiles(stims, targets = "Target", distractors = "Alternative", print_to_console = TRUE)
#'}
amaze_create_surpfiles <- function(x, targets = "targets", distractors = "distractors",
                                   position = 4, grouping = "Id", file = NULL, fileinfo = NULL,
                                   write_to_file = TRUE, print_to_console = FALSE) {
  # split at grouping variable
  x_ls <- split(x, x[grouping])

  # we only need to determine surprisals for stimuli after word 3
  for (i in 1:length(x_ls)) {

    # fetch sentence Id
    sid <- unique(x_ls[[1]][grouping]) %>% getElement(grouping)

    # from word 4 onwards, print sentences with the alternative:
    for (j in position:nrow(x_ls[[i]])-1) {

      dstrctrs <- unlist(x_ls[[i]][distractors][j+1,])
      trgts <- paste(unlist(x_ls[[i]][targets][1:j,]),
                     collapse = " ")
      sntnc <- paste(trgts, dstrctrs)

      # print sentence to file:
      # if file path is set, print to folder. If not, print to pwd
      if (is.null(file)) {
        file = ""
      }

      if (print_to_console) {
        print(sntnc)
      }

      if (write_to_file) {
        cat("!ARTICLE", file = paste0(file, sid, "_", j, ".txt"), sep="\n")
        cat(sntnc, file = paste0(file, sid, "_", j, ".txt"), append = TRUE)
      }

    }

  }

}
