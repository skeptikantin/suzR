#' Determine ams for collex() in batch
#'
#' @description
#' Function determines multiple association measures at once.
#' Experimental for now.
#'
#' @export
#' @import collostructions
#' @import dplyr
#' @param x An input dataframe like for normal collex().
#' @param corpsize Size of the corpus (as per normal collex()).
#' @param ams A vector with association measures for the batch. If unspecified, returns logl, odds, mi.
#' @param all_ams Boolean. If `TRUE`, calculates all available ams.
#' @param str.dir Should directional am be determined? Defaults to `TRUE`
#' @param decimals Default decimals for the output (as per collex()).
#' @param cxn.freq For truncated data sets, full cxn frequency.
#' @param delta.p Should deltaP be determined? Defaults to `TRUE`
#' @param tidy Should the output have lower-case ('tidy') col names?
#' @returns A data frame with the usual collex() output. Sorted alphabetically (?)
#' @examples
#' \dontrun{
#' collex_batch(x)
#'}
collex_batch <- function(x, corpsize = 1e+08L, ams = NULL, all_ams = FALSE, str.dir = TRUE,
                         decimals = 5, cxn.freq = NULL, delta.p = TRUE, tidy = TRUE) {

  # visibly bind global variables:
  SIGNIF = COLLEX = CORP.FREQ = ASSOC = OBS = EXP = DP1 = DP2 = NULL
  corp.freq = assoc = obs = dp1 = dp2 = NULL

  # if some dplyr df that wouldn't pass the current data class test:
  x <- as.data.frame(x)

  # determine which ams are to be calculated
  if (all_ams) {
    ams <- c("logl", "chisq", "cramersV", "dice",
             "fye", "fye.ln", "gmean", "jaccard", "liddell", "mi",
             "ms", "mi3", "odds", "pois", "t", "z", "z.cor", "random")
  } else if (is.null(ams)) {
    ams <- c("logl", "odds", "mi")
  }

  # first define an empty dataframe where the results are written to:
  #res <- data.frame()
  # from the first iteration, keep all cols:
  for (i in 1:length(ams)) {
    # calculate sca
    sca <- collex(x, corpsize = corpsize, am = ams[i], str.dir = str.dir, delta.p = delta.p)
    # rename sca columns
    names(sca)[names(sca) == 'STR.DIR'] <- ams[i]
    # remove the coll.str column to avoid problems with successive iterations:
    sca <- sca[, -grep("COLL.STR", colnames(sca))]
    # remove signif column:
    sca <- subset(sca, select = -SIGNIF)

    # if first iteration, copy sca to res
    if (i == 1) {
      res <- subset(sca, select = c(COLLEX, CORP.FREQ, ASSOC, OBS, EXP, DP1, DP2, grep(ams[i], colnames(sca))))
    } else {
      # first only select relevant cols to avoid duplicates in merging:
      sca <- subset(sca, select = -c(CORP.FREQ, OBS, EXP, ASSOC, DP1, DP2))
      # now merge with res:
      res <- merge(res, sca, by = "COLLEX")
    }
  }

  # sort data frame by frequency of occurrence in construction:
  res <- res[order(res$OBS, decreasing = TRUE),]
  rownames(res) <- NULL

  if (tidy) {
    names(res) <- tolower(names(res))
    res <- dplyr::select(res, collex, corp.freq, assoc, obs, exp, dp1, dp2, everything())
  }
  res
}
