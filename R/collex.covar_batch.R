#' Determine ams for collex.covar() in batch
#'
#' @description
#' Function determines multiple association measures at once.
#' Experimental for now.
#'
#' @export
#' @import collostructions
#' @import dplyr
#' @param x An input dataframe like for normal collex().
#' @param ams A vector with association measures for the batch. If unspecified, returns logl, odds, mi.
#' @param all_ams Boolean. If `TRUE`, calculates all available ams.
#' @param all_combos Boolean. If `TRUE`, calculate all possible collex combinations. Default `FALSE`.
#' @param str.dir Should directional am be determined? Defaults to `TRUE`
#' @param delta.p Should deltaP be determined? Defaults to `TRUE`
#' @param decimals Default decimals for the output (as per collex()).
#' @param raw Default `FALSE`, whether input contains raw observations or aggregated counts.
#' @param tidy Should the output have lower-case ('tidy') col names?
#' @returns A data frame with the usual collex.dist() output. Sorted by first association measure.
#' @examples
#' \dontrun{
#' collex.dist_batch(x)
#'}

collex.covar_batch <- function(x, ams = NULL, all_ams = FALSE, all_combos = FALSE,
                               str.dir = TRUE, delta.p = TRUE,
                               decimals = 5, raw = TRUE, tidy = TRUE) {

  # visibly bind global variables:
  COLLEX = CORP.FREQ = ASSOC = O.CXN1 = E.CXN1 = O.CXN2 = E.CXN2 = SHARED = SIGNIF = DP1 = DP2 = NULL

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
    cca <- collex.covar(x, am = ams[i], raw = FALSE, str.dir = str.dir, delta.p = delta.p)
    # rename sca columns
    names(cca)[names(cca) == 'STR.DIR'] <- ams[i]
    # remove the coll.str column to avoid problems with successive iterations:
    cca <- cca[, -grep("COLL.STR", colnames(cca))]
    # remove signif column:
    cca <- subset(cca, select = -c(SIGNIF))

    # if first iteration, copy sca to res
    if (i == 1) {
      res <- subset(cca, select = c(SLOT1, SLOT2, ASSOC, fS1, fS2, OBS, EXP,
                                    DP1, DP2, grep(ams[i], colnames(cca))))
    } else {
      # first only select relevant cols to avoid duplicates in merging:
      cca <- subset(cca, select = -c(fS1, fS2, OBS, EXP, ASSOC, DP1, DP2))
      # now merge with res:
      res <- merge(res, cca, by = c("SLOT1", "SLOT2"))
    }
  }

  # sort data frame by first association measure provided (usually logl):
  index <- which(grepl(ams[1], colnames(res)))
  res <- res[order(res[,index], decreasing = TRUE),] # can be used to incorporate the REVERSE arg
  rownames(res) <- NULL

  if (tidy) {
    names(res) <- tolower(names(res))
    res <- dplyr::select(res, slot1, slot2, assoc, fs1, fs2, obs, exp, dp1, dp2, everything())
  }
  res
}
