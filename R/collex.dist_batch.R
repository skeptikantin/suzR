#' Determine ams for collex.dist() in batch
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

collex.dist_batch <- function(x, ams = NULL, all_ams = FALSE, str.dir = TRUE, delta.p = TRUE,
                              decimals = 5, raw = FALSE, tidy = TRUE) {

  # visibly bind global variables:
  COLLEX = CORP.FREQ = ASSOC = O.CXN1 = E.CXN1 = O.CXN2 = E.CXN2 = SHARED = SIGNIF = DP1 = DP2 = NULL
  assoc = o.cxn1 = e.cxn1 = o.cxn2 = e.cxn2 = dp1 = dp2 = NULL

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
    dca <- collex.dist(x, am = ams[i], str.dir = str.dir, delta.p = delta.p)
    # rename sca columns
    names(dca)[names(dca) == 'STR.DIR'] <- ams[i]
    # remove the coll.str column to avoid problems with successive iterations:
    dca <- dca[, -grep("COLL.STR", colnames(dca))]
    # remove signif column:
    dca <- subset(dca, select = -c(SIGNIF, SHARED))

    # if first iteration, copy sca to res
    if (i == 1) {
      res <- subset(dca, select = c(COLLEX, ASSOC, O.CXN1, E.CXN1, O.CXN2, E.CXN2,
                                    DP1, DP2, grep(ams[i], colnames(dca))))
    } else {
      # first only select relevant cols to avoid duplicates in merging:
      dca <- subset(dca, select = -c(O.CXN1, E.CXN1, O.CXN2, E.CXN2, ASSOC, DP1, DP2))
      # now merge with res:
      res <- merge(res, dca, by = "COLLEX")
    }
  }

  # sort data frame by first association measure provided (usually logl):
  index <-which(grepl(ams[1], colnames(res)))
  res <- res[order(res[,index], decreasing = TRUE),] # can be used to incorporate the REVERSE arg
  rownames(res) <- NULL

  if (tidy) {
    names(res) <- tolower(names(res))
    res <- dplyr::select(res, collex, assoc, o.cxn1, e.cxn1, o.cxn2, e.cxn2, dp1, dp2, everything())
  }
  res
}
