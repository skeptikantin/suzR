#' Determine pearson residuals
#'
#' @description
#' Function determines pearson residuals from a chi-square test
#'
#' @export
#' @importFrom stats chisq.test

#' @param x Dataframe of tabulated counts (usual input to chisq.test())
#' @param decimals Number of decimals in output
#' @returns Table of pearson residuals
chisq.res <- function(x, decimals = 3) {

  tested.df <- chisq.test(x)
  pearson.res <- as.data.frame(tested.df$residuals)
  names(pearson.res) <- c(
    paste(names(pearson.res[1]), "res", sep="."),
    paste(names(pearson.res[2]), "res", sep="."))

  print(round(pearson.res, decimals))
  print(tested.df)

}

