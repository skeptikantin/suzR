# Script to read in csv results of the new farm
# https://doc.pcibex.net/how-to-guides/data-transformation/
#' Read results file from PCIbex Farm
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
#' @param all_ams Boolean. If `TRUE`, calculates all available ams. Default `FALSE`.
#' @param all_combos Boolean. If `TRUE`, calculate all possible collex combinations. Default `FALSE`.
#' @param str.dir Should directional am be determined? Defaults to `TRUE`
#' @param delta.p Should deltaP be determined? Defaults to `TRUE`
#' @param decimals Default decimals for the output (as per collex()).
#' @param raw Default `FALSE`, whether input contains raw observations or aggregated counts.
#' @param tidy Should the output have lower-case ('tidy') col names? Requires dplyr.
#' @returns A data frame with the usual collex.dist() output. Sorted by first association measure.
#' @examples
#' \dontrun{
#' collex.dist_batch(x)
#'}

read_pcibex_csv <- function(filepath, auto.colnames=TRUE, fun.col=function(col,cols){cols[cols==col]<-paste(col,"Ibex",sep=".");return(cols)}) {
  n.cols <- max(count.fields(filepath,sep=",",quote=NULL),na.rm=TRUE)
  if (auto.colnames){
    cols <- c()
    con <- file(filepath, "r")
    while ( TRUE ) {
      line <- readLines(con, n = 1, warn=FALSE)
      if ( length(line) == 0) {
        break
      }
      m <- regmatches(line,regexec("^# (\\d+)\\. (.+)\\.$",line))[[1]]
      if (length(m) == 3) {
        index <- as.numeric(m[2])
        value <- m[3]
        if (is.function(fun.col)){
          cols <- fun.col(value,cols)
        }
        cols[index] <- value
        if (index == n.cols){
          break
        }
      }
    }
    close(con)
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=cols))
  }
  else{
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=seq(1:n.cols)))
  }
}
