
#' Check if models are different (Sum[Tot. Above Threshold] > 0).
#'
#' @param fileA path to the summary file from Model A.
#' @param fileB path to the summary file from Model B.
#' @param thNumeric threshold value for use with numeric calculations.
#' @param thDate threshold value for use with date calculations.
#'
#' @return Boolean value with results of comparison.
#'
#' @export
#'
#' @examples
#' #'  testCI('C:/DSSAT48/CTWork/workA/Summary.OUT',
#' #'  'C:/DSSAT48/CTWork/workB/Summary.OUT',
#' #'  thNumeric=2,
#' #'  thDate=0)
#'
testCI <- function(fileA, fileB, thNumeric, thDate) {
  o <- summaryStatistics(fileA, fileB, thNumeric, thDate)
  sum(o[which(o$Statistics == 'Total Above Threshold'),-1]) > 0
}
