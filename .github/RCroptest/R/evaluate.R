#' Show Evaluate model
#'
#' @param filename path to the Evaluate output file from DSSAT-CSM to be parsed.
#'
#' @return A data.frame with the parsed data from evaluate output file.
#' @export
#'
evaluateModel <- function(filename) {
  lapply(outputModelEvaluate(filename)$data,'[[','data')
}

#' Show Statistics from comparison between Evaluate outputs.
#'
#' @param fileA path to the summary file from Model A.
#' @param fileB path to the summary file from Model B.
#'
#' @return a list  with Crop code and data.frame with results.
#'
#' @export
#'
#' @examples
#'
#' #'  evaluateStatistics('C:/DSSAT47/CTWork/workA/Evaluate.OUT',
#' #'  'C:/DSSAT47/CTWork/workB/Evaluate.OUT')
#'
evaluateStatistics <- function(fileA, fileB) {
  lapply(overallEvaluateSummary(fileA, fileB)$data, '[[', 'data')
}
