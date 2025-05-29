
#' Show Summary model
#'
#' @param filename path to the Summary output file from DSSAT-CSM to be parsed.
#'
#' @return A list with version and data.frame with the parsed data from summary output file.
#' @export
#'
summaryModel <- function(filename) {
  parseSummaryFile(filename)
}

#' Compute the overall summary statistics from models comparison.
#'
#' @param fileA path to the summary file from Model A.
#' @param fileB path to the summary file from Model B.
#' @param tNUM threshold value for use with numeric calculations.
#' @param tDTA threshodl value for use with date calculations.
#'
#' @return a data.frame with computed summary statistics between models.
#' @export
#'
summaryStatistics <- function(fileA = NULL, fileB = NULL,
                              tNUM = getOption('RCroptest.numericThreshold',
                                               default = 2),
                              tDTA = getOption('RCroptest.dateThreshold',
                                               default = 0)) {
  overallStatisticsSummary(fileA, fileB, tNUM, tDTA)$data
}

#' Differences calculated between two DSSAT-CSM Summary output files.
#'
#' @param fileA path to the summary file from Model A.
#' @param fileB path to the summary file from Model B.
#' @param tNUM threshold value for use with numeric calculations.
#' @param tDTA threshodl value for use with date calculations.
#'
#' @return a data.frame with computed differences between models.
#' @export
#'
summaryDifferences <- function(fileA=NULL, fileB=NULL,
                               tNUM=getOption('RCroptest.numericThreshold',
                                              default = 2),
                               tDTA=getOption('RCroptest.dateThreshold',
                                              default = 0)) {
  differencesStatisticsSummary(fileA, fileB, tNUM, tDTA)$differences$data
}
