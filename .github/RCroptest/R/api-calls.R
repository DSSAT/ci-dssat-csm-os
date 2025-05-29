#* @filter cors
#' Title
#'
#' @param res request data
#'
#' @noRd
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Headers",
                "Origin, X-Requested-With, Content-Type, Accept")
  plumber::forward()
}

#' apiSummaryModel
#'
#' @param filename path to file (Summary.OUT) to be parsed.
#' @param req list with information about request from plumber API.
#'
#' @return data.frame in JSON format with parser result.
apiSummaryModel <- function(req, filename) {
  debugRequest(req)
  outputModelSummary(filename)
}

#' apiSummaryStatistics
#'
#' Calculate statistics about compare summary files from two DSSAT crop models
#'
#' @param fileA path to file Summary.OUT from DSSAT Crop modelA
#' @param fileB path to file Summary.OUT from DSSAT Crop modelA
#' @param req list with information about request from plumber API.
#' @param tNUM Threshold value for numeric comparison.
#' @param tDTA Threshold value for date comparison.
#'
#' @return data.frame in JSON format with statistics comparison between models
#'
apiSummaryStatistics <- function(req, fileA, fileB, tNUM=NULL, tDTA=NULL) {
  debugRequest(req)
  params <- list(fileA = fileA, fileB = fileB, tNUM = tNUM, tDTA = tDTA)
  params <- params[!sapply(params, is.null)]
  do.call(overallStatisticsSummary, params)
}

#' apiSummaryDifferences
#'
#' Calculate statistics about compare summary files from two DSSAT crop models
#'
#' @param fileA path to file Summary.OUT from DSSAT Crop modelA
#' @param fileB path to file Summary.OUT from DSSAT Crop modelA
#' @param req list with information about request from plumber API.
#' @param tNUM Threshold for numeric comparison.
#' @param tDTA Threshold for date comparison.
#'
#' @return data.frame in JSON format with statistics comparison between models
#'
apiSummaryDifferences <- function(req, fileA, fileB, tNUM=NULL, tDTA=NULL) {
  debugRequest(req)
  params <- list(fileA = fileA, fileB = fileB, tNUM = tNUM, tDTA = tDTA)
  params <- params[!sapply(params, is.null)]
  do.call(differencesStatisticsSummary, params)
}

#' apiEvaluateModel
#'
#' @param filename path to file (Evaluate.OUT) to be parsed.
#' @param req list with information about request from plumber API.
#'
#' @return data.frame in JSON format with parser result.
#'
apiEvaluateModel <- function(req, filename) {
  debugRequest(req)
  outputModelEvaluate(filename)
}

#' apiEvaluateStatistics
#'
#' Calculate statistics about compare summary files from two DSSAT crop models
#'
#' @param fileA path to file Summary.OUT from DSSAT Crop modelA
#' @param fileB path to file Summary.OUT from DSSAT Crop modelA
#' @param req list with information about request from plumber API.
#'
#' @return data.frame in JSON format with statistics comparison between models
#'
apiEvaluateStatistics <- function(req, fileA, fileB) {
  debugRequest(req)
  overallEvaluateSummary(fileA, fileB)
}



