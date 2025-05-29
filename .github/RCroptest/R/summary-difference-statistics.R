#' differenceStatisticsSummary
#'
#' Check if input files (Summary.OUT) from both models could be compared.
#'
#' @param fileA path to file (Summary.OUT) to be parsed from modelA.
#' @param fileB path to file (Summary.OUT) to be parsed from modelB.
#' @param tNUM Threshold value for numeric comparison.
#' @param tDTA Threshold value for date comparison.
#'
#' @return a list with dataset from model A, modelB and warnings
#'
differencesStatisticsSummary <- function(fileA=NULL, fileB=NULL,
                                        tNUM=getOption('RCroptest.numericThreshold', default = 2),
                                        tDTA=getOption('RCroptest.dateThreshold', default = 0)) {

  models <- checkFilesSummary(fileA, fileB)
  headers <- checkHeadersSummary(models, tNUM, tDTA)
  idxs <- calculateIndexes(models)
  thValues <- checkThresholdValue(models, tNUM, tDTA)

  functionWarnings <- models$warnings_list

  modelA <- models$modelA$data
  modelB <- models$modelB$data

  diff <- cbind(modelA[, idxs$ids], thValues$testValues)

  # Filtered
  diff_filtered <- diff[headers$fmtRow, c(idxs$ids, headers$fmtCol)]


  idxDTA_filtered <- which(headers$fmtCol %in% idxs$idxDTA) + max(idxs$ids)
  idxNUM_filtered <- which(headers$fmtCol %in% idxs$idxNUM_temp) + max(idxs$ids)
  idxDBL_filtered <- ifelse(length(which(headers$fmtCol %in% idxs$idxDBL)) == 0,
                            NA,
                            names(headers$fmtCol)[headers$fmtCol %in% idxs$idxDBL])

  # Numerics format lengths for each type of result (DTA, NUM, DBL)
  fmtDTA <- getOption('RCroptest.dateFormat')
  fmtNUM <- getOption('RCroptest.numericFormat')
  fmtDBL <- ifelse(!is.na(idxs$idxDBL), max(as.numeric(idxs$idxDBL)), NA)

  if (length(functionWarnings) > 0) {
    sapply(functionWarnings, warning, call. = FALSE)
  }

  list_formats = list(fmtDTA = fmtDTA, fmtNUM = fmtNUM, fmtDBL = fmtDBL)

  ldiff_ind = list( DTA = idxs$idxDTA,
                       NUM = idxs$idxNUM,
                       DBL = idxs$idxDBL)
  ldiff_filt_ind = list( DTA = idxDTA_filtered,
                         NUM = idxNUM_filtered,
                         DBL = idxDBL_filtered)

  list_diff = list(data = diff,
                   indices = ldiff_ind[filterLists(ldiff_ind)],
                   titles = names(modelA))

  list_diff_filtered = list(data  = ifelse(nrow(diff_filtered) > 0,
                                            diff_filtered, NA),
                            indices = ldiff_filt_ind[filterLists(ldiff_filt_ind)],
                            titles = ifelse(nrow(diff_filtered) > 0,
                                            names(modelA)[c(idxs$ids, headers$fmtCol)],
                                            NA)
                            )

  list(list_warnings = functionWarnings,
       formats = list_formats[filterLists(list_formats)],
       differences = list_diff,
       differences_filtered = list_diff_filtered[filterLists(list_diff_filtered)]
  )
}
