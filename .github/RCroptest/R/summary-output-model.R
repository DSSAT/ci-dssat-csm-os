#' outputModelSummary
#'
#' @param filename path to file (Summary.OUT) to be parsed.
#'
#' @return a list with model information ID and a data.fraem in JSON format.
#'
outputModelSummary <- function(filename) {
  out <- parseSummaryFile(filename)

  fmtDTA <- 0
  fmtNUM <- 1

  models <- list(modelA = out)
  idxs <- calculateIndexes(models)

  list_formats = list(fmtDTA = fmtDTA, fmtNUM = fmtNUM)
  list_indices = list(DTA = idxs$idxDTA, NUM = idxs$idxNUM, DBL = idxs$idxDBL)

  list(version = out$version,
       titles = names(out$data),
       formats = list_formats[filterLists(list_formats)],
       indices = list_indices[filterLists(list_indices)],
       data = out$data)
}
