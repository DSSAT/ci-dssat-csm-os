#' outputModelEvaluate
#'
#' @param filename path to file (Evaluate.OUT) to be parsed.
#'
#' @return a list with model information ID and a data.fraem in JSON format.
#'
outputModelEvaluate <- function(filename) {

  statNames <- c('Agreement Index',
                 'D-Statistic',
                 'Model Efficiency',
                 'Mean Absolute Error',
                 'Mean Difference',
                 'Mean Obs. Values',
                 'Mean Sim. Values',
                 'Num. Obs. Values',
                 'Num. Sim. Values',
                 'Normalized Root M.S.E.',
                 'Root Mean Squared Error',
                 'R Squared (R2)',
                 'Std.Dev. Obs. Values',
                 'Std.Dev. Sim. Values')

  out <- parseEvaluateFile(filename)
  functionWarnings <- out$warnings
  outputEvM <- modelStatisticsEvaluate(out, statNames)

  lst_crops <- list(
    titles = lapply(outputEvM, names),
    indices =  lapply(outputEvM, function(w) seq(from = 2, to = ncol(w))),
    data = outputEvM)

  datas <- lapply(seq_along(names(outputEvM)),
                  function(x) lapply(lst_crops, '[[', x))
  names(datas) <- names(outputEvM)

  list(
    warnings = functionWarnings,
    crops = names(outputEvM),
    formats = list(NUM = getOption('RCroptest.numericFormat', default = 1)),
    data = datas
  )
}

