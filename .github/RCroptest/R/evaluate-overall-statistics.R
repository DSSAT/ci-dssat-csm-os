#' overallEvaluateSummary
#'
#' @param inA path to modelA file
#' @param inB path to modelB file
#'
#' @return a list with results of comparison between models A and B
#'
overallEvaluateSummary <- function(inA, inB) {

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

  functionWarnings <- vector(mode = "list")

  suppressWarnings({
    eA <- parseEvaluateFile(inA)
    eB <- parseEvaluateFile(inB)
  })

  functionWarnings <- c(functionWarnings,
                       unlist(lapply(eA$warnings, sprintf, fmt = "ModelA => %s")),
                       unlist(lapply(eB$warnings, sprintf, fmt = "ModelB => %s")))

  failCrop <- !sapply(names(eA$data),
                      function(x) all.equal.numeric(eA$data[[x]][['@RUN']],
                                                   eB$data[[x]][['@RUN']]))

  msgFail <- which(failCrop, useNames = TRUE)

  if (any(failCrop)) {
    msgWarning <- paste("Cannot compare crop with different numbers of experiments.",
                        sprintf("Please verify %s." ,
                                paste(names(msgFail),
                                      collapse = ', ')))

    functionWarnings <- c(functionWarnings, msgWarning)

    eA$data <- eA$data[which(!failCrop)]
    eB$data <- eB$data[which(!failCrop)]

    if (any(length(eA$data), length(eB$data))) {
      stop("Cannot compare Evaluate files, the final dataset for crops is not valid or empty.",
           call. = FALSE)
    }
  }

  outputEvalModelA <- modelStatisticsEvaluate(eA, statNames)
  outputEvalModelAB <- modelStatisticsEvaluate(list(eA, eB), statNames)

  row0 <- lapply(outputEvalModelA, names)
  row1 <- lapply(row0, function(z) c(z[1],
                                     rep(c('Model A', 'Model B'),
                                         len = 2 * length(z[-1]))))

  lst_crops <- list(
    titles = lapply(outputEvalModelAB, names),
    titles_row0 = row0,
    titles_row1 = row1,
    indices = lapply(outputEvalModelAB, function(w) seq(from = 2, to = ncol(w))),
    indices_row0 = lapply(outputEvalModelA, function(w) seq(from = 2, to = ncol(w))),
    indices_row1 = lapply(row1, function(w) seq(from = 2, to = length(w))),
    data = outputEvalModelAB
  )

  datas <- lapply(seq_along(names(outputEvalModelAB)),
                  function(x) lapply(lst_crops, '[[', x))
  names(datas) <- names(outputEvalModelAB)


  if (length(functionWarnings) > 0) {
    sapply(functionWarnings, warning, call. = FALSE)
  }

  list(
    warnings = functionWarnings,
    crops = names(outputEvalModelAB),
    formats = list(NUM = getOption('RCroptest.numericFormat', default = 1)),
    data = datas
  )
}

