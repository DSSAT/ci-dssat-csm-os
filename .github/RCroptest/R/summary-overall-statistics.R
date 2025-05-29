#' overallStatisticsSummary
#'
#' Check if input files (Summary.OUT) from both models could be compared.
#'
#' @param fileA path to file (Summary.OUT) to be parsed from modelA.
#' @param fileB path to file (Summary.OUT) to be parsed from modelB.
#' @param tNUM  threshold value for numeric comparison between models (default to 2).
#' @param tDTA  threshold value for date comparsion between models (default to 0).
#'
#' @importFrom dplyr summarise_all select
#' @importFrom rlang .data
#'
#' @return a list with dataset from model A, modelB and warnings
#'
overallStatisticsSummary <- function(fileA=NULL,
                                     fileB=NULL,
                                     tNUM=getOption('RCroptest.numericThreshold'),
                                     tDTA=getOption('RCroptest.dateThreshold')) {

  models <- checkFilesSummary(fileA, fileB)
  headers <- checkHeadersSummary(models, tNUM, tDTA)
  thValues <- checkThresholdValue(models, tNUM, tDTA)
  idxs <- calculateIndexes(models)

  functionWarnings <- models$warnings_list

  modelA <- models$modelA$data

  diff <- cbind(models$modelA$data[, idxs$ids], thValues$testValues)

  z0 <- diff %>%
    select(-idxs$ids) %>%
    summarise_all(funs(min, max, mean, sd, TotOutlier, Tot), na.rm = TRUE) %>%
    gather(key = 'var', value = 'val', factor_key = TRUE) %>%
    separate(col = .data$var, c('var', 'Statistics'), '_') %>%
    spread(.data$var, .data$val)

  # reorder columns
  z1 <- z0[, c("Statistics", names(diff)[-idxs$ids])]
  z2 <- data.frame(Statistics = "TotAbvTh",
                   t(apply(thValues$testFlags, 2,
                           function(x) sum(x == "Y", na.rm = TRUE))))
  z3 <- rbind(z1, z2,
              data.frame(Statistics = "PerAbvTh",
                         z2[, -1]/nrow(thValues$testFlags)),
              data.frame(Statistics = "PerOutlier",
                         z0[z0$Statistics == 'TotOutlier', -1]/nrow(thValues$testFlags)))
  rownames(z3) <- seq_len(nrow(z3))

  tagO <- c('Tot', 'min', 'mean', 'max', 'sd', 'PerAbvTh',
            'TotAbvTh', 'PerOutlier', 'TotOutlier')
  idx <- as.numeric(sapply(tagO, function(x) which(z3$Statistics == x)))
  z4 <- z3[idx,]
  z4$Statistics <- c("Total Values",
                     "Min. Difference",
                     "Mean Difference",
                     "Max. Difference",
                     "Std. Deviation",
                     "Perc. Above Threshold",
                     "Total Above Threshold",
                     "Perc. Outliers",
                     "Total Outliers")

  fmtDTA <- getOption('RCroptest.dateFormat')
  fmtNUM <- getOption('RCroptest.numericFormat')
  fmtDBL <- ifelse(!is.na(idxs$idxDBL), max(as.numeric(idxs$idxDBL)), NA)

  list_formats = list(fmtDTA = fmtDTA, fmtNUM = fmtNUM, fmtDBL = fmtDBL)
  list_indices = list(DTA = grep(pattern = "DAT", x = names(z4)),
                      NUM = seq(max(grep(pattern = "DAT",
                                         x = names(z4))) + 1,
                                ncol(z4)))

  list(list_warnings = functionWarnings,
       titles = names(z4),
       data = z4,
       formats = list_formats[filterLists(list_formats)],
       indices = list_indices[filterLists(list_indices)]
  )
}
