
#' checkColumns
#'
#' Verify two models and check the columns before comparison procedure.
#'
#' @param dA data.frame with data from modelA
#' @param dB data.frame with data from modelB
#'
#' @return list with common variables, extra variables and reference model
#'
checkColumns <- function(dA, dB) {
    nDA = names(dA)
    nDB = names(dB)
    lDA = length(nDA)
    lDB = length(nDB)

    if (lDA > lDB) {
        m0 = match(nDB, nDA)
        extra = seq_len(lDA)[!(seq_len(lDA) %in% m0)]
        cmAB = nDA[seq_len(lDA) %in% m0]
        m = 1
    } else if (lDB > lDA) {
        m0 = match(nDA, nDB)
        extra = seq_len(lDB)[!(seq_len(lDB) %in% m0)]
        cmAB = nDB[seq_len(lDB) %in% m0]
        m = 2
    } else {
        cmAB = nDA
        extra = NA
        m = 0
    }

    return(list(commonVars = cmAB, extaVars = extra, model = m))
}

#' makeDiffAt
#'
#' Calculate the absolute or relative difference between values
#' based on the type of column (DATE ou NUMERIC).
#'
#' @param val1 numeric Value from model A
#' @param val2 numeric Value from model B
#' @param idx numeric Index of the actual column
#' @param iDTA numeric Index of Date columns
#'
#' @return a numeric value with the differece between model
#'
makeDiffAt <- function(val1, val2, idx, iDTA) {
    val1 = as.numeric(unlist(val1))
    val2 = as.numeric(unlist(val2))
    if (idx %in% iDTA) {
        abs(val1 - val2)
    } else {
        ifelse(val1 == 0, 0, (val2 - val1)/val1)
    }
}

#' decimalLen
#'
#' Verify if the column type is decimal and calculate it's length
#'
#' @param col numeric vector with column content
#'
#' @return numeric Calculated decimal lenght or NA
#'
decimalLen <- function(col) {

    if (!inherits(col, "numeric"))
        col = as.data.frame(col)[, names(col)]


    info = as.character(col)[!is.na(col)]
    data = info[which(sapply(strsplit(info, "\\."), length) == 2)]

    if (length(data) > 0) {
        decs = sapply(strsplit(data, "\\."), "[[", 2)
        len_decs = stringi::stri_length(decs)
        return(max(len_decs))
    } else return(NA)
}

#
# dAgr:	Agreement Index
# DStat:	D-Statistic
# EF:	Model Efficiency
# MAE:	Mean Absolute Error
# Mean_Dif:	Average Difference
# Mean_Obs:	Average Obs. Values
# Mean_Sim:	Average Sim. Values
# N_Obs:	Num. Obs. Values
# N_Sim:	Num. Sim. Values
# nRSME:	Normalized Root Mean Squared Error
# RMSE:	Root Mean Squared Error
# Rsq:	R Squared (R2)
# StDev_Obs:	Std.Dev. Obs. Values
# StDev_Sim:	Std.Dev. Sim. Values

#' preProcessEvaluate
#'
#' @param dt data.frame with raw Evaluate data to preprocess
#'
#' @importFrom dplyr select mutate
#' @importFrom tidyr extract gather spread
#' @importFrom rlang .data
#'
#' @return data.frame Summary statistics for use with evaluate comparison.
#'
preProcessEvaluate <- function(dt) {
  dt %>%
    select(-c(.data$EXCODE:.data$RN)) %>%
    gather(key = 'valueinfo', value = 'measure', -c(.data$`@RUN`:.data$CR)) %>%
    mutate(measure = as.numeric(.data$measure)) %>%
    tidyr::extract(.data$valueinfo, c("info", "type"),
                   regex = "([A-Z0-9\\#\\%]+)([A-Z]$)") %>%
    spread(.data$type, .data$measure) %>%
    select(.data$info:.data$S) %>%
    group_by(.data$info) %>%
    summarise(s_Mean_Obs = mean(.data$M, na.rm = TRUE),
              s_Mean_Sim = mean(.data$S, na.rm = TRUE),
              s_StDev_Obs = sd(.data$M, na.rm = TRUE),
              s_StDev_Sim = sd(.data$S, na.rm = TRUE),
              s_Mean_Dif = mdiff(.data$M, .data$S),
              s_RMSE = rmse(.data$M, .data$S),
              s_DStat = mDstat(.data$M, .data$S),
              s_nRSME = nrmse(.data$M, .data$S),
              s_EF = ef(.data$M, .data$S),
              s_dAgr = idxagr(.data$M, .data$S),
              s_Rsq = mRsq(.data$M, .data$S),
              s_MAE = mae(.data$M, .data$S),
              s_N_Obs = ifelse(sum(!is.na(.data$M)) == 0, NA,
                               sum(!is.na(.data$M))),
              s_N_Sim = ifelse(sum(!is.na(.data$S)) == 0, NA,
                               sum(!is.na(.data$S)))) %>%
    gather(key = 'Stat', value = 'val', -.data$info) %>%
    mutate(Stat = gsub("s_", "", .data$Stat))
}

# Function that returns Root Mean Squared Error
mse <- function(obs, sim) {
  obs = as.numeric(obs)
  sim = as.numeric(sim)
    error <- obs - sim
    mean(error^2, na.rm = TRUE)
}


# Function that returns Root Mean Squared Error
rmse <- function(obs, sim) {
  obs = as.numeric(obs)
  sim = as.numeric(sim)
    error <- obs - sim
    sqrt(mean(error^2, na.rm = TRUE))
}

# Normalized Root Mean Squared Error
nrmse <- function(obs, sim) {
  obs = as.numeric(obs)
  sim = as.numeric(sim)
    (rmse(obs, sim)/mean(obs, na.rm = TRUE)) * 100
}

# Model Efficiency
ef <- function(obs, sim) {
    # x = obs, y = sim
  obs = as.numeric(obs)
  sim = as.numeric(sim)
    error <- sim - obs
    xbar <- mean(obs, na.rm = TRUE)
    1 - sum(error^2, na.rm = TRUE)/sum((obs - xbar)^2, na.rm = TRUE)
}

# Agreement Index
idxagr <- function(obs, sim) {
    # x = obs, y = sim
  obs = as.numeric(obs)
  sim = as.numeric(sim)
    error <- sim - obs
    xbar <- mean(obs, na.rm = TRUE)
    ybar <- mean(sim, na.rm = TRUE)
    mody <- abs(sim - xbar)
    modx <- abs(obs - ybar)
    1 - sum(error^2, na.rm = TRUE)/sum((mody + modx)^2, na.rm = TRUE)
}

# Function that returns Mean Absolute Error
mae <- function(obs, sim) {
  obs = as.numeric(obs)
  sim = as.numeric(sim)
    error <- obs - sim
    mean(abs(error), na.rm = TRUE)
}

mdiff <- function(obs, sim) {
  obs = as.numeric(obs)
  sim = as.numeric(sim)
    error <- sim - obs
    mean(error, na.rm = TRUE)
}

# D-Statistic
mDstat <- function(obs, sim) {

    obs = as.numeric(obs)
    sim = as.numeric(sim)
    sim_obs_diff = sim - obs
    s1 <- sum(sim_obs_diff^2, na.rm = TRUE)
    s2 <- sum((abs(sim - mean(obs, na.rm = TRUE)) + abs(obs - mean(obs, na.rm = TRUE)))^2, na.rm = TRUE)

    ifelse(s2 == 0, NA, round(1 - s1/s2, 3))
}

#' mRsq - Calculate R Squared (R2)
#'
#' @param obs observed values vector
#' @param sim simulated values vector
#'
#' @return R2 value
#' @importFrom stats cor
mRsq <- function(obs, sim) {
  obs = as.numeric(obs)
  sim = as.numeric(sim)
  cor(obs, sim, use = "pairwise.complete.obs") ^ 2
}


CV <- function(x, ...) {
  sd(x, ...)/mean(x, ...)
}


#' TotOutlier
#'
#' Compute the total number of outliers using boxplot.stats.
#'
#' @param x a numeric vector for which the boxplot will be constructed.
#' @param na.rm parameter to enable/disable removing NA and NaN.
#'
#' @importFrom grDevices boxplot.stats
#'
#' @return a number of outliers detected
#'
TotOutlier <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  length(boxplot.stats(x)$out)
}

#' Tot
#'
#' Compute the total number of records without NA and NaN.
#'
#' @param x a numeric vector for which the boxplot will be constructed.
#' @param ... a list with other parameters to pass to sum function.
#'
#' @return a number of records.
#'
Tot <- function(x, ...) {
  sum(!is.na(x), na.rm = TRUE)
}

calculateIndexes <- function(models) {
  ctypes <- sapply(models$modelA$data, class)
  ptypes <- grep(pattern = "double|integer|numeric", x = ctypes)
  dtypes <- grep(pattern = "double", x = ctypes)
  dLen <- sapply(models$modelA$data[, dtypes], decimalLen)

  ids <- seq(1, min(grep(pattern = "DAT", names(models$modelA$data))) - 1)
  testIDS0 <- ptypes[which(ptypes > max(ids))]
  testIDS <- seq(from = min(testIDS0), to = max(testIDS0))

  idx <- grep(pattern = "DAT", x = names(models$modelA$data[, testIDS]))
  idxDTA <- testIDS[idx]
  idxNUM <- testIDS[testIDS > max(idxDTA)]

  idxNUM_temp <- idxNUM[which(!(idxNUM %in% dtypes[which(dLen > 1)]))]
  idxDBL <- ifelse(length(dtypes) > 0, dtypes[which(dLen > 1)], NA)

  list(ids = ids,
       testIDS = testIDS,
       idx = idx,
       idxDTA = idxDTA,
       idxNUM = idxNUM,
       idxDBL = idxDBL,
       idxNUM_temp = idxNUM_temp)
}



checkThresholdValue <- function(models, tNUM, tDTA) {

  idxs <- calculateIndexes(models)

  threshold <- c(rep(tDTA, length = length(idxs$idxDTA)),
                 rep(tNUM/100, length = length(idxs$idxNUM)))

  if(nrow(models$modelA$data) > 1) {
    tresp <- as.data.frame(sapply(seq_along(idxs$testIDS), function(i) {
      try(makeDiffAt(models$modelA$data[, idxs$testIDS[i]],
                     models$modelB$data[, idxs$testIDS[i]],
                     idxs$testIDS[i], idxs$idxDTA), silent = TRUE)
    }))

    trespYN <- as.data.frame(sapply(seq_along(idxs$testIDS), function(i) {
      try(ifelse(abs(tresp[, i]) > threshold[i], "Y", "N"),
          silent = TRUE)
    }))

  } else {
    tresp <- data.frame(matrix(sapply(seq_along(idxs$testIDS), function(i) {
      try(makeDiffAt(models$modelA$data[, idxs$testIDS[i]],
                     models$modelB$data[, idxs$testIDS[i]],
                     idxs$testIDS[i], idxs$idxDTA), silent = TRUE)
    }),nrow = 1))

    trespYN <- data.frame(matrix(sapply(seq_along(idxs$testIDS), function(i) {
      try(ifelse(abs(tresp[, i]) > threshold[i], "Y", "N"),
          silent = TRUE)
    }), nrow = 1))
  }

  # check for errors and return error
  if (any(sapply(trespYN, inherits, what = 'try-error'),
          sapply(tresp, inherits, what = 'try-error'))) {
    stop("Cannot compare models due a calculation error.",
         call. = FALSE)
  }

  names(tresp) <- names(models$modelA$data)[idxs$testIDS]
  names(trespYN) <- names(tresp)

  list(testValues = tresp, testFlags = trespYN)
}

filterLists <- function(lst) {
  sapply(lst, function(w) !all(is.na(w)))
}

#' adjustNamesEvaluateModel
#'
#' @param modelData data.frame Evaluate data.
#' @param modelNames list Row names to format data.
#'
#' @return data.frame with formated data and respective names.
#'
adjustNamesEvaluateModel <- function(modelData, modelNames) {
  names(modelData)[1] <- 'Statistics'
  modelData$Statistics <- modelNames

  modelData[which(apply(modelData, 1,
                        function(w) sum(is.na(w))) != (ncol(modelData) - 1)),
            which(modelData[which(modelData$Statistics == "Num. Sim. Values"), ] > 0)]
}


#' modelStatisticsEvaluate
#'
#' Calculate statistics from raw output from Evaluate file.
#'
#' @param listData data.frame Data from raw evaluate model.
#' @param rowNames list Row names for Statistics data.frame.
#'
#' @importFrom dplyr mutate bind_rows
#' @importFrom tidyr unite spread
#' @importFrom rlang .data
#'
#' @return list Processed data.frame info with statistics for use.
#'
modelStatisticsEvaluate <- function(listData, rowNames) {
  if (length(listData) > 1 && is.null(names(listData))) {
    # modelA and modelB
    res <- lapply(seq_along(listData[[1]][['data']]), function(id) {
      mOut <- preProcessEvaluate(listData[[1]][['data']][[id]]) %>%
        mutate(model = "m1") %>%
        bind_rows(preProcessEvaluate(listData[[2]][['data']][[id]]) %>%
                    mutate(model = "m2")) %>%
        unite(col = 'info_model', .data$info, .data$model) %>%
        spread(.data$info_model, .data$val)
      adjustNamesEvaluateModel(mOut, rowNames)
    })
    names(res) <- names(listData[[1]][['data']])
    res
  } else  {
    # only one model
    lapply(listData[['data']], function(a) {
      mOut <- preProcessEvaluate(a) %>%
        spread(key = 'info', value = 'val')
      adjustNamesEvaluateModel(mOut, rowNames)
    })
  }
}

#' checkFilesSummary
#'
#' Check if input files (Summary.OUT) from both models could be compared.
#'
#' @param fileA path to file (Summary.OUT) to be parsed from modelA.
#' @param fileB path to file (Summary.OUT) to be parsed from modelB.
#'
#' @importFrom dplyr if_else
#'
#' @return a list with dataset from model A, modelB and warnings
#'
checkFilesSummary <- function(fileA, fileB) {

  functionWarnings <- vector(mode = "list")

  tmA <- parseSummaryFile(fileA)
  tmB <- parseSummaryFile(fileB)

  fA <- which(with(tmA$data, sprintf("%d-%d-%s-%s", TRNO, R, CR,
                                     substr(EXNAME, 1, 8))) %in%
                with(tmB$data, sprintf("%d-%d-%s-%s", TRNO, R, CR,
                                       substr(EXNAME, 1, 8))))

  fB <- which(with(tmB$data, sprintf("%d-%d-%s-%s", TRNO, R, CR,
                                     substr(EXNAME, 1, 8))) %in%
                with(tmA$data, sprintf("%d-%d-%s-%s", TRNO, R, CR,
                                       substr(EXNAME, 1, 8))))

  if (length(fA) < 1) {
    stop(paste(
      sprintf("Summary files from Model A (%s) and Model B (%s) does not have",
              fileA, fileB),
      "data in common. Please verify."), call. = FALSE)
  }

  functionWarnings <- c(functionWarnings, tmA$warnings, tmB$warnings)

  if (nrow(tmA$data[-fA,]) > 0 | nrow(tmB$data[-fB,]) > 0) {
    nA = nrow(tmA$data[-fA,])
    nB = nrow(tmB$data[-fB,])
    trA = paste((tmA$data[-fA,])$RUNNO, collapse = ', ')
    trB = paste((tmB$data[-fB,])$RUNNO, collapse = ', ')
    msgA = sprintf('%s %s from Model A',
                   if_else(nA > 1, 'treatments', 'treatment'), trA)
    msgB = sprintf('%s %s from Model B',
                   if_else(nB > 1, 'treatments', 'treatment'), trB)

    msgWarning <- paste(sprintf("Dropping %s.",
                                if_else(all(nA > 0, nB > 0),
                                        paste(msgA, msgB, sep = " and "),
                                        if_else(nA > 0, msgA, msgB))),
                        "Comparing the common ones.")
    functionWarnings <- c(functionWarnings, msgWarning)
  }

  tmA$data <- tmA$data[fA,]
  tmB$data <- tmB$data[fB,]

  rm(fA, fB)

  cc <- checkColumns(tmA$data, tmB$data)

  mA <- tmA
  mB <- tmB

  if (cc$model > 0) {
    msgWarning <-
      paste("Models have a different number of columns.",
            sprintf("Dropping %s '%s' from %s and comparing the common ones.",
                    ifelse(length(cc$extaVars) > 1, 'columns', 'column'),
                    paste(names(tmB$data)[cc$extaVars], collapse = ', '),
                    ifelse(cc$model == 1, 'ModelA', 'ModelB')))

    functionWarnings <- c(functionWarnings, msgWarning)

    mA$data <- as.tbl(tmA$data[, cc$commonVars])
    mB$data <- as.tbl(tmB$data[, cc$commonVars])
  }

  list(warnings_list = functionWarnings, modelA = mA, modelB = mB)
}

#' checkHeadersSummary
#'
#' Verify data from Summary files.
#'
#' @param models list with models data.frames
#' @param tNUM Threshold for numeric comparison.
#' @param tDTA Threshold for date comparison.
#'
#' @return a list with dataset from model A, modelB and warnings
#'
checkHeadersSummary <- function(models, tNUM, tDTA) {

  idxs <- calculateIndexes(models)
  thValues <- checkThresholdValue(models, tNUM, tDTA)
  testYN <- cbind(models$modelA$data[, idxs$ids], thValues$testFlags)

  # Filter Cols and Rows
  fmtCol <- which(apply(apply(testYN, 1:2,
                              function(x) x == "Y"), 2,
                        sum, na.rm = TRUE) > 0)
  fmtRow <- which(apply(apply(testYN, 1:2,
                              function(x) x == "Y"), 1,
                        sum, na.rm = TRUE) > 0)

  list(
    fmtCol = fmtCol,
    fmtRow = fmtRow
  )
}

#' Title
#'
#' @param req request data to debug
#'
#' @noRd
debugRequest <- function(req) {
  cat('Request-Header: ', req$HEADERS, '\n')
  cat('Request-Args: ', paste(req$args, collapse = '//'), '\n')
  cat('Request-Query: ', req$QUERY_STRING, '\n')
  cat('Request-Hooks: ', req$rook.url_scheme, '::', req$rook.version, '\n')
  cat('Request-Hooks::INPUT: ', paste(ls(envir = req$rook.input), collapse = '//'), '\n')
  cat('Request-Hooks::ERROR: ', paste(req$rook.error, collapse = '//'), '\n')
}
