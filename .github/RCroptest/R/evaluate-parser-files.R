#' parseEvaluateFile
#'
#' @param inputfile path to file (Evaluate.OUT) to be parsed.
#'
#' @importFrom stringi stri_trim
#' @importFrom readr read_delim
#'
#' @return a list with dataset and model name
#'
#' @export
#'
parseEvaluateFile <- function(inputfile) {

  functionWarnings <- vector(mode = "list")

  if (!file.exists(inputfile)) {
    stop(sprintf("Evaluate file %s does not exist or cannot be read.",
                 inputfile), call. = FALSE)
  }

  fileOUT <- try(readLines(inputfile), silent = TRUE)
  if (inherits(fileOUT, "try-error")) {
    stop(sprintf("Error reading file %s.",
                 inputfile), call. = FALSE)
  }

  trashLines <- grep(pattern = "[^ ]",
                     fileOUT)[!(grep(pattern = "[^ ]", fileOUT) %in%
                                  c(grep(pattern = "^\\$", fileOUT),
                                    grep(pattern = "^\\*", fileOUT)))]
  fileOUT_clean <- fileOUT[trashLines]

  # position of Header Lines inside inputfile
  trtHeaders <- which(grepl(pattern = "^@", fileOUT_clean))

  # parse DSSAT version from first line of inputfile
  # FileModelID <- stringi::stri_trim(gsub(pattern = ".*DSSAT.*(Ver.*) ([A-Z]{3}.*)",
                                         # replacement = "\\1", fileOUT[1]))

  # if (length(trtHeaders) == 0 || is.na(FileModelID)) {
  #   stop(sprintf("%s is not a valid DSSAT Evaluate output file.",
  #                inputfile), call. = FALSE)
  # }

  if (length(trtHeaders) == 0) {
    stop(sprintf("%s is not a valid DSSAT Evaluate output file.",
                 inputfile), call. = FALSE)
  }

  # test if is multiple runs.
  if (length(trtHeaders) > 1) {
    # calculate data chunck length
    chuncks_length <- c((trtHeaders - dplyr::lag(trtHeaders))[-1],
                        length(fileOUT_clean) - max(trtHeaders) + 1)

    tmpParseData <- lapply(seq_along(trtHeaders), function(w) {
      data_chunck <- fileOUT_clean[seq(from = trtHeaders[w],
                                       length.out = chuncks_length[w])]

      # Select crops using regular expression
      crop_selector <- unlist(lapply(data_chunck[-1],
                                     sub,
                                     pattern = "(.*\\s+)([A-Z]{2})(\\s+.*)",
                                     replacement = "\\2"))

      # filter unique crops for each section
      uq_crops <- unique(crop_selector)

      # change input chunck to use ';' delim (better parse with column names)
      data_chunck_delim <- sub(pattern = '^;',
                               replacement = '',
                               gsub(pattern = '[ ]+',
                                    replacement = ';',
                                    data_chunck))

      # try to read chunck data with delim
      try(suppressWarnings({readr::read_delim(paste(data_chunck_delim,
                                  collapse = '\n'),
                            delim = ";",
                            na = c("-99", "-99.0", "-99.")) }),
          silent = TRUE)
    })

    # check for errors and return error
    if (any(sapply(tmpParseData, inherits,
                   what = 'try-error'))) {
      stop("Cannot parse columns positions.", call. = FALSE)
    }

    cropsInSection <- sapply(lapply(tmpParseData, "[[", "CR"), unique)

    parseCrops <- tmpParseData[which(sapply(cropsInSection, length) == 1)]

    sectionsWithProblem <- which(sapply(cropsInSection, length) != 1)

    if (length(sectionsWithProblem) > 1) {
          msgWarning <- sprintf("Evaluate file contain mixed crop results, please verify %s: %s." ,
                          ifelse(length(sectionsWithProblem) > 1,
                                 'sections',
                                 'section'),
                          paste(sectionsWithProblem, collapse = ', '))

          functionWarnings <- c(functionWarnings, msgWarning)
    }


    names(parseCrops) <- unlist(lapply(lapply(parseCrops,'[[', 'CR'), unique))

    evaluateData <- lapply(unique(names(parseCrops)), function(pc) {
      try(do.call(rbind, parseCrops[names(parseCrops) == pc]), silent = TRUE)
    })

    # check for errors and return a formated warning
    cropWithErrors <- sapply(evaluateData, inherits, what = 'try-error')
    if (any(cropWithErrors)) {

      msgWarning <- sprintf("Bad columns positions, cannot join data by crop. Please verify %s: %s",
                            ifelse(sum(cropWithErrors) > 1, 'crops', 'crop'),
                            paste(unique(names(parseCrops))[cropWithErrors],
                                  collapse = ', '))
      functionWarnings <- c(functionWarnings, msgWarning)
    }

    dataA <- evaluateData[!cropWithErrors]

    dataA <- lapply(dataA, function(e) {
      suppressWarnings({
        rownames(e) <- seq_len(nrow(e))
        e
      })
    })

    names(dataA) <- unique(names(parseCrops))[!cropWithErrors]

  } else {
    data_chunck_delim <- sub(pattern = '^;',
                             replacement = '',
                             gsub(pattern = '[ ]+',
                                  replacement = ';',
                                  fileOUT_clean))
    # try to read chunck data with delim
    tmpA <- try(suppressWarnings({readr::read_delim(paste(data_chunck_delim,
                                                          collapse = '\n'),
                                                    delim = ";",
                                                    na = c("-99",
                                                           "-99.0",
                                                           "-99."))
      }), silent = TRUE)

    # tmpA <- try(read.table(
    #   text = fileOUT_clean,
    #   skip = 1,
    #   col.names = varN[[1]],
    #   na.strings = c("-99", "-99.0", "-99.")
    # ),
    # silent = TRUE)

    # check for errors and return error
    if (inherits(tmpA, 'try-error')) {
      stop("Cannot parse columns positions.", call. = FALSE)
    }

    dataA = split(tmpA, tmpA$CR)
  }

  if (length(functionWarnings) > 0) {
    sapply(functionWarnings, warning, call. = FALSE)
  }

  # return(list(data = dataA, fID = FileModelID, warnings = functionWarnings))
  return(list(warnings = functionWarnings, data = dataA))
}
