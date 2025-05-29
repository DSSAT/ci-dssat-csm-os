#' parseSummaryFile
#'
#' @param inputfile path to file (Summary.OUT) to be parsed.
#'
#' @importFrom stringi stri_trim stri_locate
#' @importFrom readr fwf_empty read_fwf fwf_widths
#'
#' @return a list with model information ID and a data.fraem in JSON format.
#'
#' @export
parseSummaryFile <- function(inputfile) {

  if (!file.exists(inputfile)) stop(sprintf("Summary file %s does not exist or cannot be read.",
                                            inputfile), call. = FALSE)

  fileOUT <- try(readLines(inputfile), silent = TRUE)
  if (inherits(fileOUT, "try-error")) stop(sprintf("Error reading file %s.",
                                                   inputfile), call. = FALSE)

  # position of Header Lines inside inputfile
  trtHeaders <- which(grepl(pattern = "^@", fileOUT))

  # parse DSSAT version from first line of inputfile
  FileModelID <- stringi::stri_trim(gsub(pattern = ".*DSSAT.*(Ver.*) ([A-Z]{3}.*)",
                      replacement = "\\1", fileOUT[1]))

  if (length(trtHeaders) == 0 || is.na(FileModelID)) {
    stop(sprintf("%s is not a valid DSSAT Summary output file.",
                 inputfile), call. = FALSE)
  }

  suppressMessages({
    # make names adjust to use with data.frame
    varN <- gsub(pattern = "\\.|^X",
                 replacement = "",
                 make.names(scan(text = fileOUT[trtHeaders[1]],
                                 what = character(), quiet = TRUE)))
  })

  # get start and end of column headers
  head_pos <- lapply(unlist(strsplit(fileOUT[trtHeaders], '[ ]+'))[-1],
                     function(i) stringi::stri_locate(fileOUT[trtHeaders],
                                                      regex = i))
  # get end positions
  end_pos <- c(0, sapply(head_pos, '[[', 2))
  # calculate the column width
  head_wd <- (dplyr::lead(end_pos) - end_pos)[-length(end_pos)]

  # compute columns positions
  fp <- try(readr::fwf_widths(head_wd, varN[-1]), silent = TRUE)
  if (inherits(fp, "try-error")) stop("Cannot parse columns positions.",
                                      call. = FALSE)

  suppressMessages({
    dd0 <- try(read_fwf(file = inputfile,
                        col_positions = fp,
                        skip = trtHeaders,
                        na = c("-99", "-99.0", "-99.")), silent = TRUE)
    aNA <- sapply(dd0, function(x) all(is.na(x)))
    dd0[,aNA] <- NA_integer_
  })

  if (inherits(dd0, "try-error")) stop(sprintf("Cannot load data from file %s.",
                                               inputfile), call. = FALSE)
  if (nrow(dd0) == 0) stop(sprintf("Input file %s is empty.",
                                   inputfile), call. = FALSE)

  list(version = gsub(pattern = 'Ver. ', replacement = '', FileModelID),
       data = dd0)
}
