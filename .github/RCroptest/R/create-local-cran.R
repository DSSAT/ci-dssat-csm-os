#' createLocalCRAN
#'
#' @param base_dir package path directory
#' @param type repository type: posix, win or both
#'
#' @importFrom utils download.packages
#'
createLocalCRAN <- function(base_dir = ".", type = c('both', 'win', 'posix')) {
  op <- options(install.packages.check.source = "no")

  type <- match.arg(type)
  switch(type,
    win = windows <- TRUE,
    posix = posix <- TRUE,
    both = windows <- posix <- TRUE
  )

  install_deps <- c("Depends", "Imports", "LinkingTo")

  if (file.exists('DESCRIPTION')) {
    description_file <- file.path('.', "DESCRIPTION")
  } else {
    description_file <- system.file('DESCRIPTION', package = 'RCroptest')
  }

  dcf <- read.dcf(description_file)
  deps <- unique(gsub("\\s.*", "",
                      stringi::stri_trim(unlist(
                        strsplit(dcf[, intersect(install_deps, colnames(dcf))],
                                 ","),
                        use.names = FALSE
                      ))))

  # filter dependencies to include only contrib packages
  deps <- deps[which(deps != 'R')]
  if (length(deps) > 0) {
    list_all_deps <- unique(c(deps,
                              unlist(
                                tools::package_dependencies(deps,
                                                            recursive = TRUE)
                              )))

    all_deps <- list_all_deps[which(!(
      list_all_deps %in% c('base',
                           'stats',
                           'tools',
                           'utils',
                           'methods')
    ))]

    destPath = file.path(base_dir, "localCRAN", c('src', 'bin'))

    if (any(dir.exists(destPath))) {
      unlink(file.path(base_dir, 'localCRAN'), recursive = TRUE)
    }

    if (posix) {
      dir.create(destPath[1], recursive = TRUE)
      download.packages(all_deps, destdir = destPath[1], type = 'source')
    }
    if (windows) {
      dir.create(destPath[2], recursive = TRUE)
      download.packages(all_deps, destdir = destPath[2], type = 'win.binary')
    }
  }

  options(op)
}

