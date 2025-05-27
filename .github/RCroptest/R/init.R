.onLoad <- function(libname, pkgname) {
  repos <- c(
    "https://marcionicolau.github.io/RCroptestDeps/",
    "https://cloud.r-project.org"
  )

  options(repos = c(CRAN = repos))
  invisible()
}

.onAttach <- function(libname, pkgname) {

  rv <- R.Version()
  if (as.numeric(rv$major) < 3 && as.numeric(rv$minor) < 6) {
    packageStartupMessage("RCroptest requires R-3.6.0 or later. Please install the latest\nversion of R from https://cloud.r-project.org/")
  }

  op <- options()
  op.RCroptest <- list(
    RCroptest.name = 'Marcio Nicolau',
    RCroptest.desc.author = '"Marcio Nicolau <marcio.nicolau@embrapa.br> [aut, cre]"',
    RCroptest.api.port = base::as.integer(base::Sys.getenv("RCROPTEST_PORT", unset = 9917)),
    RCroptest.api.host = base::Sys.getenv("RCROPTEST_HOST", unset = "127.0.0.1"),
    RCroptest.numericThreshold = 2,
    RCroptest.dateThreshold = 0,
    RCroptest.numericFormat = 1,
    RCroptest.dateFormat = 0,
    RCroptest.doubleFormat = 1,
    install.packages.check.source = "no"
  )

  options(op.RCroptest)

  invisible()
}

.onUnload <- function(libpath) {
  options(install.packages.check.source = "yes")
}
