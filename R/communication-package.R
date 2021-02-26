#' An R package for Feature Extraction and Model Estimation for Audio of Human Speech
#'
#' Try the example below. Have a look at the references and learn more
#' from function documentation such as [hmm()].
#' @section Options:
#'
#' TODO: think about package options, implement and describe them
#'
#' - `communication.option1`: Should the output be coloured? (Default: `TRUE`).
#' - `communication.option2`: The maximum number of detailed test
#'    reports printed for the summary reporter (default: 10).
#'
#' TODO: finalize it
#'
#' @keywords internal
"_PACKAGE"


# Using dynamic library with C++ functions
## usethis namespace: start
#' @useDynLib communication, .registration = TRUE
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL


# Package environment
communication_env <- new.env(parent = emptyenv())

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL