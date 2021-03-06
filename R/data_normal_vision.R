#' MNREAD data collected in subjects with normal vision.
#'
#' A dataset containing raw MNREAD data for 18 young adults with normal vision. Each subject was tested twice:
#' \itemize{
#' \item once on the regular polarity of the test (black print on white background)
#' \item once on the reverse polarity of the test (white print on black background)
#' }
#'
#' @format A data frame with 684 rows and 6 variables, where each line stores data for one sentence:
#' \describe{
#'   \item{subject}{subject ID code}
#'   \item{polarity}{test polarity used (regular or reverse)}
#'   \item{vd}{viewing distance in cm}
#'   \item{ps}{print size in logMAR, as written on the chart (print size uncorrected for viewing distance)}
#'   \item{rt}{reading time in seconds}
#'   \item{err}{number of errors}
#'   ...
#' }
#' @source Data collected at the Minnesota Laboratory for Low-Vision Research (UMN)
"data_normal_vision"
