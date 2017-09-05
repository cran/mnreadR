#' mnreadR: An R package for analyzing MNREAD data
#'
#' mnreadR provides simple functions to estimate the four MNREAD parameters:
#'  \itemize{
#'   \item \strong{Maximum Reading Speed} (MRS) 
#'   -> can be estimated alone with \code{\link{curveParam_RT}} and \code{\link{curveParam_RS}} 
#'   or simultaneously with the other MNREAD parameters with \code{\link{mnreadParam}}
#'   \item \strong{Critical Print Size} (CPS) 
#'   -> can be estimated alone with \code{\link{curveParam_RT}} and \code{\link{curveParam_RS}} 
#'   or simultaneously with the other MNREAD parameters with \code{\link{mnreadParam}}
#'   \item \strong{Reading Acuity} (RA) 
#'   -> can be estimated alone with \code{\link{readingAcuity}} 
#'   or simultaneously with the other MNREAD parameters with \code{\link{mnreadParam}}
#'   \item \strong{Reading ACCessibility Index} (ACC) 
#'   -> can be estimated alone with \code{\link{accIndex}} 
#'   or simultaneously with the other MNREAD parameters with \code{\link{mnreadParam}}
#'   }
#'
#' @section Notes
#' Future implementations will include NLME estimation of the MRS and CPS :-)
#'
#' @section Contact
#' Aurelie Calabrese - \email{acalabre@umn.edu}
#'
#' @docType package
#' @name mnreadR
NULL
#> NULL
