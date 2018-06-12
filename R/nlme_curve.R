#----- nlmeCurve ------
#######################--

#' NLME fit plotting of MNREAD data.
#' 
#' This function plots individual MNREAD curves and Critical Print Size (CPS) as estimated by the NLME fit.
#'
#' @param data The name of your dataframe
#' @param print_size The variable that contains print size values for each sentence (print size uncorrected for viewing distance)
#' @param viewing_distance The variable that contains the viewing distance value used for testing
#' @param reading_time The variable that contains the reading time for each sentence
#' @param errors The variable that contains the number of errors for each sentence
#' @param subjectID The variable that contains the subject identifiers
#' @param nested Optional argument to build a model with a nested structure. 'nested' specifies which variable should be nested within subject
#' @param group Optional argument to build a model with a grouped structure. 'group' specifies which variable should be used a grouping argument
#' @param CPScriterion Optional argument to specify a criterion for CPS estimation. The default criterion value is '90 of MRS'. This criterion can vary from 75 to 95 of MRS and should only be modified for specific purposes, as discussed in Cheung et al. 2008 
#'
#' @return 
#' The function returns a plot of reading speed (in log words/min) as a function of print size (in logMAR). 
#' Critical Print Size is marked as an inverted triangle.
#'
#' @section Notes:
#' This function performs print size correction for non-standard testing viewing distance before plotting the curve.
#' 
#' This function uses a nonlinear mixed effects model (NLME), as described in Cheung et al. 2008, to estimate Maximum Reading Speed (MRS) and Critical Print Size (CPS).
#' For more details on the parameters estimation, see \code{\link{nlmeParam}}.
#'
#'
#' @section Warning:
#' For the function to run properly, one needs to make sure that the variables are of the class:
#'  \itemize{
#'   \item \strong{print_size} -> numeric
#'   \item \strong{viewing_distance} -> integer
#'   \item \strong{reading_time} -> numeric
#'   \item \strong{errors} -> integer
#'   }
#'
#'
#'
#' @seealso
#' \code{\link{mnreadCurve}} for standard MNREAD curve
#'
#' \code{\link{curveParam_RT}} for MRS and CPS estimation using values of reading time (instead of reading speed)
#'
#' \code{\link{readingAcuity}} for Reading Acuity calculation
#'
#'
#'
#' @examples 
#' # inspect the structure of the dataframe
#' head(data_low_vision, 10)
#'
#' #------
#'
#' # restrict dataset to one MNREAD test per subject (regular polarity only)
#' data_regular <- data_low_vision %>%
#'     filter (polarity == "regular")
#'
#' # plot the NLME fit for data grouped by subject
#' \dontrun{ nlmeCurve(data_regular, ps, vd, rt, err, subject) }
#' 
#' #------
#'
#' # plot the NLME fit for the whole dataset with polarity nested within subject
#' \dontrun{ nlmeCurve(data_low_vision, ps, vd, rt, err, subject,
#'                     nested = polarity) }
#' 
#' #------
#'
#' # plot the NLME fit for the whole dataset with polarity nested within subject 
#' # and grouped based on treatment
#' \dontrun{ nlmeCurve(data_low_vision, ps, vd, rt, err, subject,
#'                     nested = polarity, group = treatment) }
#'
#' #------
#'
#' # plot the NLME fit for the whole dataset with polarity nested within subject 
#' # and grouped based on treatment
#' # for a specific CPS criterion of '80 of MRS'
#' \dontrun{ nlmeCurve(data_low_vision, ps, vd, rt, err, subject, 
#'                     nested = polarity, group = treatment, 
#'                     0.8) }
#'
#'
#'
#' @importFrom stats sd coef predict
#' @importFrom nlme nlsList nlme groupedData nlmeControl
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#'
#'  
#'
#' @export
nlmeCurve <- function(data, print_size, viewing_distance, reading_time, errors, subjectID, nested = NULL, group = NULL, CPScriterion = NULL) {
  # This function estimates the Maximum reading Speed (MRS) and Critical Print Size (CPS) with an NLME model and returns a plot of the fitted curves.
  
  print_size <- enquo(print_size)
  viewing_distance <- enquo(viewing_distance)
  reading_time <- enquo(reading_time)
  errors <- enquo(errors)
  subjectID <- enquo(subjectID)
  nested <- enquo(nested)
  group <-enquo(group)
  subject <- NULL
  group_var <- NULL
  nested_var <- NULL
  rs <- NULL
  log_rs <- NULL
  correct_ps <- NULL
  asym <- NULL
  lrc <- NULL
  x_intercept <- NULL
  grp_var <- NULL
  rowname <- NULL
  MRS <- NULL
  CPS <- NULL
  fitted_rs <- NULL
  predict.nested_var <- NULL
  predict.subject <- NULL
  . <- NULL
  
  message('This may take a few minutes... just relax and take a break :-)')
  
  # modify the raw dataframe as needed before running the model
  temp_df <- as.data.frame(
    data %>%
      mutate (subject = (!!subjectID) ) %>%
      mutate (nested_var = (!!nested) ) %>%
      mutate (group_var = (!!group) ) %>%
      filter ((!!errors) != "NA" & (!!reading_time) > 0) %>%
      mutate (rs = (10 - replace ((!!errors), (!!errors) > 10, 10)) / (!!reading_time) * 60) %>%
      filter (rs != "NA", rs != "-Inf") %>%
      mutate (log_rs = log10(rs)) %>%
      filter (log_rs != "NA", log_rs != "-Inf") %>%
      mutate (correct_ps = (!!print_size) + round(log10(40/(!!viewing_distance)), 2)) %>%
      filter (correct_ps != "NA", correct_ps != "-Inf")
  )
  
  # create the groupedData to be passed to the NLME model
  ## NB: no matter what structure is used for the model (nested, grouped, ...)
  ## the model call will always be the same, it is the structure of the groupedData that will change
  
  if ( "nested_var" %in% names(temp_df) == FALSE  ) {
    if ( "group_var" %in% names(temp_df) == FALSE ) {
      grouped_df <- groupedData(log_rs ~ correct_ps | subject,
                                data = temp_df %>% select(log_rs, correct_ps, subject),
                                labels = list( x = "Print size", y = "Log reading speed"),
                                units = list( x = "(logMAR)", y = "(logWPM)"))    }
    else {
      grouped_df <- groupedData(log_rs ~ correct_ps | subject,
                                outer = ~ group_var,
                                data = temp_df %>% select(log_rs, correct_ps, subject, group_var),
                                labels = list( x = "Print size", y = "Log reading speed"),
                                units = list( x = "(logMAR)", y = "(logWPM)"))    }
  }
  
  if ( "nested_var" %in% names(temp_df) == TRUE ) {
    if ( "group_var" %in% names(temp_df) == FALSE ) {
      grouped_df <- groupedData(log_rs ~ correct_ps | subject/nested_var,
                                data = temp_df %>% select(log_rs, correct_ps, subject, nested_var),
                                labels = list( x = "Print size", y = "Log reading speed"),
                                units = list( x = "(logMAR)", y = "(logWPM)"))     }
    else {
      grouped_df <- groupedData(log_rs ~ correct_ps | subject/nested_var,
                                outer = ~ group_var,
                                data = temp_df %>% select(log_rs, correct_ps, subject, nested_var, group_var),
                                labels = list( x = "Print size", y = "Log reading speed"),
                                units = list( x = "(logMAR)", y = "(logWPM)"))    }
  }
  
  
  # generate starting values for nlme by running nlsList
  my.list <- nlsList (model = log_rs ~ SSasympOff (correct_ps, asym, lrc, x_intercept) ,
                      data = grouped_df )
  my.starting.values <- colMeans( as.matrix( coef(my.list), na.rm=TRUE ), 
                                  na.rm=TRUE ) # Missing values should be omitted from the calculations
  
  # run my nlme model
  my.model <- nlme (model = log_rs ~ SSasympOff (correct_ps, asym, lrc, x_intercept),
                    data = grouped_df,
                    fixed = asym + lrc + x_intercept ~ 1, # set the fixed structure for nlme
                    # by default the random structure is set to (asym ~ 1, lrc ~ 1, x_intercept ~ 1) for grp_var
                    start = my.starting.values,
                    control = nlmeControl(maxIter = 500, pnlsTol = 0.3)) # set the control parameters for nlme
  
  # extract coefficients from the nlme model
  my.coef <- rownames_to_column(as.data.frame(coef(my.model)))
  
  # set the percentage of maximum reading speed we want to use
  # CPS is the smallest print size that yields p times the maximum reading speed
  if ( missing(CPScriterion) )  {
    CPS_crit = 0.90 }
  else {
    CPS_crit = CPScriterion }
  
  # get Maximum Reading Speed (MRS) and Critical Print Size (CPS)
  nlme.estimates <- my.coef %>% 
    mutate (MRS = asym) %>%
    mutate (CPS = log((-log10(CPS_crit))/asym) / (-exp(lrc)) + x_intercept) %>%
    separate(rowname, into = c("subject", "nested_var"), sep = "/", fill = "right")
  
  # find the max CPS value to set the x-axis limit
  max_CPS <- max(nlme.estimates$CPS)
  
  # define the figure layout
  p <- ggplot(data = temp_df,
              aes(x = correct_ps, y = log_rs))
  p <- p + facet_wrap(~subject)
  p <- p + scale_x_continuous(name = "Corrected Print Size (logMAR)")
  p <- p + scale_y_continuous(name = "Reading Speed (log words/min)")
  
  # I stopped here where it stopped working 
  # https://stat.ethz.ch/R-manual/R-devel/library/nlme/html/predict.nlme.html
  
  # plot a non-nested design 
  if ( "nested_var" %in% names(temp_df) == FALSE  ) { 

    # extract the fitted values
    # add rows with missing data for incomplete datasets
    if (max_CPS > max(temp_df$correct_ps)) {
      full_temp_df <- temp_df %>% 
        complete (subject, correct_ps = seq(min(temp_df$correct_ps), max_CPS, 0.05)) %>% 
        select (subject, correct_ps)
    }
    else {
      full_temp_df <- temp_df %>% 
        complete (subject, correct_ps) %>% 
        select (subject, correct_ps)
    }
    # extract the fixed effects predictions at the subject level 
    fitted_df <- predict(my.model, full_temp_df, level = 0:1) # 0 = populations; 1 = subject
    # merge the fitted data with temp_df
    full_fitted_df <- bind_cols(full_temp_df, fitted_df) %>%
      filter (predict.subject >= 0)

    # plot the fitted curve
    p <- p + geom_line(aes(x = correct_ps, y = predict.subject), 
                       size=1, alpha=0.7, show.legend = FALSE,
                       data = full_fitted_df)

    # plot the fitted values    
      if ( "group_var" %in% names(temp_df) == FALSE ) { 
        p <- p + geom_point() }
      else {  
        p <- p + geom_point(aes(shape = group_var)) }
    
    # add CPS
    p <- p + geom_point(aes(x = CPS, y = MRS),
                        shape = 25, size = 3, fill = "red",
                        data = nlme.estimates)
    
  }

  # plot a nested design 
  if ( "nested_var" %in% names(temp_df) == TRUE ) {
    
    # extract the fitted values
    # add rows with missing data for incomplete datasets
    if (max_CPS > max(temp_df$correct_ps)) {
      full_temp_df <- temp_df %>% 
        complete (subject, nested_var, correct_ps = seq(min(temp_df$correct_ps), max_CPS, 0.05)) %>% 
        select (subject, nested_var, correct_ps)
    }
    else {
      full_temp_df <- temp_df %>% 
        complete (subject, nested_var, correct_ps) %>% 
        select (subject, nested_var, correct_ps)
    }
    # extract the fixed effects predictions at the nested variable level
    fitted_df <- predict(my.model, full_temp_df, level = 0:2) # 0 = populations; 1 = subject; 2 = nested_var
    # merge fitted data with temp_df
    full_fitted_df <- bind_cols(full_temp_df, fitted_df) %>%
      filter (predict.nested_var >= 0)

    # plot the fitted curve
    p <- p + geom_line(aes(x = correct_ps, y = predict.nested_var, colour = nested_var), 
                       size=1, alpha=0.7, show.legend = FALSE,
                       data = full_fitted_df)
 
    # plot the fitted values
      if ( "group_var" %in% names(temp_df) == FALSE ) {
        p <- p + geom_point(aes(colour = nested_var)) }
      else {
        p <- p + geom_point(aes(colour = nested_var, shape = group_var)) }
    
    # add CPS
    p <- p + geom_point(aes(x = CPS, y = MRS, fill = nested_var),
                        shape = 25, size = 3,
                        data = nlme.estimates)
    
  }

   
  return(p)
  
}
