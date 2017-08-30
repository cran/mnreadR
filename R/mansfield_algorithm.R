#----- Mansfield algorithm -------
#######################-----------

# Maximum Reading Speed (MRS) and Critical Print Size (CPS) estimation
#
# This function calculates simultaneously:
#  \itemize{
#   \item Maximum Reading Speed (MRS)
#   \item Critical Print Size (CPS)
#   }
# while performing print size correction for non-standard testing viewing distance.
#
# @param data The name of your dataframe
# @param print_size The variable that contains print size values for each sentence (print size uncorrected for viewing distance)
# @param viewing_distance The variable that contains the viewing distance value used for testing
# @param reading_time The variable that contains the reading time for each sentence
# @param errors The variable that contains the number of errors for each sentence
# @param ... Optional grouping arguments
#
# @return The function returns a new dataframe with two variables:
#  \itemize{
#   \item "CPS" -> contains the Critical Print Size estimate (in logMAR)
#   \item "MRS" -> contains the Maximum Reading Speed estimate (in words/min)
#   }
#
# @section Notes:
# This function uses the original algorithm described in Legge (2007) to estimate Maximum Reading Speed (MRS) and Critical Print Size (CPS).
# This algorithm searches for a reading speed plateau in the data. A plateau is defined as a range of print sizes
# that supports reading speed at a significantly faster rate than the print sizes smaller or larger than the plateau range.
# Concretely, the plateau is determined as print sizes which reading speed is at least 1.96 SD faster than the other print sizes.
# The Maximum Reading Speed is estimated as the mean reading speed for print sizes included in the plateau.
# The Critical Print Size is defined as the smallest print size on the plateau.
#
# For more details on the original algorithm, see Chapter 5 of this book: Legge, G.E. (2007). Psychophysics of Reading in Normal and Low Vision. Mahwah, NJ & London: Lawrence Erlbaum Associates. ISBN 0-8058-4328-0
#  \url{https://books.google.fr/books/about/Psychophysics_of_Reading_in_Normal_and_L.html?id=BGTHS8zANiUC&redir_esc=y}
#
#
# @section Warning:
# To run the function properly, one needs to make sure that the variables are of the class:
#  \itemize{
#   \item \strong{print_size} -> numeric
#   \item \strong{viewing_distance} -> integer
#   \item \strong{reading_time} -> numeric
#   \item \strong{errors} -> integer
#   }
#
# In cases where only 3 or less sentences were read during a test,
# the function won't be able to estimate the MRS and CPS
# and will return NA values instead.
# The ACC should be used to estimate the MNREAD score in such cases
# where there are not enough data points to fit the MNREAD curve.
#

mansfield_algo <- function(df, logmar, nb_row, logRS) {

  rs <- NULL

  # set initial parameters
  fastest_mean <- 0
  smallest <- 1.4
  if (min(logmar) < smallest) {smallest = min(logmar)}

  # If there are at least 4 sentences tested, the estimation runs
  if (unique(nb_row) > 3) {

    # CPS estimation
    for (i in 2:(unique(nb_row)-2)) {  # unique(nb_row) is the number of row for the current sub-df
      for (j in (i+2):unique(nb_row)) {
        # select max(rs) outside the [i,j] window
        omax = as.numeric(
          df %>%
            filter (logmar < logmar[i] | logmar > logmar[j]) %>%
            summarise (max(rs)))
        # calculates mean log_rs within the [i,j] window
        p_mean <- mean(logRS[i:j])
        # calculates sd log_rs within the [i,j] window
        p_std <- sd(logRS[i:j])
        # run the estimation
        if (log(omax) < p_mean-(1.96*p_std) && log(omax) != -Inf && is.na(log(omax)) == FALSE) {
          if(p_mean > fastest_mean) {
            fastest_mean <- p_mean
            fastest_std <- p_std
            fastest_cps <- logmar[i] # this is the CPS we will use!!
            fastest_lps <- logmar[j] }
        }}}

    # if the estimation failed, CPS and MRS are set to NA
    if (exists("fastest_cps") == FALSE) {
      fastest_cps = NA
      CPS = NA
      MRS = NA}

    # if CPS was estimated properly, MRS is defined accordingly
    if (is.na(fastest_cps) == FALSE) {
      CPS <- fastest_cps
      mrs = as.numeric(
        df %>%
          filter (logmar > fastest_cps) %>%
          summarise (mean(rs)))
      MRS = round(mrs, 2)  }

    return(as.data.frame(cbind(CPS, MRS)))
  }

  # If there are only 3 or less sentences tested, the estimation cannot run -> MRS and CPS are set to NA
  if (unique(nb_row) <= 3) {
    CPS = NA
    MRS = NA
    return(as.data.frame(cbind(CPS, MRS)))
  }

}



