% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reading_speed.R
\name{readingSpeed_nonCorrected}
\alias{readingSpeed_nonCorrected}
\title{Reading speed calculation not corrected for the number of errors}
\usage{
readingSpeed_nonCorrected(data, reading_time)
}
\arguments{
\item{data}{The name of your dataframe}

\item{reading_time}{The variable that contains the reading time for each sentence}
}
\value{
The function returns the original dataframe with an added variable called "reading_speed_nonCorrected" that contains reading speed (in words/min) for each sentence tested.
}
\description{
This function calculates reading speed (in words per minute) using reading time (in seconds) only.
This calculation provides a simplified value of reading speed, that does not take into account the number of misread words.
}
\section{Notes}{

This function gives a less precise reading speed measurement than \code{\link{readingSpeed}}.
Unless you know what you are doing, consider using \code{\link{readingSpeed}} instead of this function.
}

\examples{
# inspect the strucutre of the dataframe
head(data_low_vision, 10)

# run the reading speed calculation
data_low_vision_new <- readingSpeed_nonCorrected(data_low_vision, rt)

# inspect the structure of the newly created dataframe
head(data_low_vision_new, 10)

}
\seealso{
\code{\link{readingSpeed}} for reading speed corrected for errors
}
