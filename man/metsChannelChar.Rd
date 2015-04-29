\name{metsChannelChar}
\alias{metsChannelChar}
\title{Calculate NRSA Channel Characteristic Metrics}
\description{
  This function calculates the channel characteristics portion of the physical
  habitat metrics for National Rivers and Streams Assessment (NRSA) data.  The
  function requires data frames containing the bank geometry and  channel
  characteristics data files.
}
\usage{
metsChannelChar(bankgeometry, channelchar)
}
\arguments{
  \item{bankgeometry}{a data frame containing the bank geometry data file.
    The data frame must include columns that are named as follows:
    \itemize{
      \item \code{UID} - universal ID value
      \item \code{SAMPLE_TYPE} - sample type
      \item \code{TRANSECT} - transect label
      \item \code{TRANSDIR} - transverse location along transect
      \item \code{PARAMETER} - identifier for each measurement, assessment,
        score, etc.
      \item \code{RESULT} - measurement associated with PARAMETER column
      \item \code{UNITS} - units of the RESULT measurement
      \item \code{FLAG} - flag
    }
  }
  \item{channelchar}{a data frame containing the channel characteristics data
    file.  The data frame must include columns that are named as follows:
    \itemize{
      \item \code{UID} - universal ID value
      \item \code{SAMPLE_TYPE} - sample type
      \item \code{TRANSECT} - transect label
      \item \code{PARAMETER} - identifier for each measurement, assessment,
        score, etc.
      \item \code{RESULT} - measurement associated with PARAMETER column
      \item \code{FLAG} - flag
    }
    Note that possible values for variables in the input data frames are
    provided in the document named "NRSA Documentation.pdf" included in the help
    directory for the package.
  }
}
\value{
  Either a data frame when metric calculation is successful or a character
  string containing an error message when metric calculation is not successful.
  The data frame contains the following columns:
  \itemize{
    \item \code{UID} - universal ID value
    \item \code{METRIC} - metric name
    \item \code{RESULT} - metric value
  }
}
\author{
Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
Tom Kincaid \email{Kincaid.Tom@epa.gov}
}
\examples{
  \dontrun{
    data(thalwegEx)
    head(thalwegEx)
    data(changeomEx)
    head(changeomEx)
    testGeneral <- metsGeneral(thalwegEx,changeomEx)
    head(testGeneral)
  }
}
\keyword{survey}
