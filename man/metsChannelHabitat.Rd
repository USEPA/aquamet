\name{metsChannelHabitat}
\alias{metsChannelHabitat}
\title{Calculate NRSA Channel Habitat Metrics}
\description{
  This function calculates the channel habitat portion of the physical habitat
  metrics for National Rivers and Streams Assessment (NRSA) data.  The function
  requires a data frame containing the thalweg data file.
}
\usage{
metsChannelHabitat(thalweg)
}
\arguments{
  \item{thalweg}{a data frame containing the thalweg data file.
    The data frame must include columns that are named as follows:
    \itemize{
      \item \code{UID} - universal ID value
      \item \code{SAMPLE_TYPE} - sample type
      \item \code{TRANSECT} - transect label
      \item \code{STATION} - station number along thalweg between transects
      \item \code{PARAMETER} - identifier for each measurement, assessment,
        score, etc.
      \item \code{RESULT} - measurement associated with PARAMETER column
      \item \code{UNITS} - units of the RESULT measurement
      \item \code{FLAG} - flag
    }
    Note that possible values for variables in the input data frame are
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
    testChanHab <- metsChannelHabitat(thalwegEx)
    head(testChanHab)
  }
}
\keyword{survey}
