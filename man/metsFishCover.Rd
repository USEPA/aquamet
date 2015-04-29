\name{metsFishCover}
\alias{metsFishCover}
\title{Calculate NRSA Fish Cover Metrics}
\description{
  This function calculates the fish cover portion of the physical habitat
  metrics for National Rivers and Streams Assessment (NRSA) data.  The function
  requires data frames containing the fish cover and stream verification form
  data files.
}
\usage{
metsFishCover(fishcover, visits)
}
\arguments{
  \item{fishcover}{a data frame containing the bank geometry data file.
    The data frame must include columns that are named as follows:
    \itemize{
      \item \code{UID} - universal ID value
      \item \code{SAMPLE_TYPE} - sample type
      \item \code{TRANSECT} - transect label
      \item \code{PARAMETER} - identifier for each measurement, assessment,
        score, etc.
      \item \code{RESULT} - measurement associated with PARAMETER column
      \item \code{FLAG} - flag
    }
  }
  \item{visits}{a data frame containing the stream verification form data file.
    The data frame contains a protocol value for each UID value.  It also should
    include columns that relate UID to values meaningfull to the user, e.g.,
    site ID, date collected, and visit number.  The data frame must include
    columns that are named as follows:
    \itemize{
      \item \code{UID} - universal ID value
      \item \code{VALXSITE} - protocol used during a site visit (BOATABLE,
        PARBYBOAT, ALTERED, INTWADE, PARBYWADE, WADEABLE)
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
    data(fishcoverEx)
    head(fishcoverEx)
    data(visitsEx)
    head(visitsEx)
    testFishCover <- metsFishCover(fishcoverEx,visitsEx)
    head(testFishCover)
  }
}
\keyword{survey}
