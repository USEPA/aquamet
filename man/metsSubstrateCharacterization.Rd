\name{metsSubstrateCharacterization}
\alias{metsSubstrateCharacterization}
\title{Calculate NRSA Substrate Characterization Metrics}
\description{
  This function calculates the substrate characterization portion of the
  physical habitat metrics for National Rivers and Streams Assessment (NRSA)
  data.  The function requires frames containing the channel crosssection,
  thalweg, and littoral data files.
}
\usage{
metsSubstrateCharacterization(channelcrosssection, thalweg, littoral)
}
\arguments{
  \item{channelcrosssection}{a data frame containing the channel crosssection
    data file.  The data frame must include columns that are named as follows:
    \itemize{
      \item \code{UID} - universal ID value
      \item \code{SAMPLE_TYPE} - sample type
      \item \code{TRANSECT} - transect label
      \item \code{TRANSDIR} - transverse location along transect
      \item \code{PARAMETER} - identifier for each measurement, assessment,
        score, etc.
      \item \code{RESULT} - measurement associated with PARAMETER column
      \item \code{FLAG} - flag
    }
  }
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
  }
  \item{littoral}{a data frame containing the littoral data file. The data frame
    must include columns that are named as follows:
    \itemize{
      \item \code{UID} - universal ID value
      \item \code{SAMPLE_TYPE} - sample type
      \item \code{TRANSECT} - transect label
      \item \code{PARAMETER} - identifier for each measurement, assessment,
        score, etc.
      \item \code{RESULT} - measurement associated with PARAMETER column
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
    data(channelxsectEx)
    head(channelxsectEx)
    data(thalwegEx)
    head(thalwegEx)
    data(littoralEx)
    head(littoralEx)
    testSubChar <- metsSubstrateCharacterization(channelxsectEx,thalwegEx,littoralEx)
    head(testSubChar)
  }
}
\keyword{survey}
