\name{metsBedStability}
\alias{metsBedStability}
\title{Calculate NRSA Bed Stability Metrics}
\description{
  This function calculates the bed stability portion of the physical habitat
  metrics for National Rivers and Streams Assessment (NRSA) data.  The function
  requires data frames containing the bankgeometry, thalweg, visits,
  channelgeometry, channelcrosssection, littoral, wood, and fishcover data
  files.
}
\usage{
metsBedStability(bankgeometry, thalweg, visits, channelgeometry,
  channelcrosssection, littoral, wood, fishcover, gisCalcs=NULL)
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
  }
  \item{channelgeometry}{a data frame containing the channel geometry data file.
    The data frame must include columns that are named as follows:
    \itemize{
      \item \code{UID} - universal ID value
      \item \code{SAMPLE_TYPE} - sample type
      \item \code{TRANSECT} - transect label
      \item \code{TRANLINE} - location (mid-channel or bank)along transect
      \item \code{BANK} - bank (left or right) along transect
      \item \code{LINE} - way point (1,2, etc.) between transects
      \item \code{METHOD} - method used to measure slope
      \item \code{PARAMETER} - identifier for each measurement, assessment,
        score, etc.
      \item \code{RESULT} - measurement associated with PARAMETER column
      \item \code{UNITS} - units of the RESULT measurement
      \item \code{FLAG} - flag
    }
  }
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
  }
  \item{wood}{a data frame containing the large woody debris data file.
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
    directory for the package.
  }
  \item{gisCalcs}{a data frame containing metric values that were determined
    using GPS based calculations.  The default value for this argument is NULL.
    The data frame must include columns that are named as follows:
    \itemize{
      \item \code{UID} - universal ID value
      \item \code{METRIC} - metric name
      \item \code{RESULT} - metric value
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
    data(bankgeomEx)
    head(bankgeomEx)
    data(thalwegEx)
    head(thalwegEx)
    data(visitsEx)
    head(visitsEx)
    data(changeomEx)
    head(changeomEx)
    data(channelxsectEx)
    head(channelxsectEx)
    data(littoralEx)
    head(littoralEx)
    data(woodEx)
    head(woodEx)
    data(fishcoverEx)
    head(fishcoverEx)
    testBedStab <- metsBedStability(bankgeomEx, thalwegEx, visitsEx, changeomEx,
       channelxsectEx, littoralEx, woodEx, fishcoverEx) 
    head(testBedStab)
  }
}
\keyword{survey}
