\name{metsShorelineSubstrate}
\alias{metsShorelineSubstrate}
\title{Calculate NLA Shoreline Substrate Metrics}
\description{
  This function calculates the shoreline substrate portion of the physical
  habitat metrics for National Lakes Assessment (NLA) data.  The function
  requires a data frame containing validated physical habitat data collected
  using the NLA protocol.
}
\usage{
metsShorelineSubstrate(df)
}
\arguments{
  \item{df}{a data frame containing shoreline substrate data.  The data frame
    must include columns that are named as follows:
    \itemize{
      \item \code{UID} - universal ID value, which uniquely identifies the site
        location, date of visit, visit order, habitat type, etc., for which
        metrics will be calculated.  For NLA, site ID, year and visit number 
        are used for this purpose.
      \item \code{STATION} - the subordinate ID value, which identifies the
        location, habitat type, order of occurence, etc., within a single UID.
        For NLA, transect is used for this purpose.
      \item \code{PARAMETER} - parameter name, which identifies the variables
        used in calculations.  In wide data frame format, this argument would be
        used to name columns.  It is assumed that this argument has the
        following values: SS_BEDROCK, SS_BOULDERS, SS_COBBLE, SS_GRAVEL,
        SS_SAND, SS_SILT, SS_ORGANIC, SS_OTHER, and SS_WOOD.
      \item \code{RESULT} - parameter values, which are the values used in
        calculations.
      \item \code{UNITS} - parameter units, which identifies the units in which
        the parameter values are recorded.
    }
  }
}
\value{
  A data frame that contains the following columns:
  \itemize{
    \item \code{UID} - universal ID value
    \item \code{PARAMETER} - metric name
    \item \code{RESULT} - metric value
  }
}
\author{
Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
Tom Kincaid \email{Kincaid.Tom@epa.gov}
}
\examples{
  \dontrun{
    data(metsShorelineSubstrateEx)
    head(metsShorelineSubstrateEx)
    testmetsShorelineSubstrate <- metsShorelineSubstrate(metsShorelineSubstrateEx)
    head(testmetsShorelineSubstrate)
  }
}
\keyword{survey}
