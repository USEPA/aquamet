\name{metsRiparianVegetationNLA}
\alias{metsRiparianVegetationNLA}
\title{Calculate NLA Riparian Zone and Vegetation Metrics}
\description{
  This function calculates the riparian zone and vegetation portion of the
  physical habitat metrics for National Lakes Assessment (NLA) data.  The
  function requires a data frame containing validated physical habitat data
  collected using the NLA protocol.  NOTE: composite indices use calc instead
  of normCover values, which 2007 mets also do.  Check with Phil to see if this
  is a bug.
}
\usage{
metsRiparianVegetationNLA(df, createSyntheticCovers=TRUE, fillinDrawdown=TRUE)
}
\arguments{
  \item{df}{a data frame containing riparian zone and vegetation data.  The data
    frame must include columns that are named as follows:
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
        following values: CANOPY, C_BIGTREES, C_SMALLTREES, UNDERSTORY,
        U_NONWOODY, U_WOODY, GC_BARE, GC_INUNDATED, GC_NONWOODY, GC_WOODY,
        HORIZ_DIST_DD, and DRAWDOWN.
      \item \code{RESULT} - parameter values, which are the values used in
        calculations.
      \item \code{UNITS} - parameter units, which identifies the units in which
        the parameter values are recorded.
    }
  }
  \item{createSyntheticCovers}{a logical value, which specifies whether to
    create synthetic cover values as proportions of drawdown and riparian cover.
    This argument should be set to FALSE when the data follows the 2007 NLA
    protocol or do not contain drawdown cover data.  The default value is TRUE.
  }
  \item{fillinDrawdown}{a logical value, which specifies whether to use the
    DRAWDOWN parameter to fill in unrecorded cover and HORIZ_DIST_DD values.
    The default value is TRUE.
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
    data(metsRiparianVegetationNLAEx)
    head(metsRiparianVegetationNLAEx)
    testmetsRiparianVegetationNLA <- metsRiparianVegetationNLA(metsRiparianVegetationNLAEx)
    head(testmetsRiparianVegetationNLA)
  }
}
\keyword{survey}
