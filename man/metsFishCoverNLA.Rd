\name{metsFishCoverNLA}
\alias{metsFishCoverNLA}
\title{Calculate NLA Fish Cover Metrics}
\description{
  This function calculates the fish cover portion of the physical habitat
  metrics for National Lakes Assessment (NLA) data.  The function requires a
  data frame containing validated physical habitat data collected using the NLA
  protocol.  NOTE: These calculations do not currently return every metric for
  every site, as this did not occur for 2007.  Using the test data for the 2007
  unit test shows that site 7545 is lacking FC[FC|FP|V][BRUSH|SNAGS] and site
  has2NAvalues is lacking another 41 metrics.
}
\usage{
metsFishCoverNLA(df, createSyntheticCovers=TRUE, fillinDrawdown=TRUE)
}
\arguments{
  \item{df}{a data frame containing fish cover data.  The data frame must
    include columns that are named as follows:
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
        following values: FC_AQUATIC, FC_BOULDERS, FC_BRUSH, FC_LEDGES,
        FC_TREES, FC_OVERHANG, FC_SNAG, and FC_STRUCTURES.  Each fish cover
        class occurs at least once in the input dataframe (this is because of
        how NA counts are set to 0 at the end of the function.
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
    data(metsFishCoverNLAEx)
    head(metsFishCoverNLAEx)
    testmetsFishCoverNLA <- metsFishCoverNLA(metsFishCoverNLAEx)
    head(testmetsFishCoverNLA)
  }
}
\keyword{survey}
