\name{metsHumanImpact}
\alias{metsHumanImpact}
\title{Calculate NLA Human Influence Metrics}
\description{
  This function calculates the human influence portion of the physical habitat
  metrics for National Lakes Assessment (NLA) data.  The function requires a
  data frame containing validated physical habitat data collected using the NLA
  protocol.
}
\usage{
metsHumanImpact(df, data2007=FALSE, fillinDrawdown=TRUE)
}
\arguments{
  \item{df}{a data frame containing human influence data.  The data frame must
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
        following values: HI_BUILDINGS, HI_COMMERCIAL, HI_CROPS, HI_DOCKS,
        HI_LANDFILL, HI_LAWN, HI_ORCHARD, HI_PARK, HI_PASTURE, HI_POWERLINES,
        HI_ROADS, and HI_WALLS.
      \item \code{RESULT} - parameter values, which are the values used in
        calculations.
      \item \code{UNITS} - parameter units, which identifies the units in which
        the parameter values are recorded.
    }
  }
  \item{data2007}{a logical value, which equals TRUE if 2007 data is being
    processed.  The default value is FALSE.
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
    data(metsHumanImpactEx)
    head(metsHumanImpactEx)
    testmetsHumanImpact <- metsHumanImpact(metsHumanImpactEx)
    head(testmetsHumanImpact)
  }
}
\keyword{survey}
