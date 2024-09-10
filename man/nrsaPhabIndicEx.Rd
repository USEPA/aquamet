\name{nrsaPhabIndicEx}
\alias{nrsaPhabIndicEx}
\docType{data}
\title{Example NRSA Physical Habitat Indicator Input Dataset}
\description{
  A dataset containing NRSA physical habitat metric and site data for use in 
  indicator function examples.
}
\usage{data(nrsaPhabIndicEx)}
\format{
  A data frame with 4 observations on the following 14 variables.
  \describe{
    \item{SITE}{unique site visit ID.}
    \item{LRBS_G08}{Relative bed stability metric used in 
    \emph{nrsaRelBedStabilityIndicator()} function.}
    \item{W1_HALL}{Metric for human disturbance used in 
    \emph{nrsaRipDistIndicator()} function.}
    \item{XCMGW}{Metric for woody ground, mid-layer, and canopy cover used in 
    \emph{nrsaRiparianVegIndicator()} function.}
    \item{XFC_NAT}{Metric for natural fish cover used in \emph{nrsaInstrmCoverIndicator()} function.}
    \item{XSLOPE}{Mean slope of sampling reach.}
    \item{XWIDTH}{Mean wetted width of sampling reach.}
    \item{SITE_ID}{Site ID}
    \item{ELEV_PT}{Elevation at x-site of sampling reach.}
    \item{WSAREASQKM}{Pourpoint watershed area of sampling site.}
    \item{AGGR_ECO9_2015}{Aggregated Omernik ecoregion of sampling site.}
    \item{LAT_DD83}{Latitude of site in decimal degrees (NAD83 datum)}
    \item{LON_DD83}{Longitude of site in decimal degrees (NAD83 datum)}
    \item{REALM}{Specification of physical habitat protocols used (WADEABLE/BOATABLE).}
  }
}
\details{
  These data are a small subset of the NRSA physical habitat metrics and site data used 
  in assigning condition for physical habitat indicators, for example purposes only.
}
\examples{
  data(nrsaPhabIndicEx)
  head(nrsaPhabIndicEx)
}
\keyword{datasets}
