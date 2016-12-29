\name{visripEx}
\alias{visripEx}
\docType{data}
\title{Example Visual Riparian Dataset}
\description{
  A dataset containing raw visual riparian physical habitat data for use in function examples.
}
\usage{data(visripEx)}
\format{
  A data frame with 1760 observations on the following 7 variables.
  \describe{
    \item{SITE}{unique site visit ID.}
    \item{TRANSECT}{reach transect label.}
    \item{BANK}{Bank location: RT=Right, LF=Left,NONE=not applicable.}
    \item{SAMPLE_TYPE}{indicator of field form from which data obtained.}
    \item{PARAMETER}{character variable of parameter measured in field.}
    \item{VALUE}{value of measured parameter.}
    \item{FLAG}{flag of value of measured parameter.}
  }
}
\details{
  These data are a small subset of the NRSA 2008-2009 visual riparian dataset for example purposes only.
}
\examples{
data(visripEx)
head(visripEx)
}
\keyword{datasets}
