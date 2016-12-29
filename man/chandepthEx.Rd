\name{chandepthEx}
\alias{chandepthEx}
\docType{data}
\title{Example Channel Depth Dataset}
\description{
  A dataset containing raw channel depth physical habitat data for use in function examples.
}
\usage{data(chandepthEx)}
\format{
  A data frame with 110 observations on the following 8 variables.
  \describe{
    \item{SITE}{unique site visit ID.}
    \item{TRANSECT}{reach transect label.}
    \item{LINE}{line on field form indicating position of measurement.}
    \item{SAMPLE_TYPE}{indicator of field form from which data obtained.}
    \item{PARAMETER}{character variable of parameter measured in field.}
    \item{VALUE}{value of measured parameter.}
    \item{UNITS}{units of measured parameter.}
    \item{FLAG}{flag of value of measured parameter.}    
  }
}
\details{
  These data are a small subset of the NRSA 2008-2009 channel depth dataset for example purposes only.
}
\examples{
data(chandepthEx)
head(chandepthEx)
}
\keyword{datasets}
