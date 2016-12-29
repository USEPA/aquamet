\name{chancharEx}
\alias{chancharEx}
\docType{data}
\title{Example Channel Characteristics Dataset}
\description{
  A dataset containing raw channel characteristics physical habitat data for use in function examples.
}
\usage{data(chancharEx)}
\format{
  A data frame with 25 observations on the following 6 variables.
  \describe{
    \item{SITE}{unique site visit ID.}
    \item{TRANSECT}{reach transect label.}
    \item{PARAMETER}{character variable of parameter measured in field.}
    \item{VALUE}{value of measured parameter.}
    \item{FLAG}{flag of value of measured parameter.}
    \item{SAMPLE_TYPE}{indicator of field form from which data obtained.}    
  }
}
\details{
  These data are a small subset of the NRSA 2008-2009 channel characteristics dataset for example purposes only.
}
\examples{
data(chancharEx)
head(chancharEx)
}
\keyword{datasets}
