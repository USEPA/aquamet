\name{thalwegEx}
\alias{thalwegEx}
\docType{data}
\title{Example Thalweg Dataset
}
\description{
  A dataset containing raw thalweg physical habitat data for use in function examples.
}
\usage{data(thalwegEx)}
\format{
  A data frame with 3045 observations on the following 8 variables.
  \describe{
    \item{SITE}{unique site visit ID.}
    \item{TRANSECT}{reach transect label.}
    \item{STATION}{station number between transects}
    \item{SAMPLE_TYPE}{indicator of field form from which data obtained.}
    \item{PARAMETER}{character variable of parameter measured in field.}
    \item{VALUE}{value of measured parameter.}
    \item{FLAG}{flag of value of measured parameter.}    
    \item{UNITS}{units of measured parameter.}
  }
}
\details{
  These data are a small subset of the NRSA 2008-2009 thalweg dataset for example purposes only.
}
\examples{
data(thalwegEx)
head(thalwegEx)
}
\keyword{datasets}
