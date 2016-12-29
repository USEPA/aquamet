\name{invlegEx}
\alias{invlegEx}
\docType{data}
\title{Example Invasive Species and Legacy Tree Dataset  
}
\description{
  A dataset containing raw invasive species and legacy tree physical habitat data for use in function examples.
}
\usage{data(invlegEx)}
\format{
  A data frame with 264 observations on the following 7 variables.
  \describe{
    \item{SITE}{unique site visit ID.}
    \item{TRANSECT}{reach transect label.}
    \item{SAMPLE_TYPE}{indicator of field form from which data obtained.}
    \item{PARAMETER}{character variable of parameter measured in field.}
    \item{VALUE}{value of measured parameter.}
    \item{FLAG}{flag of value of measured parameter.}
    \item{UNITS}{units of measured parameter.}
  }
}
\details{
  These data are a small subset of the NRSA 2008-2009 invasive species/legacy tree dataset for example purposes only.
}
\examples{
data(invlegEx)
## maybe str(invlegEx) ; plot(invlegEx) ...
}
\keyword{datasets}
