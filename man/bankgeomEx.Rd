\name{bankgeomEx}
\alias{bankgeomEx}
\docType{data}
\title{Example Bank Geometry Dataset}
\description{
  A dataset containing raw bank geometry physical habitat data for use in function examples.
}
\usage{data(bankgeomEx)}
\format{
  A data frame with 395 observations on the following 8 variables.
  \describe{
    \item{UID}{unique site visit ID.}
    \item{TRANSECT}{reach transect label.}
    \item{TRANSDIR}{location along transect: RT=Right,RC=Right center,CT=Center,LC=Left center,LF=Left,NONE=not applicable.}
    \item{SAMPLE_TYPE}{indicator of field form from which data obtained.}
    \item{PARAMETER}{character variable of parameter measured in field.}
    \item{RESULT}{value of measured parameter.}
    \item{UNITS}{units of measured parameter.}
    \item{FLAG}{flag of value of measured parameter.}
  }
}
\details{
  These data are a small subset of the NRSA 2008-2009 bank geometry dataset for example purposes only.
}
\examples{
data(bankgeomEx)
head(bankgeomEx)
}
\keyword{datasets}
