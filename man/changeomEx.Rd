\name{changeomEx}
\alias{changeomEx}
\docType{data}
\title{Example Channel Geometry Dataset}
\description{
  A dataset containing raw channel geometry physical habitat data for use in function examples.
}
\usage{data(changeomEx)}
\format{
  A data frame with 479 observations on the following 11 variables.
  \describe{
    \item{SITE}{unique site visit ID.}
    \item{TRANSECT}{reach transect label.}
    \item{SAMPLE_TYPE}{indicator of field form from which data obtained.}
    \item{PARAMETER}{character variable of parameter measured in field.}
    \item{TRANLINE}{location for transect GPS coordinates (Bank/Midstream/None).}
    \item{BANK}{bank chosen for reading (right/left/none).}
    \item{LINE}{line number on field form.}
    \item{METHOD}{method of measurement.}
    \item{UNITS}{units of measured parameter.}
    \item{VALUE}{value of measured parameter.}
    \item{FLAG}{flag of value of measured parameter.}    
  }
}
\details{
  These data are a small subset of the NRSA 2008-2009 channel geometry dataset for example purposes only.
}
\examples{
data(changeomEx)
head(changeomEx)
}
\keyword{datasets}
