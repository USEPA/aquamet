\name{littoralEx}
\alias{littoralEx}
\docType{data}
\title{
Example Littoral Substrate Dataset
}
\description{
  A dataset containing raw littoral substrate physical habitat data for use in function examples.
}
\usage{data(littoralEx)}
\format{
  A data frame with 110 observations on the following 6 variables.
  \describe{
    \item{SITE}{unique site visit ID.}
    \item{TRANSECT}{reach transect label.}
    \item{SAMPLE_TYPE}{indicator of field form from which data obtained.}
    \item{PARAMETER}{character variable of parameter measured in field.}
    \item{VALUE}{value of measured parameter.}
    \item{FLAG}{flag of value of measured parameter.}    
  }
}
\details{
  These data are a small subset of the NRSA 2008-2009 littoral substrate dataset for example purposes only.
}
\examples{
data(littoralEx)
head(littoralEx)
}
\keyword{datasets}
