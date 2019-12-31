\name{nlaPhabEx}
\alias{nlaPhabEx}
\docType{data}
\title{Example 2012 NLA Physical Habitat Dataset}
\description{
  A dataset containing raw NLA physical habitat data for use in function examples.
}
\usage{data(nlaPhabEx)}
\format{
  A data frame with 7449 observations on the following 4 variables.
  \describe{
    \item{SITE}{unique site visit ID.}
    \item{STATION}{reach transect label.}
    \item{PARAMETER}{character variable of parameter measured in field.}
    \item{VALUE}{value of measured parameter.}
  }
}
\details{
  These data are a small subset of the NLA 2012 physical habitat dataset for example purposes only.
}
\examples{
  data(nlaPhabEx)
  head(nlaPhabEx)
}
\keyword{datasets}
