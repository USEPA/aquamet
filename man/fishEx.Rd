\name{fishEx}
\alias{fishEx}
\docType{data}
\title{Example Fish Count Dataset}
\description{
  A dataset containing fish count data for use in fishMet() example.
}
\usage{data(fishEx)}
\format{
  A data frame with 22 observations on the following 6 variables.
  \describe{
    \item{UID}{unique site visit ID.}
    \item{TAXA_ID}{the taxa ID number corresponding to fishTaxa list.}
    \item{FINAL_CT}{number of fish captured for a given taxon.}
    \item{ANOM_CT}{number of anomalies observed for a given taxon.}
    \item{IS_DISTINCT}{indicator variable for distinctness in sample (0/1).}
    \item{NON_NATIVE}{Indicator variable of site-specific non-native status (Y/N).}
  }
}
\note{
  This is just a very small subset of fish count data for NRSA 2008-2009 for example purposes only.
}
\examples{
data(fishEx)
head(fishEx)
}
\keyword{datasets}
