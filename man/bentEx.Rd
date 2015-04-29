\name{bentEx}
\alias{bentEx}
\docType{data}
\title{Example Benthic Invertebrate Counts}
\description{
  A dataset containing benthic invertebrate count data for use in invertMet() example.
}
\usage{data(bentEx)}
\format{
  A data frame with 92 rows and the following 6 columns:
  \describe{
    \item{UID}{unique site visit ID.}
    \item{SAMPLE_TYPE}{sampling method used to collect sample, BERWW-wadeable reachwide sampling.}
    \item{SAMPLE_CAT}{identifier of (P)rimary or (D)uplicate sample for visit and SAMPLE_TYPE.}
    \item{TAXA_ID}{the taxa ID number corresponding to bentTaxa list.}
    \item{TOTAL}{number of individuals counted in sample for a given taxon.}
    \item{IS_DISTINCT}{indicator variable for distinctness in sample (0/1).}
  }
}
\note{
  This is just a very small subset of benthic count data for NRSA 2008-2009 for example purposes only.
}
\examples{
data(bentEx)
head(bentEx)
}
\keyword{datasets}


