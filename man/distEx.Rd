\name{distEx}
\alias{distEx}
\docType{data}
\title{Example Distinctness Input Benthic Dataset}
\description{
A dataset containing benthic invertebrate count data for use in assignDistinct() example.
}
\usage{data(distEx)}
\format{
  A data frame with 92 observations on the following 11 variables.
  \describe{
    \item{TAXA_ID}{the taxa ID number corresponding to fishTaxa list.}
    \item{UID}{unique site visit ID.}
    \item{SAMPLE_TYPE}{sampling method used to collect sample, BERWW-wadeable reachwide sampling.}
    \item{TARGET_TAXON}{name of the taxon at the level used in NRSA.}
    \item{TOTAL}{number of individuals counted in sample for a given taxon.}
    \item{PHYLUM}{taxonomic phylum name.}
    \item{CLASS}{taxonomic class name.}
    \item{ORDER}{taxonomic order name.}
    \item{FAMILY}{taxonomic family name.}
    \item{GENUS}{taxonomic genus name.}
    \item{SAMPID}{variable to uniquely identify samples, here the concatenation of UID and SAMPLE_TYPE}
  }
}
\note{
  This is just a very small subset of benthic count data for NRSA 2008-2009 for example purposes only.
}
\examples{
data(distEx)
head(distEx)
}
\keyword{datasets}
