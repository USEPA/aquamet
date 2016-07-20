\name{fishTaxa}
\alias{fishTaxa}
\docType{data}
\title{Fish Taxa}
\description{
  A dataset containing fish taxa.
}
\usage{data(fishTaxa)}
\format{
  A data frame with 734 rows and the following 15 columns:
  \describe{
    \item{PUBLICATION_DATE}{date the dataset was published.}
    \item{TAXA_ID}{the taxa ID.}
    \item{FAM_OR_CLS}{taxonomic family or class name.}
    \item{FINAL_NAME}{species common name.}
    \item{GENUS}{taxonomic genus name.}
    \item{HABITAT_GLEC}{HABITAT preference of taxon used in NRSA calculations.}
    \item{HERP}{Taxon is either reptile (R) or amphibian (A).}
    \item{MIGR_GLEC}{Migratory trait of taxon (Y/N).}
    \item{REPROD_GLEC}{Reproductive strategy of taxon (C=clean,coarse (lithophil),D=drifter,G=guarder,O=other).}
    \item{SPECIES}{taxonomic species name.}
    \item{TEMP_GLEC}{?.}
    \item{TOL_VAL}{Species tolerance values.}
    \item{TOLERANCE_NRSA}{Tolerance categories used in NRSA based on WEMAP, USGS, Region 7 (S=sensitive/intolerant,I=intermediate,T=tolerant).}
    \item{TROPHIC_GLEC}{Trophic guild (C=carnivore,I=invertivore,H=herbivore,O=omnivore).}
    \item{VEL_GLEC}{Velocity preference (R=rheophil,P=pool,O=other).}
  }
}
\note{
  Unless a data frame name is provided for argument inTaxa to the fishMet
  function, this dataset provides the inTaxa data frame.
}
\examples{
data(fishTaxa)
head(fishTaxa)
}
\keyword{datasets}


