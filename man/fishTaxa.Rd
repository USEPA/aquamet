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
    \item{FAMILY}{family name.}
    \item{FINAL_NAME}{species common name.}
    \item{GENUS}{taxonomic genus name.}
    \item{HABITAT_NRSA}{HABITAT preference of taxon used in NRSA calculations.}
    \item{HERP}{Taxon is either reptile (R) or amphibian (A).}
    \item{ITISTSN}{ITIS taxonomic serial number.}
    \item{MIGR_NRSA}{Migratory trait of taxon (Y/N).}
    \item{REPROD_NRSA}{Reproductive strategy of taxon (C=clean,coarse (lithophil),D=drifter,G=guarder,O=other).}
    \item{TEMP_NRSA}{Temperature preference trait used in NRSA (WM=warm, CL=cool, CD=cold).}
    \item{TOL_VAL_EMAPW}{Species tolerance values based on EMAP-West data.}
    \item{TOLERANCE_NRSA}{Tolerance categories used in NRSA based on WEMAP, USGS, Region 7 (S=sensitive/intolerant,I=intermediate,T=tolerant).}
    \item{TROPHIC_NRSA}{Trophic guild as used in NRSA (C=carnivore,I=invertivore,H=herbivore,O=omnivore).}
    \item{VEL_NRSA}{Velocity preference as used in NRSA (R=rheophil,P=pool,O=other).}
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


