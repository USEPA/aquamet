\name{bentTaxa_nrsa}
\alias{bentTaxa_nrsa}
\docType{data}
\title{Benthic Invertebrate Taxa for NRSA}
\description{
  A dataset containing benthic invertebrate taxa and traits used in NRSA metric calculation.
}
\usage{data(bentTaxa_nrsa)}
\format{
  A data frame with 1,334 rows and the following 21 columns:
  \describe{
    \item{PUBLICATION_DATE}{date the dataset was published.}
    \item{TAXA_ID}{the taxa ID.}
    \item{TARGET_TAXON}{name of the taxon at the level used in NRSA.}
    \item{NON_TARGET}{Taxon considered non-target for NRSA but identified in sample (Y).}
    \item{PHYLUM}{taxonomic phylum name.}
    \item{CLASS}{taxonomic class name.}
    \item{ORDER}{taxonomic order name.}
    \item{OTU_NRSA}{Operational taxonomic unit as used in NRSA O/E modeling.}
    \item{OTU_NRSA_CODE}{OTU code as used in NRSA analyses.}
    \item{FAMILY}{taxonomic family name.}
    \item{SUBFAMILY}{taxonomic subfamily name.}
    \item{TRIBE}{taxonomic tribe name.}
    \item{GENUS}{taxonomic genus name.}
    \item{FFG}{Functional feeding group trait (CF=collector-filterer,CG=collector-gatherer,PA=parasite,PI=Piercer,PR=predator,SC=scraper,SH=shredder).}
    \item{FFG_WSA}{Functional feeding group trait as used in WSA and NRSA metric calculations (CF=collector-filterer,CG=collector-gatherer,PA=parasite,PI=Piercer,PR=predator,SC=scraper,SH=shredder).}
    \item{HABIT}{Behavioral habit trait (AT=attached,BU=burrower,CB=climber,CN=clinger,DV=diver,PK=planktonic,SK=skater,SP=sprawler,SW=swimmer).}
    \item{HABIT_WSA}{Behavioral habit trait as used in WSA and NRSA calculations (AT=attached,BU=burrower,CB=climber,CN=clinger,DV=diver,PK=planktonic,SK=skater,SP=sprawler,SW=swimmer).}
    \item{PTV}{Pollution Tolerant Value.}
    \item{PTV_WSA}{Pollution Tolerance Values used in WSA and NRSA calculations.}
    \item{VOLTINISM}{Number of generations per year}
    \item{ITISTSN}{ITIS taxonomic serial number}
  }
}
\examples{
data(bentTaxa_nrsa)
head(bentTaxa_nrsa)
}
\keyword{datasets}


