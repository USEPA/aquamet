\name{bentTaxa_nla}
\alias{bentTaxa_nla}
\docType{data}
\title{Benthic Invertebrate Taxa for NLA}
\description{
  A dataset containing benthic invertebrate taxa and traits used in NLA metric calculation.
}
\usage{data(bentTaxa)}
\format{
  A data frame with 711 rows and the following 14 columns:
  \describe{
    \item{PUBLICATION_DATE}{date the dataset was published.}
    \item{TAXA_ID}{the taxa ID.}
    \item{TARGET_TAXON}{name of the taxon at the level used in NRSA.}
    \item{NON_TARGET}{Taxon considered non-target for NRSA but identified in sample (Y).}
    \item{PHYLUM}{taxonomic phylum name.}
    \item{CLASS}{taxonomic class name.}
    \item{ORDER}{taxonomic order name.}
    \item{FAMILY}{taxonomic family name.}
    \item{SUBFAMILY}{taxonomic subfamily name.}
    \item{TRIBE}{taxonomic tribe name.}
    \item{GENUS}{taxonomic genus name.}
    \item{FFG}{Functional feeding group trait (CF=collector-filterer,
    CG=collector-gatherer,PA=parasite,PI=Piercer,PR=predator,SC=scraper,
    SH=shredder).}
    \item{HABIT}{Behavioral habit trait (AT=attached,BU=burrower,CB=climber,
    CN=clinger,DV=diver,PK=planktonic,SK=skater,SP=sprawler,SW=swimmer).}
    \item{PTV}{Pollution Tolerant Value.}
  }
}
\examples{
data(bentTaxa_nla)
head(bentTaxa_nla)
}
\keyword{datasets}


