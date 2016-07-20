\name{assignDistinct}
\alias{assignDistinct}
\title{Assign Distinct Taxa Name to Samples}
\description{
  This function evaluates taxonomic distinctness within samples and assigns an 
  indicator of distinctness (IS_DISTINCT) to each taxon (0 or 1) in each sample,
  based on distinctness within that sample.  Input data is expected to be count
  data and is assumed to include a single variable named SAMPID that uniquely 
  identifies samples. The input data frame also needs to include TARGET_TAXON
  as the field containing the taxon name, TAXA_ID as the taxonomic
  identification number, and the following taxonomic fields fully populated (as
  much as possible, depending on taxonomic level of TARGET_TAXON: PHYLUM, CLASS,
  ORDER, FAMILY, and GENUS.
  
  For the purposes of macroinvertebrates, the code assumes that taxa with 
  TARGET_TAXON values of 'THIENEMANNIMYIA GENUS GR.', 'CERATOPOGONINAE',
  or 'CRICOTOPUS/ORTHOCLADIUS' are at the lowest level and are assigned a
  value of 1 for IS_DISTINCT.
}
\usage{
assignDistinct(cc)
}
\arguments{
  \item{cc}{a data frame containing taxonomic fields for PHYLUM, CLASS, ORDER,
    FAMILY, and GENUS in addition to TAXA_ID and TARGET_TAXON.
  }
}
\value{
  Input data frame (cc) with IS_DISTINCT field added.
}
\author{
Karen Blocksom \email{Blocksom.Karen@epa.gov}\cr
}
\examples{
  \dontrun{
    data(distEx)
    head(distEx)
    # Assign distinctness to each taxon within samples
    distEx.1 <- assignDistinct(distEx)
    head(distEx.1)
  }
}
\keyword{survey}
