\name{fishMet}
\alias{fishMet}
\title{Calculate NRSA Fish Metrics}
\description{
  This function calculates the NRSA fish metrics.  It assumes input counts are
  organized so that a single column represents the unique sample, with each row
  representing the count for a single taxon in that sample.
}
\usage{
fishMet(inCts, inTaxa=NULL, sampID="UID", keepHerps=FALSE, dist="IS_DISTINCT",
   ct="TOTAL")
}
\arguments{
  \item{inCts}{a data frame with a variable containing counts (with data already
    summed by taxon and sample) and with a variable indicating distinctness of
    each taxon in a sample (coded 0/1).
  }
  \item{inTaxa}{a data frame containing the taxalist in wide format with
    variables for taxonomy as well as any other traits of interest.  If this
    argument is NULL, then the data frame is  assigned from the fishTaxa data
    frame that is included in the sysdata.rda file.  Assumes the following
    traits are included: FAM_OR_CLS, GENUS, HABITAT, HERP, MIGRATORY, REPROD,
    TEMP, TOL_VAL, TOLERANCE, TROPHIC, VEL, FINAL_NAME.  If traits have _GLEC or
    _NRSA at the end of the name, the name is truncated to the above trait names
    within the function.  The default is NULL.
  }
  \item{sampID}{a character vector indicating names of variable(s) necessary to
    identify a unique sample.  The default is "UID".
  }
  \item{keepHerps}{a logical value that indicates whether or not to include
    amphibians and reptiles in counts, which requires parameter "HERP" in the
    taxa list with values only for reptiles or amphibians and otherwise missing
    or blank.  The default is FALSE.
  }
  \item{dist}{name of the variable in the inAll data frame indicating
    distinctness.  The default is "IS_DISTINCT".
  }
  \item{ct}{name of the variable in the inAll data frame providing number of
    organisms counted for a taxon in a sample.  The default is "TOTAL".
  }
}
\value{
  Either a data frame when metric calculation is successful or a character
  string containing an error message when metric calculation is not successful.
  The first columns of the data frame contain the variables specified in the 
  sampID argument.  Subsequent columns are named for each metric and contain
  metric values.  A list of metrics is provided in the document named
  "Fish_Metric_Descriptions.pdf" included in the help directory for the package.
}
\author{
Karen Blocksom \email{Blocksom.Karen@epa.gov}\cr
Tom Kincaid \email{Kincaid.Tom@epa.gov}
}
\examples{
  \dontrun{
    data(fishEx)
    head(fishEx)
    # Use fish taxalist included in package
    head(fishTaxa)
    # Calculate metrics for fishIn, using the included taxalist for fish and UID
    # as the sample identifier
    # Exclude amphibians and reptiles from calculations 
    fishMetrics <- fishMet(inCts=fishEx, inTaxa=fishTaxa, sampID='UID',
       keepHerps=FALSE, dist='IS_DISTINCT', ct='FINAL_CT')
    head(fishMetrics)
    # Now leave out the variable NON_NATIVE, and we get a subset of the metrics
    fishEx.1 <- subset(fishEx,select=-NON_NATIVE)
    # If we don't specify a taxalist, the function uses the taxalist included in
    # the package, fishTaxa
    # In addition, the default sampID is 'UID', so if we leave the argument out,
    # it assumes 'UID'
    fishMetrics.1 <- fishMet(inCts=fishEx.1, keepHerps=FALSE,
       dist='IS_DISTINCT', ct='FINAL_CT')
    head(fishMetrics.1)
  }
}
\keyword{survey}
