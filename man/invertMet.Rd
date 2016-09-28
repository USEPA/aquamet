\name{invertMet}
\alias{invertMet}
\title{Calculate NRSA Invertebrate Metrics}
\description{
  This function calculates the NRSA invertebrate metrics.  It assumes input
  counts are organized so that a single column represents the unique sample,
  with each row representing the count for a single taxon in that sample.
}
\usage{
invertMet(inCts, inTaxa=NULL, sampID="UID", dist="IS_DISTINCT", ct="TOTAL",
   taxlevel="WSA")
}
\arguments{
  \item{inCts}{a data frame with a variable containing counts (with data already
    summed by taxon and sample) and with a variable indicating distinctness of
    each taxon in a sample (coded 0/1).
  }
  \item{inTaxa}{a data frame containing the taxalist in wide format with
    variables for taxonomy as well as any other traits of interest.  If this
    argument is NULL, then the data frame is assigned from the bentTaxa data
    frame that is included in the sysdata.rda file.  The required fields in this
    data frame depend on the value of the field taxlevel. If taxlevel="WSA", the 
    following fields are required: PHYLUM, CLASS, ORDER, FAMILY, TRIBE,
    SUBFAMILY, GENUS, PTV_WSA, HABIT_WSA, FFG_WSA, and TARGET_TAXON.  Otherwise,
    the following fields are required: PHYLUM, CLASS, ORDER, FAMILY, TRIBE,
    SUBFAMILY, GENUS, PTV,HABIT, and FFG.  The default is NULL.
  }
  \item{sampID}{a character vector indicating names of variable(s) necessary to
    identify a unique sample.  The default is "UID".
  }
  \item{dist}{name of the variable in the inAll data frame indicating
    distinctness.  The default is "IS_DISTINCT".
  }
  \item{ct}{name of the variable in the inAll data frame providing number of
    organisms counted for a taxon in a sample.  The default is "TOTAL".
  }
  \item{taxlevel}{level of taxonomic aggregation used in metric calculations.
    This argument also is used as the value of METRIC_TAXA_ROOT in the output
    file.  The default is "WSA", which reflects the taxonomic resolution
    employed in the WSA-based MMI used for NRSA.  If the value is "WSA", further
    aggregation of input data and subsequent re-evaluation of distinctness
    occurs within the function.  Otherwise, taxonomic resolution is used as is.
  }
}
\value{
  Either a data frame when metric calculation is successful or a character
  string containing an error message when metric calculation is not successful.
  The first columns of the data frame contain the variables specified in the 
  sampID argument.  Subsequent columns are named for each metric and contain
  metric values.  A list of metrics is provided in the document named
  "Invert_Metric_Descriptions.pdf" included in the help directory for the
  package.
}
\author{
Karen Blocksom \email{Blocksom.Karen@epa.gov}\cr
Tom Kincaid \email{Kincaid.Tom@epa.gov}
}
\examples{
  \dontrun{
    data(bentEx)
    head(bentEx)
    # Use the included taxalist for benthics 
    head(bentTaxa)
    # Calculate metrics for bentIn, using the taxonomy in the count file as is
    bentMetrics <- invertMet(inCts=bentEx, inTaxa=bentTaxa,
       sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT'), dist='IS_DISTINCT',
       ct='TOTAL',taxlevel='LOWEST')
    head(bentMetrics)
  }
}
\keyword{survey}
