% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nrsaSubstrateCharacterization.r
\name{nrsaSubstrateCharacterization}
\alias{nrsaSubstrateCharacterization}
\title{Calculate NRSA Substrate Characterization Metrics}
\usage{
nrsaSubstrateCharacterization(
  bBottomDom = NULL,
  bBottomSec = NULL,
  bShoreDom = NULL,
  bShoreSec = NULL,
  bSizeClass = NULL,
  wSizeClass = NULL,
  wMezzoSizeClass = NULL,
  isUnitTest = FALSE,
  argSavePath = NULL
)
}
\arguments{
\item{bBottomDom}{A data frame containing size class data for the dominant 
substrate class in the channel bottom, with the following columns:
\itemize{
       \item SITE integer or character specifying the site 
                  visit
       \item TRANSECT character value specifying the transect
                      for which the value was recorded.
       \item VALUE character values
}}

\item{bBottomSec}{A data frame containing size class data for the 
secondary substrate class in the channel bottom, with the following 
columns:
\itemize{
     \item SITE integer or character specifying the site 
                visit
     \item TRANSECT character value specifying the transect
                    for which the value was recorded.
     \item VALUE numeric or character values
}}

\item{bShoreDom}{A data frame containing size class data for 
the dominant substrate class at the channel shore, with the 
following columns:
\itemize{
     \item SITE integer or character specifying the site 
                visit
     \item TRANSECT character value specifying the transect
                    for which the value was recorded.
     \item VALUE numeric or character values
}}

\item{bShoreSec}{A data frame containing size class data for the 
secondary substrate class at the channel show, with the following 
columns:
\itemize{
     \item SITE integer or character specifying the site 
                visit
     \item TRANSECT character value specifying the transect
                    for which the value was recorded.
     \item VALUE numeric or character values
}}

\item{bSizeClass}{A data frame containing size class data collected 
in the thalweg of boatable channels with the following columns:
\itemize{
     \item SITE integer or character specifying the site 
                visit
     \item TRANSECT character value specifying the transect
                    for which the value was recorded.
     \item VALUE numeric or character values
}}

\item{wSizeClass}{A data frame containing size class data collected 
at transects of wadeable channels with the following columns:
\itemize{
       \item SITE integer or character specifying the site 
                  visit
       \item VALUE numeric or character values
}}

\item{wMezzoSizeClass}{A data frame containing size class data 
collected at mezzo-transects of wadeable channels with the following 
columns:
\itemize{
       \item SITE integer or character specifying the site 
                  visit
       \item VALUE numeric or character values
}}

\item{isUnitTest}{Logical argument to determine whether errors should be ignored.
Should only be used for running a unit test. Default value is FALSE.}

\item{argSavePath}{character string specifying the path to which data 
arguments are saved as csv files, or NULL if those files are not to be 
written.}
}
\value{
Either a data frame when metric calculation is successful 
or a character string containing an error message when metric 
calculation is not successful.  The data frame contains the following 
columns:
\itemize{
   \item SITE - universal ID value
   \item METRIC - metric name
   \item VALUE - metric value
} 
Wadeable metrics calculated include: 
d16, d50, d84, dgm, lsub2d16, lsub2d16inor, lsub2d25, lsub2d50,
  lsub2d50inor, lsub2d75, lsub2d84, lsub2d84inor, lsub2dmm, lsub2dmm_nor,
  lsub2iqr, lsubd2sd,lsubd2sd_nor, lsubd_sd, lsubd_sd_nor, lsub_d16, lsub_d25,
  lsub_d50, lsub_d75, lsub_d84, lsub_dmm, lsub_dmm_nor, lsub_iqr, n, n_nor,
  pct_bdrk, pct_bigr, pct_bl, pct_cb, pct_fn, pct_gc, pct_gf, pct_hp, pct_org,
  pct_om, pct_ot, pct_rc, pct_rr, pct_rs, pct_sa, pct_safn, pct_sb, pct_sfgf,
  pct_wd, pct_xb, sub2dmm_nor, subd2sd_nor, subd_sd_nor, sub_dmm_nor
  
Boatable metrics calculated include:
d16, d50, d84, dgm, LDCBF_G08, lsub2d16inor, lsub2d50inor, lsub2d84inor,
  lsubd_sd, lsub_d16, lsub_d25, lsub_d50, lsub_d75, lsub_d84, lsub_dmm,
  lsub_iqr, n,pct_bh, pct_bl, pct_cb, pct_dbbl, pct_dbcb, pct_dbfn, pct_dbgc,
  pct_dbgf, pct_dbhp, pct_dbom, pct_dbot, pct_dbrc, pct_dbrr, pct_dbrs,
  pct_dbsa, pct_dbsb, pct_dbwd, pct_dbxb, pct_dsbl, pct_dscb, pct_dsfn,
  pct_dsgc, pct_dsgf, pct_dshp, pct_dsom, pct_dsot, pct_dsrc, pct_dsrr,
  pct_dsrs, pct_dssa, pct_dssb, pct_dswd, pct_dsxb, pct_fn, pct_gr, pct_ot,
  pct_sa, pct_safn, pct_sbbl, pct_sbcb, pct_sbfn, pct_sbgc, pct_sbgf,
  pct_sbhp, pct_sbom, pct_sbot, pct_sbrc, pct_sbrr, pct_sbrs, pct_sbsa,
  pct_sbsb, pct_sbwd, pct_sbxb, pct_ssbl, pct_sscb, pct_ssfn, pct_ssgc,
  pct_ssgf, pct_sshp, pct_ssom, pct_ssot, pct_ssrc, pct_ssrr, pct_ssrs,
  pct_sssa, pct_sssb, pct_sswd, pct_ssxb,

Descriptions for all NRSA metrics can be found at:
\href{https://github.com/USEPA/aquamet/blob/master/inst/NRSA_physical_habitat_metrics_descriptions.pdf}{NLA_Physical_Habitat_Metric_Descriptions.pdf}.
}
\description{
This function calculates the substrate characterization
portion of the physical habitat metrics for National Rivers and 
Streams Assessment (NRSA) data.  The function requires data frames 
containing the channel crosssection, thalweg, and littoral data files.
}
\author{
Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
Tom Kincaid \email{Kincaid.Tom@epa.gov}
}
