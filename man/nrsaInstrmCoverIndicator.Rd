% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NRSAphab_indicators.r
\name{nrsaInstrmCoverIndicator}
\alias{nrsaInstrmCoverIndicator}
\title{Calculate NRSA Instream Cover physical habitat indicator}
\usage{
nrsaInstrmCoverIndicator(
  x,
  sampID = "UID",
  ecoreg,
  protocol,
  xfc_nat,
  lat,
  lon,
  slope,
  xwidth,
  elev,
  area
)
}
\arguments{
\item{x}{A data frame containing, at minimum, the variables 
specified in the arguments for \emph{sampID}, ecoreg, realm, lrbs, and 
any variables necessary to calculate indicator condition classes 
for ecoregions in x. The necessary variables by realm and ecoreg 
are (none indicates no additional variables are necessary):
\itemize{
\item CPL, BOATABLE none
\item CPL, WADEABLE none
\item NAP, BOATABLE lon, area, xwidth
\item NAP, WADEABLE xwidth
\item NPL, BOATABLE lat, lon, area
\item NPL, WADEABLE lon, elev, area
\item SAP, BOATABLE lat
\item SAP, WADEABLE lat, elev
\item SPL, BOATABLE lat, lon, area
\item SPL, WADEABLE lon, elev, area
\item TPL, BOATABLE lat, lon, area
\item TPL, WADEABLE lon, elev, area
\item UMW, BOATABLE lon
\item UMW, WADEABLE area, xwidth
\item WMT, BOATABLE xwidth
\item WMT, WADEABLE lat, lon, area
\item XER, BOATABLE elev, xwidth
\item XER, WADEABLE lon, slope
}}

\item{sampID}{A character vector containing the names of all 
variables in inMets that specify a unique sample. If not specified, 
the default is \emph{UID}}

\item{ecoreg}{A string with the name of the aggregated bioregion variable. 
Valid values that correspond to regions used in NRSA are
CPL, NAP, NPL, SAP, SPL, TPL, UMW, WMTNS, and XER.}

\item{protocol}{A string with the name of the variable indicating
physical habitat protocol used. The expected values of this variable are 
'BOATABLE' and 'WADEABLE'.}

\item{xfc_nat}{A string with the name of the variable for mean areal 
proportion natural fish cover. Typically, the version used from NRSA physical 
habitat metrics is XFC_NAT.}

\item{lat}{A string with the name of the variable for latitude, assumed to 
be in decimal degrees using NAD83.}

\item{lon}{A string with the name of the variable for longitude, assumed to
be in decimal degrees using NAD83.}

\item{slope}{A string with the name of the variable for percent stream slope. For 
NRSA, the variable used is XSLOPE.}

\item{xwidth}{A string with the name of the variable for mean wetted width
in meters. For NRSA, the variable used is XWIDTH.}

\item{elev}{A string with the name of the variable for elevation in meters. 
In NRSA, the variable used is ELEV_PT.}

\item{area}{A string with the name of the variable for watershed area in km2}
}
\value{
A data frame containing the variables in \emph{sampID} and 
INSTRMCVR_COND, the condition class for instream cover.
}
\description{
Using models based on NRSA 2008-9 and 2013-14 reference sites, 
this function determines the condition class by aggregated ecoregion and
sampling protocol used. The model used to determine the expected 
Log10(XFC_NAT + 0.01) (areal instream natural fish cover) depends on the 
ecoregion and protocol.
}
\author{
Karen Blocksom \email{Blocksom.Karen@epa.gov}
}
