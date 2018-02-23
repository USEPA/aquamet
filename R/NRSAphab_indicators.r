#' @export
#' 
#' @title Assign NRSA Relative Bed Stability indicator condition class
#' 
#' @description Using models based on NRSA 2008-9 and 2013-14 reference sites, 
#' this function determines the condition class by aggregated ecoregion and
#' sampling protocol used. The model used to determine the expected Log10(RBS)
#' (LRBS) depends on the ecoregion and protocol. 
#' 
#' @param x A data frame containing, at minimum, the variables 
#' specified in the arguments for \emph{sampID}, ecoreg, realm, lrbs, and 
#' any variables necessary to calculate indicator condition classes 
#' for ecoregions in x. The necessary variables by realm and ecoreg 
#' are (none indicates no additional variables are necessary):
#' \itemize{
#' \item CPL, BOATABLE: none
#' \item CPL, WADEABLE: xwidth, slope
#' \item NAP, BOATABLE: none
#' \item NAP, WADEABLE: area
#' \item NPL, BOATABLE: area, slope
#' \item NPL, WADEABLE: elev, xwidth, slope
#' \item SAP, BOATABLE: none
#' \item SAP, WADEABLE: area
#' \item SPL, BOATABLE: area
#' \item SPL, WADEABLE: lat, area, slope
#' \item TPL, BOATABLE: area
#' \item TPL, WADEABLE: lat, lon, slope
#' \item UMW, BOATABLE: lat
#' \item UMW, WADEABLE: slope
#' \item WMT, BOATABLE: none
#' \item WMT, WADEABLE: xwidth, slope
#' \item XER, BOATABLE: none
#' \item XER, WADEABLE: xwidth
#' }
#' @param sampID A character vector containing the names of all 
#' variables in \emph{x} that specify a unique sample. If not specified, 
#' the default is \emph{UID}.
#' 
#' @param ecoreg A string with the name of the aggregated ecoregion variable. 
#' Valid values that correspond to regions used in NRSA are
#' CPL, NAP, NPL, SAP, SPL, TPL, UMW, WMTNS, and XER.
#' 
#' @param protocol A string with the name of the variable indicating  
#' physical habitat protocol used. The expected values of this variable are 
#' 'BOATABLE' and 'WADEABLE'.
#' 
#' @param lrbs A string with the name of the variable for log10(relative bed
#' stability). Typically, the version used from NRSA physical habitat metrics
#' is LRBS_G08.
#' 
#' @param lat A string with the name of the variable for latitude, assumed to 
#' be in decimal degrees using NAD83.
#' 
#' @param lon A string with the name of the variable for longitude, assumed to
#' be in decimal degrees using NAD83.
#' 
#' @param area A string with the name of the variable for watershed area in km2
#' 
#' @param elev A string with the name of the variable for elevation. In NRSA, 
#' the variable used is ELEV_PT.
#' 
#' @param slope A string with the name of the variable for stream slope. For 
#' NRSA, the variable used is XSLOPE. 
#' 
#' @param xwidth A string with the name of the variable for mean wetted width. 
#' For NRSA, the variable used is XWIDTH.
#' 
#' @return A data frame containing the variables in \emph{sampID} and BEDSED_COND, 
#' the condition class for relative bed stability.  
#' 
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#' @references Add Phil Kaufmann's technical report here
#' @keywords survey
nrsaRelBedStabilityIndicator <- function(x, sampID='UID', ecoreg, protocol, lrbs, lat, lon, area, elev, slope, xwidth){
  
  # First rename input variables to match expected names, also calculate variations of several
  # for later use.
  names(x)[names(x)==lat] <- 'lat'
  names(x)[names(x)==lon] <- 'lon'
  names(x)[names(x)==area] <- 'area'
  names(x)[names(x)==elev] <- 'elev'
  names(x)[names(x)==ecoreg] <- 'ecoreg'
  names(x)[names(x)==slope] <- 'slope'
  names(x)[names(x)==xwidth] <- 'width'
  names(x)[names(x)==protocol] <- 'protocol'
  names(x)[names(x)==lrbs] <- 'lrbs'
  
  x$lrbs <- as.numeric(x$lrbs)
  
  dfIn <- subset(x,select=c(sampID,'ecoreg','protocol','lrbs','lat','lon','area','elev','width','slope')) %>%
    plyr::mutate(l_slope = log10(slope+0.0001)
                 ,l_area = log10(area)
                 ,l_width = log10(width + 0.1)
                ) %>%
    reshape2::melt(id.vars=c(sampID,'ecoreg','protocol','lrbs')) %>%
    plyr::mutate(value=as.numeric(value))
  
  expParam <- data.frame(ecoreg=rep(c('CPL','NAP','NPL','SAP','SPL','TPL','UMW','WMT','XER'),2)
                         ,protocol=c(rep('BOATABLE',9),rep('WADEABLE',9))
                         ,intercept = c(c(-0.92405,-0.63226,-0.42002,0.44138,1.44046,1.44046,22.86206,0.36550,0.08641)
                                        ,c(-1.67044,-0.64678,-2.80718,-0.74349,0.89319,0.22205,-1.38974,-0.77810,-2.01510))
                         ,lat = c(c(NA,NA,NA,NA,NA,NA,-0.50298,NA,NA)
                                  ,c(NA,NA,NA,NA,-0.06565,0.04387,NA,NA,NA))
                         ,lon = c(rep(NA,9),rep(NA,5),0.03596,NA,NA,NA)
                         ,elev = c(rep(NA,9),c(NA,NA,0.00084015,rep(NA,6)))
                         ,l_area = c(c(NA,NA,0.44371,NA,-0.32356,-0.32356,NA,NA,NA)
                                     ,c(NA,0.32478,NA,0.48842,-0.09181,NA,NA,NA,NA))
                         ,l_width = c(c(NA,NA,NA,NA,NA,NA,NA,NA,NA)
                                      ,c(-0.49218,NA,0.64948,NA,NA,NA,NA,0.48616,1.33328))
                         ,l_slope = c(c(NA,NA,1.26686,NA,NA,NA,NA,NA,NA)
                                      ,c(-0.77290,NA,-0.70092,NA,-0.86897,-0.49057,-0.69289,-0.31541,NA))
                         ,error = c(c(1.33124,1.53888,0.51215,0.70357,1.14939,1.14939,1.25933,0.48996,0.98518)
                                  ,c(0.73642,0.52529,0.83941,0.69081,0.99030,0.93335,0.92535,0.42995,0.79439))
                         ,oe = c(rep(NA,2),0.15939,rep(NA,6)
                                 ,rep(NA,2),0.19752,NA,-0.00983,0.21704,rep(NA,3))
                         ,stringsAsFactors=F) 
  
  samps <- unique(subset(dfIn,select=c(sampID,'ecoreg','protocol','lrbs')))

  expParam.base <- subset(expParam,select=c('ecoreg','protocol','intercept','error','oe')) %>%
    merge(samps,by=c('ecoreg','protocol'),all.y=T)
  
  expParam.mod <- subset(expParam,select=c('ecoreg','protocol','lat','lon','elev','l_area','l_width','l_slope')) %>%
    reshape2::melt(id.vars=c('ecoreg','protocol'),value.name='coef',na.rm=T) %>%
    merge(dfIn,by=c('ecoreg','protocol','variable'),all.x=T) %>%
    ddply(c(sampID,'ecoreg','protocol'),summarise,sumVal=sum(coef*value)) %>%
    mutate(inputMsg=ifelse(is.na(sumVal),'Y','N'))
  
  dfIn.1 <- merge(expParam.base,expParam.mod,by=c(sampID,'ecoreg','protocol'),all.x=T) %>%
    mutate(sumVal=ifelse(is.na(sumVal),0,sumVal), inputMsg=ifelse(is.na(inputMsg),'N',inputMsg))
    
  # For ecoregions that use dirty models
  dfOut <- plyr::mutate(dfIn.1, RfE_LRBS=ifelse(inputMsg=='Y',NA,sumVal + intercept)
                        , compVal = ifelse(is.na(oe),lrbs,lrbs-RfE_LRBS)
                        , gf = ifelse(is.na(oe), RfE_LRBS-(0.67*error), oe-(0.67*error))
                        , fp = ifelse(is.na(oe), RfE_LRBS-(1.65*error), oe-(1.65*error))
                        , BEDSED_COND=ifelse(is.na(compVal)|is.na(RfE_LRBS),'Not Assessed'
                                           , ifelse(compVal>gf, 'Good'
                                                    , ifelse(compVal<=fp, 'Poor', 'Fair')))) %>%
    subset(select=c(sampID, 'BEDSED_COND'))
  
 return(dfOut)    
}


#' @export
#' 
#' @title Assign NRSA Riparian Disturbance indicator
#' condition class
#' 
#' @description Determine condition class based thresholds using 
#' the W1_HALL metric.
#' 
#' @param x A data frame containing, at minimum, the variables 
#' specified in the arguments for sampID and w1_hall 
#' 
#' @param sampID A character vector containing the names of all 
#' variables in inMets that specify a unique sample. If not specified, 
#' the default is \emph{UID}
#' 
#' @param w1_hall A string with the name of the variable representing all 
#' human disturbance. In NRSA, the name of the metric is W1_HALL.
#' 
#' @return A data frame containing the variables in \emph{sampID} and RIPDIST_COND, 
#' the condition class for riparian disturbance.
#' 
#' @references Add in from Phil Kaufmann later
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#' @keywords survey
nrsaRipDistIndicator <- function(x, sampID='UID', w1_hall){
  
  # First rename input variables to match expected names, also calculate variations of several
  # for later use.
  names(x)[names(x)==w1_hall] <- 'w1_hall'
 
  outMets <- mutate(x, w1_hall=as.numeric(w1_hall) 
                   ,RIPDIST_COND = ifelse(is.na(w1_hall),'Not Assessed'
                                          ,ifelse(w1_hall<0.33, 'Low'
                                                  , ifelse(w1_hall>=0.33 & w1_hall<1.5, 'Moderate','High')))) %>%
    subset(select=c(sampID, 'RIPDIST_COND'))
  
  return(outMets)
  
}

#' @export
#' 
#' @title Calculate NRSA Instream Cover physical habitat indicator
#' 
#' @description Using models based on NRSA 2008-9 and 2013-14 reference sites, 
#' this function determines the condition class by aggregated ecoregion and
#' sampling protocol used. The model used to determine the expected 
#' Log10(XFC_NAT + 0.01) (areal instream natural fish cover) depends on the 
#' ecoregion and protocol.
#' 
#' @param x A data frame containing, at minimum, the variables 
#' specified in the arguments for \emph{sampID}, ecoreg, realm, lrbs, and 
#' any variables necessary to calculate indicator condition classes 
#' for ecoregions in x. The necessary variables by realm and ecoreg 
#' are (none indicates no additional variables are necessary):
#' \itemize{
#' \item CPL, BOATABLE: none
#' \item CPL, WADEABLE: none
#' \item NAP, BOATABLE: lon, area, xwidth
#' \item NAP, WADEABLE: xwidth
#' \item NPL, BOATABLE: lat, lon, area
#' \item NPL, WADEABLE: lon, elev, area
#' \item SAP, BOATABLE: lat
#' \item SAP, WADEABLE: lat, elev
#' \item SPL, BOATABLE: lat, lon, area
#' \item SPL, WADEABLE: lon, elev, area
#' \item TPL, BOATABLE: lat, lon, area
#' \item TPL, WADEABLE: lon, elev, area
#' \item UMW, BOATABLE: lon
#' \item UMW, WADEABLE: area, xwidth
#' \item WMT, BOATABLE: xwidth
#' \item WMT, WADEABLE: lat, lon, area
#' \item XER, BOATABLE: elev, xwidth
#' \item XER, WADEABLE: lon, slope
#' } 
#' 
#' @param sampID A character vector containing the names of all 
#' variables in inMets that specify a unique sample. If not specified, 
#' the default is \emph{UID}
#' 
#' @param ecoreg A string with the name of the aggregated bioregion variable. 
#' Valid values that correspond to regions used in NLA are
#' CPL, EHIGH, PLAINS, UMW, and WMTNS.
#' 
#' @param protocol A string with the name of the variable indicating  
#' physical habitat protocol used. The expected values of this variable are 
#' 'BOATABLE' and 'WADEABLE'.
#' 
#' @param xfc_nat A string with the name of the variable for log10(mean areal 
#' natural fish cover + 0.01). Typically, the version used from NRSA physical 
#' habitat metrics is XFC_NAT.
#' 
#' @param lat A string with the name of the variable for latitude, assumed to 
#' be in decimal degrees using NAD83.
#' 
#' @param lon A string with the name of the variable for longitude, assumed to
#' be in decimal degrees using NAD83.
#' 
#' @param slope A string with the name of the variable for stream slope. For 
#' NRSA, the variable used is XSLOPE. 
#' 
#' @param xwidth A string with the name of the variable for mean wetted width. 
#' For NRSA, the variable used is XWIDTH.
#' 
#' @param elev A string with the name of the variable for elevation. In NRSA, 
#' the variable used is ELEV_PT.
#' 
#' @param area A string with the name of the variable for watershed area in km2
#' 
#' @return A data frame containing the variables in \emph{sampID} and 
#' INSTRMCVR_COND, the condition class for instream cover.
#' 
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#' @keywords survey
nrsaInstrmCoverIndicator <- function(x, sampID='UID', ecoreg, protocol, xfc_nat, lat, lon, slope, xwidth, elev, area){
  # First rename input variables to match expected names, also calculate variations of several
  # for later use.
  names(x)[names(x)==lat] <- 'lat'
  names(x)[names(x)==lon] <- 'lon'
  names(x)[names(x)==area] <- 'area'
  names(x)[names(x)==elev] <- 'elev'
  names(x)[names(x)==ecoreg] <- 'ecoreg'
  names(x)[names(x)==slope] <- 'slope'
  names(x)[names(x)==xwidth] <- 'xwidth'
  names(x)[names(x)==xfc_nat] <- 'xfc_nat'
  names(x)[names(x)==protocol] <- 'protocol'
  
  x$xfc_nat <- as.numeric(x$xfc_nat)
  
  dfIn <- subset(x,select=c(sampID,'ecoreg','protocol','xfc_nat','lat','lon','area','elev','xwidth','slope')) %>%
    plyr::mutate(l_area=log10(area), l_width=log10(xwidth + 0.1), l_slope=log10(slope+0.0001)
                       ,l_xfc_nat=log10(xfc_nat + 0.01), l_area=log10(area)) %>%
    reshape2::melt(id.vars=c(sampID,'ecoreg','protocol','l_xfc_nat')) %>%
    plyr::mutate(value=as.numeric(value))
  
  expParam <- data.frame(ecoreg=rep(c('CPL','NAP','NPL','SAP','SPL','TPL','UMW','WMT','XER'),2)
                         ,protocol=c(rep('BOATABLE',9),rep('WADEABLE',9))
                         ,intercept = c(c(-0.57048,-5.46962,2.42961,-3.54570,rep(2.42961,2),3.97716,-1.40552,-0.03292)
                                        ,c(-0.39218,-0.08246,-0.20615,-2.89088,rep(-0.20615,2),-0.48451,1.57993,0.96284))
                         ,lat = c(c(NA,NA,-0.02335,0.07646,-0.02335,-0.02335,NA,NA,NA)
                                  ,c(NA,NA,NA,0.06090,NA,NA,NA,0.01058,NA))
                         ,lon = c(c(NA,-0.06654,0.01564,NA,0.01564,0.01564,0.05232,NA,NA)
                                  ,c(NA,NA,0.00409,NA,0.00409,0.00409,NA,0.01895,0.01132))
                         ,elev = c(c(NA,NA,NA,NA,NA,NA,NA,NA,-0.00013276)
                                   ,c(NA,NA,0.00025270,0.00062631,0.00025270,0.00025270,NA,NA,NA))
                         ,l_area = c(c(NA,-0.46088,-0.11096,NA,-0.11096,-0.11096,NA,NA,NA)
                                     ,c(NA,NA,-0.08735,NA,-0.08735,-0.08735,0.17605,-0.08287,NA))
                         ,l_width = c(c(NA,0.92383,NA,NA,NA,NA,NA,0.48649,-0.42159)
                                      ,c(NA,-0.26338,NA,NA,NA,NA,-0.35844,NA,NA))
                         ,l_slope = c(rep(NA,17),0.18104)
                         ,error=c(c(0.23527,0.31921,0.32279,0.17528,0.32279,0.32279,0.31606,0.23044,0.31024)
                                  ,c(0.29820,0.28459,0.33531,0.31006,0.33531,0.33531,0.29010,0.21669,0.24231))
                         ,stringsAsFactors=F) 
  
  samps <- unique(subset(dfIn,select=c(sampID,'ecoreg','protocol','l_xfc_nat')))
  
  expParam.base <- subset(expParam,select=c('ecoreg','protocol','intercept','error')) %>%
    merge(samps,by=c('ecoreg','protocol'),all.y=T)

  expParam.mod <- subset(expParam,select=c('ecoreg','protocol','lat','lon','elev','l_area','l_width','l_slope')) %>%
    reshape2::melt(id.vars=c('ecoreg','protocol'),value.name='coef',na.rm=T) %>%
    merge(dfIn,by=c('ecoreg','protocol','variable'),all.x=T) %>%
    ddply(c(sampID,'ecoreg','protocol'),summarise,sumVal=sum(coef*value)) %>%
    mutate(inputMsg=ifelse(is.na(sumVal),'Y','N'))
  
  # Set missing sumVal to 0 to account for cases where the null model is used and sumVal is not calculated
  dfIn.1 <- merge(expParam.base,expParam.mod,by=c(sampID,'ecoreg','protocol'),all.x=T) %>%
      mutate(sumVal=ifelse(is.na(sumVal),0,sumVal), inputMsg=ifelse(is.na(inputMsg),'N',inputMsg))
  
  dfOut <- plyr::mutate(dfIn.1, RfE_xfc=ifelse(inputMsg=='Y',NA,sumVal + intercept)
                        , compVal = l_xfc_nat
                        , gf = RfE_xfc-(0.67*error)
                        , fp = RfE_xfc-(1.65*error)
                        , INSTRMCVR_COND=ifelse(is.na(compVal)|is.na(RfE_xfc),'Not Assessed'
                                             , ifelse(compVal>gf, 'Good'
                                                      , ifelse(compVal<=fp, 'Poor', 'Fair')))) %>%
    subset(select=c(sampID, 'INSTRMCVR_COND'))
  
  return(dfOut)    
}

#' @export
#' 
#' @title Assign NRSA Riparian Vegetation indicator condition class
#' 
#' @description Using models based on NRSA 2008-9 and 2013-14 reference sites, 
#' this function determines the condition class by aggregated ecoregion and
#' sampling protocol used. The model used to determine the expected 
#' Log10(XCMGW + 0.01) (Mean canopy + mid + ground riparian cover) depends on 
#' the ecoregion and protocol. 
#' 
#' @param x A data frame containing, at minimum, the variables 
#' specified in the arguments for \emph{sampID}, ecoreg, realm, lrbs, and 
#' any variables necessary to calculate indicator condition classes 
#' for ecoregions in x. The necessary variables by realm and ecoreg 
#' are (none indicates no additional variables are necessary):
#' \itemize{
#' \item CPL, BOATABLE: lon, area
#' \item CPL, WADEABLE: lon
#' \item NAP, BOATABLE: lat
#' \item NAP, WADEABLE: area, xwidth
#' \item NPL, BOATABLE: lat, lon
#' \item NPL, WADEABLE: lat, lon, slope
#' \item SAP, BOATABLE: none
#' \item SAP, WADEABLE: area, elev
#' \item SPL, BOATABLE: lat, lon
#' \item SPL, WADEABLE: lon, elev
#' \item TPL, BOATABLE: none
#' \item TPL, WADEABLE: lon, elev
#' \item UMW, BOATABLE: lat, area, slope, xwidth
#' \item UMW, WADEABLE: slope, xwidth
#' \item WMT, BOATABLE: none
#' \item WMT, WADEABLE: area, elev, slope
#' \item XER, BOATABLE: none
#' \item XER, WADEABLE: area, slope, width
#' }
#' @param sampID A character vector containing the names of all 
#' variables in \emph{x} that specify a unique sample. If not specified, 
#' the default is \emph{UID}.
#' 
#' @param ecoreg A string with the name of the aggregated ecoregion variable. 
#' Valid values that correspond to regions used in NRSA are
#' CPL, NAP, NPL, SAP, SPL, TPL, UMW, WMTNS, and XER.
#' 
#' @param protocol A string with the name of the variable indicating  
#' physical habitat protocol used. The expected values of this variable are 
#' 'BOATABLE' and 'WADEABLE'.
#' 
#' @param xcmgw A string with the name of the variable for log10(mean canopy + 
#' mid + ground riparian cover). Typically, the version used from NRSA 
#' physical habitat metrics is XCMGW.
#' 
#' @param lat A string with the name of the variable for latitude, assumed to 
#' be in decimal degrees using NAD83.
#' 
#' @param lon A string with the name of the variable for longitude, assumed to
#' be in decimal degrees using NAD83.
#' 
#' @param area A string with the name of the variable for watershed area in km2
#' 
#' @param elev A string with the name of the variable for elevation. In NRSA, 
#' the variable used is ELEV_PT.
#' 
#' @param slope A string with the name of the variable for stream slope. For 
#' NRSA, the variable used is XSLOPE. 
#' 
#' @param xwidth A string with the name of the variable for mean wetted width. 
#' For NRSA, the variable used is XWIDTH.
#' 
#' @return A data frame containing the variables in \emph{sampID} and RIPVEG_COND, 
#' the condition class for the Riparian Vegetation Cover indicator.  
#' 
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#' @references Add Phil Kaufmann's technical report here
#' @keywords survey
nrsaRiparianVegIndicator <- function(x, sampID='UID', ecoreg, protocol, xcmgw, lat, lon, area, elev, slope, xwidth){
  
  # First rename input variables to match expected names, also calculate variations of several
  # for later use.
  names(x)[names(x)==lat] <- 'lat'
  names(x)[names(x)==lon] <- 'lon'
  names(x)[names(x)==area] <- 'area'
  names(x)[names(x)==elev] <- 'elev'
  names(x)[names(x)==ecoreg] <- 'ecoreg'
  names(x)[names(x)==slope] <- 'slope'
  names(x)[names(x)==xwidth] <- 'xwidth'
  names(x)[names(x)==protocol] <- 'protocol'
  names(x)[names(x)==xcmgw] <- 'xcmgw'
  
  x[,c('lat','lon','area','elev','slope','xwidth','xcmgw')] <- lapply(x[,c('lat','lon','area','elev','slope','xwidth','xcmgw')],as.numeric)
  
  dfIn <- subset(x,select=c(sampID,'ecoreg','protocol','xcmgw','lat','lon','area','elev','xwidth','slope')) %>%
    plyr::mutate(l_slope = log10(slope+0.0001)
                 ,l_area = log10(area)
                 ,l_width = log10(xwidth + 0.1)
                 ,l_xcmgw = log10(xcmgw + 0.01)
    ) %>%
    reshape2::melt(id.vars=c(sampID,'ecoreg','protocol','l_xcmgw')) %>%
    plyr::mutate(value=as.numeric(value))
  # FIX VALUES IN THIS TABLE FOR XCMGW
  expParam <- data.frame(ecoreg=rep(c('CPL','NAP','NPL','SAP','SPL','TPL','UMW','WMT','XER'),2)
                         ,protocol=c(rep('BOATABLE',9),rep('WADEABLE',9))
                         ,intercept = c(c(0.83657,2.51398,1.80926,0.02698,1.80926,-0.08249,1.52755,-0.12272,-0.32820)
                                        ,c(-0.58185,0.21141,2.43249,-0.14633,1.25746,1.25746,-0.13511,0.24290,-0.21113))
                         ,lat = c(c(NA,-0.05498,-0.02245,NA,-0.02245,NA,-0.03762,NA,NA)
                                  ,c(NA,NA,-0.02325,NA,NA,NA,NA,NA,NA))
                         ,lon = c(c(0.00658,NA,0.01036,NA,0.01036,NA,NA,NA,NA)
                                  ,c(-0.00700,NA,0.01579,NA,0.01355,0.01355,NA,NA,NA))
                         ,elev = c(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),
                                   c(NA,NA,NA,0.00051106,-0.00024404,-0.00024404,NA,-0.00007192,NA))
                         ,l_area = c(c(-0.06020,NA,NA,NA,NA,NA,-0.33101,NA,NA)
                                     ,c(NA,0.09026,NA,0.04120,NA,NA,NA,-0.09638,-0.19122))
                         ,l_width = c(c(NA,NA,NA,NA,NA,NA,0.82145,NA,NA)
                                      ,c(NA,-0.30883,NA,NA,NA,NA,0.17937,NA,0.65498))
                         ,l_slope = c(c(NA,NA,NA,NA,NA,NA,0.17072,NA,NA)
                                      ,c(NA,NA,0.16417,NA,NA,NA,0.05069,-0.11520,0.19148))
                         ,error = c(c(0.11862,0.15628,0.32423,0.14138,0.32423,0.15980,0.37273,0.26191,0.15263)
                                    ,c(0.15238,0.12059,0.38555,0.14090,0.26692,0.26692,0.12999,0.15289,0.25328))
                         ,oe = c(c(NA,NA,-0.08047,NA,-0.08047,NA,NA,NA,NA)
                                 ,c(NA,NA,-0.13159,NA,NA,NA,NA,NA,NA))
                         ,stringsAsFactors=F) 
  
  samps <- unique(subset(dfIn,select=c(sampID,'ecoreg','protocol','l_xcmgw')))
  
  expParam.base <- subset(expParam,select=c('ecoreg','protocol','intercept','error','oe')) %>%
    merge(samps,by=c('ecoreg','protocol'),all.y=T)

  expParam.mod <- subset(expParam,select=c('ecoreg','protocol','lat','lon','elev','l_area','l_width','l_slope')) %>%
    reshape2::melt(id.vars=c('ecoreg','protocol'),value.name='coef',na.rm=T) %>%
    merge(dfIn,by=c('ecoreg','protocol','variable')) %>%
    plyr::ddply(c(sampID,'ecoreg','protocol'),summarise,sumVal=sum(coef*value)) %>%
    plyr::mutate(inputMsg=ifelse(is.na(sumVal),'Y','N'))
  
  dfIn.1 <- merge(expParam.base,expParam.mod,by=c(sampID,'ecoreg','protocol'),all.x=T) %>%
    plyr::mutate(sumVal=ifelse(is.na(sumVal),0,sumVal), inputMsg=ifelse(is.na(inputMsg),'N',inputMsg))

  # For ecoregions that use dirty models
  dfOut <- plyr::mutate(dfIn.1, RfE_ripveg=ifelse(inputMsg=='Y',NA,sumVal + intercept)
                        , compVal = ifelse(is.na(oe),l_xcmgw,l_xcmgw-RfE_ripveg)
                        , gf = ifelse(is.na(oe), RfE_ripveg-(0.67*error), oe-(0.67*error))
                        , fp = ifelse(is.na(oe), RfE_ripveg-(1.65*error), oe-(1.65*error))
                        , RIPVEG_COND=ifelse(is.na(compVal),'Not Assessed'
                                             , ifelse(compVal>gf, 'Good'
                                                      , ifelse(compVal<=fp, 'Poor', 'Fair')))) %>%
    subset(select=c(sampID, 'RIPVEG_COND'))
  
  return(dfOut)    
}
