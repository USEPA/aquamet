#' @export
#' 
#' @title Calculate NRSA Relative Bed Stability physical habitat indicator
#' 
#' @description 
#' 
#' @param x A data frame containing, at minimum, the variables 
#' specified in the arguments for sampID, ecoreg, realm, lrbs, and 
#' any variables necessary to calculate indicator condition classes 
#' for ecoregions in x. The necessary variables by realm and ecoreg 
#' are (none indicates no additional variables are necessary):
#' 
#' CPL, BOATABLE: none
#' CPL, WADEABLE: width, slope
#' NAP, BOATABLE: none
#' NAP, WADEABLE: area
#' NPL, BOATABLE: area, slope
#' NPL, WADEABLE: elev, width, slope
#' SAP, BOATABLE: none
#' SAP, WADEABLE: area
#' SPL, BOATABLE: area
#' SPL, WADEABLE: lat, area, slope
#' TPL, BOATABLE: area
#' TPL, WADEABLE: lat, lon, slope
#' UMW, BOATABLE: lat
#' UMW, WADEABLE: slope
#' WMT, BOATABLE: none
#' WMT, WADEABLE: width, slope
#' XER, BOATABLE: none
#' XER, WADEABLE: width
#' 
#' @param sampID A character vector containing the names of all 
#' variables in inMets that specify a unique sample. If not specified, 
#' the default is \emph{UID}.
#' 
#' @param ecoreg A string with the name of the aggregated ecoregion variable. 
#' Valid values that correspond to regions used in NRSA are
#' CPL, NAP, NPL, SAP, SPL, TPL, UMW, WMTNS, and XER.
#' 
#' @param realm A string with the name of the variable indicating realm or 
#' physical habitat protocol. The expected values of this variable are 
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
#' @param wight A string with the name of the variable for mean width. For NRSA,
#' the variable used is XWIDTH.
#' 
#' @return A data frame containing the variables in \emph{sampID} and BEDSED_COND, 
#' the condition class for relative bed stability.  
#' 
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#' @keywords survey
calcNRSA_RBS <- function(x, sampID='UID', ecoreg, realm, lrbs, lat, lon, area, elev, slope, width){
  
  # First rename input variables to match expected names, also calculate variations of several
  # for later use.
  names(x)[names(x)==lat] <- 'lat'
  names(x)[names(x)==lon] <- 'lon'
  names(x)[names(x)==area] <- 'area'
  names(x)[names(x)==elev] <- 'elev'
  names(x)[names(x)==ecoreg] <- 'ecoreg'
  names(x)[names(x)==slope] <- 'slope'
  names(x)[names(x)==width] <- 'width'
  names(x)[names(x)==realm] <- 'realm'
  names(x)[names(x)==lrbs] <- 'lrbs'
  
  dfIn <- subset(x,select=c(sampID,'ecoreg','realm','lrbs','lat','lon','area','elev','width','slope')) %>%
    plyr::mutate(l_slope = log10(slope+0.0001)
                 ,l_area = log10(area)
                 ,l_width = log10(width + 0.1)
                ) %>%
    reshape2::melt(id.vars=c(sampID,'ecoreg','realm','lrbs')) %>%
    plyr::mutate(value=as.numeric(value))
  
  expParam <- data.frame(ecoreg=rep(c('CPL','NAP','NPL','SAP','SPL','TPL','UMW','WMT','XER'),2)
                         ,realm=c(rep('BOATABLE',9),rep('WADEABLE',9))
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
  
  samps <- unique(subset(dfIn,select=c(sampID,'ecoreg','realm','lrbs')))

  expParam.base <- subset(expParam,select=c('ecoreg','realm','intercept','error','oe')) %>%
    merge(samps,by=c('ecoreg','realm'))
  
  expParam.mod <- subset(expParam,select=c('ecoreg','realm','lat','lon','elev','l_area','l_width','l_slope')) %>%
    reshape2::melt(id.vars=c('ecoreg','realm'),value.name='coef',na.rm=T) %>%
    merge(dfIn,by=c('ecoreg','realm','variable')) %>%
    ddply(c(sampID,'ecoreg','realm'),summarise,sumVal=sum(coef*value))
  
  dfIn.1 <- merge(expParam.base,expParam.mod,by=c(sampID,'ecoreg','realm'),all.x=T) %>%
    mutate(sumVal=ifelse(is.na(sumVal),0,sumVal))
    
  # For ecoregions that use dirty models
  dfOut <- plyr::mutate(dfIn.1, RfE_LRBS=sumVal + intercept
                        , compVal = ifelse(is.na(oe),lrbs,lrbs-RfE_LRBS)
                        , gf = ifelse(is.na(oe), RfE_LRBS-(0.67*error), oe-(0.67*error))
                        , fp = ifelse(is.na(oe), RfE_LRBS-(1.65*error), oe-(1.65*error))
                        , BEDSED_COND=ifelse(is.na(compVal),'Not Assessed'
                                           , ifelse(compVal>gf, 'Good'
                                                    , ifelse(compVal<=fp, 'Poor', 'Fair')))) %>%
    subset(select=c(sampID, 'BEDSED_COND'))
  
 return(dfOut)    
}


#' @export
#' 
#' @title Calculate NRSA Riparian Disturbance physical habitat indicator
#' 
#' @description 
#' 
#' @param inMets A data frame containing, at minimum, the variables 
#' specified in the arguments for sampID, ecoreg, 
#' 
#' @param sampID A character vector containing the names of all 
#' variables in inMets that specify a unique sample. If not specified, 
#' the default is \emph{UID}
#' @param ecoreg A string with the name of the aggregated bioregion variable. 
#' Valid values that correspond to regions used in NLA are
#' CPL, EHIGH, PLAINS, UMW, and WMTNS.
#' @return 
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#' @keywords survey
calcNRSA_RipDist <- function(x, sampID='UID', w1_hall){
  
  # First rename input variables to match expected names, also calculate variations of several
  # for later use.
  names(x)[names(x)==w1_hall] <- 'w1_hall'
 
  outMets <- mutate(x, w1_hall=as.numeric(w1_hall) 
                   ,RIPDIST_COND = ifelse(w1_hall<0.33, 'Low', ifelse(w1_hall>=0.33 & w1_hall<1.5, 'Moderate'
                                                                      , ifelse(w1_hall>=1.5, 'High', 'Not Assessed')))) %>%
    subset(select=c(sampID, 'RIPDIST_COND'))
  
  return(outMets)
  
}

#' @export
#' 
#' @title Calculate NRSA Instream Cover physical habitat indicator
#' 
#' @description 
#' 
#' @param x A data frame containing, at minimum, the variables 
#' specified in the arguments for sampID, ecoreg, 
#' 
#' 
#' 
#' @param sampID A character vector containing the names of all 
#' variables in inMets that specify a unique sample. If not specified, 
#' the default is \emph{UID}
#' @param ecoreg A string with the name of the aggregated bioregion variable. 
#' Valid values that correspond to regions used in NLA are
#' CPL, EHIGH, PLAINS, UMW, and WMTNS.
#' @return 
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#' @keywords survey
calcNRSA_InstrmCover <- function(x, sampID='UID', ecoreg, realm, lat, long, slope, xwidth, elev, area, xfc_nat){
  
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
  
  dfIn <- plyr::mutate(x, l_area=log10(area), l_width=log10(xwidth + 0.01), l_slope=log10(slope)
                       ,l_xfc_nat=log10(xfc_nat + 0.01))
  
  expParam <- data.frame(ecoreg=rep(c('CPL','NAP','NPL','SAP','SPL','TPL','UMW','WMT','XER'),2)
                         ,realm=c(rep('BOATABLE',9),rep('WADEABLE',9))
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
                         ,stringsAsFactors=F) %>%
    reshape2::melt(id.vars=c('ecoreg','intercept','resp','respAdj'),value.name='coef') 
  
  # Merge the two data frames and calculate the expected value for riparian veg complexity
  dfExp <- merge(dfIn.long,expParam,by=c('ecoreg','variable')) %>%
    dplyr::mutate(coef=ifelse(is.na(coef),0,coef)) %>%
    plyr::ddply(c(sampID,'ecoreg','resp','respAdj','intercept'),summarise,sumVal=sum(coef*value)) %>%
    dplyr::mutate(calcVal=sumVal + intercept) %>%
    dplyr::mutate(RfE1_LXFC_NAT=ifelse(resp=='log10',(10^calcVal)-respAdj,calcVal))
  
  
  out.1 <- merge(x, expParam, by=ecoreg) %>%
    plyr::mutate(RfE1_25 = RfE1_LXFC_NAT - (0.67*RfE1_RMSE_LXFC_NAT), RfE1_05_LXFC_NAT = RfE1_LXFC_NAT - (1.65*RfE1_RMSE_LXFC_NAT))
  
  inMets <- plyr::mutate(x, )
}