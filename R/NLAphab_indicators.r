#' @export
#' @title Calculate NLA Riparian Vegetation Complexity Indicator
#' 
#' @description Using metric values and various predictors as inputs, calculate 
#' indicator score for RVegQc3OE, the observed over expected value for
#' riparian vegetation complexity. Models to calculate expected values depend
#' on ecoregion. Use thresholds based on reference distributions by ecoregion to 
#' assign condition.
#' 
#' @param x Input data frame containing other variables as specified in
#' arguments
#' 
#' @param sampID Vector of variables necessary to identify a unique sample 
#' in \emph{x}.
#' 
#' @param lat Latitude of lake in decimal degrees (NAD 83 datum)
#' 
#' @param lon Longitude of lake in decimal degrees (NAD 83 datum)
#' 
#' @param lake_origin Lake origin, with valid values of 'NATURAL' 
#' or 'MAN_MADE'
#' 
#' @param area Area of lake or reservoir, in square kilometers
#' 
#' @param elev Lake surface elevation (meters above sea level)
#' 
#' @param ecoreg Lake ecoregion, based on aggregated Omernick ecoregions,
#' with valid values of CPL, NAP, NPL, SAP, SPL, TPL, UMW, WMT, XER.
#' 
#' @param rviWoody Index of total woody vegetation cover, calculated
#' by function \code{metsRiparianVegetationNLA()}.
#' 
#' @param rvfcGndInundated Fraction of ground cover inundated, 
#' calculated by function \code{metsRiparianVegetationNLA()}. 
#' 
#' @param rvfcUndWoody Fraction of understory cover as woody vegetation,
#' calculated by function \code{metsRiparianVegetationNLA()}
#' 
#' @param rvfcGndWoody Fraction of ground cover as woody vegetation,
#' calculated by function \code{metsRiparianVegetationNLA()}
#' 
#' @param rvfpCanBig Fraction of canopy as trees with dbh > 30 cm,
#' calculated by function \code{metsRiparianVegetationNLA()}
#' 
#' @param ssfcBedrock Fractional shoreline cover of bedrock, calculated
#' by function \code{metsShorelineSubstrate()}.
#' 
#' @param ssfcBoulders Fractional shoreline cover of boulders, calculated
#' by function \code{metsShorelineSubstrate()}.
#' 
#' @param hipwWalls Human influence weighted presence of walls, calculated 
#' by function \code{metsHumanImpact()}.
#' 
#' @return A data frame containing:
#' \itemize{
#' \item{sampID}{The variables in the argument \emph{sampID}}
#' 
#' \item{RVegQ}{Observed riparian vegetation complexity index}
#' 
#' \item{RVegQc3x15}{Expected riparian vegetation complexity index}
#' 
#' \item{RVegQc3OE}{Riparian vegetation complexity indicator 
#' value, O/E score}
#' 
#' \item{RVEG_COND}{Riparian vegetation complexity indicator 
#' condition class (Good/Fair/Poor/Not Assessed)} 
#' }
#' 
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#' @keywords survey

nlaRipVegCompIndicator <- function(x,sampID,lat,lon,lake_origin,area,elev,ecoreg
                                   ,rviWoody,rvfcGndInundated,rvfcUndWoody,rvfcGndWoody
                                   ,rvfpCanBig,ssfcBedrock,ssfcBoulders,hipwWalls){
  
  # First rename input variables to match expected names, also calculate variations of several
  # for later use.
  names(x)[names(x)==lat] <- 'lat'
  names(x)[names(x)==lon] <- 'lon'
  names(x)[names(x)==lake_origin] <- 'lake_origin'
  names(x)[names(x)==area] <- 'area'
  names(x)[names(x)==elev] <- 'elev'
  names(x)[names(x)==ecoreg] <- 'ecoreg'
  names(x)[names(x)==rviWoody] <- 'rviWoody'
  names(x)[names(x)==rvfcGndInundated] <- 'rvfcGndInundated'
  names(x)[names(x)==rvfcUndWoody] <- 'rvfcUndWoody'
  names(x)[names(x)==rvfcGndWoody] <- 'rvfcGndWoody'
  names(x)[names(x)==rvfpCanBig] <- 'rvfpCanBig'
  names(x)[names(x)==ssfcBedrock] <- 'ssfcBedrock'
  names(x)[names(x)==ssfcBoulders] <- 'ssfcBoulders'
  names(x)[names(x)==hipwWalls] <- 'hipwWalls'

  dfIn <- plyr::mutate(x, reservoir=ifelse(toupper(lake_origin) %in% c('MAN_MADE','MAN-MADE'),1,0)
    #                   ,elev=ifelse(elev<0,0,elev)
                        ,elevXlat=elev*lat
                        ,l_area=log10(area)
                        ,ssiNatBedBld=ifelse(hipwWalls>=0.10,0,(ssfcBedrock + ssfcBoulders)))
  
  # melt dfIn now in preparation for merging with the parameter df being created next
  dfIn.long <- reshape2::melt(dfIn,id.vars=c(sampID,'ecoreg')
                              ,measure.vars=c('lat','lon','l_area','reservoir','elev'
                                              ,'elevXlat'),na.rm=T)
  
  # Create data frame of regression parameters in order to calculate expected values
  expParam <- data.frame(ecoreg=c('NAP','SAP','CPL','UMW','NPL','SPL','TPL','WMT','XER')
                         ,resp = c('log10','log10','none','log10','log10','log10','log10','log10','log10')
                         ,respAdj = c(0.01,0.01,NA,0.01,0.01,0.01,0.01,0.01,0.01)
                         ,intercept = c(2.34593,0.24710,0.35438,-0.61298,-0.75460,-0.75460,-0.75460
                                        ,0.53572,0.44708)
                         ,lat = c(-0.03705,NA,NA,NA,NA,NA,NA,-0.01939,-0.02612)
                         ,lon = c(0.01723,0.01012,NA,NA,NA,NA,NA,NA,NA)
                         ,reservoir = c(-0.07954,NA,NA,NA,NA,NA,NA,-0.25957,NA)
                         ,elevXlat = c(NA,NA,-0.00003019,NA,NA,NA,NA,NA,NA)
                         ,elev = c(rep(NA,7),-0.00008953,-0.00013249)
                         ,l_area = c(rep(NA,7),0.07296,NA)
                         ,stringsAsFactors=F) %>%
    reshape2::melt(id.vars=c('ecoreg','intercept','resp','respAdj'),value.name='coef') 
  
  # Merge the two data frames and calculate the expected value for riparian veg complexity
  dfExp <- merge(dfIn.long,expParam,by=c('ecoreg','variable')) %>%
    dplyr::mutate(coef=ifelse(is.na(coef),0,coef)) %>%
    plyr::ddply(c(sampID,'ecoreg','resp','respAdj','intercept'),summarise,sumVal=sum(coef*value)) %>%
    dplyr::mutate(calcVal=sumVal + intercept) %>%
    dplyr::mutate(RVegQc3x15=ifelse(resp=='log10',(10^calcVal)-respAdj,calcVal))
  
  # Calculate the observed indicator value, depending on ecoregion
  dfObs <- dfIn %>%
    plyr::mutate(RVegQ = ifelse(ecoreg %in% c('NAP','SAP','UMW','CPL')
                          ,0.5*(rvfcGndInundated + (rviWoody/2.5))
                          ,ifelse(ecoreg %in% c('NPL','SPL','TPL')
                                  ,0.5*(rvfcGndInundated + ((rvfcUndWoody + rvfcGndWoody)/1.75))
                                  ,0.25*((rviWoody/2.5) + rvfpCanBig + rvfcGndInundated + ssiNatBedBld))))

  # Now merge the expected and observed values and calculate O/E  
  dfOE <- subset(dfObs,select=c(sampID,'ecoreg','RVegQ')) %>% 
    merge(dfExp[,c(sampID,'RVegQc3x15')], by=sampID) %>%
    plyr::mutate(RVegQc3OE = RVegQ/RVegQc3x15)
  
  # Create data frame containing O/E thresholds by ECO9 region
  tholds <- data.frame(ecoreg = c('NAP','SAP','UMW','CPL','NPL','SPL','TPL','WMT','XER')
                       ,gf = c(0.8092,0.8295,0.7835,0.6191,0.6091,0.6091,0.6091,0.7711,0.6312)
                       ,fp = c(0.5850,0.6244,0.5381,0.3449,0.2663,0.2663,0.2663,0.5162,0.3016)
                       ,stringsAsFactors=F)
  
  # Apply thresholds to O/E to assign condition classes
  dfOut <- merge(dfOE,tholds,by='ecoreg') %>%
  plyr::mutate(RVEG_COND=ifelse(is.na(RVegQc3OE),'Not Assessed',ifelse(RVegQc3OE>gf,'Good'
                        ,ifelse(RVegQc3OE>fp,'Fair','Poor')))) %>%
  subset(select=c(sampID,'RVegQ','RVegQc3x15','RVegQc3OE','RVEG_COND'))  
  
}


#' @export
#' @title Calculate NLA Littoral Vegetation Complexity Indicator
#' 
#' @description Using metric values and various predictors as inputs, calculate 
#' indicator score for LitCvrQc3OE, the observed over expected value for
#' littoral vegetation complexity. Models to calculate expected values depend
#' on ecoregion. Use thresholds based on reference distributions by ecoregion 
#' to assign condition.
#' 
#' @param x Input data frame containing other variables as specified in
#' arguments
#' 
#' @param sampID Vector of variables necessary to identify a unique sample 
#' in \emph{x}.
#' 
#' @param lat Latitude of lake in decimal degrees (NAD 83 datum)
#' 
#' @param lon Longitude of lake in decimal degrees (NAD 83 datum)
#' 
#' @param lake_origin Lake origin, with valid values of 'NATURAL' 
#' or 'MAN_MADE'
#' 
#' @param area Area of lake or reservoir, in square kilometers
#' 
#' @param elev Lake surface elevation (meters above sea level)
#' 
#' @param ecoreg Lake ecoregion, based on aggregated Omernick ecoregions,
#' with valid values of CPL, NAP, NPL, SAP, SPL, TPL, UMW, WMT, XER.
#' 
#' @param fciNatural Index of fish cover due to natural structures.
#' Calculated by the function \code{metsFishCoverNLA()}.
#' 
#' @param fcfcSnag Fractional fish cover consisting of snags.
#' Calculated by the function \code{metsFishCoverNLA()}.
#' 
#' @param amfcFloating Fractional cover of floating aquatic macrophytes.
#' Calculated by the function \code{metsFishCoverNLA()}.
#' 
#' @param amfcEmergent Fractional cover of emergent aquatic macrophytes.
#' Calculated by the function \code{metsFishCoverNLA()}.
#' 
#' @param fcfcBoulders Fraction of fish cover as boulders in 
#' littoral zone. Calculated by the function \code{metsFishCoverNLA()}.
#' 
#' @param fcfcBrush Fraction of fish cover as brush. 
#' Calculated by the function \code{metsFishCoverNLA()}.
#' 
#' @param fcfcLedges Fraction of fish cover as ledges.
#' Calculated by the function \code{metsFishCoverNLA()}.
#' 
#' @param fcfcLiveTrees Fractino of fish cover as live trees.
#' Calculated by the function \code{metsFishCoverNLA()}.
#' 
#' @param fcfcOverhang Fraction of fish cover as overhangs.
#' Calculated by the function \code{metsFishCoverNLA()}.
#' 
#' @return A data frame containing:
#' \itemize{
#' \item{sampID}{The variables in the argument \emph{sampID}}
#' 
#' \item{LitCvrQ}{Observed littoral vegetation complexity index}
#' 
#' \item{LitCvrQc3x15}{Expected littoral vegetation complexity index}
#' 
#' \item{LitCvrQc3OE}{Littoral vegetation complexity indicator 
#' value, O/E score}
#' 
#' \item{LITCVR_COND}{Littoral vegetation complexity indicator 
#' condition class (Good/Fair/Poor/Not Assessed)} 
#' }
#' 
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#' @keywords survey
#' 
#' 
nlaLitVegCompIndicator <- function(x,sampID,lat,lon,lake_origin,area,elev,ecoreg
                                   ,fciNatural,fcfcSnag,amfcFloating,amfcEmergent,fcfcBoulders
                                   ,fcfcBrush,fcfcLedges,fcfcLiveTrees,fcfcOverhang){
  
  # First rename input variables to match expected names, also calculate variations of several
  # for later use.
  names(x)[names(x)==lat] <- 'lat'
  names(x)[names(x)==lon] <- 'lon'
  names(x)[names(x)==lake_origin] <- 'lake_origin'
  names(x)[names(x)==area] <- 'area'
  names(x)[names(x)==elev] <- 'elev'
  names(x)[names(x)==ecoreg] <- 'ecoreg'
  names(x)[names(x)==fciNatural] <- 'fciNatural'
  names(x)[names(x)==fcfcSnag] <- 'fcfcSnag'
  names(x)[names(x)==amfcFloating] <- 'amfcFloating'
  names(x)[names(x)==amfcEmergent] <- 'amfcEmergent'
  names(x)[names(x)==fcfcBoulders] <- 'fcfcBoulders'
  names(x)[names(x)==fcfcBrush] <- 'fcfcBrush'
  names(x)[names(x)==fcfcLedges] <- 'fcfcLedges'
  names(x)[names(x)==fcfcLiveTrees] <- 'fcfcLiveTrees'
  names(x)[names(x)==fcfcOverhang] <- 'fcfcOverhang'
  
    dfIn <- plyr::mutate(x, reservoir = ifelse(toupper(lake_origin) %in% c('MAN_MADE','MAN-MADE'),1,0)
                 ,elev=ifelse(elev<0,0,elev)
                 ,elevXlon = elev*lon
                 ,l_elev = log10(elev+1)
                 ,l_area = log10(area)
                 ,amfcFltEmg= amfcFloating + amfcEmergent)
    
    
  # melt dfIn now in preparation for merging with the parameter df being created next
  dfIn.long <- reshape2::melt(dfIn,id.vars=c(sampID,'ecoreg')
                              ,measure.vars=c('lat','l_area','reservoir','l_elev'
                                              ,'elevXlon','elev'),na.rm=T)
  
  # Create data frame of regression parameters in order to calculate expected values
  expParam <- data.frame(ecoreg=c('NAP','SAP','CPL','UMW','NPL','SPL','TPL','WMT','XER')
                         ,resp = c('log10','log10','none','log10','log10','log10','log10','log10','log10')
                         ,respAdj = c(0.01,0.01,NA,0.01,0.01,0.01,0.01,0.01,0.01)
                         ,intercept = c(-0.8598,-0.66613,0.71804,-0.87559,-1.03378,-1.03378
                                        ,-1.03378,-1.10550,0.08706)
                         ,lat = c(NA,NA,NA,NA,NA,NA,NA,0.00407,-0.02849)
                         ,reservoir = c(NA,NA,NA,NA,0.10822,0.10822,0.10822,-0.18384,NA)
                         ,elevXlon = c(NA,-0.00000410,NA,NA,NA,NA,NA,NA,NA)
                         ,l_elev = c(NA,NA,-0.19300,NA,NA,NA,NA,NA,NA)
                         ,l_area = c(-0.08109,NA,NA,NA,NA,NA,NA,-0.05083,NA)
                         ,elev = c(rep(NA,7),-0.00004299,-0.00003932)
                         ,stringsAsFactors=F) %>%
    reshape2::melt(id.vars=c('ecoreg','intercept','resp','respAdj'),value.name='coef') 
  
  # Merge the two data frames and calculate the expected value for riparian veg complexity
  dfExp <- merge(dfIn.long,expParam,by=c('ecoreg','variable')) %>%
    plyr::mutate(coef=ifelse(is.na(coef),0,coef)) %>%
    plyr::ddply(c(sampID,'ecoreg','resp','respAdj','intercept'),summarise,sumVal=sum(coef*value)) %>%
    dplyr::mutate(calcVal=sumVal + intercept) %>%
    dplyr::mutate(LitCvrQc3x15=ifelse(resp=='log10',(10^calcVal)-0.01,calcVal))
  
  # Calculate the observed indicator value, depending on ecoregion
  dfObs <- dfIn %>%
    plyr::mutate(LitCvrQ = ifelse(ecoreg %in% c('CPL'), 0.5*(fciNatural + (fcfcSnag/0.2875))
                                  ,ifelse(ecoreg %in% c('SAP'), (1/3)*(fciNatural + (fcfcSnag/0.2875) + (amfcFltEmg/1.515))
                                          ,(1/3)*(((fcfcBoulders + fcfcBrush + fcfcLedges + fcfcLiveTrees + fcfcOverhang)/1.5) +
                                                    (fcfcSnag/0.2875) + (amfcFltEmg/1.515)))))
  
  # Now merge the expected and observed values and calculate O/E  
  dfOE <- subset(dfObs,select=c(sampID,'ecoreg','LitCvrQ')) %>% 
    merge(dfExp[,c(sampID,'LitCvrQc3x15')], by=sampID) %>%
    plyr::mutate(LitCvrQc3OE = LitCvrQ/LitCvrQc3x15)
  
  # Create data frame containing O/E thresholds by ECO9 region
  tholds <- data.frame(ecoreg = c('NAP','SAP','CPL','UMW','NPL','SPL','TPL','WMT','XER')
                       ,gf = c(0.6772,0.6575,0.6288,0.7152,0.5876,0.5876,0.5876,0.6385,0.5834)
                       ,fp = c(0.3594,0.3368,0.3704,0.4245,0.2624,0.2624,0.2624,0.3174,0.2486)
                       ,stringsAsFactors=F)

  # Apply thresholds to O/E to assign condition classes
  dfOut <- merge(dfOE,tholds,by='ecoreg') %>%
    plyr::mutate(LITCVR_COND=ifelse(is.na(LitCvrQc3OE),'Not Assessed',ifelse(LitCvrQc3OE>gf,'Good'
                                                                      ,ifelse(LitCvrQc3OE>fp,'Fair','Poor')))) %>%
    subset(select=c(sampID,'LitCvrQ','LitCvrQc3x15','LitCvrQc3OE','LITCVR_COND'))
  
  
}


#' @export
#' @title Calculate NLA Littoral-riparian Vegetation Complexity Indicator
#' 
#' @description Using metric values and various predictors as inputs, calculate 
#' indicator score for LitRipCvrQc3OE, the observed over expected value for
#' combined littoral and riparian vegetation complexity. Models to calculate 
#' expected values depend on ecoregion. Use thresholds based on reference 
#' distributions by ecoregion to assign condition.
#' 
#' @param x Input data frame containing other variables as specified in
#' arguments
#' 
#' @param sampID Vector of variables necessary to identify a unique sample 
#' in \emph{x}.
#' 
#' @param lat Latitude of lake in decimal degrees (NAD 83 datum)
#' 
#' @param lon Longitude of lake in decimal degrees (NAD 83 datum)
#' 
#' @param lake_origin Lake origin, with valid values of 'NATURAL' 
#' or 'MAN_MADE'
#' 
#' @param area Area of lake or reservoir, in square kilometers
#' 
#' @param elev Lake surface elevation (meters above sea level)
#' 
#' @param ecoreg Lake ecoregion, based on aggregated Omernick ecoregions,
#' with valid values of CPL, NAP, NPL, SAP, SPL, TPL, UMW, WMT, XER.
#' 
#' @param rvegq Observed riparian vegetation complexity index value, 
#' calculated by function \code{nlaRipVegCompIndicator()}.
#' 
#' @param litcvrq Observed littoral vegetation complexity index value,
#' calculated by function \code{nlaLitVegCompIndicator()}.
#' 
#' @return A data frame containing:
#' \itemize{
#' \item{sampID}{The variables in the argument \emph{sampID}}
#' 
#' \item{LitRipCvrQ}{Observed combined riparian and littoral vegetation
#' complexity index}
#' 
#' \item{LitRipCvrQc3x15}{Expected combined riparian and littoral
#' vegetation complexity index}
#' 
#' \item{LitRipCvrQc3OE}{Littoral and riparian vegetation complexity 
#' Observed/Expected (O/E) value}
#' 
#' \item{LITRIPCVR_COND}{Littoral and riparian vegetation complexity 
#' indicator condition class (Good/Fair/Poor/Not Assessed)} 
#' }
#' 
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#' @keywords survey
#' 
#' 
nlaLitRipVegCompIndicator <- function(x,sampID,lat,lon,lake_origin,area,elev,ecoreg
                                      ,rvegq,litcvrq){
  
  # First rename input variables to match expected names, also calculate variations of several
  # for later use.
  names(x)[names(x)==lat] <- 'lat'
  names(x)[names(x)==lon] <- 'lon'
  names(x)[names(x)==lake_origin] <- 'lake_origin'
  names(x)[names(x)==area] <- 'area'
  names(x)[names(x)==elev] <- 'elev'
  names(x)[names(x)==ecoreg] <- 'ecoreg'
  names(x)[names(x)==rvegq] <- 'rvegq'
  names(x)[names(x)==litcvrq] <- 'litcvrq'
  
  dfIn <- plyr::mutate(x, reservoir=ifelse(toupper(lake_origin) %in% c('MAN_MADE','MAN-MADE'),1,0)
                 ,elev=ifelse(elev<0,0,elev)
                 ,elevXlon=elev*lon
                 ,l_elev=log10(elev+1)
                 ,l_area=log10(area))
  
  # melt dfIn now in preparation for merging with the parameter df being created next
  dfIn.long <- reshape2::melt(dfIn,id.vars=c(sampID,'ecoreg')
                              ,measure.vars=c('lat','lon','l_area','reservoir','elev'
                                              ,'elevXlon','l_elev'),na.rm=T)
  
  # Create data frame of regression parameters in order to calculate expected values
  expParam <- data.frame(ecoreg=c('NAP','SAP','CPL','UMW','NPL','SPL','TPL','WMT','XER')
                         ,resp = c('log10','log10','none','log10','log10','log10','log10','log10','log10')
                         ,respAdj = c(0.01,0.01,NA,0.01,0.01,0.01,0.01,0.01,0.01)
                         ,intercept = c(2.41606,1.92708,0.59561,-0.70830,-0.82455,-0.82455,-0.82455,-0.08802,0.24931)
                         ,lat = c(-0.03964,NA,NA,NA,NA,NA,NA,-0.01015,-0.02529)
                         ,lon = c(0.01798,0.03141,NA,NA,NA,NA,NA,NA,NA)
                         ,reservoir = c(-0.08301,NA,NA,NA,NA,NA,NA,-0.22650,NA)
                         ,elevXlon = c(NA,-0.000115130,NA,NA,NA,NA,NA,NA,NA)
                         ,l_area = c(NA,NA,NA,NA,NA,NA,NA,0.04200,NA)
                         ,l_elev = c(NA,NA,-0.15322,NA,NA,NA,NA,NA,NA)
                         ,elev = c(NA,-0.00923,NA,NA,NA,NA,NA,-0.00006666,-0.00010090)
                         ,stringsAsFactors=F) %>%
    reshape2::melt(id.vars=c('ecoreg','intercept','resp','respAdj'),value.name='coef') 
  
  # Merge the two data frames and calculate the expected value for riparian veg complexity
  dfExp <- merge(dfIn.long,expParam,by=c('ecoreg','variable')) %>%
    plyr::mutate(coef=ifelse(is.na(coef),0,coef)) %>%
    plyr::ddply(c(sampID,'ecoreg','resp','respAdj','intercept'),summarise,sumVal=sum(coef*value)) %>%
    dplyr::mutate(calcVal=sumVal + intercept) %>%
    dplyr::mutate(LitRipCvrQc3x15=ifelse(resp=='log10',(10^calcVal)-0.01,calcVal))

  # Calculate the observed indicator value, depending on ecoregion
  dfObs <- dfIn %>%
    plyr::mutate(LitRipCvrQ = (rvegq + litcvrq)/2)
  
  # Now merge the expected and observed values and calculate O/E  
  dfOE <- subset(dfObs,select=c(sampID,'ecoreg','LitRipCvrQ')) %>% 
    merge(dfExp[,c(sampID,'LitRipCvrQc3x15')], by=sampID) %>%
    plyr::mutate(LitRipCvrQc3OE = LitRipCvrQ/LitRipCvrQc3x15)
 
  # Create data frame containing O/E thresholds by ECO9 region
  tholds <- data.frame(ecoreg = c('NAP','SAP','UMW','CPL','NPL','SPL','TPL','WMT','XER')
                       ,gf = c(0.7990,0.7999,0.8296,0.7580,0.6808,0.6808,0.6808,0.7922,0.6398)
                       ,fp = c(0.5672,0.5667,0.6252,0.5494,0.3703,0.3703,0.3703,0.5556,0.3159)
                       ,stringsAsFactors=F)
  
  # Apply thresholds to O/E to assign condition classes
  dfOut <- merge(dfOE,tholds,by='ecoreg') %>%
    plyr::mutate(LITRIPCVR_COND=ifelse(is.na(LitRipCvrQc3OE),'Not Assessed',ifelse(LitRipCvrQc3OE>gf,'Good'
                                                                          ,ifelse(LitRipCvrQc3OE>fp,'Fair','Poor')))) %>%
    subset(select=c(sampID,'LitRipCvrQ','LitRipCvrQc3x15','LitRipCvrQc3OE','LITRIPCVR_COND'))
  
  
}


#' @export
#' @title Calculate NLA Lakeshore Anthropogenic Disturbance Indicator
#' 
#' @description Using metric values as inputs, calculate 
#' indicator score for RDis_IX, the indicator of lakeshore anthropogenic disturbance. 
#' Use thresholds based on reference distributions across all ecoregions.
#' 
#' @param x Input data frame containing other variables as specified in
#' arguments
#' 
#' @param sampID Vector of variables necessary to identify a unique sample 
#' in \emph{x}.
#' 
#' @param hiiAg NLA physical habitat index of agricultural influences
#' based on weighted mean human influence metrics for crops, pasture, 
#' and orchards. Calculated by the function \code{metsHumanImpact()}. 
#' 
#' @param hiiNonAg NLA physical habitat index of non-agricultural influences
#' based on weighted mean human influence metrics for buildings, commercial,
#' docks, landfill, lawn, parks, powerlines, roads, and walls. Calculated
#' by the function \code{metsHumanImpact()}.
#' 
#' @param hifpAnyCirca Fractional cover of any human influence found within the 
#' plot area at any station. Calculated by the function \code{metsHumanImpact()}
#' 
#' @return A data frame containing:
#' \itemize{
#' \item{sampID}{The variables in the argument \emph{sampID}}
#' 
#' \item{RDis_IX}{Riparian anthropogenic disturbance index}
#' 
#' \item{RDIS_COND}{Riparian anthropogenic disturbance condition class
#'  (Low/Medium/High/Not Assessed)} 
#' }
#' 
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#' @keywords survey

nlaRipDistIndicator <- function(x,sampID,hiiAg,hiiNonAg,hifpAnyCirca){
  # First rename input variables to match expected names, also calculate variations of several
  # for later use.
  names(x)[names(x)==hiiAg] <- 'hiiAg'
  names(x)[names(x)==hiiNonAg] <- 'hiiNonAg'
  names(x)[names(x)==hifpAnyCirca] <- 'hifpAnyCirca'
  
  # Calculate RDis_IX based on input metrics
  dfObs <- plyr::mutate(x, RDis_IX = 0.5*(1 - (1/(1 + hiiNonAg + (5*hiiAg))) + hifpAnyCirca))
  
  # Assign condition class
  dfOut <- plyr::mutate(dfObs, RDIS_COND = ifelse(is.na(RDis_IX),'Not Assessed',
                                                  ifelse(RDis_IX > 0.75,'Poor',
                                                         ifelse(RDis_IX <= 0.2, 'Good','Fair')))) %>%
    subset(select=c(sampID,'RDis_IX','RDIS_COND'))
    
}  



#' @export
#' @title Calculate NLA Drawdown Indicator
#' 
#' @description Using metric values as inputs, calculate 
#' indicator score for lake drawdown, based on both horizontal
#' and vertical drawdown. Each is treated separately, and the 
#' two condition classes are then assessed to assign the 
#' larger of the two condition classes.
#' 
#' @param x Input data frame containing other variables as specified in
#' arguments
#' 
#' @param sampID Vector of variables necessary to identify a unique sample 
#' in \emph{x}.
#' 
#' @param bfxVertDD Vertical height to highwater mark in meters
#' 
#' @param bfxHorizDD Horizontal distance to highwater mark in meters
#' 
#' @param ecoreg Lake ecoregion, based on aggregated Omernick ecoregions,
#' with valid values of CPL, NAP, NPL, SAP, SPL, TPL, UMW, WMT, XER.

#' @param lake_origin Lake origin, with valid values of 'NATURAL' 
#' or 'MAN_MADE'

#' @return A data frame containing:
#' \itemize{
#' \item{sampID}{The variables in the argument \emph{sampID}}
#' 
#' \item{DRAWDOWN_COND}{Riparian anthropogenic disturbance condition class
#'  (Small/Medium/Large/Not Assessed)} 
#' }
#' 
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#' @keywords survey


nlaDrawdownIndicator <- function(x,sampID,bfxVertDD,bfxHorizDD,ecoreg,lake_origin){
  # First rename input variables to match expected names, also calculate variations of several
  # for later use.
  names(x)[names(x)==bfxVertDD] <- 'vertDD'
  names(x)[names(x)==bfxHorizDD] <- 'horizDD'
  names(x)[names(x)==ecoreg] <- 'ecoreg'
  names(x)[names(x)==lake_origin] <- 'lake_origin'

  tholdsVert <- data.frame(ecoreg = c('NAP','NAP','SAP','SAP','UMW','UMW','CPL','CPL','NPL'
                                      ,'NPL','SPL','SPL','TPL','TPL','WMT','WMT','XER','XER')
                           ,lake_origin=c(rep(c('NATURAL','MAN_MADE'),9))
                           ,p75V = c(0.122,0.122,0.200,0.200,0.111,0.111,0.03,0.03,0.06,0.36
                                     ,0.06,0.36,0.06,0.36,0.33,1.05,0.33,1.05)
                           ,p95V = c(0.47,0.47,0.76,0.76,0.500,0.500,1.00,1.00,0.28,1.20
                                     ,0.28,1.20,0.28,1.20,1.00,2.00,1.00,2.00)
                           ,stringsAsFactors=F)
  
  tholdsHoriz <- data.frame(ecoreg = c('NAP','NAP','SAP','SAP','UMW','UMW','CPL','CPL','NPL'
                                       ,'NPL','SPL','SPL','TPL','TPL','WMT','WMT','XER','XER')
                            ,lake_origin=c(rep(c('NATURAL','MAN_MADE'),9))
                            ,p75H = c(0.25,0.25,0.200,0.200,0.510,0.510,0.10,0.10,0.10,1.550
                                      ,0.10,1.550,0.10,1.550,0.64,4.39,0.64,4.39)
                            ,p95H = c(1.65,1.65,2.15,2.15,2.65,2.65,4.0,4.0,2.85,14.63
                                      ,2.85,14.63,2.85,14.63,9.43,11.37,9.43,11.37)
                            ,stringsAsFactors=F)

  dfvert <- merge(x, tholdsVert, by=c('ecoreg','lake_origin')) %>%
    merge(tholdsHoriz, by=c('ecoreg','lake_origin')) %>%
    plyr::mutate(vertDD_cond = ifelse(is.na(vertDD),'Not Assessed', ifelse(vertDD <= p75V
                            , 'Small', ifelse(vertDD > p95V, 'Large', 'Medium')))
                 ,horizDD_cond = ifelse(is.na(horizDD), 'Not Assessed'
                                        , ifelse(horizDD <= p75H, 'Small'
                                                 ,ifelse(horizDD > p95H, 'Large', 'Medium')))) %>%
    plyr::mutate(DRAWDOWN_COND = ifelse(vertDD_cond=='Large'|horizDD_cond=='Large', 'Large'
                                  , ifelse(vertDD_cond=='Medium'|horizDD_cond=='Medium', 'Medium'
                                  , ifelse(vertDD_cond=='Small'|horizDD_cond=='Small', 'Small'
                                           , 'Not Assessed')))) %>% 
    subset(select = c(sampID, 'horizDD_cond','vertDD_cond','DRAWDOWN_COND'))
  
  
  
}  