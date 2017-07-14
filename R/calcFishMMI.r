#' @export
#' @title Calculate NRSA Fish MMI
#' 
#' @description This is a function that calculates 
#' the fish MMI as used for the National Rivers and Streams 
#' Assessment, based on inputs of the appropriate metrics.
#' 
#' @param inMets A data frame containing, at minimum, the variables 
#' specified in the arguments for sampID, ecoreg, lwsarea and the metrics 
#' necessary for calculation of MMI by region. If values for more than 
#' the necessary metrics are included in the input data frame, the 
#' unnecessary  metrics will be ignored for each given site.
#' 
#' The necessary metrics, by aggregate ecoregion, are:
#' 
#'  CPL: ALIENPIND, RBCATONTAX, LOTPIND, INTLMIGRPTAX, LITHPIND, NAT_TOTLNTAX, 
#'      TOLRNTAX, INVPTAX
#'      
#'  NAP: ALIENNTAX, SALMNTAX, NAT_RHEOPIND, INTLMIGRPIND, LITHPTAX,
#'      NTOLPTAX, TOLRNTAX, INVNTAX
#'      
#'  NPL: ALIENNTAX, NAT_CYPRPIND, LOTNTAX, MIGRNTAX, LITHPIND, NTOLPTAX,
#'      NAT_INTLPIND, NAT_CARNNTAX 
#'      
#'  SAP: NAT_PTAX, NAT_CENTNTAX, NAT_NTOLBENTPTAX, NAT_MIGRNTAX,
#'      NAT_LITHPIND, NTOLPTAX, TOLRPTAX, INVPIND
#'      
#'  SPL: NAT_PIND, CYPRPTAX, RHEOPIND, NAT_MIGRPTAX, LITHNTAX,
#'      NAT_NTOLNTAX, TOLRNTAX, HERBPTAX
#'  
#'  TPL: ALIENNTAX, NAT_ICTAPIND, RHEONTAX, INTLMIGRNTAX, LITHPIND,
#'      NAT_NTOLNTAX, INTLPTAX, CARNNTAX
#'      
#'  UMW: NAT_PTAX, CYPRNTAX, INTLLOTNTAX, INTLMIGRPTAX, LITHPIND,
#'      NTOLNTAX, TOLRNTAX, INTLINVPTAX
#'      
#'  WMT: NAT_PIND, NAT_CATOPIND, INTLLOTPTAX, NAT_MIGRPTAX, LITHPTAX,
#'      NAT_TOTLNTAX, TOLRNTAX, NAT_HERBPTAX
#'      
#'  XER: NAT_PIND, CENTPTAX, RHEOPIND, MIGRPTAX, LITHNTAX, NTOLPTAX,
#'      TOLRNTAX, BENTINVPTAX
#' 
#' @param sampID A character vector containing the names of all 
#' variables in inMets that specify a unique sample. If not specified, 
#' the default is \emph{UID}
#' @param ecoreg A string with the name of the ecoregion variable. 
#' Valid values that correspond to regions used in NRSA are
#' CPL, NAP, NPL, SAP, sPL, TPL, UMW, WMT, and XER.
#' @param lwsarea Numeric values representing the log10 of 
#' watershed area in square km.
#' 
#' @return A data frame containing the variables in sampID, watershed-
#' adjusted metrics, scored metrics, and the fish MMI for each site. 
#' Each of these components will be in separate columns, but values 
#' will only be provided for the metrics in the MMI for the specific 
#' region of each site, with all other columns being missing for 
#' that site. 
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#' @keywords survey
calcFishMMI <- function(inMets, sampID='UID', ecoreg='ECOREG', lwsarea='LWSAREA'){
  
  necTraits <- c(sampID,ecoreg,lwsarea)
  if(any(necTraits %nin% names(inMets))){
    msgTraits <- which(necTraits %nin% names(inMets))
    print(paste("Some of the traits are missing from the input data frame. The following are required for metric calculations to run:"
                , necTraits[msgTraits]))
    return(NULL)
  }
  
  # Rename variables 
  names(inMets)[names(inMets)==ecoreg] <- 'ECO9'
  names(inMets)[names(inMets)==lwsarea] <- 'LWSAREA'

  # Combine all values in sampID into one sampID in df
  for(i in 1:length(sampID)){
    if(i==1) inMets$SAMPID <- inMets[,sampID[i]]
    else inMets$SAMPID <- paste(inMets$SAMPID,inMets[,sampID[i]],sep='.')
  }
  samples <- unique(inMets[,c('SAMPID',sampID)])
    
  # Check to make sure ecoregion variable is included in the input data frame
  ecoCk <- unique(inMets$ECO9)
  ecos <- c('CPL','NAP','NPL','SAP','SPL','TPL','UMW','WMT','XER')
  if(any(ecoCk %nin% ecos)){
    msgEco <- which(ecoCk %nin% ecos)
    print(paste("These ecoregions are not valid: "
                ,paste(ecoCk[msgEco],collapse=',')))
    return(NULL)
  }
  
 # We now need to make sure that all of the necessary metrics are included in the input dataset  
  metnames <- data.frame(ECO9=c(rep('CPL',8),rep('NAP',8),rep('NPL',8),rep('SAP',8),rep('SPL',8)
                                          ,rep('TPL',8),rep('UMW',8),rep('WMT',8),rep('XER',8))
                         ,PARAMETER=c(c('ALIENPIND','RBCATONTAX','LOTPIND','INTLMIGRPTAX','LITHPIND','NAT_TOTLNTAX','TOLRNTAX'
                                        ,'INVPTAX')
                                      ,c('ALIENNTAX','SALMNTAX','NAT_RHEOPIND','INTLMIGRPIND','LITHPTAX','NTOLPTAX','TOLRNTAX'
                                         ,'INVNTAX')
                                      ,c('ALIENNTAX','NAT_CYPRPIND','LOTNTAX','MIGRNTAX','LITHPIND','NTOLPTAX','NAT_INTLPIND'
                                         ,'NAT_CARNNTAX')
                                      ,c('NAT_PTAX','NAT_CENTNTAX','NAT_NTOLBENTPTAX','NAT_MIGRNTAX','NAT_LITHPIND','NTOLPTAX'
                                         ,'TOLRPTAX','INVPIND')
                                      ,c('NAT_PIND','CYPRPTAX','RHEOPIND','NAT_MIGRPTAX','LITHNTAX','NAT_NTOLNTAX','TOLRNTAX'
                                         ,'HERBPTAX')
                                      ,c('ALIENNTAX','NAT_ICTAPIND','RHEONTAX','INTLMIGRNTAX','LITHPIND','NAT_NTOLNTAX','INTLPTAX'
                                         ,'CARNNTAX')
                                      ,c('NAT_PTAX','CYPRNTAX','INTLLOTNTAX','INTLMIGRPTAX','LITHPIND','NTOLNTAX','TOLRNTAX'
                                         ,'INTLINVPTAX')
                                      ,c('NAT_PIND','NAT_CATOPIND','INTLLOTPTAX','NAT_MIGRPTAX','LITHPTAX','NAT_TOTLNTAX'
                                         ,'TOLRNTAX','NAT_HERBPTAX')
                                      ,c('NAT_PIND','CENTPTAX','RHEOPIND','MIGRPTAX','LITHNTAX','NTOLPTAX','TOLRNTAX'
                                         ,'BENTINVPTAX'))
                         ,stringsAsFactors=F)
 
  
 matchMets <- reshape2::melt(inMets,id.vars=c('SAMPID','ECO9','LWSAREA'),measure.vars=names(inMets)[names(inMets) %in% unique(metnames$PARAMETER)]
                             ,variable.name='PARAMETER',value.name='RESULT',na.rm=T) %>%
   merge(metnames,by=c('PARAMETER','ECO9')) %>%
   mutate(PARAMETER=as.character(PARAMETER))
 
 # Run a check to make sure there are exactly 8 rows per sites in the matched dataset
 numMets <- as.data.frame(table(SAMPID=matchMets$SAMPID)) %>% subset(Freq<8)
 if(nrow(numMets)>0){
   return(print(paste("Missing metrics values for these samples: ",paste(numMets$SAMPID,collapse=','),
                      ". Check input data frame against required metric list.",sep='')))
 }
 
 
  # Metrics requiring watershed adjustment - intercepts and slopes by metric and ECO9
  wsMets <- data.frame(ECO9=c(rep('CPL',5),rep('NAP',3),rep('NPL',6),rep('SAP',3),rep('SPL',2)
                                        ,rep('TPL',4),rep('UMW',3),rep('WMT',3),rep('XER',4))
                       ,PARAMETER=c(c('ALIENPIND','LOTPIND','LITHPIND','NAT_TOTLNTAX','TOLRNTAX')
                                    ,c('LITHPTAX','NTOLPTAX','TOLRNTAX')
                                    ,c('LOTNTAX','MIGRNTAX','LITHPIND','NTOLPTAX','NAT_INTLPIND','NAT_CARNNTAX')
                                    ,c('NAT_CENTNTAX','NAT_LITHPIND','INVPIND')
                                    ,c('CYPRPTAX','NAT_MIGRPTAX')
                                    ,c('ALIENNTAX','NAT_ICTAPIND','NAT_NTOLNTAX','CARNNTAX')
                                    ,c('INTLLOTNTAX','NTOLNTAX','TOLRNTAX')
                                    ,c('INTLLOTPTAX','NAT_MIGRPTAX','NAT_TOTLNTAX')
                                    ,c('MIGRPTAX','LITHNTAX','TOLRNTAX','BENTINVPTAX'))
                       ,int=c(c(-0.219734,83.680193,90.591166,10.929299,1.831029)
                              ,c(91.493806,83.244125,-0.072385)
                              ,c(0.878392,0.438798,81.213041,121.656224,84.560234,-1.380617)
                              ,c(-0.017051,85.390153,26.04262)
                              ,c(45.705777,-0.604356)
                              ,c(-0.22423,-0.189542,1.946393,-0.005878)
                              ,c(1.09723,2.216995,0.398305)
                              ,c(110.962575,90.991326,0.748128)
                              ,c(93.412006,-0.265844,-0.142977,-5.705387))
                       ,slope=c(c(0.178533,-5.644243,-21.2575,2.873952,1.559498)
                                ,c(-9.389536,-5.594874,1.002947)
                                ,c(1.759049,0.39651,-13.064343,-18.471843,-21.788603,0.928968)
                                ,c(0.776488,-10.818128,11.423482)
                                ,c(-9.448293,0.532868)
                                ,c(0.200411,0.816572,2.107837,1.292597)
                                ,c(0.659379,2.870941,1.755202)
                                ,c(-21.540681,-15.318296,1.104128)
                                ,c(-20.33135,1.369981,0.094138,9.987192))
                       ,stringsAsFactors=F)
 
 # Now merge fish data with watershed regression info and adjust necessary metrics
 fish.ws <- merge(matchMets,wsMets,by=c('ECO9','PARAMETER'),all.x=T) %>%
   plyr::mutate(RESULT=as.numeric(RESULT)) %>%
   plyr::mutate(RESULT_WS=ifelse(is.na(int),NA,int+slope*LWSAREA),RESULT_NEW=ifelse(is.na(slope),RESULT,round(RESULT-RESULT_WS,3))
          ,PARAMETER=ifelse(is.na(slope),PARAMETER,paste(PARAMETER,'WS',sep='_'))) %>%
   dplyr::select(-RESULT,-RESULT_WS) %>% plyr::rename(c('RESULT_NEW'='RESULT'))
  
  
  cfVal <- data.frame(ECO9=c(rep('CPL',8),rep('NAP',8),rep('NPL',8),rep('SAP',8),rep('SPL',8)
                                       ,rep('TPL',8),rep('UMW',8),rep('WMT',8),rep('XER',8))
                      ,PARAMETER=c(c('ALIENPIND_WS','RBCATONTAX','LOTPIND_WS','INTLMIGRPTAX','LITHPIND_WS','NAT_TOTLNTAX_WS','TOLRNTAX_WS'
                                     ,'INVPTAX')
                                   ,c('ALIENNTAX','SALMNTAX','NAT_RHEOPIND','INTLMIGRPIND','LITHPTAX_WS','NTOLPTAX_WS','TOLRNTAX_WS'
                                      ,'INVNTAX')
                                   ,c('ALIENNTAX','NAT_CYPRPIND','LOTNTAX_WS','MIGRNTAX_WS','LITHPIND_WS','NTOLPTAX_WS'
                                      ,'NAT_INTLPIND_WS','NAT_CARNNTAX_WS')
                                   ,c('NAT_PTAX','NAT_CENTNTAX_WS','NAT_NTOLBENTPTAX','NAT_MIGRNTAX','NAT_LITHPIND_WS','NTOLPTAX'
                                      ,'TOLRPTAX','INVPIND_WS')
                                   ,c('NAT_PIND','CYPRPTAX_WS','RHEOPIND','NAT_MIGRPTAX_WS','LITHNTAX','NAT_NTOLNTAX','TOLRNTAX'
                                      ,'HERBPTAX')
                                   ,c('ALIENNTAX_WS','NAT_ICTAPIND_WS','RHEONTAX','INTLMIGRNTAX','LITHPIND','NAT_NTOLNTAX_WS','INTLPTAX'
                                      ,'CARNNTAX_WS')
                                   ,c('NAT_PTAX','CYPRNTAX','INTLLOTNTAX_WS','INTLMIGRPTAX','LITHPIND','NTOLNTAX_WS','TOLRNTAX_WS'
                                      ,'INTLINVPTAX')
                                   ,c('NAT_PIND','NAT_CATOPIND','INTLLOTPTAX_WS','NAT_MIGRPTAX_WS','LITHPTAX','NAT_TOTLNTAX_WS'
                                      ,'TOLRNTAX','NAT_HERBPTAX')
                                   ,c('NAT_PIND','CENTPTAX','RHEOPIND','MIGRPTAX_WS','LITHNTAX_WS','NTOLPTAX','TOLRNTAX_WS'
                                      ,'BENTINVPTAX_WS'))
                      ,FLOOR=c(c(-0.494,0,-73.797,0,-81.385,-15.491,-3.595,9.09)
                               ,c(0,0,0,0,-55.684,-39.539,-1.611,0)
                               ,c(0,0,-5.045,-1.907,-52.18,-66.112,-42.369,-2.091)
                               ,c(80,-1.535,0,0,-56.528,31.82,0,-60.259)
                               ,c(67.61,-16.787,0,-1.326,0,1,2,0)
                               ,c(-0.298,-1.940,0,0,0,-9.403,0,-3.761)
                               ,c(85.71,0,-3.287,0,0,-8.389,-3.785,0)
                               ,c(0,0,-72.045,-74.29,25,-3.009,0,0)
                               ,c(0,0,0,-64.832,-6.202,0,-0.129,-48.74))
                      ,CEILING=c(c(14.277,3,30.655,5.88,33.825,7.213,7.123,68.75)
                                 ,c(4,2,100,8,23.496,22.490,6.853,9)
                                 ,c(2,100,4.352,1.579,53.848,29.11,62.153,1.960)
                                 ,c(100,3.62,66.67,4,28.448,100,66.67,38.399)
                                 ,c(100,62.614,92.71,31.49,6,8,14,25)
                                 ,c(2.045,17.204,4,1,97.52,4.824,50,2.235)
                                 ,c(100,9,2.110,13.33,95.35,6.445,5.549,33.33)
                                 ,c(100,68,27.826,40.433,100,3.272,2,33.33)
                                 ,c(100,25,100,38.279,1.649,100,4.67,23.306))
                      ,DISTRESP=c(c('NEGATIVE',rep('POSITIVE',5),'NEGATIVE','POSITIVE')
                                  ,c('NEGATIVE',rep('POSITIVE',5),rep('NEGATIVE',2))
                                  ,c('NEGATIVE','NEGATIVE',rep('POSITIVE',6))
                                  ,c('POSITIVE','NEGATIVE','POSITIVE','NEGATIVE',rep('POSITIVE',2),'NEGATIVE','POSITIVE')
                                  ,c('POSITIVE','NEGATIVE','POSITIVE','NEGATIVE',rep('POSITIVE',2),'NEGATIVE','POSITIVE')
                                  ,c(rep('NEGATIVE',2),'POSITIVE','NEGATIVE',rep('POSITIVE',4))
                                  ,c('POSITIVE',rep('POSITIVE',5),'NEGATIVE','POSITIVE')
                                  ,c('POSITIVE','NEGATIVE',rep('POSITIVE',4),'NEGATIVE','NEGATIVE')
                                  ,c('POSITIVE','NEGATIVE',rep('POSITIVE',4),'NEGATIVE','POSITIVE'))
                      ,stringsAsFactors=F)
 
 
 ww <- merge(fish.ws,cfVal,by=c('PARAMETER','ECO9'),all.x=T)
 
 scoreMet <- function(resptype,x,floor,ceiling){
   if(resptype=='POSITIVE'){
     zz <- round(approx(x=c(floor,ceiling),y=c(0,10),xout=x,method='linear',yleft=0,yright=10)$y,2)
   } else {
     zz <- round(approx(x=c(floor,ceiling),y=c(10,0),xout=x,method='linear',yleft=10,yright=0)$y,2)
   }
   
 }
 
 scored.mets <- data.frame(ww[,c('SAMPID','ECO9','PARAMETER')]
                         ,RESULT=with(ww,mapply(scoreMet,DISTRESP,RESULT,FLOOR,CEILING)))
 scored.mets$PARAMETER <- paste(scored.mets$PARAMETER,'_PT',sep='')
 
 print('Metric scores calculated.')
 
 # Calculate the vector of MMI scores at all sites, as sum of scored metrics, rescaled to a 0-100 range
 mmi <- plyr::ddply(scored.mets,c('SAMPID','ECO9'),summarise,RESULT=round(sum(RESULT)*(10/8),2),PARAMETER='MMI_FISH')
 
 ## Create long format dfs with metric values and metric residuals for modeled metrics
 adj.mets <- subset(ww[grep('_WS',ww$PARAMETER),],select=c('SAMPID','ECO9','PARAMETER','RESULT'))
 
 ## Now combine with metric scores and widen
 dfOut.1 <-rbind(mmi,scored.mets,adj.mets) %>% dcast(SAMPID+ECO9~PARAMETER,value.var='RESULT')
 # Reorder columns
 dfOut.2 <- merge(samples,dfOut.1,by='SAMPID') %>%
   subset(select=c(sampID,'SAMPID','ECO9','MMI_FISH',names(dfOut.1)[names(dfOut.1) %nin% c(sampID,'SAMPID','ECO9','MMI_FISH')])) %>%
   plyr::rename(c('ECO9'=ecoreg)) %>%
   dplyr::select(-SAMPID)
 
 print("Done calculating MMI score.")
 return(dfOut.2) 
  
}

