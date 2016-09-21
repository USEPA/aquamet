#' @export
#' @title Calculate only fish metrics necessary for NRSA MMI
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
calcFishMMI <- function(inMets, sampID='UID', ecoreg='ECOREG', lwsarea='WSAREA'){
  
  necTraits <- c(sampID,ecoreg,lwsarea)
  if(any(necTraits %nin% names(inMets))){
    msgTraits <- which(necTraits %nin% names(inMets))
    print(paste("Some of the traits are missing from the taxa list. The following are required for metric calculations to run:"
                , necTraits[msgTraits]))
    return(NULL)
  }
  
  # Rename variables 
  names(inMets)[names(inMets)==ecoreg] <- 'ECO9'
  names(inMets)[names(inMets)==lwsarea] <- 'WSAREA'

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
 
  
 matchMets <- reshape2::melt(inMets,id.vars=c('SAMPID','ECO9','WSAREA'),measure.vars=names(inMets)[names(inMets) %in% unique(metnames$PARAMETER)]
                             ,variable.name='PARAMETER',value.name='RESULT',na.rm=T) %>%
   merge(metnames,by=c('PARAMETER','ECO9')) %>%
   mutate(PARAMETER=as.character(PARAMETER))
 
 # Run a check to make sure there are exactly 8 rows per sites in the matched dataset
 numMets <- as.data.frame(table(SAMPID=table(matchMets$SAMPID))) %>% subset(Freq<8)
 if(nrow(numMets)>0){
   return(print(paste("Missing metrics values for these samples: ",numMets$SAMPID,". Check input data frame against required metric list.",sep='')))
 }
 
 
  # Metrics requiring watershed adjustment - intercepts and slopes by metric and ECO9
  wsMets <- data.frame(ECO9=c(rep('CPL',5),rep('NAP',3),rep('NPL',6),rep('SAP',3),rep('SPL',2)
                                        ,rep('TPL',4),rep('UMW',3),rep('WMT',3),rep('XER',3))
                       ,PARAMETER=c(c('ALIENPIND','LOTPIND','LITHPIND','NAT_TOTLNTAX','TOLRNTAX')
                                    ,c('LITHPTAX','NTOLPTAX','TOLRNTAX')
                                    ,c('LOTNTAX','MIGRNTAX','LITHPIND','NTOLPTAX','NAT_INTLPIND','NAT_CARNNTAX')
                                    ,c('NAT_CENTNTAX','NAT_LITHPIND','INVPIND')
                                    ,c('CYPRPTAX','NAT_MIGRPTAX')
                                    ,c('ALIENNTAX','NAT_ICTAPIND','NAT_NTOLNTAX','CARNNTAX')
                                    ,c('INTLLOTNTAX','NTOLNTAX','TOLRNTAX')
                                    ,c('INTLLOTPTAX','NAT_MIGRPTAX','NAT_TOTLNTAX')
                                    ,c('MIGRPTAX','LITHNTAX','TOLRNTAX'))
                       ,int=c(c(-0.209825,83.967349,90.09386,11.031451,1.922321)
                              ,c(91.323131,83.265581,-0.069501)
                              ,c(1.178205,0.497215,81.523467,119.442717,82.723027,-1.17374)
                              ,c(-0.015615,85.396592,26.110758)
                              ,c(8.606712,-4.892121)
                              ,c(-0.228218,-0.214354,1.878299,-0.047882)
                              ,c(1.075221,2.122047,0.362209)
                              ,c(111.298817,91.599417,0.725308)
                              ,c(93.425903,-0.266103,-0.14293))
                       ,slope=c(c(0.175519,-5.780754,-21.162115,2.80468,1.531341)
                                ,c(-9.193335,-5.636018,1.001382)
                                ,c(1.640487,0.378017,-13.26105,-17.583309,-21.053356,0.860391)
                                ,c(0.775378,-10.818335,11.379253)
                                ,c(11.779747,4.957571)
                                ,c(0.203559,0.834765,2.157333,1.323105)
                                ,c(0.670242,2.91785,1.774125)
                                ,c(-21.613077,-15.468566,1.108874)
                                ,c(-20.33868,1.370114,0.094112))
                       ,stringsAsFactors=F)
 
 # Now merge fish data with watershed regression info and adjust necessary metrics
 fish.ws <- merge(matchMets,wsMets,by=c('ECO9','PARAMETER'),all.x=T) %>%
   plyr::mutate(RESULT=as.numeric(RESULT)) %>%
   plyr::mutate(RESULT_WS=ifelse(is.na(int),NA,int+slope*WSAREA),RESULT_NEW=ifelse(is.na(slope),RESULT,round(RESULT-RESULT_WS,3))
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
                                      ,'BENTINVPTAX'))
                      ,FLOOR=c(c(-0.492,0,-78.178,0,-81.531,-14.198,-3.578,5.56)
                               ,c(0,0,0,0,-68.563,-77.224,-1.612,0)
                               ,c(0,0,-5.035,-1.916,-52.08,-67.773,-41.965,-2.204)
                               ,c(66.67,-1.534,0,0,-81.02,19.05,0,-60.881)
                               ,c(0,-50.384,0,-16.392,0,0,2,0)
                               ,c(-0.273,-1.839,0,0,0,-9.221,0,-3.851)
                               ,c(83.33,0,-3.293,0,0,-8.486,-3.807,0)
                               ,c(0,0,-90.451,-80.698,0,-3.134,0,0)
                               ,c(0,0,0,-70.586,-6.208,0,-0.129,0))
                      ,CEILING=c(c(14.276,3,30.775,5.88,34.099,7.305,7.182,68.75)
                                 ,c(4,2,100,8,23.354,22.511,6.841,9)
                                 ,c(3,100,4.357,1.609,54.215,28.856,62.591,2.007)
                                 ,c(100,3.636,66.67,4,28.442,100,66.67,38.39)
                                 ,c(100,24.845,92.71,14.41,6,8,14,25)
                                 ,c(2.027,17.309,4,1,97.52,4.792,50,2.211)
                                 ,c(100,9,2.119,13.33,95.35,6.453,5.512,33.33)
                                 ,c(100,62.5,27.62,40.128,100,3.283,2,33.33)
                                 ,c(100,25,100,38.276,1.648,100,3.948,33.33))
                      ,DISTRESP=c(c('NEGATIVE',rep('POSITIVE',5),'NEGATIVE','POSITIVE')
                                  ,c('NEGATIVE',rep('POSITIVE',5),rep('NEGATIVE',2))
                                  ,c('POSITIVE','NEGATIVE',rep('POSITIVE',6))
                                  ,c('POSITIVE','NEGATIVE','POSITIVE','NEGATIVE',rep('POSITIVE',2),'NEGATIVE','POSITIVE')
                                  ,c(rep('POSITIVE',6),'NEGATIVE','POSITIVE')
                                  ,c(rep('NEGATIVE',2),'POSITIVE','NEGATIVE',rep('POSITIVE',4))
                                  ,c(rep('POSITIVE',6),'NEGATIVE','POSITIVE')
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

