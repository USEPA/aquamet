#' @export
#' @title Calculate the NRSA benthic macroinvertebrate MMI
#' 
#' @description This is a function that calculates 
#' the benthic MMI as used for the National Rivers and Streams 
#' Assessment, based on inputs of the appropriate metrics.
#' 
#' @param inMets A data frame containing, at minimum, the variables 
#' specified in the arguments for sampID, ecoreg, totlnind, and the metrics 
#' necessary for calculation of MMI by region. If values for more than 
#' the necessary metrics are included in the input data frame, the 
#' unnecessary  metrics will be ignored for each given site.
#' 
#' The necessary metrics, by aggregate ecoregion, are:
#' 
#'  CPL: NOINPIND, HPRIME, SHRDNTAX, CLNGPTAX, EPT_NTAX, TOLRPTAX
#'      
#'  NAP: EPT_PTAX, DOM5PIND, SCRPNTAX, CLNGPTAX, EPT_NTAX, NTOLPTAX
#'      
#'  NPL: EPT_PTAX, HPRIME, SCRPNTAX, BURRPTAX, EPHENTAX, NTOLNTAX 
#'      
#'  SAP: EPHEPTAX, HPRIME, SCRPNTAX, BURRPTAX, EPT_NTAX, TOLRPTAX
#'      
#'  SPL: EPT_PIND, HPRIME, SCRPNTAX, BURRPTAX, EPT_NTAX, INTLNTAX
#'  
#'  TPL: EPT_PIND, HPRIME, SCRPNTAX, CLNGNTAX, EPHENTAX, STOLPTAX
#'      
#'  UMW: CHIRPTAX, HPRIME, SHRDNTAX, BURRPTAX, EPT_NTAX, STOLPTAX
#'      
#'  WMT: EPT_PTAX, DOM5PIND, SCRPNTAX, CLNGPTAX, EPT_NTAX, TOLRPTAX
#'      
#'  XER: NOINPIND, DOM5PIND, SCRPNTAX, CLNGPTAX, EPT_NTAX, TOLRPTAX
#'  
#'  Descriptions of these metrics can be found in the file 
#'  \emph{NRSA_Invertebrate_Metric_Descriptions.pdf}, included in 
#'  the documentation for this package.
#' 
#' @param sampID A character vector containing the names of all 
#' variables in inMets that specify a unique sample. If not specified, 
#' the default is \emph{UID}
#' @param ecoreg A string with the name of the ecoregion variable. 
#' Valid values that correspond to regions used in NRSA are
#' CPL, NAP, NPL, SAP, sPL, TPL, UMW, WMT, and XER.
#' @param totlnind A string with the name of the variable with the 
#' total individuals in each sample. 
#' @return A data frame containing the variables in sampID, as well as
#' the scored metrics, the benthic MMI, and the condition class for each
#' sites. The variable names are COMP_PT, DIVS_PT, FEED_PT, HABT_PT,
#' RICH_PT, TOLR_PT, MMI_BENT, BENT_MMI_COND.
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#' @keywords survey
calcNRSA_BenthicMMI <- function(inMets, sampID='UID', ecoreg='ECOREG',totlnind='TOTLNIND'){

  necTraits <- c(sampID,ecoreg,totlnind)
  if(any(necTraits %nin% names(inMets))){
    msgTraits <- which(necTraits %nin% names(inMets))
    print(paste("Some of the traits are missing from the taxa list. The following are required for metric calculations to run:"
                , necTraits[msgTraits]))
    return(NULL)
  }
  
  # Rename variables 
  names(inMets)[names(inMets)==ecoreg] <- 'ECO9'
  names(inMets)[names(inMets)==totlnind] <- 'TOTLNIND'
  
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
  
  metnames <- data.frame(ECO9=c(rep('CPL',6),rep('NAP',6),rep('NPL',6),rep('SAP',6),rep('SPL',6),rep('TPL',6)
                                          ,rep('UMW',6),rep('WMT',6),rep('XER',6))
                         ,PARAMETER=c('NOINPIND','HPRIME','SHRDNTAX','CLNGPTAX','EPT_NTAX','TOLRPTAX'
                                   ,'EPT_PTAX','DOM5PIND','SCRPNTAX','CLNGPTAX','EPT_NTAX','NTOLPTAX'
                                   ,'EPT_PTAX','HPRIME','SCRPNTAX','BURRPTAX','EPHENTAX','NTOLNTAX'
                                   ,'EPHEPTAX','HPRIME','SCRPNTAX','BURRPTAX','EPT_NTAX','TOLRPTAX'
                                   ,'EPT_PIND','HPRIME','SCRPNTAX','BURRPTAX','EPT_NTAX','INTLNTAX'
                                   ,'EPT_PIND','HPRIME','SCRPNTAX','CLNGNTAX','EPHENTAX','STOLPTAX'
                                   ,'CHIRPTAX','HPRIME','SHRDNTAX','BURRPTAX','EPT_NTAX','STOLPTAX'
                                   ,'EPT_PTAX','DOM5PIND','SCRPNTAX','CLNGPTAX','EPT_NTAX','TOLRPTAX'
                                   ,'NOINPIND','DOM5PIND','SCRPNTAX','CLNGPTAX','EPT_NTAX','TOLRPTAX')
                         ,stringsAsFactors=FALSE)
  
  matchMets <- reshape2::melt(inMets,id.vars=c('SAMPID','ECO9','TOTLNIND')
                              ,measure.vars=names(inMets)[names(inMets) %in% unique(metnames$PARAMETER)]
                              ,variable.name='PARAMETER',value.name='RESULT',na.rm=T) %>%
    merge(metnames,by=c('PARAMETER','ECO9')) %>%
    mutate(PARAMETER=as.character(PARAMETER))
  
  # Run a check to make sure there are exactly 8 rows per sites in the matched dataset
  numMets <- as.data.frame(table(SAMPID=matchMets$SAMPID)) %>% subset(Freq<6)
  if(nrow(numMets)>0){
    return(print(paste("Missing metrics values for these samples: ",numMets$SAMPID,". Check input data frame against required metric list.",sep='')))
  }
  
  
  ## Create data frame containing direction of metric response and scoring thresholds for each metric
  cfVal <- data.frame(metnames,DISTRESP=c('NEGATIVE',rep('POSITIVE',4),'NEGATIVE','POSITIVE','NEGATIVE',rep('POSITIVE',7)
                                          ,'NEGATIVE',rep('POSITIVE',5),'NEGATIVE','POSITIVE','NEGATIVE',rep('POSITIVE',3),'NEGATIVE',rep('POSITIVE',7)
                                          ,'NEGATIVE','NEGATIVE',rep('POSITIVE',2),'NEGATIVE','POSITIVE','NEGATIVE','POSITIVE','NEGATIVE'
                                          ,rep('POSITIVE',3),rep('NEGATIVE',3),rep('POSITIVE',3),'NEGATIVE')
                      ,FLOOR=c(0.7,1.62,1.00,14.3,1.0,5.56
                               ,9.52,37.2,3.0,28.6,3.00,46.2,3.85,1.10,1.0,6.45,0,4,5.41,2.05,3.0,3.45,5.0,2.44,0.67,1.16,1.0,5.0,1.0,1.0
                               ,0.67,1.41,1.0,3.0,1.0,4.35,11.2,2.01,3.0,3.77,4.0,2.51,18.5,40.6,1.0,27.0,6.0,2.27,3.33,44.7,0.0,15.8,1.0
                               ,3.57)
                      ,CEILING=c(73.0,3.31,9,54.8,17,50,57.6,76.2,12,70,24,86.1,50,3.07,6,35.3,7,28,28.6,3.44,12,25,25,27.6
                                 ,66,3.27,8,36.1,16,8,80.3,3.17,9,20,11,33.3,50.8,3.56,10,28.6,22,29.5,62.9,82.3,8,69.6,23,25,36,92.3,7,65.8
                                 ,18,36.4))
  ## Merge scoring thresholds with metric values in long format
  matchMets.1 <- merge(cfVal,matchMets,by=c('ECO9','PARAMETER'))
  
  ## The function below interpolates the score between the floor and ceiling scoring thresholds for each metric
  scoreMet1<-function(resptype,x,floor,ceiling){
    if(resptype=='POSITIVE'){
      zz<-round(approx(x=c(floor,ceiling),y=c(0,10),xout=x,method='linear',yleft=0,yright=10)$y,2) 
    } else {
      zz<-round(approx(x=c(floor,ceiling),y=c(10,0),xout=x,method='linear',yleft=10,yright=0)$y,2)
    }
    
  }
  
  ## Send metric values to the scoring function above (scoreMet1)
  scored.mets <- mutate(matchMets.1[,c('SAMPID','TOTLNIND','ECO9','PARAMETER')]
                        ,RESULT=ifelse(as.numeric(TOTLNIND)==0,0,with(matchMets.1,mapply(scoreMet1,DISTRESP,RESULT,FLOOR,CEILING))))
  scored.mets$PARAMETER <- plyr::revalue(scored.mets$PARAMETER
                                         ,c('EPT_PIND'='COMP_PT','EPT_PTAX'='COMP_PT'
                                         ,'NOINPIND'='COMP_PT','EPHEPTAX'='COMP_PT','CHIRPTAX'='COMP_PT','HPRIME'='DIVS_PT','DOM5PIND'='DIVS_PT'
                                         ,'SCRPNTAX'='FEED_PT','SHRDNTAX'='FEED_PT','BURRPTAX'='HABT_PT','CLNGPTAX'='HABT_PT','CLNGNTAX'='HABT_PT'
                                         ,'EPT_NTAX'='RICH_PT','EPHENTAX'='RICH_PT','INTLNTAX'='TOLR_PT','TOLRPTAX'='TOLR_PT'
                                         ,'NTOLNTAX'='TOLR_PT','STOLPTAX'='TOLR_PT','NTOLPTAX'='TOLR_PT'),warn_missing=F)
  ## Sum metrics scores for each sample and rescale total to 100-point scale
  mmi.scores <- ddply(scored.mets,c('SAMPID','TOTLNIND','ECO9'),summarise,PARAMETER='MMI_BENT'
                      ,RESULT=round((100/60)*sum(RESULT),2))
  
  ## Set condition class for each sample, which is based on AGGR_ECO9_2015
  # First create a table of thresholds by AGGR_ECO9_2015
  condTholds <- data.frame(ECO9=c('CPL','NAP','NPL','SAP','SPL','TPL','UMW','WMT','XER')
                           ,gf=c(54.9,55.0,56.8,45.0,35.5,40.3,36.9,50.1,57.0)
                           ,fp=c(40.7,40.9,42.6,30.8,21.3,26.2,22.7,35.9,42.8),stringsAsFactors=FALSE)
 
  ## Merge MMI scores with thresholds by ECO9 region
  cond.mmi <- merge(mmi.scores,condTholds,by='ECO9')
  cond.mmi <- mutate(cond.mmi,ECO9=as.character(ECO9),PARAMETER='BENT_MMI_COND',MMI_BENT=RESULT
                     ,RESULT=ifelse(is.na(MMI_BENT),'Not Assessed',ifelse(MMI_BENT>=gf,'Good',ifelse(MMI_BENT<fp,'Poor','Fair'))))
  
  ww <- rbind(subset(scored.mets,select=c('SAMPID','ECO9','PARAMETER','RESULT'))
              ,subset(mmi.scores,select=c('SAMPID','ECO9','PARAMETER','RESULT'))
              ,subset(cond.mmi,select=c('SAMPID','ECO9','PARAMETER','RESULT')))
  
  # Finally, we can recast the metrics df into wide format for output
  lside <- paste(paste('SAMPID',collapse='+'),'ECO9',sep='+')
  formula <- paste(lside,'~PARAMETER',sep='')
  mmiOut <- reshape2::dcast(ww,eval(formula),value.var='RESULT') 
  
  mmiOut.final <- merge(samples,mmiOut,by='SAMPID') %>%
    subset(select=c(sampID,'SAMPID','ECO9','MMI_BENT','BENT_MMI_COND'
                    ,names(mmiOut)[names(mmiOut) %nin% c(sampID,'SAMPID','ECO9','MMI_BENT','BENT_MMI_COND')])) %>%
    plyr::rename(c('ECO9'=ecoreg)) %>%
    dplyr::select(-SAMPID)
  
  return(mmiOut.final)  

}
