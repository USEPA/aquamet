#' @export
#' @title Calculate the NLA benthic macroinvertebrate MMI
#' 
#' @description This is a function that calculates 
#' the benthic MMI as used for the National Lakes 
#' Assessment, based on inputs of the appropriate metrics.
#' 
#' @param inMets A data frame containing, at minimum, the variables 
#' specified in the arguments for sampID, ecoreg, totlnind, and the metrics 
#' necessary for calculation of MMI by region. If values for more than 
#' the necessary metrics are included in the input data frame, the 
#' unnecessary  metrics will be ignored for each given site.
#' 
#' The necessary metrics, by aggregated bioregion, are:
#' 
#'  CPL: NOINPTAX, CHIRDOM5PIND, PREDNTAX, SPWLNTAX, EPT_NTAX, NTOLPIND
#'      
#'  EHIGH: NOINPTAX, CHIRDOM5PIND, COGANTAX, CLNGNTAX, EPOTNTAX, TL23NTAX
#'  
#'  PLAINS: DIPTPTAX, CHIRDOM5PIND, PREDNTAX, CLMBPTAX, EPOTNTAX, TL23PIND
#'  
#'  UMW: NOINPIND, CHIRDOM3PIND, SHRDPIND, CLNGNTAX, CRUSNTAX, TL23PTAX
#'  
#'  WMTNS: DIPTPIND, HPRIME, SCRPNTAX, CLNGNTAX, EPT_NTAX, TL23PTAX
#'  
#'  Descriptions of these metrics can be found in the file 
#'  \emph{NRSA_Invertebrate_Metric_Descriptions.pdf}, included in 
#'  the documentation for this package.
#' 
#' @param sampID A character vector containing the names of all 
#' variables in inMets that specify a unique sample. If not specified, 
#' the default is \emph{UID}
#' @param ecoreg A string with the name of the aggregated bioregion variable. 
#' Valid values that correspond to regions used in NLA are
#' CPL, EHIGH, PLAINS, UMW, and WMTNS.
#' @param totlnind A string with the name of the variable with the 
#' total individuals in each sample. 
#' @return A data frame containing the variables in sampID, as well as
#' the scored metrics, the benthic MMI, and the condition class for each
#' sites. The variable names are COMP_PT, DIVS_PT, FEED_PT, HABT_PT,
#' RICH_PT, TOLR_PT, MMI_BENT, BENT_MMI_COND.
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#' @keywords survey
calcNLA_BenthicMMI <- function(inMets, sampID='UID', ecoreg='ECOREG',totlnind='TOTLNIND'){

  necTraits <- c(sampID,ecoreg,totlnind)
  if(any(necTraits %nin% names(inMets))){
    msgTraits <- which(necTraits %nin% names(inMets))
    print(paste("Some of the traits are missing from the taxa list. The following are required for metric calculations to run:"
                , necTraits[msgTraits]))
    return(NULL)
  }
  
  # Rename variables 
  names(inMets)[names(inMets)==ecoreg] <- 'ECO_BIO'
  names(inMets)[names(inMets)==totlnind] <- 'TOTLNIND'
  
  # Combine all values in sampID into one sampID in df
  for(i in 1:length(sampID)){
    if(i==1) inMets$SAMPID <- inMets[,sampID[i]]
    else inMets$SAMPID <- paste(inMets$SAMPID,inMets[,sampID[i]],sep='.')
  }
  samples <- unique(inMets[,c('SAMPID',sampID)])
  
  # Check to make sure ecoregion variable is included in the input data frame
  ecoCk <- unique(inMets$ECO_BIO)
  ecos <- c('CPL','EHIGH','PLAINS','UMW','WMTNS')
  if(any(ecoCk %nin% ecos)){
    msgEco <- which(ecoCk %nin% ecos)
    print(paste("These ecoregions are not valid: "
                ,paste(ecoCk[msgEco],collapse=',')))
    return(NULL)
  }
  
  metnames <- data.frame(ECO_BIO=c(rep('CPL',6),rep('EHIGH',6),rep('PLAINS',6),rep('UMW',6),rep('WMTNS',6))
                         ,PARAMETER=c('NOINPTAX','CHIRDOM5PIND','PREDNTAX','SPWLNTAX','EPT_NTAX','NTOLPIND'
                                      ,'NOINPTAX','CHIRDOM5PIND','COGANTAX','CLNGNTAX','EPOTNTAX','TL23NTAX'
                                      ,'DIPTPTAX','CHIRDOM5PIND','PREDNTAX','CLMBPTAX','EPOTNTAX','TL23PIND'
                                      ,'NOINPIND','CHIRDOM3PIND','SHRDPIND','CLNGNTAX','CRUSNTAX','TL23PTAX'
                                      ,'DIPTPIND','HPRIME','SCRPNTAX','CLNGNTAX','EPT_NTAX','TL23PTAX')
                         ,stringsAsFactors=FALSE)
  
  matchMets <- reshape2::melt(inMets,id.vars=c('SAMPID','ECO_BIO','TOTLNIND')
                              ,measure.vars=names(inMets)[names(inMets) %in% unique(metnames$PARAMETER)]
                              ,variable.name='PARAMETER',value.name='RESULT',na.rm=T) %>%
    merge(metnames,by=c('PARAMETER','ECO_BIO')) %>%
    mutate(PARAMETER=as.character(PARAMETER))
  
  # Run a check to make sure there are exactly 6 rows per sites in the matched dataset
  numMets <- as.data.frame(table(SAMPID=matchMets$SAMPID)) %>% 
    subset(Freq<6) %>%
    merge(inMets,by='SAMPID') %>%
    filter(is.na(TOTLNIND)|TOTLNIND>=100)
  if(nrow(numMets)>0){
    return(print(paste("Missing metrics values for these samples: ",numMets$SAMPID,". Check input data frame against required metric list.",sep='')))
  }
  
  
  ## Create data frame containing direction of metric response and scoring thresholds for each metric
  cfVal <- data.frame(ECO_BIO=c(rep('CPL',6),rep('EHIGH',6),rep('PLAINS',6),rep('UMW',6),rep('WMTNS',6))
                                    ,PARAMETER=c('NOINPTAX','CHIRDOM5PIND','PREDNTAX','SPWLNTAX','EPT_NTAX','NTOLPIND'
                                                 ,'NOINPTAX','CHIRDOM5PIND','COGANTAX','CLNGNTAX','EPOTNTAX','TL23NTAX'
                                                 ,'DIPTPTAX','CHIRDOM5PIND','PREDNTAX','CLMBPTAX','EPOTNTAX','TL23PIND'
                                                 ,'NOINPIND','CHIRDOM3PIND','SHRDPIND','CLNGNTAX','CRUSNTAX','TL23PTAX'
                                                 ,'DIPTPIND','HPRIME','SCRPNTAX','CLNGNTAX','EPT_NTAX','TL23PTAX')
                                    ,MET_TYPE=rep(c('COMP','DIVS','FEED','HABT','RICH','TOLR'),5)
                                    ,DISTRESP=c('NEGATIVE','NEGATIVE','POSITIVE','POSITIVE','POSITIVE','POSITIVE'
                                                ,'NEGATIVE','NEGATIVE','POSITIVE','POSITIVE','POSITIVE','POSITIVE'
                                                ,'NEGATIVE','NEGATIVE','POSITIVE','POSITIVE','POSITIVE','POSITIVE'
                                                ,'NEGATIVE','NEGATIVE','NEGATIVE','POSITIVE','NEGATIVE','POSITIVE'
                                                ,'POSITIVE','POSITIVE','NEGATIVE','POSITIVE','POSITIVE','POSITIVE')
                                    ,FLOOR=c(21.88,55.71,6,5,1,6.33
                                             ,13.79,57.46,8,3,2,1
                                             ,16.67,50.44,2,10,0,0
                                             ,5.33,36.51,2.67,3,0,2.17
                                             ,5.97,1.09,0,1,0,0)
                                    ,CEILING=c(55.17,100,23,15,8,64.33
                                               ,48.72,95.24,27,12,14,9
                                               ,60,100,19,33.33,10,19.67
                                               ,89,89.29,50.67,14,3,23.81
                                               ,84.33,2.87,5,8,7,21.43)
                                    ,stringsAsFactors=FALSE)
  ## Merge scoring thresholds with metric values in long format
  matchMets.1 <- merge(cfVal,matchMets,by=c('ECO_BIO','PARAMETER'))
  
  ## The function below interpolates the score between the floor and ceiling scoring thresholds for each metric
  scoreMet1<-function(resptype,x,floor,ceiling){
    if(resptype=='POSITIVE'){
      zz<-round(approx(x=c(floor,ceiling),y=c(0,10),xout=x,method='linear',yleft=0,yright=10)$y,2) 
    } else {
      zz<-round(approx(x=c(floor,ceiling),y=c(10,0),xout=x,method='linear',yleft=10,yright=0)$y,2)
    }
    
  }
  
  ## Send metric values to the scoring function above (scoreMet1)
  scored.mets <- mutate(matchMets.1[,c('SAMPID','TOTLNIND','ECO_BIO','PARAMETER')]
                        ,RESULT=ifelse(as.numeric(TOTLNIND)<100,NA,with(matchMets.1,mapply(scoreMet1,DISTRESP,RESULT,FLOOR,CEILING))))
  scored.mets$PARAMETER <- plyr::revalue(scored.mets$PARAMETER
                                         ,c('DIPTPIND'='COMP_PT','DIPTPTAX'='COMP_PT'
                                         ,'NOINPIND'='COMP_PT','NOINPTAX'='COMP_PT','HPRIME'='DIVS_PT','CHIRDOM5PIND'='DIVS_PT'
                                         ,'CHIRDOM3PIND'='DIVS_PT','SCRPNTAX'='FEED_PT','SHRDPIND'='FEED_PT'
                                         ,'COGANTAX'='FEED_PT','PREDNTAX'='FEED_PT','CLMBPTAX'='HABT_PT','CLNGNTAX'='HABT_PT'
                                         ,'SPWLNTAX'='HABT_PT','CRUSNTAX'='RICH_PT'
                                         ,'EPT_NTAX'='RICH_PT','EPOTNTAX'='RICH_PT','NTOLPIND'='TOLR_PT','TL23NTAX'='TOLR_PT'
                                         ,'TL23PTAX'='TOLR_PT','TL23PIND'='TOLR_PT'),warn_missing=F)
  ## Sum metrics scores for each sample and rescale total to 100-point scale
  mmi.scores <- ddply(scored.mets,c('SAMPID','TOTLNIND','ECO_BIO'),summarise,PARAMETER='MMI_BENT'
                      ,RESULT=round((100/60)*sum(RESULT),1))
  
  ## Set condition class for each sample, which is based on ECO_BIO region
  # First create a table of thresholds by ECO_BIO
  condTholds <- data.frame(ECO_BIO=c('CPL','EHIGH','PLAINS','UMW','WMTNS')
                           ,gf=c(53.0,50.3,41.5,57.6,54.8)
                           ,fp=c(41.0,38.3,29.5,45.6,42.8),stringsAsFactors=FALSE)
 
  ## Merge MMI scores with thresholds by ECO_BIO region
  cond.mmi <- merge(mmi.scores,condTholds,by='ECO_BIO')
  cond.mmi <- mutate(cond.mmi,ECO_BIO=as.character(ECO_BIO),PARAMETER='BENT_MMI_COND',MMI_BENT=RESULT
                     ,RESULT=ifelse(is.na(MMI_BENT),'Not Assessed',ifelse(MMI_BENT>=gf,'Good',ifelse(MMI_BENT<fp,'Poor','Fair'))))
  
  ww <- rbind(subset(scored.mets,select=c('SAMPID','ECO_BIO','PARAMETER','RESULT'))
              ,subset(mmi.scores,select=c('SAMPID','ECO_BIO','PARAMETER','RESULT'))
              ,subset(cond.mmi,select=c('SAMPID','ECO_BIO','PARAMETER','RESULT')))
  
  # Finally, we can recast the metrics df into wide format for output
  lside <- paste(paste('SAMPID',collapse='+'),'ECO_BIO',sep='+')
  formula <- paste(lside,'~PARAMETER',sep='')
  mmiOut <- reshape2::dcast(ww,eval(formula),value.var='RESULT') 
  
  mmiOut.final <- merge(samples,mmiOut,by='SAMPID') %>%
    subset(select=c(sampID,'SAMPID','ECO_BIO','MMI_BENT','BENT_MMI_COND'
                    ,names(mmiOut)[names(mmiOut) %nin% c(sampID,'SAMPID','ECO_BIO','MMI_BENT','BENT_MMI_COND')])) %>%
    plyr::rename(c('ECO_BIO'=ecoreg)) %>%
    dplyr::select(-SAMPID)
  
  return(mmiOut.final)  

}
