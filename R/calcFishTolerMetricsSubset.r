#' @export
#' @title Calculate all tolerance-related metrics
#' @description This function calculates all of the tolerance metrics,
#' and if additional trait values are included in the taxalist (habitat,
#' trophic, migr).
#' @param indata Input data frame containing variables as identified  
#' in the arguments for \emph{sampID}, \emph{dist}, \emph{ct}, 
#' \emph{taxa_id}. If a variable for anomaly counts, as specified in 
#' the argument \emph{anom}, is included, additional metrics are 
#' calculated. The variable for the optional argument \emph{nonnat}
#' should also be included in this data frame.
#' @param inTaxa Data frame containing fish taxalist, along with autecology
#' traits. At a minimum, this taxalist must contain variables matching 
#' the argument for \emph{tol}. If additional traits are included, as 
#' specified by the arguments \emph{habitat}, \emph{trophic}, \emph{migr},
#' additional metrics that combine tolerance and other traits are also 
#' calculated. The variable specified in the argument \emph{taxa_id} 
#' must match that in \emph{indata}. 
#' @param sampID sampID A character vector containing the names of all 
#' variables in indf that specify a unique sample. If not specified, 
#' the default is \emph{UID}.
#' @param dist A string with the name of the distinctness variable, 
#' which is assumed to have only values of 0 or 1. If not specified, 
#' the default is \emph{IS_DISTINCT}.
#' @param ct A string with the name of the count variable. If not 
#' specified, the default is \emph{TOTAL}.
#' @param taxa_id A string with the name of the taxon ID variable 
#' in \emph{indata} that matches that in \emph{inTaxa}. The default 
#' value is \emph{TAXA_ID}.
#' @param tol A string with the name of the variable in the 
#' \emph{inTaxa} taxalist containing tolerance categories. Valid
#' values include S (sensitive), I (intermediate), and T (tolerant).
#' The default value is \emph{TOLERANCE}.
#' @param tolval A string with the name of the variable in the 
#' \emph{inTaxa} taxalist containing numeric tolerance values.
#' Valid values range from 0 to 10. The default value is TOL_VAL.
#' @param vel A strings with the name of the variable in the 
#' \emph{inTaxa} taxalist containing velocity preference values.
#' Valid values include R (Rheophil), P (Pool), O (Other), or 
#' blank, if unknown. The default value is \emph{VELOCITY}.
#' @param habitat A string with the name of the variable in 
#' \emph{inTaxa} containing habitat preference values. Valid
#' values include B (benthic), W (water column), E (edge), or
#' blank, if unknown. The default value is \emph{HABITAT}.
#' @param trophic A string with the name of the variable in
#' \emph{inTaxa} containing trophic category values. Valid
#' values include I (invertivore), C (carnivore), O (omnivore),
#' H (herbivore), or blank if unknown. The default value is
#' \emph{inTaxa} is TROPHIC.
#' @param migr A string with the name of the variable in 
#' \emph{inTaxa} containing migratory status value. Valid 
#' values include N (No), Y (Yes), and blank if unknown. The
#' default value is \emph{MIGRATORY}.
#' @param nonnat A string with the name of the optional variable in 
#' \emph{inCts} containing non-native status. Valid values are 'Y' for 
#' non-native and 'N' for native. The default name 
#' is \emph{NONNATIVE}.
#' @return A data frame containing the variables in sampID and 
#' the fish tolerance metrics as additional variables. Metric 
#' descriptions are included in \emph{NRSA_Fish_Metric_Descriptions.pdf},
#' included in this package. The names of
#' metrics include INTLNIND, INTLNTAX, INTLPIND, INTLPTAX, MTOLNIND, 
#' MTOLNTAX, MTOLPIND, MTOLPTAX, NTOLNIND, NTOLNTAX, NTOLPIND, 
#' NTOLPTAX, TOLRNIND, TOLRNTAX, TOLRPIND, TOLRPTAX, WTD_TV, TOTLNIND,
#' and TOTLNTAX.
#' 
#' If a non-native status variable is included, these metrics are also
#' calculated:
#' NAT_INTLNIND, NAT_INTLNTAX, NAT_INTLPIND, NAT_INTLPTAX, NAT_MTOLNIND, 
#' NAT_MTOLNTAX, NAT_MTOLPIND, NAT_MTOLPTAX, NAT_NTOLNIND, NAT_NTOLNTAX, 
#' NAT_NTOLPIND, NAT_NTOLPTAX,  NAT_TOLRNIND, NAT_TOLRNTAX, NAT_TOLRPIND, 
#' NAT_TOLRPTAX, NAT_WTD_TV, NAT_TOTLNTAX, NAT_TOTLNIND,
#' NAT_PIND, NAT_PTAX. 
#'  
#' Additional metrics calculated if the appropriate additional traits
#' are included in \emph{inTaxa}: INTLINVNIND, INTLINVNTAX, INTLINVPIND, 
#' INTLINVPTAX, INTLLOTNIND, INTLLOTNTAX, INTLLOTPIND, INTLLOTPTAX,
#' INTLMIGRNIND, INTLMIGRNTAX, INTLMIGRPIND, INTLMIGRPTAX,  INTLRHEONIND, 
#' INTLRHEONTAX, INTLRHEOPIND, INTLRHEOPTAX, NTOLBENTNIND, NTOLBENTNTAX, 
#' NTOLBENTPIND, NTOLBENTPTAX, NTOLCARNNIND, NTOLCARNNTAX, 
#' NTOLCARNPIND, NTOLCARNPTAX, NTOLINVNIND, NTOLINVNTAX, NTOLINVPIND, 
#' NTOLINVPTAX. 
#' 
#' If a non-native status variable is included, these additional metrics
#' are also calculated: NAT_INTLINVNIND, NAT_INTLINVNTAX, NAT_INTLINVPIND, 
#' NAT_INTLINVPTAX, NAT_INTLLOTNIND, NAT_INTLLOTNTAX, NAT_INTLLOTPIND, 
#' NAT_INTLLOTPTAX, NAT_INTLMIGRNIND, NAT_INTLMIGRNTAX, NAT_INTLMIGRPIND, 
#' NAT_INTLMIGRPTAX, NAT_INTLRHEONIND, NAT_INTLRHEONTAX, NAT_INTLRHEOPIND, 
#' NAT_INTLRHEOPTAX, NAT_NTOLBENTNIND, NAT_NTOLBENTNTAX, NAT_NTOLBENTPIND, 
#' NAT_NTOLBENTPTAX, NAT_NTOLCARNNIND, NAT_NTOLCARNNTAX, NAT_NTOLCARNPIND, 
#' NAT_NTOLCARNPTAX, NAT_NTOLINVNIND, NAT_NTOLINVNTAX, NAT_NTOLINVPIND, 
#' NAT_NTOLINVPTAX.
#' 
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#'  
calcFishTolMets <- function(indata, inTaxa=NULL, sampID='UID', dist='IS_DISTINCT'
                            , ct='TOTAL', taxa_id='TAXA_ID', tol='TOLERANCE'
                            , tolval='TOL_VAL', vel='VELOCITY', habitat='HABITAT'
                            , trophic='TROPHIC', migr='MIGRATORY', nonnat='NONNATIVE'){
  
  ctVars <- c(sampID,dist,ct,taxa_id)
  if(any(ctVars %nin% names(indata))){
    msgTraits <- which(ctVars %nin% names(indata))
    print(paste("Missing variables in input data frame:",paste(ctVars[msgTraits],collapse=',')))
    return(NULL)
  }
  
  if(nonnat %nin% names(indata)){
    print(paste("Cannot calculate native status-based metrics without a non-native status variable. Will calculate other metrics."))
  }
    
  # Combine all values in sampID into one sampID in df
  for(i in 1:length(sampID)){
    if(i==1) indata$SAMPID <- indata[,sampID[i]]
    else indata$SAMPID <- paste(indata$SAMPID,indata[,sampID[i]],sep='.')
  }
  # Keep data frame with crosswalk info between sampID and SAMPID
  samples <- unique(subset(indata,select=c(sampID,'SAMPID')))
  
  # If inTaxa is not specified, the default is the included fishTaxa dataset
  if(is.null(inTaxa)) {
    inTaxa <- fishTaxa
  }
  
  # Taxonomy and traits checks
  necTraits <- c(tol,tolval)
  if(any(necTraits %nin% names(inTaxa))){
    msgTraits <- which(necTraits %nin% names(inTaxa))
    return(paste("Some of the traits are missing from the taxa list. The following are required for metric calculations to run:", necTraits[msgTraits]))
  }
  optTraits <- c(habitat,trophic,migr,vel)
  if(any(optTraits %nin% names(inTaxa))){
    msgTraits <- which(optTraits %nin% names(inTaxa))
    print(paste("Optional traits are missing from the taxa list. Any tolerance metrics also using these traits will not be calculated:",
                 paste(optTraits[msgTraits],collapse=',')))
  }

  
  inTaxa <- subset(inTaxa,select=names(inTaxa) %in% c(taxa_id,tol,tolval,vel,habitat,trophic,migr))  
  
  # Rename counts and distinct variables to FINAL_CT and IS_DISTINCT
  names(indata)[names(indata)==ct] <- 'FINAL_CT'
  names(indata)[names(indata)==dist] <- 'IS_DISTINCT'
  names(indata)[names(indata)==taxa_id] <- 'TAXA_ID'
  names(indata)[names(indata)==nonnat] <- 'NONNATIVE'
  names(inTaxa)[names(inTaxa)==taxa_id] <- 'TAXA_ID'
  
  names(inTaxa)[names(inTaxa)==tol] <- 'TOLERANCE'
  names(inTaxa)[names(inTaxa)==tolval] <- 'TOL_VAL'
  
  if(vel %in% names(inTaxa)){
    names(inTaxa)[names(inTaxa)==vel] <- 'VELOCITY'  
  }
  
  if(habitat %in% names(inTaxa)){
    names(inTaxa)[names(inTaxa)==habitat] <- 'HABITAT'
  }
  
  if(trophic %in% names(inTaxa)){
    names(inTaxa)[names(inTaxa)==trophic] <- 'TROPHIC'
  }
  
  if(migr %in% names(inTaxa)){
    names(inTaxa)[names(inTaxa)==migr] <- 'MIGRATORY'
  }
  
  indata[,c('FINAL_CT','IS_DISTINCT')] <- lapply(indata[,c('FINAL_CT','IS_DISTINCT')],as.numeric)
  indata$TAXA_ID <- as.character(indata$TAXA_ID)
  inTaxa$TAXA_ID <- as.character(inTaxa$TAXA_ID)
  inTaxa$TOL_VAL <- as.numeric(inTaxa$TOL_VAL)
  
  ## for inCts1, keep only observations without missing or zero FINAL_CT values or TAXA_ID and TAXA_ID!=99999
  indata.1 <- subset(indata,!is.na(TAXA_ID) & !is.na(FINAL_CT) & FINAL_CT!=0)
  
  ## Now sum by TAXA_ID for ANOM_CT and for FINAL_CT for each sample
  # Two approaches depending on whether or not NON_NATIVE occurs in the counts data frame
  if('NONNATIVE' %in% names(indata.1)){
    indata.2 <- plyr::ddply(indata.1,c('SAMPID','TAXA_ID','NONNATIVE'),summarise,IS_DISTINCT=max(IS_DISTINCT),FINAL_CT=sum(FINAL_CT))
    CALCNAT <- 'Y'
  }else{
    indata.2 <- plyr::ddply(indata.1,c('SAMPID','TAXA_ID'),summarise,IS_DISTINCT=max(IS_DISTINCT),FINAL_CT=sum(FINAL_CT))
    CALCNAT <- 'N'
  }	
  # Find all samples with a missing TAXA_ID, which means there are no counts for the site, and output the rows so the user can 
  ## verify no sample was collected
  if(nrow(subset(indata.2,is.na(TAXA_ID)))>0) {
    print("Make sure these missing TAXA_ID values are valid.")
    print(subset(indata,is.na(TAXA_ID)))
  }
  
  # Make sure all necessary columns in inCts are numeric
  inCts <- plyr::mutate(indata.2,FINAL_CT=as.numeric(FINAL_CT),IS_DISTINCT=as.integer(IS_DISTINCT))
  
  inCts.1 <- dplyr::semi_join(inCts,subset(inTaxa,select='TAXA_ID'),by='TAXA_ID') 
  
  if(CALCNAT=='Y'){
    inCts.1 <- dplyr::select(inCts.1,SAMPID, TAXA_ID, FINAL_CT, IS_DISTINCT,NONNATIVE) %>%
    subset(!is.na(FINAL_CT) & FINAL_CT>0)
  }else{
    inCts.1 <- dplyr::select(inCts.1,SAMPID, TAXA_ID, FINAL_CT, IS_DISTINCT) %>%
      subset(!is.na(FINAL_CT) & FINAL_CT>0)
  }
  # Now create indicator variables
  inTaxa.1 <- plyr::mutate(inTaxa,NTOL=ifelse(TOLERANCE %in% c('S','I'),1,NA)
                     ,INTL=ifelse(TOLERANCE=='S',1,NA)
                     ,MTOL=ifelse(TOLERANCE=='I',1,NA)
                     ,TOLR=ifelse(TOLERANCE=='T',1,NA)
                     ) 
  # Create empty data frames with all metric names in it
  empty_base <- data.frame(t(rep(NA,13)),stringsAsFactors=F)
  names(empty_base) <- c('INTLNTAX','INTLPIND','INTLPTAX', 
                         'MTOLNTAX','MTOLPIND','MTOLPTAX','NTOLNTAX','NTOLPIND', 
                         'NTOLPTAX','TOLRNTAX','TOLRPIND','TOLRPTAX','WTD_TV')
  
  # Add native metrics if CALCNAT='Y'
  if(CALCNAT=='Y'){
    empty_base.nat <- data.frame(t(rep(NA,17)),stringsAsFactors=F)
    names(empty_base.nat) <- c('NAT_INTLNTAX','NAT_INTLPIND','NAT_INTLPTAX', 
                               'NAT_MTOLNTAX','NAT_MTOLPIND','NAT_MTOLPTAX','NAT_NTOLNTAX', 
                               'NAT_NTOLPIND','NAT_NTOLPTAX','NAT_TOLRNTAX','NAT_TOLRPIND', 
                               'NAT_TOLRPTAX','NAT_WTD_TV','NAT_PIND','NAT_PTAX','NAT_TOTLNIND','NAT_TOTLNTAX')
    empty_base <- cbind(empty_base,empty_base.nat)
  }
  

  if('VELOCITY' %in% names(inTaxa.1)){
    inTaxa.1 <- plyr::mutate(inTaxa.1, INTLRHEO=ifelse(INTL==1 & VELOCITY=='R',1,NA)
                       ,INTLLOT=ifelse(INTL==1 & VELOCITY %in% c('R','O'),1,NA)
                       )
    
    empty_vel <- data.frame(t(rep(NA,6)),stringsAsFactors=F)
    names(empty_vel) <- c('INTLRHEONTAX','INTLRHEOPIND','INTLRHEOPTAX',
                        'INTLLOTNTAX','INTLLOTPIND','INTLLOTPTAX')
    empty_base <- cbind(empty_base,empty_vel)
    
    if(CALCNAT=='Y'){
      empty_vel.nat <- empty_vel
      names(empty_vel.nat) <- paste('NAT',names(empty_vel),sep='_')
      empty_base <- cbind(empty_base,empty_vel.nat)
    }
  }
  
  if('HABITAT' %in% names(inTaxa.1)){
    inTaxa.1 <- plyr::mutate(inTaxa.1, NTOLBENT=ifelse(NTOL==1 & HABITAT=='B',1,NA))
    
    empty_hab <- data.frame(t(rep(NA,3)),stringsAsFactors=F)
    names(empty_hab) <- c('NTOLBENTNTAX','NTOLBENTPIND','NTOLBENTPTAX')
    empty_base <- cbind(empty_base,empty_hab)
    
    if(CALCNAT=='Y'){
      empty_hab.nat <- empty_hab
      names(empty_hab.nat) <- paste('NAT',names(empty_hab),sep='_')
      empty_base <- cbind(empty_base,empty_hab.nat)
    }
    
  }
  
  if('TROPHIC' %in% names(inTaxa.1)){
    inTaxa.1 <- plyr::mutate(inTaxa.1, NTOLCARN=ifelse(NTOL==1 & TROPHIC=='C',1,NA)
                       ,INTLCARN=ifelse(INTL==1 & TROPHIC=='C',1,NA)
                       ,INTLINV=ifelse(INTL==1 & TROPHIC=='I',1,NA)
                       ,NTOLINV=ifelse(NTOL==1 & TROPHIC=='I',1,NA))
    
    empty_trop <- data.frame(t(rep(NA,12)),stringsAsFactors=F)
    names(empty_trop) <- c('INTLINVNTAX','INTLINVPIND','INTLINVPTAX'
                           ,'INTLCARNNTAX','INTLCARNPIND','INTLCARNPTAX'
                           ,'NTOLCARNNTAX','NTOLCARNPIND','NTOLCARNPTAX'
                           ,'NTOLINVNTAX','NTOLINVPIND','NTOLINVPTAX')
    empty_base <- cbind(empty_base,empty_trop)
    
    if(CALCNAT=='Y'){
      empty_trop.nat <- empty_trop
      names(empty_trop.nat) <- paste('NAT',names(empty_trop),sep='_')
      empty_base <- cbind(empty_base,empty_trop.nat)
    }
  }
  
  if('MIGRATORY' %in% names(inTaxa.1)){
    inTaxa.1 <- plyr::mutate(inTaxa.1,INTLMIGR=ifelse(INTL==1 & MIGRATORY=='Y',1,NA))
    
    empty_migr <- data.frame(t(rep(NA,3)),stringsAsFactors=F)
    names(empty_migr) <- c('INTLMIGRNTAX','INTLMIGRPIND','INTLMIGRPTAX')
    empty_base <- cbind(empty_base,empty_migr)
    
    if(CALCNAT=='Y'){
      empty_migr.nat <- empty_migr
      names(empty_migr.nat) <- paste('NAT',names(empty_migr),sep='_')
      empty_base <- cbind(empty_base,empty_migr.nat)
    }
  }
 
  params<-c('INTL','NTOL','MTOL','TOLR','INTLRHEO','INTLLOT','NTOLBENT','NTOLCARN','INTLCARN'
            ,'INTLINV','NTOLINV','INTLMIGR')
  
  inTaxa.2 <- subset(inTaxa.1,select=names(inTaxa.1) %in% c('TAXA_ID',params))
  
  taxalong <- reshape2::melt(inTaxa.2,id.vars='TAXA_ID',variable.name='TRAIT',na.rm=TRUE) %>%
    plyr::mutate(TRAIT=as.character(TRAIT))
  
  inCts.2 <- plyr::ddply(inCts.1, "SAMPID", mutate, TOTLNIND=sum(FINAL_CT),
                         TOTLNTAX=sum(IS_DISTINCT)) 
  
  if(CALCNAT=='Y'){
    inCts.2 <- dplyr::select(inCts.2,SAMPID,FINAL_CT,IS_DISTINCT,TAXA_ID,TOTLNTAX,TOTLNIND,NONNATIVE)
  }else{
    inCts.2 <- dplyr::select(inCts.2, SAMPID,FINAL_CT,IS_DISTINCT,TAXA_ID,TOTLNTAX,TOTLNIND)
  }
  
  totals <- unique(inCts.2[,c('SAMPID','TOTLNTAX','TOTLNIND')])
  
  # Merge the count data with the taxalist containing only the traits of
  # interest	
  traitDF <- merge(inCts.2, taxalong, by='TAXA_ID')
  
  # Calculate no. individuals, % individuals, no. taxa, and % taxa for each
  # trait in taxalist
  outMet <- plyr::ddply(traitDF, c("SAMPID", "TRAIT"), summarise,
                  NTAX=sum(IS_DISTINCT),
                  PIND=round(sum(FINAL_CT/TOTLNIND)*100,2),
                  PTAX=round(sum(IS_DISTINCT/TOTLNTAX)*100,2), .progress='tk') 
  
  # Melt df to create metric names, then recast into wide format with metric
  # names
  outLong <- reshape2::melt(outMet,id.vars=c('SAMPID','TRAIT')) 
  outLong$variable <- paste(outLong$TRAIT,outLong$variable,sep='')     
  outWide <- reshape2::dcast(outLong,SAMPID~variable,value.var='value') %>%
    merge(totals,by='SAMPID',all.y=T)
  
  if(nrow(subset(inTaxa,!is.na(TOL_VAL)))>0){
    TVI <- tolindexFish(inCts.2,inTaxa)  
    outWide <- merge(outWide,TVI,by="SAMPID",all.x=TRUE)
  }
  
  # Now run native metrics if CALCNAT='Y'
  ## If the variable NON_NATIVE is included and populated in inCts1, create inNative data frame
  if(CALCNAT=='Y'){
    if(any(unique(inCts.2$NON_NATIVE) %nin% c('Y','N'))){
      return(print("No native and alien datasets were created because NON_NATIVE must only be 'Y' or 'N' values"))     
    }else{
      inNative <- subset(inCts.2,NONNATIVE=='N') 
      if(length(inNative)>0){
        inNative.tot <- plyr::ddply(inNative,c('SAMPID'),mutate,NAT_TOTLNIND=sum(FINAL_CT),
                                             NAT_TOTLNTAX=sum(IS_DISTINCT))
        totals.nat <- unique(inNative.tot[,c('SAMPID','NAT_TOTLNIND','NAT_TOTLNTAX')])
                
        natMets <- merge(inNative.tot, taxalong, by='TAXA_ID') %>%
          plyr::ddply(c('SAMPID','TRAIT','NAT_TOTLNTAX','NAT_TOTLNIND'),summarise,
                                  NTAX=sum(IS_DISTINCT),
                                  PIND=round(sum(FINAL_CT/NAT_TOTLNIND)*100,2),
                                  PTAX=round(sum(IS_DISTINCT/NAT_TOTLNTAX)*100,2), .progress='tk')

        
        natMets.long <- reshape2::melt(natMets,id.vars=c('SAMPID','TRAIT','NAT_TOTLNTAX','NAT_TOTLNIND')) %>% 
          mutate(variable=paste('NAT_',TRAIT,variable,sep=''))
        
        natMets.1 <- reshape2::dcast(natMets.long,SAMPID~variable,value.var='value') %>%
          merge(totals.nat,by='SAMPID',all.y=T)
        
        
        outWide.1 <- merge(outWide,natMets.1,all=T)
        
        if(nrow(subset(inTaxa,!is.na(TOL_VAL)))>0){
          TVI <- tolindexFish(inNative,inTaxa) %>%
            plyr::rename(c('WTD_TV'='NAT_WTD_TV'))
          outWide.1 <- merge(outWide.1,TVI,by="SAMPID",all.x=TRUE) 
        }

        outWide.1 <- plyr::mutate(outWide.1,NAT_PTAX=round((NAT_TOTLNTAX/TOTLNTAX)*100,2),NAT_PIND=round((NAT_TOTLNIND/TOTLNIND)*100,2)) %>%
          select(-TOTLNTAX,-TOTLNIND)        
        
      }else{
        outWide.1 <- select(outWide,-TOTLNTAX,-TOTLNIND)
      }
    }
    }else{
        outWide.1 <- select(outWide,-TOTLNTAX,-TOTLNIND)
      }
   
  outWide.all <- gtools::smartbind(outWide.1,empty_base) %>% 
    filter(!is.na(SAMPID)) %>%
    merge(samples,by='SAMPID',all.y=T)
  
  # If we re-melt df now, we have missing values where the metric should be a
  # zero, so we can set NAs to 0 now
  outLong.1 <- reshape2::melt(outWide.all,id.vars=c(sampID,'SAMPID')) %>%
    plyr::mutate(value=ifelse(is.na(value) & variable %nin% c('WTD_TV','NAT_WTD_TV'),0,value))
  
  # Finally, we can recast the metrics df into wide format for output
  lside <- paste(paste(sampID,collapse='+'),'SAMPID',sep='+')
  formula <- paste(lside,'~variable',sep='')
  outWide.2 <- reshape2::dcast(outLong.1,eval(formula),value.var='value') 
  
  # Merge metrics with the original indata so that those without metrics because
  # no sample was collected are still output with missing values
  outAll <- merge(outWide.2,totals,by='SAMPID',all.x=T) %>%
    dplyr::select(-SAMPID)
  
  return(outAll)
      
}