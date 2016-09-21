#' @export
#' @title Calculate other subsets of fish metrics  
#' @description This function calculates other metrics using the
#' traits for velocity preference, migratory tendency, reproductive habits,
#' and temperature preference, depending on what traits are included
#' in the input taxalist. Native status versions of metrics are calculated
#' if a non-native status variable is included in the input count data.
#' @param indata Input data frame containing variables as identified  
#' in the arguments for \emph{sampID}, \emph{dist}, \emph{ct}, 
#' \emph{taxa_id}, as well as the optional variable 
#' for non-native status in \emph{nonnat}. 
#' @param inTaxa Data frame containing fish taxalist, along with autecology
#' traits. At a minimum, this taxalist must contain at least one of the
#' variables matching \emph{migr}, \emph{vel}, \emph{reprod}, \emph{temp}. 
#' The variable specified in the argument \emph{taxa_id} 
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
#' @param vel A strings with the name of the variable in the 
#' \emph{inTaxa} taxalist containing velocity preference values.
#' Valid values include R (Rheophil), P (Pool), O (Other), or 
#' blank, if unknown. The default value is \emph{VELOCITY}.
#' @param migr A string with the name of the variable in 
#' \emph{inTaxa} containing migratory status value. Valid 
#' values include N (No), Y (Yes), and blank if unknown. The
#' default value is \emph{MIGRATORY}.
#' @param reprod A string with the name of the variable in 
#' \emph{inTaxa} containing reproductive trait values. Valid 
#' values include C (Clean, coarse lithophil), D (Drifter),
#' G (Guarder), O (Other), or blank if unknown. The default
#' value is \emph{REPROD}.
#' @param temp A string with the name of the variable in 
#' \emph{inTaxa} containing the temperature preference values.
#' Valid values include WM (warm), CD (cold water), CL (cool water), 
#' or blank if unknow. The default value is \emph{TEMP}.
#' @param nonnat A string with the name of the optional variable in 
#' \emph{inCts} containing non-native status. Valid values are 'Y' for 
#' non-native and 'N' for native. The default name 
#' is \emph{NONNATIVE}.
#' @return A data frame containing the variables in sampID and 
#' the fish metrics as additional variables. Metric 
#' descriptions are included in \emph{NRSA_Fish_Metric_Descriptions.pdf},
#' included in this package. The names of
#' metrics include  RHEONTAX, RHEOPIND, RHEOPTAX, LOTNTAX, LOTPIND, 
#' LOTPTAX,  MIGRNTAX, MIGRPIND, MIGRPTAX,  LITHNTAX, LITHPIND, 
#' LITHPTAX,  COLDNTAX, COLDPIND, COLDPTAX, TOTLNIND,
#' and TOTLNTAX.
#' 
#' If a non-native status variable is included, these metrics are also
#' calculated:
#' NAT_RHEONTAX, NAT_RHEOPIND, NAT_RHEOPTAX, NAT_LOTNTAX, 
#' NAT_LOTPIND, NAT_LOTPTAX, NAT_ MIGRNTAX, NAT_MIGRPIND, NAT_MIGRPTAX, 
#' NAT_ LITHNTAX, NAT_LITHPIND, NAT_LITHPTAX, NAT_ COLDNTAX, NAT_COLDPIND, 
#' NAT_COLDPTAX, NAT_NAT_TOTLNTAX, NAT_NAT_TOTLNIND, NAT_PIND, NAT_PTAX. 
#'  
#' 
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#'  
calcFishOtherMets <- function(indata, inTaxa=NULL, sampID='UID', dist='IS_DISTINCT'
                            , ct='TOTAL', taxa_id='TAXA_ID', vel='VELOCITY'
                            , migr='MIGRATORY', reprod='REPROD', temp='TEMP'
                            , nonnat='NONNATIVE'){
  
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
  optTraits <- c(migr,vel,reprod,temp)
  if(all(optTraits %nin% names(inTaxa))){
    return(print("Need at least one of the following in inTaxa taxalist: migr, vel, reprod, temp"))
  }

  if(any(optTraits %nin% names(inTaxa))){
    msgTraits <- which(optTraits %nin% names(inTaxa))
    print(paste("Some optional traits are missing from the taxa list. Any tolerance metrics also using these traits will not be calculated:\n",
                optTraits[msgTraits],"\n"))
  }
  
  
  inTaxa <- subset(inTaxa,select=names(inTaxa) %in% c(taxa_id,vel,migr,reprod,temp))  
  
  # Rename counts and distinct variables to FINAL_CT and IS_DISTINCT
  names(indata)[names(indata)==ct] <- 'FINAL_CT'
  names(indata)[names(indata)==dist] <- 'IS_DISTINCT'
  names(indata)[names(indata)==taxa_id] <- 'TAXA_ID'
  names(indata)[names(indata)==nonnat] <- 'NONNATIVE'
  names(inTaxa)[names(inTaxa)==taxa_id] <- 'TAXA_ID'
    
  if(vel %in% names(inTaxa)){
    names(inTaxa)[names(inTaxa)==vel] <- 'VELOCITY'  
  }
    
  if(migr %in% names(inTaxa)){
    names(inTaxa)[names(inTaxa)==migr] <- 'MIGRATORY'
  }

  if(reprod %in% names(inTaxa)){
    names(inTaxa)[names(inTaxa)==reprod] <- 'REPROD'  
  }
  
  if(temp %in% names(inTaxa)){
    names(inTaxa)[names(inTaxa)==temp] <- 'TEMP'
  }
  
  indata[,c('FINAL_CT','IS_DISTINCT')] <- lapply(indata[,c('FINAL_CT','IS_DISTINCT')],as.numeric)
  indata$TAXA_ID <- as.character(indata$TAXA_ID)
  inTaxa$TAXA_ID <- as.character(inTaxa$TAXA_ID)
  
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
  inTaxa.1 <- inTaxa
   
  # Create empty data frames with all metric names in it
  empty_base <- data.frame(SAMPID=samples$SAMPID,stringsAsFactors=F)
  
  if('VELOCITY' %in% names(inTaxa.1)){
    inTaxa.1 <- plyr::mutate(inTaxa.1, RHEO=ifelse(VELOCITY=='R',1,NA)
                             ,LOT=ifelse(VELOCITY %in% c('R','O'),1,NA))
    
    empty_vel <- data.frame(t(rep(NA,6)),stringsAsFactors=F)
    names(empty_vel) <- c('RHEONTAX','RHEOPIND','RHEOPTAX',
                          'LOTNTAX','LOTPIND','LOTPTAX')
    empty_base <- cbind(empty_base,empty_vel)
    
    if(CALCNAT=='Y'){
      empty_vel.nat <- empty_vel
      names(empty_vel.nat) <- paste('NAT',names(empty_vel),sep='_')
      empty_base <- cbind(empty_base,empty_vel.nat)
    }
  }
    
  if('MIGRATORY' %in% names(inTaxa.1)){
    inTaxa.1 <- plyr::mutate(inTaxa.1,MIGR=ifelse(MIGRATORY=='Y',1,NA))
    
    empty_migr <- data.frame(t(rep(NA,3)),stringsAsFactors=F)
    names(empty_migr) <- c('MIGRNTAX','MIGRPIND','MIGRPTAX')
    empty_base <- cbind(empty_base,empty_migr)
    
    if(CALCNAT=='Y'){
      empty_migr.nat <- empty_migr
      names(empty_migr.nat) <- paste('NAT',names(empty_migr),sep='_')
      empty_base <- cbind(empty_base,empty_migr.nat)
    }
  }
  
  if('REPROD' %in% names(inTaxa.1)){
    inTaxa.1 <- plyr::mutate(inTaxa.1,LITH=ifelse(REPROD=='C',1,NA))
    
    empty_repr <- data.frame(t(rep(NA,3)),stringsAsFactors=F)
    names(empty_repr) <- c('LITHNTAX','LITHPIND','LITHPTAX')
    empty_base <- cbind(empty_base,empty_repr)
    
    if(CALCNAT=='Y'){
      empty_repr.nat <- empty_repr
      names(empty_repr.nat) <- paste('NAT',names(empty_repr),sep='_')
      empty_base <- cbind(empty_base,empty_repr.nat)
    }
  }

  if('TEMP' %in% names(inTaxa.1)){
    inTaxa.1 <- plyr::mutate(inTaxa.1,COLD=ifelse(TEMP=='CD',1,NA))
    
    empty_temp <- data.frame(t(rep(NA,3)),stringsAsFactors=F)
    names(empty_temp) <- c('COLDNTAX','COLDPIND','COLDPTAX')
    empty_base <- cbind(empty_base,empty_temp)
    
    if(CALCNAT=='Y'){
      empty_temp.nat <- empty_temp
      names(empty_temp.nat) <- paste('NAT',names(empty_temp),sep='_')
      empty_base <- cbind(empty_base,empty_temp.nat)
    }
  }
  
  params<-c('COLD','LITH','RHEO','LOT','MIGR')
  
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
        
        outWide.1 <- plyr::mutate(outWide.1,NAT_PTAX=round((NAT_TOTLNTAX/TOTLNTAX)*100,2),NAT_PIND=round((NAT_TOTLNIND/TOTLNIND)*100,2)) %>%
          select(-TOTLNTAX,-TOTLNIND)        
        
      }else{
        outWide.1 <- select(outWide,-TOTLNTAX,-TOTLNIND)
      }
    }
  }else{
    outWide.1 <- select(outWide,-TOTLNTAX,-TOTLNIND)
  }
  
  outWide.all <- gtools::smartbind(outWide.1,select(empty_base,-SAMPID)) %>% 
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