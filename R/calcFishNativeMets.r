#' @export
#' @title Calculate all native status metrics
#' @description This function calculates all of the metrics based
#' only on native status, including alien metrics.
#' @param indata Input data frame containing variables as identified  
#' in the arguments for \emph{sampID}, \emph{dist}, \emph{ct}, 
#' \emph{taxa_id}. 
#' @param sampID sampID A character vector containing the names of all 
#' variables in indf that specify a unique sample. If not specified, 
#' the default is \emph{UID}.
#' @param dist A string with the name of the distinctness variable, 
#' which is assumed to have only values of 0 or 1. If not specified, 
#' the default is \emph{IS_DISTINCT}.
#' @param ct A string with the name of the count variable. If not 
#' specified, the default is \emph{TOTAL}.
#' @param taxa_id A string with the name of the taxon variable 
#' in \emph{indata}. The default value is \emph{TAXA_ID}.
#' @param nonnat A string with the name of the optional variable in 
#' \emph{inCts} containing non-native status. Valid values are 'Y' for 
#' non-native and 'N' for native. The default name 
#' is \emph{NONNATIVE}.
#' @return A data frame containing the variables in sampID and 
#' the fish tolerance metrics as additional variables. The names of
#' metrics include  NAT_TOTLNTAX, NAT_PTAX, NAT_PIND, ALIENNTAX, 
#' ALIENPTAX, ALIENPIND.
#' 
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#'  
calcFishNativeMets <- function(indata, sampID='UID', dist='IS_DISTINCT'
                              , ct='TOTAL', taxa_id='TAXA_ID', nonnat='NONNATIVE'){
  
  ctVars <- c(sampID,dist,ct,taxa_id,nonnat)
  if(any(ctVars %nin% names(indata))){
    msgTraits <- which(ctVars %nin% names(indata))
    print(paste("Missing variables in input data frame:",paste(ctVars[msgTraits],collapse=',')))
    return(NULL)
  }
  
  # Combine all values in sampID into one sampID in df
  for(i in 1:length(sampID)){
    if(i==1) indata$SAMPID <- indata[,sampID[i]]
    else indata$SAMPID <- paste(indata$SAMPID,indata[,sampID[i]],sep='.')
  }
  # Keep data frame with crosswalk info between sampID and SAMPID
  samples <- unique(subset(indata,select=c(sampID,'SAMPID')))
  
  # Rename counts and distinct variables to FINAL_CT and IS_DISTINCT
  names(indata)[names(indata)==ct] <- 'FINAL_CT'
  names(indata)[names(indata)==dist] <- 'IS_DISTINCT'
  names(indata)[names(indata)==taxa_id] <- 'TAXA_ID'
  names(indata)[names(indata)==nonnat] <- 'NONNATIVE'
  
  ## for inCts1, keep only observations without missing or zero FINAL_CT values or TAXA_ID and TAXA_ID!=99999
  indata.1 <- subset(indata,!is.na(TAXA_ID) & !is.na(FINAL_CT) & FINAL_CT!=0)
  
  indata.2 <- plyr::ddply(indata.1,c('SAMPID','TAXA_ID','NONNATIVE'),summarise,IS_DISTINCT=max(IS_DISTINCT),FINAL_CT=sum(FINAL_CT))

  # Make sure all necessary columns in inCts are numeric
  inCts <- plyr::mutate(indata.2,FINAL_CT=as.numeric(FINAL_CT),IS_DISTINCT=as.integer(IS_DISTINCT))
  
  if(any(unique(inCts$NON_NATIVE) %nin% c('Y','N'))){
    return(print("No native and alien datasets were created because NON_NATIVE must only be 'Y' or 'N' values"))     
  }
  
  inCts.1 <- plyr::ddply(inCts.1, "SAMPID", mutate, TOTLNIND=sum(FINAL_CT),TOTLNTAX=sum(IS_DISTINCT)) %>%
    mutate(ALIEN=ifelse(NONNATIVE=='Y',1,NA),NAT=ifelse(NONNATIVE=='N',1,NA))
  
  empty_base <- data.frame(t(rep(NA,7)),stringsAsFactors=F)
  names(empty_base) <- c('NAT_TOTLNIND','NAT_TOTLNTAX','NAT_PIND','NAT_PTAX','ALIENPIND','ALIENNTAX','ALIENPTAX')
  
  outMet <- plyr::ddply(inCts.1, c("SAMPID"), summarise,
                        ALIENNTAX=sum(IS_DISTINCT*ALIEN),
                        ALIENPIND=round(sum(FINAL_CT*ALIEN/TOTLNIND)*100,2),
                        ALIENPTAX=round(sum(IS_DISTINCT*ALIEN/TOTLNTAX)*100,2),
                        NAT_TOTLNTAX=sum(IS_DISTINCT*NAT),
                        NAT_TOTLNIND=sum(FINAL_CT*NAT),
                        NAT_PIND=round(sum(FINAL_CT*NAT)/TOTLNIND*100,2),
                        NAT_PTAX=round(sum(IS_DISTINCT*NAT)/TOTLNTAX*100,2),.progress='tk') 
      
        
 ## START HERE!!! Do we even need the empty_base data frame? All sites should be represented in outMet with at least some value for all 
 # samples - may be missing though
  outMet.1 <- gtools::smartbind(outMet,select(empty_base,-SAMPID)) %>% 
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