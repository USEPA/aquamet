#' @export
#' @title Calculate all taxonomy-related metrics
#' @description This function calculates all of the taxonomy metrics,
#' and if additional trait values are included in the taxalist (habitat,
#' trophic, migr). Native status versions of metrics are calculated
#' if a non-native status variable is included in the input count data. 
#' @param indata Input data frame containing variables as identified  
#' in the arguments for \emph{sampID}, \emph{dist}, \emph{ct}, 
#' \emph{taxa_id}, as well as the optional variable 
#' for non-native status in \emph{nonnat}. 
#' @param inTaxa Data frame containing fish taxalist, along with autecology
#' traits. At a minimum, this taxalist must contain variables matching 
#' the argument for \emph{family}, \emph{genus}, and \emph{comname}. 
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
#' @param nonnat A string with the name of the optional variable in 
#' \emph{inCts} containing non-native status. Valid values are 'Y' for 
#' non-native and 'N' for native. The default name 
#' is \emph{NONNATIVE}.
#' @param family A string with the name of the variable in the 
#' \emph{inTaxa} taxalist containing family name. The default value 
#' is \emph{FAMILY}.
#' @param genus A string with the name of the variable in the 
#' \emph{inTaxa} taxalist containing the genus name. The default
#' value is \emph{GENUS}
#' @param comname A string with the name of the variable in the 
#' \emph{inTaxa} taxalist containing the common name. The 
#' default value is \emph{NAME}.
#' @return A data frame containing the variables in sampID and 
#' the fish tolerance metrics as additional variables. Metric 
#' descriptions are included in \emph{NRSA_Fish_Metric_Descriptions.pdf},
#' included in this package. The names of
#' metrics include  SALMPIND, SALMNTAX, SALMPTAX, ICTAPIND, ICTANTAX, 
#' ICTAPTAX, CATOPIND, CATONTAX, CATOPTAX, RBCATOPIND, RBCATONTAX, 
#' RBCATOPTAX, CENTPIND, CENTNTAX, CENTPTAX, CYPRPIND, CYPRNTAX, CYPRPTAX,
#' TOTLNTAX, and TOTLNIND. 
#' 
#' If a non-native status variable is included, these metrics are also
#' calculated:
#' NAT_SALMPIND, NAT_SALMNTAX, NAT_SALMPTAX, NAT_ICTAPIND, NAT_ICTANTAX, 
#' NAT_ICTAPTAX, NAT_CATOPIND, NAT_CATONTAX, NAT_CATOPTAX, NAT_RBCATOPIND, 
#' NAT_RBCATONTAX, NAT_RBCATOPTAX, NAT_CENTPIND, NAT_CENTNTAX, NAT_CENTPTAX, 
#' NAT_CYPRPIND, NAT_CYPRNTAX, NAT_CYPRPTAX, NAT_TOTLNTAX, NAT_TOTLNIND,
#' NAT_PIND, NAT_PTAX. 
#' 
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#'  
calcFishTaxMets <- function(indata, inTaxa=NULL, sampID='UID', dist='IS_DISTINCT'
                            , ct='TOTAL', taxa_id='TAXA_ID', nonnat='NONNATIVE'
                            , family='FAMILY', genus='GENUS',comname='NAME'){
  
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
  necTraits <- c(family, genus, comname)
  if(any(necTraits %nin% names(inTaxa))){
    msgTraits <- which(necTraits %nin% names(inTaxa))
    return(paste("Some of the traits are missing from the taxa list. The following are \nrequired for metric calculations to run:", necTraits[msgTraits]))
  }
  
  inTaxa <- subset(inTaxa,select=names(inTaxa) %in% c(taxa_id,family,genus,comname))  
  
  # Rename counts and distinct variables to FINAL_CT and IS_DISTINCT
  names(indata)[names(indata)==ct] <- 'FINAL_CT'
  names(indata)[names(indata)==dist] <- 'IS_DISTINCT'
  names(indata)[names(indata)==taxa_id] <- 'TAXA_ID'
  names(indata)[names(indata)==nonnat] <- 'NONNATIVE'

  names(inTaxa)[names(inTaxa)==taxa_id] <- 'TAXA_ID'
  names(inTaxa)[names(inTaxa)==family] <- 'FAMILY'
  names(inTaxa)[names(inTaxa)==genus] <- 'GENUS'
  names(inTaxa)[names(inTaxa)==comname] <- 'NAME'
  
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
  inTaxa.1 <- plyr::mutate(inTaxa,SALM=ifelse(toupper(FAMILY)=='SALMONIDAE',1,NA)
                     ,ICTA=ifelse(toupper(FAMILY)=='ICTALURIDAE',1,NA)
                     ,CATO=ifelse(toupper(FAMILY)=='CATOSTOMIDAE',1,NA)
                     ,RBCATO=ifelse(toupper(GENUS) %in% c('MOXOSTOMA', 'HYPENTELIUM'
                                  , 'MINYTREMA', 'ERIMYZON' , 'CATOSTOMUS', 'CYCLEPTUS'
                                  , 'PANTOSTEUS' , 'THOBURNIA'),1,NA)
                     ,CENT=ifelse(toupper(FAMILY)=='CENTRARCHIDAE' & ((toupper(GENUS)!='MICROPTERUS')|is.na(GENUS)),1,NA)
                     ,CYPR=ifelse(toupper(FAMILY)=='CYPRINIDAE' & toupper(NAME) %nin% 
                                    c('COMMON CARP','GOLDFISH','BIGHEAD CARP','GRASS CARP','MIRROR CARP'),1,NA)
                     ) 
  # Create empty data frames with all metric names in it
  empty_base <- data.frame(t(rep(NA,18)),stringsAsFactors=F)
  names(empty_base) <- c('SALMPIND','SALMNTAX','SALMPTAX'
                         ,'ICTAPIND','ICTANTAX','ICTAPTAX'
                         ,'CATOPIND','CATONTAX','CATOPTAX'
                         ,'RBCATOPIND','RBCATONTAX','RBCATOPTAX'
                         ,'CENTPIND','CENTNTAX','CENTPTAX'
                         ,'CYPRPIND','CYPRNTAX','CYPRPTAX')
    
  
  # Add native metrics if CALCNAT='Y'
  if(CALCNAT=='Y'){
    empty_base.nat <- empty_base
    names(empty_base.nat) <- paste('NAT',names(empty_base),sep='_')
    empty_base <- cbind(empty_base,empty_base.nat)
  }
  
  params<-c('SALM','ICTA','CATO','RBCATO','CENT','CYPR')
  
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