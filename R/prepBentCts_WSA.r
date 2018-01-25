#' @export
#' @title Aggregate benthic macroinvertebrate data for use 
#' with WSA and NRSA benthic metrics and MMIs
#' 
#' @description This function aggregates count data to match
#' those taxonomic levels used in WSA and in the NRSA MMI 
#' 
#' @param inCts A data frame containing, at minimum, the variables 
#' specified in the arguments for sampID, ct, and taxa_id
#' @param inTaxa a data frame containing taxonomic information, 
#' including variables for PHYLUM, CLASS, ORDER, FAMILY, SUBFAMILY, 
#' and TRIBE, as well as autecology traits with names that match those 
#' in the arguments ffg, habit, and ptv. In addition, there
#' should be a variable with the name in argument taxa_id that matches 
#' with all of those in the indf data frame
#' @param sampID A character vector containing the names of all 
#' variables in indf that specify a unique sample. If not specified, 
#' the default is \emph{UID}
#' @param ct A string with the name of the count variable. If not 
#' specified, the default is \emph{TOTAL}.
#' @param taxa_id A string with the name of the taxon ID variable 
#' in \emph{inCts} that matches that in \emph{inTaxa}. The default 
#' value is \emph{TAXA_ID}.
#' @return A data frame containing the \emph{sampID} fields, plus 
#' TAXA_ID, TOTAL, and IS_DISTINCT. Taxonomy is aggregated to the level
#' used for the NRSA MMI and distinctness is recalculated on this
#' aggregated dataset.
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#' 
prepBentCts_WSA <- function(inCts,inTaxa=bentTaxa,sampID='UID',ct='TOTAL'
                            ,taxa_id='TAXA_ID'){
  
  ctVars <- c(sampID,ct,taxa_id)
  if(any(ctVars %nin% names(inCts))){
    msgTraits <- which(ctVars %nin% names(inCts))
    print(paste("Missing variables in input data frame:",paste(names(inCts)[msgTraits],collapse=',')))
    return(NULL)
  }
  
  inCts <- subset(inCts,select=c(sampID,ct,taxa_id))
  
#   for(i in 1:length(sampID)){
#     if(i==1) inCts$SAMPID <- inCts[,sampID[i]]
#     else inCts$SAMPID <- paste(inCts$SAMPID,inCts[,sampID[i]],sep='.')
#   }  
  
  # If necessary, load the bentTaxa data frame and assign it to inTaxa.  Though
  # NON_TARGET taxa are included in the table provided by NRSA, we need to exclude
  # them from our calculations.
  if(is.null(inTaxa)) {
    inTaxa <- bentTaxa
    inTaxa <- subset(inTaxa, is.na(NON_TARGET) | NON_TARGET == "")
  }
  
  ## This code assumes that the following are columns in the taxa file: PHYLUM, CLASS, ORDER, FAMILY, SUBFAMILY, TRIBE, HABIT, FFG, PTV, 
  ##      TARGET_TAXON, 
  ## The two-letter codes for HABIT and FFG are assumed to match those used for NRSA. All names are assumed to be uppercase
  necTraits <- c('PHYLUM','CLASS','ORDER','FAMILY','GENUS','TARGET_TAXON',taxa_id)
  if(any(necTraits %nin% names(inTaxa))){
    msgTraits <- which(necTraits %nin% names(inTaxa))
    return(paste("Some of the traits are missing from the taxa list. The following are \nrequired for metric calculations to run:\n", necTraits[msgTraits], "\n"))
  }
  
  # Rename ct and taxa_id 
  names(inCts)[names(inCts)==ct] <- 'TOTAL'
  names(inCts)[names(inCts)==taxa_id] <- 'TAXA_ID'
  names(inTaxa)[names(inTaxa)==taxa_id] <- 'TAXA_ID'
  
  inTaxa.1 <- dplyr::select(inTaxa,TAXA_ID,TARGET_TAXON,PHYLUM,CLASS,ORDER,FAMILY,GENUS)
  
  # Must first create input dataset using WSA taxonomy and traits
  inCts.1 <- merge(inCts,inTaxa.1,by=c('TAXA_ID')) %>%
    plyr::mutate(TOTAL=as.numeric(TOTAL)) %>%
    filter(TOTAL>0)
  ## Roll mites, oligochaetes, and polychaetes up to family level
  fixTaxa <- with(inCts.1,which(CLASS %in% c('ARACHNIDA','POLYCHAETA','OLIGOCHAETA') & !is.na(FAMILY) & FAMILY!=''))

  inCts.1$TARGET_TAXON[fixTaxa] <- inCts.1$FAMILY[fixTaxa]
  
  inCts.2 <- merge(inCts.1,subset(inTaxa.1,select=c('TAXA_ID','TARGET_TAXON')),by='TARGET_TAXON',all.x=TRUE) %>%
    plyr::rename(c('TAXA_ID.y'='TAXA_ID')) %>% 
    plyr::mutate(TAXA_ID=ifelse(TARGET_TAXON %in% c('CRICOTOPUS/ORTHOCLADIUS','THIENEMANNIMYIA GENUS GR.'),3581
                                ,ifelse(TARGET_TAXON %in% c('CERATOPOGONINAE'),3566,TAXA_ID))
                 ,TARGET_TAXON=ifelse(TAXA_ID==3581,'CHIRONOMIDAE',ifelse(TAXA_ID==3566,'CERATOPOGONIDAE',TARGET_TAXON)))   
  
  inCts.3 <- plyr::ddply(inCts.2,c(sampID,'TAXA_ID'),summarise,TOTAL=sum(TOTAL)) %>%
    merge(inTaxa.1,by='TAXA_ID')
  
  inCts.4 <- assignDistinct(inCts.3,c(sampID),taxlevels=c('PHYLUM','CLASS','ORDER','FAMILY','GENUS')
                            ,final.name='TARGET_TAXON'
                            ,special.taxa=c('THIENEMANNIMYIA GENUS GR.', 'CERATOPOGONINAE', 'CRICOTOPUS/ORTHOCLADIUS')) %>% 
    plyr::mutate(IS_DISTINCT=ifelse(is.na(IS_DISTINCT),0,IS_DISTINCT))
  
  outCts <- subset(inCts.4,select=c(sampID,'TAXA_ID','TOTAL','IS_DISTINCT'))

  return(outCts)
}