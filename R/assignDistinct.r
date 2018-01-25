#' @export
#' 
#' @title Assign distinctness to each taxon
#' @description This function evaluates taxonomic distinctness 
#' within samples and assigns an indicator of distinctness 
#' (IS_DISTINCT) to each taxon (0 or 1) in each sample, based 
#' on distinctness within that sample.  Input data is expected 
#' to be count data with all counts above 0. 
#' @param cc Data frame containing all taxonomic fields specified
#' in the argument \emph{taxlevels} and the variables specified in 
#' the argument \emph{sampleID}.
#' @param sampleID A character vector containing the names of all 
#' variables in \emph{cc} that specify a unique sample. If not specified, 
#' the default is \emph{UID}
#' @param taxlevels A character vector containing the list of taxonomic
#' levels in data, in order from broadest to most specific
#' @param final.name An optional string containing the name of the column containing
#' the final taxon name for each record. This argument is only required if
#' the \emph{special.taxa} argument is provided. 
#' @param special.taxa An optional character vector of taxon names that are considered
#' the lowest or target taxonomic level, even if they do not fit into the
#' hierarchical taxonomic level provided. These taxa are assigned an 
#' IS_DISTINCT value of 1.
#' @return A data frame equivalent to the input data frame (cc) 
#' with IS_DISTINCT field added.
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#' @examples
#' \dontrun{
#'   data(distEx)
#'   head(distEx)
#'   # Assign distinctness to each taxon within samples
#'   distEx.1 <- assignDistinct(distEx,sampleID=c('UID','SAMPLE_TYPE'),
#'   taxlevels=c('PHYLUM','CLASS','ORDER','FAMILY','GENUS'),
#'   final.name='TARGET_TAXON',
#'   special.taxa=c('THIENEMANNIMYIA GENUS GR.', 'CERATOPOGONINAE', 'CRICOTOPUS/ORTHOCLADIUS'))
#'   
#'   head(distEx.1)
#'   }
#' @keywords survey

assignDistinct <- function(cc, sampleID='UID',taxlevels,final.name=NULL,special.taxa=NULL) {

  for(i in 1:length(sampleID)){
    if(i==1) cc$SAMP_ID <- cc[,sampleID[i]]
    else cc$SAMP_ID <- paste(cc$SAMP_ID,cc[,sampleID[i]],sep='.')
  }      
  
  # Set all variable names in dataset and arguments to uppercase
  names(cc) <- toupper(names(cc))
  taxlevels <- toupper(taxlevels)
  if(!is.null(final.name)){
    final.name <- toupper(final.name)
  }
  if(!is.null(special.taxa)){
    special.taxa <- toupper(special.taxa)
  }
  # Create new data frame from original input and add empty IS_DISTINCT variable
  cc.1 <- cc
  cc.1$IS_DISTINCT <- NA
  
  # Set all names to uppercase
  for(f in 1:length(taxlevels)){
    cc.1[,taxlevels[f]] <- toupper(cc.1[,taxlevels[f]])

  }
  
  # First count number of taxa at each taxonomic level in taxlevels argument
  for(i in 1:length(taxlevels)){
    print(taxlevels[i])
    # freqLevel <- as.data.frame(table(SAMP_ID=cc.1$SAMP_ID, TAXON=cc.1[,taxlevels[i]])
    #                            ,responseName=paste('n',taxlevels[i],sep=''),stringsAsFactors=F) %>%
    #   plyr::rename(c('TAXON'=taxlevels[i]))
    # cc.1 <- merge(cc.1, freqLevel, by=c('SAMP_ID',taxlevels[i]),all.x=T)
    freqLevel <- ddply(cc.1,c('SAMP_ID',taxlevels[1:i]),summarise,n=length(SAMP_ID)) %>%
      plyr::rename(c('n'=paste('n',taxlevels[i],sep='')))

    cc.1 <- merge(cc.1,freqLevel,by=c('SAMP_ID',taxlevels[1:i]),all.x=T)

  }
  
  # Now run through each taxon level to assign IS_DISTINCT
  
  # First account for cases where the final.name is in the list of taxa in special.taxa (if not null)
  if(!is.null(special.taxa)){
    whichSpec <- with(cc.1,which(eval(as.name(final.name)) %in% special.taxa))
    cc.1$IS_DISTINCT[whichSpec] <- 1
  }
  
  
  for(j in length(taxlevels):1){
    print(taxlevels[j])
    nTax <- paste('n',taxlevels[j],sep='')
    whichVal <- which(!is.na(cc.1[,taxlevels[j]]) & cc.1[,taxlevels[j]]!='' & is.na(cc.1$IS_DISTINCT) & cc.1[,nTax]==1)
    cc.1$IS_DISTINCT[whichVal] <- 1

    whichVal.1 <- which(!is.na(cc.1[,taxlevels[j]]) & cc.1[,taxlevels[j]]!='' & is.na(cc.1$IS_DISTINCT) & cc.1[,nTax]>1)
    cc.1$IS_DISTINCT[whichVal.1] <- 0
  }
  
  
  outdata<-subset(cc.1,select=c(names(cc),'IS_DISTINCT')) %>%
    dplyr::select(-SAMP_ID)
  
  return(outdata)
}
