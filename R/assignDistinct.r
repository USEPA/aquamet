#' @export
#' 
#' @title Assign distinctness to each taxon
#' @description This function evaluates taxonomic distinctness 
#' within samples and assigns an indicator of distinctness 
#' (IS_DISTINCT) to each taxon (0 or 1) in each sample, based 
#' on distinctness within that sample.  Input data is expected 
#' to be count data with all counts above 0. The input data frame 
#' also needs to include TARGET_TAXON as the field containing the 
#' taxon name, TAXA_ID as the taxonomic identification number, and 
#' the following taxonomic fields fully populated (as much as 
#' possible, depending on taxonomic level of TARGET_TAXON: 
#' PHYLUM, CLASS, ORDER, FAMILY, and GENUS.
#' 
#' For the purposes of macroinvertebrates, the code assumes that 
#' taxa with TARGET_TAXON values of 'THIENEMANNIMYIA GENUS GR.', 
#' 'CERATOPOGONINAE', or 'CRICOTOPUS/ORTHOCLADIUS' are at the lowest 
#' level and are assigned a value of 1 for IS_DISTINCT.
#' @param cc Data frame containingtaxonomic fields for PHYLUM, 
#' CLASS, ORDER, FAMILY, and GENUS in addition to TAXA_ID, 
#' TARGET_TAXON, and the variables specified in the argument 
#' \emph{sampleID}.
#' @param sampleID A character vector containing the names of all 
#' variables in indf that specify a unique sample. If not specified, 
#' the default is \emph{UID}
#' @return A data frame equivalent to the input data frame (cc) 
#' with IS_DISTINCT field added.
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#' @examples
#' \dontrun{
#'   data(distEx)
#'   head(distEx)
#'   # Assign distinctness to each taxon within samples
#'   distEx.1 <- assignDistinct(distEx)
#'   head(distEx.1)
#'   }
#' @keywords survey

assignDistinct <- function(cc, sampleID='UID') {

################################################################################
# Function: assignDistinct
# Title: Assign Distinct Taxa Name to Samples
# Programmer: Karen Blocksom
# Date: January 27, 2012
# Description:
#   This function evaluates taxonomic distinctness within samples and assigns an 
# indicator of distinctness (IS_DISTINCT) to each taxon (0 or 1) in each sample,
# based on distinctness within that sample.  Input data is expected to be count
# data and is assumed to include a single variable named SAMP_ID that uniquely 
# identifies samples. The input data frame also needs to include TARGET_TAXON
# as the field containing the taxon name, TAXA_ID as the taxonomic
# identification number, and the following taxonomic fields fully populated (as
# much as possible, depending on taxonomic level of TARGET_TAXON: PHYLUM, CLASS,
# ORDER, FAMILY, and GENUS.
#                                                                            
#   For the purposes of macroinvertebrates, the code assumes that taxa with 
# TARGET_TAXON values of 'THIENEMANNIMYIA GENUS GR.', 'CERATOPOGONINAE',
# or 'CRICOTOPUS/ORTHOCLADIUS' are at the lowest level and are assigned a
# value of 1 for IS_DISTINCT.
#
# Function Revisions:
#   01/27/12 kab: Created as EvaluateDistinct.R to evaluate distinctness of
#                 taxa, assuming the use of TARGET_TAXON as the taxa name.
#   08/09/13 kab: Modified to change SAMP_ID to SAMPID to match other metric
#                 code.
#   7/21/2016 kab: Modified arguments to include sampID field so that it's 
#                 easier to call this function with any dataset, changed
#                 SAMPID back to SAMP_ID to distinguish it from outside the
#                 function.
# Arguments:
#   cc = a data frame containing taxonomic fields for PHYLUM, CLASS, ORDER,
#     FAMILY, and GENUS in addition to TAXA_ID and TARGET_TAXON.
#   sampID = vector of key variable names identifying samples, with default
#           of 'UID'
# Output:
#   A data frame containing distinct taxa names.
#
################################################################################
    for(i in 1:length(sampleID)){
      if(i==1) cc$SAMP_ID <- cc[,sampleID[i]]
      else cc$SAMP_ID <- paste(cc$SAMP_ID,cc[,sampleID[i]],sep='.')
    }      
    
		cc$IS_DISTINCT<-NA
		freqFam<-as.data.frame(table(SAMP_ID=cc$SAMP_ID, FAMILY=cc$FAMILY),
		   responseName="nFam", stringsAsFactors=FALSE)
		freqFam<-subset(freqFam,nFam>0)
		cc<-merge(cc,freqFam,by=c('SAMP_ID','FAMILY'),all.x=TRUE)
		freqOrd<-as.data.frame(table(SAMP_ID=cc$SAMP_ID, ORDER=cc$ORDER),
		   responseName="nOrd", stringsAsFactors=FALSE)
		freqOrd<-subset(freqOrd,nOrd>0)
		cc<-merge(cc,freqOrd,by=c('SAMP_ID','ORDER'),all.x=TRUE)
		freqCls<-as.data.frame(table(SAMP_ID=cc$SAMP_ID,CLASS=cc$CLASS),
		   responseName="nCls", stringsAsFactors=FALSE)
		freqCls<-subset(freqCls,nCls>0)
		cc<-merge(cc,freqCls,by=c('SAMP_ID','CLASS'),all.x=TRUE)
		freqPhy<-as.data.frame(table(SAMP_ID=cc$SAMP_ID, PHYLUM=cc$PHYLUM),
		   responseName="nPhy", stringsAsFactors=FALSE)
		freqPhy<-subset(freqPhy,nPhy>0)
		cc<-merge(cc,freqPhy,by=c('SAMP_ID','PHYLUM'),all.x=TRUE)

		whichGen<-with(cc,which(TARGET_TAXON==GENUS|TARGET_TAXON %in%
		   c('THIENEMANNIMYIA GENUS GR.', 'CERATOPOGONINAE',
       'CRICOTOPUS/ORTHOCLADIUS')))
		cc$IS_DISTINCT[whichGen]<-1
		whichFam<-with(cc,which(TARGET_TAXON==FAMILY & nFam==1))
		cc$IS_DISTINCT[whichFam]<-1
		whichFam1<-with(cc,which(TARGET_TAXON==FAMILY & nFam>1))
		cc$IS_DISTINCT[whichFam1]<-0 
		whichOrd<-with(cc,which(TARGET_TAXON==ORDER & nOrd==1))
		cc$IS_DISTINCT[whichOrd]<-1
		whichOrd1<-with(cc,which(TARGET_TAXON==ORDER & nOrd>1))
		cc$IS_DISTINCT[whichOrd1]<-0
		whichCls<-with(cc,which(TARGET_TAXON==CLASS & nCls==1))
		cc$IS_DISTINCT[whichCls]<-1
		whichCls1<-with(cc,which(TARGET_TAXON==CLASS & nCls>1))
		cc$IS_DISTINCT[whichCls1]<-0
		whichPhy<-with(cc,which(TARGET_TAXON==PHYLUM & nPhy==1))
		cc$IS_DISTINCT[whichPhy]<-1
		whichPhy1<-with(cc,which(TARGET_TAXON==PHYLUM & nPhy>1))
		cc$IS_DISTINCT[whichPhy1]<-0
		
		outdata<-subset(cc,select=names(cc) %nin% c('SAMP_ID','nFam','nOrd','nCls','nPhy'))

	return(outdata)
}
