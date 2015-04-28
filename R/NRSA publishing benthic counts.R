# TODO: Add comment
# 
# Author: kblockso
###############################################################################

source('L:\\Priv\\CORFiles\\IM\\Rwork\\nrsa\\code\\NRSAvalidation.r')
source('L:\\Priv\\CORFiles\\IM\\Rwork\\SharedCode\\dd.r')
detach("package:reshape")

chan <- odbcConnect('nrsa2')
 tblBenthic <- fetchNRSATable(chan, 'tblBENTCNT2')
 tblBenthic$tmp <- NULL
 tblTaxa <- fetchNRSATable(chan, 'tblTAXA', where = ("ASSEMBLAGE_NAME = 'INVERTEBRATES' AND 
					PARAMETER IN('FFG','HABIT','PTV','PHYLUM','CLASS','ORDER','FAMILY','GENUS','NON_TARGET','TARGET_TAXON')"))

 bugData <- dcast(tblBenthic,UID+SAMPLE_TYPE+SAMPLE_CAT+TAXA_ID~PARAMETER,value_var='RESULT')
 taxaData <- dcast(tblTaxa,TAXA_ID+ASSEMBLAGE_NAME~PARAMETER,value_var='RESULT')
 
 bentTaxa <- merge(bugData, taxaData, all.x = T)
 bentTaxa<-  subset(bentTaxa,is.na(NON_TARGET))
 
 siteInfo <- fetchNRSATable(chan, 'tblVISITS2') 
 siteInfo <- siteInfo[,c('UID','SITE_ID','VISIT_NO','DATE_COL')]
 
 allBent <- merge(siteInfo,bentTaxa, all.y = T)
 
 
 bentForNARS <- allBent[,c("UID", "SITE_ID","VISIT_NO", "DATE_COL","SAMPLE_TYPE","SAMPLE_CAT"                 
				 ,"TAXA_ID", "TARGET_TAXON","TOTAL","IS_DISTINCT","TOTAL300","IS_DISTINCT300"
				 ,"PHYLUM", "CLASS", "ORDER","FAMILY", "GENUS","FFG","HABIT","PTV")]
 
 ##  Prepare to publish to NARS
 pD <- ddFetchMetaData('NRSA2', 'tblPARAMETERDESCRIPTIONS2')
 pD<-  subset(pD,SAMPLE_TYPE %in% c('TAXA: Invertebrates','BERWW'))
 
 cD <- ddFetchMetaData('NRSA2', 'tblCOLUMNDESCRIPTIONS2')

 bentReady <- ddPrepareDataForDistribution(bentForNARS)
 bentMeta <- ddCreateMetadata(pD, cD, bentReady)
# ddWriteData(bentReady,bentMeta,'NRSA','bentcts','data')

##  Updating super bubble

createValid <- function(filename,fileDescription, reason = as.character(NA)){
	directory <- 'nrsa'
	studyDescription <- 'National Rivers and Streams Assessment 2008-09'
	dateCreated <- Sys.Date()
	dataType <- 'Validated/Metric Data'
	studyYear <- '2008-09'
	reason <- 'New data entered.'
	
	superBubble <- data.frame(FILENAME = filename
			,DIRECTORY = directory
			,STUDY_DESCRIPTION =studyDescription
			,FILE_DESCRIPTION = fileDescription
			,PUBLICATION_DATE = dateCreated
			,DATA_TYPE = dataType
			,STUDY_YEAR = studyYear
			,REASON = reason
			,stringsAsFactors = FALSE)
	return(superBubble)
}  

validBent <- createValid(c('BENTCTS'), c('Count data - benthic macroinvertebrates'))

bubble <- odbcConnect('superbubble')

#  dbInsert(bubble,'tblVALIDFILES',validBent, c('FILENAME','DIRECTORY','DATA_TYPE')) 

##  Recreating valid_files from superBubble
newValidFiles <- dbFetch(bubble,'tblVALIDFILES')   
newValidFiles$PUBLICATION_DATE <- format( newValidFiles$PUBLICATION_DATE,"%b %d, %Y")
str(newValidFiles)


# write.table(newValidFiles, file="P:\\dbms\\valid_files",quote=FALSE, sep=":",row.names=FALSE,col.names=FALSE)



bob <- gsub( '\\n',"WOOHOO", comments$COMMENT)

