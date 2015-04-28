# TODO: Add comment
# 
# Author: kblockso
###############################################################################

source('L:\\Priv\\CORFiles\\IM\\Rwork\\nrsa\\code\\NRSAvalidation.r')
source('L:\\Priv\\CORFiles\\IM\\Rwork\\SharedCode\\dd.r')
detach("package:reshape")

chan <- odbcConnect('nrsa2')
 tblBenthic <- fetchNRSATable(chan, 'tblBENTMET')
 tblBenthic$tmp <- NULL

 bentData <- dcast(tblBenthic,UID+SAMPLE_TYPE+SAMPLE_CAT~PARAMETER,value_var='RESULT')
 
 siteInfo <- fetchNRSATable(chan, 'tblVISITS2') 
 siteInfo <- siteInfo[,c('UID','SITE_ID','VISIT_NO','DATE_COL')]
 
 allBent <- merge(siteInfo,bentData, all.y = T)
 
 bentForNARS <- allBent
 
 ##  Prepare to publish to NARS
 pD <- ddFetchMetaData('NRSA2', 'tblPARAMETERDESCRIPTIONS2')
 pD<-  subset(pD,SAMPLE_TYPE %in% c('BERWW'))
 
 cD <- ddFetchMetaData('NRSA2', 'tblCOLUMNDESCRIPTIONS2')

 bentReady <- ddPrepareDataForDistribution(bentForNARS)
 bentMeta <- ddCreateMetadata(pD, cD, bentReady)
# ddWriteData(bentReady,bentMeta,'NRSA','bentmet','data')

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

validBent <- createValid(c('BENTMET'), c('Metric data - benthic macroinvertebrates'))

bubble <- odbcConnect('superbubble')

#  dbInsert(bubble,'tblVALIDFILES',validBent, c('FILENAME','DIRECTORY','DATA_TYPE')) 

##  Recreating valid_files from superBubble
newValidFiles <- dbFetch(bubble,'tblVALIDFILES')   
newValidFiles$PUBLICATION_DATE <- format( newValidFiles$PUBLICATION_DATE,"%b %d, %Y")
str(newValidFiles)


# write.table(newValidFiles, file="P:\\dbms\\valid_files",quote=FALSE, sep=":",row.names=FALSE,col.names=FALSE)



bob <- gsub( '\\n',"WOOHOO", comments$COMMENT)

