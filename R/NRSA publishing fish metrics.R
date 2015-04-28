# TODO: Add comment
# 
# Author: kblockso
###############################################################################

source('L:\\Priv\\CORFiles\\IM\\Rwork\\nrsa\\code\\NRSAvalidation.r')
source('L:\\Priv\\CORFiles\\IM\\Rwork\\SharedCode\\dd.r')
detach(package:reshape)

chan <- odbcConnect('nrsa2')
 tblFish <- fetchNRSATable(chan, 'tblFISHMET')
 tblFish$tmp <- NULL

 fishData <- dcast(tblFish,UID+SAMPLE_TYPE~PARAMETER,value_var='RESULT')
 
 samps<-unique(subset(fishData,select=c('UID','SAMPLE_TYPE')))
  
 finfo<-fetchNRSATable(chan,'tblFISHINFO2',where="PARAMETER IN('METHOD','NOTFISH','RCH_LENGTH')")
 finfwide<-dcast(finfo,UID+SAMPLE_TYPE+FLAG~PARAMETER,value_var="RESULT")
 sampinfo<-merge(samps,subset(finfwide,select=c('UID','METHOD','NOTFISH','RCH_LENGTH')),by='UID',all=TRUE)
 
 fishData1<-merge(sampinfo,fishData,by=c('UID','SAMPLE_TYPE'),all.x=TRUE)
 
 siteInfo <- fetchNRSATable(chan, 'tblVISITS2') 
 siteInfo <- siteInfo[,c('UID','SITE_ID','VISIT_NO','DATE_COL')]
 
 allFish <- merge(siteInfo,fishData1, all.y = T)
 
 fishForNARS <- allFish
 
 ##  Prepare to publish to NARS
 pD <- ddFetchMetaData('NRSA2', 'tblPARAMETERDESCRIPTIONS2')
 pD<-  subset(pD,SAMPLE_TYPE %in% c('FISHW'))
 
 cD <- ddFetchMetaData('NRSA2', 'tblCOLUMNDESCRIPTIONS2')

 fishReady <- ddPrepareDataForDistribution(fishForNARS)
 fishMeta <- ddCreateMetadata(pD, cD, fishReady)
# ddWriteData(fishReady,fishMeta,'NRSA','fishmet','data')

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

validFish <- createValid(c('FISH'), c('Metric data - Fish'))

bubble <- odbcConnect('superbubble')

#  dbUpdate(bubble,'tblVALIDFILES',validFish, c('FILENAME','DIRECTORY','DATA_TYPE')) 

##  Recreating valid_files from superBubble
newValidFiles <- dbFetch(bubble,'tblVALIDFILES')   
newValidFiles$PUBLICATION_DATE <- format( newValidFiles$PUBLICATION_DATE,"%b %d, %Y")
str(newValidFiles)


# write.table(newValidFiles, file="P:\\dbms\\valid_files",quote=FALSE, sep=":",row.names=FALSE,col.names=FALSE)



