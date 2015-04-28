# TODO: Add comment
# 
# Author: ssanroma
###############################################################################




source('L:\\Priv\\CORFiles\\IM\\Rwork\\nrsa\\code\\NRSAvalidation.r')
source('L:\\Priv\\CORFiles\\IM\\Rwork\\SharedCode\\dd.r')

chan <- odbcConnect('nrsa2')
 tblPeriphyton <- fetchNRSATable(chan, 'tblPERIPHYTON', where = "COUNT_TYPE in ('SHORTNORMAL','LIVE-DEAD')")
 tblPeriphyton$tmp <- NULL
 tblTaxa <- fetchNRSATable(chan, 'tblTAXA', where = ("ASSEMBLAGE_NAME = 'ALGAE'"))

 tblPeriphyton <- rename(tblPeriphyton, c('PARAMETER','RESULT'), c('variable','value'))
 tblTaxa <- rename(tblTaxa, c('PARAMETER','RESULT'), c('variable','value'))
 
 firstHalf <- subset(tblPeriphyton, as.numeric(UID) <= 13946)
 aa <- subset(firstHalf, select = c("UID","SAMPLE_CAT","SAMPLE_TYPE","ALG_GROUP","COUNT_TYPE","TAXA_ID","FLAG","variable"))
 bb <- unique(aa)
 cc <- aa[duplicated(aa),]
 
 secondHalf <- subset(tblPeriphyton, as.numeric(UID) > 13946)
 aaa <- subset(firstHalf, select = c("UID","SAMPLE_CAT","SAMPLE_TYPE","ALG_GROUP","COUNT_TYPE","TAXA_ID","FLAG","variable"))
 bbb <- unique(aaa)
 ccc <- aa[duplicated(aaa),]
 
# aa <- unique(tblPeriphyton$UID)
# 
# periData <- cast(tblPeriphyton)
 taxaData <- cast(tblTaxa)
 
 periFirst <- cast(firstHalf)
 periSecond <- cast(secondHalf)
 periData <- as.data.frame(rbind(periFirst,periSecond))
 
 
 periTaxa <- merge(periData, taxaData, all.x = T)
 
 siteInfo <- fetchNRSATable(chan, 'tblVISITS2') 
 siteInfo <- siteInfo[,c('UID','SITE_ID','VISIT_NO','DATE_COL')]
 
 comments <- fetchNRSATable(chan, 'tblCOMMENTS2', where = "SAMPLE_TYPE IN ('PERIW','PERIB')")
 comments <- subset(comments, select = c('UID','SAMPLE_CAT', 'FLAG', 'COMMENT'))
 
 allPeri <- merge(siteInfo,periTaxa, all.y = T)
 allPeri <- merge(allPeri, comments, all.x = T)
 
 periForNARS <- allPeri[,c("UID", "SITE_ID",                  
				 "VISIT_NO", "DATE_COL",                 
				 "TAXA_ID", "NRSA_TAXON_NAME",                     
				 "PHYLUM", "CLASS", "ORDER",                    
				 "FAMILY", "GENUS",  "SAMPLE_CAT",               
				 "SAMPLE_TYPE", "ALG_GROUP",                
				 "COUNT_TYPE",                     
				 "CELLS_BOTTLE", "DEAD_BOTTLE",              
				 "DEAD_CELLS_CNTD", "LIVE_BOTTLE",              
				 "LIVE_CELLS_CNTD", "NO_CELLS_CNTD",            
				 "NO_NAT_UNITS_CNTD", "NO_VALVES_CNTD",           
				 "PROP_BIOVOL_ALL_ALGAE", "PROP_BIOVOL_SOFT_ALGAE",   
				 "PROP_CELLS_ALL_ALGAE", "PROP_CELLS_SOFT_ALGAE",    
				 "PROP_NAT_UNITS_SOFT_ALGAE", "PROP_UNITS_ALL_ALGAE",     
				 "PROP_VALVES_CNTD", "UNITS_BOTTLE",             
				 "ASSEMBLAGE_NAME",                   
				 "LUMP_LEVEL_1", "LUMP_LEVEL_2", "FLAG", "COMMENT"  )]
 
 ##  Prepare to publish to NARS
 
 pD <- ddFetchMetaData('NRSA2', 'tblPARAMETERDESCRIPTIONS2')
 cD <- ddFetchMetaData('NRSA2', 'tblCOLUMNDESCRIPTIONS2')

 periReady <- ddPrepareDataForDistribution(periForNARS)
 periMeta <- ddCreateMetadata(pD, cD, periReady)
# ddWriteData(periReady,periMeta,'NRSA','peri','data')

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

validPeri <- createValid(c('PERI'), c('Lab data - periphyton'))

bubble <- odbcConnect('superbubble')

#  dbUpdate(bubble,'tblVALIDFILES',validPeri, c('FILENAME','DIRECTORY','DATA_TYPE')) 

##  Recreating valid_files from superBubble
newValidFiles <- dbFetch(bubble,'tblVALIDFILES')   
newValidFiles$PUBLICATION_DATE <- format( newValidFiles$PUBLICATION_DATE,"%b %d, %Y")
str(newValidFiles)


# write.table(newValidFiles, file="P:\\dbms\\valid_files",quote=FALSE, sep=":",row.names=FALSE,col.names=FALSE)



