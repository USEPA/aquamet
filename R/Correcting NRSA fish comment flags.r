# TODO: Add comment
# 
# Author: ssanroma
###############################################################################

source('L:\\Priv\\CORFiles\\IM\\Rwork\\nrsa\\code\\NRSAvalidation.r')

nrsa <- odbcConnect('NRSA')
nrsa2 <- odbcConnect('NRSA2')

fishcom <- fetchNRSATable(nrsa2, 'tblCOMMENTS2', where = "SAMPLE_TYPE in 
	('FISHB','FISHW','FISH','FISHINF','FISHTV')")

fishcom2B <- fetchNRSATable(nrsa2, 'tblCOMMENTS2', where = "SAMPLE_TYPE = 'FISHB'")
fishcom2W <- fetchNRSATable(nrsa2, 'tblCOMMENTS2', where = "SAMPLE_TYPE = 'FISHW'")

fishcomb <- sqlFetch(nrsa,'FISHCOMB', stringsAsFactors = F) 
fishcomb <- rename(fishcomb, 'BATCHNO','UID')
fishcomb$FLAG <- trimws(fishcomb$FLAG)
fishcomb$TRANSECT <- trimws(fishcomb$TRANSECT)

fishcomw <- sqlFetch(nrsa,'FISHCOMW', stringsAsFactors = F) 
fishcomw <- rename(fishcomw, 'BATCHNO','UID')
fishcomw$FLAG <- trimws(fishcomw$FLAG)

## Merging wadeable comments to find correct FLAG
zz <- merge(fishcom, fishcomw, by = c('UID','COMMENT','PAGE'))
zz$FLAG <- ifelse(zz$FLAG.y != '', zz$FLAG.y, NA)
zz$REASON <- 'FLAG value was not properly brought over when converted from NRSA.'
insertW <- subset(zz, select = c('UID','SAMPLE_TYPE','TRANSECT','STATION','FLAG','COMMENT','PAGE','REASON','SAMPLE_ID','SAMPLE_CAT','SAM_CODE'))
insertW <- rename(insertW, c('UID'), c('BATCHNO'))

deleteW <- subset(zz, select = c('UID','SAMPLE_TYPE','TRANSECT','STATION','FLAG.x','COMMENT','PAGE','REASON','SAMPLE_ID','SAMPLE_CAT','SAM_CODE'))
deleteW <- rename(deleteW, c('UID','FLAG.x'), c('BATCHNO','FLAG'))

## Merging boatable comments to find correct FLAG
yy <- merge(fishcom, fishcomb, by = c('UID','COMMENT','TRANSECT','PAGE'))
yy$FLAG <- yy$FLAG.y
yy$REASON <- 'FLAG value was not properly brought over when converted from NRSA.'
insertB <- subset(yy, select = c('UID','SAMPLE_TYPE','TRANSECT','STATION','FLAG','COMMENT','PAGE','REASON','SAMPLE_ID','SAMPLE_CAT','SAM_CODE'))
insertB <- rename(insertB, c('UID'), c('BATCHNO'))

deleteB <- subset(yy, select = c('UID','SAMPLE_TYPE','TRANSECT','STATION','FLAG.x','COMMENT','PAGE','REASON','SAMPLE_ID','SAMPLE_CAT','SAM_CODE'))
deleteB <- rename(deleteB, c('UID','FLAG.x'), c('BATCHNO','FLAG'))

## Creating combined files to delete and insert
comDelete <- rbind(deleteW, deleteB)
comDelete$SAMPLE_CAT <- as.character(comDelete$SAMPLE_CAT)
comDelete$SAMPLE_ID <- as.character(comDelete$SAMPLE_ID)
comDelete$SAM_CODE <- as.character(comDelete$SAM_CODE)
comDelete$BATCHNO <- as.numeric(comDelete$BATCHNO)

comInsert <- rbind(insertW, insertB)

## Deleting from NRSA2 COMMENTS2
dbDelete(nrsa2, 'tblCOMMENTS2', comDelete)

## Inserting into NRSA tblCOMMENTS2
dbInsert(nrsa2, 'tblCOMMENTS2', comInsert, c('BATCHNO','SAMPLE_TYPE','TRANSECT'
						,'STATION','PAGE','SAMPLE_CAT','SAMPLE_CODE','FLAG'))


