# valLegalCheck.r
#
# 10/30/09 cws Created
# 11/16/09 cws Added use of SAMPLE_TYPE in metadata and standardized metadata
#          to upper case names.  Separated file/table io from the work that's
#          easy to test.
# 12/23/09 cws Added SAMPLE_TYPE to expected results in unit test.
#  6/11/10 cws Unit test now including UID in expected return value using
#          imperfect/real data, due to changes in constructNRSAValidationResults().
# 09/16/10 cws Removing hardcoding of NRSA database name, using NRSAdbName
#          instead.
# 12/14/10 ssr modified arguements for constructNRSAVaslidationResults and 
#          modified valLegalCheckTest to expect output from modified
#          constructNRSAValidationResults
#  1/18/12 cws Updated to work with validationLegal() now returning character
#          descriptions of errors (which it should have been all this time)
#          instead of dataframes when an error ocurs.  Also returning a zero
#          row dataframe when there is no legal metadata.  Updating unit test
#          accordingly, and to work with standardization of study parameter
#          descriptions metadata.

valLegalCheck <- function(tableName,since=NULL)
# Performs legal value checks on a data table, creating a spreadsheet file of
# issues for review in folder determined by NRSAvalidationLocation if any
# issues are discovered.
#
# Returns NULL on success, or character string describing error or if there
# are no illegal values.
#
# ARGUMENTS:
# tableName Character string with name of table to do legal validation on.
#
# ASSUMPTIONS:
# 
{
  # Retrieve data to be validated, form metadata and site information
  chan <- odbcConnect(NRSAdbName)
  on.exit(valLegalCheck.cleanup(chan))

   if(!is.null(since)) {
      since <- paste("INSERTION > CAST('", since, "' AS DATETIME)", sep='')
  }


 df <- fetchNRSATable(chan, tableName,where=since)
  if(!is.data.frame(df)) return(df)
  
  meta.df <- fetchNRSATable(chan, 'tblPARAMETERDESCRIPTIONS2')#, filterSideChannels=FALSE)
  if(!is.data.frame(meta.df)) return(meta.df)

  siteInfo <- fetchNRSATable(chan, 'tblVISITS2')#, filterSideChannels=FALSE)
  if(!is.data.frame(siteInfo)) return(siteInfo)

  # Create validation results
  validationResults <- valLegalCheck.1(df, meta.df, siteInfo)
  if(!is.data.frame(validationResults)) return(validationResults)
  
  print(sprintf("Detected %d illegal values", nrow(validationResults)))

  # Write validation results to a spreadsheet for review.
  rc <- writeNRSAValidationResults(validationResults
                                  ,paste(NRSAvalidationLocation
                                        ,'valLegal'
                                        ,tableName
                                        ,'.csv'
                                        ,sep=''
                                        )
                                  )
  return(NULL)
}

valLegalCheck.1 <- function(df, meta.df, siteInfo)
# Do all the work once the tables are in hand.  Separated for unit testing.
# Return dataframe of issues on success, or character string describing the
# error if one occurs.
#
# ARGUMENTS:
# df       dataframe of data to be checked
# meta.df  dataframe of metadata containing legal values and form imformation
# siteInfo dataframe of site visit information - site_id, visit_no, date_col.
#
#
{
  # Keep only metdata of use, and column names upper case so they are the same
  # names as in the data.
  legal.meta <- subset(meta.df
                      ,!(LEGAL_VALUES=='' | is.na(LEGAL_VALUES))
                      ,select=c(PARAMETER, LEGAL_VALUES, SAMPLE_TYPE, FORMABBR)
                      )
  if(nrow(legal.meta) == 0) {
      # No legal values occur in the metadata.  This may or may not be a problem.
      return(subset(within(df, TESTDESCRIPTION <- NA), FALSE))
  }

  # Comb through data for values out of their expected ranges
  rr <- validationLegal(df, 'RESULT', 'PARAMETER'
                       ,legal.meta
                       ,otherTests=NULL
                       )
  if(!is.data.frame(rr))
      return(rr)
  
  if(nrow(rr) == 0) {
      return("No illegal values were detected.")
  }

  # Construct validation results
  vv <- constructNRSAValidationResults(rr, meta.df, siteInfo, imageExist = constructImageLocation)

  return(vv)
}

valLegalCheck.cleanup <- function(chan)
# Clean up if something causes an early termination
{
  odbcClose(chan)
}

valLegalCheckTest <- function()
# tests valLegalCheck.1
{
  baseData <- expand.grid(UID=1:5
                         ,TRANSECT=LETTERS[1:10]
                         ,STATION=1:10
                         ,PARAMETER=c('BAR_PRES','BACKWATER','CHANUNCD'
                                     ,'BARWIDTH' ,'WETWIDTH','DEP_POLE'
                                     ,'DEP_SONR','OTHERSTUFF'
                                     )
                         )
  baseData$UID <- as.character(baseData$UID)
  baseData$TRANSECT <- as.character(baseData$TRANSECT)
  baseData$PARAMETER <- as.character(baseData$PARAMETER)
  baseData$RESULT <- ''
  baseData[baseData$PARAMETER=='BAR_PRES'
          ,]$RESULT <- rep(c('','Y','N')
                          ,length.out=length(baseData[baseData$PARAMETER=='BAR_PRES',]$RESULT)
                          )
  baseData[baseData$PARAMETER=='BACKWATER'
          ,]$RESULT <- rep(c('','Y','N')
                          ,length.out=length(baseData[baseData$PARAMETER=='BACKWATER',]$RESULT)
                          )
  baseData[baseData$PARAMETER=='CHANUNCD'
          ,]$RESULT <- rep(c('','CA','DR','FA','GL','PP','PB','PI','PD','PL'
                            ,'PT','PO','RA','RI'
                            )
                          ,length.out=length(baseData[baseData$PARAMETER=='CHANUNCD',]$RESULT)
                          )
  baseData$SAMPLE_TYPE <- 'PHAB_THAL'
  
  meta.df <- data.frame(PARAMETER=  c('BAR_PRES' , 'BACKWATER' , 'CHANUNCD'                               )
                       ,LEGAL_VALUES=c('|Y|N'     , '|Y|N'      , '|CA|DR|FA|GL|PP|PB|PI|PD|PL|PT|PO|RA|RI')
                       ,FORMABBR  = c('Thal'    , 'Thal'    , 'Thal'                                      )
                       ,SAMPLE_TYPE  = c('PHAB_THAL'    , 'PHAB_THAL'    , 'PHAB_THAL'                    )
                       ,stringsAsFactors=FALSE
                       )

  siteInfo <- data.frame(UID=as.character(1:50)
                        ,SITE_ID=paste('site'
                                      ,as.character(rep(1:25, each=2))
                                      ,sep=''
                                      )
                        ,DATE_COL=paste('2008', 4:5, rep(1:25, each=2), sep='-')
                        ,VISIT_NO=rep(1:2, times=25)
                        ,SAMPLE_TYPE=paste('type', rep(1:3, length.out=50))
                        ,OTHERJUNK='other junk'
                        ,stringsAsFactors=FALSE
                        )

  # Perform legal value check with perfect data
  rr <- valLegalCheck.1(baseData, meta.df, siteInfo)
  checkEquals("No illegal values were detected.", rr
             ,paste("Error: Problem detecting illegal values in perfect data: "
                   ,rr
                   )
             )
  
  # Perform legal value check with imperfect data
  realData <- baseData
  realData[realData$UID=='1' & realData$TRANSECT=='A' & realData$STATION==1 &
           realData$PARAMETER=='BAR_PRES',]$RESULT <- 'Wrong'
  realData[realData$UID=='1' & realData$TRANSECT=='B' & realData$STATION==2 &
           realData$PARAMETER=='BACKWATER',]$RESULT <- 'Wrong'
  realData[realData$UID=='3' & realData$TRANSECT=='C' & realData$STATION==3 &
           realData$PARAMETER=='CHANUNCD',]$RESULT <- 'Wrong'
  realData[realData$UID=='4' & realData$TRANSECT=='D' & realData$STATION==4 &
           realData$PARAMETER=='BARWIDTH',]$RESULT <- '123456'

  rr <- valLegalCheck.1(realData, meta.df, siteInfo)
  expected <- data.frame(UID=c('1','1','3')
                        ,SITE_ID=c('site1', 'site1', 'site2')
                        ,DATE_COL=c("2008-4-1", "2008-4-1", "2008-4-2")
                        ,VISIT_NO=as.integer(c(1,1,1))
                        ,SAMPLE_TYPE=rep('PHAB_THAL', 3)
                        ,TRANSECT=c('A', 'B', 'C')
                        ,STATION=as.integer(c(1,2,3))
                        ,PARAMETER=c("BAR_PRES", "BACKWATER", "CHANUNCD")
                        ,TESTDESCRIPTION=c("Value must be |Y|N"
                                          ,"Value must be |Y|N"
                                          ,"Value must be |CA|DR|FA|GL|PP|PB|PI|PD|PL|PT|PO|RA|RI"
                                          )
                        ,RESULT=c('Wrong', 'Wrong', 'Wrong')
                        ,FORMIMAGE=c('=HYPERLINK(\"file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site1_V1_A.tif\" , \"Thal\")'
                                    ,'=HYPERLINK(\"file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site1_V1_B.tif\" , \"Thal\")'
                                    ,'=HYPERLINK(\"file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site2_V1_C.tif\" , \"Thal\")'
                                    )
                        ,COMMENTS=rep("                                              ", 3)
                        ,stringsAsFactors=FALSE
                        )
  checkEquals(expected, rr
             ,"Error: Did not correctly identify illegal values"
             )
  
}
# end of file
