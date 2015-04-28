# valRangeCheck.r
#
# 10/30/09 cws Created
# 11/03/09 cws checked process with fake metadata.
# 11/12/09 cws Added use of SAMPLE_TYPE in metadata and standardized metadata
#          to upper case names.
# 12/16/09 cws Adding argument for supplemental range test function to allow
#          capability already present in validationRange() (SharedCode)
# 04/19/10 SOMEBODY TAKE CREDIT FOR CHANGING THIS
# 05/03/10 cws Temporarily modified to write only results of supplemental range
#          check on thalweg table: writing output to valRangeSupplementalThalweg.csv,
#          filters out results that don't start with "Value should be about" so
#          only incremnt checks are retained.
# 06/01/10 cws Undoing above temporary mod.
# 09/16/10 cws Removing hardcoding of NRSA database name, using NRSAdbName
#          instead.
# 11/09/10 ssr Added test.  Added call to constructImageLocation.
#  1/18/12 cws Updated unit test to work with recent standardization of study 
#          metadata which now uses underscores: RANGE_HIGH, RANGE_LOW, VAR_TYPE.  
#          Also did some prettyprinting.
#

valRangeCheck <- function(tableName, supplementalTest=NULL, since=NULL)
# Performs range value checks on a data table, creating a spreadsheet file of
# issues for review in folder determined by NRSAvalidationLocation
#
# Returns NULL on success, or character string describing error.
#
# ARGUMENTS:
# tableName Character string with name of table to do range validation on.
# supplementalTest Function that provides supplemental range checks on the data
# since     Character string specifying the insertion time prior to which data
#             will not be included.  This string must be in an MS SQL datetime
#             format, e.g. '2010-04-01'. If NULL, all data will be included.
#
# ASSUMPTIONS:
# 
{
  # Retrieve data to be validated, form metadata and site information
  chan <- odbcConnect(NRSAdbName)
  on.exit(valRangeCheck.cleanup(chan))

  if(!is.null(since)) {
      since <- paste("INSERTION > CAST('", since, "' AS DATETIME)", sep='') 
  }
   
  df <- fetchNRSATable(chan, tableName,where=since)
  if(!is.data.frame(df)) return(df)
  
  meta.df <- fetchNRSATable(chan, 'tblPARAMETERDESCRIPTIONS2')#, filterSideChannels=FALSE)

  if(!is.data.frame(meta.df)) return(meta.df)

  siteInfo <- fetchNRSATable(chan, 'tblVISITS2')#, filterSideChannels=FALSE)
  if(!is.data.frame(siteInfo)) return(siteInfo)

  # Create range validation results
  validationResults <- valRangeCheck.1(df, meta.df, siteInfo, supplementalTest)
  if(!is.data.frame(validationResults)) return(validationResults)

  print(sprintf("Detected %d out-of-range values", nrow(validationResults)))

  # Write validation results to a spreadsheet for review.
  rc <- writeNRSAValidationResults(validationResults
                                  ,paste(NRSAvalidationLocation
                                        ,'valRangeSupplemental'
                                        ,tableName
                                        ,'.csv'
                                        ,sep=''
                                        )
                                  )
}

valRangeCheck.1 <- function(df, meta.df, siteInfo, supplementalTest)
# Does all the work
{
  # Keep only metdata of use, and column names upper case so they are the same
  # names as in the data.
  range.meta <- subset(meta.df
                      ,(!(RANGE_LOW=='' | is.na(RANGE_LOW))  |
                        !(RANGE_HIGH=='' | is.na(RANGE_HIGH))
                       ) &
                       PARAMETER %in% unique(df$PARAMETER)
                      ,select=c(FORMABBR, PARAMETER, UNITS, SAMPLE_TYPE
                               ,VAR_TYPE, RANGE_LOW, RANGE_HIGH
                               )
                      )
  if(nrow(range.meta) == 0) {
      return("No range values occur in the metadata for this table.")
  }

#  names(range.meta) <- toupper(names(range.meta))


  # Comb through data for values out of their expected ranges
  if('UNITS' %in% names(df)) {
      cols <- c('PARAMETER','UNITS')
  } else {
      cols <- 'PARAMETER'
  }

  rr <- validationRange(df, 'RESULT', cols
                       ,range.meta
                       ,otherTests=supplementalTest
                       )
  if(!is.data.frame(rr)) return(rr)
  if(nrow(rr)==0) {
      return("No out of range values found.")
  }
  # Create formimage location if images exist
#  imageLocation <- constructImageLocation(rr, meta.df, siteInfo)

  # Construct validation results
  vv <- constructNRSAValidationResults(rr, meta.df, siteInfo, imageExist = constructImageLocation)

  return(vv)
}

valRangeCheck.cleanup <- function(chan)
# Clean up if something causes an early termination
{
  odbcClose(chan)
}

valRangeCheckTest <- function()
  # Create well formed data.  This table contains data from both wadeable
  # and boatable thalweg forms.  UIDs that are even will have 10 stations per
  # transect, UIDs that are odd will have 15 or 12, depending on whether they
  # are wadeable or boatable, respectively.  Even boatable UIDs will have depth
  # units in meters, odd UIDs in ft.
{
  ## Wadeable thalweg
  baseWT <- expand.grid(UID = 1:10
                       ,TRANSECT = LETTERS[1:10]
                       ,STATION = 0:14
                       ,SAMPLE_TYPE = 'PHAB_THALW'
                       ,PARAMETER = c('BACKWATER', 'BAR_PRES', 'CHANUNCD'
                                     ,'DEPTH', 'INCREMNT', 'POOLFMCD'
                                     ,'SEDIMENT', 'SIDCHN', 'WETWIDTH'
                                     , 'BARWIDTH', 'REACHLENGTH'
                                     )
                       )
  baseWT[baseWT$PARAMETER == 'BACKWATER', 'RESULT'] <- rep(c('Y','N')
                                                          ,length.out=10*10*15
                                                          )
  baseWT[baseWT$PARAMETER == 'BAR_PRES', 'RESULT'] <- rep(c('Y','N')
                                                         ,length.out=10*10*15
                                                         )
  baseWT[baseWT$PARAMETER == 'CHANUNCD' &
         baseWT$UID %in% as.character(1:10)
        ,'RESULT'] <- rep(c('PP','PT','PL','PB','PD','GL','RI','RA','CA','FA','DR')
                         ,length.out=10*10*15
                         )
  baseWT[baseWT$PARAMETER == 'DEPTH', 'RESULT'] <- rep(12,length.out=10*10*15)
  baseWT[baseWT$PARAMETER == 'INCREMNT', 'RESULT'] <- rep(2,length.out=10*10*15)
  baseWT[baseWT$PARAMETER == 'POOLFMCD', 'RESULT'] <- rep(c('N','W','W'
                                                           ,'R','B','F'
                                                           )
                                                         ,length.out=10*10*15
                                                         )
  baseWT[baseWT$PARAMETER == 'SEDIMENT', 'RESULT'] <- rep(c('Y','N')
                                                         ,length.out=10*10*15
                                                         )
  baseWT[baseWT$PARAMETER == 'SIDCHN', 'RESULT'] <- rep(c('Y','N')
                                                         ,length.out=10*10*15
                                                         )
  baseWT[baseWT$PARAMETER == 'WETWIDTH', 'RESULT'] <- rep(5.5
                                                         ,length.out=10*10*15
                                                         )
  baseWT[baseWT$PARAMETER == 'BARWIDTH', 'RESULT'] <- rep(5.5
                                                         ,length.out=10*10*15
                                                         )
  baseWT[baseWT$PARAMETER == 'REACHLENGTH', 'RESULT'] <- rep(150
                                                         ,length.out=10*10*15
                                                         )
  baseWT <- subset(baseWT
                  ,!(as.integer(UID) %% 2 == 0 & as.integer(STATION) > 9)
                  )

  ## Boatable thalweg
  baseBT <- expand.grid(UID=11:20
                       ,TRANSECT = LETTERS[1:10]
                       ,STATION = 0:11
                       ,SAMPLE_TYPE = 'PHAB_THAL'
                       ,PARAMETER = c('CHANUNCD', 'DEP_POLE', 'DEP_SONR'
                                     ,'OFF_CHAN', 'SIZE_CLS', 'SNAG'
                                     )
                       )
  baseBT[baseBT$PARAMETER == 'CHANUNCD', 'RESULT'] <- rep(c('PO','GL','RI','RA','CA','FA','DR')
                                                         ,length.out=10*10*12
                                                         )
  baseBT[baseBT$PARAMETER == 'DEP_POLE', 'RESULT'] <- rep(c(17,NA)
                                                         ,length.out=10*10*12/2
                                                         )
  baseBT[baseBT$PARAMETER == 'DEP_SONR', 'RESULT'] <- rep(c(NA,18)
                                                         ,length.out=10*10*12/2
                                                         )
  baseBT[baseBT$PARAMETER == 'OFF_CHAN', 'RESULT'] <- rep(c('Y','N')
                                                         ,length.out=10*10*12
                                                         )
  baseBT[baseBT$PARAMETER == 'SIZE_CLS', 'RESULT'] <- rep(c('BH','BL','CP','GR'
                                                          ,'SA','FN','OT'
                                                          )
                                                          ,length.out=10*10*12
                                                          )
  baseBT[baseWT$PARAMETER == 'SNAG', 'RESULT'] <- rep(c('Y','N')
                                                     ,length.out=10*10*12
                                                     )
  baseBT <- subset(baseBT
                  ,!(as.integer(UID) %% 2 == 0 & as.integer(STATION) > 9)
                  )

  baseTest <- rbind(baseWT, baseBT)#, baseBC)
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$TRANSECT <- as.character(baseTest$TRANSECT)
  baseTest$STATION <- as.character(baseTest$STATION)
  baseTest$SAMPLE_TYPE <- as.character(baseTest$SAMPLE_TYPE)
  baseTest$PARAMETER <- as.character(baseTest$PARAMETER)
  baseTest$FLAG <- as.character(NA)
  baseTest$UNITS <- as.character(NA)
  baseTest[baseTest$PARAMETER %in% c('DEP_POLE','DEP_SONR') &
           as.integer(baseTest$UID) %% 2 == 0,]$UNITS <- 'M'
  baseTest[baseTest$PARAMETER %in% c('DEP_POLE','DEP_SONR') &
           as.integer(baseTest$UID) %% 2 != 0,]$UNITS <- 'FT'
  baseTest[baseTest$PARAMETER == 'DEPTH',]$UNITS <- 'CM'
  baseTest[baseTest$PARAMETER == 'INCREMNT',]$UNITS <- 'M'
  baseTest[baseTest$PARAMETER == 'REACHLENGTH',]$UNITS <- 'M'
  baseTest[baseTest$PARAMETER == 'WETWIDTH',]$UNITS <- 'M'

  ## Creating meta.df
  testMeta <- data.frame(matrix(
         c('Thal','SIZE_CLS','Substrate particle size class','NONE','PHAB_THAL',NA,NA,NA,'|BH|BL|CB|GR|SA|FN|OT'
         ,'Thal','SNAG','Snag (Y/N)','NONE','PHAB_THAL',NA,NA,NA,'|Y|N'
         ,'Thal','DEP_POLE','Depth by pole','M','PHAB_THAL',NA,0.1,7,''
         ,'Thal','DEP_POLE','Depth by pole','FT','PHAB_THAL',NA,0.1,25,''
         ,'Thal','DEP_SONR','Depth by sonar','FT','PHAB_THAL',NA,0.1,100,''
         ,'Thal','DEP_SONR','Depth by sonar','M','PHAB_THAL',NA,0.1,30,''
         ,'Thal','BANK','Bank chosen for reading (Right/Left)','NONE','PHAB_THAL',NA,NA,NA,'NONE|Right|Left'
         ,'Thal','OFF_CHAN','Presence of off channel/backwater (Y/N)','NONE','PHAB_THAL',NA,NA,NA,'|Y|N'
         ,'Thal','CHANUNCD','Channel habitat','NONE','PHAB_THAL',NA,NA,NA,'|PO|GL|RI|RA|CA|FA|DR'
         ,'Thal','BACKWATER','Backwater (Y/N)','NONE','PHAB_THALW',NA,NA,NA,'|Y|N'
         ,'Thal','BAR_PRES','Bar present (Y/N)','NONE','PHAB_THALW',NA,NA,NA,'|Y|N'
         ,'Thal','BARWIDTH','Bar width (m)','M','PHAB_THALW',NA,NA,NA,''
         ,'Thal','CHANUNCD','Channel unit code','NONE','PHAB_THALW',NA,NA,NA,'|PP|PT|PL|PB|PD|P|GL|RI|RA|CA|FA|DR'
         ,'Thal','DEPTH','Thalweg depth (cm)','CM','PHAB_THALW',NA,0,150,''
         ,'Thal','INCREMNT','Uniform increment between transects (m)','M','PHAB_THALW',NA,NA,NA,''
         ,'Thal','REACHLENGTH','Total reach length (m).','M','PHAB_THALW',NA,NA,NA,''
         ,'Thal','POOLFMCD','Pool form code','NONE','PHAB_THALW',NA,NA,NA,'|N|W|R|B|F|BF|BR|BW|FB|FR|FW|FWR|NR|RB|RF|RW|RWB|RWF|WB|WBF|WBR|WF|WR|WRF'
         ,'Thal','SEDIMENT','Soft/small sediment (Y/N)','NONE','PHAB_THALW',NA,NA,NA,'|Y|N'
         ,'Thal','SIDCHN','Side channel (Y/N)','NONE','PHAB_THALW',NA,NA,NA,'|Y|N'
         ,'Thal','WETWIDTH','Wetted width (m)','M','PHAB_THALW',NA,0,30,''
         ,'Thal','DLDLL','Count of dry large dia long length lwd','NONE','PHAB_THALW',NA,0,5,''
         ,'Thal','DLDML','Count of dry large dia med. length lwd','NONE','PHAB_THALW',NA,0,5,''
         ,'Thal','DLDSL','Count of dry large dia short length lwd','NONE','PHAB_THALW',NA,0,10,''
         ,'Thal','DMDLL','Count of dry med. dia long length lwd','NONE','PHAB_THALW',NA,0,10,''
         ,'Thal','DMDML','Count of dry med. dia med. length lwd','NONE','PHAB_THALW',NA,0,15,''
         ,'Thal','DMDSL','Count of dry med. dia short length lwd','NONE','PHAB_THALW',NA,0,15,''
         ,'Thal','DSDLL','Count of dry small dia long length lwd','NONE','PHAB_THALW',NA,0,15,''
         ,'Thal','DSDML','Count of dry small dia med. length lwd','NONE','PHAB_THALW',NA,0,20,''
         ,'Thal','DSDSL','Count of dry small dia short length lwd','NONE','PHAB_THALW',NA,0,30,''
         ,'Thal','DXDLL','Count of dry xlarge dia long length lwd','NONE','PHAB_THALW',NA,0,5,''
         ,'Thal','DXDML','Count of dry xlarge dia med. length lwd','NONE','PHAB_THALW',NA,0,5,''
         ,'Thal','DXDSL','Count of dry xlarge dia short length lwd','NONE','PHAB_THALW',NA,0,5,''
         ,'Thal','OFF_CHAN','Presence of off channel/backwater (Y/N)','NONE','PHAB_THALW',NA,NA,NA,'|Y|N'
         ,'Thal','SUB_5_7','Extra substrate station (5 or 7)','NONE','PHAB_THALW',NA,5,7,''
         ,'Thal','WLDLL','Count of wet large dia long length lwd','NONE','PHAB_THALW',NA,0,5,''
         ,'Thal','WLDML','Count of wet large dia med. length lwd','NONE','PHAB_THALW',NA,0,10,''
         ,'Thal','WLDSL','Count of wet large dia short length lwd','NONE','PHAB_THALW',NA,0,15,''
         ,'Thal','WMDLL','Count of wet med. dia long length lwd','NONE','PHAB_THALW',NA,0,10,''
         ,'Thal','WMDML','Count of wet med. dia med. length lwd','NONE','PHAB_THALW',NA,0,15,''
         ,'Thal','WMDSL','Count of wet med. dia short length lwd','NONE','PHAB_THALW',NA,0,25,''
         ,'Thal','WSDLL','Count of wet small dia long length lwd','NONE','PHAB_THALW',NA,0,10,''
         ,'Thal','WSDML','Count of wet small dia med. length lwd','NONE','PHAB_THALW',NA,0,15,''
         ,'Thal','WSDSL','Count of wet small dia short length lwd','NONE','PHAB_THALW',NA,0,20,''
         ,'Thal','WXDLL','Count of wet xlarge dia long length lwd','NONE','PHAB_THALW',NA,0,5,''
         ,'Thal','WXDML','Count of wet xlarge dia med length lwd','NONE','PHAB_THALW',NA,0,5,''
         ,'Thal','WXDSL','Count of wet xlarge dia shrt length lwd','NONE','PHAB_THALW',NA,0,5,''
         ,'Thal','XSIZE_CLS','Substrate particle size class','NONE','PHAB_THALW',NA,NA,NA,'|RS|RR|RC|XB|SB|CB|GC|GF|SA|FN|HP|WD|OT')
                   ,ncol=9, byrow=TRUE
                 )
                ,stringsAsFactors=FALSE
                )
       names(testMeta) <- c('FORMABBR', 'PARAMETER', 'LABEL', 'UNITS'
        , 'SAMPLE_TYPE', 'VAR_TYPE', 'RANGE_LOW', 'RANGE_HIGH', 'LEGALVALUES')



  # siteInfo
  testInfo <- data.frame(matrix( c(1, 'SITE_01', '01/01/2009', '1', 'VERIF', 'AK'
                                 , 2, 'SITE_02', '01/02/2009', '1', 'VERIF', 'AL'
                                 , 3, 'SITE_03', '01/03/2009', '1', 'VERIF', 'CA'
                                 , 4, 'SITE_04', '01/04/2009', '2', 'VERIF', 'CT'
                                 , 5, 'SITE_05', '01/05/2009', '1', 'VERIF', 'MN'
                                 , 6, 'SITE_06', '01/06/2009', '1', 'VERIF', 'MA'
                                 , 7, 'SITE_07', '01/07/2009', '1', 'VERIF', 'ME'
                                 , 8, 'SITE_08', '01/08/2009', '1', 'VERIF', 'TX'
                                 , 9, 'SITE_09', '01/09/2009', '1', 'VERIF', 'WI'
                                 ,10, 'SITE_10', '01/10/2009', '1', 'VERIF', 'NC'
                                 ,11, 'SITE_11', '01/11/2009', '2', 'VERIF', 'SC'
                                 ,12, 'SITE_12', '01/12/2009', '1', 'VERIF', 'NY'
                                 ,13, 'SITE_13', '01/13/2009', '1', 'VERIF', 'ND'
                                 ,14, 'SITE_14', '01/14/2009', '1', 'VERIF', 'SD'
                                 ,15, 'SITE_15', '01/15/2009', '1', 'VERIF', 'OK'
                                 ,16, 'SITE_16', '01/16/2009', '1', 'VERIF', 'NM'
                                 ,17, 'SITE_17', '01/17/2009', '1', 'VERIF', 'WA'
                                 ,18, 'SITE_18', '01/18/2009', '2', 'VERIF', 'OR'
                                 ,19, 'SITE_19', '01/19/2009', '1', 'VERIF', 'NV'
                                 ,20, 'SITE_20', '01/20/2009', '1', 'VERIF', 'ID'
                                 )
                               ,ncol=6, byrow=TRUE
                               )
                        ,stringsAsFactors=FALSE
                        )
  names(testInfo) <- c('UID', 'SITE_ID', 'DATE_COL', 'VISIT_NO', 'SAMPLE_TYPE', 'STATE')


  # Create poorly formed data based on correct data
  realTest <- baseTest

  #    Make some values missing
  realTest[realTest$UID=='1' & realTest$PARAMETER=='DEPTH'
             & realTest$STATION=='0',]$RESULT <- '300'
  realTest[realTest$UID=='8' & realTest$PARAMETER=='SIDCHN'
             & realTest$STATION=='3',]$RESULT <- 'MAYBE'

  realTest[realTest$UID=='12' & realTest$PARAMETER=='DEP_POLE'
             & realTest$STATION=='0',]$RESULT <- '600'
  realTest[realTest$UID=='16' & realTest$PARAMETER=='SNAG'
             & realTest$STATION=='6',]$RESULT <- 'BOB'
             
  tt <- valRangeCheck.1(baseTest, testMeta, testInfo, supplementalTest=NULL)
  checkEquals(tt, "No out of range values found."
             ,"Error: Detected errors where there are none"
             )
#  intermediateMessage('baseTest check complete.')

  uu <- valRangeCheck.1(realTest, testMeta, testInfo, supplementalTest=NULL)

  
  expected <- data.frame(matrix(
    c('1','SITE_01','01/01/2009','1','PHAB_THALW','A','0','DEPTH','Value expected to be between 0 and 150 inclusive','300','CM',NA,'=HYPERLINK("file:///L:/Apps/Scantron/Images/0001/Flowing Waters/NRSA_Thal_SITE_01_V1_A.tif" , "Thal")','                                              '
     ,'1','SITE_01','01/01/2009','1','PHAB_THALW','B','0','DEPTH','Value expected to be between 0 and 150 inclusive','300','CM',NA,'=HYPERLINK("file:///L:/Apps/Scantron/Images/0001/Flowing Waters/NRSA_Thal_SITE_01_V1_B.tif" , "Thal")','                                              '
     ,'1','SITE_01','01/01/2009','1','PHAB_THALW','C','0','DEPTH','Value expected to be between 0 and 150 inclusive','300','CM',NA,'=HYPERLINK("file:///L:/Apps/Scantron/Images/0001/Flowing Waters/NRSA_Thal_SITE_01_V1_C.tif" , "Thal")','                                              '
     ,'1','SITE_01','01/01/2009','1','PHAB_THALW','D','0','DEPTH','Value expected to be between 0 and 150 inclusive','300','CM',NA,'=HYPERLINK("file:///L:/Apps/Scantron/Images/0001/Flowing Waters/NRSA_Thal_SITE_01_V1_D.tif" , "Thal")','                                              '
     ,'1','SITE_01','01/01/2009','1','PHAB_THALW','E','0','DEPTH','Value expected to be between 0 and 150 inclusive','300','CM',NA,'=HYPERLINK("file:///L:/Apps/Scantron/Images/0001/Flowing Waters/NRSA_Thal_SITE_01_V1_E.tif" , "Thal")','                                              '
     ,'1','SITE_01','01/01/2009','1','PHAB_THALW','F','0','DEPTH','Value expected to be between 0 and 150 inclusive','300','CM',NA,'=HYPERLINK("file:///L:/Apps/Scantron/Images/0001/Flowing Waters/NRSA_Thal_SITE_01_V1_F.tif" , "Thal")','                                              '
     ,'1','SITE_01','01/01/2009','1','PHAB_THALW','G','0','DEPTH','Value expected to be between 0 and 150 inclusive','300','CM',NA,'=HYPERLINK("file:///L:/Apps/Scantron/Images/0001/Flowing Waters/NRSA_Thal_SITE_01_V1_G.tif" , "Thal")','                                              '
     ,'1','SITE_01','01/01/2009','1','PHAB_THALW','H','0','DEPTH','Value expected to be between 0 and 150 inclusive','300','CM',NA,'=HYPERLINK("file:///L:/Apps/Scantron/Images/0001/Flowing Waters/NRSA_Thal_SITE_01_V1_H.tif" , "Thal")','                                              '
     ,'1','SITE_01','01/01/2009','1','PHAB_THALW','I','0','DEPTH','Value expected to be between 0 and 150 inclusive','300','CM',NA,'=HYPERLINK("file:///L:/Apps/Scantron/Images/0001/Flowing Waters/NRSA_Thal_SITE_01_V1_I.tif" , "Thal")','                                              '
     ,'1','SITE_01','01/01/2009','1','PHAB_THALW','J','0','DEPTH','Value expected to be between 0 and 150 inclusive','300','CM',NA,'=HYPERLINK("file:///L:/Apps/Scantron/Images/0001/Flowing Waters/NRSA_Thal_SITE_01_V1_J.tif" , "Thal")','                                              '
     ,'12','SITE_12','01/12/2009','1','PHAB_THAL','A','0','DEP_POLE','Value expected to be between 0.1 and 7 inclusive','600','M',NA,'=HYPERLINK("file:///L:/Apps/Scantron/Images/0001/Flowing Waters/NRSA_Thal_SITE_12_V1_A.tif" , "Thal")','                                              '
     ,'12','SITE_12','01/12/2009','1','PHAB_THAL','B','0','DEP_POLE','Value expected to be between 0.1 and 7 inclusive','600','M',NA,'=HYPERLINK("file:///L:/Apps/Scantron/Images/0001/Flowing Waters/NRSA_Thal_SITE_12_V1_B.tif" , "Thal")','                                              '
     ,'12','SITE_12','01/12/2009','1','PHAB_THAL','C','0','DEP_POLE','Value expected to be between 0.1 and 7 inclusive','600','M',NA,'=HYPERLINK("file:///L:/Apps/Scantron/Images/0001/Flowing Waters/NRSA_Thal_SITE_12_V1_C.tif" , "Thal")','                                              '
     ,'12','SITE_12','01/12/2009','1','PHAB_THAL','D','0','DEP_POLE','Value expected to be between 0.1 and 7 inclusive','600','M',NA,'=HYPERLINK("file:///L:/Apps/Scantron/Images/0001/Flowing Waters/NRSA_Thal_SITE_12_V1_D.tif" , "Thal")','                                              '
     ,'12','SITE_12','01/12/2009','1','PHAB_THAL','E','0','DEP_POLE','Value expected to be between 0.1 and 7 inclusive','600','M',NA,'=HYPERLINK("file:///L:/Apps/Scantron/Images/0001/Flowing Waters/NRSA_Thal_SITE_12_V1_E.tif" , "Thal")','                                              '
     ,'12','SITE_12','01/12/2009','1','PHAB_THAL','F','0','DEP_POLE','Value expected to be between 0.1 and 7 inclusive','600','M',NA,'=HYPERLINK("file:///L:/Apps/Scantron/Images/0001/Flowing Waters/NRSA_Thal_SITE_12_V1_F.tif" , "Thal")','                                              '
     ,'12','SITE_12','01/12/2009','1','PHAB_THAL','G','0','DEP_POLE','Value expected to be between 0.1 and 7 inclusive','600','M',NA,'=HYPERLINK("file:///L:/Apps/Scantron/Images/0001/Flowing Waters/NRSA_Thal_SITE_12_V1_G.tif" , "Thal")','                                              '
     ,'12','SITE_12','01/12/2009','1','PHAB_THAL','H','0','DEP_POLE','Value expected to be between 0.1 and 7 inclusive','600','M',NA,'=HYPERLINK("file:///L:/Apps/Scantron/Images/0001/Flowing Waters/NRSA_Thal_SITE_12_V1_H.tif" , "Thal")','                                              '
     ,'12','SITE_12','01/12/2009','1','PHAB_THAL','I','0','DEP_POLE','Value expected to be between 0.1 and 7 inclusive','600','M',NA,'=HYPERLINK("file:///L:/Apps/Scantron/Images/0001/Flowing Waters/NRSA_Thal_SITE_12_V1_I.tif" , "Thal")','                                              '
     ,'12','SITE_12','01/12/2009','1','PHAB_THAL','J','0','DEP_POLE','Value expected to be between 0.1 and 7 inclusive','600','M',NA,'=HYPERLINK("file:///L:/Apps/Scantron/Images/0001/Flowing Waters/NRSA_Thal_SITE_12_V1_J.tif" , "Thal")','                                              ')
                               ,ncol=14, byrow=TRUE
                               )
                        ,stringsAsFactors=FALSE
                        )
  names(expected) <- c('UID','SITE_ID','DATE_COL','VISIT_NO','SAMPLE_TYPE','TRANSECT'
                      ,'STATION','PARAMETER','TESTDESCRIPTION','RESULT'
                      ,'UNITS','FLAG','FORMIMAGE','COMMENTS'
                      )

  checkEquals(uu, expected
             ,"Error: Did not properly detect errors where they exist"
             )
#  intermediateMessage('realTest check complete.')

}

# end of file