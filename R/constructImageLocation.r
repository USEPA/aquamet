# constructImageLocation.r
#
# 11/09/10 ssr Created.
# 12/14/10 ssr added test
#

  # Create formimage location if images exist

constructImageLocation <- function (df, ssFmt='EXCEL')

# Returns constructed dataframe with the following columns: SITE_ID, VISIT_NO,
# DATE_COL, TRANSECT, PARAMETER, TESTDESCRIPTION, RESULT, UNITS (if present
# in df), FORMIMAGE, COMMENTS.
#
# POSSIBLE FUTURE EXTENSIONS:
# + Handling fish related form images, which have _P1, _P2, etc. at end of
#   the file name.  There is also the Lower Missisippi form which may have these
#   extensions.
#
# ARGUMENTS:
# df            data frame of validation results.  Is expected to have the
#                 columns expected for NRSA validation results, and will
#                 specifically use UID, TRANSECT, PARAMETER, TESTDESCRIPTION,
#                 RESULT, UNITS (if present) and TESTDESCRIPTION.
#
# ssFmt         type of spreadsheet that will be used:  Excel (EXCEL) or 
#                 Open Office (OO)

  {
  probs <- NULL
  tableKeys <- names(df)[names(df) %in% NRSAKeyColumns & names(df) != 'UID']
  NRSASiteKeys <-  names(df)[names(df) %in% c('SITE_ID','DATE_COL','VISIT_NO')]
  
  # Make sure that siteInfo contains the appropriate columns
  if('FORMABBR' %nin% names(df)) {
      probs<-"Error: FORMABBR not a column in meta.df"
      return(as.matrix(probs))
  }

  if(all(tableKeys %nin% names(df))) {
      probs<-"Error: not all tableKeys not are columns in siteInfo"
      return(as.matrix(probs))
  }

  if(all(NRSASiteKeys %nin% c('SITE_ID','DATE_COL','VISIT_NO'))) {
      probs<-"Error: Data file must contain SITE_ID, DATE_COL, and VISIT_NO"
      return(as.matrix(probs))
  }
  
    
#  aa <- subset(df, select=c('UID',NRSASiteKeys,'PARAMETER','UNITS'))
#  bb <- subset(meta.df, select=c('FORMABBR','SAMPLE_TYPE','PARAMETER','UNITS'))
#  cc <- subset(siteInfo, select=c('UID',tableKeys))
#
#  dd <- merge(aa, bb)
#  df <- merge(dd, cc)
  
  parametersChRip_p2 <- c("CANBTRE", "CANSTRE", "CANVEG"
                         ,"UNDERVEG", "UNDNWDY", "UNDWDY"
                         ,"GCNWDY", "GCWDY", "BARE"
                         ,"WALL", "BUILD", "PAVE", "ROAD", "PIPES", "LANDFL"
                         ,"PARK", "ROW", "PAST", "LOG", "MINE"
                         ,"ALGAE", "MACPHY", "WOODY", "BRISH", "LVTREE"
                         ,"OVRHNG", "UNDCUT", "BOULDR", "STRUCT"
                         ,"SHOR2RIP", "CONSTRT", "SEEOVRBK"
                         ,"DENSIOM"
                         )


  df$ffName <- ifelse(df$FORMABBR %in% c('ChRip', 'Thal')
                     # The value of TRANSECT is a part of these form file names
                     ,ifelse(df$FORMABBR=='ChRip' &
                             format(as.POSIXct(df$DATE_COL), '%Y')> 2008 &
                             df$SAMPLE_TYPE %in% c('PHAB_CHANB','PHAB_CHANBFRONT')
                            # Both sides of the boatable form of ChRip were
                            # exported as a single TIFF in 2008, but as separate
                            # files in 2009 because of scanner troubles.  Use
                            # PARAMETER to determine which file to hyperlink to.
                           ,paste('NRSA'
                                 ,df$FORMABBR
                                 ,df$SITE_ID
                                 ,paste('V', df$VISIT_NO, sep='')
                                 ,df$TRANSECT
                                 ,ifelse(df$PARAMETER %in% parametersChRip_p2
                                        ,'P2'
                                        ,'P1'
                                        )
                                 ,sep='_'
                                 )
                         # Otherwise just include transect without any page num.
                           ,paste('NRSA'
                                 ,df$FORMABBR
                                 ,df$SITE_ID
                                 ,paste('V', df$VISIT_NO, sep='')
                                 ,df$TRANSECT
                                 ,sep='_'
                                 )
                           )
                     # Other forms do not contain transect in their file names
                     ,paste('NRSA'
                           ,df$FORMABBR
                           ,df$SITE_ID
                           ,paste('V', df$VISIT_NO, sep='')
                           ,sep='_'
                           )
                     )

  df$ffName <- paste('L:/Apps/Scantron/Images/'
                    ,format(as.POSIXct(df$DATE_COL), '%Y')
                    ,'/Flowing Waters/'
                    ,df$ffName
                    ,'.tif'
                    , sep=''
                    )


  # Write field form file name as URL.  This is spreadsheet dependent.
  #   Microsoft Excel: =HYPERLINK("file:/path/fname.tif", "optional label")
  #                    Note there can be 1-3 slashes after the 'file:', but
  #                    double-quotes are required.
  #   OpenOffice Calc: =HYPERLINK('file:///path/fname.tif'; 'optional label')
  #                    Note that 1 or 3 slashes are required after the file:
  #                    and the semicolon is required to delimit the arguments,
  #                    as is the use of double-quotes.
  #   Gnu Gnumeric:    =HYPERLINK() does not work, may be fixed soon. feh.
  if(ssFmt=='OO') {
      df$FORMIMAGE <- paste('=HYPERLINK("file:///'
                           ,df$ffName, '" ; "', df$FORMABBR
                           ,'")'
                           ,sep=''
                           )
  } else if(ssFmt=='EXCEL') {
      df$FORMIMAGE <- paste('=HYPERLINK("file:///'
                           ,df$ffName, '" , "', df$FORMABBR
                           ,'")'
                           ,sep=''
                           )
  }
  intermediateMessage('.1')

  df <- subset(df, select=c('UID',NRSASiteKeys,tableKeys,'PARAMETER','FORMIMAGE'))
                         

  return(df)
  }

##############################
  
# constructImageLocationTest.r
#  Need to test for ssFmt='EXCEL' and ssFmt='OO'
#  Need to test for missing NRSASiteKeys or required columns


constructImageLocationTest.1 <- function()
# Tests constructNRSAValidationResults() with one intrasite key, one data year
# and no UNITS column, and prepared for Open Office Calc instead of Excel.
{
  baseData <- data.frame(UID=c('1','1','1','2','2','3')
                        ,TRANLINE=c('A','C','E','B','B','XJ')
                        ,SAMPLE_TYPE='test'
                        ,UNITS='furlongs'
                        ,FLAG=c(as.character(NA), '', 'K', '', '', '')
                        ,PARAMETER=c('p1','p2','p3','p2','p4','p1')
                        ,TESTDESCRIPTION=c('bad', 'worse', 'whoa'
                                          ,rep('odd relationship', 2)
                                          ,'just odd'
                                          )
                        ,RESULT=c('one','two','three','four','five','six')
                        ,stringsAsFactors=FALSE
                        )
  formInfo <- data.frame(PARAMETER=paste('p', as.character(1:20), sep='')
                        ,FORMABBR=paste('form'
                                       ,as.character(rep(1:10, each=2))
                                       ,sep=''
                                       )
                        ,SAMPLE_TYPE='test'
                        ,UNITS='furlongs'
                        ,MOREMETADATA='another metadata column'
                        ,stringsAsFactors=FALSE
                        )
  sites <- data.frame(UID=as.character(1:50)
                     ,SITE_ID=paste('site'
                                   ,as.character(rep(1:25, each=2))
                                   ,sep=''
                                   )
                     ,DATE_COL=paste('2008', 4:5, rep(1:25, each=2), sep='-')
                     ,VISIT_NO=rep(1:2, times=25)
                     ,OTHERJUNK='other junk'
                     ,stringsAsFactors=FALSE
                     )

  byVars <- c('SAMPLE_TYPE', 'PARAMETER')
  metadataCols <- c('PARAMETER','FORMABBR','SAMPLE_TYPE')
#  formMetadata<-formMetadata[!duplicated(formMetadata[c('PARAMETER','SAMPLE_TYPE')]),]
  df <- merge(baseData, formInfo#[metadataCols]
             ,by=byVars, all.x=TRUE, all.y=FALSE
             )

  df <- merge(df, sites#[c('UID','SITE_ID','DATE_COL','VISIT_NO')]
             ,by='UID', all.x=TRUE, all.y=FALSE)


  tt <- merge(baseData, formInfo, by=c('PARAMETER','UNITS','SAMPLE_TYPE')
             ,all.x=TRUE, all.y=FALSE
             )
  expected <- merge(tt, sites, by='UID', all.x=TRUE, all.y=FALSE)
  expected$FORMIMAGE <- paste('=HYPERLINK("file:///'
                             ,'L:/Apps/Scantron/Images/'
                             ,'2008'
                             ,'/Flowing Waters/'
                             ,paste('NRSA'
                                   ,expected$FORMABBR
                                   ,expected$SITE_ID
                                   ,paste('V', expected$VISIT_NO, sep='')
                                   ,sep='_'
                                   )
                             ,'.tif'
                             , '" , "', expected$FORMABBR
                             ,'")'
                             ,sep=''
                             )
  expected$COMMENTS <- "                                              "

  expected <- expected[c('UID','SITE_ID','DATE_COL','VISIT_NO','SAMPLE_TYPE'
                        ,'TRANLINE','PARAMETER','FORMIMAGE'
                        )
                      ]
  rownames(expected)<-NULL
  rr <- constructImageLocation(df, ssFmt='EXCEL')
  checkIdentical(expected, rr
             ,"Error: Incorrect construction of image location for Excel"
             )

# Creating file with ssFmt = 'OO' 
  expected2 <- merge(tt, sites, by='UID', all.x=TRUE, all.y=FALSE)
  expected2$FORMIMAGE <- paste('=HYPERLINK("file:///'
                             ,'L:/Apps/Scantron/Images/'
                             ,'2008'
                             ,'/Flowing Waters/'
                             ,paste('NRSA'
                                   ,expected2$FORMABBR
                                   ,expected2$SITE_ID
                                   ,paste('V', expected$VISIT_NO, sep='')
                                   ,sep='_'
                                   )
                             ,'.tif'
                             , '" ; "', expected2$FORMABBR
                             ,'")'
                             ,sep=''
                             )
  expected2$COMMENTS <- "                                              "

  expected2 <- expected2[c('UID','SITE_ID','DATE_COL','VISIT_NO','SAMPLE_TYPE'
                        ,'TRANLINE','PARAMETER','FORMIMAGE'
                        )
                      ]
  rownames(expected2)<-NULL
  rr2 <- constructImageLocation(df, ssFmt='OO')
  checkIdentical(expected2, rr2
             ,"Error: Incorrect construction of image location for Open Office"
                 )

}
