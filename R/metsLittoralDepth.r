#  metsLittoralDepth.r
#  
# 01/25/10 rch copied, plagerized and made up this code.
# 02/18/10 cws removed source() of NRSAValidation.r and summaryby.r
# 06/01/10 cws removed odd code that somehow showed up here.
# 09/16/10 cws Removing hardcoding of NRSA database name, using NRSAdbName
#          instead.
# 11/04/10 cws handles missing littoral file (ie when study has no boatable
#          sites) with some grace.
# 11/18/10 cws added require(Hmisc) for %nin% operator
#
require(Hmisc)
require(RODBC)
require(RUnit) 

metsLittoralDepth <- function()
#Calculates Littoral Depth metrics:
# Boatable Protocal:
#=xlit mxlit mnlit vlit
#
#These metrics are saved to a csv file in the directory specified by
# NRSAMetricsLocation.
# Returns NULL on success or a character string describing the problem if one
# occurs.
# ARGUMENTS:
# none
{
  # read in densiometer readings from database
  litt <- odbcConnect(NRSAdbName)
  on.exit(metsLittoralDepth.cleanup(litt))

  rawdat <- fetchNRSATable(litt, 'tblCHANDEPTH2')
  if(is.character(rawdat)) return(rawdat)
  
  # calculate the metrics
  mets <- metsLittoralDepth.1(rawdat)
  #  if(is.character(mets)) return(mets)
        
  # write the results
  rc <- writeNRSACalcResults(mets, 'metsLittoralDepth.csv')

  return(rc)
}



metsLittoralDepth.1 <- function(indat)
# Does all the real work for metsLittoralDepth.
# Returns a dataframe of calculations if successful
# or a character string describing the problem if
# one was encountered.
#
# ARGUMENTS:
# indat		dataframe of littoral data.
# protocols	dataframe relating UID to the
#			  sampling protocol used at the site.
#
{
  intermediateMessage('Littoral Depth mets', loc='start')

  cdData <- subset(indat,PARAMETER %in% c('SONAR','POLE'))
  #cdData <- merge(cdData, protocols, by='UID', all.x=TRUE, all.y=FALSE)
  if(nrow(cdData)==0) return("There is no littoral depth data")
  intermediateMessage('.1')

  mdx <- summaryby(cdData,'mean',"xlit")
  mds <- summaryby(cdData,'sd',"vlit")
  mdm <- summaryby(cdData,'max',"mxlit")
  mdn <- summaryby(cdData,'min',"mnlit")

  intermediateMessage('.2')
  mets <- rbind(mdx,mds,mdm,mdn)

  intermediateMessage('. Done.', loc='end')

  return(mets)
 
}

metsLittoralDepthTest <- function()
# Unit test for metsLittoralDepth.1
# IGNORE THE RESULTS for Boatable sites.  The test data is from WEMAP data and
# has only wadable sites.  The  metsLittoralDepth.1 function needs data for
# both SAMPLE_TYPES, so the data was duplicated and RESULTS for Boatable obs.
# were set to zero.
{
  # Create correctly formated test data, and run data through metsLittoralDepth.1
  testData<-metsLittoralDepth.inputdata()

  testDataResult<- metsLittoralDepth.1(testData)

  metsExpected <-metsLittoralDepth.inputmetrics()

  # compare results from baseData (testDataResult) with expectedResults  (metsExpected)

  # Calculated values should be within 10E-7 of expected values, should
  # only be missing where they are supposed to be missing and nonmissing where
  # they are supposed to be nonmissing.
  # Note: the errs dataframe can be printed to show where the errors occur when
  # debugging.
  tt <- merge(testDataResult, metsExpected, by=c('UID','METRIC'),all=TRUE)
  tt$diff <- tt$RESULT - tt$EXPECTED

  errs <- subset(tt, abs(diff) > 10^-7 | is.na(RESULT) != is.na(EXPECTED))
  checkEquals(0, nrow(errs)
             ,"Error: Littoral Depth metrics are broken"
             )

  test2 <- metsLittoralDepth.1(subset(testData, PARAMETER %nin% c('POLE','SONAR')))
  checkEquals("There is no littoral depth data", test2
             ,"Error: Littoral Depth metrics do not handle stream-only studies"
             )
}


 metsLittoralDepth.cleanup <- function(indb)
# Clean up when metsLittoralDepth() terminates
{
  odbcClose(indb)
}


metsLittoralDepth.inputdata <-function()
{
  newdat <- rbind(expand.grid(LINE=1:5
                              ,TRANSECT = LETTERS[1:11]
                              ,UID = c('OSM04461-0004','WCAP99-0585','WWYP99-0555')
                              ,SAMPLE_TYPE = 'PHAB_CHANBFRONT'
                              ,PARAMETER='SONAR'
                              )
                   )

                 newdat$RESULT <- c(0.03048,0,0,0.03048,0.06096,0.1524,0.1524,0.12192,0.06096,
                        0.06096,0.12192,0.18288,0.06096,0.06096,0.03048,0.1524,0.12192,0.03048,
                        0.03048,0.06096,0.09144,0.06096,0.18288,0.1524,0.18288,0.27432,0.42672,
                        0.4572,0.51816,0.48768,0.06096,0.06096,0.12192,0.1524,0.18288,0.03048,
                        0.09144,0.06096,0.09144,0.1524,NA,NA,NA,NA,NA,0.06096,0.03048,0.06096,
                        0.06096,0.24384,0.03048,0.09144,0.09144,0.09144,0.09144,0.4,0.5,0.3,0.2,0.2,
                        0.2,0.2,0.3,0.4,0.4,0.2,0.3,0.3,0.2,0.3,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                        NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                        NA,NA,NA,0.3,0.4,0.7,0.8,1.1,0.3,0.5,0.6,0.9,1.2,0.2,0.3,0.4,0.4,0.5,0.6,0.7,0.8,0.5,
                        0.3,0.3,0.4,0.5,0.4,0.4,0.2,0.3,0.6,0.5,0.5,0.5,0.4,0.3,0.2,0.2,1,0.8,0.3,0.6,0.4,0.3,
                        0.5,0.4,0.5,0.3,0.1,0.1,0.3,0.3,0.2,0.8,0.5,0.4,0.4,0.2)
 
 
            newdat$UID <- as.character(newdat$UID)
            newdat$TRANSECT <- as.character(newdat$TRANSECT)
            newdat$SAMPLE_TYPE <- as.character(newdat$SAMPLE_TYPE)
            newdat$PARAMETER <- as.character(newdat$PARAMETER)

                return(newdat)
 
               }


 metsLittoralDepth.inputmetrics <-function()
             {
                              newdat<- rbind(data.frame(UID = c('OSM04461-0004','WCAP99-0585','WWYP99-0555'),
                                                                    METRIC='xlit',
                                                                    RESULT=c(0.1243584,0.2933333333,0.4654545455)
 ) 
                                                  , data.frame(UID = c('OSM04461-0004','WCAP99-0585','WWYP99-0555'),
                                                                    METRIC='vlit',
                                                                    RESULT=c(0.120014099,0.0961150105,0.2405381173)
 ) 
                                                  , data.frame(UID = c('OSM04461-0004','WCAP99-0585','WWYP99-0555'),
                                                                    METRIC='mxlit',
                                                                    RESULT=c(0.51816,0.5,1.2)
 ) 
                                                  , data.frame(UID = c('OSM04461-0004','WCAP99-0585','WWYP99-0555'),
                                                                    METRIC='mnlit',
                                                                    RESULT=c(0,0.2,0.1)
 ) 
                                                  
)
                               newdat<- rename(newdat, 'RESULT','EXPECTED')

                               return(newdat)

                 }
