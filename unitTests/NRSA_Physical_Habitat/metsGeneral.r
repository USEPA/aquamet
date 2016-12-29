# metsGeneral.r
# RUnit tests


metsGeneralTest <- function()
# Unit test for metsGeneral.1
# IGNORE THE RESULTS for Boatable sites.  The test data is from WEMAP data and
#has only wadable sites.  The  metsGeneral.1 function needs data for
#both SAMPLE_TYPES, so the data was duplicated and RESULTS for Boatable obs.
#were set to zero.
{
  testData <- metsGeneral.testData()

  metsExpected <- metsGeneral.expectedMets()

  metsGeneralTest.process(testData, metsExpected)

  wd <- subset(testData, UID %in% c('WCAP99-0587','WCAP99-0592','WCAP99-0905'))
  wm <- subset(metsExpected, UID %in% c('WCAP99-0587','WCAP99-0592','WCAP99-0905'))
  metsGeneralTest.process(wd, wm)

  bd <- subset(testData, !(UID %in% c('WCAP99-0587','WCAP99-0592','WCAP99-0905')))
  bm <- subset(metsExpected, !(UID %in% c('WCAP99-0587','WCAP99-0592','WCAP99-0905')))
  metsGeneralTest.process(bd, bm)

}



metsGeneralTest.process <- function(testData, metsExpected)
# performs the bulk of the unit test on the given data and expected results
# testData<- bd; metsExpected<-bm
{
  testDataResult<- metsGeneral.1(testData)

  #compare results from baseData (testDataResult) with expectedResults  (metsExpected)

  metsExpected <- rename(metsExpected, 'RESULT','EXPECTED')
  testDataResult$RESULT<-as.numeric(testDataResult$RESULT)

  # Calculated values should be within 10E-7 of expected values, should
  # only be missing where they are supposed to be missing and nonmissing where
  # they are supposed to be nonmissing.
  # Note: the errs dataframe can be printed to show where the errors occur when
  # debugging.
  tt <- merge(testDataResult, metsExpected, by=c('UID','METRIC'), all=TRUE)
  tt$diff <- tt$RESULT - tt$EXPECTED

  errs <- subset(tt, abs(diff) > 10^-7 | is.na(RESULT) != is.na(EXPECTED))
  checkEquals(0, nrow(errs)
             ,"Error: General  metrics are broken"
             )

}



metsGeneral.cleanup <- function(indb)
# Clean up when metsGeneral() terminates
{
  odbcClose(indb)
}



metsGeneral.testData <- function()
# Creates test data for metsGeneral unit test
# Wadeable UIDs: WCAP99-0587, WCAP99-0592, WCAP99-0905
# Boatable UIDs: WCAP99-0585, WCAP99-0591
{
  # Create correctly formated test data, and run data through metsGeneral.1
  testData <- rbind(data.frame(UID ='WCAP99-0585',
                               PARAMETER='ACTRANSP',
                               SAMPLE_TYPE='PHAB_THAL',
                               TRANSECT=c(LETTERS[1:10]),
                               STATION='0',
                               RESULT=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),stringsAsFactors=FALSE
                              ),
                    data.frame(UID ='WCAP99-0585 w/DISTANCE',
                               PARAMETER='DISTANCE',
                               SAMPLE_TYPE='PHAB_THAL',
                               TRANSECT=c(LETTERS[1:10]),
                               STATION='0',
                               RESULT=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),stringsAsFactors=FALSE
                              ),
                    data.frame(UID ='WCAP99-0587',
                               PARAMETER='INCREMNT',
                               SAMPLE_TYPE='PHAB_THALW',
                               TRANSECT=c("A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B",
     "B","B","B","C","C","C","C","C","C","C","C","C","C","D","D","D","D","D","D","D","D","D","D","E",
     "E","E","E","E","E","E","E","E","E","E","E","F","F","F","F","F","F","F","F","F","F","G","G","G","G",
     "G","G","G","G","G","G","H","H","H","H","H","H","H","H","H","H","H","I","I","I","I","I","I","I","I","I",
     "I","J","J","J","J","J","J","J","J","J","J"),
                               STATION=c("0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","10","11","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5",
     "6","7","8","9","0","1","2","3","4","5","6","7","8","9","10","0","1","2","3","4","5","6","7","8","9",
     "0","1","2","3","4","5","6","7","8","9"),
                               RESULT=c("1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,
     NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,
     NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,
     NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,
     NA,NA,NA,NA,NA,NA,NA),stringsAsFactors=FALSE
                               ),
                    data.frame(UID ='WCAP99-0591',
                               PARAMETER='ACTRANSP',
                               SAMPLE_TYPE='PHAB_THAL',
                               TRANSECT=c(LETTERS[1:10]),
                               STATION=rep('0',10),
                               RESULT="600",
                               stringsAsFactors=FALSE
                               ),
                    data.frame(UID ='WCAP99-0591 w/DISTANCE',
                               PARAMETER='DISTANCE',
                               SAMPLE_TYPE='PHAB_THAL',
                               TRANSECT=c(LETTERS[1:10]),
                               STATION=rep('0',10),
                               RESULT=rep("800",10),
                               stringsAsFactors=FALSE
                               ),
                    data.frame(UID ='WCAP99-0591 w/ACTRANSP+DISTANCE',
                               PARAMETER=rep(c('ACTRANSP','DISTANCE'), each=10),
                               SAMPLE_TYPE='PHAB_THAL',
                               TRANSECT=c(LETTERS[1:10],LETTERS[1:10]),
                               STATION=rep("0",10),
                               RESULT=rep(c('600','800'), each=10),
                               stringsAsFactors=FALSE
                               ),
                    data.frame(UID ='WCAP99-0591 w/ACTRANSP+DISTANCE and some NA',
                               PARAMETER=rep(c('ACTRANSP','DISTANCE'), each=10),
                               SAMPLE_TYPE='PHAB_THAL',
                               TRANSECT=c(LETTERS[1:10],LETTERS[1:10]),
                               STATION=rep("0",10),
                               RESULT=c(NA,"600","600","600","600","600","600","600","600",NA
                                       ,"800",NA,"800","800","800","800","800","800","800",NA
                                       ),
                               stringsAsFactors=FALSE
                               ),
                    data.frame(UID ='WCAP99-0592',
                               PARAMETER='INCREMNT',
                               SAMPLE_TYPE='PHAB_THALW',
                               TRANSECT=c("A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B",
     "B","B","B","C","C","C","C","C","C","C","C","C","C","D","D","D","D","D","D","D","D","D","D","E",
     "E","E","E","E","E","E","E","E","E","F","F","F","F","F","F","F","F","F","F","G","G","G","G","G","G",
     "G","G","G","G","H","H","H","H","H","H","H","H","H","H","I","I","I","I","I","I","I","I","I","I","J","J",
     "J","J","J","J","J","J","J","J","XF","XF","XF","XG","XG","XG","XG","XG","XG","XG","XG","XG",
     "XG"),
                               STATION=c("0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","7","8","9","0","1","2","3","4","5","6","7","8","9"),
                               RESULT=c("1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,
     "1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,
     NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,
     NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",
     NA,NA,NA,NA,NA,NA,NA,NA,NA),stringsAsFactors=FALSE
                               ),
                    data.frame(UID ='WCAP99-0905',
                               PARAMETER='INCREMNT',
                               SAMPLE_TYPE='PHAB_THALW',
                               TRANSECT=c("A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B",
     "B","B","B","C","C","C","C","C","C","C","C","C","C","D","D","D","D","D","D","D","D","D","D","E",
     "E","E","E","E","E","E","E","E","E","F","F","F","F","F","F","F","F","F","F","G","G","G","G","G","G",
     "G","G","G","G","H","H","H","H","H","H","H","H","H","H","I","I","I","I","I","I","I","I","I","I","J","J",
     "J","J","J","J","J","J","J","J","XI","XJ"),
                               STATION=c("0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","0","0"),
                               RESULT=c("1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6",
     "1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6",
     "1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6",
     "1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6",
     "1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6",
     "1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6",
     "1.6","1.6","1.6","1.6","1.6"),stringsAsFactors=FALSE
                               ),
                    data.frame(UID ='WCAP99-0585',
                               PARAMETER='OFF_CHAN',
                               SAMPLE_TYPE='PHAB_THAL',
                               TRANSECT=LETTERS[1:10],
                               STATION='0',
                               RESULT=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
                               ),
                    data.frame(UID ='WCAP99-0587',
                               PARAMETER='SIDCHN',
                               SAMPLE_TYPE='PHAB_THALW',
                               TRANSECT=c("A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B",
     "B","B","B","C","C","C","C","C","C","C","C","C","C","D","D","D","D","D","D","D","D","D","D","E",
     "E","E","E","E","E","E","E","E","E","E","E","F","F","F","F","F","F","F","F","F","F","G","G","G","G",
     "G","G","G","G","G","G","H","H","H","H","H","H","H","H","H","H","H","I","I","I","I","I","I","I","I",
     "I","I","J","J","J","J","J","J","J","J","J","J"),
                               STATION=c("0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","10","11","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5",
     "6","7","8","9","0","1","2","3","4","5","6","7","8","9","10","0","1","2","3","4","5","6","7","8","9",
     "0","1","2","3","4","5","6","7","8","9"),
                               RESULT=c("N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N"),stringsAsFactors=FALSE
                               ),
                    data.frame(UID ='WCAP99-0591',
                               PARAMETER='OFF_CHAN',
                               SAMPLE_TYPE='PHAB_THAL',
                               TRANSECT=c("A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A",
                   "A","A","A","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B",
        "B","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","D","D","D",
     "D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","E","E","E","E","E","E","E",
     "E","E","E","E","E","E","E","E","E","E","E","E","E","F","F","F","F","F","F","F","F","F","F","F","F",
     "F","F","F","F","F","F","F","F","G","G","G","G","G","G","G","G","G","G","G","G","G","G","G","G",
     "G","G","G","G","H","H","H","H","H","H","H","H","H","H","H","H","H","H","H","H","H","H","H",
     "H","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","J","J","J","J","J","J","J","J",
     "J","J","J","J","J","J","J","J","J","J","J","J"),
                               STATION=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15",
     "16","17","18","19","20","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16",
     "17","18","19","20","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17",
     "18","19","20","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18",
     "19","20","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19",
     "20","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20",
     "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","1",
     "2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","1","2",
     "3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","1","2","3",
     "4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"),
                               RESULT=c("N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","Y","N","N","N","N","N","Y","Y",
     "N","N","N","Y","Y","N","N","N","N","N","N","N","N","N","N","N","Y","Y","Y","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","Y","Y","N","N","N","Y","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","Y"," ","N","N","N",
     "N","N","N","N","N","N","N","N","Y","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","Y","Y","Y",
     "Y","Y","Y","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","Y","Y"),stringsAsFactors=FALSE
                               ),
                    data.frame(UID ='WCAP99-0592',
                               PARAMETER='SIDCHN',
                               SAMPLE_TYPE='PHAB_THALW',
                               TRANSECT=c("A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B",
     "B","B","B","C","C","C","C","C","C","C","C","C","C","D","D","D","D","D","D","D","D","D","D","E",
     "E","E","E","E","E","E","E","E","E","F","F","F","F","F","F","F","F","F","F","G","G","G","G","G","G",
     "G","G","G","G","H","H","H","H","H","H","H","H","H","H","I","I","I","I","I","I","I","I","I","I","J","J",
     "J","J","J","J","J","J","J","J","XF","XF","XF","XG","XG","XG","XG","XG","XG","XG","XG","XG",
     "XG"),
                               STATION=c("0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","7","8","9","0","1","2","3","4","5","6","7","8","9"),
                               RESULT=c("Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y",
     "Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y",
     "Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y",
     "Y","Y","Y","Y"," ","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y",
     "Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y",
     "Y","Y","Y","Y"),stringsAsFactors=FALSE
                               ),
                    data.frame(UID ='WCAP99-0905',
                               PARAMETER='SIDCHN',
                               SAMPLE_TYPE='PHAB_THALW',
                               TRANSECT=c("A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B",
     "B","B","B","C","C","C","C","C","C","C","C","C","C","D","D","D","D","D","D","D","D","D","D","E",
     "E","E","E","E","E","E","E","E","E","F","F","F","F","F","F","F","F","F","F","G","G","G","G","G","G",
     "G","G","G","G","H","H","H","H","H","H","H","H","H","H","I","I","I","I","I","I","I","I","I","I","J","J",
     "J","J","J","J","J","J","J","J","XI","XJ"),
                               STATION=c("0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","0","0"),
                               RESULT=c("N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y",
     "Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","N","Y"),stringsAsFactors=FALSE
                              )
  )



      testData$UID <- as.character(testData$UID)
      testData$TRANSECT <- as.character(testData$TRANSECT)
      testData$STATION <- as.numeric(testData$STATION)
      testData$SAMPLE_TYPE <- as.character(testData$SAMPLE_TYPE)
      testData$PARAMETER <- as.character(testData$PARAMETER)
      testData$RESULT <- as.character(testData$RESULT)

  return(testData)
}



metsGeneral.expectedMets <- function()
# Create dataframe of expected metrics calculations
# The sidecnt value for WCAP99-0905 was changed from 3 to 2 due to the
# difference in how the value was calculated -- EMAP defines it as the
# number of side channel transects in the sub_bank file, while NRSA defines
# it as the number of side channel transects recorded in the tblTHALWEG2 table.
{
  metsExpected <- rbind(data.frame(UID = c('WCAP99-0585','WCAP99-0587','WCAP99-0591','WCAP99-0592','WCAP99-0905'),
                                   METRIC='pct_side',
                                   RESULT=c( NA,0,10.552763819,100,28 )
                                   ,stringsAsFactors=FALSE
                                  )
                       ,data.frame(UID = c(#'WCAP99-0585','WCAP99-0585 w/DISTANCE',
                                           'WCAP99-0587','WCAP99-0591','WCAP99-0591 w/DISTANCE','WCAP99-0591 w/ACTRANSP+DISTANCE','WCAP99-0591 w/ACTRANSP+DISTANCE and some NA','WCAP99-0592','WCAP99-0905'
                                          ),
                                   METRIC='reachlen',
                                   RESULT=c(#NA,NA,
                                            153,6000,8000,6000,5600,148.5,158.4
                                           )
                                   ,stringsAsFactors=FALSE
                                  )
                       ,data.frame(UID = c('WCAP99-0585','WCAP99-0587','WCAP99-0591','WCAP99-0592','WCAP99-0905'),
                                   METRIC='sidecnt',
                                   RESULT=c(NA,0,NA,2,2 )
                                   ,stringsAsFactors=FALSE
                                  )
                       )
  return(metsExpected)
}



# end of file
