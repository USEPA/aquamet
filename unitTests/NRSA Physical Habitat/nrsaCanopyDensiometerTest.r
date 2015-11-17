# nrsaCanopyDensiometerTest.r
# RUnit tests


nrsaCanopyDensiometerTest <- function()
# Unit test for nrsaCanopyDensiometer
{
    intermediateMessage('.2.0 Canopy Densiometer test of data', loc='end')
    intermediateMessage('.2.1 Create dataset from WEMAP', loc='end')
    testData <- nrsaCanopyDensiometer.testData() %>% dplyr::rename(SITE=UID, VALUE=RESULT, DIRECTION=TRANSDIR)
    test.w <- subset(testData, SAMPLE_TYPE == 'PHAB_CHANW')
    test.b <- subset(testData, SAMPLE_TYPE == 'PHAB_CHANB')

    intermediateMessage ('.2.2 Create expected dataset', loc='end')
    metsExpected <- nrsaCanopyDensiometer.expectedMets () %>% dplyr::rename(SITE=UID, VALUE=RESULT)
   
#in this dataset, the protoccol is determined by SAMPLE_TYPE.  Here where
#SAMPLE_TYPE == PHAB_CHANW, the protocol is WADEABLE
#SAMPLE_TYPE == PHAB_CHANB the protocol is BOATABLE   
#to test for one/both protocols, subset the data by SAMPLE_TYPE as a proxy for protocol

    intermediateMessage ('.2.3 Test with both protocols', loc='end')
  
    nrsaCanopyDensiometerTest.process (test.b, test.w, metsExpected)

    intermediateMessage ('.2.4 Test with wadeable protocol', loc='end')
    expected.w <- subset (metsExpected, SITE %in%  c('WAZP99-0545','WAZP99-0551','WCAP99-0534',
                                           'WCAP99-0535','WCAP99-0536','WCAP99-0539',
                                           'WMTP99-0549'))
    nrsaCanopyDensiometerTest.process (NULL, test.w, expected.w)
  
    intermediateMessage ('.2.5 Test with boatable protocol', loc='end')
    expected.b <- subset (metsExpected, SITE %in%  c('WMTP99-0525','WMTP99-0587'))
    nrsaCanopyDensiometerTest.process (test.b, NULL, expected.b)
} 


nrsaCanopyDensiometerTest.process <- function (bTestData, wTestData, metsExpected)
{
    testDataResult<- nrsaCanopyDensiometer(bTestData, wTestData)
  
    #compare results from baseData (testDataResult) with expectedResults  (metsExpected)
    metsExpected <- dplyr::rename(metsExpected, EXPECTED=VALUE)
    rr <- testDataResult

    # Calculated values should be within 10E-7 of expected values, should
    # only be missing where they are supposed to be missing and nonmissing where
    # they are supposed to be nonmissing.
    # Note: the errs dataframe can be printed to show where the errors occur when
    # debugging.
    tt <- merge(rr, metsExpected, by=c('SITE','METRIC'),all=T)
    tt$diff <- tt$VALUE - tt$EXPECTED

    errs <- subset(tt, abs(diff) > 10^-5 | is.na(VALUE) != is.na(EXPECTED))
    checkEquals(0, nrow(errs)
               ,"Error: Canopy Densiometer  metrics are broken"
               )
}



nrsaCanopyDensiometer.cleanup <- function(indb)
# Clean up when nrsaCanopyDensiometer() terminates
{
  odbcClose(indb)
}



nrsaCanopyDensiometer.testData <- function()
# creates dataframe of canopy densiometer data for unit test
{
  # Create correctly formated test data, and run data through nrsaCanopyDensiometer
  testData <- rbind(expand.grid(TRANSDIR =c('CD','CL','CR','CU','LF','RT')
                               ,TRANSECT = LETTERS[1:11]
                               ,UID = c('WAZP99-0545','WAZP99-0551','WCAP99-0534'
                                       ,'WCAP99-0535','WCAP99-0536','WCAP99-0539'
                                       ,'WMTP99-0549'
                                       )
                               ,SAMPLE_TYPE = 'PHAB_CHANW'
                               ,PARAMETER = 'DENSIOM'
                               )
                   )

  testData$RESULT <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                      ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                      ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,17,17,17,17
                      ,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17
                      ,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17
                      ,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17
                      ,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17
                      ,17,17,17,17,17,17,16,14,17,17,14,14,13,12,17,17
                      ,11,15,16,9,17,17,16,16,11,10,17,17,12,17,14,15
                      ,17,16,8,9,0,0,17,17,0,17,0,5,17,12,17,17,17,17
                      ,17,17,3,0,6,2,7,5,0,0,2,0,12,3,0,3,0,0,10,2,5,13
                      ,0,9,17,13,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
                      ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
                      ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
                      ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
                      ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
                      ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
                      ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
                      ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,13,15,15,17,16
                      ,17,17,17,17,17,17,17,17,17,17,17,17,17,17,16,16
                      ,15,1,17,15,16,15,15,17,15,16,16,17,14,17,17,15
                      ,13,12,15,16,14,17,17,16,17,17,11,NA,NA,NA,NA,NA
                      ,NA,NA,NA,NA,NA,NA,NA,17,17,17,17,17,17,0,0,0,0,3
                      ,2,0,0,0,0,2,0,0,0,0,0,2,2,0,0,0,0,5,17,0,0,0,0,7
                      ,17,0,0,0,0,4,12,0,0,0,0,10,14,0,0,0,0,8,0,0,0,0
                      ,0,4,2,0,0,0,0,4,15,0,0,0,0,5,12
                      )
  testData$UID <- as.character(testData$UID)
  testData$TRANSECT <- as.character(testData$TRANSECT)
  testData$TRANSDIR <- as.character(testData$TRANSDIR)
  testData$SAMPLE_TYPE <- as.character(testData$SAMPLE_TYPE)
  testData$PARAMETER <- as.character(testData$PARAMETER)

  testBoat <- rbind(expand.grid(TRANSDIR =c('UP','DN','LF','RT')
                               ,TRANSECT = LETTERS[1:11]
                               ,UID = c('WMTP99-0525','WMTP99-0587')
                               ,SAMPLE_TYPE='PHAB_CHANB'
                               ,PARAMETER = 'DENSIOM'
                               )
                   )

  intermediateMessage('.2.2 Create and add Boat data', loc='end')

  testBoat$RESULT <- c(2,10, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2, 1, 10, 13
                      ,0, 8, 8,11, 0, 8, 0, 0, 0, 0, 1, 0,14,2, 0, 0, 0, 0, 0
                      ,0, 0, 0, 0, 0, 0, 0, 9,13, 4,15, 0, 0, 0, 0,10, 0,10, 0
                      ,13, 4,10, 2, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0
                      ,0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0
                      )

  testBoat$UID <- as.character(testBoat$UID)
  testBoat$TRANSECT <- as.character(testBoat$TRANSECT)
  testBoat$TRANSDIR <- as.character(testBoat$TRANSDIR)
  testBoat$SAMPLE_TYPE <- as.character(testBoat$SAMPLE_TYPE)
  testBoat$PARAMETER <- as.character(testBoat$PARAMETER)

  testData<-rbind (testData,testBoat)

  return(testData)
}



nrsaCanopyDensiometer.expectedMets <- function()
# creates dataframe of canopy densiometer metrics calculation results for unit test
{
  metsExpected <- rbind(data.frame(UID = c('WAZP99-0545','WAZP99-0551','WCAP99-0534',
                                           'WCAP99-0535','WCAP99-0536','WCAP99-0539',
                                           'WMTP99-0549'),
                                   METRIC='xcdenmid',
                                   RESULT=c(0,100.000, 62.433155, 22.05882352,
                                            NA ,93.7908496,0)
                                           )
                       ,data.frame(UID = c('WAZP99-0545','WAZP99-0551',
                                           'WCAP99-0534','WCAP99-0535','WCAP99-0536',
                                           'WCAP99-0539','WMTP99-0549'),
                                   METRIC='vcdenmid',
                                   RESULT=c(  0,  0, 38.81681136, 29.11616158, NA ,
                                            7.94748513, 0)
                                           )
                       ,data.frame(UID = c('WAZP99-0545','WAZP99-0551',
                                           'WCAP99-0534','WCAP99-0535','WCAP99-0536',
                                           'WCAP99-0539','WMTP99-0549'),
                                   METRIC='nmid',
                                   RESULT=c(44,44,44, 8, 0,36,44  )
                                  )
                       ,data.frame(UID = c('WAZP99-0545','WAZP99-0551',
                                           'WCAP99-0534','WCAP99-0535','WCAP99-0536'
                                          ,'WCAP99-0539','WMTP99-0549'),
                                   METRIC='xcdenbk',
                                   RESULT=c(  0,100.000, 87.433155, 61.764705,NA ,
                                            90.52287, 39.304812)
                                           )
                       ,data.frame(UID = c('WAZP99-0545','WAZP99-0551',
                                           'WCAP99-0534','WCAP99-0535','WCAP99-0536'
                                          ,'WCAP99-0539','WMTP99-0549'),
                                   METRIC='vcdenbk',
                                   RESULT=c( 0, 0,25.3689502,37.3579585,NA ,
                                            23.016811,32.594532)
                                           )
                       ,data.frame(UID = c('WAZP99-0545','WAZP99-0551',
                                           'WCAP99-0534','WCAP99-0535','WCAP99-0536',
                                           'WCAP99-0539','WMTP99-0549'),
                                   METRIC='nbnk',
                                   RESULT=c( 22,22,22, 4, 0, 18, 22)
                                  )
                       ,data.frame(UID = c('WMTP99-0525','WMTP99-0587'),
                                   METRIC='xcdenbk',
                                   RESULT=c(12.700534,16.176470)
                                  )
                       ,data.frame(UID = c('WMTP99-0525','WMTP99-0587'),
                                   METRIC='vcdenbk',
                                   RESULT=c(23.799516,27.279878)
                                  )
                       ,data.frame(UID = c('WMTP99-0525','WMTP99-0587'),
                                   METRIC='nbnk',
                                   RESULT=c(44,36)
                                  )
                       )

  metsExpected <- mutate(metsExpected, UID=as.character(UID), METRIC=as.character(METRIC))
  return(metsExpected)
}



# end of file
